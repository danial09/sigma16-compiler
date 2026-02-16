use std::collections::{HashMap, HashSet};
use std::io;
use std::path::PathBuf;
use std::time::{Duration, Instant};

use anyhow::Result;
use crossterm::event::{
    self, DisableMouseCapture, EnableMouseCapture, Event, KeyCode, KeyEvent, KeyEventKind,
    KeyModifiers,
};
use crossterm::execute;
use crossterm::terminal::{
    disable_raw_mode, enable_raw_mode, EnterAlternateScreen, LeaveAlternateScreen,
};
use ratatui::backend::CrosstermBackend;
use ratatui::layout::{Constraint, Direction, Layout, Rect};
use ratatui::style::{Color, Modifier, Style};
use ratatui::text::{Line, Span};
use ratatui::widgets::{Block, Borders, Clear, List, ListItem, Paragraph, Tabs, Wrap};
use ratatui::Terminal;
use tui_textarea::{Input, Key, TextArea};

use s16_compiler::backend::compile_ir_to_sigma16_mapped;
use s16_compiler::ir::{AstNodeId, AstNodeKind, ControlFlowComponent, ProgramIR};

pub fn run_tui(path: Option<PathBuf>, initial_text: String) -> Result<()> {
    // Setup terminal
    enable_raw_mode()?;
    let mut stdout = io::stdout();
    execute!(stdout, EnterAlternateScreen, EnableMouseCapture)?;
    let backend = CrosstermBackend::new(stdout);
    let mut terminal = Terminal::new(backend)?;

    let mut app = App::new(path, initial_text);
    let res = run_app(&mut terminal, &mut app);

    // Restore terminal
    disable_raw_mode().ok();
    execute!(
        terminal.backend_mut(),
        LeaveAlternateScreen,
        DisableMouseCapture
    )
    .ok();
    terminal.show_cursor().ok();

    res
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum RightTab {
    Asm,
    Ir,
    Ast,
    Mappings,
    Errors,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum Mode {
    Normal,
    OpenPrompt,
    Help,
}

struct App {
    file_path: Option<PathBuf>,
    editor: TextArea<'static>,
    right_tab: RightTab,
    status: String,
    last_error: Option<String>,
    ir: Option<ProgramIR>,
    ir_lines: Vec<String>,
    asm_lines: Vec<String>,
    asm_ir_mapping: Vec<Option<usize>>, // per-ASM-line -> IR index (if any)
    // Cached AST spans by line for display
    ast_spans: Vec<(AstNodeId, usize, usize, AstNodeKind)>,
    // Selection within IR list
    ir_selected: usize,
    // Last compile time
    last_compiled_at: Option<Instant>,
    // UI mode
    mode: Mode,
    // Open file path input buffer
    open_input: String,
}

impl App {
    fn new(path: Option<PathBuf>, text: String) -> Self {
        let editor = make_editor(&text);

        Self {
            file_path: path,
            editor,
            right_tab: RightTab::Asm,
            status:
                "Ready. F5/Ctrl+R: Compile | Ctrl+S: Save | Tab: Switch pane | Esc/Ctrl+Q: Quit"
                    .to_string(),
            last_error: None,
            ir: None,
            ir_lines: Vec::new(),
            asm_lines: Vec::new(),
            asm_ir_mapping: Vec::new(),
            ast_spans: Vec::new(),
            ir_selected: 0,
            last_compiled_at: None,
            mode: Mode::Normal,
            open_input: String::new(),
        }
    }

    fn compile(&mut self) {
        let source = self.editor.lines().join("\n");
        match s16_compiler::compile_to_ir(&source) {
            Ok(ir) => {
                self.ir_lines = ir.to_lines();
                self.ir_selected = 0.min(self.ir_lines.len().saturating_sub(1));
                self.ast_spans = ir.source_map.list_ast_spans_by_line();
                self.last_error = None;
                // Build assembly from the IR using Basic allocator by default
                let asm = compile_ir_to_sigma16_mapped(&ir);
                self.asm_lines = asm.lines.clone();
                self.asm_ir_mapping = asm.asm_ir_mapping.clone();
                self.status = format!(
                    "Compiled successfully ({} IR instructions, {} ASM lines)",
                    self.ir_lines.len(),
                    self.asm_lines.len()
                );
                self.last_compiled_at = Some(Instant::now());
                self.ir = Some(ir);
            }
            Err(e) => {
                self.last_error = Some(format!("{}", e));
                self.status = "Compilation failed (see Errors tab)".to_string();
                self.ir = None;
                self.ir_lines.clear();
                self.asm_lines.clear();
                self.asm_ir_mapping.clear();
                self.ast_spans.clear();
            }
        }
    }

    fn save(&mut self) {
        if let Some(path) = &self.file_path {
            let source = self.editor.lines().join("\n");
            match std::fs::write(path, source) {
                Ok(_) => self.status = format!("Saved to {}", path.display()),
                Err(e) => self.status = format!("Save failed: {}", e),
            }
        } else {
            self.status = "No file path set; cannot save. (Run with a file path)".to_string();
        }
    }

    fn cursor_pos_0(&self) -> (usize, usize) {
        let (line1, col1) = self.editor.cursor();
        // tui-textarea is 0-based internally; ensure non-negative
        (line1, col1)
    }
}

fn make_editor(text: &str) -> TextArea<'static> {
    let mut editor = TextArea::default();
    editor.set_block(Block::default().borders(Borders::ALL).title("Source"));
    editor.set_style(Style::default());
    editor.set_cursor_line_style(Style::default().fg(Color::Yellow));
    editor.set_cursor_style(Style::default().add_modifier(Modifier::REVERSED));
    if !text.is_empty() {
        editor.insert_str(text);
    }
    editor
}

fn run_app(terminal: &mut Terminal<CrosstermBackend<io::Stdout>>, app: &mut App) -> Result<()> {
    let mut last_tick = Instant::now();
    let tick_rate = Duration::from_millis(100);

    loop {
        terminal.draw(|f| ui(f, app))?;

        let timeout = tick_rate.saturating_sub(last_tick.elapsed());
        if crossterm::event::poll(timeout)? {
            match event::read()? {
                Event::Key(key) => {
                    if handle_key(app, key)? {
                        break;
                    }
                }
                Event::Resize(_, _) => {}
                _ => {}
            }
        }
        if last_tick.elapsed() >= tick_rate {
            last_tick = Instant::now();
        }
    }
    Ok(())
}

fn handle_key(app: &mut App, key: KeyEvent) -> Result<bool> {
    // Process only "press" events to avoid handling repeat/release twice.
    if key.kind != KeyEventKind::Press {
        return Ok(false);
    }
    // Mode-specific handling first
    match app.mode {
        Mode::Help => {
            if key.code == KeyCode::F(1) || key.code == KeyCode::Esc {
                app.mode = Mode::Normal;
            }
            return Ok(false);
        }
        Mode::OpenPrompt => match key.code {
            KeyCode::Enter => {
                let path_str = app.open_input.trim();
                if path_str.is_empty() {
                    app.status = "Open canceled".to_string();
                    app.mode = Mode::Normal;
                    return Ok(false);
                }
                let p = PathBuf::from(path_str);
                match std::fs::read_to_string(&p) {
                    Ok(content) => {
                        app.file_path = Some(p);
                        app.editor = make_editor(&content);
                        app.status = "File opened".to_string();
                        app.mode = Mode::Normal;
                    }
                    Err(e) => {
                        app.last_error = Some(format!("Open failed: {}", e));
                        app.status = "Open failed (see Errors tab)".to_string();
                        app.mode = Mode::Normal;
                    }
                }
                return Ok(false);
            }
            KeyCode::Esc => {
                app.mode = Mode::Normal;
                app.status = "Open canceled".to_string();
                return Ok(false);
            }
            KeyCode::Backspace => {
                app.open_input.pop();
                return Ok(false);
            }
            KeyCode::Char(c) => {
                if !key.modifiers.contains(KeyModifiers::CONTROL) {
                    app.open_input.push(c);
                }
                return Ok(false);
            }
            KeyCode::Tab
            | KeyCode::BackTab
            | KeyCode::Up
            | KeyCode::Down
            | KeyCode::Left
            | KeyCode::Right
            | KeyCode::Home
            | KeyCode::End
            | KeyCode::PageUp
            | KeyCode::PageDown
            | KeyCode::Delete
            | KeyCode::F(_) => {
                return Ok(false);
            }
            _ => {
                return Ok(false);
            }
        },
        Mode::Normal => {}
    }

    // Global quit
    if (key.code == KeyCode::Char('q') && key.modifiers.contains(KeyModifiers::CONTROL))
        || key.code == KeyCode::Esc
    {
        return Ok(true);
    }

    // Compile
    if key.code == KeyCode::F(5)
        || (key.code == KeyCode::Char('r') && key.modifiers.contains(KeyModifiers::CONTROL))
    {
        app.compile();
        return Ok(false);
    }

    // Save
    if key.code == KeyCode::Char('s') && key.modifiers.contains(KeyModifiers::CONTROL) {
        app.save();
        return Ok(false);
    }

    // Open
    if key.code == KeyCode::Char('o') && key.modifiers.contains(KeyModifiers::CONTROL) {
        app.mode = Mode::OpenPrompt;
        app.open_input.clear();
        app.status = "Open: type a file path and press Enter (Esc to cancel)".to_string();
        return Ok(false);
    }

    // Help
    if key.code == KeyCode::F(1) {
        app.mode = Mode::Help;
        return Ok(false);
    }

    // Switch right tab
    if key.code == KeyCode::Tab {
        app.right_tab = match app.right_tab {
            RightTab::Asm => RightTab::Ir,
            RightTab::Ir => RightTab::Ast,
            RightTab::Ast => RightTab::Mappings,
            RightTab::Mappings => RightTab::Errors,
            RightTab::Errors => RightTab::Asm,
        };
        return Ok(false);
    }
    if key.code == KeyCode::BackTab {
        app.right_tab = match app.right_tab {
            RightTab::Asm => RightTab::Errors,
            RightTab::Ir => RightTab::Asm,
            RightTab::Ast => RightTab::Ir,
            RightTab::Mappings => RightTab::Ast,
            RightTab::Errors => RightTab::Mappings,
        };
        return Ok(false);
    }

    // IR selection when IR tab focused
    if matches!(app.right_tab, RightTab::Ir) {
        match key.code {
            KeyCode::Up => {
                app.ir_selected = app.ir_selected.saturating_sub(1);
                return Ok(false);
            }
            KeyCode::Down => {
                if !app.ir_lines.is_empty() {
                    app.ir_selected = (app.ir_selected + 1).min(app.ir_lines.len() - 1);
                }
                return Ok(false);
            }
            _ => {}
        }
    }

    // Forward to editor by default
    // Map KeyEvent to tui-textarea Input
    let input = key_event_to_input(key);
    if let Some(input) = input {
        app.editor.input(input);
    }
    Ok(false)
}

fn key_event_to_input(key: KeyEvent) -> Option<Input> {
    // Minimal mapping; tui-textarea also supports raw paste etc.
    // Special case: BackTab (Shift+Tab) -> represent as Tab with shift=true
    if matches!(key.code, KeyCode::BackTab) {
        return Some(Input {
            key: Key::Tab,
            ctrl: key.modifiers.contains(KeyModifiers::CONTROL),
            alt: key.modifiers.contains(KeyModifiers::ALT),
            shift: true,
        });
    }

    let k = match key.code {
        KeyCode::Backspace => Key::Backspace,
        KeyCode::Enter => Key::Enter,
        KeyCode::Left => Key::Left,
        KeyCode::Right => Key::Right,
        KeyCode::Up => Key::Up,
        KeyCode::Down => Key::Down,
        KeyCode::Home => Key::Home,
        KeyCode::End => Key::End,
        KeyCode::PageUp => Key::PageUp,
        KeyCode::PageDown => Key::PageDown,
        KeyCode::Tab => Key::Tab,
        KeyCode::Delete => Key::Delete,
        KeyCode::Esc => return None,
        KeyCode::F(_) => return None,
        KeyCode::Char(c) => {
            // Ctrl shortcuts handled earlier
            if key.modifiers.contains(KeyModifiers::CONTROL) {
                return None;
            }
            return Some(Input {
                key: Key::Char(c),
                ctrl: false,
                alt: key.modifiers.contains(KeyModifiers::ALT),
                shift: key.modifiers.contains(KeyModifiers::SHIFT),
            });
        }
        _ => return None,
    };
    Some(Input {
        key: k,
        ctrl: key.modifiers.contains(KeyModifiers::CONTROL),
        alt: key.modifiers.contains(KeyModifiers::ALT),
        shift: key.modifiers.contains(KeyModifiers::SHIFT),
    })
}

fn ui(f: &mut ratatui::Frame, app: &mut App) {
    let size = f.area();

    // Layout: vertical split: main (editor + right pane), status bar
    let chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Min(3), Constraint::Length(1)])
        .split(size);

    draw_main_area(f, app, chunks[0]);
    draw_status(f, app, chunks[1]);
    // Overlays
    match app.mode {
        Mode::OpenPrompt => draw_open_prompt(f, app, chunks[0]),
        Mode::Help => draw_help(f, chunks[0]),
        Mode::Normal => {}
    }
}

fn draw_main_area(f: &mut ratatui::Frame, app: &mut App, area: Rect) {
    let cols = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([Constraint::Percentage(60), Constraint::Percentage(40)])
        .split(area);

    // Editor on the left
    f.render_widget(&app.editor, cols[0]);

    // Right side: tabs + content stacked vertically
    let right_chunks = Layout::default()
        .direction(Direction::Vertical)
        .constraints([Constraint::Length(3), Constraint::Min(3)])
        .split(cols[1]);

    let titles = ["ASM", "IR", "AST", "Mappings", "Errors"]
        .into_iter()
        .map(|t| Line::from(Span::styled(t, Style::default().fg(Color::Cyan))));
    let selected = match app.right_tab {
        RightTab::Asm => 0,
        RightTab::Ir => 1,
        RightTab::Ast => 2,
        RightTab::Mappings => 3,
        RightTab::Errors => 4,
    };
    let tabs = Tabs::new(titles)
        .select(selected)
        .block(Block::default().borders(Borders::ALL).title("Analysis"))
        .highlight_style(Style::default().fg(Color::Yellow));
    f.render_widget(tabs, right_chunks[0]);

    match app.right_tab {
        RightTab::Asm => draw_asm(f, app, right_chunks[1]),
        RightTab::Ir => draw_ir(f, app, right_chunks[1]),
        RightTab::Ast => draw_ast(f, app, right_chunks[1]),
        RightTab::Mappings => draw_mappings(f, app, right_chunks[1]),
        RightTab::Errors => draw_errors(f, app, right_chunks[1]),
    }
}

fn draw_status(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let (line0, col0) = app.cursor_pos_0();
    let filename = app
        .file_path
        .as_ref()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| "<unnamed>".to_string());
    let msg = format!(
        "{}  |  Ln {}, Col {}  |  {}",
        filename,
        line0 + 1,
        col0 + 1,
        app.status
    );
    let bar = Paragraph::new(msg).block(Block::default().borders(Borders::ALL));
    f.render_widget(bar, area);
}

fn draw_ir(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let mut items: Vec<ListItem> = Vec::with_capacity(app.ir_lines.len());

    // Two-tier highlighting:
    // - stmt_set: IR instrs for the specific AST node under cursor (bold green)
    // - block_set: IR instrs for the surrounding control-flow block (blue)
    let mut stmt_set: HashSet<usize> = HashSet::new();
    let mut block_set: HashSet<usize> = HashSet::new();

    if let Some(ir) = &app.ir {
        let (l, c) = app.cursor_pos_0();

        // Helper: prefer meaningful AST kinds for hover selection
        fn preferred_kind(k: AstNodeKind) -> bool {
            matches!(
                k,
                AstNodeKind::Binary
                    | AstNodeKind::Unary
                    | AstNodeKind::Assign
                    | AstNodeKind::If
                    | AstNodeKind::While
                    | AstNodeKind::For
            )
        }

        // Choose best AST under cursor; if the direct node has no IR, climb to a containing node
        let best_ast = if let Some(base) = ir.source_map.get_ast_info_at(l, c) {
            // Start with what the compiler reports (already biased to nodes with IR when possible)
            let mut chosen = base.clone();
            let has_ir = !ir.source_map.get_instrs_for_ast(chosen.id).is_empty();
            if !has_ir {
                // Consider ancestors: spans that fully contain the base span
                let mut candidates: Vec<(usize, AstNodeId, AstNodeKind)> = Vec::new();
                for (id, s, e, kind) in &app.ast_spans {
                    if *s <= base.span.start && base.span.end <= *e {
                        let len = e - s;
                        candidates.push((len, *id, *kind));
                    }
                }
                // Prefer candidates with IR; among those, prefer preferred kinds; tie-break by smallest span
                candidates.sort_by_key(|(len, _, _)| *len);
                let mut pick: Option<AstNodeId> = None;
                // Pass 1: has IR + preferred kind
                for &(_, id, kind) in &candidates {
                    if preferred_kind(kind) && !ir.source_map.get_instrs_for_ast(id).is_empty() {
                        pick = Some(id);
                        break;
                    }
                }
                // Pass 2: has IR (any kind)
                if pick.is_none() {
                    for &(_, id, _) in &candidates {
                        if !ir.source_map.get_instrs_for_ast(id).is_empty() {
                            pick = Some(id);
                            break;
                        }
                    }
                }
                if let Some(id) = pick {
                    if let Some(info) = ir.source_map.get_ast_info_by_id(id) {
                        chosen = info;
                        // has_ir = true;
                    }
                }
            }
            Some(chosen)
        } else {
            None
        };

        if let Some(ast_info) = best_ast {
            // Primary: statement/expression-specific IR
            for idx in ir.source_map.get_instrs_for_ast(ast_info.id) {
                stmt_set.insert(idx);
            }

            // Infer which block component we're in from the mappings of the statement's IR
            let mut comp_counts: HashMap<ControlFlowComponent, usize> = HashMap::new();
            for &idx in &stmt_set {
                for m in ir.source_map.get_mappings_for_instr(idx) {
                    if let Some(comp) = m.component {
                        match comp {
                            ControlFlowComponent::ThenBranch
                            | ControlFlowComponent::ElseBranch
                            | ControlFlowComponent::LoopBody => {
                                *comp_counts.entry(comp).or_insert(0) += 1;
                            }
                            _ => {}
                        }
                    }
                }
            }

            let block_component: Option<ControlFlowComponent> = comp_counts
                .into_iter()
                .max_by_key(|(_, n)| *n)
                .map(|(c, _)| c);

            // Find the innermost enclosing control statement (If/While/For)
            let mut control_span: Option<(usize, usize)> = None;
            for (_, s, e, kind) in &app.ast_spans {
                if *s <= ast_info.span.start && ast_info.span.end <= *e {
                    if matches!(
                        kind,
                        AstNodeKind::If | AstNodeKind::While | AstNodeKind::For
                    ) {
                        match control_span {
                            Some((cs, ce)) => {
                                if (*e - *s) < (ce - cs) {
                                    control_span = Some((*s, *e));
                                }
                            }
                            None => control_span = Some((*s, *e)),
                        }
                    }
                }
            }

            if let (Some(comp), Some((cs, ce))) = (block_component, control_span) {
                // Secondary: all IR in the same block component within the control span
                for idx in 0..ir.instrs.len() {
                    if stmt_set.contains(&idx) {
                        continue;
                    }
                    let maps = ir.source_map.get_mappings_for_instr(idx);
                    let in_block = maps.iter().any(|m| {
                        if m.component != Some(comp) {
                            return false;
                        }
                        if let Some(info) = ir.source_map.get_ast_info_by_id(m.ast_node_id) {
                            info.span.start >= cs && info.span.end <= ce
                        } else {
                            false
                        }
                    });
                    if in_block {
                        block_set.insert(idx);
                    }
                }
            }
        } else {
            // Fallback: if no AST node, highlight by position mapping as before
            for idx in ir.source_map.get_instrs_at(l, c) {
                stmt_set.insert(idx);
            }
        }
    }

    // Render with layered styles: selection -> block (blue) -> statement (bold green)
    for (i, line) in app.ir_lines.iter().enumerate() {
        let mut style = Style::default();
        if i == app.ir_selected {
            style = style.fg(Color::Yellow);
        }
        if block_set.contains(&i) {
            style = style.fg(Color::Blue);
        }
        if stmt_set.contains(&i) {
            style = style.add_modifier(Modifier::BOLD).fg(Color::Green);
        }
        let text = format!("{:3}: {}", i, line);
        items.push(ListItem::new(text).style(style));
    }

    let list = List::new(items).block(Block::default().borders(Borders::ALL).title("IR"));
    f.render_widget(list, area);
}

fn draw_ast(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let mut items: Vec<ListItem> = Vec::new();
    for (id, start, end, kind) in &app.ast_spans {
        let text = format!(
            "{:>3}  {:<8}  lines {}..{}",
            id.0,
            fmt_kind(*kind),
            start + 1,
            end + 1
        );
        items.push(ListItem::new(text));
    }
    if items.is_empty() {
        items.push(ListItem::new("<no AST spans – compile (F5)>"));
    }
    let list = List::new(items).block(Block::default().borders(Borders::ALL).title("AST Spans"));
    f.render_widget(list, area);
}

fn draw_mappings(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let mut lines: Vec<Line> = Vec::new();
    if let Some(ir) = &app.ir {
        // Position-based
        let (l, c) = app.cursor_pos_0();
        if let Some(info) = ir.source_map.get_ast_info_at(l, c) {
            let instrs = ir.source_map.get_instrs_for_ast(info.id);
            lines.push(Line::from(format!(
                "Cursor at ({}, {}): AST id={} kind={:?} span=({}-{}) -> IR {:?}",
                l + 1,
                c + 1,
                info.id.0,
                info.kind,
                info.span.start,
                info.span.end,
                instrs
            )));

            // Also show ASM lines corresponding to these IRs
            if !app.asm_ir_mapping.is_empty() {
                use std::collections::HashSet;
                let set: HashSet<usize> = instrs.into_iter().collect();
                let mut asm_lines_for_ast: Vec<usize> = Vec::new();
                for (i, m) in app.asm_ir_mapping.iter().enumerate() {
                    if let Some(mi) = m {
                        if set.contains(mi) {
                            asm_lines_for_ast.push(i);
                        }
                    }
                }
                if !asm_lines_for_ast.is_empty() {
                    lines.push(Line::from(format!(
                        "  -> ASM line idxs: {:?}",
                        asm_lines_for_ast
                    )));
                }
            }
        } else {
            lines.push(Line::from("Cursor not over any AST node."));
        }
        // IR-selected mapping
        if app.ir_selected < ir.instrs.len() {
            let idx = app.ir_selected;
            let maps = ir.source_map.get_mappings_for_instr(idx);
            lines.push(Line::from(format!(
                "Selected IR [{}]: {} mapping(s)",
                idx,
                maps.len()
            )));
            for m in maps {
                lines.push(Line::from(format!(
                    "  -> AST id={} component={:?} desc={}",
                    m.ast_node_id.0, m.component, m.description
                )));
            }
            // Also show ASM lines where this IR appears
            if !app.asm_ir_mapping.is_empty() {
                let mut asm_lines_for_ir: Vec<usize> = Vec::new();
                for (i, m) in app.asm_ir_mapping.iter().enumerate() {
                    if matches!(m, Some(mi) if *mi == idx) {
                        asm_lines_for_ir.push(i);
                    }
                }
                if !asm_lines_for_ir.is_empty() {
                    lines.push(Line::from(format!(
                        "  -> ASM line idxs: {:?}",
                        asm_lines_for_ir
                    )));
                }
            }
        }
    } else {
        lines.push(Line::from("Compile the source (F5) to see mappings."));
    }
    let p = Paragraph::new(lines).block(Block::default().borders(Borders::ALL).title("Mappings"));
    f.render_widget(p, area);
}

fn draw_errors(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let msg = app
        .last_error
        .clone()
        .unwrap_or_else(|| "No errors.".to_string());
    let p = Paragraph::new(msg).block(Block::default().borders(Borders::ALL).title("Errors"));
    f.render_widget(p, area);
}

fn draw_asm(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let mut items: Vec<ListItem> = Vec::new();
    if app.asm_lines.is_empty() {
        items.push(ListItem::new("<no ASM – compile (F5)>"));
    } else {
        // Build a set of IR indices related to AST under cursor for contextual highlight
        let mut ast_related: HashSet<usize> = HashSet::new();
        if let Some(ir) = &app.ir {
            let (l, c) = app.cursor_pos_0();
            if let Some(info) = ir.source_map.get_ast_info_at(l, c) {
                for idx in ir.source_map.get_instrs_for_ast(info.id) {
                    ast_related.insert(idx);
                }
            }
        }

        for (i, line) in app.asm_lines.iter().enumerate() {
            let ir_map = app.asm_ir_mapping.get(i).cloned().unwrap_or(None);
            let label = match ir_map {
                Some(ix) => format!("{:>4}", ix),
                None => "   -".to_string(),
            };
            let composed = format!("{}  {}", label, line);
            let mut style = Style::default();
            if let Some(ix) = ir_map {
                if Some(ix) == Some(app.ir_selected) {
                    style = style.fg(Color::Yellow).add_modifier(Modifier::BOLD);
                } else if ast_related.contains(&ix) {
                    style = style.fg(Color::Green);
                }
            } else {
                style = style.fg(Color::DarkGray);
            }
            items.push(ListItem::new(composed).style(style));
        }
    }
    let list = List::new(items).block(
        Block::default()
            .borders(Borders::ALL)
            .title("Assembly  [col: IR index or '-']"),
    );
    f.render_widget(list, area);
}

fn fmt_kind(k: AstNodeKind) -> &'static str {
    match k {
        AstNodeKind::Assign => "Assign",
        AstNodeKind::If => "If",
        AstNodeKind::While => "While",
        AstNodeKind::For => "For",
        AstNodeKind::Number => "Number",
        AstNodeKind::Variable => "Variable",
        AstNodeKind::Binary => "Binary",
        AstNodeKind::Unary => "Unary",
    }
}

fn centered_rect(percent_x: u16, percent_y: u16, r: Rect) -> Rect {
    let popup_layout = Layout::default()
        .direction(Direction::Vertical)
        .constraints([
            Constraint::Percentage((100 - percent_y) / 2),
            Constraint::Percentage(percent_y),
            Constraint::Percentage((100 - percent_y) / 2),
        ])
        .split(r);

    let vertical = Layout::default()
        .direction(Direction::Horizontal)
        .constraints([
            Constraint::Percentage((100 - percent_x) / 2),
            Constraint::Percentage(percent_x),
            Constraint::Percentage((100 - percent_x) / 2),
        ])
        .split(popup_layout[1]);

    vertical[1]
}

fn draw_open_prompt(f: &mut ratatui::Frame, app: &App, area: Rect) {
    let area = centered_rect(70, 20, area);
    let block = Block::default().title("Open File").borders(Borders::ALL);
    let text = vec![
        Line::from("Enter path and press Enter:"),
        Line::from(app.open_input.as_str()),
    ];
    let p = Paragraph::new(text).block(block).wrap(Wrap { trim: false });
    f.render_widget(Clear, area);
    f.render_widget(p, area);
}

fn draw_help(f: &mut ratatui::Frame, area: Rect) {
    let area = centered_rect(70, 60, area);
    let block = Block::default().title("Help").borders(Borders::ALL);
    let lines = vec![
        Line::from("Keybindings:"),
        Line::from("  F5 / Ctrl+R  - Compile"),
        Line::from("  Ctrl+S       - Save (to current path)"),
        Line::from("  Ctrl+O       - Open file (type path)"),
        Line::from("  Tab/Shift+Tab- Switch right tab"),
        Line::from("  Esc/Ctrl+Q   - Quit"),
        Line::from("  F1           - Toggle this help"),
        Line::from(""),
        Line::from("Tabs:"),
        Line::from("  ASM       - Shows generated Sigma16 assembly"),
        Line::from("  IR        - Shows generated IR; arrows move selection; mapped instrs from cursor highlighted"),
        Line::from("  AST       - Lists AST node spans by lines (compile first)"),
        Line::from("  Mappings  - Shows AST at cursor and IR mapping, and for selected IR line"),
        Line::from("  Errors    - Last error message"),
    ];
    let p = Paragraph::new(lines)
        .block(block)
        .wrap(Wrap { trim: false });
    f.render_widget(Clear, area);
    f.render_widget(p, area);
}
