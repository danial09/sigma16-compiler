fn strlen(s) {
  i = 0;
  while s[i] != 0 {
    i = i + 1;
  }
  return i;
}

str = "Hello, world!";
len = strlen(str);
