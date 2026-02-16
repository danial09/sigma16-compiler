// Recursive factorial sample to exercise call/return and the stack

// A simple recursive factorial
fn fact(n) {
  if n > 1 {
    // recursive case
    return n * fact(n - 1);
  } else {
    // base case
    return 1;
  }
}

// Choose an input value
x = 5;

// Compute factorial(x) and store in result
result = fact(x);
