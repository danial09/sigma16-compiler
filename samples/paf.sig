array a[3] = [1, 2, 3];

x = 5;
y = inc(x);

// pointers and arrays
p = &a;      // address of array base (a[0])
*p = 42;     // store through pointer
a[1] = y;    // store into array element
z = *(p);    // load through pointer

fn inc(x) {
  return x + 1;
}

