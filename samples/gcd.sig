// Greatest Common Divisor (Euclidean algorithm)
fn gcd(a, b) {
    // Keep finding remainder until b is 0
    while b != 0 {
        temp = b;
        b = a % b;
        a = temp;
    }
    return a;
}

result = gcd(48, 18);  // Returns 6
