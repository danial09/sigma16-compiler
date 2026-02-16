// Check if a number is prime
fn is_prime(n) {
    if n <= 1 {
        return 0;  // Not prime
    }

    if n == 2 {
        return 1;  // 2 is prime
    }

    // Check divisibility from 2 to n-1
    i = 2;
    while i < n {
        if n % i == 0 {
            return 0;  // Found divisor, not prime
        }
        i = i + 1;
    }

    return 1;  // No divisors found, is prime
}

result = is_prime(17);  // Returns 1 (true)
