num = 1234;
result = sum_digits(num);  // Returns 10
result2 = sum_digits(5678);  // Returns 26

// Sum of digits in a number
fn sum_digits(n) {
    sum = 0;

    // Extract each digit using modulo
    while n > 0 {
        digit = n % 10;  // Get last digit
        sum = sum + digit;
        n = n / 10;      // Remove last digit
    }

    return sum;
}
