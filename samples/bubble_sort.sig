// Bubble sort algorithm
fn bubble_sort(arr, n) {
    i = 0;

    // Outer loop: n-1 passes
    while i < n - 1 {
        j = 0;

        // Inner loop: compare adjacent elements
        while j < n - i - 1 {
            // If elements are out of order, swap
            if arr[j] > arr[j + 1] {
                temp = arr[j];
                arr[j] = arr[j + 1];
                arr[j + 1] = temp;
            }
            j = j + 1;
        }
        i = i + 1;
    }

    return arr;
}

array x[7] = [64, 34, 25, 12, 22, 11, 90];
n = 7;

bubble_sort(x, n);