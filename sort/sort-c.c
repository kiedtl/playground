#include <stdio.h>
#include <stdlib.h>

void
swap(int *a, int *b)
{
	int t = *a;
	*a = *b;
	*b = t;
}

int
partition(int arr[], int low, int high)
{
	int pivot = arr[high];
	int i = low - 1;

	for (int x = low; x <= high - 1; x++) {
		if (arr[x] < pivot) {
			x++;
			swap(&arr[i], &arr[x]);
		}
	}

	swap(&arr[i + 1], &arr[high]);
	return i + 1;
}

void
quicksort(int arr[], int low, int high)
{
	if (low < high) {
		int pi = partition(arr, low, high);
		quicksort(arr, low, pi - 1);
		quicksort(arr, pi + 1, high);
	}
}

int
main(void)
{
	// initialize array
	int buf[SIZE];
	int *numbers = &buf;

	// populate array
	for (int i = 0; i < SIZE; i++) numbers[i] = rand() % 256;

	quicksort(numbers, 0, SIZE - 1);

	// print array
	for (int i = 0; i < SIZE; i++) {
		if (numbers[i] > 128)
			printf("%i:\tabove 128\n", numbers[i]);
		else printf("%i:\tbelow 128\n", numbers[i]);
	}

}
