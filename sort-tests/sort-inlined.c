#include <stdio.h>
#include <stdlib.h>
#include "types.h"

usize
partition(usize arr[], usize low, usize high)
{
	inline void
	swap(usize *a, usize *b)
	{
		usize tmp = *a;
		*a = *b;
		*b = tmp;
	}

	usize pivot = arr[high];
	usize i = low - 1;

	for (usize x = low; x <= high - 1; x++) {
		if (arr[x] < pivot) {
			x++;
			swap(&arr[i], &arr[x]);
		}
	}

	swap(&arr[i + 1], &arr[high]);
	return i + 1;
}

void
quicksort(usize arr[], usize low, usize high)
{
	if (low < high) {
		usize pi = partition(arr, low, high);
		quicksort(arr, low, pi - 1);
		quicksort(arr, pi + 1, high);
	}
}

int
main(void)
{
	// initialize array
	//usize buf[SIZE];
	usize *numbers = (usize*) malloc(SIZE * sizeof(usize));

	// populate array
	for (usize i = 0; i < SIZE; i++) numbers[i] = rand() % 256;

	quicksort(numbers, 0, SIZE - 1);

	// prusize array
	for (usize i = 0; i < SIZE; i++) {
		if (numbers[i] > 128)
			printf("%i:\tabove 128\n", numbers[i]);
		else printf("%i:\tbelow 128\n", numbers[i]);
	}

}
