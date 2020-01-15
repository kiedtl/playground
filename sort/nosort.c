#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
	// initialize array
	int *numbers = (int*) malloc(SIZE * sizeof(int));

	// populate array
	for (int i = 0; i < SIZE; i++) numbers[i] = rand() % 256;

	// print array
	for (int i = 0; i < SIZE; i++) {
		if (numbers[i] > 128)
			printf("%i:\tabove 128\n", numbers[i]);
		else printf("%i:\tbelow 128\n", numbers[i]);
	}

}
