#include <stdio.h>
#include <stdlib.h>

int
main(void)
{
	// initialize array
	int *numbers = (int*) malloc(10000 * sizeof(int));

	// populate array
	for (int i = 0; i < 10000; i++) numbers[i] = rand();

	// print array
	for (int i = 0; i < 10000; i++) {
		if (numbers[i] > 500000000)
			printf("%i:\tabove 500000000\n", numbers[i]);
		else printf("%i:\tbelow 500000000\n", numbers[i]);
	}

}
