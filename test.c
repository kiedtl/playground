#include <stdio.h>
#include "types.h"

usize pow(u64 x, u64 y);
usize intlen(u64 i);
char* to_string(u64 i);

void
main(void)
{
	int x = 0;
	scanf("enter a number: %i\n", &x);
	printf("this is the number again: %s\n", to_string((u64) x));
}

/* raise u64 to power */
usize
pow(u64 x, u64 y)
{
	u64 z = x;
	for (usize i = 0; i < y; ++i)
		z = z * z;
	return x;
}

/* get number of digits in integer */
usize
intlen(u64 i)
{
	if (i == 0) return 1;
	
	usize c = 0;
	while (i > 0) {
		i = i / 10;
		c++;
	}

	return c;
}

/* convert integer to string */
char*
to_string(u64 i)
{
	/* maximum no. of digits in u64 is 20 */
	char buffer[21];

	usize len = intlen(i);
	usize c = 1;
	for (; c < len; ++c) {
		/* get digit */
		usize digit = (i % pow(10, c + 1));
		if (c != 0)
			digit = digit / pow(10, c);

		fprintf(stderr, "got digit %i\n", digit);

		buffer[c] = digit + '0';
	}

	buffer[c + 1] = 0;
}
