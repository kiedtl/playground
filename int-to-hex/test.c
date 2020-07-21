#include <stdio.h>

int
main(void)
{
	char *xdigits = "0123456789ABCDEF";

	int lower = 0; // FALSE
	int in = 1234567890;
	int ex = in;
	char buf[10];
	char *out = &buf[9];

	for (; in; in >>= 4) {
		*--out = xdigits[(in & 15)] | lower;
	}

	printf("integer: '%i'\n", ex);
	printf("calculated answer: '%s'\n", out);
	printf("musl's answer: %X\n", ex);
	return 0;
}
