#include <stdio.h>

int
main(void)
{
	int lower = 0; // FALSE
	int in = 1234567890;
	int ex = in;
	char buf[50]; /* i never know how much to allocate */
	char *out = &buf[49];

	for (; in; in >>= 3) {
		*--out = '0' + (in & 7);
	}

	printf("integer: '%i'\n", ex);
	printf("calculated answer: '%s'\n", out);
	printf("musl's answer: %o\n", ex);
	return 0;
}
