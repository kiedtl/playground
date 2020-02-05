#include <stdio.h>

const unsigned int max = 512;

unsigned
hash(char *s)
{
	unsigned h;

	h = 42;
	while (*s) h += 11 * h + *s++;
	return h & (max - 1);
}

int
main(int argc, char **argv)
{
	fprintf(stdout, "%i\n", hash(argv[1]));
	return 0;
}

