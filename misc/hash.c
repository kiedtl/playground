/*
 * hash: simple hash function(?)
 */
#include <stdio.h>

unsigned
hash(char *s, int salt, int max)
{
	unsigned h = salt;

	while (*s) h += 11 * h + *s++;
	return h & (max - 1);
}

int
main(int argc, char **argv)
{
	fprintf(stdout, "%i\n", hash(argv[1], 12, 512));
	return 0;
}

