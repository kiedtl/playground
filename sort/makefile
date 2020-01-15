SIZE ?= 500

all: sort nosort

clean:
	rm sort nosort -f

sort: sort.c
	gcc -O3 -DSIZE=$(SIZE) sort.c -o sort

nosort: nosort.c
	gcc -O3 -DSIZE=$(SIZE) nosort.c -o nosort

bench: sort nosort
	hyperfine ./sort ./nosort

.PHONY: all bench
