SIZE ?= 65535

all: sort-c sort-v #nosort

clean:
	rm -f sort #nosort

sort-c: sort-c.c
	gcc -O4 -DSIZE=$(SIZE) sort-c.c -o sort-c

sort-v: sort-v.v
	v -prod sort-v.v

#nosort: nosort.c
#	gcc -O3 -DSIZE=$(SIZE) nosort.c -o nosort

bench: sort-c sort-v #nosort
	hyperfine ./sort-c ./sort-v #./nosort

.PHONY: all bench
