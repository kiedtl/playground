all: asm-hi c-hi go-hi rust-hi

clean:
	rm -f asm-hi c-hi go-hi rust-hi

asm-hi: asm-hi.asm
	nasm asm-hi.asm -o asm-hi

c-hi: c-hi.c
	gcc -O3 c-hi.c -o c-hi

go-hi: go-hi.go
	gcc -O3 go-hi.go -o go-hi

rust-hi: rust-hi.rs
	rustc --opt-level=O3 --lto rust-hi.rs -o rust-hi

.PHONY: all clean
