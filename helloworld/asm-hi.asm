global _start

section .text

_start:
	mov rax, 1
	mov rdi, 1
	mov rsi, str
	mov rdx, strlen
	syscall

	mov rax, 60
	mov rdi, 0
	syscall

section .rodata
	str: db "Hello, world!", 10
	strlen: equ $ - str
