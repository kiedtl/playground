	.arch armv6
	.eabi_attribute 28, 1
	.eabi_attribute 20, 1
	.eabi_attribute 21, 1
	.eabi_attribute 23, 3
	.eabi_attribute 24, 1
	.eabi_attribute 25, 1
	.eabi_attribute 26, 2
	.eabi_attribute 30, 6
	.eabi_attribute 34, 1
	.eabi_attribute 18, 4
	.file	"hash.c"
	.text
	.align	2
	.global	hash
	.arch armv6
	.syntax unified
	.arm
	.fpu vfp
	.type	hash, %function
hash:
	@ args = 0, pretend = 0, frame = 24
	@ frame_needed = 1, uses_anonymous_args = 0
	@ link register save eliminated.
	str	fp, [sp, #-4]!
	add	fp, sp, #0
	sub	sp, sp, #28
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	str	r2, [fp, #-24]
	ldr	r3, [fp, #-20]
	str	r3, [fp, #-8]
	b	.L2
.L3:
	ldr	r2, [fp, #-8]
	mov	r3, r2
	lsl	r3, r3, #2
	add	r3, r3, r2
	lsl	r3, r3, #1
	add	r2, r3, r2
	ldr	r3, [fp, #-16]
	add	r1, r3, #1
	str	r1, [fp, #-16]
	ldrb	r3, [r3]	@ zero_extendqisi2
	add	r3, r2, r3
	ldr	r2, [fp, #-8]
	add	r3, r2, r3
	str	r3, [fp, #-8]
.L2:
	ldr	r3, [fp, #-16]
	ldrb	r3, [r3]	@ zero_extendqisi2
	cmp	r3, #0
	bne	.L3
	ldr	r3, [fp, #-24]
	sub	r3, r3, #1
	mov	r2, r3
	ldr	r3, [fp, #-8]
	and	r3, r3, r2
	mov	r0, r3
	add	sp, fp, #0
	@ sp needed
	ldr	fp, [sp], #4
	bx	lr
	.size	hash, .-hash
	.section	.rodata
	.align	2
.LC0:
	.ascii	"%i\012\000"
	.text
	.align	2
	.global	main
	.syntax unified
	.arm
	.fpu vfp
	.type	main, %function
main:
	@ args = 0, pretend = 0, frame = 8
	@ frame_needed = 1, uses_anonymous_args = 0
	push	{r4, fp, lr}
	add	fp, sp, #8
	sub	sp, sp, #12
	str	r0, [fp, #-16]
	str	r1, [fp, #-20]
	ldr	r2, .L7
.LPIC0:
	add	r2, pc, r2
	ldr	r3, .L7+4
	ldr	r3, [r2, r3]
	ldr	r4, [r3]
	ldr	r3, [fp, #-20]
	add	r3, r3, #4
	ldr	r3, [r3]
	mov	r2, #512
	mov	r1, #12
	mov	r0, r3
	bl	hash(PLT)
	mov	r3, r0
	mov	r2, r3
	ldr	r3, .L7+8
.LPIC1:
	add	r3, pc, r3
	mov	r1, r3
	mov	r0, r4
	bl	fprintf(PLT)
	mov	r3, #0
	mov	r0, r3
	sub	sp, fp, #8
	@ sp needed
	pop	{r4, fp, pc}
.L8:
	.align	2
.L7:
	.word	_GLOBAL_OFFSET_TABLE_-(.LPIC0+8)
	.word	stdout(GOT)
	.word	.LC0-(.LPIC1+8)
	.size	main, .-main
	.ident	"GCC: (GNU) 9.2.0"
	.section	.note.GNU-stack,"",%progbits
