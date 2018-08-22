	.text
	.file	"04conditional-call.c"
	.globl	main                    # -- Begin function main
	.p2align	4, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# %bb.0:
	pushq	%rbp
	.cfi_def_cfa_offset 16
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movabsq	$.L.str, %rdi
	leaq	-8(%rbp), %rsi
	movl	$0, -4(%rbp)
	movb	$0, %al
	callq	__isoc99_scanf
	cmpl	$5, -8(%rbp)
	movl	%eax, -16(%rbp)         # 4-byte Spill
	jle	.LBB0_2
# %bb.1:
	movl	$2, -12(%rbp)
	jmp	.LBB0_3
.LBB0_2:
	movl	$1, -12(%rbp)
.LBB0_3:
	movl	-8(%rbp), %eax
	addl	$1, %eax
	movl	%eax, -8(%rbp)
	cmpl	$2, -12(%rbp)
	jne	.LBB0_5
# %bb.4:
	movabsq	$.L.str.1, %rdi
	movb	$0, %al
	callq	printf
	movl	%eax, -20(%rbp)         # 4-byte Spill
	jmp	.LBB0_6
.LBB0_5:
	movl	-12(%rbp), %eax
	addl	-8(%rbp), %eax
	movl	%eax, -8(%rbp)
.LBB0_6:
	xorl	%eax, %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Lfunc_end0:
	.size	main, .Lfunc_end0-main
	.cfi_endproc
                                        # -- End function
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%d"
	.size	.L.str, 3

	.type	.L.str.1,@object        # @.str.1
.L.str.1:
	.asciz	"Dependent Int = 2"
	.size	.L.str.1, 18


	.ident	"clang version 6.0.0 (tags/RELEASE_600/final)"
	.section	".note.GNU-stack","",@progbits
