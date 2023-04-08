	.text
	.file	"pair.ll"
	.p2align	4, 0x90                         # -- Begin function car
	.type	.Lcar,@function
.Lcar:                                  # @car
	.cfi_startproc
# %bb.0:
	movabsq	$72057594037927936, %rax        # imm = 0x100000000000000
	andq	%rdi, %rax
	retq
.Lfunc_end0:
	.size	.Lcar, .Lfunc_end0-.Lcar
	.cfi_endproc
                                        # -- End function
	.section	".note.GNU-stack","",@progbits
