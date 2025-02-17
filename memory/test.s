	.text
#	.attribute	4, 16
#	.attribute	5, "rv32i2p1_m2p0_a2p1_c2p0_zmmul1p0"
#	.file	"test.c"
	.globl	main                            # -- Begin function main
#	.p2align	1
#	.type	main,@function
main:                                   # @main
# %bb.0:
	addi	sp, sp, -16
	sw	ra, 12(sp)                      # 4-byte Folded Spill
	sw	s0, 8(sp)                       # 4-byte Folded Spill
	addi	s0, sp, 16
	li	a0, 0
	sw	a0, -12(s0)
	lw	ra, 12(sp)                      # 4-byte Folded Reload
	lw	s0, 8(sp)                       # 4-byte Folded Reload
	addi	sp, sp, 16
	ret
.Lfunc_end0:
#	.size	main, .Lfunc_end0-main
                                        # -- End function
#	.ident	"Homebrew clang version 19.1.7"
#	.section	".note.GNU-stack","",@progbits
#	.addrsig
