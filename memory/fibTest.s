	.text
	.attribute	4, 16
	.attribute	5, "rv32i2p1_m2p0_a2p1_c2p0_zmmul1p0"
	.file	"fibTest.c"
	.globl	fibRec                          # -- Begin function fibRec
	.p2align	1
	.type	fibRec,@function
fibRec:                                 # @fibRec
# %bb.0:
	addi	sp, sp, -32
	sw	ra, 28(sp)                      # 4-byte Folded Spill
	sw	s0, 24(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 32
	sw	a0, -16(s0)
	lw	a1, -16(s0)
	li	a0, 0
	blt	a0, a1, .LBB0_2
	j	.LBB0_1
.LBB0_1:
	li	a0, 0
	sw	a0, -12(s0)
	j	.LBB0_5
.LBB0_2:
	lw	a0, -16(s0)
	li	a1, 1
	bne	a0, a1, .LBB0_4
	j	.LBB0_3
.LBB0_3:
	li	a0, 1
	sw	a0, -12(s0)
	j	.LBB0_5
.LBB0_4:
	lw	a0, -16(s0)
	addi	a0, a0, -1
	call	fibRec
	sw	a0, -20(s0)                     # 4-byte Folded Spill
	lw	a0, -16(s0)
	addi	a0, a0, -2
	call	fibRec
	mv	a1, a0
	lw	a0, -20(s0)                     # 4-byte Folded Reload
	add	a0, a0, a1
	sw	a0, -12(s0)
	j	.LBB0_5
.LBB0_5:
	lw	a0, -12(s0)
	lw	ra, 28(sp)                      # 4-byte Folded Reload
	lw	s0, 24(sp)                      # 4-byte Folded Reload
	addi	sp, sp, 32
	ret
.Lfunc_end0:
	.size	fibRec, .Lfunc_end0-fibRec
                                        # -- End function
	.globl	fibIter                         # -- Begin function fibIter
	.p2align	1
	.type	fibIter,@function
fibIter:                                # @fibIter
# %bb.0:
	addi	sp, sp, -32
	sw	ra, 28(sp)                      # 4-byte Folded Spill
	sw	s0, 24(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 32
	sw	a0, -16(s0)
	lw	a1, -16(s0)
	li	a0, 0
	blt	a0, a1, .LBB1_2
	j	.LBB1_1
.LBB1_1:
	li	a0, 0
	sw	a0, -12(s0)
	j	.LBB1_9
.LBB1_2:
	lw	a0, -16(s0)
	li	a1, 1
	bne	a0, a1, .LBB1_4
	j	.LBB1_3
.LBB1_3:
	li	a0, 1
	sw	a0, -12(s0)
	j	.LBB1_9
.LBB1_4:
	li	a0, 0
	sw	a0, -20(s0)
	li	a0, 1
	sw	a0, -24(s0)
	li	a0, 2
	sw	a0, -32(s0)
	j	.LBB1_5
.LBB1_5:                                # =>This Inner Loop Header: Depth=1
	lw	a1, -32(s0)
	lw	a0, -16(s0)
	blt	a0, a1, .LBB1_8
	j	.LBB1_6
.LBB1_6:                                #   in Loop: Header=BB1_5 Depth=1
	lw	a0, -20(s0)
	lw	a1, -24(s0)
	add	a0, a0, a1
	sw	a0, -28(s0)
	lw	a0, -24(s0)
	sw	a0, -20(s0)
	lw	a0, -28(s0)
	sw	a0, -24(s0)
	j	.LBB1_7
.LBB1_7:                                #   in Loop: Header=BB1_5 Depth=1
	lw	a0, -32(s0)
	addi	a0, a0, 1
	sw	a0, -32(s0)
	j	.LBB1_5
.LBB1_8:
	lw	a0, -24(s0)
	sw	a0, -12(s0)
	j	.LBB1_9
.LBB1_9:
	lw	a0, -12(s0)
	lw	ra, 28(sp)                      # 4-byte Folded Reload
	lw	s0, 24(sp)                      # 4-byte Folded Reload
	addi	sp, sp, 32
	ret
.Lfunc_end1:
	.size	fibIter, .Lfunc_end1-fibIter
                                        # -- End function
	.globl	main                            # -- Begin function main
	.p2align	1
	.type	main,@function
main:                                   # @main
# %bb.0:
	addi	sp, sp, -32
	sw	ra, 28(sp)                      # 4-byte Folded Spill
	sw	s0, 24(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 32
	li	a0, 0
	sw	a0, -20(s0)                     # 4-byte Folded Spill
	sw	a0, -12(s0)
	li	a0, 25
	sw	a0, -16(s0)
	lw	a0, -16(s0)
	call	fibRec
	call	printInt
	lw	a0, -16(s0)
	call	fibIter
	call	printInt
	lw	a0, -20(s0)                     # 4-byte Folded Reload
	lw	ra, 28(sp)                      # 4-byte Folded Reload
	lw	s0, 24(sp)                      # 4-byte Folded Reload
	addi	sp, sp, 32
	ret
.Lfunc_end2:
	.size	main, .Lfunc_end2-main
                                        # -- End function
	.ident	"Homebrew clang version 19.1.7"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym fibRec
	.addrsig_sym fibIter
	.addrsig_sym printInt
