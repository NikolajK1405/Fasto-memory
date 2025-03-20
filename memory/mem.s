	.text
	.attribute	4, 16
	.attribute	5, "rv32i2p1_m2p0_a2p1_c2p0_zmmul1p0"
	.file	"mem.c"
	.globl	allocate                        # -- Begin function allocate
	.p2align	1
	.type	allocate,@function
allocate:                               # @allocate
# %bb.0:
	addi	sp, sp, -48
	sw	ra, 44(sp)                      # 4-byte Folded Spill
	sw	s0, 40(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 48
	sw	a0, -16(s0)
	sw	a1, -20(s0)
	lui	a0, %hi(hsp)
	lw	a0, %lo(hsp)(a0)
	lw	a0, 4(a0)
	bnez	a0, .LBB0_2
	j	.LBB0_1
.LBB0_1:
	lui	a0, %hi(hsp)
	sw	a0, -44(s0)                     # 4-byte Folded Spill
	lw	a0, %lo(hsp)(a0)
	lw	a0, 12(a0)
	call	allocateHeap
	lw	a1, -44(s0)                     # 4-byte Folded Reload
	lw	a2, %lo(hsp)(a1)
	sw	a0, 4(a2)
	lw	a2, %lo(hsp)(a1)
	lw	a0, 4(a2)
	lw	a3, 12(a2)
	add	a0, a0, a3
	addi	a0, a0, -1
	sw	a0, 8(a2)
	lw	a0, %lo(hsp)(a1)
	lw	a0, 4(a0)
	sw	a0, -24(s0)
	lw	a0, %lo(hsp)(a1)
	lw	a0, 12(a0)
	lw	a2, -24(s0)
	sw	a0, 0(a2)
	lw	a2, -24(s0)
	li	a0, 0
	sw	a0, 4(a2)
	lw	a0, -24(s0)
	lw	a1, %lo(hsp)(a1)
	sw	a0, 0(a1)
	j	.LBB0_2
.LBB0_2:
	lw	a0, -16(s0)
	lw	a1, -20(s0)
	mul	a0, a0, a1
	addi	a0, a0, 8
	sw	a0, -28(s0)
	lw	a0, -28(s0)
	addi	a0, a0, 3
	andi	a0, a0, -4
	sw	a0, -28(s0)
	li	a0, 0
	sw	a0, -32(s0)
	lui	a1, %hi(hsp)
	lw	a1, %lo(hsp)(a1)
	lw	a1, 0(a1)
	sw	a1, -36(s0)
	sw	a0, -40(s0)
	j	.LBB0_3
.LBB0_3:                                # =>This Inner Loop Header: Depth=1
	lw	a0, -36(s0)
	beqz	a0, .LBB0_14
	j	.LBB0_4
.LBB0_4:                                #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -36(s0)
	lw	a0, 0(a0)
	sw	a0, -40(s0)
	lw	a1, -40(s0)
	lw	a0, -28(s0)
	addi	a0, a0, 8
	bge	a0, a1, .LBB0_6
	j	.LBB0_5
.LBB0_5:
	lw	a2, -28(s0)
	lw	a1, -36(s0)
	lw	a0, 0(a1)
	sub	a0, a0, a2
	sw	a0, 0(a1)
	lw	a0, -36(s0)
	lw	a1, 0(a0)
	add	a0, a0, a1
	sw	a0, -36(s0)
	lw	a0, -28(s0)
	lw	a1, -36(s0)
	sw	a0, 0(a1)
	lw	a0, -16(s0)
	lw	a1, -36(s0)
	sw	a0, 4(a1)
	lw	a0, -36(s0)
	addi	a0, a0, 4
	sw	a0, -12(s0)
	j	.LBB0_15
.LBB0_6:                                #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -40(s0)
	lw	a1, -28(s0)
	blt	a0, a1, .LBB0_11
	j	.LBB0_7
.LBB0_7:
	lw	a0, -32(s0)
	bnez	a0, .LBB0_9
	j	.LBB0_8
.LBB0_8:
	lw	a0, -36(s0)
	lw	a0, 4(a0)
	lui	a1, %hi(hsp)
	lw	a1, %lo(hsp)(a1)
	sw	a0, 0(a1)
	j	.LBB0_10
.LBB0_9:
	lw	a0, -36(s0)
	lw	a0, 4(a0)
	lw	a1, -32(s0)
	sw	a0, 4(a1)
	j	.LBB0_10
.LBB0_10:
	lw	a0, -16(s0)
	lw	a1, -36(s0)
	sw	a0, 4(a1)
	lw	a0, -36(s0)
	addi	a0, a0, 4
	sw	a0, -12(s0)
	j	.LBB0_15
.LBB0_11:                               #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -36(s0)
	sw	a0, -32(s0)
	lw	a0, -36(s0)
	lw	a0, 4(a0)
	sw	a0, -36(s0)
	j	.LBB0_12
.LBB0_12:                               #   in Loop: Header=BB0_3 Depth=1
	j	.LBB0_13
.LBB0_13:                               #   in Loop: Header=BB0_3 Depth=1
	j	.LBB0_3
.LBB0_14:
	li	a0, 0
	sw	a0, -12(s0)
	j	.LBB0_15
.LBB0_15:
	lw	a0, -12(s0)
	lw	ra, 44(sp)                      # 4-byte Folded Reload
	lw	s0, 40(sp)                      # 4-byte Folded Reload
	addi	sp, sp, 48
	ret
.Lfunc_end0:
	.size	allocate, .Lfunc_end0-allocate
                                        # -- End function
	.globl	deallocate                      # -- Begin function deallocate
	.p2align	1
	.type	deallocate,@function
deallocate:                             # @deallocate
# %bb.0:
	addi	sp, sp, -16
	sw	ra, 12(sp)                      # 4-byte Folded Spill
	sw	s0, 8(sp)                       # 4-byte Folded Spill
	addi	s0, sp, 16
	sw	a0, -12(s0)
	lw	a0, -12(s0)
	addi	a0, a0, -4
	sw	a0, -16(s0)
	lui	a1, %hi(hsp)
	lw	a0, %lo(hsp)(a1)
	lw	a0, 0(a0)
	lw	a2, -16(s0)
	sw	a0, 4(a2)
	lw	a0, -16(s0)
	lw	a1, %lo(hsp)(a1)
	sw	a0, 0(a1)
	lw	ra, 12(sp)                      # 4-byte Folded Reload
	lw	s0, 8(sp)                       # 4-byte Folded Reload
	addi	sp, sp, 16
	ret
.Lfunc_end1:
	.size	deallocate, .Lfunc_end1-deallocate
                                        # -- End function
	.type	hs,@object                      # @hs
	.data
	.p2align	2, 0x0
hs:
	.word	0
	.word	0
	.word	0
	.word	1000                            # 0x3e8
	.size	hs, 16

	.type	hsp,@object                     # @hsp
	.section	.sdata,"aw",@progbits
	.globl	hsp
	.p2align	2, 0x0
hsp:
	.word	hs
	.size	hsp, 4

	.ident	"Homebrew clang version 19.1.7"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym allocateHeap
	.addrsig_sym hs
	.addrsig_sym hsp
