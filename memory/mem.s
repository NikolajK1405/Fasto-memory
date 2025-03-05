	.text
	.attribute	4, 16
	.attribute	5, "rv32i2p1_m2p0_a2p1_c2p0_zmmul1p0"
	.file	"mem.c"
	.globl	allocate                        # -- Begin function allocate
	.p2align	1
	.type	allocate,@function
allocate:                               # @allocate
# %bb.0:
	addi	sp, sp, -32
	sw	ra, 28(sp)                      # 4-byte Folded Spill
	sw	s0, 24(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 32
	sw	a0, -16(s0)
	sw	a1, -20(s0)
	lui	a0, %hi(heap)
	lw	a0, %lo(heap)(a0)
	bnez	a0, .LBB0_2
	j	.LBB0_1
.LBB0_1:
	lui	a0, %hi(heapSize)
	lw	a0, %lo(heapSize)(a0)
	call	allocateHeap
	mv	a1, a0
	lui	a0, %hi(heap)
	sw	a1, %lo(heap)(a0)
	lw	a0, %lo(heap)(a0)
	lui	a1, %hi(hp)
	sw	a0, %lo(hp)(a1)
	j	.LBB0_2
.LBB0_2:
	lui	a0, %hi(hp)
	lw	a1, %lo(hp)(a0)
	sw	a1, -24(s0)
	lw	a1, -16(s0)
	lw	a2, -20(s0)
	mul	a1, a1, a2
	lw	a2, %lo(hp)(a0)
	add	a1, a1, a2
	addi	a1, a1, 4
	sw	a1, %lo(hp)(a0)
	lw	a1, -16(s0)
	lw	a2, -24(s0)
	sw	a1, 0(a2)
	lw	a1, %lo(hp)(a0)
	lui	a0, %hi(heapSize)
	lw	a2, %lo(heapSize)(a0)
	lui	a0, %hi(heap)
	lw	a0, %lo(heap)(a0)
	add	a0, a0, a2
	bgeu	a0, a1, .LBB0_4
	j	.LBB0_3
.LBB0_3:
	li	a0, 123
	call	printInt
	li	a0, 0
	sw	a0, -12(s0)
	j	.LBB0_5
.LBB0_4:
	lw	a0, -24(s0)
	sw	a0, -12(s0)
	j	.LBB0_5
.LBB0_5:
	lw	a0, -12(s0)
	lw	ra, 28(sp)                      # 4-byte Folded Reload
	lw	s0, 24(sp)                      # 4-byte Folded Reload
	addi	sp, sp, 32
	ret
.Lfunc_end0:
	.size	allocate, .Lfunc_end0-allocate
                                        # -- End function
	.type	heap,@object                    # @heap
	.section	.sbss,"aw",@nobits
	.p2align	2, 0x0
heap:
	.word	0
	.size	heap, 4

	.type	heapSize,@object                # @heapSize
	.section	.sdata,"aw",@progbits
	.p2align	2, 0x0
heapSize:
	.word	200                             # 0xc8
	.size	heapSize, 4

	.type	hp,@object                      # @hp
	.section	.sbss,"aw",@nobits
	.p2align	2, 0x0
hp:
	.word	0
	.size	hp, 4

	.ident	"Homebrew clang version 19.1.7"
	.section	".note.GNU-stack","",@progbits
	.addrsig
	.addrsig_sym allocateHeap
	.addrsig_sym printInt
	.addrsig_sym heap
	.addrsig_sym heapSize
	.addrsig_sym hp
