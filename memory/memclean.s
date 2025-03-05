allocate:                               # @allocate
# %bb.0:
	addi	sp, sp, -32
	sw	ra, 28(sp)                      # 4-byte Folded Spill
	sw	s0, 24(sp)                      # 4-byte Folded Spill
	addi	s0, sp, 32
	sw	a0, -16(s0)
	sw	a1, -20(s0)
	la	a0, heap
	lw	a0, 0(a0)
	bnez	a0, .LBB0_2
	j	.LBB0_1
.LBB0_1:
	la	a0, heapSize
	lw	a0, 0(a0)
	call	allocateHeap
	mv	a1, a0
	la	a0, heap
	sw	a1, 0(a0)
	lw	a0, 0(a0)
	la	a1, hp
	sw	a0, 0(a1)
	j	.LBB0_2
.LBB0_2:
	la	a0, hp
	lw	a1, 0(a0)
	sw	a1, -24(s0)
	lw	a1, -16(s0)
	lw	a2, -20(s0)
	mul	a1, a1, a2
	lw	a2, 0(a0)
	add	a1, a1, a2
	addi	a1, a1, 4
	sw	a1, 0(a0)
	lw	a1, -16(s0)
	lw	a2, -24(s0)
	sw	a1, 0(a2)
	lw	a1, 0(a0)
	la	a0, heapSize
	lw	a2, 0(a0)
	la	a0, heap
	lw	a0, 0(a0)
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
                                        # -- End function
heap:
	.word	0

heapSize:
	.word	200                             # 0xc8

hp:
	.word	0

