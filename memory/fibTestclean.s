	.text	0x00400000
	la	x3, d.heap
	jal	f.main
	jal	p.stop

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
                                        # -- End function
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
                                        # -- End function
f.main:

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
                                        # -- End function
printInt:				#Print from a0:
	sw	ra, -4(sp)		# Save ra on stack
	addi 	sp, sp, -4 	# Allocate stack space
	jal  	p.putint	# Call p.putint
	addi	sp, sp 4	# Deallocate sp
	lw   	ra, -4(sp)	# Restore ra
	ret  
p.stop:
	li	x17, 93
	li	x10, 0
	ecall

p.putint:
	li	x17, 1
	ecall
	li	x17, 4
	la	x10, m.space
	ecall
	jr	x1

	.data
m.space:
	.asciz	" "
d.heap:
	.space	100000
