

	.text
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
	sw	a0, -40(s0)                     # 4-byte Folded Spill
	lw	a0, %lo(hsp)(a0)
	lw	a0, 12(a0)
	call	allocateHeap
	mv	a1, a0
	lw	a0, -40(s0)                     # 4-byte Folded Reload
	lw	a2, %lo(hsp)(a0)
	sw	a1, 4(a2)
	lw	a2, %lo(hsp)(a0)
	lw	a1, 4(a2)
	lw	a3, 12(a2)
	add	a1, a1, a3
	addi	a1, a1, -1
	sw	a1, 8(a2)
	lw	a2, %lo(hsp)(a0)
	lw	a1, 4(a2)
	sw	a1, 0(a2)
	lw	a2, %lo(hsp)(a0)
	lw	a1, 12(a2)
	lw	a2, 0(a2)
	sw	a1, 0(a2)
	lw	a0, %lo(hsp)(a0)
	lw	a1, 0(a0)
	li	a0, 0
	sw	a0, 16(a1)
	j	.LBB0_2
.LBB0_2:
	lw	a0, -16(s0)
	lw	a1, -20(s0)
	mul	a0, a0, a1
	addi	a0, a0, 8
	sw	a0, -24(s0)
	lw	a0, -24(s0)
	addi	a0, a0, 3
	andi	a0, a0, -4
	sw	a0, -24(s0)
	li	a0, 0
	sw	a0, -28(s0)
	lui	a1, %hi(hsp)
	lw	a1, %lo(hsp)(a1)
	lw	a1, 0(a1)
	sw	a1, -32(s0)
	sw	a0, -36(s0)
	j	.LBB0_3
.LBB0_3:                                # =>This Inner Loop Header: Depth=1
	lw	a0, -32(s0)
	beqz	a0, .LBB0_14
	j	.LBB0_4
.LBB0_4:                                #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -32(s0)
	lw	a0, 0(a0)
	sw	a0, -36(s0)
	lw	a1, -36(s0)
	lw	a0, -24(s0)
	addi	a0, a0, 8
	bge	a0, a1, .LBB0_6
	j	.LBB0_5
.LBB0_5:
	lw	a2, -24(s0)
	lw	a1, -32(s0)
	lw	a0, 0(a1)
	sub	a0, a0, a2
	sw	a0, 0(a1)
	lw	a0, -32(s0)
	lw	a1, -36(s0)
	slli	a1, a1, 2
	add	a0, a0, a1
	lw	a1, -24(s0)
	slli	a1, a1, 2
	sub	a0, a0, a1
	sw	a0, -32(s0)
	lw	a0, -24(s0)
	lw	a1, -32(s0)
	sw	a0, 0(a1)
	lw	a0, -16(s0)
	lw	a1, -32(s0)
	sw	a0, 16(a1)
	lw	a0, -32(s0)
	addi	a0, a0, 16
	sw	a0, -12(s0)
	j	.LBB0_15
.LBB0_6:                                #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -36(s0)
	lw	a1, -24(s0)
	blt	a0, a1, .LBB0_11
	j	.LBB0_7
.LBB0_7:
	lw	a0, -28(s0)
	bnez	a0, .LBB0_9
	j	.LBB0_8
.LBB0_8:
	lw	a0, -32(s0)
	lw	a0, 16(a0)
	lui	a1, %hi(hsp)
	lw	a1, %lo(hsp)(a1)
	lw	a1, 0(a1)
	sw	a0, 0(a1)
	j	.LBB0_10
.LBB0_9:
	lw	a0, -32(s0)
	lw	a0, 16(a0)
	lw	a1, -28(s0)
	sw	a0, 16(a1)
	j	.LBB0_10
.LBB0_10:
	lw	a0, -16(s0)
	lw	a1, -32(s0)
	sw	a0, 16(a1)
	lw	a0, -32(s0)
	addi	a0, a0, 16
	sw	a0, -12(s0)
	j	.LBB0_15
.LBB0_11:                               #   in Loop: Header=BB0_3 Depth=1
	lw	a0, -32(s0)
	sw	a0, -28(s0)
	lw	a0, -32(s0)
	lw	a0, 16(a0)
	sw	a0, -32(s0)
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
                                        # -- End function
deallocate:                             # @deallocate
# %bb.0:
	addi	sp, sp, -16
	sw	ra, 12(sp)                      # 4-byte Folded Spill
	sw	s0, 8(sp)                       # 4-byte Folded Spill
	addi	s0, sp, 16
	sw	a0, -12(s0)
	lui	a1, %hi(hsp)
	lw	a0, %lo(hsp)(a1)
	lw	a0, 0(a0)
	lw	a2, -12(s0)
	sw	a0, 0(a2)
	lw	a0, -12(s0)
	addi	a0, a0, -16
	lw	a1, %lo(hsp)(a1)
	sw	a0, 0(a1)
	lw	ra, 12(sp)                      # 4-byte Folded Reload
	lw	s0, 8(sp)                       # 4-byte Folded Reload
	addi	sp, sp, 16
	ret
.Lfunc_end1:
                                        # -- End function
	.data
hs:
	.word	0
	.word	0
	.word	0
	.word	1000                            # 0x3e8

hsp:
	.word	hs

	.text
allocateHeap: #Use sbreak systemcall to allocate heap space
	li 		a7, 9		# Load sbreak syscall into a7
	ecall				# Allocate heap size form a0
#	mv		gp, a0		Should hp be in gp/x3?
	ret  				# Hp returned in a0

printInt:				#Print from a0:
	sw	ra, -4(sp)		# Save ra on stack
	addi 	sp, sp, -4 	# Allocate stack space
	jal  	p.putint	# Call p.putint
	addi	sp, sp 4	# Deallocate sp
	lw   	ra, -4(sp)	# Restore ra
	ret  
