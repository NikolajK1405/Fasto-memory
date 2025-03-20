	.text	0x00400000
	la	x3, d.heap
	jal	f.main
	jal	p.stop
# User functions
# Function main
f.main:
	sw	x1, -4(x2)
	sw	x19, -12(x2)
	sw	x18, -8(x2)
	addi	x2, x2, -12
	li	x18, 7
# was:	li	_size_3_, 7
	bge	x18, x0, l.safe_4_
# was:	bge	_size_3_, x0, l.safe_4_
	li	x10, 2
# was:	li	x10, 2
	la	x11, m.BadSize
# was:	la	x11, m.BadSize
	j	p.RuntimeError
l.safe_4_:
	mv	x10, x18
# was:	mv	x10, _size_3_
	li	x11, 4
# was:	li	x11, 4
	sw	x1, -4(x2)
# was:	sw	x1, -4(x2)
	sw	x31, -16(x2)
# was:	sw	x31, -16(x2)
	sw	x30, -12(x2)
# was:	sw	x30, -12(x2)
	sw	x29, -8(x2)
# was:	sw	x29, -8(x2)
	sw	x28, -4(x2)
# was:	sw	x28, -4(x2)
	addi	x2, x2, -20
# was:	addi	x2, x2, -20
	jal	allocate
# was:	jal	allocate, x10 x11
	addi	x2, x2, 20
# was:	addi	x2, x2, 20
	lw	x31, -16(x2)
# was:	lw	x31, -16(x2)
	lw	x30, -12(x2)
# was:	lw	x30, -12(x2)
	lw	x29, -8(x2)
# was:	lw	x29, -8(x2)
	lw	x28, -4(x2)
# was:	lw	x28, -4(x2)
	lw	x1, -4(x2)
# was:	lw	x1, -4(x2)
	mv	x19, x10
# was:	mv	_let_a_2_, x10
	addi	x11, x19, 4
# was:	addi	_addr_5_, _let_a_2_, 4
	mv	x10, x0
# was:	mv	_i_6_, x0
l.loop_beg_7_:
	bge	x10, x18, l.loop_end_8_
# was:	bge	_i_6_, _size_3_, l.loop_end_8_
	sw	x10, 0(x11)
# was:	sw	_i_6_, 0(_addr_5_)
	addi	x11, x11, 4
# was:	addi	_addr_5_, _addr_5_, 4
	addi	x10, x10, 1
# was:	addi	_i_6_, _i_6_, 1
	j	l.loop_beg_7_
l.loop_end_8_:
	li	x11, 0
# was:	li	_arr_ind_11_, 0
	addi	x10, x19, 4
# was:	addi	_arr_data_12_, _let_a_2_, 4
	bge	x11, x0, l.nonneg_15_
# was:	bge	_arr_ind_11_, x0, l.nonneg_15_
l.error_14_:
	li	x10, 3
# was:	li	x10, 3
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_15_:
	lw	x12, 0(x19)
# was:	lw	_size_13_, 0(_let_a_2_)
	bge	x11, x12, l.error_14_
# was:	bge	_arr_ind_11_, _size_13_, l.error_14_
	slli	x11, x11, 2
# was:	slli	_arr_ind_11_, _arr_ind_11_, 2
	add	x10, x10, x11
# was:	add	_arr_data_12_, _arr_data_12_, _arr_ind_11_
	lw	x10, 0(x10)
# was:	lw	_tmp_10_, 0(_arr_data_12_)
# 	mv	_let_tmp_9_,_tmp_10_
# 	mv	x10,_let_tmp_9_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x10, 1
# was:	li	_arr_ind_18_, 1
	addi	x11, x19, 4
# was:	addi	_arr_data_19_, _let_a_2_, 4
	bge	x10, x0, l.nonneg_22_
# was:	bge	_arr_ind_18_, x0, l.nonneg_22_
l.error_21_:
	li	x10, 4
# was:	li	x10, 4
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_22_:
	lw	x12, 0(x19)
# was:	lw	_size_20_, 0(_let_a_2_)
	bge	x10, x12, l.error_21_
# was:	bge	_arr_ind_18_, _size_20_, l.error_21_
	slli	x10, x10, 2
# was:	slli	_arr_ind_18_, _arr_ind_18_, 2
	add	x11, x11, x10
# was:	add	_arr_data_19_, _arr_data_19_, _arr_ind_18_
	lw	x10, 0(x11)
# was:	lw	_tmp_17_, 0(_arr_data_19_)
# 	mv	_let_tmp_16_,_tmp_17_
# 	mv	x10,_let_tmp_16_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x11, 2
# was:	li	_arr_ind_25_, 2
	addi	x10, x19, 4
# was:	addi	_arr_data_26_, _let_a_2_, 4
	bge	x11, x0, l.nonneg_29_
# was:	bge	_arr_ind_25_, x0, l.nonneg_29_
l.error_28_:
	li	x10, 5
# was:	li	x10, 5
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_29_:
	lw	x12, 0(x19)
# was:	lw	_size_27_, 0(_let_a_2_)
	bge	x11, x12, l.error_28_
# was:	bge	_arr_ind_25_, _size_27_, l.error_28_
	slli	x11, x11, 2
# was:	slli	_arr_ind_25_, _arr_ind_25_, 2
	add	x10, x10, x11
# was:	add	_arr_data_26_, _arr_data_26_, _arr_ind_25_
	lw	x10, 0(x10)
# was:	lw	_tmp_24_, 0(_arr_data_26_)
# 	mv	_let_tmp_23_,_tmp_24_
# 	mv	x10,_let_tmp_23_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x10, 3
# was:	li	_arr_ind_32_, 3
	addi	x11, x19, 4
# was:	addi	_arr_data_33_, _let_a_2_, 4
	bge	x10, x0, l.nonneg_36_
# was:	bge	_arr_ind_32_, x0, l.nonneg_36_
l.error_35_:
	li	x10, 6
# was:	li	x10, 6
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_36_:
	lw	x12, 0(x19)
# was:	lw	_size_34_, 0(_let_a_2_)
	bge	x10, x12, l.error_35_
# was:	bge	_arr_ind_32_, _size_34_, l.error_35_
	slli	x10, x10, 2
# was:	slli	_arr_ind_32_, _arr_ind_32_, 2
	add	x11, x11, x10
# was:	add	_arr_data_33_, _arr_data_33_, _arr_ind_32_
	lw	x10, 0(x11)
# was:	lw	_tmp_31_, 0(_arr_data_33_)
# 	mv	_let_tmp_30_,_tmp_31_
# 	mv	x10,_let_tmp_30_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x11, 4
# was:	li	_arr_ind_39_, 4
	addi	x12, x19, 4
# was:	addi	_arr_data_40_, _let_a_2_, 4
	bge	x11, x0, l.nonneg_43_
# was:	bge	_arr_ind_39_, x0, l.nonneg_43_
l.error_42_:
	li	x10, 7
# was:	li	x10, 7
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_43_:
	lw	x10, 0(x19)
# was:	lw	_size_41_, 0(_let_a_2_)
	bge	x11, x10, l.error_42_
# was:	bge	_arr_ind_39_, _size_41_, l.error_42_
	slli	x11, x11, 2
# was:	slli	_arr_ind_39_, _arr_ind_39_, 2
	add	x12, x12, x11
# was:	add	_arr_data_40_, _arr_data_40_, _arr_ind_39_
	lw	x10, 0(x12)
# was:	lw	_tmp_38_, 0(_arr_data_40_)
# 	mv	_let_tmp_37_,_tmp_38_
# 	mv	x10,_let_tmp_37_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x11, 5
# was:	li	_arr_ind_46_, 5
	addi	x12, x19, 4
# was:	addi	_arr_data_47_, _let_a_2_, 4
	bge	x11, x0, l.nonneg_50_
# was:	bge	_arr_ind_46_, x0, l.nonneg_50_
l.error_49_:
	li	x10, 8
# was:	li	x10, 8
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_50_:
	lw	x10, 0(x19)
# was:	lw	_size_48_, 0(_let_a_2_)
	bge	x11, x10, l.error_49_
# was:	bge	_arr_ind_46_, _size_48_, l.error_49_
	slli	x11, x11, 2
# was:	slli	_arr_ind_46_, _arr_ind_46_, 2
	add	x12, x12, x11
# was:	add	_arr_data_47_, _arr_data_47_, _arr_ind_46_
	lw	x10, 0(x12)
# was:	lw	_tmp_45_, 0(_arr_data_47_)
# 	mv	_let_tmp_44_,_tmp_45_
# 	mv	x10,_let_tmp_44_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x11, 6
# was:	li	_arr_ind_53_, 6
	addi	x10, x19, 4
# was:	addi	_arr_data_54_, _let_a_2_, 4
	bge	x11, x0, l.nonneg_57_
# was:	bge	_arr_ind_53_, x0, l.nonneg_57_
l.error_56_:
	li	x10, 9
# was:	li	x10, 9
	la	x11, m.BadIndex
# was:	la	x11, m.BadIndex
	j	p.RuntimeError
l.nonneg_57_:
	lw	x12, 0(x19)
# was:	lw	_size_55_, 0(_let_a_2_)
	bge	x11, x12, l.error_56_
# was:	bge	_arr_ind_53_, _size_55_, l.error_56_
	slli	x11, x11, 2
# was:	slli	_arr_ind_53_, _arr_ind_53_, 2
	add	x10, x10, x11
# was:	add	_arr_data_54_, _arr_data_54_, _arr_ind_53_
	lw	x10, 0(x10)
# was:	lw	_tmp_52_, 0(_arr_data_54_)
# 	mv	_let_tmp_51_,_tmp_52_
# 	mv	x10,_let_tmp_51_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x10, 0
# was:	li	_mainres_1_, 0
# 	mv	x10,_mainres_1_
	addi	x2, x2, 12
	lw	x19, -12(x2)
	lw	x18, -8(x2)
	lw	x1, -4(x2)
	jr	x1
# Library functions in Fasto namespace
f.ord:
	mv	x10, x10
	jr	x1
f.chr:
	andi	x10, x10, 255
	jr	x1
# Internal procedures (for syscalls, etc.)
p.putint:
	li	x17, 1
	ecall
	li	x17, 4
	la	x10, m.space
	ecall
	jr	x1
p.getint:
	li	x17, 5
	ecall
	jr	x1
p.putchar:
	li	x17, 11
	ecall
	li	x17, 4
	la	x10, m.space
	ecall
	jr	x1
p.getchar:
	li	x17, 12
	ecall
	jr	x1
p.putstring:
	lw	x7, 0(x10)
	addi	x6, x10, 4
	add	x7, x6, x7
	li	x17, 11
l.ps_begin:
	bge	x6, x7, l.ps_done
	lbu	x10, 0(x6)
	ecall
	addi	x6, x6, 1
	j	l.ps_begin
l.ps_done:
	jr	x1
p.stop:
	li	x17, 93
	li	x10, 0
	ecall
p.RuntimeError:
	mv	x6, x10
	li	x17, 4
	la	x10, m.RunErr
	ecall
	li	x17, 1
	mv	x10, x6
	ecall
	li	x17, 4
	la	x10, m.colon_space
	ecall
	mv	x10, x11
	ecall
	la	x10, m.nl
	ecall
	jal	p.stop
	.data	
# Fixed strings for runtime I/O
m.RunErr:
	.asciz	"Runtime error at line "
m.colon_space:
	.asciz	": "
m.nl:
	.asciz	"\n"
m.space:
	.asciz	" "
# Message strings for specific errors
m.BadSize:
	.asciz	"negative array size"
m.BadIndex:
	.asciz	"array index out of bounds"
m.DivZero:
	.asciz	"division by zero"
# String literals (including lengths) from program
	.align	2
s.true:
	.word	4
	.ascii	"true"
	.align	2
s.false:
	.word	5
	.ascii	"false"
	.align	2
# Space for Fasto heap
d.heap:
	.space	100000

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
                                        # -- End function
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
