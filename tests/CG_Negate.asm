	.text	0x00400000
	la	x3, d.heap
	jal	f.main
	jal	p.stop
# User functions
# Function read_int
f.read_int:
	sw	x1, -4(x2)
	addi	x2, x2, -4
# 	mv	_param_i_1_,x10
	jal	p.getint
# was:	jal	p.getint, 
# 	mv	_read_intres_2_,x10
# 	mv	x10,_read_intres_2_
	addi	x2, x2, 4
	lw	x1, -4(x2)
	jr	x1
# Function write_nl
f.write_nl:
	sw	x1, -4(x2)
	addi	x2, x2, -4
# 	mv	_param_a_3_,x10
# 	mv	_tmp_6_,_param_a_3_
# 	mv	_let_x_5_,_tmp_6_
# 	mv	x10,_let_x_5_
	jal	p.putint
# was:	jal	p.putint, x10
	la	x10, s.X_9_
# was:	la	_tmp_8_, s.X_9_
# s.X_9_: string "\n"
# 	mv	_let_y_7_,_tmp_8_
# 	mv	x10,_tmp_8_
	jal	p.putstring
# was:	jal	p.putstring, x10
	li	x10, 0
# was:	li	_write_nlres_4_, 0
# 	mv	x10,_write_nlres_4_
	addi	x2, x2, 4
	lw	x1, -4(x2)
	jr	x1
# Function main
f.main:
	sw	x1, -4(x2)
	sw	x22, -24(x2)
	sw	x21, -20(x2)
	sw	x20, -16(x2)
	sw	x19, -12(x2)
	sw	x18, -8(x2)
	addi	x2, x2, -24
	li	x11, 6
# was:	li	_size_16_, 6
	bge	x11, x0, l.safe_17_
# was:	bge	_size_16_, x0, l.safe_17_
	li	x10, 9
# was:	li	x10, 9
	la	x11, m.BadSize
# was:	la	x11, m.BadSize
	j	p.RuntimeError
l.safe_17_:
	mv	x10, x3
# was:	mv	_arr_13_, x3
	slli	x12, x11, 2
# was:	slli	_tmp_22_, _size_16_, 2
	addi	x12, x12, 4
# was:	addi	_tmp_22_, _tmp_22_, 4
	add	x3, x3, x12
# was:	add	x3, x3, _tmp_22_
	sw	x11, 0(x10)
# was:	sw	_size_16_, 0(_arr_13_)
	addi	x13, x10, 4
# was:	addi	_addr_18_, _arr_13_, 4
	mv	x12, x0
# was:	mv	_i_19_, x0
l.loop_beg_20_:
	bge	x12, x11, l.loop_end_21_
# was:	bge	_i_19_, _size_16_, l.loop_end_21_
	sw	x12, 0(x13)
# was:	sw	_i_19_, 0(_addr_18_)
	addi	x13, x13, 4
# was:	addi	_addr_18_, _addr_18_, 4
	addi	x12, x12, 1
# was:	addi	_i_19_, _i_19_, 1
	j	l.loop_beg_20_
l.loop_end_21_:
	lw	x19, 0(x10)
# was:	lw	_size_12_, 0(_arr_13_)
	mv	x18, x3
# was:	mv	_let_x_11_, x3
	slli	x11, x19, 2
# was:	slli	_tmp_28_, _size_12_, 2
	addi	x11, x11, 4
# was:	addi	_tmp_28_, _tmp_28_, 4
	add	x3, x3, x11
# was:	add	x3, x3, _tmp_28_
	sw	x19, 0(x18)
# was:	sw	_size_12_, 0(_let_x_11_)
	addi	x21, x18, 4
# was:	addi	_addrg_23_, _let_x_11_, 4
	mv	x20, x0
# was:	mv	_i_24_, x0
	addi	x22, x10, 4
# was:	addi	_elem_14_, _arr_13_, 4
l.loop_beg_25_:
	bge	x20, x19, l.loop_end_26_
# was:	bge	_i_24_, _size_12_, l.loop_end_26_
	lw	x10, 0(x22)
# was:	lw	_res_15_, 0(_elem_14_)
	addi	x22, x22, 4
# was:	addi	_elem_14_, _elem_14_, 4
# 	mv	x10,_res_15_
	jal	f.read_int
# was:	jal	f.read_int, x10
# 	mv	_tmp_27_,x10
# 	mv	_res_15_,_tmp_27_
	sw	x10, 0(x21)
# was:	sw	_res_15_, 0(_addrg_23_)
	addi	x21, x21, 4
# was:	addi	_addrg_23_, _addrg_23_, 4
	addi	x20, x20, 1
# was:	addi	_i_24_, _i_24_, 1
	j	l.loop_beg_25_
l.loop_end_26_:
# 	mv	_arr_31_,_let_x_11_
	lw	x10, 0(x18)
# was:	lw	_size_30_, 0(_arr_31_)
	mv	x11, x3
# was:	mv	_let_y_29_, x3
	slli	x12, x10, 2
# was:	slli	_tmp_40_, _size_30_, 2
	addi	x12, x12, 4
# was:	addi	_tmp_40_, _tmp_40_, 4
	add	x3, x3, x12
# was:	add	x3, x3, _tmp_40_
	sw	x10, 0(x11)
# was:	sw	_size_30_, 0(_let_y_29_)
	addi	x12, x11, 4
# was:	addi	_addrg_34_, _let_y_29_, 4
	mv	x13, x0
# was:	mv	_i_35_, x0
	addi	x14, x18, 4
# was:	addi	_elem_32_, _arr_31_, 4
l.loop_beg_36_:
	bge	x13, x10, l.loop_end_37_
# was:	bge	_i_35_, _size_30_, l.loop_end_37_
	lw	x15, 0(x14)
# was:	lw	_res_33_, 0(_elem_32_)
	addi	x14, x14, 4
# was:	addi	_elem_32_, _elem_32_, 4
# 	mv	_int_neg_39_,_res_33_
	sub	x15, x0, x15
# was:	sub	_fun_arg_res_38_, x0, _int_neg_39_
# 	mv	_res_33_,_fun_arg_res_38_
	sw	x15, 0(x12)
# was:	sw	_res_33_, 0(_addrg_34_)
	addi	x12, x12, 4
# was:	addi	_addrg_34_, _addrg_34_, 4
	addi	x13, x13, 1
# was:	addi	_i_35_, _i_35_, 1
	j	l.loop_beg_36_
l.loop_end_37_:
	mv	x14, x11
# was:	mv	_arr_43_, _let_y_29_
	lw	x10, 0(x14)
# was:	lw	_size_42_, 0(_arr_43_)
	mv	x11, x3
# was:	mv	_let_z_41_, x3
	slli	x12, x10, 2
# was:	slli	_tmp_59_, _size_42_, 2
	addi	x12, x12, 4
# was:	addi	_tmp_59_, _tmp_59_, 4
	add	x3, x3, x12
# was:	add	x3, x3, _tmp_59_
	sw	x10, 0(x11)
# was:	sw	_size_42_, 0(_let_z_41_)
	addi	x12, x11, 4
# was:	addi	_addrg_46_, _let_z_41_, 4
	mv	x13, x0
# was:	mv	_i_47_, x0
	addi	x14, x14, 4
# was:	addi	_elem_44_, _arr_43_, 4
l.loop_beg_48_:
	bge	x13, x10, l.loop_end_49_
# was:	bge	_i_47_, _size_42_, l.loop_end_49_
	lw	x15, 0(x14)
# was:	lw	_res_45_, 0(_elem_44_)
	addi	x14, x14, 4
# was:	addi	_elem_44_, _elem_44_, 4
	li	x16, 4
# was:	li	_int_neg_57_, 4
	sub	x16, x0, x16
# was:	sub	_lt_L_55_, x0, _int_neg_57_
# 	mv	_lt_R_56_,_res_45_
	slt	x16, x16, x15
# was:	slt	_cond_54_, _lt_L_55_, _lt_R_56_
	bne	x16, x0, l.then_51_
# was:	bne	_cond_54_, x0, l.then_51_
	j	l.else_52_
l.then_51_:
# 	mv	_int_neg_58_,_res_45_
	sub	x15, x0, x15
# was:	sub	_fun_arg_res_50_, x0, _int_neg_58_
	j	l.endif_53_
l.else_52_:
# 	mv	_fun_arg_res_50_,_res_45_
l.endif_53_:
# 	mv	_res_45_,_fun_arg_res_50_
	sw	x15, 0(x12)
# was:	sw	_res_45_, 0(_addrg_46_)
	addi	x12, x12, 4
# was:	addi	_addrg_46_, _addrg_46_, 4
	addi	x13, x13, 1
# was:	addi	_i_47_, _i_47_, 1
	j	l.loop_beg_48_
l.loop_end_49_:
# 	mv	_arr_62_,_let_z_41_
	lw	x18, 0(x11)
# was:	lw	_size_61_, 0(_arr_62_)
	mv	x10, x3
# was:	mv	_let_t_60_, x3
	slli	x12, x18, 2
# was:	slli	_tmp_71_, _size_61_, 2
	addi	x12, x12, 4
# was:	addi	_tmp_71_, _tmp_71_, 4
	add	x3, x3, x12
# was:	add	x3, x3, _tmp_71_
	sw	x18, 0(x10)
# was:	sw	_size_61_, 0(_let_t_60_)
	addi	x19, x10, 4
# was:	addi	_addrg_65_, _let_t_60_, 4
	mv	x20, x0
# was:	mv	_i_66_, x0
	addi	x21, x11, 4
# was:	addi	_elem_63_, _arr_62_, 4
l.loop_beg_67_:
	bge	x20, x18, l.loop_end_68_
# was:	bge	_i_66_, _size_61_, l.loop_end_68_
	lw	x22, 0(x21)
# was:	lw	_res_64_, 0(_elem_63_)
	addi	x21, x21, 4
# was:	addi	_elem_63_, _elem_63_, 4
# 	mv	_tmp_70_,_res_64_
# 	mv	_fun_arg_res_69_,_tmp_70_
	mv	x10, x22
# was:	mv	x10, _fun_arg_res_69_
	jal	p.putint
# was:	jal	p.putint, x10
# 	mv	_res_64_,_fun_arg_res_69_
	sw	x22, 0(x19)
# was:	sw	_res_64_, 0(_addrg_65_)
	addi	x19, x19, 4
# was:	addi	_addrg_65_, _addrg_65_, 4
	addi	x20, x20, 1
# was:	addi	_i_66_, _i_66_, 1
	j	l.loop_beg_67_
l.loop_end_68_:
	li	x10, 0
# was:	li	_mainres_10_, 0
# 	mv	x10,_mainres_10_
	addi	x2, x2, 24
	lw	x22, -24(x2)
	lw	x21, -20(x2)
	lw	x20, -16(x2)
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
s.X_9_:
	.word	1
	.ascii	"\n"
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