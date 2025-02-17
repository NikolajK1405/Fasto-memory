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
# was:	li	_size_9_, 6
	bge	x11, x0, l.safe_10_
# was:	bge	_size_9_, x0, l.safe_10_
	li	x10, 4
# was:	li	x10, 4
	la	x11, m.BadSize
# was:	la	x11, m.BadSize
	j	p.RuntimeError
l.safe_10_:
	mv	x10, x3
# was:	mv	_arr_6_, x3
	slli	x12, x11, 2
# was:	slli	_tmp_15_, _size_9_, 2
	addi	x12, x12, 4
# was:	addi	_tmp_15_, _tmp_15_, 4
	add	x3, x3, x12
# was:	add	x3, x3, _tmp_15_
	sw	x11, 0(x10)
# was:	sw	_size_9_, 0(_arr_6_)
	addi	x13, x10, 4
# was:	addi	_addr_11_, _arr_6_, 4
	mv	x12, x0
# was:	mv	_i_12_, x0
l.loop_beg_13_:
	bge	x12, x11, l.loop_end_14_
# was:	bge	_i_12_, _size_9_, l.loop_end_14_
	sw	x12, 0(x13)
# was:	sw	_i_12_, 0(_addr_11_)
	addi	x13, x13, 4
# was:	addi	_addr_11_, _addr_11_, 4
	addi	x12, x12, 1
# was:	addi	_i_12_, _i_12_, 1
	j	l.loop_beg_13_
l.loop_end_14_:
	lw	x19, 0(x10)
# was:	lw	_size_5_, 0(_arr_6_)
	mv	x18, x3
# was:	mv	_let_x_4_, x3
	slli	x11, x19, 2
# was:	slli	_tmp_21_, _size_5_, 2
	addi	x11, x11, 4
# was:	addi	_tmp_21_, _tmp_21_, 4
	add	x3, x3, x11
# was:	add	x3, x3, _tmp_21_
	sw	x19, 0(x18)
# was:	sw	_size_5_, 0(_let_x_4_)
	addi	x21, x18, 4
# was:	addi	_addrg_16_, _let_x_4_, 4
	mv	x20, x0
# was:	mv	_i_17_, x0
	addi	x22, x10, 4
# was:	addi	_elem_7_, _arr_6_, 4
l.loop_beg_18_:
	bge	x20, x19, l.loop_end_19_
# was:	bge	_i_17_, _size_5_, l.loop_end_19_
	lw	x10, 0(x22)
# was:	lw	_res_8_, 0(_elem_7_)
	addi	x22, x22, 4
# was:	addi	_elem_7_, _elem_7_, 4
# 	mv	x10,_res_8_
	jal	f.read_int
# was:	jal	f.read_int, x10
# 	mv	_tmp_20_,x10
# 	mv	_res_8_,_tmp_20_
	sw	x10, 0(x21)
# was:	sw	_res_8_, 0(_addrg_16_)
	addi	x21, x21, 4
# was:	addi	_addrg_16_, _addrg_16_, 4
	addi	x20, x20, 1
# was:	addi	_i_17_, _i_17_, 1
	j	l.loop_beg_18_
l.loop_end_19_:
# 	mv	_arr_23_,_let_x_4_
	lw	x11, 0(x18)
# was:	lw	_size_24_, 0(_arr_23_)
	li	x10, 128
# was:	li	_let_y_22_, 128
	addi	x18, x18, 4
# was:	addi	_arr_23_, _arr_23_, 4
	mv	x12, x0
# was:	mv	_ind_var_25_, x0
l.loop_beg_27_:
	bge	x12, x11, l.loop_end_28_
# was:	bge	_ind_var_25_, _size_24_, l.loop_end_28_
	lw	x13, 0(x18)
# was:	lw	_tmp_26_, 0(_arr_23_)
	addi	x18, x18, 4
# was:	addi	_arr_23_, _arr_23_, 4
# 	mv	_divide_L_30_,_let_y_22_
# 	mv	_divide_R_31_,_tmp_26_
	bne	x13, x0, l.safe_32_
# was:	bne	_divide_R_31_, x0, l.safe_32_
	li	x10, 5
# was:	li	x10, 5
	la	x11, m.DivZero
# was:	la	x11, m.DivZero
	j	p.RuntimeError
l.safe_32_:
	div	x10, x10, x13
# was:	div	_fun_arg_res_29_, _divide_L_30_, _divide_R_31_
# 	mv	_let_y_22_,_fun_arg_res_29_
	addi	x12, x12, 1
# was:	addi	_ind_var_25_, _ind_var_25_, 1
	j	l.loop_beg_27_
l.loop_end_28_:
# 	mv	_tmp_34_,_let_y_22_
# 	mv	_let_t_33_,_tmp_34_
# 	mv	x10,_let_t_33_
	jal	p.putint
# was:	jal	p.putint, x10
	li	x10, 0
# was:	li	_mainres_3_, 0
# 	mv	x10,_mainres_3_
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