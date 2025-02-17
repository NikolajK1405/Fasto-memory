	.text	0x00400000
	la	x3, d.heap
	jal	f.main
	jal	p.stop
# User functions
# Function main
f.main:
	sw	x1, -4(x2)
	addi	x2, x2, -4
	li	x10, 2
# was:	li	_int_neg_4_, 2
	sub	x12, x0, x10
# was:	sub	_size_3_, x0, _int_neg_4_
	bge	x12, x0, l.safe_5_
# was:	bge	_size_3_, x0, l.safe_5_
	li	x10, 1
# was:	li	x10, 1
	la	x11, m.BadSize
# was:	la	x11, m.BadSize
	j	p.RuntimeError
l.safe_5_:
	li	x11, 1
# was:	li	_element_6_, 1
	mv	x10, x3
# was:	mv	_let_x_2_, x3
	slli	x13, x12, 2
# was:	slli	_tmp_11_, _size_3_, 2
	addi	x13, x13, 4
# was:	addi	_tmp_11_, _tmp_11_, 4
	add	x3, x3, x13
# was:	add	x3, x3, _tmp_11_
	sw	x12, 0(x10)
# was:	sw	_size_3_, 0(_let_x_2_)
	addi	x13, x10, 4
# was:	addi	_addr_7_, _let_x_2_, 4
	mv	x14, x0
# was:	mv	_i_8_, x0
l.loop_beg_9_:
	bge	x14, x12, l.loop_end_10_
# was:	bge	_i_8_, _size_3_, l.loop_end_10_
	sw	x11, 0(x13)
# was:	sw	_element_6_, 0(_addr_7_)
	addi	x13, x13, 4
# was:	addi	_addr_7_, _addr_7_, 4
	addi	x14, x14, 1
# was:	addi	_i_8_, _i_8_, 1
	j	l.loop_beg_9_
l.loop_end_10_:
# 	mv	_mainres_1_,_let_x_2_
# 	mv	x10,_mainres_1_
	addi	x2, x2, 4
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