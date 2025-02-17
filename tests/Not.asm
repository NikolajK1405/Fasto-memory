	.text	0x00400000
	la	x3, d.heap
	jal	f.main
	jal	p.stop
# User functions
# Function main
f.main:
	sw	x1, -4(x2)
	sw	x18, -8(x2)
	addi	x2, x2, -8
	li	x10, 0
# was:	li	_bool_not_3_, 0
	xori	x10, x10, 1
# was:	xori	_let_x_2_, _bool_not_3_, 1
	li	x11, 1
# was:	li	_bool_not_5_, 1
	xori	x18, x11, 1
# was:	xori	_let_y_4_, _bool_not_5_, 1
# 	mv	_tmp_7_,_let_x_2_
	mv	x11, x10
# was:	mv	_let_t_6_, _tmp_7_
	la	x10, s.true
# was:	la	x10, s.true
	bne	x11, x0, l.wBoolF_8_
# was:	bne	_let_t_6_, x0, l.wBoolF_8_
	la	x10, s.false
# was:	la	x10, s.false
l.wBoolF_8_:
	jal	p.putstring
# was:	jal	p.putstring, x10
	mv	x11, x18
# was:	mv	_tmp_10_, _let_y_4_
# 	mv	_let_p_9_,_tmp_10_
	la	x10, s.true
# was:	la	x10, s.true
	bne	x11, x0, l.wBoolF_11_
# was:	bne	_let_p_9_, x0, l.wBoolF_11_
	la	x10, s.false
# was:	la	x10, s.false
l.wBoolF_11_:
	jal	p.putstring
# was:	jal	p.putstring, x10
	li	x10, 0
# was:	li	_mainres_1_, 0
# 	mv	x10,_mainres_1_
	addi	x2, x2, 8
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