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
