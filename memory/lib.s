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
