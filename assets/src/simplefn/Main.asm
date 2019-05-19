Main.$wask_info:
_c500:
	addq $16,%r12
	cmpq 856(%r13),%r12
	ja _c504
_c503:
	cmpq $42,%r14
	jne _c4ZY
_c4ZZ:
	addq $-16,%r12
	movl $Main.Fail_closure+2,%ebx
	jmp *(%rbp)
_c4ZY:
	movq $Main.Done_con_info,-8(%r12)
	movq %r14,(%r12)
	leaq -7(%r12),%rbx
	jmp *(%rbp)
_c504:
	movq $16,904(%r13)
	movl $Main.$wask_closure,%ebx
	jmp *-8(%r13)
