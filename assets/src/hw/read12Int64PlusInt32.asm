	movq 15(%rbx),%rcx
	movq 23(%rbx),%rdx
	movq 31(%rbx),%rbx
	movq %rcx,%rsi
	addq %rdx,%rsi
	movq (%rsi),%rdi
	movq 8(%rsi),%r8
	movq 16(%rsi),%r9
    ...
	movq %rcx,64(%rsp)
	movq 48(%rsi),%rcx
	movq %rax,72(%rsp)
	movq 56(%rsi),%rax
    ...
    movq $Data.ByteString.Internal.PS_con_info,-72(%r12)
    ...
	addq $100,%r9
	movq %r9,-48(%r12)
	...
    movq $GHC.Int.I64#_con_info,-32(%r12)
    addq %r8,%rdi
	addq %rdi,%rsi
	addq %rsi,%rbx
	addq %rbx,%rdx
	addq %rdx,%rax
    ...
    movq %rax,-24(%r12)
    ...
    movq $(,)_con_info,-16(%r12)
    