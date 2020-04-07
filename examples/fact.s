@ ==================
@ generated with mfc
@ ==================
int_format: .asciz "%d\n"
.align
print:
ldr r0, =int_format
pop {r1}
push {lr}
bl printf
pop {pc}
.global main
.extern printf
main:
push {lr}
