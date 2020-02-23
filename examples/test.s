@ ==================
@ generated with mfc
@ ==================
int_format: .asciz "%d"
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
mov  r1, #0
add  r0, SP, #0
str  r1, [r0]
label_2:
add  r5, SP, #0
ldr  r6, [r5]
mov  r7, #10
cmp  r6, r7
blt  label_3
b label_4
label_3:
add  r8, SP, #0
ldr  r9, [r8]
push {r9}
bl print
add  r12, SP, #0
ldr  r13, [r12]
mov  r14, #1
add  r11, r13, r14
add  r10, SP, #0
str  r11, [r10]
b label_2
label_4:
exit: b exit
