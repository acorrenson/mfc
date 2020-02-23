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
mov  r1, #0
add  r0, SP, #0
str  r1, [r0]
label_1:
add  r4, SP, #0
ldr  r5, [r4]
mov  r6, #10
cmp  r5, r6
blt  label_2
b label_3
label_2:
add  r5, SP, #0
ldr  r6, [r5]
push {r6}
bl print
add  r8, SP, #0
ldr  r9, [r8]
add  r7, r9, #1
add  r6, SP, #0
str  r7, [r6]
b label_1
label_3:
exit: b exit
