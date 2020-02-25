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
mov  r1, #4
add  r2, SP, #0
strb  r1, [r2]
mov  r1, #1
add  r2, SP, #1
strb  r1, [r2]
label_0:
add  r1, SP, #0
ldrb  r2, [r1]
mov  r1, #1
cmp  r2, r1
bge  label_1
b label_2
label_1:
add  r1, SP, #1
ldrb  r3, [r1]
add  r2, SP, #0
ldrb  r1, [r2]
mul  r2, r3, r1
add  r1, SP, #1
strb  r2, [r1]
add  r2, SP, #0
ldrb  r1, [r2]
sub  r2, r1, #1
add  r1, SP, #0
strb  r2, [r1]
b label_0
label_2:
add  r2, SP, #1
ldrb  r1, [r2]
push {r1}
bl print
exit: b exit