/*
 * CopyRight (C) 2019 CyFio OranGe™
 * Created Time: Thursday,May 23rd 2019, 2:26:48 pm
 * Author: CyFio OranGe
 * Author E-mail: 213170687@seu.edu.cn
 * ------------------------------------------------
 * Last Modified: Monday,May 27th 2019, 9:27:32 pm
 * Modified By CyFio(213170687@seu.edu.cn)
 * ------------------------------------------------
 * Filename: my_dos.asm
 * Project: My_Elevator
 * Description: use English to avoid garbling in dos
 */
data segment
;ioport definition
ioport          equ     3100h-0280h     ;TPC card base io address(in DOS)
;ioport         equ     0               ;TPC card base io address(in IDE)
io8254a         equ     ioport+280h     ;Timer0: value = 2000, function = 3 
io8254b         equ     ioport+281h     ;Timer1: value = 1000, function = 3
io8254c         equ     ioport+283h     ;8254 Control port 
io8255a         equ     ioport
int_vect        equ     073H            ;use INT 073h
irq_mask_2_7    equ     011111011b      ;IRQ mask
irq_mask_9_15   equ     011110111b      ;IRQ mask
ioport_cent     equ     3000h           ;TPC card 9054 io address(in DOS)
csreg           dw      ?
ipreg           dw      ?               ;Old IRQ storage

is_running      db      0
int_count       db      0
 
menu            db      '************************************************',0DH,0AH 
                db      '*      liftor   procedures                      *',0DH,0AH
                db      '*      8254cs~280h 8255cs~288h                  *',0Dh,0Ah      
                db      '*      lightrow~288h lightred~290h              *',0Dh,0Ah
                db      '*      pc0-pc7-----JP13                         *',0Dh,0Ah    
                db      '*      pb0-pb7-----led0~led7                    *',0Dh,0Ah 
                db      '*      out1--------pa0 gate0&&gate1--5V         *',0Dh,0Ah
                db      '*      clk0---1MHZ     clk1---out0              *',0Dh,0Ah 
                db      '*      D0-D7-----JP14    WR----IOW              *',0Dh,0Ah  
                db      '*      by08117117    xjh                        *',0Dh,0Ah 
                db      '************************************************',0Dh,0Ah 
                db      '1~8------1 ~ 8floor!',0DH,0AH 
                db      'esc----------exit!',0DH,0AH 
                db      'return--------run!',0DH,0AH
                db      'A------------go directly!',0Dh,0Ah  
                db      'B------------stop!',0Dh,0Ah 
                db      'C------------off/on!',0Dh,0Ah,'$' 
lift_info       db      'lift running!', 0dh, 0ah, '$'
zawaludo        db      'The WORLD!!!',0dh,0ah,'$'

data ends
stacks segment
db 100 dup (?)
stacks ends
code segment
assume cs:code,ds:data,ss:stacks,es:data

;Enable Local Interrupt Input
start:cli
mov ax,data
mov ds,ax
mov es,ax
mov ax,stacks
mov ss,ax
mov dx,offset menu
mov ah,09h
int 21h

call init8254
call int_start

main:
    mov ah,1      ;wait key input
    int 16h
    jz main
    mov ah, 0
    int 16h

    push dx
    xor dh, dh
    mov dl, al
    call printf 
    pop dx

    cmp al, 's'
    jne is_esc
    mov bl, is_running
    cmp bl, 0
    jne stop_it
    call continue
    jmp is_esc
stop_it:
    call the_world
is_esc:
    cmp al,27     ;if key is ESC
  ;  je exit       ;quit
    jne main

exit: cli
mov bl, irq_mask_2_7 ;recover irq mask
not bl
in al, 21h
or al, bl
out 21h, al
mov bl, irq_mask_9_15
not bl
in al, 0a1h
or al, bl
out 0a1h, al
mov dx,ipreg ;recover irq vector
mov ax,csreg
mov ds,ax
mov ah,25h
mov al,int_vect
int 21h
mov dx,ioport_cent+68h ;set TPC card's 9054 to close interrupt
in ax,dx
and ax,0f7ffh
out dx,ax
mov ah,4ch
int 21h

init8254 proc near
    push dx 
    push ax
    mov dx,io8254c;8254 Timer0->function3 
    mov al,36h 
    out dx,al 
    mov dx,io8254a;Timer0.value=2000
    mov ax,2000
    out dx,al 
    mov al,ah 
    out dx,al 
    mov dx,io8254c;8254 Timer1->function3 
    mov al,76h
    out dx,al 
    ;Elevator stops at first
 ;   mov dx,io8254b; Timer1.value=1000 
 ;   mov ax,1000
 ;   out dx,al 
 ;   mov al,ah 
 ;   out dx,al 
    pop ax
    pop dx 
    ret
init8254 endp

int_start proc near
cli
push ax
push bx
push dx
push ss
push ds
push es
mov ax,data
mov ds,ax
mov es,ax
mov ax,stacks
mov ss,ax
mov dx,ioport_cent+68h ;et TPC card's 9054 to enable interrupt
in ax,dx
or ax,0900h
out dx,ax               
mov al,int_vect ;keep old INT vector
mov ah,35h
int 21h
mov ax,es
mov csreg,ax
mov ipreg,bx
mov ax,cs ;set new INT vector
mov ds,ax
mov dx,offset int_proc
mov al,int_vect
mov ah,25h
int 21h
in al, 21h ;set IRQ mask
and al, irq_mask_2_7
out 21h, al
in al, 0a1h
and al, irq_mask_9_15
out 0a1h, al
mov ax,data
mov ds,ax
pop es
pop ds
pop ss
pop dx
pop bx
pop ax
sti
ret
int_start endp

printf proc near
    push ax
    mov ah, 2
    int 21h
    mov dl, 0dh
    int 21h
    mov dl, 0ah
    int 21h
    pop ax 
    ret
printf endp

the_world proc near ;stop the timer, stop the elevator
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov dx, offset zawaludo
    mov ah, 09h
    int 21h
    mov al, 0
    mov is_running, al
    mov dx,io8254c;this OCW writing step stops the timer
    mov al,76h
    out dx,al 
    pop ds
    pop ax
    pop dx
    sti
    ret
the_world endp

continue proc near ;电梯继续
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
 ;   mov dx, offset zawaludo
;    mov ah, 09h
;    int 21h
    mov al, 1
    mov is_running, al
    mov dx,io8254c;8254 Timer1->function3 
    mov al,76h
    out dx,al 
    mov dx,io8254b;Timer1.value = 1000 
    mov ax,1000
    out dx,al 
    mov al,ah 
    out dx,al 
    pop ds
    pop ax
    pop dx
    sti
    ret
continue endp

int_proc proc far ;INT process
    push dx
    push ax
    push ds

    mov ax, seg lift_info
    mov ds, ax

    mov al, int_count   ;INT count
    inc al
    mov int_count, al

    mov dx, offset lift_info
    mov ah, 09h
    int 21h

    mov al,20h ;Send EOI
    out 0a0h,al
    out 20h,al
    pop ds
    pop ax
    pop dx
    iret
int_proc endp



code ends
end start
