;
; CopyRight (C) 2019 CyFio OranGe™
; Created Time: Thursday,May 23rd 2019, 2:26:48 pm
; Author: CyFio OranGe
; Author E-mail: 213170687@seu.edu.cn
; ------------------------------------------------
; Last Modified: Tuesday,May 28th 2019, 9:07:28 am
; Modified By CyFio(213170687@seu.edu.cn)
; ------------------------------------------------
; Filename: my_dos.asm
; Project: My_Elevator
; Description: use English to avoid garbling in dos
;
data segment
;ioport definition
ioport          equ     3100h-0280h     ;TPC card base io address(in DOS)
;ioport         equ     0               ;TPC card base io address(in IDE)
io8254a         equ     ioport+280h     ;Timer0: value = 2000, function = 3 
io8254b         equ     ioport+281h     ;Timer1: value = 1000, function = 3
io8254c         equ     ioport+283h     ;8254 Control port 
io8255a         equ     ioport+288h     ;8255 PA
io8255b         equ     ioport+289h     ;8255 PB
io8255c         equ     ioport+28ah     ;8255 PC
io8255d         equ     ioport+28bh     ;8255 Control port
io74273         equ     ioport+290h     ;74273 port
oled_row        equ     ioport+298h     ;oled row select
oled_col        equ     ioport+2a0h     ;oled col select

;user constant
true            equ     1
false           equ     0               ;bool define
up              equ     1
down            equ     0               ;direction define
;key map
ESC             equ     27
key_run         equ     'r'
key_stop        equ     's'
key_direct      equ     'd'
key_pause       equ     'p'

;interrupt
int_vect        equ     073H            ;use INT 073h
irq_mask_2_7    equ     011111011b      ;IRQ mask
irq_mask_9_15   equ     011110111b      ;IRQ mask
ioport_cent     equ     3000h           ;TPC card 9054 io address(in DOS)
csreg           dw      ?
ipreg           dw      ?               ;Old IRQ storage

int_count       db      0

;strings
menu            db      '************************************************',0DH,0AH 
                db      '*      liftor   procedures                      *',0DH,0AH
                db      '*      8254cs~280h 8255cs~288h                  *',0Dh,0Ah      
                db      '*      74273cs~290h                             *',0Dh,0Ah      
                db      '*      oled_row~298h oled_col~290h              *',0Dh,0Ah
                db      '*      8255b---seg_led                          *',0Dh,0Ah
                db      '*      273-----led0~led7                        *',0Dh,0Ah    
                db      '*      gate0&&gate1--5V                         *',0Dh,0Ah
                db      '*      clk0---1MHZ     clk1---out0              *',0Dh,0Ah 
                db      '*      clk               WR----IOW              *',0Dh,0Ah  
                db      '*      S0--5V                                   *',0Dh,0Ah  
                db      '*      by08117117    xjh                        *',0Dh,0Ah 
                db      '************************************************',0Dh,0Ah 
                db      '1~8------1 ~ 8floor!',0DH,0AH 
                db      'esc----------exit!',0DH,0AH 
                db      'r--------run!',0DH,0AH
                db      'd------------go directly!',0Dh,0Ah  
                db      's------------stop!',0Dh,0Ah 
                db      'p------------pause!',0Dh,0Ah,'$' 
str_update      db      'elevator running!                        ',0dh, 0ah, '$'
str_arrival     db      'elevator arrival!                        ', 0ah, '$'
str_continue    db      'elevator continues!                      ',0ah,'$'
str_pause       db      'elevator paused!                         ',0ah,'$'
str_start       db      'elevator starts!                         ',0ah,'$'
str_stop        db      'elevator stops!                          ',0ah,'$'
str_direct_st   db      'elevator direct mode: on                 ',0ah,'$'
str_direct_ed   db      'elevator direct mode: off                ',0ah,'$'

;elevator data
is_on           db      true           ;whether the elevator is on
is_running      db      false          ;whether the elevator is running
level_select    db      00000000b      ;current levels selected
select_count    db      0              ;number of selected levels
cur_level       db      1              ;current level the elevator locates
direction       db      up             ;current moving direction
is_ignore       db      false          ;whether the elevator ignore level select

;data table
seg_led_code    db   3fh,06h,5bh,4fh,66h,6dh,7dh,07h,7fh,6fh ;seg_led code
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
call init8255
call int_start
call far ptr seg_show

main:
    mov ah,1      ;wait key input
    int 16h
    jz main_continue
    mov ah, 0
    int 16h

    push dx
    xor dh, dh
    mov dl, al
    call printf 
    pop dx

is_esc:
    cmp al,27     ;if key is ESC
    je exit       ;quit
    call key_update
    call far ptr oled_update

main_continue:
    jmp main

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

;param: none
;ret:   none
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
    pop ax
    pop dx 
    ret
init8254 endp
;param: none
;ret:   none
init8255 proc near
    push dx
    push ax
    mov dx, io8255b
    mov al, 10001000b ;PB->output
    out dx, al
    pop ax
    pop dx
    ret
init8255 endp
;param: none
;ret:   none
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
;param: al:character
;ret:   none
printf proc near
    push dx
    push ax
    mov ah, 2
    int 21h
    mov dl, 0dh
    int 21h
    mov dl, 0ah
    int 21h
    pop ax 
    pop dx
    ret
printf endp
;param: none
;ret:   none
elevator_stop proc near ;stop the timer, stop the elevator
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov dx, offset str_stop
    call far ptr print_str
    mov al, false
    mov is_running, al
    mov dx,io8254c;this OCW writing step stops the timer
    mov al,76h
    out dx,al 
    mov al, 0
    mov level_select, 0
    mov dx, io74273
    out dx, al
    pop ds
    pop ax
    pop dx
    sti
    ret
elevator_stop endp
;param: none
;ret:   none
elevator_pause proc near ;stop the timer, stop the elevator
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov dx, offset str_pause
    call far ptr print_str

    pop ds
    pop ax
    pop dx
    sti
    ret
elevator_pause endp
;param: none
;ret:   none
elevator_start proc near ;elevator starts
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov al, true
    mov is_running, al
    mov dx,io8254c;8254 Timer1->function3 
    mov al,76h
    out dx,al 
    mov dx,io8254b;Timer1.value = 1000 
    mov ax,1000
    out dx,al 
    mov al,ah 
    out dx,al 
    mov dx, offset str_start
    call far ptr print_str
    pop ds
    pop ax
    pop dx
    sti
    ret
elevator_start endp
;param: none
;ret:   none
elevator_continue proc near ;elevator continues
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax

    mov dx, offset str_continue
    call far ptr print_str
    pop ds
    pop ax
    pop dx
    sti
    ret
elevator_continue endp
;brief: check if al refers to a level
;param al:keycode
;ret   bl:is_level
is_level proc near
    push ax
    mov bl, true
    sub al, '0'
    cmp al, 9
    jc is_level_ret
    mov bl, false
is_level_ret:
    pop ax
    ret
is_level endp
;brief: check if the elevator is running in direct-mode
;param none
;ret   bl:if the elevator is running in direct-mode
check_ignore proc near
    push ax
    mov al, is_running
    mov bl, is_ignore
    and bl, al
check_ignore_ret: 
    pop ax
    ret
check_ignore endp
;param al:keycode
;ret   none
update_led_data proc near
    push ax
    push bx
    push cx
    push dx
    
    mov cl, al
    sub cl, '1'
    mov al, cur_level
    cmp cl, al
    jne update_led_data_count
    mov al, is_running
    cmp al, true
    jne update_led_data_count
    jmp update_led_data_ret
update_led_data_count:
    mov dl, select_count
    mov bh, level_select
    shr bh, cl
    and bh, true
    cmp bh, true
    jne update_led_data_minus
update_led_data_plus:
    inc dl
    mov select_count, dl
    jmp update_led_data_view
update_led_data_minus:
    dec dl
    mov select_count, dl
update_led_data_view:
    mov ah, 1
    shl ah, cl
    mov al, level_select
    xor al, ah
    mov level_select, al
    call far ptr led_show
update_led_data_ret:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
update_led_data endp
;param al:keycode
;ret   none
key_update proc near 
    push ax
    push dx
    push bx
    
    mov bl, is_on
    cmp bl, true
    jne key_update_p
    call check_ignore
    cmp bl, true
    jne key_update_r
key_update_num:
    call is_level
    cmp bl, true
    jne key_update_r
    call update_led_data
    jmp key_update_ret
key_update_r:
    mov bl, is_running
    cmp bl, false
    jne key_update_s
    cmp al, key_run
    jne key_update_s
    call elevator_start
    jmp key_update_ret
key_update_s:   
    mov bl, is_running
    cmp bl, true
    jne key_update_p
    cmp al, key_stop
    jne key_update_p
    call elevator_stop
    jmp key_update_ret
key_update_p:   
    mov bl, is_running
    cmp bl, true
    jne key_update_ret
    cmp al, key_pause
    jne key_update_ret
    mov bl, is_on
    xor bl, true
    mov is_on, bl
    cmp bl, true
    jne key_update_resume
    call elevator_pause
    jmp key_update_ret
key_update_resume:
    call elevator_continue
key_update_ret:
    push bx
    pop dx
    pop ax
    ret
key_update endp
;param: none
;ret:   none
led_show proc far
    push ax
    push dx
    mov al, level_select
    mov dx, io74273
    out dx, al
    pop dx
    pop ax
    ret
led_show endp
;param: none
;ret:   none
seg_show proc far
    push ax
    push dx
    push bx
    lea bx, seg_led_code
    mov al, cur_level
    dec al
    xor ah, ah
    add bx, ax
    mov al, [bx]
    mov dx, io8255b
    out dx, al
    pop bx
    pop dx
    pop ax
    ret
seg_show endp
;param: none
;ret:   none
oled_update proc far
    ret
oled_update endp
;param: dx: str offset
;ret:   none
print_str proc far
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov ah, 09h
    int 21h
    pop ds
    pop ax
    ret
print_str endp
;param: none
;ret:   none
direction_change proc far
    push ax
    push bx
    push cx
    push dx
    mov al, level_select
    cmp al, 0
    jne direction_change_main
    mov al, false
    mov is_running, al
    jmp direction_change_ret
direction_change_main:
    mov cl, cur_level
    xor bh, bh          ;use bh as flag
    mov bl, direction
    cmp bl, up
    jne direction_change_down
direction_change_up:
    mov ah, al
    shr ah, cl
    cmp ah, 0 ;if current direction is up and upper levels selected, go up
    jne direction_change_ret
    xor bl, up
    mov direction, bl ;change direction
    inc bh
    cmp bh, 2
    jnc direction_change_down
    mov al, false
    mov is_running, al
    jmp direction_change_ret
direction_change_down:
    mov ah, al
    mov ch, 9
    sub ch, cl
    shl ah, ch
    cmp ah, 0
    jne direction_change_ret
    xor bl, up
    mov direction, bl ;change direction
    inc bh
    cmp bh, 2
    jnc direction_change_up
    mov al, false
    mov is_running, al
direction_change_ret:
    pop dx
    pop cx
    pop bx
    pop ax
    ret
direction_change endp
;param: none
;ret:   none
elevator_action proc far
    push ax
    mov ah, cur_level
    mov al, direction
    cmp al, up
    jne elevator_action_down
elevator_action_up:
    inc ah
    jmp elevator_action_ret
elevator_action_down:
    dec ah
elevator_action_ret:
    mov cur_level, ah
    call far ptr seg_show
    pop ax
    ret
elevator_action endp
;param: none
;ret:   bl: is_arrival
arrival_check proc far
    push ax
    push dx
    push cx
    xor bl, bl  ;default: not arrival
    mov al, level_select
    mov dl, al
    mov cl, cur_level
    dec cl
    mov ah, 1
    shl ah, cl
    and dl, ah  ;if current level selected
    cmp dl,0
    je arrival_check_ret;not arrival
    mov bl, true
    xor al, ah  ;arrival, level unselected
    mov level_select, al
    call far ptr led_show
    
arrival_check_ret:
    pop cx
    pop dx
    pop ax
    ret
arrival_check endp
;param: none
;ret:   none
elevator_arrival proc far ;elevator arrive
    cli
    push dx
    push ax
    push ds
    mov ax, data
    mov ds, ax
    mov al, false
    mov is_running, al
    mov dx,io8254c;8254 Timer1->function3 
    mov al,76h
    out dx,al 

    mov dx, offset str_arrival
    call far ptr print_str
    pop ds
    pop ax
    pop dx
    sti
    ret
elevator_arrival endp
;param: none
;ret:   none
elevator_update proc far
    push bx
    call direction_change
    call elevator_action
    call arrival_check
    cmp bl, true
    jne elevator_update_ret
    call elevator_arrival
elevator_update_ret:
    pop bx
    ret
elevator_update proc endp
;param: none
;ret:   none
int_proc proc far ;INT process
    cli
    push dx
    push ax
    push ds

    mov ax, seg str_update    mov ds, ax

    mov al, int_count   ;INT count
    inc al
    mov int_count, al

    mov dx, offset str_update    
    call far ptr print_str

    call far ptr elevator_update

    mov al,20h ;Send EOI
    out 0a0h,al
    out 20h,al
    pop ds
    pop ax
    pop dx
    sti
    iret
int_proc endp

code ends
end start
