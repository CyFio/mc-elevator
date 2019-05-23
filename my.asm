data segment
;ioport    equ 3100h-0280h    ;tpc卡中设备的io地址
ioport    equ 0    ;tpc卡中设备的io地址
io8254a     equ ioport+280h        ;0计数端口计数为2000，方式3 
io8254b     equ ioport+281h        ;1计数端口计数为1000，方式3
io8254c     equ ioport+283h        ;8254控制端  
int_vect EQU 073H ;中断0-7 的向量为:08h-0fh,中断8-15 的向量为:70h-77h
irq_mask_2_7 equ 011111011b ;中断掩码,中断0-7 时从低至高相应位为零,中断8-15 时第2 位为零
irq_mask_9_15 equ 011110111b;中断0-7 时全一,中断8-15 时从低至高相应位为零
ioport_cent equ 3000h ;tpc 卡中9054 芯片的io 地址
csreg dw ?
ipreg dw ? ;旧中断向量保存空间

; 运行状态
is_running     db      0
;字符串
menu           db      '************************************************',0DH,0AH 
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
               
lift_info   db 'lift running!', 0dh, 0ah, '$'
zawaludo    db 'The WORLD!!!',0dh,0ah,'$'

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
    mov ah,1      ;等待键盘输入
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
    cmp al,27     ;判断是否为ESC键
  ;  je exit       ;若是则退出
    jne main

exit: cli
mov bl, irq_mask_2_7 ;恢复中断掩码
not bl
in al, 21h
or al, bl
out 21h, al
mov bl, irq_mask_9_15
not bl
in al, 0a1h
or al, bl
out 0a1h, al
mov dx,ipreg ;恢复原中断向量
mov ax,csreg
mov ds,ax
mov ah,25h
mov al,int_vect
int 21h
mov dx,ioport_cent+68h ;设置 tpc 卡中9054 芯片io 口,关闭中断
in ax,dx
and ax,0f7ffh
out dx,ax
mov ah,4ch
int 21h

init8254 proc near
    push dx 
    push ax
    mov dx,io8254c;8254的计数器0置方式3 
    mov al,36h 
    out dx,al 
    mov dx,io8254a;计数器0初始值为2000
    mov ax,2000
    out dx,al 
    mov al,ah 
    out dx,al 
    mov dx,io8254c;8254的计数器1置方式3 
    mov al,76h
    out dx,al 
    ;初始状态电梯停止
 ;   mov dx,io8254b;计数器1初始值为1000 
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
mov dx,ioport_cent+68h ;设置 tpc 卡中9054 芯片io 口,使能中断
in ax,dx
or ax,0900h
out dx,ax
mov al,int_vect ;保存原中断向量
mov ah,35h
int 21h
mov ax,es
mov csreg,ax
mov ipreg,bx
mov ax,cs ;设置新中断向量
mov ds,ax
mov dx,offset int_proc
mov al,int_vect
mov ah,25h
int 21h
in al, 21h ;设置中断掩码
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

the_world proc near ;电梯停止
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
    mov dx,io8254c;字面意思是将8254的计数器1置方式3,其实是暂停计时器
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
    mov dx,io8254c;8254的计数器1置方式3 
    mov al,76h
    out dx,al 
    mov dx,io8254b;计数器1初始值为1000 
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

int_proc proc far ;中断处理程序
    push dx
    push ax
    push ds
    mov ax, seg lift_info
    mov ds, ax
    mov al,20h ;Send EOI
    out 0a0h,al
    out 20h,al
    mov dx, offset lift_info
    mov ah, 09h
    int 21h
    pop ds
    pop ax
    pop dx
    iret
int_proc endp



code ends
end start