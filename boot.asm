; MiniOS16 1.0 by NNW Dev — robustní bootloader (1.44MB floppy)
; - čte KERNEL_SECTORS sektorů počínaje LBA 1 (hned za boot sektorem)
; - cíl: ES:BX = 0x1000:0x0000
; - opakované pokusy při chybě, jednoduché hlášky
; - hlídá přetečení BX -> zvyšuje ES o 0x20 (512B = 0x20 paragraphů)

[org 0x7C00]
bits 16

%define KERNEL_SEG     0x1000
%define KERNEL_OFF     0x0000
%define KERNEL_LBA0    1            ; kernel začíná na druhém sektoru (LBA=1)
%define KERNEL_SECTORS 8            ; <<< NASTAV podle velikosti kernel.bin (ceil(size/512))

%define SPT 18                      ; sectors per track
%define HEADS 2

start:
    cli
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0x7C00
    sti

    ; uložit boot drive (BIOS dal do DL)
    mov [bootdrv], dl

    ; text
    mov si, msgBoot
    call puts

    ; nastavit cílový segment pro kernel
    mov ax, KERNEL_SEG
    mov es, ax
    mov bx, KERNEL_OFF
    xor di, di                ; di nepoužíváme, jen pro jistotu

    ; init proměnných
    mov word [dest_off], KERNEL_OFF
    mov word [lba], KERNEL_LBA0
    mov word [left], KERNEL_SECTORS

read_next_sector:
    cmp word [left], 0
    je load_done

    ; LBA -> CHS do [cyl],[hd],[sec]
    mov ax, [lba]
    call lba_to_chs

    ; nastavit ES:BX na aktuální offset
    mov bx, [dest_off]

    ; pokusy o čtení (max 3x)
    mov byte [retries], 3
.retry:
    mov ah, 0x02              ; BIOS disk read
    mov al, 1                 ; číst 1 sektor
    mov ch, [cyl]             ; cylinder
    mov cl, [sec]             ; sektor (1..SPT)
    mov dh, [hd]              ; hlava
    mov dl, [bootdrv]         ; jednotka
    int 0x13
    jc .fail

    ; úspěch: posunout ukazatele
    add word [dest_off], 512
    ; ošetření přetečení BX: pokud po přičtení 512 < 512, došlo k overflow
    ; (ale jelikož to sčítáme do [dest_off], přepočítej ES dle [dest_off])
    mov bx, [dest_off]
    ; když BX přetekl přes 0xFFFF, nepoznáme to takhle,
    ; proto použijeme logiku "každých 512B -> ES += 0x20" průběžně:
    ; jednoduché a spolehlivé: vždy když přičteš 512, udělej ES += 0x20 a BX -= 0x200
    ; -> přepiš posun:
    sub word [dest_off], 512
    ; uprav ES/BX přímo:
    add bx, 512
    ; místo toho:
    cmp bx, 0x0200
    jb .no_carry_adjust       ; (viz níže poznámka) zvolíme jiné řešení

    ; --- Jednodušší a bez pastí: držíme BX v 16 bitech a ES zvětšujeme o 0x20 na každý sektor
.no_carry_adjust:
    ; obnov ES:BX přes step-per-sector:
    ; krok: ES += 0x20; BX = BX (KERNEL_OFF) — uděláme to jinak:
    ; Nastavíme politiku: po každém úspěšném čtení:
    ;   ES += 0x20
    ;   BX = KERNEL_OFF
    ; Tím se každý sektor mapuje do nového segmentu a nehrozí overflow BX.
    mov ax, es
    add ax, 0x20              ; 0x20 paragraphů = 512 bajtů
    mov es, ax
    mov bx, KERNEL_OFF

    ; posun čítačů
    inc word [lba]
    dec word [left]
    jmp read_next_sector

.fail:
    dec byte [retries]
    jnz .retry

    ; po 3 pokusech to vzdej
    mov si, msgDiskErr
    call puts
    jmp halt_forever

load_done:
    mov si, msgOK
    call puts

    ; skok do kernelu na první segment, kde začal (KERNEL_SEG:KERNEL_OFF)
    ; POZOR: kernel je rozprostřený "po segmentech" (ES se zvedal o 0x20 každým sektorem).
    ; To kernelu nevadí, protože se spouští z prvního bajtu na KERNEL_SEG:0000 a zbytek je
    ; lineárně za ním v paměti (fyzicky spojitě).
    jmp KERNEL_SEG:KERNEL_OFF

halt_forever:
    cli
.h: hlt
    jmp .h

; -------------------------
; LBA v AX -> CHS (1.44MB)
; výstup: [cyl],[hd],[sec]
; -------------------------
lba_to_chs:
    push bx
    push dx
    push cx

    ; sec  = (LBA % SPT) + 1
    mov bx, ax
    xor dx, dx
    mov cx, SPT
    div cx              ; AX = LBA/SPT, DX = LBA%SPT
    mov dl, dl
    inc dl
    mov [sec], dl

    ; temp = LBA/SPT v AX
    mov bx, ax
    xor dx, dx
    mov cx, HEADS
    div cx              ; AX = temp/HEADS (=cyl), DX = temp%HEADS (=head)
    mov [cyl], al
    mov [hd], dl

    pop cx
    pop dx
    pop bx
    ret

; -------------------------
; puts: vypíše ASCIIZ (DS:SI)
; -------------------------
puts:
    pusha
.nxt:
    lodsb
    test al, al
    jz .done
    mov ah, 0x0E
    mov bh, 0
    mov bl, 7
    int 0x10
    jmp .nxt
.done:
    popa
    ret

; -------------------------
; Proměnné a zprávy
; -------------------------
bootdrv    db 0
retries    db 0
lba        dw 0
left       dw 0
dest_off   dw 0

cyl  db 0
hd   db 0
sec  db 0

msgBoot    db 13,10,'Booting MiniOS16...',13,10,0
msgOK      db 'Kernel loaded.',13,10,0
msgDiskErr db 'Disk read error',13,10,0

times 510-($-$$) db 0
dw 0xAA55