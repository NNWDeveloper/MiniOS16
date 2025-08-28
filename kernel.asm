; MiniOS16 1.2 — KERNEL
org 0x0000
bits 16

%define CMD_MAX 128

; ---------- Konst a layout (1.44MB floppy) ----------
%define SPT     18      ; sectors per track
%define HEADS   2
%define BYTES   512

; FAT12 (typická 1.44MB)
%define BPB_Rsvd  1
%define BPB_FATs  2
%define BPB_SPF   9
%define BPB_RootEnt 224
; Odvozené
%define ROOT_DIR_SECT ((BPB_RootEnt*32 + BYTES - 1)/BYTES) ; 224*32/512 = 14
%define FAT_START_LBA  BPB_Rsvd
%define ROOT_START_LBA (BPB_Rsvd + BPB_FATs*BPB_SPF)
%define DATA_START_LBA (ROOT_START_LBA + ROOT_DIR_SECT)

; ---------- Start ----------
start:
    cli
    mov ax, cs
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 0xFFFE
    sti

    ; Zachovej číslo boot disku v DL, pokud ho sem skákal bootloader.
    ; Když není známé, necháme výchozí 0 (floppy A:).
    ; (Pro jistotu DL neměníme zde.)

    call video_init
    call cls

    mov si, banner
    call print_string

shell_loop:
    mov si, prompt
    call print_string

    mov di, cmdline
    mov cx, CMD_MAX
    call read_line

    ; trim začátek
    mov si, cmdline
    call skip_spaces
    cmp byte [si], 0
    je shell_loop

    ; dispatch — porovnáme s příkazy (case-sensitive, bez extra mezer uvnitř názvu)
    push si
      mov di, cmd_help     ; help
      call compare_cmd
      jc .do_help
    pop si
    push si
      mov di, cmd_cls      ; cls
      call compare_cmd
      jc .do_cls
    pop si
    push si
      mov di, cmd_echo     ; echo
      call compare_cmd
      jc .do_echo
    pop si
    push si
      mov di, cmd_mem      ; mem
      call compare_cmd
      jc .do_mem
    pop si
    push si
      mov di, cmd_beep     ; beep
      call compare_cmd
      jc .do_beep
    pop si
    push si
      mov di, cmd_about    ; about
      call compare_cmd
      jc .do_about
    pop si
    push si
      mov di, cmd_reboot   ; reboot
      call compare_cmd
      jc .do_reboot
    pop si
    push si
      mov di, cmd_halt     ; halt
      call compare_cmd
      jc .do_halt
    pop si
    push si
      mov di, cmd_sum      ; sum a b
      call compare_cmd
      jc .do_sum
    pop si
    push si
      mov di, cmd_hex      ; hex n
      call compare_cmd
      jc .do_hex
    pop si
    push si
      mov di, cmd_hexdump  ; hexdump addr len
      call compare_cmd
      jc .do_hexdump
    pop si
    push si
      mov di, cmd_color    ; color fg bg
      call compare_cmd
      jc .do_color
    pop si
    push si
      mov di, cmd_uptime   ; uptime
      call compare_cmd
      jc .do_uptime
    pop si
    push si
      mov di, cmd_sec      ; sec lba count
      call compare_cmd
      jc .do_sec
    pop si
    push si
      mov di, cmd_ls       ; ls (root FAT12)
      call compare_cmd
      jc .do_ls
    pop si

    mov si, msgUnknown
    call print_string
    jmp shell_loop

; ---------- Handlery příkazů ----------

.do_help:
    mov si, msgHelp
    call print_string
    jmp shell_loop

.do_cls:
    call cls
    jmp shell_loop

.do_echo:
    pop si
    call skip_word_forward
    ; vytiskni zbytek řádku (už je zero-terminated)
    call print_string
    call newline
    jmp shell_loop

.do_mem:
    int 0x12           ; AX = KB konvenční paměť
    mov si, msgMem1
    call print_string
    call print_dec16   ; AX už hotové
    mov si, msgMem2
    call print_string
    jmp shell_loop

.do_beep:
    call beep_440
    call newline
    jmp shell_loop

.do_about:
    mov si, msgAbout
    call print_string
    jmp shell_loop

.do_reboot:
    mov si, msgReboot
    call print_string
    int 0x19           ; BIOS bootstrap
    jmp $

.do_halt:
    mov si, msgHalt
    call print_string
    cli
.hang: hlt
    jmp .hang

.do_sum:
    pop si
    call skip_word_forward
    call skip_spaces
    call parse_number          ; AX=a
    jc .sum_err
    mov bx, ax
    call skip_spaces
    call parse_number          ; AX=b
    jc .sum_err
    add ax, bx
    mov si, msgSum
    call print_string
    call print_dec16
    call newline
    jmp shell_loop
.sum_err:
    mov si, msgUsageSum
    call print_string
    jmp shell_loop

.do_hex:
    pop si
    call skip_word_forward
    call skip_spaces
    call parse_number
    jc .hex_err
    mov si, msgHex
    call print_string
    call print_hex16
    call newline
    jmp shell_loop
.hex_err:
    mov si, msgUsageHex
    call print_string
    jmp shell_loop

.do_hexdump:
    pop si
    call skip_word_forward
    call skip_spaces
    call parse_number         ; addr
    jc .hd_err
    mov bx, ax
    call skip_spaces
    call parse_number         ; len
    jc .hd_err
    mov cx, ax
    call hexdump              ; DS:BX, CX
    jmp shell_loop
.hd_err:
    mov si, msgUsageHexDump
    call print_string
    jmp shell_loop

.do_color:
    ; color <fg> <bg>  (0..15)
    pop si
    call skip_word_forward
    call skip_spaces
    call parse_number
    jc .col_err
    mov bl, al                ; fg
    call skip_spaces
    call parse_number
    jc .col_err
    shl al, 4
    or bl, al                 ; BL = bg<<4 | fg
    mov [text_attr], bl
    mov si, msgOK
    call print_string
    jmp shell_loop
.col_err:
    mov si, msgUsageColor
    call print_string
    jmp shell_loop

.do_uptime:
    ; čti tick count z BDA 0x40:0x6C (~18.2 Hz)
    push ds
    mov ax, 0x0040
    mov ds, ax
    mov bx, [0x006C]
    pop ds
    ; převeď na ms ~ (ticks*55)
    mov ax, bx
    mov bx, 55
    mul bx       ; DX:AX = AX*55 (stačí AX pro < 1193s)
    mov si, msgUptime
    call print_string
    ; vytiskni milisekundy
    call print_dec16
    mov si, msgMs
    call print_string
    jmp shell_loop

.do_sec:
    ; sec lba <LBA> <count>
    pop si
    call skip_word_forward
    call skip_spaces
    call parse_number         ; LBA
    jc .sec_err
    mov [lba_tmp], ax
    call skip_spaces
    call parse_number         ; count
    jc .sec_err
    mov cx, ax
    mov ax, [lba_tmp]
    ; Převod LBA->CHS (pro 1.44MB, 18spt, 2hlavy)
    push dx
    push bx
    mov bx, SPT*HEADS         ; 36
    xor dx, dx
    div bx                    ; AX=LBA/(36) -> cylinder, DX=rem
    mov ch, al                ; CH=cylinder (0..79)
    mov al, dl                ; AL=rem
    xor ah, ah
    mov bl, SPT               ; 18
    xor dx, dx
    div bl                    ; AL=head, DL=sector-1
    mov dh, al                ; DH=head (0..1)
    mov cl, dl
    inc cl                    ; CL=sector (1..18)
    pop bx
    pop dx
    ; cílový buffer ES:BX
    mov ax, cs
    mov es, ax
    mov bx, sector_buf
    ; DL = drive (ponecháme jak je; pro floppy obvykle 0x00)
    mov ah, 0x02
    int 0x13                  ; BIOS read
    jc .sec_bios_err
    ; hexdump prvního sektoru
    mov bx, sector_buf
    mov cx, 512
    call hexdump
    jmp shell_loop
.sec_bios_err:
    mov si, msgDiskErr
    call print_string
    jmp shell_loop
.sec_err:
    mov si, msgUsageSec
    call print_string
    jmp shell_loop

.do_ls:
    ; vypíše root directory (FAT12 layout 1.44MB)
    ; Root začíná na LBA 19 a má 14 sektorů.
    ; Načteme jeden po druhém a projdeme 32B položky.
    mov si, msgLS
    call print_string
    mov ax, ROOT_START_LBA
    mov [lba_tmp], ax
    mov cx, ROOT_DIR_SECT
.ls_loop_sect:
    cmp cx, 0
    je .ls_done
    ; LBA->CHS (viz výše)
    mov ax, [lba_tmp]
    push dx
    push bx
    mov bx, SPT*HEADS
    xor dx, dx
    div bx
    mov ch, al
    mov al, dl
    xor ah, ah
    mov bl, SPT
    xor dx, dx
    div bl
    mov dh, al
    mov cl, dl
    inc cl
    pop bx
    pop dx
    mov ax, cs
    mov es, ax
    mov bx, sector_buf
    mov ah, 0x02
    mov al, 1
    int 0x13
    jc .ls_next
    ; Projdi 16 položek po 32 B v jednom sektoru
    mov si, sector_buf
    mov di, 16
.ls_entry_loop:
    cmp di, 0
    je .ls_next
    ; 0x00 = žádné další, 0xE5 = smazané, 0x2E/0x2E2E = . a ..
    mov al, [si]
    cmp al, 0x00
    je .ls_done            ; konec dir
    cmp al, 0xE5
    je .ls_skip
    ; atributy na offsetu 11
    mov bl, [si+11]
    test bl, 0x08          ; volume label? přeskoč
    jnz .ls_skip
    test bl, 0x10          ; subdir – klidně vypiš stejně
    ; vytiskni 8+3 s mezerou
    call print_dir_name    ; DS:SI na položku
.ls_skip:
    add si, 32
    dec di
    jmp .ls_entry_loop
.ls_next:
    inc word [lba_tmp]
    dec cx
    jmp .ls_loop_sect
.ls_done:
    jmp shell_loop

; ---------- Video/IO ----------

video_init:
    mov ax, 3
    int 0x10
    ret

cls:
    mov ax, 0x0600
    mov bh, [text_attr]
    mov cx, 0
    mov dx, 0x184F
    int 0x10
    mov ah, 0x02
    xor bh, bh
    xor dx, dx
    int 0x10
    ret

putch:
    pusha
    mov ah, 0x0E
    mov bh, 0
    mov bl, [text_attr]
    int 0x10
    popa
    ret

print_string:
    pusha
.next:
    lodsb
    test al, al
    jz .done
    call putch
    jmp .next
.done:
    popa
    ret

newline:
    mov al, 13
    call putch
    mov al, 10
    call putch
    ret

read_line:
    ; IN: DI=buffer, CX=max
    pusha
    xor si, si
.rl:
    mov ah, 0
    int 0x16
    cmp al, 13
    je .done
    cmp al, 8
    je .back
    cmp si, cx
    jae .rl
    mov [di], al
    inc di
    inc si
    call putch
    jmp .rl
.back:
    cmp si, 0
    je .rl
    dec di
    dec si
    mov al, 8
    call putch
    mov al, ' '
    call putch
    mov al, 8
    call putch
    jmp .rl
.done:
    mov byte [di], 0
    call newline
    popa
    ret

; ---------- Parser / porovnání ----------

skip_spaces:
.s1:
    mov al, [si]
    cmp al, ' '
    jne .s2
    inc si
    jmp .s1
.s2:
    ret

skip_word_forward:
    ; SI na začátku slova -> posuň za něj a přeskoč mezery
.sw1:
    mov al, [si]
    cmp al, 0
    je .sw2
    cmp al, ' '
    je .sw2
    inc si
    jmp .sw1
.sw2:
    call skip_spaces
    ret

compare_cmd:
    ; IN: SI=input, DI=cmd (zero-term)
    ; OUT: CF=1 pokud se shoduje a další znak je 0 nebo mezera
    push si
    push di
.c1:
    mov al, [si]
    mov bl, [di]
    cmp bl, 0
    je .c_endword
    cmp al, bl
    jne .c_no
    inc si
    inc di
    jmp .c1
.c_endword:
    mov al, [si]
    cmp al, 0
    je .c_yes
    cmp al, ' '
    je .c_yes
.c_no:
    clc
    pop di
    pop si
    ret
.c_yes:
    stc
    pop di
    pop si
    ret

; ---------- Čísla ----------

; parse_number: dec nebo 0xHEX
; IN: SI -> text čísla, OUT: AX=hodnota, CF=0 ok, CF=1 chyba; SI posunut za číslo
parse_number:
    pusha
    ; hex?
    mov al, [si]
    cmp al, '0'
    jne .dec
    mov al, [si+1]
    cmp al, 'x'
    je .hex_start
    cmp al, 'X'
    je .hex_start
.dec:
    ; dec: AX=0, čti 0..9
    xor ax, ax
.pd_loop:
    mov dl, [si]
    cmp dl, '0'
    jb .pd_end
    cmp dl, '9'
    ja .pd_end
    sub dl, '0'
    ; AX = AX*10 + DL
    mov bx, ax
    shl ax, 1
    mov dx, bx
    shl dx, 3
    add ax, dx
    xor dx, dx
    add ax, dx      ; (DX=0) no-op, jen pro jistotu
    add ax, dx
    add ax, dx
    add ax, dx
    ; přičti číslici
    mov dh, 0
    mov dl, dl      ; DL už má číslici 0..9
    add ax, dx
    inc si
    jmp .pd_loop
.pd_end:
    mov [tmp16], ax
    popa
    mov ax, [tmp16]
    clc
    ret
.hex_start:
    add si, 2
    xor ax, ax
.ph_loop:
    mov dl, [si]
    cmp dl, 0
    je .ph_end
    ; 0-9
    cmp dl, '0'
    jb .ph_end
    cmp dl, '9'
    jbe .ph_d0
    ; A-F
    cmp dl, 'A'
    jb .ph_end
    cmp dl, 'F'
    jbe .ph_dA
    ; a-f
    cmp dl, 'a'
    jb .ph_end
    cmp dl, 'f'
    ja .ph_end
.ph_d0:
    sub dl, '0'
    jmp .ph_acc
.ph_dA:
    sub dl, 'A'
    add dl, 10
    jmp .ph_acc
.ph_acc:
    shl ax, 4
    xor dh, dh
    mov dh, 0
    mov ah, 0
    add al, dl
    inc si
    jmp .ph_loop
.ph_end:
    mov [tmp16], ax
    popa
    mov ax, [tmp16]
    clc
    ret

; ---------- Tisk čísla ----------

print_dec16:
    pusha
    cmp ax, 0
    jne .pd_conv
    mov al, '0'
    call putch
    popa
    ret
.pd_conv:
    xor cx, cx
    mov bx, 10
.pd_loop2:
    xor dx, dx
    div bx
    push dx
    inc cx
    cmp ax, 0
    jne .pd_loop2
.pd_out:
    pop dx
    add dl, '0'
    mov al, dl
    call putch
    loop .pd_out
    popa
    ret

print_hex16:
    pusha
    mov si, hexPrefix
    call print_string
    mov cx, 4
    mov bx, ax
.ph_nib:
    rol bx, 4
    mov al, bl
    and al, 0x0F
    cmp al, 9
    jbe .ph_d
    add al, 'A' - 10
    jmp .ph_o
.ph_d:
    add al, '0'
.ph_o:
    call putch
    loop .ph_nib
    popa
    ret

hexdump:
    ; IN: DS:BX pointer, CX length
    pusha
.hd_l:
    cmp cx, 0
    je .hd_done
    mov ax, bx
    call print_hex16
    mov si, colonSpace
    call print_string
    mov al, [bx]
    push ax
    call print_hex8
    mov si, space
    call print_string
    pop ax
    mov dl, al
    cmp dl, 32
    jb .np
    cmp dl, 126
    ja .np
    mov al, dl
    call putch
    jmp .cont
.np:
    mov al, '.'
    call putch
.cont:
    call newline
    inc bx
    dec cx
    jmp .hd_l
.hd_done:
    popa
    ret

print_hex8:
    pusha
    mov ah, al
    mov dl, ah
    shr dl, 4
    and dl, 0x0F
    call hex_digit
    mov al, dl
    call putch
    mov dl, ah
    and dl, 0x0F
    call hex_digit
    mov al, dl
    call putch
    popa
    ret

hex_digit:
    ; IN: DL 0..15, OUT: DL ASCII
    cmp dl, 9
    jbe .hd_d
    add dl, 7
.hd_d:
    add dl, '0'
    ret

; ---------- Specifické pomocníky ----------

print_dir_name:
    ; DS:SI -> 32B položka diru
    ; Tiskne 8+3  (bez teček uvnitř, vloží vlastní '.')
    pusha
    ; jméno 8
    mov cx, 8
.pn1:
    mov al, [si]
    cmp al, ' '
    je .pn1_blank
    call putch
    jmp .pn1_next
.pn1_blank:
    ; nic netiskni
    nop
.pn1_next:
    inc si
    loop .pn1
    ; přípona 3
    mov al, [si+3]      ; přeskoč attribute/čas pokud by bylo – NE, přípona je na +8..+10
    ; oprav SI na začátek ext
    sub si, 8
    add si, 8
    ; zjisti, zda je ext prázdná (samy mezery)
    mov cx, 3
    mov bx, si
    mov di, 0
.pe_chk:
    mov al, [bx]
    cmp al, ' '
    jne .pe_has
    inc bx
    loop .pe_chk
    jmp .pn_done_nodot
.pe_has:
    mov al, '.'
    call putch
    mov cx, 3
.pe_print:
    mov al, [si]
    cmp al, ' '
    je .pe_skip
    call putch
.pe_skip:
    inc si
    loop .pe_print
.pn_done_nodot:
    call newline
    popa
    ret

beep_440:
    pusha
    in al, 0x61
    mov ah, al
    or al, 3
    out 0x61, al
    mov cx, 0x2000
.b1: loop .b1
    mov al, ah
    out 0x61, al
    popa
    ret

; ---------- Data ----------
banner    db 'MiniOS16 1.2   16bit real mode educational OS.  type help',13,10,0
prompt    db 'MiniOS16> ',0
text_attr db 0x07

msgUnknown db 'Neznamy prikaz. Napis "help".',13,10,0
msgHelp    db 'Prikazy: help, cls, echo, mem, beep, about, reboot, halt, sum, hex, hexdump, color, uptime, sec, ls',13,10,0
msgMem1    db 'Konvencni pamet: ',0
msgMem2    db ' KB',13,10,0
msgAbout   db 'MiniOS16 by NNW Dev (16-bit real mode, edukacni).',13,10,0
msgReboot  db 'Reboot...',13,10,0
msgHalt    db 'CPU zastaveno (HLT).',13,10,0
msgSum     db 'Soucet = ',0
msgHex     db 'Hex = ',0
msgUsageSum db 'Pouziti: sum <a> <b> (dec/0xHEX)',13,10,0
msgUsageHex db 'Pouziti: hex <n> (dec/0xHEX)',13,10,0
msgUsageHexDump db 'Pouziti: hexdump <addr> <len>',13,10,0
msgUsageColor db 'Pouziti: color <fg 0..15> <bg 0..15>',13,10,0
msgOK       db 'OK',13,10,0
msgUptime   db 'Uptime (pribl.) ms: ',0
msgMs       db ' ms',13,10,0
msgUsageSec db 'Pouziti: sec lba <LBA> <count>',13,10,0
msgDiskErr  db 'Disk read error',13,10,0
msgLS       db 'Root dir:',13,10,0

hexPrefix  db '0x',0
colonSpace db ': ',0
space      db ' ',0

cmd_help   db 'help',0
cmd_cls    db 'cls',0
cmd_echo   db 'echo',0
cmd_mem    db 'mem',0
cmd_beep   db 'beep',0
cmd_about  db 'about',0
cmd_reboot db 'reboot',0
cmd_halt   db 'halt',0
cmd_sum    db 'sum',0
cmd_hex    db 'hex',0
cmd_hexdump db 'hexdump',0
cmd_color  db 'color',0
cmd_uptime db 'uptime',0
cmd_sec    db 'sec',0
cmd_ls     db 'ls',0

cmdline times CMD_MAX db 0
tmp16  dw 0
lba_tmp dw 0

sector_buf times 512 db 0

; zarovnej na 4 KiB (neni nutne, jen pro klid)
times 4096-($-$$) db 0
