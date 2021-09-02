
TITLE hexdump

; hexdump.asm
; Louise Thompson
; Spring 2019

; hexdump utility for MS-DOS

INCLUDE cs240.inc
.8086

DOSEXIT=4C00h
DOS=21h
    
.data
.code

GetProgramSegmentPrefix PROC
; Returns: BX = DOS Program Segment Prefix

    push ax
    
    DOSPSP = 62h            ; DOS command to get segment prefix
    mov ah, DOSPSP          ; Fetch DOS program segment prefix
    int DOS                 ; Returned in BX
    
    pop ax
    ret
GetProgramSegmentPrefix ENDP

GetCommandTail PROC
; DX = buffer to write command tail to (must be big enough). It will be NUL-terminated
    
    pushf
    push ax
    push bx
    push cx
    push dx
    push si
    push di
    push es

    DOSCMDSIZE = 80h        ; Location of command tail length
    DOSCMDTAIL = 81h        ; Location of command tail characters
        
    call GetProgramSegmentPrefix    ; Returned in BX
    mov es, bx              ; ES will be used to reference segment

;   DOS PSP -> +-----------------------+
;     ...      |                       |
;  PSP + 80 -> | Tail Length (1 byte)  |
;  PSP + 81 -> | <Tail characters...   |
    
    mov si, DOSCMDTAIL          ; Load the starting offset into SI
    mov bx, dx                  ; Load the destination into BX
    mov di, 0
    mov cx, 0                   ; Load the count into CX
    mov cl, es:[DOSCMDSIZE]
    
    jmp cond                    ; Jump to the loop entry
top:
    mov dl, es:[si]             ; Get a character
    cmp di, 0                   ; Check if this is a leading space (DI = 0)
    jne saveChar
    cmp dl, ' '                 ; ...And character is a space
    je skipChar
saveChar:   
    mov [bx][di], dl            ; Save the character
    inc di                      ; Increment DI
skipChar:   
    inc si                      ; Increment source pointer and decrement count
    dec cx
cond:   
    cmp cx, 0                   ; See if we have copied all the characers.
    jg top

done:   
    mov BYTE PTR [bx][di], 0        ; NUL-terminate the string
    
    pop es
    pop di
    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    popf
    ret
GetCommandTail ENDP  

FileRead PROC
; Returns: AX = Number of bytes read
; AH = 3Fh
; BX = file handle
; CX = number of bytes to read
; DS:DX -> buffer for data
; 
; Return:
; CF clear if successful
; AX = number of bytes actually read (0 if at EOF before call)
; CF set on error
; AX = error code (05h,06h) (see #01680 at AH=59h/BX=0000h)
    
    DOS_FILE_READ = 3Fh

    FILEREAD_size = 4
    FILEREAD_buffer = 6
    FILEREAD_handle = 8
;  +--------------+ <- sp
;  | registers    |
;  | old bp       | <- bp
;  | ret. address |
;  | size arg     |
;  | buffer arg   |
;  | handle arg   |
;  +--------------+

    push bp
    mov bp, sp
    pushf
    push bx
    push cx
    push dx

    mov bx, [bp + FILEREAD_handle]
    mov cx, [bp + FILEREAD_size]
    mov dx, [bp + FILEREAD_buffer]
    mov ah, DOS_FILE_READ
    int DOS
    jc error
    
    pop dx
    pop cx
    pop bx
    popf
    pop bp
    ret
    
error:  
.data
file_read_error_msg BYTE "Error reading from file", 0
.code
    mov dx, OFFSET file_read_error_msg
    call PrintString
    call NextLine

    mov ax, DOSEXIT         ; Exit DOS
    int DOS
    ret
FileRead ENDP
    
OpenFile PROC
; DX = ASCIZ filename
; Returns file handle in AX, exits on error.
    
; AH=3Dh                  ;
; AL= access and sharing modes
; DS:DX ASCIZ filename
    
; CF clear if successful
; AX file handle
; CF set on error
; AX = error code
    
    DOS_OPEN_FILE = 3Dh
    OPENFILE_filename = 4
;  +--------------+ <- sp
;  | registers    |
;  | old bp       | <- bp
;  | ret. address |
;  | filename arg |
;  +--------------+
    
    push bp
    mov bp, sp
    pushf
    push dx
        
    mov dx, [bp + OPENFILE_filename]
    mov al, 00h             ; Read only, no sharing info.
    mov ah, DOS_OPEN_FILE
    int DOS
    jc error
    
    pop dx
    popf
    pop bp
    ret

    
error:  
.data
open_file_error_msg BYTE "Error opening file: ", 0
.code
    mov ax, dx
    mov dx, OFFSET open_file_error_msg
    push dx                 ; Push the prompt
    push ax                 ; Push the filename
    mov dx, 2               ; Push the count
    push dx

    call WriteStrings       ; Write the strings out

    mov ax, DOSEXIT         ; Exit DOS
    int DOS
OpenFile ENDP
    
WriteStrings PROC
    WRITESTRINGS_count = 4
    WRITESTRINGS_strings = 6
;  +--------------+ <- sp
;  | registers    |
;  | old bp       | <- bp
;  | ret. address |
;  | count arg    |
;  | filename arg |
;  | ...          |
;  | filename arg |
;  +--------------+
    
    push bp
    mov bp, sp
    pushf
    push bx
    push cx
    push dx
    
    lea bx, [bp + WRITESTRINGS_strings]
    
    mov cx, [bp + WRITESTRINGS_count]
    dec cx
    shl cx, 1
    add bx, cx
    
    mov cx, [bp + WRITESTRINGS_count]
    
top:
    mov dx, ss:[bx]
    call PrintString
    
    sub bx, 2
    loop top
    
    call NextLine

    pop dx
    pop cx
    pop bx
    popf
    pop bp
    ret
WriteStrings ENDP

PrintChar PROC
; prints a character
; DL = char to print
    push ax
    mov ah, 02h     ; set ah to DOS code for write-char
    int 21h         ; DOS!
    pop ax

    ret
PrintChar ENDP

PrintString PROC
; prints a string
; pointer to top of string in DX

    pushf   
    push ax
    push bx
    push dx
    push si

    mov si, 0           ; set counter
    mov bx, dx          ; move pointer to bx    

    top:
        mov dl, [bx + si]   ; move character to dl
        cmp dl, 0       ; check if nul
        je return_proc      ; return
        
        mov ah, 02h     ; set ah to DOS code for write-char
        int 21h         ; DOS!
        
        inc si          ; incriment counter
        jmp top
    
    return_proc:
        pop si
        pop dx
        pop bx
        pop ax
        popf

        ret

PrintString ENDP

NextLine PROC
    push ax
    push dx

    mov dx, 0dh
    mov ah, 02h
    int 21h
    mov dx, 0ah
    mov ah, 02h
    int 21h

    pop dx
    pop ax

    ret

NextLine ENDP

.data
hex_spaces  BYTE "   ", 0
spaces      BYTE " ", 0
.code
FillHex PROC
; CX = length of array
    push dx
    pop dx

    push si
    push dx
    mov si, 16 
    sub si, cx

    cmp si, 8
    jg extra_space

top:
    mov dx, OFFSET hex_spaces
    call PrintString
    dec si
    cmp si, 0
    jne top

    pop dx
    pop si
    ret

extra_space:
    mov dx, OFFSET spaces
    call PrintString
    jmp top

FillHex ENDP

.data
hex_digits BYTE "0123456789abcdef", 0
.code
HexOut PROC
; writes an array of bytes in hexadecimal
; BX = address of bytes, CX = length of array

    pushf
    push si
    push ax
    push bx
    push cx
    push dx

    cmp cx, 0
    je return

    mov si, 0

print_byte:

    push bx

    mov ax, 0
    mov al, [bx + si]

    push cx
    mov cl, 4
    shr al, cl
    pop cx

    and al, 0Fh

    mov bx, 0
    mov bl, al

    push si

    mov si, OFFSET hex_digits

    mov dl, [si + bx]
    mov ah, 02h
    int 21h

    pop si

    pop bx
    push bx

    mov ax, 0
    mov al, [bx + si]

    and al, 0Fh

    mov bx, 0
    mov bl, al

    push si

    mov si, OFFSET hex_digits

    mov dl, [si + bx]
    mov ah, 02h
    int 21h

    pop si

    mov dl, " "
    mov ah, 02h
    int 21h

    pop bx

    cmp si, 7
    je extra_space

continue:
    inc si

    cmp si, cx
    jne print_byte

    cmp cx, 16
    je return
    call FillHex

return:

    pop dx
    pop cx
    pop bx
    pop ax
    pop si
    popf

    ret

extra_space:
    mov dl, " "
    mov ah, 02h
    int 21h
    jmp continue

HexOut ENDP

OffsetOut PROC
; 16 bit hexadecimal
; BX = address of bytes, CX = length of array

    pushf
    push si
    push ax
    push bx
    push cx
    push dx

    mov si, 3
    jmp print_byte
continue1:
    mov si, 2
    jmp print_byte
continue2:
    mov si, 1
    jmp print_byte
continue3: 
    mov si, 0

print_byte:

    push bx

    mov ax, 0
    mov al, [bx + si]

    push cx
    mov cl, 4
    shr al, cl
    pop cx

    and al, 0Fh

    mov bx, 0
    mov bl, al

    push si

    mov si, OFFSET hex_digits

    mov dl, [si + bx]
    mov ah, 02h
    int 21h

    pop si

    pop bx
    push bx

    mov ax, 0
    mov al, [bx + si]

    and al, 0Fh

    mov bx, 0
    mov bl, al

    push si

    mov si, OFFSET hex_digits

    mov dl, [si + bx]
    mov ah, 02h
    int 21h

    pop si
    pop bx

    cmp si, 3
    je continue1
    cmp si, 2
    je continue2
    cmp si, 1
    je continue3

    pop dx
    pop cx
    pop bx
    pop ax
    pop si
    popf

    ret

OffsetOut ENDP

.data
text_spaces  BYTE " ", 0
.code
FillText PROC
; CX = length of array
    push dx
    pop dx

    push si
    push dx
    mov si, 16 
    sub si, cx
top:
    mov dx, OFFSET text_spaces
    call PrintString
    dec si
    cmp si, 0
    jne top

    pop dx
    pop si
    ret
FillText ENDP

WriteText PROC
; Writes right column of text for hexdump
; BX = offset of string, CX = number of bytes in string

    pushf
    push ax
    push bx
    push cx
    push dx
    push si

    push dx
    mov dl, ' '
    call PrintChar
    mov dl, 7Ch
    call PrintChar
    pop dx

    mov si, 0       ; set counter

print_char:
    cmp cx, si
    je finish

    mov dx, 0
    mov dl, [bx + si]

    cmp dx, 32
    jl print_period
    cmp dx, 126
    jg print_period

continue:
    call PrintChar

    inc si
    jmp print_char

finish:
    cmp cx, 16
    je return
    call FillText

return:
    push dx
    mov dl, '|'
    call PrintChar
    pop dx

    pop si
    pop dx
    pop cx
    pop bx
    pop ax
    popf

    ret

print_period:
    mov dl, '.'
    jmp continue

WriteText ENDP
    
.data
filename BYTE "file.txt", 0, 100 dup(0)
pbuffer BYTE "File contents: "
buffer BYTE 17 dup(0)
prevbuffer BYTE 17 dup(0)
handle WORD ?
same WORD ?
same_trigger WORD 0
fileplace DWORD 0h
.code
IncOffset PROC
    .386
    push cx

    mov cx, ax
top2:
    inc DWORD PTR [fileplace]
    dec cx
    cmp cx, 0
    jne top2

    pop cx

    ret
IncOffset ENDP

CopyBuffer PROC
    pushf
    push bx
    push dx
    push si

    mov si, 0
    mov di, 0
top:
    mov bx, OFFSET buffer
    mov dl, [bx + si]
    mov bx, OFFSET prevbuffer
    mov [bx][di], dl

    inc si
    inc di
    cmp si, 17
    jne top

    pop si
    pop dx
    pop bx
    popf

    ret

CopyBuffer ENDP

CompareBuffer PROC
    pushf
    push bx
    push dx
    push si

    mov same, 1

    mov si, 0
top:
    mov bx, OFFSET buffer
    mov dx, 0
    mov dl, [bx + si]
    mov bx, OFFSET prevbuffer
    mov cx, 0
    mov cl, [bx + si]

    cmp dx, cx
    jne return          ; if there is a difference, return 1

    inc si
    cmp si, 17
    jne top

    mov same, 0

return:

    pop si
    pop dx
    pop bx
    popf

    ret
CompareBuffer ENDP

main PROC
    .8086
    mov ax, @data
    mov ds, ax

    .386
    mov dx, OFFSET filename
    call GetCommandTail

    push dx
    call OpenFile
    add sp, 2
    mov handle, ax
    jmp top
    
top:
    mov same, 1
    mov dx, handle
    push dx
    mov dx, OFFSET buffer
    push dx
    mov dx, LENGTHOF buffer - 1
    push dx
    call FileRead
    add sp, 6

    mov bx, ax      ; NUL terminate the string (won't always work!)
    mov buffer[bx], 0

    cmp fileplace, 0
    je continue


    call CompareBuffer
    cmp same, 0
    jne continue

    mov same_trigger, 1
    call IncOffset

    jmp top

continue:
    cmp same_trigger, 1
    je triggered

continue2:
    cmp ax, 0
    je finish

   mov dx, OFFSET buffer

    push bx
    mov bx, offset fileplace
    call offsetout
    pop bx
    call IncOffset

    push ax
    push dx
    mov dl, ' '
    mov ah, 02h     ; set ah to DOS code for write-char
    int 21h         ; DOS!
    pop dx
    pop ax
    push dx
    mov dl, ' '
    call PrintChar
    pop dx

    push bx
    push cx
    mov bx, dx
    mov cx, ax
    call HexOut
    mov cx, ax
    call WriteText
    pop cx
    pop bx

    call CopyBuffer

    call NextLine
    
    cmp ax, 16
    je top

finish:
    cmp fileplace, 0
    je done

    push bx
    mov bx, offset fileplace
    call offsetout
    pop bx

   call NextLine

done:   
    mov ax, DOSEXIT
    int DOS
    ret

triggered:
    push dx
    mov dl, '*'
    call PrintChar
    pop dx
    mov same_trigger, 0
    call NextLine
    jmp continue2
main ENDP
END main        
    .386
    push GetCommandTail
    .8086
    call TraceRegisterChanges