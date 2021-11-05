[bits 16]

jmp enable_vbe_mode

enable_vbe_mode:
	push es
	mov ax, 0x4F00
	mov di, vbe_information
	int 0x10
	pop es
	
	mov ax, WORD [vbe_information.video_modes]
	mov [.offset], ax
	mov ax, [vbe_information.video_modes + 2]
	mov [.segment], ax

	mov fs, [.segment]
	mov si, [.offset]

	.switch_to_mode:
		mov dx, [fs:si]
		add si, 0x0002
		mov [.offset], si
		mov [.mode], dx
		mov ax, 0
		mov fs, ax

		cmp [.mode], WORD 0xFFFF
		je .error

		push es
		mov ax, 0x4F01
		mov cx, [.mode]
		mov dx, [vbe_mode_information]
		mov di, vbe_mode_information
		int 0x10
		pop es
		push es

		mov ax, [.width]
		cmp ax, [vbe_mode_information.width]
		jne .next_mode

		mov ax, [vbe_mode_information.width]
		mov [vbe_mode_data.width], ax

		mov ax, [.height]
		cmp ax, [vbe_mode_information.height]
		jne .next_mode

		mov ax, [vbe_mode_information.height]
		mov [vbe_mode_data.height], ax

		mov al, [.bpp]
		cmp al, [vbe_mode_information.bpp]
		jne .next_mode

		mov ax, [vbe_mode_information.bpp]
		mov [vbe_mode_data.bpp], ax

		mov ax, [vbe_mode_information.pitch]
		mov [vbe_mode_data.pitch], ax

		pop es
		push es
		mov ax, 0x4F02
		mov bx, [.mode]
		or bx, 0x4000
		mov di, 0
		int 0x10
		pop es

		xor ax, ax
		xor bx, bx
		xor cx, cx
		xor dx, dx
		jmp .pass

	.next_mode:
		mov ax, [.segment]
		mov fs, ax
		mov si, [.offset]
		jmp .switch_to_mode

	.error:
		jmp .pass

	.segment dw 0
	.offset dw 0
	.mode dw 0
	.width dw 1280
	.height dw 720
	.bpp db 32
	.pitch dw 0

	.frame_buffer dd 0

	.expression_1 dd 0
	.expression_2 dd 0
	.expression_3 dd 0

	.x dw 5
	.y dw 5

	.pass:
		mov ax, vbe_mode_information
		jmp enable_protected_mode
		;jmp $

%include "bootloader/requirements/gdt.asm"
%include "bootloader/requirements/out.asm"


vbe_mode_data:
	.width dw 0
	.height dw 0
	.bpp db 0
	.pitch dw 0
	.frame_nuffer dd 0

enable_protected_mode:
	push ax
	cli
	call enable_a20_line
	lgdt [gdt_descriptor]
	mov eax, cr0
	or eax, 1
	mov cr0, eax
	jmp code_segment:protected_mode_entry_point


enable_a20_line:
	in al, 0x92
	or al, 2
	out 0x92, al
	ret

[bits 32]

%include "bootloader/requirements/long_mode_switch.asm"

protected_mode_entry_point:
	mov ax, data_segment
	mov ss, ax
	mov gs, ax
	mov es, ax
	mov ds, ax
	mov fs, ax

	call gdt_64
	call enable_long_mode
	;jmp $
	jmp code_segment:long_mode_entry_point

print_int:
	mov bx, ax
	mov ecx, DWORD 0xb8000
	.print_loop:
		cmp al, 0
		je .exit_print_loop

		mov dl, 10
		div dl

		xor ah, 00110000b
		mov [ecx], ah
		inc ecx
		inc ecx
		xor bx, bx
		shl ax, 8
		shr ax, 8
		jmp .print_loop

	.exit_print_loop:
	ret

[bits 64]

[extern _start]
[extern _draw]
global _bpp
global _pitch
global _width
global _height
global _frame_buffer

_bpp db 0
_pitch dw 0
_width dw 0
_height dw 0
_frame_buffer dd 0

long_mode_entry_point:
	mov ax, data_segment
	mov gs, ax
	mov ss, ax
	mov es, ax
	mov fs, ax
	mov ds, ax
	mov edi, 0xb8000
	mov ecx, 500
	mov rax, 0x00FFFFFF
	;mov rax, 0x1f201f201f201f20
	rep stosq

	;mov ax, WORD [vbe_mode_information.bpp]
	;mov bx, WORD [vbe_mode_information.width]
	;mov cx, WORD [vbe_mode_information.height]
	;mov dx, WORD [vbe_mode_information.pitch]
	;mov [_bpp], ax
	;mov [_width], bx
	;mov [_height], cx
	;mov [_pitch], dx
	;mov eax, [vbe_mode_information.frame_buffer]
	;mov [_frame_buffer], eax
	;mov esi, [_frame_buffer]
	;mov edi, esi

	;mov [edi], DWORD 0x00FFFFFF

	xor eax, eax
	xor ebx, ebx
	xor ecx, ecx
	xor edx, edx
	;call _draw
	;call _start

	jmp $


success:
	mov [0xb8000], byte "H"
	jmp $


%include "bootloader/requirements/vbe.asm"


times 3048-($-$$) db 0

