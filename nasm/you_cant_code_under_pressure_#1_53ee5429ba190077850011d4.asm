SECTION .text
global double_integer

double_integer:
  mov eax,edi
  cmp eax,0d
  mov bx,2d
  jge positive
  neg eax
  mul bx
  neg eax
  jmp end
  positive:
  mul bx
  end:
  ret