SECTION .text
global simple_multiplication

; Multiplies and returns the argument by 8 if the argument is even, else 9 if the argument is odd.
; arg0         = (int64_t) The argument to multiply.
; return value = (int64_t) The result.
simple_multiplication:
  mov eax,edi
  mov ebx,8
  test eax,1
  jz even
  inc ebx
  even:
  mul ebx
  ret