SECTION .text
global multiply

; Multiplies two numbers.
; arg0         = (int32_t) First operand.
; arg1         = (int32_t) Second operand.
; return value = (int32_t) Product of the two operands.
multiply:
  mov eax,edi
  mul esi
  ret