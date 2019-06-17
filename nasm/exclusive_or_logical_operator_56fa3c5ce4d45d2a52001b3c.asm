SECTION .text
global xorf

;  Returns a boolean indicating whether one of the arguments is true.
;  arg0         = (bool) A boolean.
;  arg1         = (bool) A boolean.
;  return value = (bool) true if one argument is true, else false.
xorf:
  xor rdi,rsi
  mov rax,rdi
  ret