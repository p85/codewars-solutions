SECTION .text
global make_negative

make_negative:
  mov eax,edi
  cmp eax,0d
  jl done
  neg eax
  done:
  ret