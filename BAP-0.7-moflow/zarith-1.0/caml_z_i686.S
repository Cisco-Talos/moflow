/*
   Assembly version for the fast path of some functions in Z:
   - x86 target
   - System 5 ABI and assembly syntax
   - GNU as

 
   This file is part of the Zarith library 
   http://forge.ocamlcore.org/projects/zarith .
   It is distributed under LGPL 2 licensing, with static linking exception.
   See the LICENSE file included in the distribution.
   
   Copyright (c) 2010-2011 Antoine Miné, Abstraction project.
   Abstraction is part of the LIENS (Laboratoire d'Informatique de l'ENS),
   a joint laboratory by:
   CNRS (Centre national de la recherche scientifique, France),
   ENS (École normale supérieure, Paris, France),
   INRIA Rocquencourt (Institut national de recherche en informatique, France).
        
 */


        /* helper functions */
        /* **************** */

        
        /* optional underscope prefix for symbols */
#ifdef Z_UNDERSCORE_PREFIX
#define SYMB(x) _##x
#else
#define SYMB(x)  x
#endif

        /* function prolog & epilog */

#if defined(Z_ELF) || defined(Z_COFF)
#define FUNCTION_ALIGN 16
#endif
#if defined(Z_MACOS)
#define FUNCTION_ALIGN 4
#endif

#if defined(Z_ELF)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .type   SYMB(ml_as_z_##proc), @function; \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc) \
        .size   SYMB(ml_as_z_##proc), .-SYMB(ml_as_z_##proc)
#endif

#if defined(Z_COFF) || defined(Z_MACOS)
#define PROLOG(proc) \
        .text;   \
        .globl  SYMB(ml_as_z_##proc); \
        .align  FUNCTION_ALIGN; \
SYMB(ml_as_z_##proc):

#define EPILOG(proc)
#endif

        /* calling C functions */

#define C_JMP(proc) \
        jmp     SYMB(ml_z_##proc)


        /* operation counter */
        
#ifdef Z_PERF_COUNTER
#define OP \
        incl    SYMB(ml_z_ops_as)
#else
#define OP
#endif


        
        /* unary arithmetics */
        /* ***************** */

        /* neg */
        PROLOG(neg)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lneg
        not     %eax
        add     $3, %eax
        jo      .Lneg
        OP
        ret
.Lneg:
        C_JMP(neg)
        EPILOG(neg)


        /* abs */
        PROLOG(abs)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Labs
        test    %eax, %eax
        jns     .Labs2
        not     %eax
        add     $3, %eax
        jo      .Labs
.Labs2:
        OP
        ret
.Labs:
        C_JMP(abs)
        EPILOG(abs)

        
        /* succ */
        PROLOG(succ)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lsucc
        add     $2, %eax
        jo      .Lsucc
        OP
        ret
.Lsucc:
        C_JMP(succ)
        EPILOG(succ)


        /* pred */
        PROLOG(pred)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lpred
        sub     $2, %eax
        jo      .Lpred
        OP
        ret
.Lpred:
        C_JMP(pred)
        EPILOG(pred)


        
        
        /* binary arithmetics */
        /* ****************** */

        /* add */
        PROLOG(add)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Ladd
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Ladd
        dec     %eax
        add     %ecx, %eax
        jo      .Ladd
        OP
        ret
.Ladd:
        C_JMP(add)
        EPILOG(add)

 
        /* sub */
        PROLOG(sub)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lsub
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lsub
        sub     %ecx, %eax
        jo      .Lsub
        inc     %eax
        OP
        ret
.Lsub:
        C_JMP(sub)
        EPILOG(sub)

        
        /* mul */
        PROLOG(mul)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lmul
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lmul
        dec     %eax
        sar     %ecx
        imul    %ecx, %eax
        jo      .Lmul
        inc     %eax
        OP
        ret
.Lmul:
        C_JMP(mul)
        EPILOG(sub)

        
        /* div */
        PROLOG(div)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Ldiv
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Ldiv
        sar     %ecx
        jz      .Ldiv   /* division by zero */
        cmp     $-1, %ecx
        je      .Ldivneg
        sar     %eax
        cdq
        idiv    %ecx
        sal     %eax
        inc     %eax
        OP
        ret
.Ldivneg:
        /* division by -1, the only one that can overflow */
        not     %eax
        add     $3, %eax
        jo      .Ldiv
        OP
        ret
.Ldiv:
        C_JMP(div)
        EPILOG(div)

        
        /* rem */
        PROLOG(rem)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lrem
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lrem
        sar     %ecx
        jz      .Lrem   /* division by zero */
        cmp     $-1, %ecx
        je      .Lremneg
        sar     %eax
        cdq
        idiv    %ecx
        sal     %edx
        lea     1(%edx), %eax
        OP
        ret
.Lremneg:
        /* division by -1 */
        mov     $1, %eax
        OP
        ret
.Lrem:
        C_JMP(rem)
        EPILOG(rem)

        
        /* bit operations */
        /* ************** */

        
        /* not */
        PROLOG(lognot)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llognot
        dec     %eax
        not     %eax
        ret
.Llognot:
        C_JMP(lognot)
        EPILOG(lognot)


        /* or */
        PROLOG(logor)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogor
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogor
        or      %ecx, %eax
        OP
        ret
.Llogor:
        C_JMP(logor)
        EPILOG(logor)

        /* and */
        PROLOG(logand)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogand
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogand
        and     %ecx, %eax
        OP
        ret
.Llogand:
        C_JMP(logand)
        EPILOG(logand)

        
        /* xor */
        PROLOG(logxor)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Llogxor
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Llogxor
        xor     %ecx, %eax
        inc     %eax
        OP
        ret
.Llogxor:
        C_JMP(logxor)
        EPILOG(logxor)


        /* shift_left */
        PROLOG(shift_left)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lshift_left
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lshift_left
        sar     %ecx
        cmp     $31, %ecx
        jae     .Lshift_left
        dec     %eax
        sal     %cl, %eax
        mov     %eax, %edx
        sar     %cl, %edx
        inc     %edx
        cmp     4(%esp), %edx
        jne     .Lshift_left /* overflow */
        inc     %eax
        OP
        ret
.Lshift_left:
        C_JMP(shift_left)
        EPILOG(shift_left)


        /* shift_right */
        PROLOG(shift_right)
        mov     4(%esp), %eax
        test    $1, %eax
        jz      .Lshift_right
        mov     8(%esp), %ecx
        test    $1, %ecx
        jz      .Lshift_right
        sar     %ecx
        cmp     $31, %ecx
        jae     .Lshift_right2
        sar     %cl, %eax
        or      $1, %eax
        OP
        ret
.Lshift_right2:
        /* shift by 31 or more */
        test    %eax, %eax
        js      .Lshift_right3
        mov     $1, %eax
        OP
        ret
.Lshift_right3:
        mov     $-1, %eax
        OP
        ret
.Lshift_right:
        C_JMP(shift_right)
        EPILOG(shift_right)
