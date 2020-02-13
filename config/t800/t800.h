/* Definitions of target machine for GNU compiler for INMOS transputer family.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

   Written by Yury Shevchuk <sizif@botik.ru>

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */


/* This file is laid out along the manual (tm.texi), and is supposed
   to be read with printed manual in hand. Therefore it contains no
   common comments, just transputer-specific ones. */


/*************************************************************
 Controlling the Compilation Driver, @file{gcc}
*************************************************************/

/* #define SWITCH_TAKES_ARG(@var{char}) */
/* #define WORD_SWITCH_TAKES_ARG(@var{name}) */
/* #define SWITCHES_NEED_SPACES */
/* #define CPP_SPEC */
/* #define SIGNED_CHAR_SPEC */
/* #define CC1_SPEC */
/* #define CC1PLUS_SPEC */
/* #define ASM_SPEC */
/* #define ASM_FINAL_SPEC */
/* #define LINK_SPEC */
/* #define LIB_SPEC */
/* #define STARTFILE_SPEC */
/* #define ENDFILE_SPEC */
/* #define LINK_LIBGCC_SPECIAL */
/* #define RELATIVE_PREFIX_NOT_LINKDIR */
/* #define STANDARD_EXEC_PREFIX */
/* #define MD_EXEC_PREFIX */
/* #define STANDARD_STARTFILE_PREFIX */
/* #define MD_STARTFILE_PREFIX */
/* #define LOCAL_INCLUDE_DIR */
/* #define SYSTEM_INCLUDE_DIR */
/* #define STANDARD_INCLUDE_DIR */
/* #define INCLUDE_DEFAULTS */


/*************************************************************
 Run-time Target Specification
*************************************************************/

#define CPP_PREDEFINES  "-Dtransputer"

/* #define STDC_VALUE */

extern int target_flags;

/* configure can set this using -D in CFLAGS */
#ifndef TARGET_CPU_DEFAULT
#define TARGET_CPU_DEFAULT 800
#endif

#if TARGET_CPU_DEFAULT == 800
#define MASK_CPU_DEFAULT  MASK_CPU_CAPABILITIES_T800
#elif TARGET_CPU_DEFAULT == 805
#define MASK_CPU_DEFAULT  MASK_CPU_CAPABILITIES_T805
#elif TARGET_CPU_DEFAULT == 425
#define MASK_CPU_DEFAULT  MASK_CPU_CAPABILITIES_T425
#elif TARGET_CPU_DEFAULT == 450
#define MASK_CPU_DEFAULT  MASK_CPU_CAPABILITIES_T450
#elif TARGET_CPU_DEFAULT == 9000
#define MASK_CPU_DEFAULT  MASK_CPU_CAPABILITIES_T9000
#else
#error "Improper value for TARGET_CPU_DEFAULT.  Someone improved configure?"
#endif

/* Processor type to capabilities mask mapping */

#define MASK_CPU_CAPABILITIES_T800 \
  (MASK_HAVE_FPU | MASK_HAVE_FPENTRY)

#define MASK_CPU_CAPABILITIES_T805 \
  (MASK_HAVE_FPU | MASK_HAVE_FPENTRY | MASK_HAVE_POP)

#define MASK_CPU_CAPABILITIES_T425 \
  (MASK_HAVE_POP)

#define MASK_CPU_CAPABILITIES_T450 \
  (MASK_HAVE_POP | MASK_HAVE_GTU | MASK_HAVE_SIXTEEN \
   | MASK_HAVE_XTEND | MASK_HAVE_SLMUL)

#define MASK_CPU_CAPABILITIES_T9000 \
  (MASK_HAVE_FPU | MASK_HAVE_POP | MASK_HAVE_GTU \
   | MASK_HAVE_XTEND | MASK_HAVE_SIXTEEN)

#define MASK_CPU_CAPABILITIES_ALL \
  (0			\
   | MASK_HAVE_FPU	\
   | MASK_HAVE_FPENTRY	\
   | MASK_HAVE_FPGE	\
   | MASK_HAVE_POP	\
   | MASK_HAVE_GTU	\
   | MASK_HAVE_SIXTEEN	\
   | MASK_HAVE_XTEND	\
   | MASK_HAVE_SLMUL	\
  )

/* Masks for the -m switches */

#define MASK_USE_cmpqi			000000000001
#define MASK_DATASEG_PC_RELATIVE	000000000002
#define MASK_SHORT16			000000000004
#define MASK_SHORT32			000000000010
/* processor capabilities */
#define MASK_HAVE_FPU			000000001000
#define MASK_HAVE_FPENTRY		000000002000
#define MASK_HAVE_FPGE			000000004000
#define MASK_HAVE_POP			000000010000
#define MASK_HAVE_GTU			000000020000
#define MASK_HAVE_SIXTEEN		000000040000
#define MASK_HAVE_XTEND			000000100000
#define MASK_HAVE_SLMUL			000000200000

/* Debugging: enable cmpqi pattern */

#define TARGET_USE_cmpqi   (target_flags & MASK_USE_cmpqi)

/* Data segment access relative to PC (ldc/ldpi/ldnl) as opposed to
   access via pointer to the data segment start (ldl/ldnl) */

#define TARGET_DATASEG_PC_RELATIVE  (target_flags & MASK_DATASEG_PC_RELATIVE)
#define TARGET_DATASEG_BY_POINTER  (! TARGET_DATASEG_PC_RELATIVE)

/* Make short ints 16-bit wide.  This is closer to traditions, but
   gives inefficient code if the target lacks 16-bit operation support */

#define TARGET_SHORT16 \
  (target_flags & MASK_SHORT16 \
   || (target_flags & (MASK_SHORT32|MASK_HAVE_SIXTEEN) == MASK_HAVE_SIXTEEN))

/* Target has FP support at all (there are fp-registers) */

#define TARGET_HAVE_FPU  (target_flags & MASK_HAVE_FPU)

/* Target has the indirect fpu* instructions -- t800,t805.  The
   absence of this bit means the direct counterparts of the fpu*
   instructions are available -- t9000 */

#define TARGET_HAVE_FPENTRY  (target_flags & MASK_HAVE_FPENTRY)

/* Target has the fpge instruction */

#define TARGET_HAVE_FPGE  (target_flags & MASK_HAVE_FPGE)

/* Target has the pop instruction */

#define TARGET_HAVE_POP  (target_flags & MASK_HAVE_POP)

/* Target has the gtu instruction */

#define TARGET_HAVE_GTU  (target_flags & MASK_HAVE_GTU)

/* Target has the quick sign extension instructions (xbword, lbx,
   (and if HAVE_SIXTEEN) xsword, lsx)  */

#define TARGET_HAVE_XTEND  (target_flags & MASK_HAVE_XTEND)

/* Target has the 16-bit load/store/subscript instructions
   (ls,ss,ssub) -- t9000, t450 */

#define TARGET_HAVE_SIXTEEN  (target_flags & MASK_HAVE_SIXTEEN)

/* Target has signed long multiplication (slmul, sulmul) -- t450 */

#define TARGET_HAVE_SLMUL  (target_flags & MASK_HAVE_SLMUL)


#define TARGET_SWITCHES_DEFAULT \
  (0 \
   | MASK_USE_cmpqi \
   | MASK_DATASEG_PC_RELATIVE \
   | MASK_CPU_DEFAULT \
  )

#define TARGET_SWITCHES \
  {{"no-cmpqi",			-MASK_USE_cmpqi},		\
   {"dataseg-pc-relative",	+MASK_DATASEG_PC_RELATIVE},	\
   {"dataseg-by-pointer",	-MASK_DATASEG_PC_RELATIVE},	\
   {"short16",			-MASK_SHORT32},			\
   {"short16",			+MASK_SHORT16},			\
   {"short32",			-MASK_SHORT16},			\
   {"short32",			+MASK_SHORT32},			\
   {"t800", 			-MASK_CPU_CAPABILITIES_ALL},	\
   {"t800", 			+MASK_CPU_CAPABILITIES_T800},	\
   {"t805", 			-MASK_CPU_CAPABILITIES_ALL},	\
   {"t805", 			+MASK_CPU_CAPABILITIES_T805},	\
   {"t425", 			-MASK_CPU_CAPABILITIES_ALL},	\
   {"t425", 			+MASK_CPU_CAPABILITIES_T425},	\
   {"t450", 			-MASK_CPU_CAPABILITIES_ALL},	\
   {"t450", 			+MASK_CPU_CAPABILITIES_T450},	\
   {"t9000", 			-MASK_CPU_CAPABILITIES_ALL},	\
   {"t9000", 			+MASK_CPU_CAPABILITIES_T9000},	\
   {"fpu",			+MASK_HAVE_FPU},		\
   {"no-fpu",			-MASK_HAVE_FPU},		\
   {"fpentry",			+MASK_HAVE_FPENTRY},		\
   {"no-fpentry",		-MASK_HAVE_FPENTRY},		\
   {"fpge",			+MASK_HAVE_FPGE},		\
   {"no-fpge",			-MASK_HAVE_FPGE},		\
   {"pop",			+MASK_HAVE_POP},		\
   {"no-pop",			-MASK_HAVE_POP},		\
   {"gtu",			+MASK_HAVE_GTU},		\
   {"no-gtu",			-MASK_HAVE_GTU},		\
   {"sixteen",			+MASK_HAVE_SIXTEEN},		\
   {"no-sixteen",		-MASK_HAVE_SIXTEEN},		\
   {"xtend",			+MASK_HAVE_XTEND},		\
   {"no-xtend",			-MASK_HAVE_XTEND},		\
   {"slmul",			+MASK_HAVE_SLMUL},		\
   {"no-slmul",			-MASK_HAVE_SLMUL},		\
   SUBTARGET_SWITCHES						\
   {"",  TARGET_SWITCHES_DEFAULT | SUBTARGET_SWITCHES_DEFAULT}}

/* #define TARGET_OPTIONS  {} */

/* These are meant to be redefined by subtargets */
#define SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES_DEFAULT  0
#define SUBTARGET_OPTIONS

#define TARGET_VERSION fprintf (stderr, " (Transputer, TTOOLS syntax)");

/* #define OVERRIDE_OPTIONS */
/* #define OPTIMIZATION_OPTIONS(LEVEL) */


/*************************************************************
 Storage Layout
*************************************************************/

#define BITS_BIG_ENDIAN                 0
#define BYTES_BIG_ENDIAN                0
#define WORDS_BIG_ENDIAN                0
#define BITS_PER_UNIT                   8
#define BITS_PER_WORD                   32
/* - #define MAX_BITS_PER_WORD */
#define UNITS_PER_WORD                  4
#define POINTER_SIZE                    BITS_PER_WORD


/* Transputers most naturally operate on signed 32-bit values.  Tried
   to switch this off: gives slightly worse code sometimes, but it
   seems to be mostly due to the reuse of pseudos, which is not good
   with our insns that pop their inputs.  */

#define PROMOTE_MODE(MODE,UNSIGNEDP,TYPE) \
  {                                                 \
    if (GET_MODE_CLASS (MODE) == MODE_INT           \
        && GET_MODE_SIZE (MODE) < UNITS_PER_WORD)   \
      (MODE) = SImode;                              \
    (UNSIGNEDP) = 0;                                \
  }

#define PROMOTE_FUNCTION_ARGS
#define PROMOTE_FUNCTION_RETURN
/*-#define PROMOTE_FOR_CALL_ONLY */

#define PARM_BOUNDARY            BITS_PER_WORD
#define STACK_BOUNDARY           BITS_PER_WORD
#define FUNCTION_BOUNDARY        BITS_PER_UNIT
#define BIGGEST_ALIGNMENT        BITS_PER_WORD
#define BIGGEST_FIELD_ALIGNMENT  BITS_PER_WORD

/* #define MAX_OFILE_ALIGNMENT */
/* #define DATA_ALIGNMENT(type, basic_align) */
/* #define CONSTANT_ALIGNMENT(constant, basic_align) */

#define EMPTY_FIELD_BOUNDARY  BITS_PER_WORD

/* - #define STRUCTURE_SIZE_BOUNDARY */

#define STRICT_ALIGNMENT  1

/* Don't need this with the present definition of ROUND_TYPE_ALIGN  */
/* -#define PCC_BITFIELD_TYPE_MATTERS */

/* Inhibit bitfields to cross a word boundary for better access.  */
#define BITFIELD_NBYTES_LIMITED  1

/* #define ROUND_TYPE_SIZE(struct, size, align) */

/* Make structures at least word-aligned.  Otherwise we get bulky code
   when passing a byte-aligned structure as a function arg because of
   our STRICT_ALIGNMENT (see calls.c:expand_call()). Small structure
   assignment will also be better this way.  */

#define ROUND_TYPE_ALIGN(STRUCT, COMPUTED, SPECIFIED) \
  (MAX (MAX ((COMPUTED), (SPECIFIED)),                              \
        TREE_CODE (STRUCT) == RECORD_TYPE ? BITS_PER_WORD : 0))

/* #define MAX_FIXED_MODE_SIZE */
/* #define CHECK_FLOAT_VALUE(mode, value) */

#define TARGET_FLOAT_FORMAT  IEEE_FLOAT_FORMAT


/*************************************************************
 Layout of Source Language Data Types
*************************************************************/

/* Use default value */
/* #define INT_TYPE_SIZE */

/* T800 doesn't have 16-bit loads/stores, so supporting 16-bit shorts
   is no pleasure.  But it is still supported as an option for the
   sake of compatibility with alien librares and existing software
   that counts on short being half-word-wide.  */

#define SHORT_TYPE_SIZE  (TARGET_SHORT16 ? 16 : 32)

/* #define LONG_TYPE_SIZE */
/* #define LONG_LONG_TYPE_SIZE */
/* #define CHAR_TYPE_SIZE */
/* #define FLOAT_TYPE_SIZE */
/* #define DOUBLE_TYPE_SIZE */
/* #define LONG_DOUBLE_TYPE_SIZE */

/* lb loads char w/o sign extension; so this may lead to better code */
#define DEFAULT_SIGNED_CHAR  0

/* Let's have int-sized enums to simplify access */
#define DEFAULT_SHORT_ENUMS  0

/* #define SIZE_TYPE */
/* #define PTRDIFF_TYPE */
/* #define WCHAR_TYPE */
/* #define WCHAR_TYPE_SIZE */
/* #define OBJC_INT_SELECTORS */
/* #define OBJC_SELECTORS_WITHOUT_LABELS */

#define TARGET_BELL                     007
#define TARGET_BS                       010
#define TARGET_TAB                      011
#define TARGET_NEWLINE                  012
#define TARGET_VT                       013
#define TARGET_FF                       014
#define TARGET_CR                       015


/*************************************************************
 Register Usage
*************************************************************/

/*** Basic Caracteristics of Registers **********************/

/* Hard registers. Cannot use enum here because things like
        #if ARG_POINTER_REGNUM != FRAME_POINTER_REGNUM
   in emit-rtl.c won't work with enums.

   Two fake registers are added, for use as STACK_POINTER_REGNUM and
   ARG_POINTER_REGNUM.  Those are used in code generation and are
   eliminated in reload pass in favor of FRAME_POINTER_REGNUM (which
   is Wreg). */

#define R_AREG     (0)
#define R_BREG     (1)
#define R_CREG     (2)
#define R_FAREG    (3)
#define R_FBREG    (4)
#define R_FCREG    (5)
#define R_WREG     (6)
#define R_FAKE1    (7)
#define R_FAKE2    (8)

#define FIRST_PSEUDO_REGISTER           9

#define FIXED_REGISTERS \
  /*Areg,Breg,Creg,FAreg,FBreg,FCreg,Wreg,Fake1,Fake2*/ \
  {    0,   0,   0,    0,    0,    0,   1,    1,    1 }

/* No one survives a function call */

#define CALL_USED_REGISTERS \
  /*Areg,Breg,Creg,FAreg,FBreg,FCreg,Wreg,Fake1,Fake2*/ \
  {    1,   1,   1,    1,    1,    1,   1,   1,    1 }

/* ??? Reconsider this once setjmp is implemented... */

#define NON_SAVING_SETJMP  1

#define CONDITIONAL_REGISTER_USAGE \
  if (! TARGET_HAVE_FPU) {				\
      int i; 						\
      HARD_REG_SET x;					\
      COPY_HARD_REG_SET (x, reg_class_contents[(int)FLOAT_REGS]); \
      for (i = 0; i < FIRST_PSEUDO_REGISTER; i++ )	\
       if (TEST_HARD_REG_BIT (x, i)) 			\
	 fixed_regs[i] = call_used_regs[i] = 1; 	\
  }							\

/* -#define INCOMING_REGNO (out) */
/* -#define OUTGOING_REGNO (in) */


/*** Order of Allocation of Registers ***********************/

/* #define REG_ALLOC_ORDER */
/* #define ORDER_REGS_FOR_LOCAL_ALLOC */


/*** How Values Fit in Registers ****************************/

/* A single fp register is enough to hold any mode.  For integer
   registers, act in the standard way.  */
#define HARD_REGNO_NREGS(REGNO, MODE) \
  (FP_REGNO_P (REGNO) ? 1 : IN_WORDS(GET_MODE_SIZE (MODE)))

/* Disallow placing non-fp values in fp registers, since they are hard
   to access.  {TI,XF,TF,XC,TC}mode cannot go in a reg at all: fp regs do
   not support them and there's no enough integer regs to hold them.
   Everything else is OK.  */
#define HARD_REGNO_MODE_OK(REGNO, MODE) \
  (GET_MODE_UNIT_SIZE (MODE) <= 2*UNITS_PER_WORD                    \
   && ((REGNO) <= R_CREG                                            \
        || (GET_MODE_CLASS (MODE) == MODE_FLOAT                     \
            || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)))

#define MODES_TIEABLE_P(MODE1, MODE2) \
  (GET_MODE_CLASS (MODE1) == GET_MODE_CLASS (MODE2))


/*** Handling Leaf Functions ********************************/

/* #define LEAF_REGISTERS */
/* #define LEAF_REG_REMAP (regno) */

/*** Registers That Form a Stack ****************************/

/* reg-stack2.c requires that STACK_REGS be not just "defined", but
   rather defined to the number of register class containing all stack
   registers. */

#define STACK_REGS  STACK_REGS_

/* Amount of register stacks on the machine.  Transputers has two:
   [ABC]reg and F[ABC]reg.  On the models without FPU F[ABC]reg are
   considered fixed registers, so compiler doesn't touch them. */

#define STACK_REG_NSTACKS  2

/* Register stacks' boundaries.  The regs that belong to the same
   stack must have consequent numbers, the lowest numbered register
   being the stack top.  */

#define STACK_REG_FIRST(STACKNO)        ((STACKNO) == 0? R_AREG: R_FAREG)
#define STACK_REG_LAST(STACKNO)         ((STACKNO) == 0? R_CREG: R_FCREG)

/* Tell which register stack the stack register REGNO belongs to.  */

#define STACK_REG_STACKNO(REGNO)        ((REGNO) >= R_FAREG)

/* This macro tells stack-register convertor pass what is the proper
   reg-stack position for an insn operand.  The convertor inserts
   reg-stack shuffling insns before the insn to bring the insn's operands
   into the proper positions.

   Unlike REG_CLASS_FROM_LETTER, this macro may be asked about
   non-register operands containing a single stack register inside.
   In this case, it is expected to return the class appropriate for
   that register.
   
   'U' is an EXTRA_CONSTRAINT used for nonlocal memory references,
   that contain a stack register somewhere inside a MEM.  Indicate
   to the stack-reg converter that this register needs to be at the
   reg-stack top before executing the insn.  */

#define STACK_REG_CLASS_FROM_LETTER(C) \
  ((C) == 'r' ? GENERAL_REGS :                 \
   (C) == 'a' ? AREG :                         \
   (C) == 'b' ? BREG :                         \
   (C) == 'c' ? CREG :                         \
   (C) == 'f' && TARGET_HAVE_FPU ? FLOAT_REGS :\
   (C) == 't' && TARGET_HAVE_FPU ? FAREG :     \
   (C) == 'u' && TARGET_HAVE_FPU ? FBREG :     \
   (C) == 'v' && TARGET_HAVE_FPU ? FCREG :     \
   (C) == 'U' ? AREG :                         \
   (C) == 'R' ? AREG :                         \
   NO_REGS)

/* The following two macros handle `extra operands' that some insns
   may have.  The registers that are live before a call or function
   return are the extra operands.  Since these have no constraints,
   stack register convertor cannot obtain the information it needs
   for them in the regular way.  Instead, STACK_REG_EXTRA_OPERAND_CLASS
   is used to determine the proper reg-stack position for an extra operand,
   and STACK_REG_EXTRA_OPERAND_FLAGS describes various properties of
   the operand.  */

/* Require the regs to be in the natural order on the reg-stack before
   function call or return. */

#define STACK_REG_EXTRA_OPERAND_CLASS(INSN, REG) \
  REGNO_REG_CLASS (REGNO (REG))

/* Extra operands must always have the flag INPUT.  POPPED indicates
   that the insn automatically pops this operand off the stack. See
   reg-stack.h, reg-stack.c for more details.  */

#define STACK_REG_EXTRA_OPERAND_FLAGS(INSN, REG) \
  (GET_CODE (INSN) == CALL_INSN         ? (POPPED|INPUT) :  \
   GET_CODE (PATTERN (INSN)) == RETURN  ? (INPUT)                    :  \
   0)

/* Machine-dependent reg-stack shuffling routines called from reg-stack.new.  */

#define STACK_REG_EMIT_DROPS(STK, DROP_SET)   t800_emit_drops (STK, DROP_SET)
#define STACK_REG_EMIT_SWAPS(OLDSTK, NEWSTK)  t800_emit_swaps (OLDSTK, NEWSTK)
#define STACK_REG_EMIT_PUSHES(STK, PUSH_SET)  t800_emit_pushes (STK, PUSH_SET)

/* Tell reg-stack converter not to bother changing virtual stack
   register numbers to hard register numbers, because on transputers
   (unlike i387) it is not necessary.  Register operands never occur
   explicitly in transputer assembler syntax, but are instead implied
   by the insn which they are for. */

#define STACK_REGS_SUBSTITUTION_UNNECESSARY


/*** Obsolete Macros for Controlling Register Usage *********/

/* #define OVERLAPPING_REGNO_P(@var{regno}) */

#define INSN_CLOBBERS_REGNO_P(INSN, REGNO) \
  insn_clobbers_regno_p (INSN, REGNO)

/* #define PRESERVE_DEATH_INFO_REGNO_P(@var{regno}) */


/*************************************************************
 Register Classes
*************************************************************/

/* reg-stack2.c requires STACK_REGS to be #defined to the class
   containing all stack registers.  So we name the corresponding enum
   member differently to avoid failure if preprocessor does not permit
   "recursive" definitions.

   AB_REGS is the union of AREG ind BREG; it is used when handling
   insn with two commutative operands in AREG and BREG.
   FAB_REGS is its floating counterpart.  */

enum reg_class
{
    NO_REGS,
    AREG, BREG, CREG,
    FAREG, FBREG, FCREG,
    WREG,
    AB_REGS,                    /* [AB]reg: AREG+BREG */
    FAB_REGS,                   /* F[AB]reg: FAREG+FBREG */
    GENERAL_REGS,               /* Areg, Breg, Creg */
    FLOAT_REGS,                 /* FAreg, FBreg, FCreg */
    WORKSPACE_REGS,             /* Wreg, Fake1, Fake2 */
    BASE_REGS,                  /* Areg, Breg, Creg, Wreg, Fake1, Fake2 */
    STACK_REGS_,                /* [ABC]reg, F[ABC]reg */
    ALL_REGS,
    LIM_REG_CLASSES
};

#define N_REG_CLASSES                   (int) LIM_REG_CLASSES

#define REG_CLASS_NAMES \
  { "NO_REGS",                                          \
    "AREG", "BREG", "CREG",                             \
    "FAREG", "FBREG", "FCREG",                          \
    "WREG",                                             \
    "AB_REGS",                                          \
    "FAB_REGS",                                         \
    "GENERAL_REGS",                                     \
    "FLOAT_REGS",                                       \
    "WORKSPACE_REGS",                                   \
    "BASE_REGS",                                        \
    "STACK_REGS",                                       \
    "ALL_REGS" }

#define REG_CLASS_CONTENTS \
  { 0,                      /* NOREGS */                \
    001, 002, 004,          /* AREG, BREG, CREG */      \
    010, 020, 040,          /* FAREG, FBREG, FCREG */   \
    0100,                   /* WREG */                  \
    003,                    /* AB_REGS */               \
    030,                    /* FAB_REGS */              \
    007,                    /* GENERAL_REGS */          \
    070,                    /* FLOAT_REGS */            \
    0700,                   /* WORKSPACE_REGS */        \
    0707,                   /* BASE_REGS */             \
    0077,                   /* STACK_REGS */            \
    0777 }                  /* ALL_REGS */

#define REGNO_REG_CLASS(REGNO) \
  (REGNO <= R_WREG			\
     ? ((enum reg_class)((REGNO)+1))	\
     : (WORKSPACE_REGS))		\

#define BASE_REG_CLASS  BASE_REGS
#define INDEX_REG_CLASS  NO_REGS

#define REG_CLASS_FROM_LETTER(C) \
  (((C) == 'r' || (C) == 'a' || (C) == 'b' || (C) == 'c')                    \
     ? GENERAL_REGS :                                                        \
   TARGET_HAVE_FPU && ((C) == 'f' || (C) == 't' || (C) == 'u' || (C) == 'v') \
     ? FLOAT_REGS :                                                          \
   NO_REGS)

#define REGNO_OK_FOR_INDEX_P(REGNO)  0

#define REGNO_OK_FOR_BASE_P(REGNO) \
  (TEST_HARD_REG_BIT (reg_class_contents[(int) BASE_REGS],      \
                      ((REGNO) < FIRST_PSEUDO_REGISTER)         \
                        ? (REGNO) : reg_renumber[(REGNO)]))

#define PREFERRED_RELOAD_CLASS(X,CLASS) CLASS

/* #define PREFERRED_OUTPUT_RELOAD_CLASS (@var{x}, @var{class}) */
/* #define LIMIT_RELOAD_CLASS(@var{mode}, @var{class}) */

/* We need a secondary reload for an address of the stack slot
   corresponding to the failed pseudo which required the primary
   reload.  It is currently needed only when reloading an FP
   register.  QImode reloads are now done in SImode; it is safe
   because the stack slots for pseudos always get BIGGEST_ALIGNMENT,
   which is BITS_PER_WORD, so that QImode pseudos actually get
   SImode stack slots.

   true_regnum returns regno if X is a register and -1 otherwise;
   we don't need a scratch only if X is a hard reg.
   ... of if X is an (easy) constant.  */

#define SECONDARY_RELOAD_CLASS(CLASS, MODE, X) \
  ((t800_fp_class (CLASS)					\
    && (X) != CONST0_RTX (GET_MODE (X))				\
    && ((unsigned) true_regnum(X) >= FIRST_PSEUDO_REGISTER	\
        || ABCreg_operand (X, MODE)))				\
   ? GENERAL_REGS : NO_REGS)

#if 0 /* Although we need secondary memory for copying between general
	 and fp registers, we don't define this, since standard
	 handling of secondary memory reloads doesn't expect that
	 moving between fp registers and stack slots used as secondary
	 memory in turn requires a scratch general register.  Instead,
	 we handle such moves ourselves in reload_{in,out}*.  */

/* Copying between floating and any other reg requires an intermediate
   memory location on T800 */

#define SECONDARY_MEMORY_NEEDED(CLASS1, CLASS2, M) \
  secondary_memory_needed(CLASS1, CLASS2, M)

#endif

/*-#define SECONDARY_MEMORY_NEEDED_RTX (mode) */
/*-#define SECONDARY_MEMORY_NEEDED_MODE (mode) */

/* Allow using all registers for reloading since there are so
   few of them on T800.  */

#define SMALL_REGISTER_CLASSES

/* Don't leave extra pseudos for global-alloc: they have little
   chances to be allocated anyway.  Hope this will speed us up...  */

#define CLASS_LIKELY_SPILLED_P(class)  0

/* This is the size of MODE in words, except for the FP registers,
   where a single reg always suffices.  */

#define CLASS_MAX_NREGS(CLASS, MODE) \
 (t800_fp_class (CLASS) ? 1 : IN_WORDS (GET_MODE_SIZE (MODE)))

/* #define CLASS_CANNOT_CHANGE_SIZE */

/* 'I' stands for word-offset constant.  */

#define CONST_OK_FOR_LETTER_P(VALUE, C) \
  ((C) == 'I'? ((VALUE) % UNITS_PER_WORD) == 0 :	\
   0)

/* 'G' stands for 2.0, for `fpumulby2' and `fpudivby2';
   'H' stands for 2e32, for `fpuexpinc32' and `fpuexpdec32'.

   Yet another special floating value is 0.0, which is (unlike
   2.0 end 2e32) permitted by LEGITIMATE_CONSTANT_P.  It currently
   needs no constraint.  */

#define CONST_DOUBLE_OK_FOR_LETTER_P(VALUE, C) \
  fp_specval_ok_for_letter (VALUE, C)


/* 'S' stands for a local memory reference, which is either
     (mem (plus (Wreg) (offset)))  or  (mem (Wreg))

   'R' is a simple nonlocal memory reference, (mem (ABCreg))

   'U' is a general nonlocal memory reference, either (mem (ABCreg))
     or  (mem (plus (ABCreg) (offset)))

   'R' and 'U' are also recognized by STACK_REG_CLASS_FROM_LETTER so
   that reg-stack pass is able to find out the proper reg-stack
   position for the address register.  */

#define EXTRA_CONSTRAINT(X, C) \
  ((C) == 'S'? local_operand (X, GET_MODE (X)) :    		\
   (C) == 'R'? nonlocal_operand (X, GET_MODE (X)) :		\
   (C) == 'U'? nonlocal_plus_operand (X, GET_MODE (X)) :	\
   0)


/*************************************************************
 Describing Stack Layout and Calling Conventions
*************************************************************/

/*** Basic Stack Layout *************************************/

/* The stack layout in this hypothetical run-time model is as follows:
            +--------------------+
            |   arg N            |
            |   ...              |
            |   arg 0            |
            +--------------------+
            |   return addr      |
            +--------------------+
            |   local N          |
            |   ...              | (get_frame_size())
            |   local 0          |
            +--------------------+
            |   outgoing arg N   |
            |   ...              | (current_function_outgoing_args_size)
            |   outgoing arg 0   |
     Wreg ->+--------------------+
  */

#define STACK_GROWS_DOWNWARD
#undef FRAME_GROWS_DOWNWARD
#undef ARGS_GROW_DOWNWARD

#define STARTING_FRAME_OFFSET  WORD_ROUND (current_function_outgoing_args_size)

/* Leaving this undefined saves a lot of hair in allocate_dynamic_stack_space(),
   making no effect otherwise (it defaults to 0 in function.c) */
/* #define STACK_POINTER_OFFSET  (0) */

#define FIRST_PARM_OFFSET(FUNDECL)  (0)

/* See the comment for STACK_POINTER_OFFSET above */
/* #define STACK_DYNAMIC_OFFSET(FUNDECL) */

#define DYNAMIC_CHAIN_ADDRESS(FRAMEADDR)  ((void *)abort ())
/*-#define SETUP_FRAME_ADDRESSES () */
/*-#define RETURN_ADDR_RTX (count, frameaddr) */
/*-#define RETURN_ADDR_IN_PREVIOUS_FRAME */


/*** Registers That Address the Stack Frame *****************/

/* Stack pointer is a base reg for outgoing args;
   Frame pointer is a base reg for local variables (stack slots);
   Arg pointer is a base reg for incoming args.

   On the T800, all these areas are addressed with Wreg.  However, it
   is not good to define all three pointers to Wreg as GCC will
   duplicate arg pointer in a general reg if ARG_POINTER_REGNUM ==
   STACK_POINTER_REGNUM (see function.c).  Furthermore, the difference
   between frame and arg pointers becomes known only after all stack
   slots are allocated, i.e. in the reload pass. Therefore we use a
   separate (fake) register for argument pointer and let the reload
   pass eliminate it when the frame size becomes determined.  */

#define STACK_POINTER_REGNUM  R_FAKE2
#define FRAME_POINTER_REGNUM  R_WREG
/* #define HARD_FRAME_POINTER_REGNUM */
#define ARG_POINTER_REGNUM    R_FAKE1

/* ?? #define STATIC_CHAIN_REGNUM */
/* ?? #define STATIC_CHAIN_INCOMING_REGNUM */
/* ?? #define STATIC_CHAIN */
/* ?? #define STATIC_CHAIN_INCOMING */


/*** Eliminating Frame Pointer and Arg Pointer **************/

/* Wreg is the only register designated for addressing stack frame,
   and we use it as a frame pointer.  Arg pointer is a fake register
   which is eliminated to Wreg in reload pass.  For functions which do
   not call alloca() stack pointer is another fake register, also
   eliminated in favor of Wreg.  In functions that do call alloca() we
   use a pseudo for stack pointer, so the mentioned fake stack pointer
   register should not appear at all. */

#define FRAME_POINTER_REQUIRED  (1)

/*-#define INITIAL_FRAME_POINTER_OFFSET(DEPTH_VAR) */

#define ELIMINABLE_REGS \
  {{ ARG_POINTER_REGNUM, R_WREG},	\
   { STACK_POINTER_REGNUM, R_WREG}}

#define CAN_ELIMINATE(FROM, TO)  (1)

#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET_VAR) \
  switch (FROM)								\
    {									\
    case ARG_POINTER_REGNUM:						\
      (OFFSET_VAR) = WORD_ROUND (current_function_outgoing_args_size)	\
	+ WORD_ROUND (get_frame_size ())				\
	  + UNITS_PER_WORD;  /* caller's Iptr */			\
      break;								\
									\
    case STACK_POINTER_REGNUM:						\
      (OFFSET_VAR) = 0;							\
      break;								\
									\
    default:								\
      abort ();								\
    }

/* #define LONGJMP_RESTORE_FROM_STACK */


/*** Passing Function Arguments on the Stack ****************/

#define PROMOTE_PROTOTYPES

/*-#define PUSH_ROUNDING(npushed) */

#define ACCUMULATE_OUTGOING_ARGS

/*-#define REG_PARM_STACK_SPACE(FNDECL) */
/*-#define MAYBE_REG_PARM_STACK_SPACE */
/*-#define FINAL_REG_PARM_STACK_SPACE(const_size, var_size) */
/*-#define OUTGOING_REG_PARM_STACK_SPACE */
/*-#define STACK_PARMS_IN_REG_PARM_AREA */

#define RETURN_POPS_ARGS(FNDECL, FUNTYPE, STACK_SIZE)  0


/*** Passing Arguments in Registers *************************/

/* Normally the first three words of the arguments are passed in the
   registers of the integer reg-stack; those are pushed onto the stack
   by the `call' insn so that the callee will see all the arguments
   arriving on the stack.

   However, sometimes the caller needs to behave differently. First,
   when calling a function by a pointer rather than name, `gcall' insn
   is used instead of `call', which does not push integer reg-stack onto
   the stack.  So we tell calls.c to put all the parms onto the stack in
   this case.

   Second, there may be complex cases when we should pass some arg on
   the stack; in this case we pass all args on the stack to avoid the
   complexity.  This situation is detected by FUNCTION_ARG_PRESCAN and
   indicated by CUM.must_pass_in_stack.
   
   In this case, FUNCTION_ARG returns 0 for all args, forcing them
   onto the stack, and returns const0_rtx when called after
   processing args to produce next_arg_reg. This unusual value is
   a sign to the call patterns to adapt to this unusual situation. */

#define FUNCTION_ARG(CUM, MODE, TYPE, NAMED) \
  ((CUM).must_pass_in_stack                                         \
    ? ((TYPE) != void_type_node ? 0 : const0_rtx)                   \
    : ((CUM).lst_free_reg <= R_CREG                                 \
        ? gen_rtx (REG, (MODE), (CUM).lst_free_reg)                 \
        : 0 /* pass on stack */))

/* The called function finds all args on the stack. */

#define FUNCTION_INCOMING_ARG(CUM, MODE, TYPE, NAMED)  0

#define FUNCTION_ARG_PARTIAL_NREGS(CUM, MODE, TYPE, NAMED) \
  (! (CUM).must_pass_in_stack                                       \
   && (CUM).lst_free_reg <= R_CREG                                  \
   && (CUM).lst_free_reg + T800_ARG_SIZE (MODE, TYPE) > R_CREG + 1  \
   ? R_CREG + 1 - (CUM).lst_free_reg                                \
   : 0)                                                             \

/* There seems to be no need for this.  */

#define FUNCTION_ARG_PASS_BY_REFERENCE(CUM, MODE, TYPE, NAMED)  0


/* This macro has a chance to glance over all the arguments of the
   function being called before FUNCTION_ARG is asked where each
   particular argument should be passed.

   On transputers, it can command to pass everything on stack if there
   is an arg that would normally be passed in registers but cannot be
   passed there for some reason (eg MUST_PASS_IN_STACK says it
   shouldn't).

   We also choose to pass everything in stack if current function uses
   dynamic stack space allocation: in this case we have a pseudo stack
   pointer which should be swapped with Wreg just before the actual
   call, and we wouldn't have a scratch register required for this if
   we passed in registers as usual.

   If the passing on stack has been triggered, the `call' and
   `call_value' patterns will use `j' or `gcall' instead of `call' to
   avoid pushing [ABC]reg onto the stack.  */

#define FUNCTION_ARG_PRESCAN(CUM, MODE, TYPE, NAMED) \
  do {                                                                  \
    (CUM).must_pass_in_stack |= current_call_is_indirect;               \
    (CUM).must_pass_in_stack |= current_function_calls_alloca;          \
    if (! (CUM).must_pass_in_stack                                      \
        && (CUM).lst_free_reg <= R_CREG)                                \
      {                                                                 \
        (CUM).must_pass_in_stack |= MUST_PASS_IN_STACK (MODE, TYPE);    \
                                                                        \
        /* If it is a large object that we would pass part in regs,     \
           part on stack, it may be more efficient to pass it merely on \
           stack.  */                                                   \
									\
        if ((CUM).lst_free_reg == R_AREG                                \
            && T800_ARG_SIZE (MODE, TYPE) > 3)                          \
          (CUM).must_pass_in_stack = 1;                                 \
                                                                        \
        FUNCTION_ARG_ADVANCE (CUM, MODE, TYPE, NAMED);                  \
      }                                                                 \
  } while (0)

/* Helper macro used from other macros in this section.  */
#define T800_ARG_SIZE(MODE, TYPE) \
  IN_WORDS ((MODE) != BLKmode                           \
             ? GET_MODE_SIZE (MODE)                     \
             : int_size_in_bytes (TYPE))

typedef struct {
  /* The first arg_regno free after scanning the arguments so far.  */
  int lst_free_reg;

  /* The flag set by FUNCTION_ARG_PRESCAN saying we dont want to pass
     anything in registers for current call.  */
  int must_pass_in_stack;
} CUMULATIVE_ARGS;

#define INIT_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME) \
  ((CUM).lst_free_reg = R_AREG,                     \
   (CUM).must_pass_in_stack = 0)

/* Rewind CUMULATIVE_ARGS after FUNCTION_ARG_PRESCAN. */
#define RESET_CUMULATIVE_ARGS(CUM, FNTYPE, LIBNAME) \
  ((CUM).lst_free_reg = R_AREG)


/* - #define INIT_CUMULATIVE_INCOMING_ARGS(CUM, FNTYPE, LIBNAME) */

#define FUNCTION_ARG_ADVANCE(CUM, MODE, TYPE, NAMED) \
 do {                                                       \
   if (FUNCTION_ARG(CUM, MODE, TYPE, NAMED))                \
     (CUM).lst_free_reg += T800_ARG_SIZE (MODE, TYPE);      \
 } while (0)
  
/* #define FUNCTION_ARG_PADDING(@var{mode}, @var{type}) */
/* #define FUNCTION_ARG_BOUNDARY(@var{mode}, @var{type}) */

#define FUNCTION_ARG_REGNO_P(REGNO)  ((unsigned)(REGNO) <= R_CREG)


/*** How Scalar Function Values Are Returned ****************/

/* Integer values are returned on the integer reg-stack.  Returning
   floating values there is not efficient, since moves between
   integer and and floating regs are costly.  The rest two ways are to
   return either in a floating reg or in memory.  We choose the first:
   floating values are returned on the floating reg-stack.

   The `ret' instruction does not restore regs from stack, so the
   caller will see returns just where the callee has put them (hence
   FUNCTION_OUTGOING_VALUE not needed). */

/* - #define TRADITIONAL_RETURN_FLOAT */

/* This definition is agreed with PROMOTE_MODE & PROMOTE_FUNCTION_RETURN */

#define FUNCTION_VALUE(VALTYPE, FUNC) \
  gen_rtx (REG,                                                             \
           ((TREE_CODE (VALTYPE) == INTEGER_TYPE                            \
             || TREE_CODE (VALTYPE) == ENUMERAL_TYPE                        \
             || TREE_CODE (VALTYPE) == BOOLEAN_TYPE                         \
             || TREE_CODE (VALTYPE) == CHAR_TYPE                            \
             || TREE_CODE (VALTYPE) == POINTER_TYPE                         \
             || TREE_CODE (VALTYPE) == OFFSET_TYPE)                         \
            && TYPE_PRECISION (VALTYPE) < BITS_PER_WORD)                    \
           ? word_mode : TYPE_MODE (VALTYPE),                               \
           (TREE_CODE (VALTYPE) == REAL_TYPE && TARGET_HAVE_FPU)            \
                                  ? R_FAREG : R_AREG)

/* - #define FUNCTION_OUTGOING_VALUE(@var{valtype}, @var{func}) */

#define LIBCALL_VALUE(MODE) \
  gen_rtx (REG, MODE, (TARGET_HAVE_FPU					    \
                       && (GET_MODE_CLASS (MODE) == MODE_FLOAT              \
                           || GET_MODE_CLASS (MODE) == MODE_COMPLEX_FLOAT)) \
                      ? R_FAREG : R_AREG)

#define FUNCTION_VALUE_REGNO_P(REGNO) \
  ((REGNO) == R_AREG || (TARGET_HAVE_FPU && (REGNO) == R_FAREG))

/* #define APPLY_RESULT_SIZE */


/*** How Large Values Are Returned **************************/

/* Passing struct value address as invisible first arg seems
   to be the easiest way */

/* - #define RETURN_IN_MEMORY(TYPE) */
/* - #define DEFAULT_PCC_STRUCT_RETURN */
/* #define STRUCT_VALUE_REGNUM */
/* #define STRUCT_VALUE_INCOMING_REGNUM */

#define STRUCT_VALUE  0
#define STRUCT_VALUE_INCOMING  0

/* - #define PCC_STATIC_STRUCT_RETURN */


/*** Caller-Saves Register Allocation ***********************/

#define DEFAULT_CALLER_SAVES
/* #define CALLER_SAVE_PROFITABLE(REFS, CALLS) */


/*** Function Entry and Exit ********************************/

#define FUNCTION_PROLOGUE(file, frame_size) \
  {									\
    int totsize = IN_WORDS (frame_size) +				\
                  IN_WORDS (current_function_outgoing_args_size);	\
									\
    /* Unused on t800; we don't expect to handle this */		\
    if (current_function_pretend_args_size)				\
      abort ();								\
									\
    if (totsize)							\
      fprintf (file, "\tajw -%d\n", totsize);				\
  }

/* We must use `return' pattern instead of epilogue on T800, because
   jump to epilogue would clobber all regs, including the one carrying
   return value.  */

/* - #define FUNCTION_EPILOGUE(FILE, SIZE)  */

#define EXIT_IGNORE_STACK                   0

/* - #define DELAY_SLOTS_FOR_EPILOGUE */
/* - #define ELIGIBLE_FOR_EPILOGUE_DELAY(INSN, N) */


/*** Generating Code for Profiling **************************/

/* For the first time, put stubs here */

#define FUNCTION_PROFILER(FILE, LABELNO)                        {}
/* #define PROFILE_BEFORE_PROLOGUE */
#define FUNCTION_BLOCK_PROFILER(FILE, LABELNO)                  {}
#define BLOCK_PROFILER(FILE, BLOCKNO)                           {}


/*************************************************************
 Implementing the Varargs Macros
*************************************************************/

/* This is no problem on T800, since all the args are incoming
   on stack. Thus, standard implementation of VARARGS is applicable */

/* #define EXPAND_BUILTIN_SAVEREGS(ARGS) */
#define SETUP_INCOMING_VARARGS(CUM,MODE,TYPE,PRETEND_SIZE,NO_RTL) {}


/*************************************************************
 Trampolines for Nested Functions
*************************************************************/

/* Not implemented -- standard C/C++ should not need those */

#define TRAMPOLINE_TEMPLATE(FILE)                               abort ();
/* #define TRAMPOLINE_SECTION */
#define TRAMPOLINE_SIZE                                         1
/* #define TRAMPOLINE_ALIGNMENT */
#define INITIALIZE_TRAMPOLINE(ADDR, FNADDR, STATIC_CHAIN)       abort ();
/* - #define ALLOCATE_TRAMPOLINE(FP) */
/* #define INSN_CACHE_SIZE */
/* #define INSN_CACHE_LINE_WIDTH */
/* #define INSN_CACHE_DEPTH */
/* #define TRANSFER_FROM_TRAMPOLINE */


/*************************************************************
 Implicit Calls to Library Routines
*************************************************************/

/* #define MULSI3_LIBCALL */
/* #define DIVSI3_LIBCALL */
/* #define UDIVSI3_LIBCALL */
/* #define MODSI3_LIBCALL */
/* #define UMODSI3_LIBCALL */
/* #define MULDI3_LIBCALL */
/* #define DIVDI3_LIBCALL */
/* #define UDIVDI3_LIBCALL */
/* #define MODDI3_LIBCALL */
/* #define UMODDI3_LIBCALL */
/* #define TARGET_EDOM */
/* #define GEN_ERRNO_RTX */

/* Use mem{set,cpy} rather than b{zero,copy}. */
#define TARGET_MEM_FUNCTIONS

/* ? #define LIBGCC_NEEDS_DOUBLE */
/* ? #define FLOAT_ARG_TYPE */
/* ? #define FLOATIFY(@var{passed-value}) */
/* ? #define FLOAT_VALUE_TYPE */
/* ? #define INTIFY(@var{float-value}) */

/* - #define nongcc_SI_type */

/* perform_@dots{} */
/* #define NEXT_OBJC_RUNTIME */


/*************************************************************
 Addressing Modes
*************************************************************/

/* - #define HAVE_POST_INCREMENT */
/* - #define HAVE_PRE_INCREMENT */
/* - #define HAVE_POST_DECREMENT */
/* - #define HAVE_PRE_DECREMENT */

/* The only T800 commands permitting constant address are `j',
   `cj' and `call'.  The address in this case is a code label
    or a function symref.

   ??? This used to accept LABEL_REF's only.  Do those extra
   checks make any difference?  */

#if 0  /* Looks like 0 works just as well... */
#define CONSTANT_ADDRESS_P(X) \
  (GET_CODE (X) == LABEL_REF				\
   || (GET_CODE (X) == SYMBOL_REF			\
       && SYMBOL_REF_FLAG (X) == 0)			\
   || (GET_CODE (X) == CONST				\
       && ! t800_dataseg_symrefs_mentioned_p (X)))
#else
#define CONSTANT_ADDRESS_P(X)  0
#endif


#define MAX_REGS_PER_ADDRESS  1

/* This is not made a function since it uses REG_OK_FOR_BASE_P,
   which is call-place-sensitive.

   We accept Wreg-based addresses even in floating modes for the sake
   of ABCreg loads/stores in floating modes.

   We accept Wreg-based addresses and addresses with index in QI and
   HI modes, because a few patterns are able to handle such addresses
   with multi-insn sequences; this tends to make better code than
   standard reloading.  */

#define GO_IF_LEGITIMATE_ADDRESS(MODE, X, LABEL) \
  switch (GET_CODE (X))							\
    {									\
     rtx X1;								\
									\
     case REG:								\
       if (REG_OK_FOR_BASE_P (X))					\
	 goto LABEL;							\
       break;								\
									\
     case PLUS:								\
       if (GET_CODE (XEXP (X, 0)) != REG				\
	   || ! REG_OK_FOR_BASE_P (XEXP (X, 0)))			\
	 break;								\
									\
       X1 = XEXP (X, 1);						\
									\
       /* instruction set supports word offsets only */			\
									\
       if (GET_CODE (X1) == CONST_INT					\
	   && (INTVAL (X1) % UNITS_PER_WORD) == 0)			\
	 goto LABEL;							\
									\
       /* In dataseg-by-pointer model, constant expressions		\
	  involving data segment symbols are valid offsets */		\
									\
       if (! TARGET_DATASEG_BY_POINTER)					\
	 break;								\
									\
       if (GET_CODE (X1) == SYMBOL_REF					\
	   && SYMBOL_REF_FLAG (X1) == 1)				\
	 goto LABEL;							\
									\
       if (GET_CODE (X1) == CONST)					\
	 {								\
	   X1 = XEXP (X1, 0);						\
	   if (GET_CODE (X1) == PLUS					\
	       && GET_CODE (XEXP (X1, 1)) == CONST_INT			\
	       && (INTVAL (XEXP (X1, 1)) % UNITS_PER_WORD) != 0) 	\
	     break;							\
									\
	   /* The rest may be arbitrarily complex expression		\
	      of symrefs.  Assuming data and code segment labels 	\
	      never mix in CONST, a simple check for presence		\
	      of data segment symref should be enough.  */		\
									\
	   if (t800_dataseg_symrefs_mentioned_p (X1))			\
	     goto LABEL;						\
	 }								\
       }

#ifdef REG_OK_STRICT
#define REG_OK_FOR_INDEX_P(X)  0
#define REG_OK_FOR_BASE_P(X)   (REGNO_OK_FOR_BASE_P (REGNO (X)))
#else /* ! REG_OK_STRICT */
#define REG_OK_FOR_INDEX_P(X)  0
#define REG_OK_FOR_BASE_P(X) \
  (REGNO (X) >= FIRST_PSEUDO_REGISTER \
   || TEST_HARD_REG_BIT (reg_class_contents[(int) BASE_REGS], REGNO (X)))
#endif /* REG_OK_STRICT */

/* Make it a function for the sake of debugging */

#define LEGITIMIZE_ADDRESS(x, oldx, mode, win) \
  do {									\
    extern rtx t800_legitimize_address (rtx, rtx, enum machine_mode);	\
    rtx newx =  t800_legitimize_address (x, oldx, mode);		\
    if (newx != x)							\
      {									\
        x = newx;							\
        goto win;							\
      }									\
  } while (0)

/* ? maybe access to uneven address in word mode? */

#define GO_IF_MODE_DEPENDENT_ADDRESS(ADDR, LABEL)

/* The only immediate floating constant we can handle is 0.0 (with
   fpldzero{sn,db}).  Turn all others into memory constants.
   We could have a define_expand to load immediate DImode.  But I
   suspect we'll get better code by forcing them to memory, too,
   because DImode operations are done by parts, anyway.  */

#define LEGITIMATE_CONSTANT_P(X) \
  ((GET_MODE_CLASS (GET_MODE (X)) != MODE_FLOAT \
    || (X) == CONST0_RTX (GET_MODE (X))) \
   && GET_CODE (X) != CONST_DOUBLE)


/*************************************************************
 Condition Code Status
*************************************************************/

/*-#define CC_STATUS_MDEP */
/*-#define CC_STATUS_MDEP_INIT */
/*-#define NOTICE_UPDATE_CC(@var{exp}, @var{insn}) */
/*-#define EXTRA_CC_MODES */
/*-#define EXTRA_CC_NAMES */
/*-#define SELECT_CC_MODE(OP, X, Y) */

/* ??? probably we could make use of this */

/* #define CANONICALIZE_COMPARISON (code, op0, op1) */

/* #define REVERSIBLE_CC_MODE (mode) */


/*************************************************************
 Describing Relative Costs of Operations
*************************************************************/

/* ??? this is still very raw... */

/* Tell the truth about integer contant cost by computing the number
   of pfix insns required to load it.  Scale the cost down in order to
   ecourage the use of consts instead of pseudos which are likely to
   spill.  */

#define CONST_COSTS(X,CODE,OUTER_CODE) \
  case CONST_INT:                               \
    return t800_const_cost (INTVAL (X)) / 3;    \
  case LABEL_REF:                               \
  case SYMBOL_REF:                              \
  case CONST:                                   \
  case CONST_DOUBLE:                            \
    /* 0 makes the same effect as 4             \
       with current test suite */               \
    return 1;

/* #define RTX_COSTS(X, CODE, OUTER_CODE) */

/* Strange as it is, this seems to have no effect on the result
   of compilation of all the test cases.  */
#define ADDRESS_COST(ADDRESS) 0     /* t800_address_cost (ADDRESS) */

/* On the T800, copying between floating-point and fixed-point
   registers is expensive.  */
extern char register_move_cost[][FIRST_PSEUDO_REGISTER];

#define REGISTER_MOVE_COST(CLASS1, CLASS2) \
  (register_move_cost[CLASS1][CLASS2])

/* SImode move between reg of general reg-stack and stack slot is sometimes
   as fast as a reg-to-reg move. Byte and float-point moves require
   address reloading and are therefore declared more costly (is it the
   right way?).  */
#define MEMORY_MOVE_COST(MODE) \
  ((MODE) == SImode? 4 : 8)

/* With BRANCH_COST == 1, the compiler tries to use conditional jumps
   in logical expressions extensively, which is not too good since
   it may turn out being inv_cj comprising two insns.  Try making it
   a bit coster...  */

#define BRANCH_COST  2

/* Do word access whenever possible.  */
#define SLOW_BYTE_ACCESS  1

/* #define SLOW_ZERO_EXTEND */
/* #define SLOW_UNALIGNED_ACCESS */
/* #define DONT_REDUCE_ADDR */

/* Since `lb/sb' and `ldnl/stnl' pop the addresses, it's better to use
   `move' when more than one load+store is required.  Note that expr.c
   actually copies by pieces only if move_by_pieces_ninsns() < MOVE_RATIO,
   though the documentation makes to think this ought to be <=.
   So increase it by 1...  */

#define MOVE_RATIO                          (1+1)

/* `call' is better than `gcall' */
#define NO_FUNCTION_CSE  1
#define NO_RECURSIVE_FUNCTION_CSE  1

/* #define ADJUST_COST (@var{insn}, @var{link}, @var{dep_insn}, @var{cost}) */

/*************************************************************
 Dividing the Output into Sections (Texts, Data, @dots{})
*************************************************************/

#define TEXT_SECTION_ASM_OP  ".text"
#define DATA_SECTION_ASM_OP  ".data"

/* #define SHARED_SECTION_ASM_OP */
/* #define INIT_SECTION_ASM_OP */

#define BSS_SECTION_ASM_OP  ".bss"

/* Define the .bss section for ASM_OUTPUT_LOCAL to use. */

#define EXTRA_SECTIONS in_bss

#define EXTRA_SECTION_FUNCTIONS \
void								\
bss_section ()							\
{								\
  if (in_section != in_bss)					\
    {								\
      fprintf (asm_out_file, "%s\n", BSS_SECTION_ASM_OP);	\
      in_section = in_bss;					\
    }								\
}

/* #define READONLY_DATA_SECTION */
/* #define SELECT_SECTION(EXP, RELOC) */
/* #define SELECT_RTX_SECTION(MODE, RTX) */
/* #define JUMP_TABLES_IN_TEXT_SECTION */

/* This macro is called just after making RTX for data object whose
   declaration is DECL.  We use it to mark SYMBOL_REF with `1' in
   SYMBOL_REF_FLAG if is belongs to data segment, `0' if it is in text
   segment.  This info is useful in dataseg-by-pointer model; see movM
   patterns for details.  */

#define ENCODE_SECTION_INFO(decl)					\
  do {									\
    int data_symbol;                                                    \
                                                                        \
    if (TREE_CODE (decl) == FUNCTION_DECL)                              \
      data_symbol = 0;                                                  \
    else if (TREE_CODE (decl) == STRING_CST && flag_writable_strings)	\
      data_symbol = 1;                                                  \
    else if (TREE_CONSTANT (decl))                                      \
      data_symbol = 0;                                                  \
    else                                                                \
      data_symbol = 1;                                                  \
                                                                        \
    if (data_symbol)                                                    \
      {                                                                 \
        rtx rtl = (TREE_CODE_CLASS (TREE_CODE (decl)) != 'd'		\
                   ? TREE_CST_RTL (decl) : DECL_RTL (decl));		\
        SYMBOL_REF_FLAG (XEXP (rtl, 0)) = 1;				\
      }                                                                 \
  } while (0)

/* #define STRIP_NAME_ENCODING (@var{var}, @var{sym_name}) */


/*************************************************************
 Position Independent Code
*************************************************************/

/* #define PIC_OFFSET_TABLE_REGNUM */
/* #define FINALIZE_PIC */

/* do not bother for the first time */
#define LEGITIMATE_PIC_OPERAND_P(X)         0


/*************************************************************
 Defining the Output Assembler Language
*************************************************************/

/*** The Overall Framework of an Assembler File *************/

#define ASM_FILE_START(STREAM)              {}
#define ASM_FILE_END(STREAM)                {}
#define ASM_IDENTIFY_GCC(FILE)              {}
#define ASM_COMMENT_START                   "//"
#define ASM_APP_ON                          ""
#define ASM_APP_OFF                         ""
/* #define ASM_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */
/* #define ASM_OUTPUT_SOURCE_LINE(STREAM, LINE) */
/* #define ASM_OUTPUT_IDENT(@var{stream}, @var{string}) */
/* #define ASM_OUTPUT_SECTION_NAME (stream, string) */
/* #define OBJC_PROLOGUE */


/*** Output of Data *****************************************/

/* T800 does not have a float format wider that DOUBLE.  */

/*-#define ASM_OUTPUT_LONG_DOUBLE(STREAM, VALUE) */

#ifdef HOST_WORDS_BIG_ENDIAN
  /* Doubles on host are stored in memory with the high order word first. */		\
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  do { union { double d; int i[2];} u;                          	\
    u.d = (VALUE);                                              	\
    fprintf (STREAM, "\t.word 0x%x, 0x%x\t// double %.20g\n",   	\
             u.i[1], u.i[0], u.d);					\
  } while (0)
#else
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  do { union { double d; int i[2];} u;                          	\
    u.d = (VALUE);                                              	\
    fprintf (STREAM, "\t.word 0x%x, 0x%x\t// double %.20g\n",   	\
             u.i[0], u.i[1], u.d);                              	\
  } while (0)
#endif

#define ASM_OUTPUT_FLOAT(STREAM, VALUE) \
  do { union { float f; int i[2];} u;                           \
    u.f = (VALUE);                                              \
    fprintf (STREAM, "\t.word 0x%x\t// float %.12g\n",          \
             u.i[0], u.f);                                      \
  } while (0)

/*-#define ASM_OUTPUT_THREE_QUARTER_FLOAT (stream, value) */
/*-#define ASM_OUTPUT_SHORT_FLOAT (stream, value) */
/*-#define ASM_OUTPUT_BYTE_FLOAT (stream, value) */
/*-#define ASM_OUTPUT_QUADRUPLE_INT(STREAM, EXP) */
/*-#define ASM_OUTPUT_DOUBLE_INT(STREAM, EXP) */

#define ASM_OUTPUT_INT(STREAM, EXP) \
  (fprintf (STREAM, "\t.word "),        \
   output_addr_const (STREAM, EXP),		\
   putc ('\n', STREAM))

#define ASM_OUTPUT_SHORT(STREAM, EXP) \
  (GET_CODE (EXP) == CONST_INT                          		\
    ? fprintf (STREAM, TARGET_SHORT16? "\t.half %u\n": "\t.word %u\n",	\
	       INTVAL (EXP))    					\
    : (abort (),0))

#define ASM_OUTPUT_CHAR(STREAM, EXP) \
  (GET_CODE (EXP) == CONST_INT						\
    ? fprintf (STREAM,							\
               (unsigned char) INTVAL (EXP) >= ' '			\
               && INTVAL (EXP) != '\177'				\
                 ? "\t.byte '%c'\n"					\
                 : "\t.byte '\\%o'\n", (unsigned char) INTVAL (EXP))	\
    : (abort (),0))

#define ASM_OUTPUT_BYTE(STREAM, VALUE) \
  fprintf (STREAM, "\t.byte %u\n", (char) VALUE)

#define ASM_BYTE_OP                         ".byte"

/* Rely on defaults.h for this */

/*-#define ASM_OUTPUT_ASCII(STREAM, PTR, LEN) */

/* #define ASM_OUTPUT_POOL_PROLOGUE(FILE, FUNNAME, FUNDECL, SIZE) */
/* #define ASM_OUTPUT_SPECIAL_POOL_ENTRY (FILE, X, MODE, ALIGN, LABELNO, JUMPTO) */
/* #define IS_ASM_LOGICAL_LINE_SEPARATOR (C) */

#define ASM_OPEN_PAREN                      "("
#define ASM_CLOSE_PAREN                     ")"


/*** Output of Uninitialized Variables **********************/

/* #define ASM_OUTPUT_COMMON(STREAM, NAME, SIZE, ROUNDED) */

#define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGNMENT) \
  do {                                                              \
    if ((ALIGNMENT) > BITS_PER_UNIT)				    \
      fprintf (STREAM, "\t.align %u\n", (ALIGNMENT)/BITS_PER_UNIT); \
    fprintf (STREAM, ".comm ");                                     \
    assemble_name (STREAM, NAME);                                   \
    fprintf (STREAM, ", %u;\n", SIZE);                              \
  } while (0)

/* #define ASM_OUTPUT_SHARED_COMMON(STREAM, NAME, SIZE, ROUNDED) */
/* #define ASM_OUTPUT_LOCAL(STREAM, NAME, SIZE, ROUNDED) */

#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGNMENT) \
  do {                                                              \
    bss_section ();                                                 \
    if ((ALIGNMENT) > BITS_PER_UNIT)				    \
      fprintf (STREAM, "\t.align %u\n", (ALIGNMENT)/BITS_PER_UNIT); \
    ASM_OUTPUT_LABEL (STREAM, NAME);                                \
    fprintf (STREAM, "\t.byte ?[%u]\n", SIZE);                      \
  } while (0)

/* #define ASM_OUTPUT_SHARED_LOCAL(STREAM, NAME, SIZE, ROUNDED) */


/*** Output and Generation of Labels ************************/

#define ASM_OUTPUT_LABEL(STREAM, NAME) \
  do {                                                          \
    assemble_name (STREAM, NAME);                               \
    fprintf (STREAM, ":\n");                                    \
  } while (0)

/* #define ASM_DECLARE_FUNCTION_NAME(STREAM, NAME, DECL) */
/* #define ASM_DECLARE_FUNCTION_SIZE(STREAM, NAME, DECL) */
/* #define ASM_DECLARE_OBJECT_NAME(STREAM, NAME, DECL) */
/* #define ASM_FINISH_DECLARE_OBJECT (stream, decl, toplevel, atend) */

#define ASM_GLOBALIZE_LABEL(STREAM, NAME) \
  do {                                                          \
    fprintf (STREAM, ".globl ");                                \
    assemble_name (STREAM, NAME);                               \
    fprintf (STREAM, ";\n");                                    \
  } while (0)

#define ASM_OUTPUT_EXTERNAL(STREAM, DECL, NAME) \
  do {                                                          \
    fprintf (STREAM, ".globl ");                                \
    assemble_name (STREAM, NAME);                               \
    fprintf (STREAM, ";\n");                                    \
  } while (0)

#define ASM_OUTPUT_EXTERNAL_LIBCALL(STREAM, SYMREF) \
  do {                                                          \
    fprintf (STREAM, ".globl ");                                \
    assemble_name (STREAM, XSTR (SYMREF, 0));                   \
    fprintf (STREAM, ";\n");                                    \
  } while (0)

#define ASM_OUTPUT_LABELREF(STREAM, NAME) \
  fprintf (STREAM, "_%s", NAME)

#define ASM_OUTPUT_INTERNAL_LABEL(STREAM, PREFIX, NUM) \
  fprintf (STREAM, "%s@%u:\n", PREFIX, NUM)

#define ASM_GENERATE_INTERNAL_LABEL(STRING, PREFIX, NUM) \
  sprintf (STRING, "*%s@%u", PREFIX, NUM)

#define ASM_FORMAT_PRIVATE_NAME(OUTVAR, NAME, NUMBER) \
  sprintf ((OUTVAR) = (char *) alloca (strlen (NAME) + 12),     \
           "%s@@%u", NAME, NUMBER)

/* #define OBJC_GEN_METHOD_LABEL(BUF, IS_INST, CLASS_NAME, CAT_NAME, SEL_NAME) */


/*** Macros Controlling Initialization Routines ************/

/* #define INIT_SECTION_ASM_OP */
/* #define HAS_INIT_SECTION */
/* #define INVOKE__main */
/* #define ASM_OUTPUT_CONSTRUCTOR(STREAM, NAME) */
/* #define ASM_OUTPUT_DESTRUCTOR(STREAM, NAME) */
/* #define OBJECT_FORMAT_COFF */
/* #define OBJECT_FORMAT_ROSE */
/* #define REAL_NM_FILE_NAME */


/*** Output of Assembler Instructions ***********************/

/* As the T800 is a stack machine, assembler has no syntax
   for register names. Nevertheless, we should provide
   register names, eg for use in clobber section of `asm' statement */

#define REGISTER_NAMES \
  {"Areg","Breg","Creg","FAreg","FBreg","FCreg","Wreg","Fake1","Fake2"}

/* "Wreg[0]" may be mentioned in the clobbers section of asm_operands
   to indicate that the asm clobbers the word at the top of workspace.
   See ASM_SPECIAL_CLOBBER below for how this is handled.  */

#define ADDITIONAL_REGISTER_NAMES \
  {"Wreg[0]", -10}

/* ASM_SPECIAL_CLOBBER allows you to handle clobbers of something other
   than registers of their pseudonyms in the clobbers section of
   asm_operands.  Make ADDITIONAL_REGISTER_NAMES return some weird
   register number for the special clobber, and define
   ASM_SPECIAL_CLOBBER do something special when this number is seen in
   expand_asm_operands.  */

/* Some instructions that can be used in `asm' clobber the word near
   the stack top (Wreg[0]).  This word belongs to the outgoing
   arguments area, so the clobberage won't harm unless the asm occurs
   in one of the function's arguments.  ??? It should not occur in such
   context, because `asm's are statements, not expressions---but with
   GNU extensions one *can* put statements within expressions, so the
   danger exists.  But what the hell, people using asm are supposed
   to be conscious enough to avoid this.

   When such asm is seen we want to ensure that the outgoing arguments
   area is at least one word large; otherwise, the asm would clobber
   one of local variables or even the function return address.  This is
   what this macro is doing.  */

#define ASM_SPECIAL_CLOBBER(REGNO) \
  if (REGNO == -10)							\
    {									\
      if (current_function_outgoing_args_size < UNITS_PER_WORD)		\
	current_function_outgoing_args_size = UNITS_PER_WORD;		\
    }

  
/* #define ASM_OUTPUT_OPCODE(STREAM, PTR) */
/* #define FINAL_PRESCAN_INSN(INSN, OPVEC, NOPERANDS) */

/* On T800, there is no diversity of possible operands. An operand
   may be either an integer constant or symbol/label ref, that's all.
   The last case is in the competence of PRINT_OPERAND_ADDRESS, so that
   the former is the only one we should process here.
   Valid CODE characters are:
   w - scale byte offset down to word offset (divide by word size);
   */
#define PRINT_OPERAND(STREAM, X, CODE)  print_operand(STREAM, X, CODE)

/* No punctuation chars currently used in t800.md */
/* #define PRINT_OPERAND_PUNCT_VALID_P(CODE) */

/* The only valid address operands are symbol and label refs */
#define PRINT_OPERAND_ADDRESS(STREAM, X) \
  output_addr_const (STREAM, X);

/* #define DBR_OUTPUT_SEQEND(FILE) */

/* #define REGISTER_PREFIX */
/* #define LOCAL_LABEL_PREFIX */
/* #define USER_LABEL_PREFIX */
/* #define IMMEDIATE_PREFIX */

/* #define ASSEMBLER_DIALECT */

/* ???, only for profiling. Put a stub... */
#define ASM_OUTPUT_REG_PUSH(STREAM, REGNO)                      abort ()
#define ASM_OUTPUT_REG_POP(STREAM, REGNO)                       abort ()


/*** Output of Dispatch Tables ******************************/

#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, VALUE, REL) \
  do {								\
    extern int t800_expected_table_label;			\
								\
    if ((REL) == t800_expected_table_label)			\
      fprintf (STREAM, "\t.word L@%u-L@%ua\n", VALUE, REL);	\
  } while (0)

#define ASM_OUTPUT_ADDR_VEC_ELT(STREAM, VALUE) \
  abort ();

#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE) \
  do {								\
    ASM_OUTPUT_ALIGN (FILE, 2);					\
    ASM_OUTPUT_INTERNAL_LABEL (FILE, PREFIX, NUM);		\
  } while (0)

/* - #define ASM_OUTPUT_CASE_END(STREAM, NUM, TABLE) */


/*** Assembler Commands for Alignment ***********************/

/* #define ASM_OUTPUT_ALIGN_CODE(FILE) */
/* #define ASM_OUTPUT_LOOP_ALIGN(FILE) */

#define ASM_OUTPUT_SKIP(STREAM, NBYTES) \
  fprintf (STREAM, "\t.byte 0[%u];\n", NBYTES);
  
/* - #define ASM_NO_SKIP_IN_TEXT */

#define ASM_OUTPUT_ALIGN(STREAM, POWER) \
  fprintf (STREAM, "\t.align %u;\n", 1 << (POWER));


/*************************************************************
 Controlling Debugging Information Format
*************************************************************/


/*** Macros Affecting All Debugging Formats ****************/

#define DBX_REGISTER_NUMBER(REGNO)  (REGNO)

/* Leave those undefined for now, so that default computation be used.
   We will probably have to correct this, as we always eliminate the
   frame pointer  */
/* #define DEBUGGER_AUTO_OFFSET (@var{x}) */
/* #define DEBUGGER_ARG_OFFSET (@var{offset}, @var{x}) */
/* #define PREFERRED_DEBUGGING_TYPE */

/*** Specific Options for DBX Output ***********************/

/* Leave this disabled until ast learns to undersatnd stabs  */
/* #define DBX_DEBUGGING_INFO */
/* - #define XCOFF_DEBUGGING_INFO */

/* For the following, defaults seems to be OK for us.  */
/* #define DEFAULT_GDB_EXTENSIONS */
/* #define DEBUG_SYMS_TEXT */
/* #define ASM_STABS_OP */
/* #define ASM_STABD_OP */
/* #define ASM_STABN_OP */
/* #define DBX_NO_XREFS */
/* #define DBX_CONTIN_LENGTH */
/* #define DBX_CONTIN_CHAR */
/* #define DBX_STATIC_STAB_DATA_SECTION */
/* #define DBX_TYPE_DECL_STABS_CODE */
/* #define DBX_STATIC_CONST_VAR_CODE */
/* #define DBX_REGPARM_STABS_CODE */
/* #define DBX_REGPARM_STABS_LETTER */
/* #define DBX_MEMPARM_STABS_LETTER */
/* #define DBX_FUNCTION_FIRST */
/* #define DBX_LBRAC_FIRST */

/*** Open-Ended Hooks for DBX Format ***********************/

/* #define DBX_OUTPUT_LBRAC (@var{stream}, @var{name}) */
/* #define DBX_OUTPUT_RBRAC (@var{stream}, @var{name}) */
/* #define DBX_OUTPUT_ENUM (@var{stream}, @var{type}) */
/* #define DBX_OUTPUT_FUNCTION_END(STREAM, FUNCTION) */
/* #define DBX_OUTPUT_STANDARD_TYPES(SYMS) */

/*** File Names in DBX Format ******************************/

/* #define DBX_WORKING_DIRECTORY */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILENAME(STREAM, NAME) */
/* #define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY(STREAM, NAME) */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILE_END(STREAM, NAME) */
/* #define DBX_OUTPUT_SOURCE_FILENAME(STREAM, NAME) */

/*** Macros for SDB and DWARF Output ***********************/

/* #define SDB_DEBUGGING_INFO */
/* #define DWARF_DEBUGGING_INFO */
/* #define PUT_SDB_@dots{} */
/* #define SDB_DELIM */
/* #define SDB_GENERATE_FAKE */
/* #define SDB_ALLOW_UNKNOWN_REFERENCES */
/* #define SDB_ALLOW_FORWARD_REFERENCES */


/*************************************************************
 Cross Compilation and Floating Point Format
*************************************************************/

/* Fortunately, both Besta and T800 use IEEE floating-point format.
   As it is, we have nothing to do with this section */

/* #define REAL_VALUE_TYPE */
/* #define REAL_VALUES_EQUAL(X, Y) */
/* #define REAL_VALUES_LESS(X, Y) */
/* #define REAL_VALUE_LDEXP(X, SCALE) */
/* #define REAL_VALUE_FIX(X) */
/* #define REAL_VALUE_UNSIGNED_FIX(X) */
/* #define REAL_VALUE_FIX_TRUNCATE(X) */
/* #define REAL_VALUE_UNSIGNED_FIX_TRUNCATE(X) */
/* #define REAL_VALUE_ATOF(STRING) */
/* #define REAL_INFINITY */
/* #define REAL_VALUE_ISINF(X) */
/* #define REAL_VALUE_ISNAN(X) */
/* #define REAL_ARITHMETIC(OUTPUT, CODE, X, Y) */
/* #define REAL_VALUE_NEGATE(X) */
/* #define REAL_VALUE_TRUNCATE (@var{mode}, @var{x})
/* #define REAL_VALUE_TO_INT(LOW, HIGH, X) */
/* #define REAL_VALUE_FROM_INT(X, LOW, HIGH) */


/*************************************************************
 Miscellaneous Parameters
*************************************************************/

#if 0 /* Wait for predicates to stabilize... */
#define PREDICATE_CODES \
  {"local_operand", {SUBREG, MEM}},         \
  {"ABCreg_operand", {SUBREG, REG}},        \
  {"FABCreg_operand", {SUBREG, REG}},       \
  {"Wreg_operand", {SUBREG, REG}},          \
  {"word_offset_operand", {CONST_INT}},
#endif

#define CASE_VECTOR_MODE                    SImode

/* PC-relative case vector avoids a lot of fixup when loading the
   program and reduces fixup table size as well.  A couple of extra
   instructions for tablejump seems a reasonable price for this.  */

#define CASE_VECTOR_PC_RELATIVE

/* #define CASE_DROPS_THROUGH */
/* #define CASE_VALUES_THRESHOLD */

/* Define if operations between registers always perform the operation
   on the full register even if a narrower mode is specified.  */

#define WORD_REGISTER_OPERATIONS

/* Define if loading in MODE, an integral mode narrower than BITS_PER_WORD
   will either zero-extend or sign-extend.  The value of this macro should
   be the code that says which one of the two operations is implicitly
   done, NIL if none.  */

#define LOAD_EXTEND_OP(MODE)  ZERO_EXTEND

/* On T800, default mode is "round to nearest" for all commands */

#define IMPLICIT_FIX_EXPR  FIX_ROUND_EXPR

/* ?? #define FIXUNS_TRUNC_LIKE_FIX_TRUNC */

/* On T800, default mode is "round to nearest" for all commands */

#define EASY_DIV_EXPR  ROUND_DIV_EXPR

/* We do this with lb/sb or ld[n]l/st[n]l, ie maximum a word at a time.  */

#define MOVE_MAX  UNITS_PER_WORD

/* #define MAX_MOVE_MAX */

/* T800 uses shift conts "as is". Large counts make processor to
   hang for a long time (e.g. 0x7fffffff yields hang for as long
   as 4 min on T800/20). So, truncating user-specified count to
   reasonable value is a good job, let compiler do it. */

/*-#define SHIFT_COUNT_TRUNCATED */

/* ??? try and see... */

#define TRULY_NOOP_TRUNCATION(OUTPREC, INPREC)  1

#define STORE_FLAG_VALUE  1

/* #define FLOAT_STORE_FLAG_VALUE */

#define Pmode          SImode

#define FUNCTION_MODE  QImode

/* #define INTEGRATE_THRESHOLD(DECL) */

#define SCCS_DIRECTIVE

/* #define HANDLE_PRAGMA(STREAM) */
/* #define DOLLARS_IN_IDENTIFIERS */
/* #define NO_DOLLAR_IN_LABEL */
/* #define DEFAULT_MAIN_RETURN */
/* #define HAVE_ATEXIT */
/* #define EXIT_BODY */
/* #define INSN_SETS_ARE_DELAYED(insn) */
/* #define INSN_REFERENCES_ARE_DELAYED(insn) */
/* #define MACHINE_DEPENDENT_REORG(insn)  */



/*************************************************************
 Custom additions
*************************************************************/

/* Is the reg a floating reg? */
/* ??? is it used ? */
#define FP_REGNO_P(regno)   ((regno) >= 3 && (regno) <= 5)
#define FP_REG_P(X)         (REG_P (X) && FP_REGNO_P (REGNO (X)))

/* How much bytes below Wreg are used by transputer firmware.  Note
   that stack converter also uses 6 words below Wreg as scratch
   locations while rotating the stack, so the should not be set lower
   than that. */
#define WORKSPACE_RESERVED_BYTES  (7 * UNITS_PER_WORD)

/* Is the reg a stack reg? */
#define STACK_REG_P(X)      (REG_P (X) && REGNO(X) <= 5)

/* Convert size in bytes to size in words */
#define IN_WORDS(SIZE_IN_BYTES) \
  (((SIZE_IN_BYTES) + UNITS_PER_WORD - 1) / UNITS_PER_WORD)

#define WORD_ROUND(VALUE) \
  (((VALUE) + UNITS_PER_WORD - 1) & -UNITS_PER_WORD)

/* Check that an operand has a floating mode; used in condition
   of DEFINE_{INSN,EXPAND}s */

#define FLOAT_OPERAND(N) \
  (GET_MODE_CLASS (GET_MODE (operands[N])) == MODE_FLOAT)

/* This is where cmpM pattern saves its arguments so that
   following sCC or bCC can use them */
extern
struct t800_compare {
  struct rtx_def *op[2];   /* comparison operands */
  int fp;                  /* floating comparison required */
} t800_compare;

/* Point that a function's arguments should be processed (in calls.c)
   from last to first, so that the integer regstack will be in the
   proper order before a call insn; no stack rearrangement will be needed.
   It is not quite right to define this macro here as it was not
   intended for user redefinition, but this yields the desired effect. */

#define PUSH_ARGS_REVERSED	/* If it's last to first */

/* Define the order in which it is preferable to load registers onto a
   reg-stack.  Used from reg-stack.new when reordering load atoms.  */
#define STACK_REG_LOAD_ORDER(CLASS) \
  ((CLASS) == AB_REGS ?  AREG*2 + 1     :       \
   (CLASS) == FAB_REGS ? FAREG*2 + 1    :       \
   (CLASS)*2)

/* Temporary hack: tell if an insn is OK for reg-stack.new:check_rules()
   despite its having a non-popped input and a non-earlyclobbered output.
   Get rid of this eventually.  */

#define INSN_OK_FOR_RULE_3(insn) \
   (INSN_CODE (insn) == CODE_FOR__dup           \
    || INSN_CODE (insn) == CODE_FOR__fpdup      \
    || INSN_CODE (insn) == CODE_FOR__fpdup + 1)


/* This tells local-alloc not to tie registers, as it usually does. 
   This affects the distribution of registers: the regs that are
   first by allocation order are used more, the regs that are last by
   allocation order are used less. The final effect of this is that
   when reload spills the least used hard regno, this affects less
   pseudos, ie the spilling becomes less painful.

   Note that this makes local-alloc harder, since more qantities are
   generated. Also, tying is useless only for stack-like registers.  */

#define DONT_TIE_REGS


/* Enable use of free registers for reloading.  */

#define SMARTER_RELOAD_PASS


/* This means that jump insn clobbers some registers (rather unusual
   thing for a jump to do...). This tells reg-stack convertor to use
   an alternative technique when emitting reg-stack argeeing
   sequences. Besides, it inhibits jump optimization after the
   register allocation has been done. Maybe really only certain parts
   of jump_optimize need to be skipped on such machines--consider it
   later...  */

#define JUMP_CLOBBERS_REGS


/* Enable optional EXOTIC pass */

#define EXOTIC_PASS


/* This is called before generating rtl for every function.  */

#define INIT_EXPANDERS \
  { t800_temp_slot_init ();		\
    init_fp_specval ();			\
    t800_init_once_completed = 1; }

/* This allows instruction patterns to find out whether we're doing
   initialization (such as expr_init_once) or the actual RTL generation.  */

extern int t800_init_once_completed;

/* Tell combiner that we do not actually have some insns, in spite of the
   presence of the code in optab.  */

#define ABSENT_INSN_CODE(CODE) \
  ((CODE) == CODE_FOR_nothing   \
   || (CODE) == CODE_FOR_cmpqi	\
   || (CODE) == CODE_FOR_cmphi	\
  )

/* This is used as a condition in binary insn patterns, like add or
   mul, to theck that input operands are not the same register, which
   is illegal on t800.  x and y are the input operands.

   We allow operands to be in the same register prior to exotic to
   leave maximum freedom to optimizers.  Exotic will fix the things up. */

#define T800_DISTINCT_REGS(x, y) \
  (exotic_completed == 0 || ! reg_overlap_mentioned_p (x, y))

/* Generate RTX for data segment memory location to be used instead of
   plain SYMBOL_REF.  */

/* ??? This is currently for testing only.  The offset is
   intentionally weird */

#define T800_DATASEG_START_RTX \
  t800_gen_local (SImode, 4444444)

/* In ast, symbol always stands for absolute address, regardless of
   context.  But in dataseg-by-pointer mode we access data by adding
   segment start address to symbol's offset from the segment start;
   machine description arranges for data segment symbols appearing in
   this context only.  So when we are asked to print a data segment
   symbol, we print the expression for that symbol's offset from data
   segment start instead. */

#define T800_PRINT_OPERAND_SYMREF(file, x, code) \
  do {							\
    if (TARGET_DATASEG_BY_POINTER			\
	&& SYMBOL_REF_FLAG (x) == 1)			\
      {							\
	fputc ('(', file);				\
	output_addr_const (file, x);			\
	fputs ("-@@_DATA@start)", file);		\
      }							\
    else						\
      output_addr_const (file, x);			\
    if ((code) != 0 && (code) != 'w')			\
      abort ();						\
    if ((code) == 'w')					\
      fprintf (file, "/%d", UNITS_PER_WORD);		\
  } while (0)

/* Indicate that assembler can handle things like `adc .LL1-.LL2'. */

#define T800_AS_ADC_LABELDIFF_OK  1

/* C statements to output code for `return' pattern.  */

#define T800_OUTPUT_RETURN \
  {									\
    int totsize = IN_WORDS (get_frame_size ()) +			\
                  IN_WORDS (current_function_outgoing_args_size);	\
  									\
    if (current_function_pretend_args_size)				\
      abort ();								\
  									\
    if (totsize)							\
      {									\
        rtx xop[1] = { GEN_INT (totsize) };				\
        output_asm_insn ("ajw +%0", xop);				\
      }									\
    return "ret";							\
 }

/* Predicate expression which valid asm operands should conform to.
   If this macro is not defined, general_operand is used.

   We want something more restrictive than general_operand, because
   otherwise we may wind up needing to reload from/to nonlocal memory,
   and that is hard to handle.  First, we may run out of spills
   ("fixed or forbidden register was spilled").  Second, nonlocal
   output reloads are troublesome because store instruction pops the
   memory address, which is likely to be required for a later insn,
   which gets unhappy in the reg-stack pass.  */

#define ASM_OPERAND_PREDICATE(X, MODE) \
  (register_operand (X, MODE) || local_operand (X, MODE))

#define PSEUDO_STACK_POINTER




/* Prototypes for functions in t800.c */

#ifndef PROTO
#if defined (USE_PROTOTYPES) ? USE_PROTOTYPES : defined (__STDC__)
#define PROTO(ARGS) ARGS
#else
#define PROTO(ARGS) ()
#endif
#endif

/* `rtx' is not defined yet, because this file is processed before
   rtl.h. Define it temporarily for use in the prototypes.  */

#define rtx     struct rtx_def *

int t800_address_cost PROTO((rtx));
int t800_const_cost PROTO((int));

/* Cannot tell about enum machine_mode in prototype for it is not
   declared yet, and enum predefinition is forbidden.   */

rtx t800_gen_local ();
rtx t800_gen_local_address PROTO((int));
rtx t800_get_reloaded_address PROTO((rtx));
rtx force_ABCreg ();

#undef rtx
