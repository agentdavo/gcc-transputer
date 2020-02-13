/* Definitions of target machine for GNU compiler for INMOS transputer,
   ACE EXPERT Transputer Runtime Model.
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


/* Include generic definitions for transputer family.  A number of
   macros are redefined in this file as needed for the Expert model. */

#include "t800/t800.h"


/*************************************************************
 Controlling the Compilation Driver, @file{gcc}
*************************************************************/

/*>#define SWITCH_TAKES_ARG (char) */
/*>#define WORD_SWITCH_TAKES_ARG (name) */

#undef SWITCHES_NEED_SPACES
#undef CPP_SPEC
#undef NO_BUILTIN_SIZE_TYPE
#undef NO_BUILTIN_PTRDIFF_TYPE
#undef SIGNED_CHAR_SPEC
#undef CC1_SPEC
#undef CC1PLUS_SPEC
#undef ASM_SPEC
#undef ASM_FINAL_SPEC
#undef LINK_SPEC

#undef LIB_SPEC
#define LIB_SPEC  "-F -lm -lanc -lc -lm -lcrt -lOS"
#undef STARTFILE_SPEC
#define STARTFILE_SPEC  "ldfile_n%s crt0.o%s"

#undef ENDFILE_SPEC
#undef LINK_LIBGCC_SPECIAL
#undef LINK_LIBGCC_SPECIAL_1
#undef RELATIVE_PREFIX_NOT_LINKDIR

/* This is passed via -D when compiling gcc.o, so we shoudn't #undef it */
/* #undef STANDARD_EXEC_PREFIX */

#undef MD_EXEC_PREFIX

/* This is passed via -D when compiling gcc.o, so we shoudn't #undef it */
/* #undef STANDARD_STARTFILE_PREFIX */

#undef MD_STARTFILE_PREFIX
#undef MD_STARTFILE_PREFIX_1
#undef LOCAL_INCLUDE_DIR
#undef SYSTEM_INCLUDE_DIR
#undef STANDARD_INCLUDE_DIR
#undef INCLUDE_DEFAULTS


/*************************************************************
 Run-time Target Specification
*************************************************************/

#undef CPP_PREDEFINES
#define CPP_PREDEFINES "-D__t800__ -Dunix"

/*>#define STDC_VALUE */
/*>extern int target_flags; */

/* Prologue should include stack extension stuff */
#define MASK_STACK_EXTEND	(010000000000)
#define TARGET_STACK_EXTEND	(target_flags & MASK_STACK_EXTEND)

/*>#define TARGET_SWITCHES */
/*>#define TARGET_OPTIONS */

#undef SUBTARGET_SWITCHES
#define SUBTARGET_SWITCHES \
  {"stack-extend",	 MASK_STACK_EXTEND},	\
  {"no-stack-extend",	-MASK_STACK_EXTEND},	\

#undef SUBTARGET_SWITCHES_DEFAULT
#define SUBTARGET_SWITCHES_DEFAULT \
  (MASK_STACK_EXTEND | MASK_SHORT16)

#undef TARGET_VERSION
#define TARGET_VERSION fprintf (stderr, " (T800, ACE syntax)");

#define OVERRIDE_OPTIONS \
  do {							\
    target_flags &= ~MASK_DATASEG_PC_RELATIVE;	\
  } while (0)

/*>#define OPTIMIZATION_OPTIONS (level) */
/*>#define CAN_DEBUG_WITHOUT_FP */


/*************************************************************
 Storage Layout
*************************************************************/

/*>#define BITS_BIG_ENDIAN */
/*>#define BYTES_BIG_ENDIAN */
/*>#define WORDS_BIG_ENDIAN */
/*>#define FLOAT_WORDS_BIG_ENDIAN */
/*>#define BITS_PER_UNIT */
/*>#define BITS_PER_WORD */
/*>#define MAX_BITS_PER_WORD */
/*>#define UNITS_PER_WORD */
/*>#define MAX_UNITS_PER_WORD */
/*>#define POINTER_SIZE */

/* ACE compiler is known to promote arguments to word, just like the
   generic description does.  Not sure about return values... */

/*>#define PROMOTE_MODE (m, unsignedp, type) */
/*>#define PROMOTE_FUNCTION_ARGS */
/*>#define PROMOTE_FUNCTION_RETURN */
/*>#define PROMOTE_FOR_CALL_ONLY */

/*>#define PARM_BOUNDARY */
/*>#define STACK_BOUNDARY */

#undef FUNCTION_BOUNDARY
#define FUNCTION_BOUNDARY  BITS_PER_WORD

/*>#define BIGGEST_ALIGNMENT */
/*>#define BIGGEST_FIELD_ALIGNMENT */
/*?#define MAX_OFILE_ALIGNMENT */
/*>#define DATA_ALIGNMENT (type, basic-align) */
/*>#define CONSTANT_ALIGNMENT (constant, basic-align) */
/*>#define EMPTY_FIELD_BOUNDARY */
/*>#define STRUCTURE_SIZE_BOUNDARY */
/*>#define STRICT_ALIGNMENT */
/*>#define PCC_BITFIELD_TYPE_MATTERS */
/*>#define BITFIELD_NBYTES_LIMITED */
/*>#define ROUND_TYPE_SIZE (struct, size, align) */
/*>#define ROUND_TYPE_ALIGN (struct, computed, specified) */
/*>#define MAX_FIXED_MODE_SIZE */
/*>#define CHECK_FLOAT_VALUE (mode, value, overflow) */
/*>#define TARGET_FLOAT_FORMAT */


/*************************************************************
 Layout of Source Language Data Types
*************************************************************/

/* #define INT_TYPE_SIZE */
/* #define MAX_INT_TYPE_SIZE */

/* Generic description uses 32-bit short for simplicity (for now at
   least), but ACE assumes short is 16-bit.  t9000 has support for 16
   bit loads/stores, but t4/t8 has not, so this will be costly...  */

#undef SHORT_TYPE_SIZE
#define SHORT_TYPE_SIZE  (BITS_PER_WORD/2)

/*>#define LONG_TYPE_SIZE */
/*>#define MAX_LONG_TYPE_SIZE */
/*>#define LONG_LONG_TYPE_SIZE */
/*>#define CHAR_TYPE_SIZE */
/*>#define MAX_CHAR_TYPE_SIZE */
/*>#define FLOAT_TYPE_SIZE */
/*>#define DOUBLE_TYPE_SIZE */
/*>#define LONG_DOUBLE_TYPE_SIZE */

#undef DEFAULT_SIGNED_CHAR
#define DEFAULT_SIGNED_CHAR  1

/*>?#define DEFAULT_SHORT_ENUMS */

/* This is consistent with /home/parix/include/sys/size_t.h */
#undef SIZE_TYPE
#define SIZE_TYPE  "unsigned int"

/* This is consistent with /home/parix/include/stddef.h */
#undef PTRDIFF_TYPE
#define PTRDIFF_TYPE  "int"

/* This is consistent with /home/parix/include/sys/wchar_t.h */
#undef WCHAR_TYPE
#define WCHAR_TYPE  "char"

#undef WCHAR_TYPE_SIZE
#define WCHAR_TYPE_SIZE  8

/*>#define MAX_WCHAR_TYPE_SIZE */
/*>#define OBJC_INT_SELECTORS */
/*>#define OBJC_SELECTORS_WITHOUT_LABELS */

/*>#define TARGET_BELL */
/*>#define TARGET_BS */
/*>#define TARGET_TAB */
/*>#define TARGET_NEWLINE */
/*>#define TARGET_VT */
/*>#define TARGET_FF */
/*>#define TARGET_CR */


/*************************************************************
 Register Usage
*************************************************************/


/*** Basic Characteristics of Registers ********************/

/*>#define FIRST_PSEUDO_REGISTER */
/*>#define FIXED_REGISTERS */
/*>#define CALL_USED_REGISTERS */
/*>#define CONDITIONAL_REGISTER_USAGE */
/*>?#define NON_SAVING_SETJMP */
/*>#define INCOMING_REGNO (out) */
/*>#define OUTGOING_REGNO (in) */

/*** Order of Allocation of Registers **********************/

/*>#define REG_ALLOC_ORDER */
/*>#define ORDER_REGS_FOR_LOCAL_ALLOC */

/*** How Values Fit in Registers ***************************/

/*>#define HARD_REGNO_NREGS (regno, mode) */
/*>#define HARD_REGNO_MODE_OK (regno, mode) */
/*>#define MODES_TIEABLE_P (mode1, mode2) */

/*** Handling Leaf Functions *******************************/

/*?#define LEAF_REGISTERS */
/*?#define LEAF_REG_REMAP (regno) */

/*** Registers That Form a Stack ***************************/

/*>#define STACK_REGS */
/*>#define FIRST_STACK_REG */
/*>#define LAST_STACK_REG */

/*** Obsolete Macros for Controlling Register Usage ********/

/*>#define OVERLAPPING_REGNO_P (regno) */
/*>#define INSN_CLOBBERS_REGNO_P (insn, regno) */
/*>#define PRESERVE_DEATH_INFO_REGNO_P (regno) */


/*************************************************************
 Register Classes
*************************************************************/

/*>enum reg_class */
/*>#define N_REG_CLASSES */
/*>#define REG_CLASS_NAMES */
/*>#define REG_CLASS_CONTENTS */
/*>#define REGNO_REG_CLASS (regno) */
/*>#define BASE_REG_CLASS */
/*>#define INDEX_REG_CLASS */
/*>#define REG_CLASS_FROM_LETTER (char) */
/*>#define REGNO_OK_FOR_BASE_P (num) */
/*>#define REGNO_OK_FOR_INDEX_P (num) */
/*>#define PREFERRED_RELOAD_CLASS (x, class) */
/*>#define PREFERRED_OUTPUT_RELOAD_CLASS (x, class) */
/*>#define LIMIT_RELOAD_CLASS (mode, class) */
/*>#define SECONDARY_RELOAD_CLASS (class, mode, x) */
/*>#define SECONDARY_INPUT_RELOAD_CLASS (class, mode, x) */
/*>#define SECONDARY_OUTPUT_RELOAD_CLASS (class, mode, x) */
/*>#define SECONDARY_MEMORY_NEEDED (class1, class2, m) */
/*>#define SECONDARY_MEMORY_NEEDED_RTX (mode) */
/*>#define SECONDARY_MEMORY_NEEDED_MODE (mode) */
/*>#define SMALL_REGISTER_CLASSES */
/*>#define CLASS_LIKELY_SPILLED_P (class) */
/*>#define CLASS_MAX_NREGS (class, mode) */
/*>#define CLASS_CANNOT_CHANGE_SIZE */
/*>#define CONST_OK_FOR_LETTER_P (value, c) */
/*>#define CONST_DOUBLE_OK_FOR_LETTER_P (value, c) */
/*>#define EXTRA_CONSTRAINT (value, c) */


/*************************************************************
 Stack Layout and Calling Conventions
*************************************************************/


/*** Basic Stack Layout ************************************/

/* The frame is laid out as follows:
                        +----------------------+
                        |           :          |
                        | last incoming arg    |
                        |           :          |
             argptr  -> | first incoming arg   |
                        | arguments size, bytes|  caller's frame
                        |           :          |
			| caller's entry point |
callee's frame link  -> | caller's frame-link  |
                        | <scratch>            |
                        +----------------------+
                        | resptr               |
        ARG_POINTER  -> | argptr               |
                        | MDLptr               |
			| caller's Iptr        |
                        +----------------------+
                        | local N              |
                        |           :          |
                        | local 0              |
                        | outgoing arg N       |
                        |           :          |
                        | outgoing arg 0       |
                        | outgoing args size   |
			| data segment ptr     |
			| callee's entry point |  callee's frame
			| callee's frame link  |
FRAME_POINTER (Wptr) -> | <scratch>            |
                        +----------------------+

Note that ARG_POINTER_REGNO points to the three hidden arguments, not
to the actual argument block.  That's because the offset from Wreg to
the actual argument block is not known until runtime (even worse, it
varies from caller to caller).

***/

/*>#define STACK_GROWS_DOWNWARD */
/*>#define FRAME_GROWS_DOWNWARD */
/*>#define ARGS_GROW_DOWNWARD */

#undef STARTING_FRAME_OFFSET
#define STARTING_FRAME_OFFSET \
  (STACK_POINTER_OFFSET + WORD_ROUND (current_function_outgoing_args_size))

/* When there are no outgoing arguments, we don't need "args size"
   word either.  */

#undef STACK_POINTER_OFFSET
#define STACK_POINTER_OFFSET \
  (current_function_outgoing_args_size? 5 : 4) * UNITS_PER_WORD

#undef FIRST_PARM_OFFSET
#define FIRST_PARM_OFFSET(fundecl)  0

/*?#define STACK_DYNAMIC_OFFSET(fundecl) */
/*?#define DYNAMIC_CHAIN_ADDRESS(frameaddr) */
#undef SETUP_FRAME_ADDRESSES 
#undef RETURN_ADDR_RTX/* (count, frameaddr) */
#undef RETURN_ADDR_IN_PREVIOUS_FRAME


/*** Registers That Address the Stack Frame  ***************/

/*>#define STACK_POINTER_REGNUM */
/*>#define FRAME_POINTER_REGNUM */
/*>#define HARD_FRAME_POINTER_REGNUM */
/*>#define ARG_POINTER_REGNUM */
#undef STATIC_CHAIN_REGNUM
#undef STATIC_CHAIN_INCOMING_REGNUM
/*?#define STATIC_CHAIN */
/*?#define STATIC_CHAIN_INCOMING */

/*** Eliminating Frame Pointer and Arg Pointer *************/

/*>#define FRAME_POINTER_REQUIRED */
#undef INITIAL_FRAME_POINTER_OFFSET /* (depth-var) */


#undef ELIMINABLE_REGS
#define ELIMINABLE_REGS \
  {{ ARG_POINTER_REGNUM, R_WREG},	\
   { STACK_POINTER_REGNUM, R_WREG}}

#undef CAN_ELIMINATE
#define CAN_ELIMINATE(FROM, TO)  1

#undef INITIAL_ELIMINATION_OFFSET
#define INITIAL_ELIMINATION_OFFSET(FROM, TO, OFFSET_VAR) \
  switch (FROM)								\
    {									\
    case ARG_POINTER_REGNUM:						\
      (OFFSET_VAR) = STACK_POINTER_OFFSET				\
	+ WORD_ROUND (current_function_outgoing_args_size)		\
	 + WORD_ROUND (get_frame_size ())				\
	  + UNITS_PER_WORD  /* MDLptr */				\
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

/* ??? Don't know yet how setjump/longjump are implemented in libc  */

/*?#define LONGJMP_RESTORE_FROM_STACK */


/*** Passing Function Arguments on the Stack ***************/

/* ? */
#define PROMOTE_PROTOTYPES

#undef PUSH_ROUNDING /* (npushed) */
/*>#define ACCUMULATE_OUTGOING_ARGS */
#undef REG_PARM_STACK_SPACE /* (fndecl) */
#undef MAYBE_REG_PARM_STACK_SPACE
#undef FINAL_REG_PARM_STACK_SPACE /* (const_size, var_size) */
#undef OUTGOING_REG_PARM_STACK_SPACE
#undef STACK_PARMS_IN_REG_PARM_AREA
/*>#define RETURN_POPS_ARGS (funtype, stack-size) */

/*** Passing Arguments in Registers ************************/

/* In EXPERT model all arguments are passed on stack. */

/* ??? Even for libcalls? */

#undef FUNCTION_ARG
#define FUNCTION_ARG(cum, mode, type, named)  0

#undef FUNCTION_INCOMING_ARG

#undef FUNCTION_ARG_PARTIAL_NREGS
#define FUNCTION_ARG_PARTIAL_NREGS(cum, mode, type, named)  0

#undef FUNCTION_ARG_PASS_BY_REFERENCE
#define FUNCTION_ARG_PASS_BY_REFERENCE(cum, mode, type, named)  0

#undef FUNCTION_ARG_PRESCAN /* (cum, mode, type, named) */
#undef FUNCTION_ARG_CALLEE_COPIES /* (cum, mode, type, named) */

#undef CUMULATIVE_ARGS
#define CUMULATIVE_ARGS  int

#undef INIT_CUMULATIVE_ARGS
#define INIT_CUMULATIVE_ARGS(cum, fntype, libname) \
  do {} while (0)

#undef INIT_CUMULATIVE_INCOMING_ARGS
#define INIT_CUMULATIVE_INCOMING_ARGS(cum, fntype, libname) \
  do {} while (0)

#undef FUNCTION_ARG_ADVANCE
#define FUNCTION_ARG_ADVANCE(cum, mode, type, named) \
  do {} while (0)

/*>#define FUNCTION_ARG_PADDING (mode, type) */
/*>#define FUNCTION_ARG_BOUNDARY (mode, type) */

#undef FUNCTION_ARG_REGNO_P
#define FUNCTION_ARG_REGNO_P(regno)  0


/*** How Scalar Function Values Are Returned ***************/

/* The function result values are returned in one of three ways:
  - integral and pointer values are returned in Areg.
  - floating-point values are returned in FAreg.
  - structured values are returned in a location allocated by the caller and
    pointed to by resptr.  Additionally, resptr is returned in Areg.
***/

/*?#define TRADITIONAL_RETURN_FLOAT */

#undef FUNCTION_VALUE
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
           TREE_CODE (VALTYPE) == REAL_TYPE ? R_FAREG : R_AREG)

/*>#define FUNCTION_OUTGOING_VALUE (valtype, func) */
/*>#define LIBCALL_VALUE (mode) */
/*>#define FUNCTION_VALUE_REGNO_P (regno) */
/* #define APPLY_RESULT_SIZE */


/*** How Large Values Are Returned *************************/

#undef RETURN_IN_MEMORY
#undef DEFAULT_PCC_STRUCT_RETURN

/* Caller puts the structure value address into Creg, and it gets
   pushed onto the stack by `call' insn.  However, Areg and Breg
   are loaded by the code output from `call' pattern, so we want
   the value address to be in Areg before the `call' pattern. */

#undef STRUCT_VALUE_REGNUM
#define STRUCT_VALUE_REGNUM  (R_AREG)

#undef STRUCT_VALUE
#undef STRUCT_VALUE_INCOMING_REGNUM

/* Callee finds the structure value address among the hidden arguments
   addressed with ARG_POINTER_REGNUM */

#undef STRUCT_VALUE_INCOMING
#define STRUCT_VALUE_INCOMING \
  gen_rtx (MEM, SImode,							\
	   plus_constant (gen_rtx (REG, SImode, ARG_POINTER_REGNUM),	\
			  UNITS_PER_WORD))

#undef PCC_STATIC_STRUCT_RETURN


/*** Caller-Saves Register Allocation **********************/

#undef DEFAULT_CALLER_SAVES
#undef CALLER_SAVE_PROFITABLE /* (refs, calls) */


/*** Function Entry and Exit *******************************/

#undef FUNCTION_PROLOGUE
#define FUNCTION_PROLOGUE(file, frame_size) \
  {									\
    int totsize = IN_WORDS (frame_size)					\
                  + IN_WORDS (current_function_outgoing_args_size)	\
		  + IN_WORDS (STACK_POINTER_OFFSET);			\
									\
    if (current_function_pretend_args_size)				\
      /* Unused on transputers; we don't expect to handle this */	\
      abort ();								\
									\
    if (TARGET_STACK_EXTEND)						\
      {									\
        rtx ajw_label = (rtx) gen_label_rtx ();				\
									\
	fprintf (file, "\tldlp 0\n");					\
	fprintf (file, "\tldl 1\n");					\
	fprintf (file, "\tldnl 5\n");					\
	fprintf (file, "\tdiff\n");					\
	fprintf (file, "\tldc %d\n", totsize*UNITS_PER_WORD);		\
	fprintf (file, "\tgt\n");					\
	fprintf (file, "\teqc 0\n");					\
	fprintf (file, "\tcj ");					\
        output_asm_label (ajw_label);					\
	fprintf (file, "\n");						\
									\
									\
        {								\
	  HOST_WIDE_INT preferred_fragment_size				\
	    /* Avoid too small fragments */				\
	    = (totsize < 0x200? 0x200: totsize);			\
									\
	  /* 1st arg to CRT_stack_extender is a flags set.  This	\
	     definition is according to include/sys/crt.h */		\
									\
          enum {							\
            RETURN_DOUBLE = 1,	/* REAL64 instead of REAL32 in FAreg */ \
            RETURN_AREG = 2,	/* result in Areg */			\
            RETURN_FAREG = 4	/* result in FAreg */			\
          };								\
									\
	  int flags = 0;						\
									\
	  if (current_function_returns_struct)				\
	    flags |= RETURN_AREG;	/* structure value address */	\
	  else if (current_function_return_rtx)				\
	    {								\
	      enum machine_mode m = GET_MODE (current_function_return_rtx); \
									\
	      if (GET_MODE_CLASS (m) == MODE_FLOAT			\
		  || GET_MODE_CLASS (m) == MODE_COMPLEX_FLOAT)		\
		flags |= RETURN_FAREG;					\
	      else							\
		flags |= RETURN_AREG;					\
									\
	      if (GET_MODE_SIZE (m) > UNITS_PER_WORD)			\
		flags |= RETURN_DOUBLE;					\
	    }								\
									\
	  fprintf (file, "\tldc %u\n", preferred_fragment_size*UNITS_PER_WORD); \
	  fprintf (file, "\tldc %u\n", totsize*UNITS_PER_WORD);		\
	  fprintf (file, "\tldc 0x%x\n", flags);			\
	}								\
									\
	fprintf (file, "\t.globl __CRT_stack_extender\n");		\
	fprintf (file, "\tcall __CRT_stack_extender\n");		\
        output_asm_label (ajw_label);					\
	fprintf (file, ":\n");						\
      }									\
    fprintf (file, "\tldl 1\n");					\
    fprintf (file, "\tldnl 1\n");	/* .data ptr */			\
    fprintf (file, "\tldc ");						\
    assemble_name (file, XSTR (XEXP (DECL_RTL (current_function_decl),0),0));\
    fprintf (file, "-2\n");						\
    fprintf (file, "\tldpi\n");		/* this finction entry point */	\
    fprintf (file, "\tldlp 5\n");	/* frame link */		\
    fprintf (file, "\tajw -%u\n", totsize);				\
    fprintf (file, "\tstl 1\n");	/* frame link */		\
    fprintf (file, "\tstl 2\n");	/* this function entry point */	\
    fprintf (file, "\tstl 3\n");	/* .data ptr */			\
									\
    /* Note: Wreg[4] is the args_size word.  See T800_OUTPUT__CALL */	\
  }

/*>-#define EXIT_IGNORE_STACK */
/*>-#define FUNCTION_EPILOGUE (file, size) */
/*>-#define DELAY_SLOTS_FOR_EPILOGUE */
/*>-#define ELIGIBLE_FOR_EPILOGUE_DELAY (insn, n) */


/*** Generating Code for Profiling *************************/

/* ??? ... later */
/* #define FUNCTION_PROFILER (file, labelno) */
/* #define PROFILE_BEFORE_PROLOGUE */
/* #define FUNCTION_BLOCK_PROFILER (file, labelno) */
/* #define BLOCK_PROFILER (file, blockno) */
/* #define BLOCK_PROFILER_CODE */


/*************************************************************
 Implementing the Varargs Macros
*************************************************************/

/*>#define EXPAND_BUILTIN_SAVEREGS (args) */
/*>#define SETUP_INCOMING_VARARGS (args_so_far, mode, type, */


/*************************************************************
 Trampolines for Nested Functions
*************************************************************/

/* ??? ... later */
/* #define TRAMPOLINE_TEMPLATE (file) */
/* #define TRAMPOLINE_SECTION */
/* #define TRAMPOLINE_SIZE */
/* #define TRAMPOLINE_ALIGNMENT */
/* #define INITIALIZE_TRAMPOLINE (addr, fnaddr, static_chain) */
/* #define ALLOCATE_TRAMPOLINE (fp) */
/* #define INSN_CACHE_SIZE */
/* #define INSN_CACHE_LINE_WIDTH */
/* #define INSN_CACHE_DEPTH */
/* #define CLEAR_INSN_CACHE (BEG, END) */
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
/* #define INIT_TARGET_OPTABS */
/* #define TARGET_EDOM */
/* #define GEN_ERRNO_RTX */

/* Use mem{set,cpy} rather than b{zero,copy}. */
#define TARGET_MEM_FUNCTIONS

/* #define LIBGCC_NEEDS_DOUBLE */
/* #define FLOAT_ARG_TYPE */
/* #define FLOATIFY (passed-value) */
/* #define FLOAT_VALUE_TYPE */
/* #define INTIFY (float-value) */
/* nongcc_SI_type */
/* nongcc_word_type */
/* perform_@dots{} */
/* #define NEXT_OBJC_RUNTIME */


/*************************************************************
 Addressing Modes
*************************************************************/

/*>#define HAVE_POST_INCREMENT */
/*>#define HAVE_PRE_INCREMENT */
/*>#define HAVE_POST_DECREMENT */
/*>#define HAVE_PRE_DECREMENT */
/*>#define CONSTANT_ADDRESS_P (x) */
/*>#define MAX_REGS_PER_ADDRESS */
/*>#define GO_IF_LEGITIMATE_ADDRESS (mode, x, label) */
/*>#define REG_OK_FOR_BASE_P (x) */
/*>#define REG_OK_FOR_INDEX_P (x) */
/*>#define LEGITIMIZE_ADDRESS (x, oldx, mode, win) */
/*>#define GO_IF_MODE_DEPENDENT_ADDRESS (addr, label) */
/*>#define LEGITIMATE_CONSTANT_P (x) */


/*************************************************************
 Condition Code Status
*************************************************************/

/*>#define CC_STATUS_MDEP */
/*>#define CC_STATUS_MDEP_INIT */
/*>#define NOTICE_UPDATE_CC (exp, insn) */
/*>#define EXTRA_CC_MODES */
/*>#define EXTRA_CC_NAMES */
/*>#define SELECT_CC_MODE (op, x, y) */
/*>#define CANONICALIZE_COMPARISON (code, op0, op1) */
/*>#define REVERSIBLE_CC_MODE (mode) */


/*************************************************************
 Describing Relative Costs of Operations
*************************************************************/

/*>#define CONST_COSTS (x, code, outer_code) */
/*>#define RTX_COSTS (x, code, outer_code) */
/*>#define ADDRESS_COST (address) */
/*>#define REGISTER_MOVE_COST (from, to) */
/*>#define MEMORY_MOVE_COST (m) */
/*>#define BRANCH_COST */
/*>#define SLOW_BYTE_ACCESS */
/*>#define SLOW_ZERO_EXTEND */
/*>#define SLOW_UNALIGNED_ACCESS */
/*>#define DONT_REDUCE_ADDR */
/*>#define MOVE_RATIO */
/*>#define NO_FUNCTION_CSE */
/*>#define NO_RECURSIVE_FUNCTION_CSE */
/*>#define ADJUST_COST (insn, link, dep_insn, cost) */


/*************************************************************
 Dividing the Output into Sections (Texts, Data, @dots{})
*************************************************************/

#undef TEXT_SECTION_ASM_OP
#define TEXT_SECTION_ASM_OP  "\t.text"

#undef DATA_SECTION_ASM_OP 
#define DATA_SECTION_ASM_OP  "\t.data"

/* #define SHARED_SECTION_ASM_OP */
/* #define INIT_SECTION_ASM_OP */

#undef BSS_SECTION_ASM_OP
#define BSS_SECTION_ASM_OP  "\t.bss"

/* Define the .bss section for ASM_OUTPUT_LOCAL to use. */

#undef EXTRA_SECTIONS
#define EXTRA_SECTIONS in_bss

#undef EXTRA_SECTION_FUNCTIONS
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
/* #define SELECT_SECTION (exp, reloc) */
/* #define SELECT_RTX_SECTION (mode, rtx) */
/* #define JUMP_TABLES_IN_TEXT_SECTION */
/* #define ENCODE_SECTION_INFO (decl) */
/* #define STRIP_NAME_ENCODING (var, sym_name) */


/*************************************************************
 Position Independent Code
*************************************************************/

/* #define PIC_OFFSET_TABLE_REGNUM */
/* #define PIC_OFFSET_TABLE_REG_CALL_CLOBBERED */
/* #define FINALIZE_PIC */
/* #define LEGITIMATE_PIC_OPERAND_P (x) */


/*************************************************************
 Defining the Output Assembler Language
*************************************************************/


/*** The Overall Framework of an Assembler File  ***********/

#undef ASM_FILE_START
#define ASM_FILE_START(stream)  {}
#undef ASM_FILE_END
#define ASM_FILE_END(stream)  {}

/* Use default definition for that */

#undef ASM_IDENTIFY_GCC(file)

#undef ASM_COMMENT_START
#define ASM_COMMENT_START  ";"

#undef ASM_APP_ON
#define ASM_APP_ON ""

#undef ASM_APP_OFF
#define ASM_APP_OFF ""

#undef ASM_OUTPUT_SOURCE_FILENAME
#define ASM_OUTPUT_SOURCE_FILENAME(stream, name) \
  fprintf (stream, "\t.file \"%s\"\n", name)

#undef ASM_OUTPUT_SOURCE_LINE
/* #define ASM_OUTPUT_SOURCE_LINE(stream, line) */

#undef ASM_OUTPUT_IDENT
/* #define ASM_OUTPUT_IDENT (stream, string) */

#undef ASM_OUTPUT_SECTION_NAME
/* #define ASM_OUTPUT_SECTION_NAME (stream, string) */

/*>#define OBJC_PROLOGUE */


/*** Output of Data ****************************************/

/*>-#define ASM_OUTPUT_LONG_DOUBLE(stream, value) */

#ifdef HOST_WORDS_BIG_ENDIAN
  /* Doubles on host are stored in memory with the high order word first. */		\
#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  do {								\
    union { double d; int i[2];} u;				\
    u.d = (VALUE);                                              \
    fprintf (STREAM, "\t.word 0x%x, 0x%x\t%s double %.20g\n",   \
             u.i[1], u.i[0], ASM_COMMENT_START, u.d);           \
  } while (0)
#else
#undef ASM_OUTPUT_DOUBLE
#define ASM_OUTPUT_DOUBLE(STREAM, VALUE) \
  do {								\
    union { double d; int i[2];} u;				\
    u.d = (VALUE);                                              \
    fprintf (STREAM, "\t.word 0x%x, 0x%x\t%s double %.20g\n",   \
             u.i[0], u.i[1], ASM_COMMENT_START, u.d);           \
  } while (0)
#endif /* HOST_WORDS_BIG_ENDIAN */

#undef ASM_OUTPUT_FLOAT
#define ASM_OUTPUT_FLOAT(STREAM, VALUE) \
  do {								\
    union { float f; int i[2];} u;				\
    u.f = (VALUE);                                              \
    fprintf (STREAM, "\t.word 0x%x\t%s float %.12g\n",          \
             u.i[0], ASM_COMMENT_START, u.f);			\
  } while (0)

#undef ASM_OUTPUT_THREE_QUARTER_FLOAT /* (stream, value) */
#undef ASM_OUTPUT_SHORT_FLOAT /* (stream, value) */
#undef ASM_OUTPUT_BYTE_FLOAT /* (stream, value) */
#undef ASM_OUTPUT_QUADRUPLE_INT /* (stream, exp) */
#undef ASM_OUTPUT_DOUBLE_INT /* (stream, exp) */

#undef ASM_OUTPUT_INT
#define ASM_OUTPUT_INT(stream, exp) \
  do {						\
    fprintf (stream, "\t.word ");		\
    output_addr_const (stream, exp);		\
    putc ('\n', stream);			\
  } while (0)

#undef ASM_OUTPUT_SHORT
#define ASM_OUTPUT_SHORT(stream, exp) \
  do {						\
    fprintf (stream, "\t.short ");		\
    output_addr_const (stream, exp);		\
    putc ('\n', stream);			\
  } while (0)

#undef ASM_OUTPUT_CHAR
#define ASM_OUTPUT_CHAR(stream, exp) \
  do {						\
    fprintf (stream, "\t.byte ");		\
    output_addr_const (stream, exp);		\
    putc ('\n', stream);			\
  } while (0)

#undef ASM_OUTPUT_BYTE
#define ASM_OUTPUT_BYTE(STREAM, VALUE) \
  fprintf (STREAM, "\t.byte %u\n", (unsigned char) (VALUE))

#undef ASM_BYTE_OP
#define ASM_BYTE_OP  ".byte"

/* Rely on defaults.h for this */

#undef ASM_OUTPUT_ASCII
/* #define ASM_OUTPUT_ASCII(stream, ptr, len) */

/* #define ASM_OUTPUT_POOL_PROLOGUE(file funname fundecl size) */
/* #define ASM_OUTPUT_SPECIAL_POOL_ENTRY(file, x, mode, align, labelno, jumpto) */

/* ACE assembler considers ';' to be comment start character, not a
   logical line separator.  Don't know if there is another char acting
   as logical line separator; assume there isn't.  */

#define IS_ASM_LOGICAL_LINE_SEPARATOR(c)  (0)

#undef ASM_OPEN_PAREN
#define ASM_OPEN_PAREN  "("

#undef ASM_CLOSE_PAREN
#define ASM_CLOSE_PAREN  ")"


/*** Output of Uninitialized Variables *********************/

#undef ASM_OUTPUT_COMMON
/* #define ASM_OUTPUT_COMMON(stream, name, size, rounded) */

#undef ASM_OUTPUT_ALIGNED_COMMON
#define ASM_OUTPUT_ALIGNED_COMMON(STREAM, NAME, SIZE, ALIGNMENT) \
  do {									\
    if ((ALIGNMENT) > BITS_PER_UNIT)					\
      fprintf (STREAM, "\t.align %u\n", (ALIGNMENT)/BITS_PER_UNIT);	\
    fprintf (STREAM, "\t.comm ");					\
    assemble_name (STREAM, NAME);					\
    fprintf (STREAM, ",%u\n", SIZE);					\
  } while (0)

#undef ASM_OUTPUT_SHARED_COMMON
/* #define ASM_OUTPUT_SHARED_COMMON (stream, name, size, rounded) */

#undef ASM_OUTPUT_LOCAL
/* #define ASM_OUTPUT_LOCAL(stream, name, size, rounded) */

#undef ASM_OUTPUT_ALIGNED_LOCAL
#define ASM_OUTPUT_ALIGNED_LOCAL(STREAM, NAME, SIZE, ALIGNMENT) \
  do {									\
    bss_section ();							\
    if ((ALIGNMENT) > BITS_PER_UNIT)					\
      fprintf (STREAM, "\t.align %u\n", (ALIGNMENT)/BITS_PER_UNIT); 	\
    assemble_name (STREAM, NAME);					\
    fprintf (STREAM, ":\n\t.space %u\n", SIZE);				\
  } while (0)

#undef ASM_OUTPUT_SHARED_LOCAL
/* #define ASM_OUTPUT_SHARED_LOCAL (stream, name, size, rounded) */


/*** Output and Generation of Labels ***********************/

#undef ASM_OUTPUT_LABEL
#define ASM_OUTPUT_LABEL(stream, name) \
  do {					\
    assemble_name (stream, name);	\
    fputs (":\n", stream);		\
  } while (0)

#undef ASM_DECLARE_FUNCTION_NAME
/* #define ASM_DECLARE_FUNCTION_NAME(stream, name, decl) */

#undef ASM_DECLARE_FUNCTION_SIZE
/* #define ASM_DECLARE_FUNCTION_SIZE(stream, name, decl) */

#undef ASM_DECLARE_OBJECT_NAME
/* #define ASM_DECLARE_OBJECT_NAME(stream, name, decl) */

#undef ASM_FINISH_DECLARE_OBJECT
/* #define ASM_FINISH_DECLARE_OBJECT(stream, decl, toplevel, atend) */

#undef ASM_GLOBALIZE_LABEL
#define ASM_GLOBALIZE_LABEL(stream, name) \
  do {						\
    fputs ("\t.globl ", stream);		\
    assemble_name (stream, name);		\
    fputc ('\n', stream);			\
  } while (0)

/* ACE compiler handles external references by mere globalizing the name */

#undef ASM_OUTPUT_EXTERNAL
#define ASM_OUTPUT_EXTERNAL(stream, decl, name) \
  ASM_GLOBALIZE_LABEL (stream, name)

#undef ASM_OUTPUT_EXTERNAL_LIBCALL
#define ASM_OUTPUT_EXTERNAL_LIBCALL(stream, symref) \
  do {							\
    fputs ("\t.globl ", stream);			\
    assemble_name (stream, XSTR (symref, 0));		\
    fputc ('\n', stream);				\
  } while (0)

#undef ASM_OUTPUT_LABELREF
#define ASM_OUTPUT_LABELREF(stream, name) \
  fprintf (stream, "_%s", name)

#undef ASM_OUTPUT_INTERNAL_LABEL
#define ASM_OUTPUT_INTERNAL_LABEL(stream, prefix, num) \
  fprintf (stream, ".L%s%u:\n", prefix, num)

#undef ASM_GENERATE_INTERNAL_LABEL
#define ASM_GENERATE_INTERNAL_LABEL(string, prefix, num) \
  sprintf (string, "*.L%s%u", prefix, num)

/* Parix as seems to dislike dots in the middle of a name, so
   traditional "%s.%d" won't do.  OK, we'll achieve uniquity
   exploiting the fact that C identifiers never start in a digit.
   ASM_OUTPUT_LABELREF will prepend '_' to this, thus making a valid
   identifier.. */

#undef ASM_FORMAT_PRIVATE_NAME
#define ASM_FORMAT_PRIVATE_NAME(outvar, name, number) \
  do {							\
    (outvar) = (char *) alloca (strlen (name) + 12);	\
    sprintf (outvar, "%d%s", number, name);		\
  } while (0)

/* defaults.h will take care of this if we define SET_ASM_OP */

#undef ASM_OUTPUT_DEF
/* #define ASM_OUTPUT_DEF (stream, name, value) */

#undef SET_ASM_OP
#define SET_ASM_OP  ".set"

/* #define OBJC_GEN_METHOD_LABEL (buf, is_inst, class_name, cat_name, sel_name) */


/*** Macros Controlling Initialization Routines ************/

/* ??? ... later */

/* #define INIT_SECTION_ASM_OP */
/* #define HAS_INIT_SECTION */
/* #define INVOKE__main */
/* #define ASM_OUTPUT_CONSTRUCTOR (stream, name) */
/* #define ASM_OUTPUT_DESTRUCTOR (stream, name) */
/* #define OBJECT_FORMAT_COFF */
/* #define OBJECT_FORMAT_ROSE */
/* #define REAL_NM_FILE_NAME */


/*** Output of Assembler Instructions **********************/

/*>#define REGISTER_NAMES */
/*>#define ADDITIONAL_REGISTER_NAMES */

/* Expert model always reserves Wreg[0] as a scratch location, so
   we don't need to do anything special for asm's that clobber Wreg[0] */

#undef ASM_SPECIAL_CLOBBER

/*>#define ASM_OUTPUT_OPCODE (stream, ptr) */
/*>#define FINAL_PRESCAN_INSN (insn, opvec, noperands) */
/*>#define PRINT_OPERAND (stream, x, code) */
/*>#define PRINT_OPERAND_PUNCT_VALID_P (code) */
/*>#define PRINT_OPERAND_ADDRESS (stream, x) */
/*>-#define DBR_OUTPUT_SEQEND(file) */
/*>-#define REGISTER_PREFIX */
/*>-#define LOCAL_LABEL_PREFIX */
/*>-#define USER_LABEL_PREFIX */
/*>-#define IMMEDIATE_PREFIX */
/*>-#define ASSEMBLER_DIALECT */
/*>#define ASM_OUTPUT_REG_PUSH (stream, regno) */
/*>#define ASM_OUTPUT_REG_POP (stream, regno) */


/*** Output of Dispatch Tables *****************************/

/* Note: the following two should generate the same labels as
   ASM_OUTPUT_INTERNAL_LABEL (stream, "L", value), sans trailing ":\n".
   Definitions for those in defaults.h are currently unusable because
   they use ASM_OUTPUT_INTERNAL_LABEL, and ":\n" it appends confuses
   assemblers. */

#undef ASM_OUTPUT_ADDR_DIFF_ELT
#define ASM_OUTPUT_ADDR_DIFF_ELT(STREAM, VALUE, REL) \
  do {								\
    extern int t800_expected_table_label;			\
								\
    if ((REL) == t800_expected_table_label)			\
      fprintf (STREAM, "\t.word\t.LL%u-.LL%ua\n", VALUE, REL);  \
  } while (0)

/*>-#define ASM_OUTPUT_ADDR_VEC_ELT(stream, value) */
/*>#define ASM_OUTPUT_CASE_LABEL(FILE, PREFIX, NUM, TABLE) */
/*>-#define ASM_OUTPUT_CASE_END(stream, num, table) */


/*** Assembler Commands for Alignment **********************/

/* Alignment for pieces of code reached only by jumping.  Seems worthy
   to give them word-align. */

#undef ASM_OUTPUT_ALIGN_CODE
#define ASM_OUTPUT_ALIGN_CODE(file) \
  fputs ("\t.align 4\n", file)

/* Likewise for top-of-the-loop labels */

#undef ASM_OUTPUT_LOOP_ALIGN
#define ASM_OUTPUT_LOOP_ALIGN(file) \
  fputs ("\t.align 4\n", file)

#undef ASM_OUTPUT_SKIP
#define ASM_OUTPUT_SKIP(stream, nbytes) \
  fprintf (stream, "\t.space %u\n", nbytes)

/* .space does the right thing (puts zeroes) even in .text segment.
   (as -l doesn't show that, but examining .o file with od suggests
   that this is the case) */

#undef ASM_NO_SKIP_IN_TEXT
/* #define ASM_NO_SKIP_IN_TEXT */

#undef ASM_OUTPUT_ALIGN
#define ASM_OUTPUT_ALIGN(stream, power) \
  fprintf (stream, "\t.align %u\n", 1<<(power))


/*************************************************************
 Controlling Debugging Information Format
*************************************************************/


/*** Macros Affecting All Debugging Formats ****************/

/* ??? ... later */
/* #define DBX_REGISTER_NUMBER (regno) */
/* #define DEBUGGER_AUTO_OFFSET (x) */
/* #define DEBUGGER_ARG_OFFSET (offset, x) */
/* #define PREFERRED_DEBUGGING_TYPE */

/*** Specific Options for DBX Output ***********************/

/* #define DBX_DEBUGGING_INFO */
/* #define XCOFF_DEBUGGING_INFO */
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
/* #define DBX_BLOCKS_FUNCTION_RELATIVE */

/*** Open-Ended Hooks for DBX Format ***********************/

/* #define DBX_OUTPUT_LBRAC (stream, name) */
/* #define DBX_OUTPUT_RBRAC (stream, name) */
/* #define DBX_OUTPUT_ENUM (stream, type) */
/* #define DBX_OUTPUT_FUNCTION_END (stream, function) */
/* #define DBX_OUTPUT_STANDARD_TYPES (syms) */

/*** File Names in DBX Format ******************************/

/* #define DBX_WORKING_DIRECTORY */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILENAME (stream, name) */
/* #define DBX_OUTPUT_MAIN_SOURCE_DIRECTORY (stream, name) */
/* #define DBX_OUTPUT_MAIN_SOURCE_FILE_END (stream, name) */
/* #define DBX_OUTPUT_SOURCE_FILENAME (stream, name) */

/*** Macros for SDB and DWARF Output ***********************/

/* #define SDB_DEBUGGING_INFO */
/* #define DWARF_DEBUGGING_INFO */
/* #define PUT_SDB_@dots{} */
/* #define SDB_DELIM */
/* #define SDB_GENERATE_FAKE */
/* #define SDB_ALLOW_UNKNOWN_REFERENCES */
/* #define SDB_ALLOW_FORWARD_REFERENCES */


/*************************************************************
 Cross Compilation and Floating Point
*************************************************************/

/* Both t8 and sparc have IEEE, so no need to bother with this (lucky me :) */

/* #define REAL_VALUE_TYPE */
/* #define REAL_VALUES_EQUAL (x, y) */
/* #define REAL_VALUES_LESS (x, y) */
/* #define REAL_VALUE_LDEXP (x, scale) */
/* #define REAL_VALUE_FIX (x) */
/* #define REAL_VALUE_UNSIGNED_FIX (x) */
/* #define REAL_VALUE_RNDZINT (x) */
/* #define REAL_VALUE_UNSIGNED_RNDZINT (x) */
/* #define REAL_VALUE_ATOF (string, mode) */
/* #define REAL_INFINITY */
/* #define REAL_VALUE_ISINF (x) */
/* #define REAL_VALUE_ISNAN (x) */
/* #define REAL_ARITHMETIC (output, code, x, y) */
/* #define REAL_VALUE_NEGATE (x) */
/* #define REAL_VALUE_TRUNCATE (mode, x) */
/* #define REAL_VALUE_TO_INT (low, high, x) */
/* #define REAL_VALUE_FROM_INT (x, low, high) */


/*************************************************************
 Miscellaneous Parameters
*************************************************************/

/*>#define PREDICATE_CODES */
/*>#define CASE_VECTOR_MODE */
/*>#define CASE_VECTOR_PC_RELATIVE */
/*>#define CASE_DROPS_THROUGH */
/*>#define CASE_VALUES_THRESHOLD */
/*>#define WORD_REGISTER_OPERATIONS */
/*>#define LOAD_EXTEND_OP (mode) */
/*>#define IMPLICIT_FIX_EXPR */
/*>#define FIXUNS_TRUNC_LIKE_FIX_TRUNC */
/*>#define EASY_DIV_EXPR */
/*>#define MOVE_MAX */
/*>#define MAX_MOVE_MAX */
/*>#define SHIFT_COUNT_TRUNCATED */
/*>#define TRULY_NOOP_TRUNCATION (outprec, inprec) */
/*>#define STORE_FLAG_VALUE */
/*>#define FLOAT_STORE_FLAG_VALUE */
/*>#define Pmode */
/*>#define FUNCTION_MODE */
/*>#define INTEGRATE_THRESHOLD (decl) */

/* Ignoring #sccs is better that barfing on it, I think... */

#define SCCS_DIRECTIVE

#undef NO_IMPLICIT_EXTERN_C
/* #define NO_IMPLICIT_EXTERN_C */

/*>#define HANDLE_PRAGMA(stream) */
/*>#define VALID_MACHINE_ATTRIBUTE(type, attributes, identifier) */
/*>#define COMP_TYPE_ATTRIBUTES(type1, type2) */
/*>#define SET_DEFAULT_TYPE_ATTRIBUTES(type) */

/*>#define DOLLARS_IN_IDENTIFIERS */

/* ACE as accepts both dollars and dots in labels, ok */

#undef NO_DOLLAR_IN_LABEL
#undef NO_DOT_IN_LABEL

/* don't think any UNIXish environment requires it */

#undef DEFAULT_MAIN_RETURN

/* 'nm [target]libc.a | grep atexit' finds it. */

#define HAVE_ATEXIT

#undef EXIT_BODY

/*>#define INSN_SETS_ARE_DELAYED(insn) */
/*>#define INSN_REFERENCES_ARE_DELAYED(insn) */
/*>#define MACHINE_DEPENDENT_REORG(insn)  */

/*************************************************************
 Custom additions
*************************************************************/

/* In Parix `as', `ldc x' loads offset to x from current pc (rather
   than absolute value of x).  This affects certain patterns in
   t800.md.  */

#define T800_LDC_SYMBOL_PC_RELATIVE

/* Parix `as' automatically scales symbols in ldl/ldnl/ldnlp operand
   expression.  Therefore we always output symrefs without scaling,
   which would be normally done for code == 'w' */

#undef T800_PRINT_OPERAND_SYMREF
#define T800_PRINT_OPERAND_SYMREF(file, x, code) \
  output_addr_const (file, x)

/* This overrides the standard handling of incoming arguments in
   function.c:assign_parms().

   copy_to_reg is important; otherwise, DECL_RTL for incoming
   arguments looks like (mem (mem (reg))), which is not a valid
   address, and code in fixup_var_refs_1 (when it happens to be
   called) comes up with an unrecognizable insn for it.  */

#undef SETUP_INCOMING_ARGS_RTX
#define SETUP_INCOMING_ARGS_RTX(internal_arg_pointer, second_time) \
  do {									\
    internal_arg_pointer = gen_rtx (MEM, Pmode, virtual_incoming_args_rtx); \
    RTX_UNCHANGING_P (internal_arg_pointer) = 1;			\
    if (! second_time)							\
      {									\
        internal_arg_pointer = copy_to_reg (internal_arg_pointer);	\
        RTX_UNCHANGING_P (internal_arg_pointer) = 1;			\
      }									\
  } while (0)

/* Generate RTX for data segment memory location to be used instead of
   plain SYMBOL_REF.  */

#undef T800_DATASEG_START_RTX
#define T800_DATASEG_START_RTX \
  (gen_rtx (MEM, Pmode,					\
	    gen_rtx (PLUS, Pmode,			\
		     gen_rtx (REG, Pmode, R_WREG),	\
		     GEN_INT (3 * UNITS_PER_WORD))))


/* Indicate that assembler cannot handle things like `adc .LL1-.LL2'. */

#undef T800_AS_ADC_LABELDIFF_OK
#define T800_AS_ADC_LABELDIFF_OK  0


/* C statements to output code for `return' pattern.  */

#undef T800_OUTPUT_RETURN
#define T800_OUTPUT_RETURN \
  int totsize = IN_WORDS (get_frame_size ()) +				\
                IN_WORDS (current_function_outgoing_args_size) +       	\
		IN_WORDS (STACK_POINTER_OFFSET);			\
  rtx xop[1];								\
									\
  if (current_function_pretend_args_size)				\
    abort ();								\
									\
  xop[0] = GEN_INT (totsize);						\
  output_asm_insn ("ajw +%0", xop);					\
									\
  /* ... structured values are returned in a location allocated by	\
     the caller and pointed to by resptr.  Additionally, resptr is	\
     returned in Areg */						\
									\
  if (current_function_returns_struct)					\
    {									\
      xop[0] = GEN_INT (4);						\
      output_asm_insn ("ldl %0", xop);					\
    }									\
  return "ret";


/* C statements to output code for `call' pattern(s).  */

#undef T800_OUTPUT__CALL
#define T800_OUTPUT__CALL \
  do {									\
    rtx xop[1];								\
									\
    /* if the functioin takes any args, fill the args_size word */	\
    if (operands[2] != const0_rtx) 					\
      {									\
        xop[0] = operands[2];						\
        output_asm_insn ("ldc %0", xop);				\
        output_asm_insn ("stl 4", xop);					\
      }									\
									\
    /* If the function being called returns an aggregate, Areg is	\
       preloaded with output location address (STRUCT_VALUE_REGNUM).	\
       Otherwise, Areg contains a garbage which I think is perfectly	\
       valid to pass as resptr in this case, it isn't going to be used	\
       anyway.								\
									\
       If the call doesn't take any args, we don't load argptr either	\
       -- unless the call is aggregate valued, in which case we need	\
       to load something for argptr in order for resptr to arrive in	\
       the proper register.  */						\
									\
    /* argptr */							\
    if (operands[2] != const0_rtx					\
        || GET_MODE (PATTERN (insn)) == BLKmode)			\
      {									\
        xop[0] = GEN_INT (WORD_ROUND (STACK_POINTER_OFFSET));		\
        output_asm_insn ("ldlp %w0", xop);				\
      }									\
									\
    /* MDLptr */							\
    xop[0] = GEN_INT (1 * UNITS_PER_WORD  /* MDLptr offset */		\
		      + WORD_ROUND (get_frame_size ())			\
		      + WORD_ROUND (current_function_outgoing_args_size)\
		      + WORD_ROUND (STACK_POINTER_OFFSET));		\
    output_asm_insn ("ldl %w0", xop);					\
									\
    output_asm_insn ("call %1", operands);				\
									\
    return "";								\
  } while (0)


/* C statements to output code for `gcall' pattern(s).

   For gcall, we have to do by hand what normal `call' does
   automatically, including storing return address on the stack */

#undef T800_OUTPUT__GCALL
#define T800_OUTPUT__GCALL \
  do {									\
    rtx xop[1];								\
									\
    output_asm_insn ("ajw -4", xop);					\
									\
    /* If the function being called returns an aggregate, Areg is	\
       preloaded with output location address (STRUCT_VALUE_REGNUM),	\
       while function address is shifted to Breg.  If so, we first	\
       store Areg down to resptr, which also shifts function address	\
       to its normal position (Areg).					\
									\
       For non-aggregate-valued calls, we don't write resptr: it isn't	\
       going to be used then anyway.					\
									\
       For argumentless calls, we don't write argptr. */		\
									\
    /* resptr */							\
    if (GET_MODE (XEXP (XVECEXP (PATTERN (insn), 0, 0), 1)) != VOIDmode)\
      output_asm_insn ("stl 3", xop);					\
									\
    /* argptr */							\
    if (operands[2] != const0_rtx)					\
      {									\
        xop[0] = GEN_INT (WORD_ROUND (STACK_POINTER_OFFSET)		\
                          + 4 * UNITS_PER_WORD);  /* ajw compensation */\
        output_asm_insn ("ldlp %w0", xop);				\
        output_asm_insn ("stl 2", xop);					\
      }									\
									\
    /* MDLptr */							\
    xop[0] = GEN_INT (1 * UNITS_PER_WORD  /* MDLptr offset */		\
		      + WORD_ROUND (get_frame_size ())			\
		      + WORD_ROUND (current_function_outgoing_args_size)\
		      + WORD_ROUND (STACK_POINTER_OFFSET)		\
		      + 4 * UNITS_PER_WORD);  /* ajw compensation */	\
    output_asm_insn ("ldl %w0", xop);					\
    output_asm_insn ("stl 1", xop);					\
									\
    /* return address */						\
    output_asm_insn ("ldc 2", xop);					\
    output_asm_insn ("ldpi", xop);					\
    output_asm_insn ("stl 0", xop);					\
									\
    /* alternative 0 is for address in Areg, alternative 1 for		\
       constant address.  The latter uses `j' instead of `gcall' */	\
									\
    if (which_alternative == 0)						\
      output_asm_insn ("gcall", xop);					\
    else								\
      output_asm_insn ("j %1", operands);				\
									\
    return "";								\
  } while (0)

/* Parix AS doesn't know what `pfix' is.  Let's put it simpler...  */

#undef T800_OUTPUT_NOP
#define T800_OUTPUT_NOP  return ".byte 0x20";

#undef T800_EXPAND_ALLOCATE_STACK
#define T800_EXPAND_ALLOCATE_STACK \
    fatal ("The program being compiled requires dynamic stack space allocation which is not implemented in Expert run-time model.  So much sorry.");
