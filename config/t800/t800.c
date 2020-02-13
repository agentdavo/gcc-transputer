/* Subroutines used for code generation on transputer.
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

#include <stdio.h>
#include <limits.h>
#include "config.h"
#include "rtl.h"
#include "real.h"
#include "insn-config.h"
#include "insn-flags.h"
#include "regs.h"
#include "expr.h"
#include "recog.h"
#include "reload.h"
#include "output.h"
#include "hard-reg-set.h"
#include "reg-stack.h"


rtx force_ABCreg PROTO((enum machine_mode, rtx));
rtx my_emit_jump_insn PROTO((rtx, rtx));
int t800_expand_compare PROTO((enum rtx_code, rtx *));
static int is_offset PROTO((rtx op));
int t800_dataseg_symrefs_mentioned_p PROTO((rtx));
static void t800_output_addr_const PROTO((FILE *, rtx, int));
static void assert_sane_reg PROTO((rtx));


/* This is where cmpM pattern saves its arguments so that
   following sCC or bCC can use them */

struct t800_compare t800_compare;

/* This allows instruction patterns to find out whether we're doing
   initialization or the actual RTL generation.  */

int t800_init_once_completed;

/* This implements the REGISTER_MOVE_COST macro.  */

char register_move_cost[FIRST_PSEUDO_REGISTER][FIRST_PSEUDO_REGISTER] = {
  { 2,  2,  2, 10, 10, 10,  2,  2,  2},     /*R_AREG*/   
  { 2,  2,  2, 10, 10, 10,  2,  2,  2},     /*R_BREG*/   
  { 2,  2,  2, 10, 10, 10,  2,  2,  2},     /*R_CREG*/   
  {10, 10, 10,  2,  2,  2, 14, 14, 14},     /*R_FAREG*/  
  {10, 10, 10,  2,  2,  2, 14, 14, 14},     /*R_FBREG*/  
  {10, 10, 10,  2,  2,  2, 14, 14, 14},     /*R_FCREG*/  
  { 2,  2,  2, 14, 14, 14,  2,  2,  2},     /*R_WREG*/   
  { 2,  2,  2, 14, 14, 14,  2,  2,  2},     /*R_FAKE1*/
  { 2,  2,  2, 14, 14, 14,  2,  2,  2},     /*R_FAKE2*/
};

/* Stored by output__my_fancy_tablejump, checked by ASM_OUTPUT_ADDR_DIFF_ELT
   to make sure that table_rel_label has not been optimized away. */

int t800_expected_table_label;


/************************************************************
 Helper functions for t800.h macros
************************************************************/

/* PRINT_OPERAND implementation.  On transputers, what eventually gets
   printed is always a constant expression.  But operand (X) may be
   some other RTX, eg MEM; the machine description allows this because
   it is more efficient.  This routine first finds the constant to be
   printed inside of X. */

void
print_operand(stream, x, code)
    FILE *stream;
    rtx x;
    int code;
{
  if (GET_CODE (x) == MEM)
    x = XEXP (x, 0);

  if (GET_CODE (x) == PLUS)
    {
      /* (plus ([WABC]reg) (const_int|symbol_ref|const))  */
      assert_sane_reg (XEXP (x, 0));
      x = XEXP (x, 1);
    }

  if (GET_CODE (x) == REG)
    {
      /* equivalent to (plus ([WABC]reg) (const_int 0))  */
      assert_sane_reg (x);
      x = GEN_INT (0);
    }

  /* Now x should be suitable for t800_output_addr_const.  It will
     abort if not.  */

  t800_output_addr_const (stream, x, code);
}


/* Variation of generic output_addr_const: less general but handles
   `code' argument from PRINT_OPERAND */

void
t800_output_addr_const (file, x, code)
     FILE *file;
     rtx x;
     int code;
{
  HOST_WIDE_INT val;

 restart:
  switch (GET_CODE (x))
    {
    case SYMBOL_REF:
#ifdef T800_PRINT_OPERAND_SYMREF
      /* Some assemblers don't need the scaling we do for 'w' on SYMBOL_REF's */
      T800_PRINT_OPERAND_SYMREF (file, x, code);
#else
      output_addr_const (file, x);
      if (code == 'w')
	fprintf (file, "/%d", UNITS_PER_WORD);
#endif
      break;

    case LABEL_REF:
    case CODE_LABEL:
      output_addr_const (file, x);
      break;

    case CONST_INT:
      switch (code)
	{
	case 'w':	/* word offset */
	  if (INTVAL (x) % UNITS_PER_WORD)
	    abort ();
	  /*FALL THROUGH*/
	case 'q':	/* quotient */
	  val = INTVAL (x) / UNITS_PER_WORD;
	  break;

	case 'r':	/* remainder */
	  val = INTVAL (x) % UNITS_PER_WORD;
	  break;

	case 0:		/* output it as it is */
	  val = INTVAL (x);
	  break;

	default:
	  abort ();
	}
	  
      fprintf (file,
#if HOST_BITS_PER_WIDE_INT == HOST_BITS_PER_INT
	       "%d",
#else
	       "%ld",
#endif
	       val);
      break;

    case CONST:
      t800_output_addr_const (file, XEXP (x, 0), code);
      break;

    case PLUS:
      /* Some assemblers need integer constants to appear last (eg masm).  */
      if (GET_CODE (XEXP (x, 0)) == CONST_INT)
	{
	  t800_output_addr_const (file, XEXP (x, 1), code);
	  if (INTVAL (XEXP (x, 0)) >= 0)
	    fprintf (file, "+");
	  t800_output_addr_const (file, XEXP (x, 0), code);
	}
      else
	{
	  t800_output_addr_const (file, XEXP (x, 0), code);
	  if (INTVAL (XEXP (x, 1)) >= 0)
	    fprintf (file, "+");
	  t800_output_addr_const (file, XEXP (x, 1), code);
	}
      break;

    case MINUS:
      /* Avoid outputting things like x-x or x+5-x,
	 since some assemblers can't handle that.  */
      x = simplify_subtraction (x);
      if (GET_CODE (x) != MINUS)
	goto restart;

      t800_output_addr_const (file, XEXP (x, 0), code);
      fprintf (file, "-");
      if (GET_CODE (XEXP (x, 1)) == CONST_INT
	  && INTVAL (XEXP (x, 1)) < 0)
	{
	  fprintf (file, ASM_OPEN_PAREN);
	  t800_output_addr_const (file, XEXP (x, 1), code);
	  fprintf (file, ASM_CLOSE_PAREN);
	}
      else
	t800_output_addr_const (file, XEXP (x, 1), code);
      break;

    default:
      abort ();
    }
}


/* Check if we have register eliminations done.  Any fake registers
   must be eliminated by now; the only valid register in WREG family
   after reload is Wreg itself. */

static void
assert_sane_reg (x)
     rtx x;
{
  if (GET_CODE (x) != REG || REGNO (x) > R_WREG)
    abort ();
}


int
t800_address_cost (x)
    rtx x;
{
  int cost = 0;

again:
  switch (GET_CODE (x))
    {
    case PLUS:
      if (GET_CODE (XEXP (x, 1)) != CONST_INT)
        /* An illegal address; the return value does not matter.  */
        goto Default;

      cost = t800_const_cost (INTVAL (XEXP (x, 1)) / UNITS_PER_WORD);
      x = XEXP (x, 0);
      goto again;

    case REG:
      /* Local addresses are preferable, since Wreg is not subject to
         spill.  Prefer them whenever possible.  */
      return cost + 1
	- (TEST_HARD_REG_BIT (reg_class_contents[WORKSPACE_REGS], REGNO (x)));

    case LABEL_REF:
    case SYMBOL_REF:
      /* There is no way to find the true cost of it, because we don't
         know how far the label is. Make it relatively cheap in the
         hope that the labels are not too far, on the average.  */
      return cost + 3;

    Default:
    default:
      /* Probably invalid address */
      return 10;
    }
}

/* Tell how costly the constant is. On transputers, this can be
   measured as the amount of {pfix,nfix} insns needed.  */

int
t800_const_cost (i)
  int i;
{
  int cost = 0;

  if (i < 0)
    {
      if (i >= (signed int)0xfffffff0U)
        /* This takes an extra nfix only to obtain sign-extension */
        cost++;
      i = ~i;
    }
  while (i = (unsigned)i >> 4)
    cost++;

  return cost;
}

/* The implementation of SECONDARY_MEMORY_NEEDED macro.  Copying
   between floating and any other reg on transputers requires an
   intermediate memory location. */

#ifdef SECONDARY_MEMORY_NEEDED
int
secondary_memory_needed(class1, class2, mode)
  enum reg_class class1;
  enum reg_class class2;
  enum machine_mode mode;
{
  int float_reg_1 = 1;
  int float_reg_2 = 1;

  GO_IF_HARD_REG_SUBSET (reg_class_contents[class1],
                         reg_class_contents[FLOAT_REGS], m1);
  float_reg_1 = 0;
m1:

  GO_IF_HARD_REG_SUBSET (reg_class_contents[class2],
                         reg_class_contents[FLOAT_REGS], m2);
  float_reg_2 = 0;
m2:

  return float_reg_1 ^ float_reg_2;
}
#endif /* SECONDARY_MEMORY_NEEDED */


static int regno_to_find;

static int
find_popped_regno (insn, popped_reg)
     rtx insn, popped_reg;
{
  register popped_regno;

  if (GET_CODE (popped_reg) == SUBREG)
    popped_regno = REGNO (SUBREG_REG (popped_reg)) + SUBREG_WORD (popped_reg);
  else
    popped_regno = REGNO (popped_reg);

  return (popped_regno == regno_to_find);
}

int
insn_clobbers_regno_p (insn, regno)
  rtx insn;
  int regno;
{
  regno_to_find = regno;
  return note_popped_inputs (insn, find_popped_regno);
}

int
t800_fp_class (c)
  enum reg_class c;
{
  GO_IF_HARD_REG_SUBSET (reg_class_contents[c],
                         reg_class_contents[FLOAT_REGS], yes);
  return 0;
yes:
  return 1;
}

/* Attempt to convert memory address X into a valid one.  */

rtx
t800_legitimize_address (x, oldx, mode)
     rtx x;
     rtx oldx;
     enum machine_mode mode;
{

  /* (PLUS (MULT r1 4) r2)) , where r[12] are ABCregs, can be
     legitimized by loading into a reg with a single `wsub' insn. */

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 0)) == MULT)
    {
      rtx factor = XEXP (XEXP (x, 0), 1);
      if (wsub_scale_operand (factor, VOIDmode))
        {
          rtx newx = gen_reg_rtx (SImode);
          rtx base = XEXP (x, 1);
          rtx index = XEXP (XEXP (x, 0), 0);

          base = force_ABCreg (SImode, force_operand (base, 0));
          index = force_ABCreg (SImode, force_operand (index, 0));
          emit_insn (gen__wsub (newx, base, index, factor));
	  return newx;
        }
    }

  /* Now if this is a (plus (...) (const_int offset)) where `offset'
     is not a word-multiple, just copy it to a pseudo.  Otherwise
     memory_address will generate superfluous pseudo in attempt to
     make an indexed address, which is not useful here anyway.  */

  if (GET_CODE (x) == PLUS
      && GET_CODE (XEXP (x, 1)) == CONST_INT
      && (INTVAL (XEXP (x, 1)) % UNITS_PER_WORD) != 0)
    return copy_addr_to_reg (x);

  /* Know no special tricks for legitimizing this; let the compiler do
     it in common way */
  return x;
}


/************************************************************
 Predicates for use in (match_operand ...)
************************************************************/

/* Return 1 if an operand OP is a local memory reference */

int
local_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  /* Strip SUBREG; this allows for (subreg (failed_pseudo_reg))
     and (subreg (mem (local_operand_address)))  */
  if (GET_CODE (op) == SUBREG)
      op = SUBREG_REG (op);

  /* When reload is in progress, pseudo-regs are the ones failed to get
     hard regs and therefore destined to stack slots. Thus, pseudo-reg
     is a local operand when reload is in progress */
  if (reload_in_progress && true_regnum (op) >= FIRST_PSEUDO_REGISTER)
      return(1);

  /* Reject non-memory operations */
  if (GET_CODE (op) != MEM)
    return 0;

  return local_operand_address (XEXP (op,0), mode);
}


/* Return non-zero if an operand OP is a local memory address, i.e.
   (Wreg [+ offset]).  Offset is checked to be satisfactory for MODE.
   (in assumption that Wreg is always aligned well enough for any mode).  */

int
local_operand_address (addr, mode)
     rtx addr;
     enum machine_mode mode;
{
  if (GET_CODE (addr) == PLUS)
    {
      if (GET_CODE (XEXP (addr, 1)) != CONST_INT
	  || (INTVAL (XEXP (addr, 1)) % UNITS_PER_WORD) != 0)
        return 0;
      addr = XEXP (addr, 0);
    }

  return Wreg_operand (addr, Pmode);
}


/* Return 1 if an operand OP is a non-local memory reference,
   i.e. (mem (match_operand "ABCreg_operand")) */

int
nonlocal_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) != MEM)
    return 0;

  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  return (ABCreg_operand (XEXP (op, 0), SImode));
}


/* Return 1 if an operand OP is an offsetted non-local memory reference,
   i.e. (mem (plus (match_operand "ABCreg_operand") (const_int))) */

int
nonlocal_plus_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  rtx addr;

  if (GET_CODE (op) != MEM)
    return 0;
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  addr = XEXP (op, 0);
  if (GET_CODE (addr) == PLUS)
    {
      rtx x = XEXP (addr, 1);

      if (GET_CODE (x) == CONST_INT
	  && (INTVAL (x) % UNITS_PER_WORD) == 0)
	goto win;

      if (TARGET_DATASEG_BY_POINTER)
	{
	  if (GET_CODE (x) == SYMBOL_REF
	      && SYMBOL_REF_FLAG (x) == 1)
	    goto win;

	  if (GET_CODE (x) == CONST)
	    {
	      rtx x1 = XEXP (x, 0);

	      /* If there is a non-word constant offset, this address
		 does not match.  */

	      if (GET_CODE (x1) == PLUS
		  && GET_CODE (XEXP (x1, 1)) == CONST_INT
		  && (INTVAL (XEXP (x1, 1)) % UNITS_PER_WORD) != 0)
		return 0;

	      /* The rest may be arbitrarily complex expression
		 of symrefs.  Assuming data and code segment labels
		 never mix in CONST, a simple check for presence
		 of data segment symref should be enough.  */

	      if (t800_dataseg_symrefs_mentioned_p (x1))
		goto win;
	    }
	}
      return 0;
    win:
      addr = XEXP (addr, 0);
    }

  return (ABCreg_operand (addr, SImode));
}


/* Return 1 if OP is a valid offset from a word pointer, i.e.
   a constant which is a multiple of word size.  For use as
   operand for ldnlp.  */

int
word_offset_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT
      && (INTVAL (op) % UNITS_PER_WORD) == 0)
    return 1;

  /* In dataseg-by-pointer model, data segment SYMBOL_REF's are
     converted into offsets from data segment start by print_operand.
     As they are word aligned, they can be given to ldnlp.  */

  if (TARGET_DATASEG_BY_POINTER)
    {
      if (GET_CODE (op) == SYMBOL_REF
	  && SYMBOL_REF_FLAG (op) == 1)
	return 1;

      if (GET_CODE (op) == CONST
	  && (GET_CODE (XEXP (op, 0)) == PLUS
	      || GET_CODE (XEXP (op, 0)) == MINUS))

	/* Walk RHS first as it seems more likely to prove
	   unacceptable (non-word-multiple CONST_INT) */

	return word_offset_operand (XEXP (XEXP (op, 0), 1), VOIDmode)
	  && word_offset_operand (XEXP (XEXP (op, 0), 0), VOIDmode);
    }
  return 0;
}


/* Return 1 if OP is a valid operand for adc instruction, which is a
   CONST_INT or (in dataseg-by-pointer model) data segment symbolic
   expression, because it will be relocated in link time.

   ??? We could try to use immediate_operand for that, but it requires
   to make LEGITIMATE_CONSTANT more restrictive (not accept
   LABEL_REFs, for example), and I'm afraid that will affect handling
   of JUMP/CALL insns, for which LABEL_REF is valid.  It's simpler to
   have a separate predicate... */

int
adc_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{	
  if (GET_CODE (op) == CONST_INT)
    return 1;

  if (TARGET_DATASEG_BY_POINTER)
    {
      if (GET_CODE (op) == SYMBOL_REF
	  && SYMBOL_REF_FLAG (op) == 1)
	return 1;

      if (GET_CODE (op) == CONST
	  && t800_dataseg_symrefs_mentioned_p (XEXP (op, 0)))
	return 1;
    }
  return 0;
}

/* Return 1 if OP is a symbolic offset expression, such as
   (CONST (MINUS L1 L2)) */

int
const_offset_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  if (GET_CODE (op) != CONST)
    return 0;

  return is_offset (XEXP (op,0));
}

/* Helper routine for const_offset_operand.  Given a CONST body,
   return 1 if it is of int type (e.g. L1-L2), return 0 if it is of
   pointer type (e.g. L1+5) */

static int
is_offset (op)
     register rtx op;
{
  switch (GET_CODE (op))
    {
    case PLUS:
      return is_offset (XEXP (op, 0)) && is_offset (XEXP (op, 0));

    case MINUS:
      return is_offset (XEXP (op, 0)) == is_offset (XEXP (op, 0));

    case CONST_INT:
      return 1;

    case LABEL_REF:
    case SYMBOL_REF:
      return 0;

    default:
      abort ();
    }
}


/* Return 1 if OP is a valid operand for ldpi instruction, ie. is a
   program-counter-relative address.  */

int
ldpi_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (mode != VOIDmode && GET_MODE (op) != mode)
    return 0;

  if (GET_CODE (op) == LABEL_REF)
    return 1;

  /* Int type CONST's (e.g L1-L2) are not acceptable for ldpi -- we
     don't want to let ldpi add PC to them! */

  if (GET_CODE (op) == CONST && const_offset_operand (op, mode))
    return 0;
  
  /* If data segment is pc-relative, all SYMBOL_REF's are OK; but if
     data segment is addressed by pointer, we don't want to use ldpi
     for loading data segment addresses, so reject data segment
     SYMBOL_REF's.  (ENCODE_SECTION_INFO marks data segment SYMREF's
     with `1' in SYMBOL_REF_FLAG) */

  if (TARGET_DATASEG_PC_RELATIVE)
    return (GET_CODE (op) == SYMBOL_REF || GET_CODE (op) == CONST);
  else
    {
     /* Reject SYMREF's if they belong to data segment
       (ENCODE_SECTION_INFO marks such SYMREF's with `1' in
       SYMBOL_REF_FLAG) */

      if (GET_CODE (op) == SYMBOL_REF)
	return SYMBOL_REF_FLAG (op) != 1;

      /* Likewise for SYMREF's hidden inside CONST */
    
      if (GET_CODE (op) == CONST)
	return ! t800_dataseg_symrefs_mentioned_p (op);
    }

  return 0;
}


int
t800_dataseg_symrefs_mentioned_p (pat)
     rtx pat;
{
  register char *fmt;
  register int i, j;

  if (GET_CODE (pat) == SYMBOL_REF)
    return SYMBOL_REF_FLAG (pat) == 1;
  else
    {
      fmt = GET_RTX_FORMAT (GET_CODE (pat));
      for (i = GET_RTX_LENGTH (GET_CODE (pat)); --i >= 0; )
	if (fmt[i] == 'E')
	  {
	    for (j = XVECLEN (pat, i); --j >= 0; )
	      if (t800_dataseg_symrefs_mentioned_p (XVECEXP (pat, i, j)))
		return 1;
	  }
	else if (fmt[i] == 'e')
	  if (t800_dataseg_symrefs_mentioned_p (XEXP (pat, i)))
	    return 1;
    }
  return 0;
}


/* Returns 1 if OP is either a register or a local operand.
   This is used in place of register_operand to avoid preloading from
   local variables to pseudo registers that may probably fail to get a
   hard register. */

int
ABCreg_or_local_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
    return ABCreg_operand(op, mode) || local_operand(op, mode);
}

/* Ditto for nonmemory_operand */

int nonmem_or_local_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
    return nonmemory_operand(op, mode) || local_operand(op, mode);
}

int
FABCreg_or_nonlocal_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
    return FABCreg_operand(op, mode) || nonlocal_operand(op, mode);
}

/* A class of operands that remain in the class no matter what pseudos
   fail to get a hard register.  */

int
nice_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_CODE (op) == CONST_INT)
    return 1;
  return ABCreg_operand(op, mode) || local_operand(op, mode);
}


int
ABCreg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != REG)
    return 0;

  /* Hard ABCregs undoubtedly *are* ABCregs */

  if (REGNO (op) == R_AREG || REGNO (op) == R_BREG || REGNO (op) == R_CREG)
    return 1;

  /* Allow pseudos because they will turn into, or will be reloaded
     into the proper hard regs.  Reject virtual registers since they
     are known to be changed into Wreg[+offset], so there is no chance
     for them to become ABCreg's.

     We don't reject pseudos even during reload; this might be
     questionable, but consistent with the behaviour of
     register_operand.  */

  if (REGNO (op) > LAST_VIRTUAL_REGISTER)
    return 1;

  return 0;
}

int
FABCreg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != REG)
    return 0;

  /* Hard FABCregs undoubtedly *are* FABCregs */

  if (REGNO (op) == R_FAREG || REGNO (op) == R_FBREG || REGNO (op) == R_FCREG)
    return 1;

  /* Allow pseudos because they will turn into, or will be reloaded
     into the proper hard regs.  Reject virtual registers since they
     are known to be changed into Wreg[+offset], so there is no chance
     for them to become FABCreg's.  */

  if (REGNO (op) > LAST_VIRTUAL_REGISTER)
    return 1;

  return 0;
}

int
Wreg_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) != mode && mode != VOIDmode)
    return 0;

  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != REG)
    return 0;

  /* Virtual registers are OK here, as they will be replaced by WREG
     or its equivalent (fake arg/stack pointers). But other pseudos
     must be rejected, as WREG is a fixed register and cannot be
     reloaded into.  */

  switch (REGNO (op))
    {
      case R_WREG:
      case R_FAKE1:
      case R_FAKE2:
      case VIRTUAL_INCOMING_ARGS_REGNUM:
      case VIRTUAL_STACK_VARS_REGNUM:
      case VIRTUAL_STACK_DYNAMIC_REGNUM:
      case VIRTUAL_OUTGOING_ARGS_REGNUM:
        return 1;
    }

  return 0;
}


/* Returns 1 if OP is a legitimate offset multiplier (size of array
   element) for `wsub' or `wsubdb' command.  */

int
wsub_scale_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  /* Don't accept CONST_INT or anything similar
     if the caller wants something floating.  */
  if (GET_MODE (op) == VOIDmode && mode != VOIDmode
      && GET_MODE_CLASS (mode) != MODE_INT)
    return 0;

  if (GET_CODE (op) == CONST_INT
      && (GET_MODE (op) == mode || mode == VOIDmode
          || GET_MODE (op) == VOIDmode))
    switch (INTVAL (op))
      {
      case UNITS_PER_WORD/2:
	if (! TARGET_HAVE_SIXTEEN)
	  break;
	/* fall through */
      case UNITS_PER_WORD:
      case UNITS_PER_WORD*2:
        return 1;
      }
  return 0;
}


/* Return 1 if the OP is zero in mode MODE.  This is really used for
   FP modes only; integer modes probably won't work with the current
   definition.  */

int
zero_operand (op, mode)
     register rtx op;
     enum machine_mode mode;
{
  return op == CONST0_RTX (mode);
}


static REAL_VALUE_TYPE dconst2e32;
static REAL_VALUE_TYPE fconst2e32;

void
init_fp_specval ()
{
  fconst2e32 = REAL_VALUE_ATOF ("2E32", SFmode);
  dconst2e32 = REAL_VALUE_ATOF ("2E32", DFmode);
}

int
fp_specval_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  if (GET_MODE (op) == mode
      && GET_CODE (op) == CONST_DOUBLE)
    {
      REAL_VALUE_TYPE rv;

      REAL_VALUE_FROM_CONST_DOUBLE (rv, op);
      if (REAL_VALUES_EQUAL (rv, dconst2)
          || REAL_VALUES_EQUAL (rv, dconst2e32)
          || REAL_VALUES_EQUAL (rv, fconst2e32))
        return 1;
    }
  return 0;
}

int
fp_specval_ok_for_letter (x, c)
     rtx x;
     int c;
{
  REAL_VALUE_TYPE rv;

  REAL_VALUE_FROM_CONST_DOUBLE (rv, x);
  switch (c)
    {
    case 'G': return REAL_VALUES_EQUAL (rv, dconst2);
    case 'H': return REAL_VALUES_EQUAL (rv, dconst2e32)
                     || REAL_VALUES_EQUAL (rv, fconst2e32);
    }

  return 0;
}

/* The predicate that accepts anything of the proper mode for the
   operand.  */

int
whatever_operand (op, mode)
     rtx op;
     enum machine_mode mode;
{
  enum machine_mode opmode = GET_MODE (op);
  
  return opmode == mode || mode == VOIDmode
         /* accept CONST_INT (which is VOIDmode) for integer modes */
         || GET_MODE_CLASS (mode) == MODE_INT && opmode == VOIDmode;
}


/************************************************************
 Helper functions called from t800.md
************************************************************/


enum { INVERSE = 0x1, NOT_01 = 0x2 };


/* Emit insn for an sCOND pattern.  */

void
t800_expand_scond (cond, operand0)
     enum rtx_code cond;
     rtx operand0;
{
  rtx temp = operand0;
  int r = t800_expand_compare (cond, &temp);

  /* This may generate insn sequence that mentions operand0 more than
     once.  This is OK in this particular case, since `eqc' does not
     pop its input.  */

  while (r & NOT_01 || r & INVERSE)
    {
      emit_insn (gen__eqc (operand0, temp, GEN_INT (0)));
      r &= ~NOT_01;
      r ^= INVERSE;
      temp = operand0;
    }

  if (temp != operand0)
    emit_move_insn (operand0, temp);
}


/* Emit insn for a bCOND pattern.  */

void
t800_expand_bcond (cond, dest_label)
     enum rtx_code cond;
     rtx dest_label;
{
  rtx temp = NULL_RTX;
  int r = t800_expand_compare (cond, &temp);

  if (r & INVERSE)
    my_emit_jump_insn (gen__cj (dest_label, temp), dest_label);
  else
    my_emit_jump_insn (gen__inv_cj (dest_label, temp), dest_label);
}


/* Common part for rtl generation for sCOND/bCOND groups of patterns.
   The operands to be compared are stored in the structure
   t800_compare by cmpM pattern.

   This function generates insn sequence that carries out the
   essential part of the comparison; that is, the trailing `eqc 0's
   that may be required to invert the result or produce a valid 0/1
   comparison result are never included in the sequence.  Instead, it
   is responsibility of the caller to invert/normalize the result
   according to taste.  t800_expand_scond and t800_expand_bcond,
   which call this function, do this in distinct ways.

   Return value is a two-bit mask.
     00:  the code emitted produces the 0/1 result;
     01:  the code emitted produces the inverse 0/1 result;
     10:  the code emitted produces the 0/not0 result;
     11:  the code emitted produces the inverse 0/not0 result.
     
   OUT_REG is a pointer to the rtx where the result of comparison
   is suggested to come.  If *OUT_REG == 0, we allocate a new pseudo
   for the output.  On return, we put there the actual rtx for the
   comparison result.  */

int
t800_expand_compare (cond, out_reg)
     enum rtx_code cond;
     rtx *out_reg;
{
  rtx op[2] = {t800_compare.op[0], t800_compare.op[1]};
  rtx (*gen_gt) PROTO((rtx, rtx, rtx)) = t800_compare.fp? gen__fpgt: gen__gt;
  
  /* Convert QImode comparison to SImode.  Unsigned comparisons are
     converted to the cheap signed SImode ones by zero-extending the
     operands.  For other comparisons, we use zero-extension whenever
     possible as well, because BYTE_LOADS_ZERO_EXTEND.  */

  if (GET_MODE (op[0]) == QImode || GET_MODE (op[0]) == HImode)
    switch (cond)
      {
      case GTU:  cond = GT; goto zero_extend;
      case LTU:  cond = LT; goto zero_extend;
      case GEU:  cond = GE; goto zero_extend;
      case LEU:  cond = LE; goto zero_extend;
      case EQ:
      case NE:
      zero_extend:
        if (GET_CODE (op[1]) == CONST_INT)
	    op[1] = GEN_INT (INTVAL (op[1])
			     & (GET_MODE (op[0]) == QImode? 0xff: 0xffff));
	else
	  op[1] = convert_to_mode (SImode, op[1], 1/*unsignedp*/);
        op[0] = convert_to_mode (SImode, op[0], 1/*unsignedp*/);
        op[0] = force_reg (SImode, op[0]);
        break;

      default:
        /* Comparing with a small CONST_INT is a common case where we can
           avoid sign-extension.  */
        if (GET_CODE (op[1]) == CONST_INT
            && INTVAL (op[1]) >= 0
	    && INTVAL (op[1]) <= (GET_MODE (op[0]) == QImode? 127: 32767))
          goto zero_extend;
        op[1] = convert_to_mode (SImode, op[1], 0/*unsignedp*/);
        op[0] = convert_to_mode (SImode, op[0], 0/*unsignedp*/);
        op[0] = force_reg (SImode, op[0]);
        break;
      }

  if (GET_CODE (op[1]) == CONST_INT)
    switch (cond)
      {
      case EQ:
      case NE:
        /* If comparing with 0 for branching, no comarison insn is
           necessary.  Otherwise, emit `eqc'.  */
        if (INTVAL (op[1]) == 0)
          {
            *out_reg = op[0];
            return (cond != NE) | NOT_01;
          }
        else
          {
            if (! *out_reg)
              *out_reg = gen_reg_rtx (SImode);
            emit_insn (gen__eqc (*out_reg, op[0], op[1]));
            return (cond != EQ);
          }

#if 0  /* It's not clear whether it makes better or worse.  */
      case GE:
      case LT:

        /* If possible, convert to the easy GT/LE case by decrementing
           the constant we compare with:
             x >= c  =>  x > c-1
             x < c   =>  x <= c-1
           This is not true if c == INT_MIN.
           This isn't worth doing with c == 0, because in this case we
           would convert the cheap positive constant to the more costly
           negative one.
           Note that we should use INT_MIN for the target; hope it is the
           same as the one for host. */

        if (INTVAL (op[1]) != 0)
            && INTVAL (op[1]) != INT_MIN)
          {
            op[1] = GEN_INT (INTVAL (op[1]) - 1);
            cond = (cond == GE? GT: LE);
          }
        break;
#endif
    }

  if (CONSTANT_P (op[1]) || Wreg_operand (op[1], VOIDmode))
    op[1] = copy_to_mode_reg (GET_MODE (op[0]), op[1]);

  if (! *out_reg)
    *out_reg = gen_reg_rtx (SImode);
  
  switch (cond)
    {
      case EQ:  /* diff; eqc 0  || fpeq */ 
      case NE:  /* diff         || fpeq; eqc 0 */
        if (t800_compare.fp)
          {
            emit_insn (gen__fpeq (*out_reg, op[0], op[1]));
            return (cond != EQ);
          }
        else
          {
            emit_insn (gen_subsi3 (*out_reg, op[0], op[1]));
            return (cond != NE) | NOT_01;
          }

      case GE:  /* (reversed) gt; eqc 0 */
      case LT:  /* (reversed) gt */
	if (TARGET_HAVE_FPGE && t800_compare.fp)
	  {
	    emit_insn (gen__fpge (*out_reg, op[0], op[1]));
	    return (cond != GE);
	  }
        emit_insn ((*gen_gt) (*out_reg, op[1], op[0]));
        return (cond != LT);

      case GT:  /* gt */
      case LE:  /* gt; eqc 0 */
	if (TARGET_HAVE_FPGE && t800_compare.fp)
	  {
	    emit_insn (gen__fpge (*out_reg, op[1], op[0]));
	    return (cond != LE);
	  }
        emit_insn ((*gen_gt) (*out_reg, op[0], op[1]));
        return (cond != GT);

      case GEU: /* ldiff; eqc 0 */
      case LTU: /* ldiff */
	if (TARGET_HAVE_GTU && ! t800_compare.fp)
	  {
	    emit_insn (gen__gtu (*out_reg, op[1], op[0]));
	    return (cond != LTU);
	  }
        emit_insn (gen__ldiff (*out_reg, gen_reg_rtx (SImode),
                               op[0], op[1], force_reg (SImode, GEN_INT (0))));
        return (cond != LTU);

      case GTU: /* (reversed) ldiff */
      case LEU: /* (reversed) ldiff; eqc 0 */
	if (TARGET_HAVE_GTU && ! t800_compare.fp)
	  {
	    emit_insn (gen__gtu (*out_reg, op[0], op[1]));
	    return (cond != GTU);
	  }
        emit_insn (gen__ldiff (*out_reg, gen_reg_rtx (SImode),
                               op[1], op[0], force_reg (SImode, GEN_INT (0))));
        return (cond != GTU);

      default:
        abort ();
    }
}


/* Same as emit_jump insn except it establishes the JUMP_LABEL field.
   This is to get around the bug of calling functions like redirect_jump
   which use JUMP_LABEL prior to jump optimization, ie before the
   JUMP_LABEL got a value.
   Eliminate it when and whether the bug is fixed. */

rtx
my_emit_jump_insn (pattern, label)
     rtx pattern;
     rtx label;
{
  rtx insn = emit_jump_insn (pattern);
  JUMP_LABEL (insn) = label;
  return insn;
}


/* Return 1 iff x is a register, possibly enclosed in a
   mode-conversion SUBREG.  */

int
reg_p (x)
    rtx x;
{
  return GET_CODE (x) == REG
         || GET_CODE (x) == SUBREG && GET_CODE (SUBREG_REG (x)) == REG;
}


/* Return 1 iff X is, or is likely to be allocated to, an fp register.  */

int
t800_fp_reg_p (x)
    rtx x;
{
  return (TARGET_HAVE_FPU
	  && GET_CODE (x) == REG
          && (GET_MODE_CLASS (GET_MODE (x)) == MODE_FLOAT
	      || GET_MODE_CLASS (GET_MODE (x)) == MODE_COMPLEX_FLOAT)
	  && (REGNO (x) >= FIRST_PSEUDO_REGISTER
	      || FP_REGNO_P (REGNO (x))));
}

/* Return 1 iff X is an ABCreg.  Much like ABCreg_operand, but does
   not accept pseudos during reload.  */

int
t800_ABCreg_p (op)
     register rtx op;
{
  if (GET_CODE (op) == SUBREG)
    op = SUBREG_REG (op);
  if (GET_CODE (op) != REG)
    return 0;

  if (REGNO (op) == R_AREG || REGNO (op) == R_BREG || REGNO (op) == R_CREG)
    return 1;

  if (REGNO (op) > LAST_VIRTUAL_REGISTER && ! reload_in_progress)
    return 1;

  return 0;
}


/* Copy x into a new pseudo if it is not an ABCreg */

rtx
force_ABCreg (mode, x)
     enum machine_mode mode;
     rtx x;
{
  if (ABCreg_operand (x, mode))
    return x;

  if (Wreg_operand (x, VOIDmode))
    return copy_to_mode_reg (mode, x);

  return force_reg (mode, x);
}


/* The essential part of gen_call() and gen_call_value().  */

void
t800_expand_call(ret, fun, stack_size_rtx, next_arg_reg, struct_value_size_rtx)
    rtx ret;
    rtx fun;
    rtx stack_size_rtx;
    rtx next_arg_reg;
    rtx struct_value_size_rtx;
{
  rtx addr = XEXP (fun, 0);
  int aggregate_valued
    = (struct_value_size_rtx && INTVAL (struct_value_size_rtx));
  rtx insn;
  rtx temp;
  rtx saveslot;

  if (ret == NULL_RTX)
    ret = gen_rtx (REG, SImode, R_AREG);

  /* If the function address is not constant, or if FUNCTION_ARG decided
     it's better to pass everything on stack, normal `call' insn
     won't do. Use gcall instead. */

  if (! CONSTANT_P (addr)
      || next_arg_reg == const0_rtx)  /* "passed-on-stack" sign
					 from FUNCTION_ARG */
    {
      int (*function_address_predicate)()
        = insn_operand_predicate[CODE_FOR__gcall][1];

      if (function_address_predicate
	  && (! function_address_predicate (addr, Pmode)))
        addr = copy_to_reg (addr);

#ifdef PSEUDO_STACK_POINTER
      if (current_function_calls_alloca)
	{
	  /* Just before the call, retarget Wreg to where our pseudo
	     stack pointer points.  Save the normal value of Wreg in a
	     stack slot just above the
	     current_function_outgoing_args_size, to be able to revert
	     Wreg to normal after the call.

	     Careful: Areg and Breg at the moment may carry stack value
	     return address and the function address, do not clobber them.  */

	  temp = gen_rtx (REG, Pmode, R_CREG);
	  saveslot = t800_gen_local (Pmode, current_function_outgoing_args_size);

	  emit_insn (gen_move_insn (temp, stack_pointer_rtx));
	  emit_insn (gen__gajw (hard_stack_pointer_rtx, temp));
	  emit_insn (gen_move_insn (saveslot, temp));
	}
#endif /* PSEUDO_STACK_POINTER */

#ifdef STRUCT_VALUE_REGNUM
      /* If this call takes return value address in a reg, we must
	 arrange for using a special flavor of gcall which has
	 function address constrained to another register.  See
	 T800_OUTPUT__GCALL and gcall patterns. */

      if (aggregate_valued)
        emit_call_insn (gen__gcall_aggregate (ret, addr, stack_size_rtx));
      else
        emit_call_insn (gen__gcall (ret, addr, stack_size_rtx));
#else
      insn = emit_call_insn (gen__gcall (ret, addr, stack_size_rtx));
      if (aggregate_valued)
        {
          /* sign to T800_OUTPUT_GCALL in expert model */
          PUT_MODE (PATTERN (insn), BLKmode);
        }
#endif /* STRUCT_VALUE_REGNUM */

#ifdef PSEUDO_STACK_POINTER
      if (current_function_calls_alloca)
	{
	  /* Restore Wreg to its normal position */
	  emit_insn (gen_move_insn (temp, saveslot));
	  emit_insn (gen__gajw (hard_stack_pointer_rtx, temp));
	  /* The old Wreg value is left on the regstack.  Never mind,
	     stack converter will take care of it */
	}
    }
#endif /* PSEUDO_STACK_POINTER */

  else
    {
      /* The normal case */
      insn = emit_call_insn (gen__call (ret, addr, stack_size_rtx));
      if (aggregate_valued)
        {
          /* sign to T800_OUTPUT_CALL in expert model */
          PUT_MODE (PATTERN (insn), BLKmode);
        }
    }
}

/* Temporary slot management.  define_expand's use a temporary stack
   slot when dealing with insns that require certain operand to be in
   memory rather than in a register.  The slot is allocated once per
   function, on the first call to t800_temp_slot.  All subsequent
   calls to t800_temp_slot return rtx of the appropriate mode for the
   same slot.  */

#define WIDEST_MODE DFmode

rtx t800_temp_slots[NUM_MACHINE_MODES];

void
t800_temp_slot_init ()
{
  int mode;
  for (mode=NUM_MACHINE_MODES; --mode >= 0; )
    t800_temp_slots[mode] = NULL_RTX;
}

rtx
t800_temp_slot (mode)
    enum machine_mode mode;
{
  extern int virtuals_instantiated;  /* from function.c */

  if (t800_temp_slots[mode] == NULL_RTX)
    {
      if (t800_temp_slots[WIDEST_MODE] == NULL_RTX)
        t800_temp_slots[WIDEST_MODE] =
          assign_stack_local (WIDEST_MODE, GET_MODE_SIZE (WIDEST_MODE), 0);
      if (mode != WIDEST_MODE)
	t800_temp_slots[mode] =
	  gen_rtx (MEM, mode, XEXP (t800_temp_slots[WIDEST_MODE], 0));
    }

  /* If we allocated the slot in the RTL generation phase,
     assign_stack_local have used virtual frame pointer in the slot
     address.  If we are called after instantiation (eg during reload)
     returning the slot with such an address is not legal, since the
     slot would be used as is and never instantiated.  So make sure
     any virtuals in the slot's address are instantiated.  */
  else if (virtuals_instantiated)
    {
      instantiate_decl (t800_temp_slots[mode], GET_MODE_SIZE (mode), 1);
    }

  return t800_temp_slots[mode];
}


/* For an arbitrary MEM rtx, return a MEM rtx which is a valid
   nonlocal_operand.  This is accomplished by copying the memory
   address into a pseudo, if necessary.  */

rtx
t800_force_nonlocal (x)
    rtx x;
{
  rtx addr;

  if (GET_CODE (x) != MEM)
    abort ();

  addr = XEXP (x,0);

  if (ABCreg_operand (addr, Pmode))
    return x;

  return gen_rtx (MEM, GET_MODE (x), copy_addr_to_reg (addr));
}

/* For an arbitrary MEM rtx, return a MEM rtx which is a valid
   nonlocal_operand.  This is accomplished by copying the memory
   address into the register REG, if necessary.  */

rtx
t800_force_nonlocal_using (x, reg)
    rtx x;
    rtx reg;
{
  rtx addr;

  if (GET_CODE (x) != MEM)
    abort ();

  addr = XEXP (x,0);

  if (ABCreg_operand (addr, Pmode))
    return x;

  emit_move_insn (reg, addr);
  return gen_rtx (MEM, GET_MODE (x), reg);
}

/* Generate rtx for a workspace slot address.  */

rtx
t800_gen_local_address (offset)
    int offset;
{
  register rtx addr = gen_rtx (REG, SImode, R_WREG);
  if (offset)
    return gen_rtx (PLUS, SImode, addr, GEN_INT (offset));
  else
    return addr;
}

/* Generate rtx for a workspace slot.  */

rtx
t800_gen_local (mode, offset)
    enum machine_mode mode;
    int offset;
{
  return gen_rtx (MEM, mode, t800_gen_local_address (offset));
}

/* Given an object for which reload_memory_operand is true, return the
   address of the operand, taking into account anything that reload
   may do.  Stolen from config/a29k/a29k.c  */

rtx
t800_get_reloaded_address (op)
     rtx op;
{
  if (GET_CODE (op) == SUBREG)
    {
      if (SUBREG_WORD (op) != 0)
	abort ();

      op = SUBREG_REG (op);
    }

  if (GET_CODE (op) == REG)
    op = reg_equiv_mem[REGNO (op)];

  return find_replacement (&XEXP (op, 0));
}



/************************************************************
 Machine-dependent functions called from reg-stack.c
************************************************************/

#define ABC_STACKNO   0
#define FABC_STACKNO  1

/* Helper procedure for t800_emit_{drops,swaps}()
   Emit insns according to the control string STR.  Reflect the
   effect of insn emitted on the REGSTACK.
   STR is the control string. Character assignments are:
    'r' -   swap two regs near ABC-stack top    "rev"
    'd' -   drop ABC-stack top                  "stl -5"
    's' -   store reg temporarily               "stl -1"
    'l' -   load temporarily stored reg         "ldl -1"
    'R' -   swap two regs near FABC-stack top   "fprev"
    'D' -   drop FABC-stack top                 "ldlp -3; fpstnlxx"
    'S' -   store FABC-stack top temporarily    "ldlp -3; fpldnlxx"
    'L' -   load temporarily stored reg         "ldlp -5; fpstnlxx"

   Temporary locations layout:
     Wreg[-1] Integer store area
     Wreg[-2] Floating store area, high
     Wreg[-3] Floating store area, low
     Wreg[-4] Drop area, high
     Wreg[-5] Drop area, low
   */

static void
emit_t800_insns (str, st)
    char *str;
    stack st;
{
  static int last_stored_virtual;
  static int last_stored_fp_virtual;
  static enum machine_mode last_stored_fp_mode;
  enum machine_mode mode;
  rtx pattern;
  int offset;
  int r0,r1;

  for ( ; *str; emit_insn (pattern))
    switch (*str++)
      {
      case 'd':
	if (TARGET_HAVE_POP)
	  {
	    /* Use the T805 insn to drop the reg at stack top */
	    pattern = gen__pop (hard_reg[STACK_REG_FIRST(ABC_STACKNO)][SImode]);
	    CLEAR_HARD_REG_BIT (st->reg_set, st->reg[st->top[ABC_STACKNO]++]);
	    break;
	  }
	else
	  {
	    /* Drop is just like a store except we use a different
	       stack offset to avoid the conflict with load/store.  */
	    offset = -5 * UNITS_PER_WORD;
	    goto store;
	  }

      case 's':
        /* Store a reg temporarily; it will be loaded back to stack
           later in the sequence. Store into a spare memory location--
           the one at a small negative offset from Wreg.  */
        offset = -1 * UNITS_PER_WORD;
        last_stored_virtual = st->reg[st->top[ABC_STACKNO]];

      store:
        pattern = gen__stl (t800_gen_local (SImode, offset),
                            hard_reg[STACK_REG_FIRST(ABC_STACKNO)][SImode]);
        CLEAR_HARD_REG_BIT (st->reg_set, st->reg[st->top[ABC_STACKNO]++]);
        break;

      case 'l':
        /* Load a reg saved by 's' */
        offset = -1 * UNITS_PER_WORD;
        pattern = gen__ldl (hard_reg[STACK_REG_FIRST(ABC_STACKNO)][SImode],
                            t800_gen_local (SImode, offset));
        SET_HARD_REG_BIT (st->reg_set, last_stored_virtual);
        st->reg[--(st->top[ABC_STACKNO])] = last_stored_virtual;
        break;

      case 'r':
        pattern = gen__rev (hard_reg[STACK_REG_FIRST(ABC_STACKNO)][SImode],
                            hard_reg[STACK_REG_FIRST(ABC_STACKNO)+1][SImode]);
        r0 = st->reg[st->top[ABC_STACKNO]];
        r1 = st->reg[st->top[ABC_STACKNO] + 1];
        st->reg[st->top[ABC_STACKNO]] = r1;
        st->reg[st->top[ABC_STACKNO] + 1] = r0;
        break;

      case 'D':
        offset = -5 * UNITS_PER_WORD;
        goto fp_store;

      case 'S':
        offset = -3 * UNITS_PER_WORD;
        last_stored_fp_virtual = st->reg[st->top[FABC_STACKNO]];

      fp_store:
        mode = st->mode[REG_BY_HARD_REGNO (st, R_FAREG)];
        pattern = gen_rtx (SET, mode,
                           t800_force_nonlocal_using (t800_gen_local (mode, offset), 
						      hard_reg[STACK_REG_FIRST(ABC_STACKNO)][Pmode]),
                           hard_reg[STACK_REG_FIRST(FABC_STACKNO)][mode]);
        CLEAR_HARD_REG_BIT (st->reg_set, st->reg[st->top[FABC_STACKNO]++]);
        break;

      case 'L':
        /* Load a reg saved by 's' */
        offset = -3 * UNITS_PER_WORD;
        mode = st->mode[REG_BY_HARD_REGNO (st, R_FAREG)];
        pattern = gen_rtx (SET, mode,
                           hard_reg[STACK_REG_FIRST(FABC_STACKNO)][mode],
                           t800_force_nonlocal_using (t800_gen_local (mode, offset), 
						      hard_reg[STACK_REG_FIRST(ABC_STACKNO)][Pmode]));
        SET_HARD_REG_BIT (st->reg_set, last_stored_fp_virtual);
        st->reg[--(st->top[FABC_STACKNO])] = last_stored_fp_virtual;
        break;

      case 'R':
        mode = st->mode[REG_BY_HARD_REGNO (st, R_FAREG)];
        pattern = gen__fprev (hard_reg[STACK_REG_FIRST(FABC_STACKNO)][mode],
                              hard_reg[STACK_REG_FIRST(FABC_STACKNO)+1][mode]);
        r0 = st->reg[st->top[FABC_STACKNO]];
        r1 = st->reg[st->top[FABC_STACKNO] + 1];
        st->reg[st->top[FABC_STACKNO]] = r1;
        st->reg[st->top[FABC_STACKNO] + 1] = r0;
        break;

      default:
        abort ();
      }
}


/* Procedure for discarding regs from reg-stack.

   There are two ways to delete a reg from reg-stack.
   1) if the reg is at the top of stack, it can be popped (stl <dummy>);
   2) if the reg is at the bottom, it can be forgotten without
      an insn, since the transputer's reg-stacks are not `strict'.
   There is no insn to drop a reg that is in the middle of the stack.
   In this case we have to swap some regs to bring the reg to be
   dropped into suitable position (top, in practice).

   REGSTACK is the state of the stack at the point where we are
     to insert dropping insns. REGSTACK is modified to reflect the
     effect of inserted insns.
   DROP_SET is the hard-reg-set indicating which *virtual* regs
     to drop.  */

void
t800_emit_drops (regstack, drop_set)
    stack regstack;
    HARD_REG_SET drop_set;
{
  int i,j,k;

  /* Calling this with empty drop_set seems fairly frequent.
     Don't bother going through the loop in this case...  */
  GO_IF_HARD_REG_EQUAL (drop_set, reg_class_contents[NO_REGS], end);

  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    {
      int dropmask = 0x111;

      if (STACK_REG_LAST(k) + 1 - STACK_REG_FIRST(k) != 3)
        abort ();

      /* Fill dropmask with a hex mask for registers to be popped.
         Registers that are not currently on stack are also marked
         dropped to reduce the number of cases.   */

      for (i = regstack->top[k]; i <= STACK_REG_LAST(k); ++i)
        if (! TEST_HARD_REG_BIT (drop_set, regstack->reg[i]))
          dropmask &= ~(1 << (i - regstack->top[k]) * 4);

again:
      switch (dropmask)
        {
        /*     CBA */
        case 0x000:          /* Nothing to drop */
          break;

        case 0x011:          /* Drop Breg,Areg */
#if 0
          emit_t800_insns ("dd", regstack);
          break;
#else
          /* The second drop is better to do with `rev' on T800. On
             T805 as well, because `rev' is short and `pop' is long.  */
          emit_t800_insns ("d", regstack);
          dropmask = 0x101;
          goto again;
#endif

        case 0x001:          /* Drop Areg */
          emit_t800_insns ("d", regstack);
          break;

        case 0x010:          /* Drop Breg only */
          /* Swap reg B to top of stack, then drop the top.  */
          emit_t800_insns ("rd", regstack);
          break;

        case 0x101:
          /* Swap reg B to top of stack, thus converting to case 0x110.  */
          emit_t800_insns ("r", regstack);

          /* FALL THROUGH */

        case 0x110: j = regstack->top[k] + 0; goto bottom_drop;
        case 0x100: j = regstack->top[k] + 1; goto bottom_drop;
        case 0x111: j = regstack->top[k] - 1; goto bottom_drop;
        bottom_drop:

          /* The regs to drop are at the bottom of stack -- no insns
             needed.  Just forget that the stack contains these regs.
             J is the index of the most deep non-dropped reg.  */

          for (i = STACK_REG_LAST(k); i > j; i--)
            CLEAR_HARD_REG_BIT (regstack->reg_set, regstack->reg[i]);

          for (i = STACK_REG_LAST(k); j >= regstack->top[k]; j--,i--)
            regstack->reg[i] = regstack->reg[j];

          regstack->top[k] = i + 1;
          break;

        default:
          /* "cannot happen" */
          abort ();
        }
    }
end:
  ;
}


/* Procedure for reg-stack reordering.

   OLDSTACK is the state of the stack at the point where we are to
   insert dropping insns. OLDSTACK is modified to reflect the effect
   of inserted insns.
   NEWSTACK is the desired stack state.

   Stack reordering sequences:

   Transformation   Insn sequence
        210         <none; the order is right>
        201         rev
        102         rev; =>120
        120         stl temp; rev; ldl temp
        012         rev; =>021
        021         stl temp; rev; ldl temp; =>201
   */

void
t800_emit_swaps (oldstack, newstack)
     stack oldstack;
     stack newstack;
{
  int i,j,k;
  char *p;

  /* The only difference must be the order of the stacks */

  GO_IF_HARD_REG_EQUAL (oldstack->reg_set, newstack->reg_set, win);
    abort ();
win:

  /* See if we need to store a reg from ABC-stack temporarily into
     memory.  This can be the case if:
     (a) ABC-stack is full and the reg at the bottom needs to be
     swapped; we need to remove one reg as `rev' cannot access the reg
     at the bottom;
     (b) FABC-stack is full as well and the reg at the bottom needs to be
     swapped; we'll need to remove the reg at the top of FABC-stack,
     which requires a reg of ABC-stack for address.
     
     The code below occasionally uses stack->reg[R] instead of
     REG_BY_HARD_REGNO (stack, R); it yields the same effect when the
     stack is full.  */

  if (STACK_DEPTH (oldstack, ABC_STACKNO) == 3
      && (oldstack->reg[R_CREG] != newstack->reg[R_CREG]
          || STACK_DEPTH (oldstack, FABC_STACKNO) == 3
          && oldstack->reg[R_FCREG] != newstack->reg[R_FCREG]))
    {
      struct stack_def tmpstack;

      /* We need to store a reg.  If the reg currently on top is the
         one that needs to be at the bottom in the new stack, emit
         `rev' before storing.  */
      if (oldstack->reg[R_AREG] == newstack->reg[R_CREG])
        emit_t800_insns ("rs", oldstack);
      else
        emit_t800_insns ("s", oldstack);

      /* Handle the simplified situation by a recursive call.  To do
         this, we need to make a modified copy of the new stack that
         does not contain the register just stored.  Note that this
         reg is always either top or next-to-top in the new stack.  */
      bcopy (newstack, &tmpstack, sizeof (struct stack_def));
      if (TEST_HARD_REG_BIT (oldstack->reg_set, tmpstack.reg[R_AREG]))
        {
          /* The reg stored is next-to-top in newstack */
          CLEAR_HARD_REG_BIT (tmpstack.reg_set, tmpstack.reg[R_BREG]);
          tmpstack.reg[R_BREG] = tmpstack.reg[R_AREG];
          tmpstack.top[ABC_STACKNO]++;
        }
      else
        {
          CLEAR_HARD_REG_BIT (tmpstack.reg_set, tmpstack.reg[R_AREG]);
          tmpstack.top[ABC_STACKNO]++;
        }
      
      t800_emit_swaps (oldstack, &tmpstack);

      /* Load the stored reg back, then fall into the common part
         to check whether it is in the correct place.  */
      emit_t800_insns ("l", oldstack);
    }

  else

  /* See if we need to store a reg from FABC-stack temporarily into
     memory.  Now we're sure to have a room on ABC-stack to hold the
     address needed to do the store.  */
  if (STACK_DEPTH (oldstack, FABC_STACKNO) == 3
      && oldstack->reg[R_FCREG] != newstack->reg[R_FCREG])
    {
      struct stack_def tmpstack;

      if (oldstack->reg[R_FAREG] == newstack->reg[R_FCREG])
        emit_t800_insns ("RS", oldstack);
      else
        emit_t800_insns ("S", oldstack);

      bcopy (newstack, &tmpstack, sizeof (struct stack_def));
      if (TEST_HARD_REG_BIT (oldstack->reg_set, tmpstack.reg[R_FAREG]))
        {
          /* The reg stored is next-to-top in newstack */
          CLEAR_HARD_REG_BIT (tmpstack.reg_set, tmpstack.reg[R_FBREG]);
          tmpstack.reg[R_FBREG] = tmpstack.reg[R_FAREG];
          tmpstack.top[FABC_STACKNO]++;
        }
      else
        {
          CLEAR_HARD_REG_BIT (tmpstack.reg_set, tmpstack.reg[R_FAREG]);
          tmpstack.top[FABC_STACKNO]++;
        }
      
      t800_emit_swaps (oldstack, &tmpstack);
      emit_t800_insns ("L", oldstack);
    }

  /* Now the situation must be simple enough to do with no storing;
     all we have to do is to emit `rev' and `fprev' if necessary.  */

  if (STACK_DEPTH (oldstack, ABC_STACKNO) >= 2
      && REG_BY_HARD_REGNO (oldstack, R_AREG)
      != REG_BY_HARD_REGNO (newstack, R_AREG))
    emit_t800_insns ("r", oldstack);

  if (STACK_DEPTH (oldstack, FABC_STACKNO) >= 2
      && REG_BY_HARD_REGNO (oldstack, R_FAREG)
      != REG_BY_HARD_REGNO (newstack, R_FAREG))
    emit_t800_insns ("R", oldstack);
}


/* Initialize specified registers to something (zero). The order
   of pushing is arbitrary (which is not too good, but simple).
   Reflect the effect of insns emitted in REGSTACK. */

void
t800_emit_pushes (regstack, push_set)
     stack regstack;
     HARD_REG_SET push_set;
{
  int i,k;
  enum machine_mode mode;

  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    for (i = STACK_REG_FIRST (k); i <= STACK_REG_LAST(k); ++i)
      {
        if (!TEST_HARD_REG_BIT (push_set, i))
          continue;
        mode = (k == ABC_STACKNO? SImode : SFmode);
        emit_insn (gen_rtx (SET, mode, hard_reg[STACK_REG_FIRST(k)][mode],
                            CONST0_RTX (mode)));
        regstack->reg[--regstack->top[k]] = i;
        SET_HARD_REG_BIT (regstack->reg_set, i);
      }
}
