/* Optional RTL-fixing pass for GNU compiler.
   Copyright (C) 1994 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* This file performs a pass over the RTL code, adopting it to various
   strange requirements of certain processors.

   (`Certain' means T800 and relatives for now)

   Condition 1.  Certain machines require the arguments for a binary
   insns to be in distinct registers.  If we find an insn like `add
   Rx,Rx', we convert it by creating new pseudo and emitting the insn
   to copy the operand into it just before that insn.

   Condition 2.  Certain machines require every register to be used
   just once, as the value in a register is destroyed by the use.
   (** we do nothing for them here now, but instead force them to
    stack slots during register allocation.  */

#include <stdio.h>
#include "config.h"
#include "rtl.h"
#include "insn-config.h"
#include "recog.h"

static void fix_multiple_reg_references PROTO((rtx, int, int, char *const *,
					       int));

/* Set to 1 by toplev once the exotic pass has been run. */

int exotic_completed = 0;


void
exotic (insns, nregs)
     rtx insns;
     int nregs;
{
#ifdef EXOTIC_PASS
  rtx insn;

  for (insn = insns; insn; insn = NEXT_INSN (insn))
    {
      /* Skip non-insns */
      if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
	continue;

      switch (GET_CODE (PATTERN (insn)))
	{
	case USE:
	case CLOBBER:
	case ASM_INPUT:
	case ADDR_VEC:
	case ADDR_DIFF_VEC:
	  continue;
	}

      find_multiple_reg_references (insn, fix_multiple_reg_references);
    }
}


/* Check is there is a pseudo mentioned in more than one input operand
   of the INSN.  For every pair of operands referencing the same reg
   FUN is called with INSN and numbers of the operands as its arguments.
   Return 1 is multiple uses foung, 0 otherwise.

   This is also called from recog_for_combine.  */

int
find_multiple_reg_references (insn, fun)
     rtx insn;
     void (*fun) PROTO((rtx, int, int, char *const *, int));
{
  rtx op1, op2;
  int i, j;
  int found = 0;
  char *const *constraints;
  int n_operands, n_dups;

  n_operands = asm_noperands (PATTERN (insn));
  if (n_operands >= 0)
    {
      /* This insn is an `asm' with operands.  */

      /* expand_asm_operands makes sure there aren't too many operands.  */
      if (n_operands > MAX_RECOG_OPERANDS)
	abort ();

      constraints = (char **) alloca (n_operands * sizeof (char *));
      decode_asm_operands (PATTERN (insn), recog_operand, recog_operand_loc,
			   (char **)constraints, NULL_PTR);
      n_dups = 0;
    }
  else
    {
      /* Ordinary insn */

      int insn_code = recog_memoized (insn);
      if (insn_code < 0)
        fatal_insn_not_found (insn);

      n_operands = insn_n_operands[insn_code];
      constraints = insn_operand_constraint[insn_code];
      insn_extract (insn);
      n_dups = insn_n_dups[insn_code];
    }

  for (i = n_operands - 1; i >= 0; i--)
    {
      /* Don't consider output operands */
      if (constraints[i][0] == '=')
	continue;

      op1 = recog_operand[i];
      if (GET_CODE (op1) == SUBREG)
	op1 = SUBREG_REG (op1);
      if (GET_CODE (op1) != REG)
	continue;

      for (j = i - 1; j >= 0; j--)
	{
	  if (constraints[j][0] == '=')
	    continue;

	  op2 = recog_operand[j];
	  if (GET_CODE (op2) == SUBREG)
	    op2 = SUBREG_REG (op2);
	  if (GET_CODE (op2) != REG)
	    continue;

	  if (REGNO (op1) == REGNO (op2)
	      && REGNO (op1) >= FIRST_PSEUDO_REGISTER)
	    {
	      if (! fun)
		return 1;
	      (*fun) (insn, i, j, constraints, n_dups);
	      found = 1;
	    }
	}
    }

  return found;
}


/* Fix insn that has two input operands in the same reg.  OI1 and OI2
   are the numbers of conflicting operands.  recog_operands & relatives
   are assumed to be set up for the INSN  */

static void
fix_multiple_reg_references (insn, oi1, oi2, constraints, n_dups)
     rtx insn;
     int oi1, oi2;
     char *const *constraints;
     int n_dups;
{
  int chosen_oi;
  rtx new_reg;
  rtx *loc;
  rtx seq;
  int i;

  /* Chose which of two conflictiong operands we will fix. */

  if (constraints[oi1][0] != '+')
    chosen_oi = oi1;
  else if (constraints[oi2][0] != '+')
    chosen_oi = oi2;
  else
    /* Two input/output operands using the same pseudo?  Imaginable (say,
       different SUBREGs of it), but current t800 md has no such insns.  */
    abort ();

  loc = recog_operand_loc[chosen_oi];
  if (GET_CODE (*loc) == SUBREG)
    loc = &SUBREG_REG (*loc);
  if (GET_CODE (*loc) != REG)
    abort ();

  /* Generate new pseudo and emit an insn to copy the duplicate reg there */
  start_sequence ();
  new_reg = gen_reg_rtx (GET_MODE (*loc));
  emit_move_insn (new_reg, *loc);
  seq = get_insns ();
  end_sequence ();

  emit_insns_before (seq, insn);

  /* Change the chosen operand to use the new pseudo */

  *loc = new_reg;

  /* Change all DUP's of that operand, too.  */

  for (i = n_dups - 1; i >= 0; i--)
    {
      if (recog_dup_num[i] != chosen_oi)
	continue;
      loc = recog_dup_loc[i];
      if (GET_CODE (*loc) == SUBREG)
	loc = &SUBREG_REG (*loc);
      if (GET_CODE (*loc) != REG)
	abort ();
      *loc = new_reg;
    }

  /* We have modified the insn; mark it unrecognized to be safe */

  INSN_CODE (insn) = -1;

#endif /* EXOTIC_PASS */
}

