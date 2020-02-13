/* Convertor to stack-like register model for GNU compiler.
   Copyright (C) 1992, 1993, 1994, 1995 Free Software Foundation, Inc.

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


/* This is a replacement reg-stack converter.  It is build on ideas
   and code from reg-stack.c, but is (hopefully) tunable to different
   architectures.

   Generalized insn operating on stack-regs affects a register stack
   as follows:

   (1) The insn takes some input regs, that may be (in general)
   anywhere on the stack.

   (2) It may pop some of the input regs off the stack. These are
   called `popped inputs', and must be grouped near the top of stack.

   (3) It may push some outputs onto the stack. These are called
   `pushed outputs', and are added at the top of stack.

   (4) It may write some outputs over the non-popped inputs. These are
   `non-pushed' outputs.

   Here is how the sorts of operands according to above classification
   are determined:

   (a) By default, the inputs are non-popped. If the insn pops some
   input, its pattern should either have a popped_reg attribute
   (for normal insns) or contain a clobber for this input (for ASMs).

   (b) By default, outputs are pushed. For output to be non-pushed,
   there should exist a non-popped input constrained to match this
   output.

   The converter comprises two passes over rtl.  The first pass does
   life analysis for stack registers, collecting information which is
   used in the second pass, which is the converter proper.

   The second pass scans the insns, keeping track of the current state
   of register stack(s).  For every insn mentioning stack registers,
   it does the following:

   (1) Brings the insns inputs to proper positions on the stack,
   according to insn's constraints, by emitting stack shuffling
   sequence before the insn.

   (2) Emits code after the insn to remove the regs which are not live
   after the insn from the stack.  This way, register stack contains
   *only* live registers.

   (3) Optionally replaces references to stack regs in the insn with
   "hard" registers, whose numbers are convertable into offsets from
   stack top, which can be necessary for assembly output.  t800 does
   not need this, and the code for this is incomplete.

   */


/* Note: all the code under #ifdef STACK_REG_STRICT or #ifdef
   FUNCTION_EPILOGUE is untested or almost untested. */


/* These are already included by reg-stack.c:
#include <stdio.h>
#include "config.h"
 */

#ifdef STACK_REGS

#include "tree.h"
#include "rtl.h"
#include "insn-config.h"
#include "insn-codes.h"
#include "insn-flags.h"         /* for gen_nop */
#include "insn-attr.h"
#include "regs.h"
#include "flags.h"
#include "recog.h"
#include "output.h"
#include "hard-reg-set.h"
#include "reg-stack.h"

/* highest instruction uid */
static int max_uid = 0;

/* Number of basic blocks in the current function.  */
static int blocks;

/* Element N is first insn in basic block N.
   This info lasts until we finish compiling the function.  */
static rtx *block_begin;

/* Element N is last insn in basic block N.
   This info lasts until we finish compiling the function.  */
static rtx *block_end;

/* Element N is nonzero if control can drop into basic block N.
   Bit 0 says the control drops in from the preceding block.
   Bit 1 says the control drops in from an agreeing sequence emitted
   before the block.  */
static char *block_drops_in;

/* Element N is the reg-stack state at the entry of block N */
static stack block_stack_in;

/* Element N is the reg-stack registers' life at the end of block N */
static HARD_REG_SET *block_out_reg_set;

/* Element N is the mode of the registers live at the end of block N.  */
static enum machine_mode (*block_out_reg_mode)[FIRST_PSEUDO_REGISTER];

/* This is the register file for all register after conversion */
rtx hard_reg[FIRST_PSEUDO_REGISTER][(int) MAX_MACHINE_MODE];

/* This is where the BLOCK_NUM values are really stored.  This is set
   up by find_blocks and used there and in life_analysis.  It can be used
   later, but only to look up an insn that is the head or tail of some
   block.  life_analysis and the stack register conversion process can
   add insns within a block. */
static short *block_number;

/* Get the basic block number of an insn.  See the comment just above.  */

#define BLOCK_NUM(INSN)  \
  ((INSN_UID (INSN) > max_uid)	\
   ? (abort() , -1) : block_number[INSN_UID (INSN)])


extern rtx forced_labels;

struct reg_life_data;
struct operand_data;

rtx stack_reg_mentioned                 PROTO((rtx));
rtx stack_reg_but_one_mentioned         PROTO((rtx, rtx));
void reg_to_stack                       PROTO((rtx, FILE *));
static void record_label_references     PROTO((rtx, rtx));
static int analyse_constraints          PROTO((rtx, int, rtx *,
                                               char *const *,
                                               struct insn_info *));
static int letter_wins                  PROTO((int, struct operand_data *));
static int check_rules                  PROTO((rtx, stack,
                                               struct insn_info *));
static int check_rule2                  PROTO((stack, struct insn_info *));
static int ior_set                      PROTO((rtx, rtx));
static void record_reg_life_pat         PROTO((rtx, struct reg_life_data *));
static int n_extra_stack_reg_operands   PROTO((rtx));
static int record_extra_stack_reg_operands PROTO((rtx, HARD_REG_SET *,
                                                  enum machine_mode *));
static rtx skip_uses                    PROTO((rtx));
static rtx find_jump_table              PROTO((rtx));
static void record_reg_life             PROTO((rtx, stack));
static void find_blocks                 PROTO((rtx));
static int uses_reg_or_mem		PROTO((rtx));
static rtx stack_result                 PROTO((tree));
static void stack_reg_life_analysis     PROTO((rtx));
static void ior_regset_and_mode         PROTO((HARD_REG_SET *,
                                               enum machine_mode *,
                                               stack));
static rtx emit_zero_init_before        PROTO((HARD_REG_SET *, stack, rtx));
static void replace_reg                 PROTO((rtx *, int));
static int get_hard_regnum              PROTO((stack, rtx));
int stacks_equal                        PROTO((stack, stack));
void init_stack                         PROTO((stack));
int print_stack                         PROTO((stack));
static void delete_insn_for_stacker     PROTO((rtx));
static int del_stack_reg_move           PROTO((rtx, stack));
static void displace_reg                PROTO((stack, int, rtx));
static int place_reg                    PROTO((stack, int, rtx));
static int place_inputs                 PROTO((int, stack, stack,
                                               struct insn_info *));
static int place_regs                   PROTO((stack, stack,
                                               struct insn_info *));
void set_hard_reg_bits                  PROTO((HARD_REG_SET *, rtx));
void set_hard_reg_bits_3                PROTO((HARD_REG_SET *, int, enum machine_mode));
void clear_hard_reg_bits                PROTO((HARD_REG_SET *, rtx));
static void subst_regs_recursively      PROTO((rtx *, stack,
                                               HARD_REG_SET *));
void print_insn_info                    PROTO((struct insn_info *));
static void analyse_insn                PROTO((rtx, struct insn_info *));
static int sort_insn_info_cmp           PROTO((struct insn_info_reg *,
                                               struct insn_info_reg *));
static void sort_insn_info_in           PROTO((struct insn_info *));
static void sort_insn_info_out          PROTO((struct insn_info *));
static void pop_inputs                  PROTO((stack, struct insn_info *,
                                              int));
static void subst_stack_regs            PROTO((rtx, stack, stack));
static void change_stack                PROTO((rtx, stack, stack,
                                               rtx (*)()));
static void goto_block_pat              PROTO((rtx, stack, rtx));
static rtx subst_stack_regs_from_to     PROTO((rtx, rtx, stack, stack));
static rtx subst_stack_regs_in_group    PROTO((rtx, rtx, stack, stack));
static rtx recognize_load_atom          PROTO((rtx, rtx));
static void atom_force_single_reg       PROTO((rtx, rtx, rtx));
static void atom_force_single_reg_1     PROTO((rtx *, rtx));
static void convert_regs                PROTO(());
static void print_blocks                PROTO((FILE *, rtx, rtx));
static void dump_stack_info             PROTO((FILE *));
static rtx walk_alter_subreg            PROTO((rtx));
int note_popped_inputs                  PROTO((rtx, int (*)()));


/* Return the first stack register mentioned within PAT,
   or NULL_RTX if no stack registers are mentioned.
   Pseudo registers are accepted as well, since they may be assigned
   to stack registers; thus this function can be used from
   `note_insn_popped_input'.  */

rtx
stack_reg_mentioned (pat)
     rtx pat;
{
  register char *fmt;
  register int i, j;
  register rtx reg;

  if (GET_CODE (pat) == REG
      && (STACK_REG_P (pat)
	  || REGNO (pat) >= FIRST_PSEUDO_REGISTER))
    return pat;

  /* Return stack registers together with enclosing subreg, if there
     is one, for the sake of note_popped_inputs */

  if (GET_CODE (pat) == SUBREG
      && GET_CODE (SUBREG_REG (pat)) == REG
      && (STACK_REG_P (SUBREG_REG (pat))
	  || REGNO (SUBREG_REG (pat)) >= FIRST_PSEUDO_REGISTER))
    return pat;

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)); --i >= 0; )
    if (fmt[i] == 'E')
      {
        for (j = XVECLEN (pat, i); --j >= 0; )
          if (reg = stack_reg_mentioned (XVECEXP (pat, i, j)))
            return reg;
      }
    else if (fmt[i] == 'e')
      if (reg = stack_reg_mentioned (XEXP (pat, i)))
        return reg;

  return NULL_RTX;
}

/* Return the first stack register mentioned within PAT which is not
   THEONE, or NULL_RTX if no stack registers other than THEONE are
   mentioned.  */

rtx
stack_reg_but_one_mentioned (pat, theone)
    rtx pat, theone;
{
  register char *fmt;
  register int i, j;
  register rtx reg;

  if (STACK_REG_P (pat))
    return rtx_equal_p (pat, theone)? NULL_RTX : pat;

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)); --i >= 0; )
    if (fmt[i] == 'E')
      {
        for (j = XVECLEN (pat, i); --j >= 0; )
          if (reg = stack_reg_but_one_mentioned (XVECEXP (pat, i, j), theone))
            return reg;
      }
    else if (fmt[i] == 'e')
      if (reg = stack_reg_but_one_mentioned (XEXP (pat, i), theone))
        return reg;

  return NULL_RTX;
}


/* Convert register usage from "flat" register file usage to a "stack
   register file.  FIRST is the first insn in the function, FILE is the
   dump file, if used.

   First compute the beginning and end of each basic block.  Do a
   register life analysis on the stack registers, recording the result
   for the head and tail of each basic block.  Then convert each insn one
   by one.  Run a last jump_optimize() pass, if optimizing, to eliminate
   any cross-jumping created when the converter inserts pop insns.  */

void
reg_to_stack (first, file)
     rtx first;
     FILE *file;
{
  register rtx insn;
  register int i;
  int stack_reg_seen = 0;
  int mode;

  for (mode = 0; mode < MAX_MACHINE_MODE; ++mode)
    for (i = 0; i < FIRST_PSEUDO_REGISTER; ++i)
      hard_reg[i][mode] = gen_rtx (REG, (enum machine_mode)mode, i);

  /* Count the basic blocks.  Also find maximum insn uid.  */
  {
    register RTX_CODE prev_code = BARRIER;
    register RTX_CODE code;

    max_uid = 0;
    blocks = 0;
    for (insn = first; insn; insn = NEXT_INSN (insn))
      {
        /* Note that this loop must select the same block boundaries
	   as code in find_blocks.  Also note that this code is not the
	   same as that used in flow.c.  */

        if (INSN_UID (insn) > max_uid)
          max_uid = INSN_UID (insn);

        /* Remember whether or not this insn mentions stack regs. */

        code = GET_CODE (insn);
        if ((GET_RTX_CLASS (code) == 'i'
	     && stack_reg_mentioned (PATTERN (insn)))
            || (code == CALL_INSN
                || (code == JUMP_INSN
		    && GET_CODE (PATTERN (insn)) == RETURN))
               && n_extra_stack_reg_operands (insn))
          {
            stack_reg_seen = 1;
            PUT_MODE (insn, QImode);

            /* Get rid of SUBREGs beforehand to simplify the code of this pass */
            walk_alter_subreg (PATTERN (insn));
          }
        else
          PUT_MODE (insn, VOIDmode);

        switch (code)
          {
            case JUMP_INSN:
              {
                /* Treat tablejump insn and the table following it as
                   a whole; don't allow the table to fall into
                   a separate block. */
                rtx insn2 = find_jump_table (PATTERN (insn));
                if (insn2)
                  {
                    insn = insn2;
                    if (INSN_UID (insn) > max_uid)
                      max_uid = INSN_UID (insn);
                  }
              }
            case INSN:
            case CALL_INSN:
              if (prev_code != JUMP_INSN && prev_code != BARRIER)
                break;
            case CODE_LABEL:
              blocks++;
          }

	if (code == CODE_LABEL)
	  LABEL_REFS (insn) = insn; /* delete old chain */

        if (code != NOTE)
          prev_code = code;
      }
  }

  /* If no stack register reference exists in this insn, there isn't
     anything to convert.  */

  if (! stack_reg_seen)
    return;

  /* If there are stack registers, there must be at least one block. */

  if (! blocks)
    abort ();

  /* Allocate some tables that last till end of compiling this function
     and some needed only in find_blocks and life_analysis. */

  block_begin = (rtx *) alloca (blocks * sizeof (rtx));
  block_end = (rtx *) alloca (blocks * sizeof (rtx));
  block_drops_in = (char *) alloca (blocks);

  block_stack_in = (stack) alloca (blocks * sizeof (struct stack_def));
  block_out_reg_set = (HARD_REG_SET *) alloca (blocks * sizeof (HARD_REG_SET));
  block_out_reg_mode = (enum machine_mode (*)[FIRST_PSEUDO_REGISTER])
    alloca (blocks * FIRST_PSEUDO_REGISTER * sizeof (enum machine_mode));
  bzero ((char *) block_stack_in, blocks * sizeof (struct stack_def));
  bzero ((char *) block_out_reg_set, blocks * sizeof (HARD_REG_SET));

  block_number = (short *) alloca ((max_uid + 1) * sizeof (short));

  find_blocks (first);
  stack_reg_life_analysis (first);

  /* Dump the life analysis debug information before jump
     optimization, as that will destroy the LABEL_REFS we keep the
     information in. */

  if (file)
    dump_stack_info (file);

  convert_regs ();

#ifndef JUMP_CLOBBERS_REGS
  if (optimize)
    jump_optimize (first, 2, 0, 0);
#endif
}

/* Check PAT, which is in INSN, for LABEL_REFs.  Add INSN to the
   label's chain of references, and note which insn contains each
   reference. */

static void
record_label_references (insn, pat)
     rtx insn, pat;
{
  register enum rtx_code code = GET_CODE (pat);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (pat, 0);
      register rtx ref;

      if (GET_CODE (label) != CODE_LABEL)
        abort ();

      /* Don't make a duplicate in the code_label's chain. */

      for (ref = LABEL_REFS (label);
	   ref && ref != label;
	   ref = LABEL_NEXTREF (ref))
        if (CONTAINING_INSN (ref) == insn)
          return;

      CONTAINING_INSN (pat) = insn;
      LABEL_NEXTREF (pat) = LABEL_REFS (label);
      LABEL_REFS (label) = pat;

      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        record_label_references (insn, XEXP (pat, i));
      if (fmt[i] == 'E')
        {
          register int j;
          for (j = 0; j < XVECLEN (pat, i); j++)
            record_label_references (insn, XVECEXP (pat, i, j));
        }
    }
}

#if 0   /* Currently unused */
/* Return a pointer to the REG expression within PAT.  If PAT is not a
   REG, possible enclosed by a conversion rtx, return the inner part of
   PAT that stopped the search. */

rtx *
get_true_reg (pat)
     rtx *pat;
{
  for(;;)
    switch (GET_CODE (*pat))
      {
/*      case SUBREG:    it might be not a mere conversion! */
      case SIGN_EXTEND:
      case ZERO_EXTEND:
      case FLOAT_EXTEND:
      case TRUNCATE:
      case FLOAT_TRUNCATE:
      case FLOAT:
      case UNSIGNED_FLOAT:
      case FIX:
      case UNSIGNED_FIX:
        pat = & XEXP (*pat, 0);
        continue;
      default:
        return pat;
      }
}
#endif



/* Scan the OPERANDS and OPERAND_CONSTRAINTS of an insn, collecting
   the stack-reg-relevant info in the insn_info structure.

   The code supposes matching operands to have the same mode (or rather,
   to have the modes that occupy the same number of hard registers.)  */

struct operand_data {
  rtx x;                    /* the operand rtx */
  int n;                    /* the operand number */
  enum reg_class class;     /* class derived from STACK_REG_CLASS_OK_FOR_LETTER */
  char flags;               /* bits from enum operand_flags */
  char match;               /* if >= 0, the operand matched by this one  */
  char comm1;               /* this operand has '%' constraint */
  char comm2;               /* previous operand has '%' constraint */
  char comm1_win;           /* really can commute this op with the next one */
  char comm2_win;           /* ditto for the previous one */
  char *constraint;         /* running pointer to the constraint string */
};

static int
analyse_constraints (insn, n_operands, operands, operand_constraints, info)
     rtx insn;
     int n_operands;
     rtx *operands;
     char *const *operand_constraints;
     struct insn_info *info;
{
  struct operand_data dat[MAX_RECOG_OPERANDS];
  struct operand_data *this_op;
  struct operand_data *last_op = &dat[n_operands - 1];
  int n_alternatives;
  int this_alternative;


  /* Constraintless pattern would cause problems in the code below. */
  if (n_operands == 0 || operand_constraints[0] == 0)
    return 0;

  for (this_op = dat; this_op <= last_op; this_op++)
    {
      this_op->n = this_op - dat;
      this_op->x = operands[this_op->n];
      this_op->constraint = operand_constraints[this_op->n];
      this_op->match = -1;
      this_op->comm1 = this_op->comm2 = 0;
      this_op->flags = INPUT;
    }

  /* Compute the number of alternatives in the operands.  reload has
     already guaranteed that all operands have the same number of
     alternatives.  */

  n_alternatives = info->is_asm
    ? n_occurrences (',', operand_constraints[0]) + 1
    : insn_n_alternatives[INSN_CODE (insn)];

  /* ??? Bug: this code will return -1 if the constraints are empty. */

  this_alternative = 0;
  while (this_alternative < n_alternatives)
    {
      int lose = 0;

      /* No operands match, no narrow class requirements yet.  */
      for (this_op = dat; this_op <= last_op; this_op++)
        {
          /* [=+%] constraint chars affect all the alternatives.
             Do not reset their respective flags.  */
          this_op->flags &= (INPUT|OUTPUT|PUSHED);
          this_op->class = NO_REGS;
          this_op->comm1_win = 0;
          this_op->comm2_win = 0;
        }

      for (this_op = dat; this_op <= last_op; this_op++)
        {
          char *p = this_op->constraint;
          int win = 0;
          int c;

          if (*p == 0 || *p == ',')
            win = this_op->comm1_win = this_op->comm2_win = 1;

          while (*p && (c = *p++) != ',')
            {
              switch (c)
                {
                case '?':
                case '!':
                case '*':
                  break;

                case '=':
                  this_op->flags |= PUSHED|OUTPUT;
                  this_op->flags &= ~INPUT;
                  break;

                case '+':
                  this_op->flags |= INPUT|OUTPUT; 
                  this_op->flags &= ~PUSHED;
                  break;

                case '%':
                  this_op->comm1 = (this_op + 1)->comm2 = 1;
                  break;

                case '&':
                  this_op->flags |= EARLYCLOBBER; 
		  break;

                case '#':
                  /* Ignore the rest of this alternative. */
                  while (*p && *p != ',')
		    p++;
                  break;

                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                  if (operands_match_p (this_op->x, dat[c - '0'].x))
                    {
                      this_op->match = c - '0';
                      this_op->flags &= ~POPPED;
                      dat[c - '0'].flags &= ~PUSHED;
                      win = 1;
                    }
                  break;

		  /* 'P' in constraints is the way asms use to indicate
		     the the input opperand is popped by the asm */

		case 'P':
                  this_op->flags |= POPPED;
                  break;

                default:
                  /* Take shortcut if we know this alternative fails  */
                  if (lose)
                    break;

                  /* This code offers every letter a chance to
                     contribute to the class.  However, the real
                     contributors are (some of) the register letters
                     and EXTRA_CONSTRAINT letters.  For all the rest,
                     STACK_REG_CLASS_FROM_LETTER returns NO_REGS, so
                     this statement does not change this_op->class.  */

                  this_op->class = reg_class_subunion[(int) this_op->class]
                                    [(int) STACK_REG_CLASS_FROM_LETTER (c)];

                  win |= letter_wins (c, this_op);
                  if (this_op->comm1)
                    this_op->comm1_win |= letter_wins (c, this_op+1);
                  if (this_op->comm2)
                    this_op->comm2_win |= letter_wins (c, this_op-1);
                  break;
                }

            }

          this_op->constraint = p;

          /* If this operand did not win somehow,
             this alternative loses.  */
          if (! win)
            lose = 1;
        }

      if (! lose)
        break;
      this_alternative++;
    }

  if (this_alternative == n_alternatives)
    return -1;


  /* Merge the classes for commutative operands and flags for matched ones.
     ??? If the modes of the matched operands differ so that NREGS are
     different, we'll crash.  Is this allowed in GCC at all?  Anyway,
     T800 md does not use this.  */

  for (this_op = dat; this_op <= last_op; this_op++)
    {
      if (this_op->comm1 && this_op->comm1_win && (this_op+1)->comm2_win)
        this_op->class = (this_op+1)->class =
          reg_class_subunion[this_op->class][(this_op+1)->class];
      if (this_op->match >= 0)
        dat[this_op->match].flags |= this_op->flags;
    }

  /* Transfer the information from OPERAND_DATA to INFO structure */

  CLEAR_HARD_REG_SET (info->out_regs);

  for (this_op = dat; this_op <= last_op; this_op++)
    {
      rtx op = this_op->x;

      if (this_op->match >= 0)
        continue;

      while (GET_CODE (op) == SUBREG
             || GET_CODE (op) == STRICT_LOW_PART
             || GET_CODE (op) == ZERO_EXTRACT
             || GET_CODE (op) == SIGN_EXTRACT)
        op = XEXP (op, 0);

      /* If the operand is not a REG, it may be something containing a
         stack-reg within.  In this case, this stack-reg is input for
         the insn.  Note that this approach will not work if more than
         one stack register is hidden within the object, although
         there is no check to ensure that the condition holds.  */

      if (GET_CODE (op) != REG)
        {
          op = stack_reg_mentioned (op);
          if (!op)
            /* no stack-reg within */
            continue;
          this_op->flags |= INPUT;
          this_op->flags &= ~OUTPUT;
        }

      if (! STACK_REG_P (op))
        continue;

      /* Take care of operands that appear within a CLOBBER.

	 One possibility is that the clobbered register does not
	 appear anywhere else in the insn, eg. (clobber (match_scratch)).
	 The purpose of such CLOBBERs is to reserve some room on a
	 reg-stack; register allocators and reload will honor the
	 clobber and reserve an appropriate register.

	 The register will not be live before the insn, and hence
	 cannot be involved in stack reordering.  We don't need to
	 know about such operands in subst_stack_regs.

	 Another possibility is that the clobbered register is also an
	 input for the insn.  If this is a POPPED input, it will be
	 popped implicitly; otherwise, it will be popped explicitly
	 since life analysis puts REG_UNUSED note for every clobbered
	 register it sees.  This way, the processing is no different
	 from the case of plain non-clobbered input, so we again don't
	 need to give any info about the clobber.

	 This way, we need to recognize the clobbered operands, which
	 now look like a plain output, and ignore them.  */

      if (this_op->flags & OUTPUT)
	{
	  rtx body = PATTERN (insn);
	  int j;

	  if (GET_CODE (body) == PARALLEL)
	    {
	      for (j = XVECLEN (body, 0) - 1; j >= 0; j--)
		if (GET_CODE (XVECEXP (body, 0, j)) == CLOBBER)
		  {
		    rtx clobber = XVECEXP (body, 0, j);
		    rtx reg = XEXP (clobber, 0);

		    if (reg == op)
		      break;
		  }
	      if (j >= 0)
		/* yes, this operand mentioned within a CLOBBER -- skip it */
		continue;
	    }
	}

      if (this_op->flags & OUTPUT)
        set_hard_reg_bits (&info->out_regs, op);

      if ((this_op->flags & (PUSHED|OUTPUT)) == (PUSHED|OUTPUT))
        {
          info->out_last++;
          info->out_last->reg = op;
          info->out_last->flags = this_op->flags;
          info->out_last->class = this_op->class;
          continue;
        }

      /* This is non-matching INPUT operand or INPUT|OUTPUT operand */

      {
        struct insn_info_reg *p = ++info->in_last;
        
        p->reg = op;
        p->flags = this_op->flags;
        p->class = this_op->class;

        /* If this is a pure INPUT, find whether it is POPPED */
        if ((p->flags & (INPUT|OUTPUT)) == INPUT)
          {
            if (info->is_asm)
              {

#if 0 /* Using clobber to indicate which input is implicitly popped by
	 asm is now obsolete; asms should use the 'P' constraint on
	 the operands that are implicitly popped.  Using clobbers for
	 this creates problems with register allocation, and leaves no
	 way to specify that the insn destroys the value in a
	 register, but does not implicitly pop it (well, no need for
	 this now, but...)  */

                rtx body = PATTERN (insn);
                int j;

                if (GET_CODE (body) == PARALLEL)
                  for (j = 0; j < XVECLEN (body, 0); j++)
                    if (GET_CODE (XVECEXP (body, 0, j)) == CLOBBER)
                      {
                        rtx clobber = XVECEXP (body, 0, j);
                        rtx reg = XEXP (clobber, 0);

                        if (operands_match_p (this_op->x, reg))
                          {
                            this_op->flags |= POPPED|JUMP_POPPED;
                            break;
                          }
                      }
#endif
              }
            else
              {
                int this_op_mask = 1 << this_op->n;
                if (get_attr_popped_inputs (insn) & this_op_mask)
#ifndef HAVE_ATTR_popped_inputs_on_jump
                  p->flags |= POPPED|JUMP_POPPED;
#else
                  p->flags |= POPPED;
                if (GET_CODE (insn) == JUMP_INSN
                    && get_attr_popped_inputs_on_jump (insn) & this_op_mask)
                  p->flags |= JUMP_POPPED;
#endif
              }
          }
      }
    }

  return this_alternative;
}



static int
letter_wins (c, data)
  int c;
  struct operand_data *data;
{
  rtx op = data->x;

  switch (c)
    {
    case 'p':
      return strict_memory_address_p (GET_MODE (op), op);

    case 'g':
      /* Anything goes unless it is a REG and really has a hard reg
         but the hard reg is not in the class GENERAL_REGS.  */
      return (GENERAL_REGS == ALL_REGS
              || GET_CODE (op) != REG
              || reg_fits_class_p (op, GENERAL_REGS, 0, GET_MODE (op)));

    case 'r':
      return (GET_CODE (op) == REG
              && (GENERAL_REGS == ALL_REGS
                  || reg_fits_class_p (op, GENERAL_REGS, 0, GET_MODE (op))));

    case 'X':
      /* This is used for a MATCH_SCRATCH in the cases when we
         don't actually need anything.  So anything goes any time. */
      return 1;

    case 'm':
      return GET_CODE (op) == MEM;

    case '<':
      return (GET_CODE (op) == MEM
              && (GET_CODE (XEXP (op, 0)) == PRE_DEC
                  || GET_CODE (XEXP (op, 0)) == POST_DEC));

    case '>':
      return (GET_CODE (op) == MEM
              && (GET_CODE (XEXP (op, 0)) == PRE_INC
                  || GET_CODE (XEXP (op, 0)) == POST_INC));

    case 'E':
      /* Match any CONST_DOUBLE, but only if
         we can examine the bits of it reliably.  */
      if ((HOST_FLOAT_FORMAT != TARGET_FLOAT_FORMAT
                               || HOST_BITS_PER_WIDE_INT != BITS_PER_WORD)
          && GET_CODE (op) != VOIDmode && ! flag_pretend_float)
        return 0;
      /* FALL THROUGH */
    case 'F':
      return (GET_CODE (op) == CONST_DOUBLE);

    case 'G':
    case 'H':
      return (GET_CODE (op) == CONST_DOUBLE
              && CONST_DOUBLE_OK_FOR_LETTER_P (op, c));

    case 's':
      if (GET_CODE (op) == CONST_INT
          || (GET_CODE (op) == CONST_DOUBLE
              && GET_MODE (op) == VOIDmode))
        return 0;
      /* FALL THROUGH */
    case 'i':
      return (CONSTANT_P (op));

    case 'n':
      return (GET_CODE (op) == CONST_INT
              || (GET_CODE (op) == CONST_DOUBLE
                  && GET_MODE (op) == VOIDmode));

    case 'I':
    case 'J':
    case 'K':
    case 'L':
    case 'M':
    case 'N':
    case 'O':
    case 'P':
      return (GET_CODE (op) == CONST_INT
              && CONST_OK_FOR_LETTER_P (INTVAL (op), c));

#ifdef EXTRA_CONSTRAINT
    case 'Q':
    case 'R':
    case 'S':
    case 'T':
    case 'U':
      return (EXTRA_CONSTRAINT (op, c));
#endif

    case 'V':
      return (GET_CODE (op) == MEM && ! offsettable_memref_p (op));

    case 'o':
      return (offsettable_memref_p (op));

    default:
      return (GET_CODE (op) == REG
              && reg_fits_class_p (op, REG_CLASS_FROM_LETTER (c),
                                   0, GET_MODE (op)));
    }
}



/* Rules are now reformulated:
    1. PUSHED OUTPUTs must be grouped near stack top;
    2. POPPED INPUTs must be grouped near stack top;
    3. non-POPPED INPUT with constraint joint with some PUSHED OUTPUT
       requires this OUTPUT to use '&' earlyclobber. Otherwise, reload may
       use the same virtual register for these operands, which may cause
       reg-stack overflow, since the output register is pushed BEFORE the
       input one is popped as dead in this insn.
    4. PUSHED OUTPUT must have a single-register constraint; this determines
       the order of pushing.
    */

#define CHECK_ERROR(insn, str, arg) \
    do { if (asm_noperands (PATTERN (insn)) >= 0)    \
           error_for_asm (insn, str, arg);           \
         else                                        \
           abort ();                                 \
    } while (0)

static int
check_rules (insn, regstack, info)
     rtx insn;
     stack regstack;
     struct insn_info *info;
{
  int malformed_asm = 0;
  int i, j, k;
  struct insn_info_reg *p, *q;
  HARD_REG_SET popped_ins;
  HARD_REG_SET pushed_outs;

  CLEAR_HARD_REG_SET (popped_ins);
  CLEAR_HARD_REG_SET (pushed_outs);

  for (p = info->out; p <= info->out_last; p++)
    {
      /* Enforce rule #4: PUSHED OUTPUTs must have single-reg constraint */
      if (reg_class_size[p->class] != 1)
        {
          CHECK_ERROR (insn, "Output constraint %d must specify a single register", i);
          malformed_asm = 1;
        }
      IOR_HARD_REG_SET (pushed_outs, reg_class_contents[p->class]);
    }

  for (p = info->in; p <= info->in_last; p++)
    {
      if (p->flags & OUTPUT)
        continue;
      if (p->flags & POPPED)
        set_hard_reg_bits_3 (&popped_ins, get_hard_regnum (regstack, p->reg),
                             GET_MODE (p->reg));
      else
        { /* Rule #3 */
          for (q = info->out; q <= info->out_last; q++)
            if (! (q->flags & EARLYCLOBBER))
              {
                /* Doing this check properly requires knowing the
                   operand class required by REG_CLASS_FROM_LETTER, not
                   STACK_REG_CLASS_FROM_LETTER.  Do this later, since it
                   is of little use on T800; for now, just abort on any
                   non-earlyclobbered output in the insn that have at
                   least one non-popped input.

                   Hmm... this requires `dup' insn to have
                   the earlyclobber. This is unpleasant and quite
                   unnecessary, since del_stack_reg_move () would remove
                   this dup as a no-op move if SRC is the same as DEST.
                   As a quick hack, detect this and (probably) similar
                   cases with a machine-specific macro.  */
#ifdef INSN_OK_FOR_RULE_3
                if (! INSN_OK_FOR_RULE_3 (insn))
#endif
		  {
		    CHECK_ERROR (insn, "Output operand's constraint must use '&' earlyclobber", 0);
		    malformed_asm = 1;
		    break;
		  }
              }
        }
    }

  /* Rules #1, #2: */
  /* Clear bits for valid (grouped at stack tops) regs */
  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    {
      for (j = STACK_REG_FIRST(k); j <= STACK_REG_LAST(k); ++j)
        {
          if (! TEST_HARD_REG_BIT (pushed_outs, j))
            break;
          CLEAR_HARD_REG_BIT (pushed_outs, j);
        }
      for (j = STACK_REG_FIRST(k); j <= STACK_REG_LAST(k); ++j)
        {
          if (! TEST_HARD_REG_BIT (popped_ins, j))
            break;
          CLEAR_HARD_REG_BIT (popped_ins, j);
        }
    }

  /* Enforce rule #1: PUSHED OUTPUTs must be grouped near stack top */
  GO_IF_HARD_REG_EQUAL (pushed_outs, reg_class_contents[NO_REGS], win1);
    CHECK_ERROR (insn, "Output regs must be grouped at top of stack", 0);
    malformed_asm = 1;

win1:

  /* Enforce rule #2: POPPED INPUTs must be grouped near stack top */
  GO_IF_HARD_REG_EQUAL (popped_ins, reg_class_contents[NO_REGS], win2);
    CHECK_ERROR (insn, 
     "Implicitly popped input regs must be grouped at top of stack", 0);
    malformed_asm = 1;

win2:

  return (- malformed_asm);
}


/* Reduced version of check-rules for use from PLACE_INPUTS().
   Only rule #2 is checked */

static int
check_rule_2 (regstack, info)
     stack regstack;
     struct insn_info *info;
{
  int i, j, k;
  struct insn_info_reg *p;
  HARD_REG_SET popped_ins;

  CLEAR_HARD_REG_SET (popped_ins);

  for (p = info->in; p <= info->in_last; p++)
    if (p->flags & POPPED)
      set_hard_reg_bits_3 (&popped_ins, get_hard_regnum (regstack, p->reg),
                           GET_MODE (p->reg));

  /* Rule #2: */
  /* Clear bits for valid (grouped at stack tops) regs */
  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    for (j = STACK_REG_FIRST(k); j <= STACK_REG_LAST(k); ++j)
      {
        if (! TEST_HARD_REG_BIT (popped_ins, j))
          break;
        CLEAR_HARD_REG_BIT (popped_ins, j);
      }

  /* Check rule #2: POPPED INPUTs must be grouped near stack top */
  GO_IF_HARD_REG_EQUAL (popped_ins, reg_class_contents[NO_REGS], win2);
    return 0;
win2:
  return 1;
}


static HARD_REG_SET *ior_set_arg;

static int
ior_set (insn, reg)
     rtx insn;
     rtx reg;
{
  set_hard_reg_bits (ior_set_arg, reg);
  return 0;
}

/* Scan PAT, which is a part of INSN, and record registers appearing in
   a SET_DEST in DEST, and other registers in SRC.  For an SRC, store
   the mode of the value currently living in the SRC in regstack->mode.
   This information needs not be very accurate, since it is used only
   to find out what is the correct mode to reload in/out of a reg
   during the stack shuffling.  */

struct reg_life_data {
  HARD_REG_SET dest;
  HARD_REG_SET src;
  HARD_REG_SET clobbered;
  stack regstack;
};

static void
record_reg_life_pat (pat, data)
     rtx pat;
     struct reg_life_data *data;
{
  register char *fmt;
  register int i,j;

  switch (GET_CODE (pat))
    {
    case REG:
      if (STACK_REG_P (pat))
        {
          int regno = REGNO (pat);
          int r = regno + HARD_REGNO_NREGS (regno, GET_MODE (pat));
          while (--r >= regno)
            {
              SET_HARD_REG_BIT (data->src, r);
              data->regstack->mode[r] = GET_MODE (pat);
            }
        }
      return;

    case SET:
        {
          /* Uncover the real SET_DEST.  Note that we don't expect
             SUBREGs with SUBREG_REG != 0 here as those are stripped
             at the start of the reg-stack pass.  */

          rtx lhs = SET_DEST (pat);
          while (GET_CODE (lhs) == SUBREG
                 || GET_CODE (lhs) == STRICT_LOW_PART
                 || GET_CODE (lhs) == ZERO_EXTRACT
                 || GET_CODE (lhs) == SIGN_EXTRACT)
            lhs = XEXP (lhs, 0);

          /* MEM address should count for src rather than dest.  */

          if (GET_CODE (lhs) == MEM)
            record_reg_life_pat (XEXP (lhs, 0), data);
          else if (STACK_REG_P (lhs))
            set_hard_reg_bits (&data->dest, lhs);

          record_reg_life_pat (SET_SRC (pat), data);
        }
      return;

    case ASM_OPERANDS:
      for (j = ASM_OPERANDS_INPUT_LENGTH(pat) - 1; j >= 0; j--)
	record_reg_life_pat (ASM_OPERANDS_INPUT(pat, j), data);
      return;

    case USE:
      record_reg_life_pat (XEXP (pat, 0), data);
      return;

    case CLOBBER:
      if (STACK_REG_P (XEXP (pat, 0)))
        set_hard_reg_bits (&data->clobbered, XEXP (pat, 0));
      return;
    }

  fmt = GET_RTX_FORMAT (GET_CODE (pat));
  for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
    if (fmt[i] == 'E')
      for (j = XVECLEN (pat, i) - 1; j >= 0; j--)
        record_reg_life_pat (XVECEXP (pat, i, j), data);
    else if (fmt[i] == 'e')
      record_reg_life_pat (XEXP (pat, i), data);
}


/* Return number of extra stack-reg operands for the INSN (which must be
   CALL or RETURN). Extra operands are those not enclosed in the
   insn's pattern, but rather mentioned in one of the USE insns
   immediately preceding the INSN.  */

static int
n_extra_stack_reg_operands (insn)
    rtx insn;
{
  int n = 0;

  while ((insn = PREV_INSN (insn))
          && GET_CODE (insn) == INSN
          && GET_CODE (PATTERN (insn)) == USE)
    if (STACK_REG_P (XEXP (PATTERN (insn), 0)))
      n++;
  return(n);
}


/* Extension of n_extra_stack_reg_operands: scan extra operands,
   recording the encountered stack registers in `reg_set' and their
   modes in `mode'.  */

static int
record_extra_stack_reg_operands (insn, reg_set, mode)
    rtx insn;
    HARD_REG_SET *reg_set;
    enum machine_mode *mode;
{
  rtx body;
  int n = 0;

  if (reg_set)
    CLEAR_HARD_REG_SET (*reg_set);

  while ((insn = PREV_INSN (insn))
          && GET_CODE (insn) == INSN
          && GET_CODE (body = PATTERN (insn)) == USE)
    if (STACK_REG_P (XEXP (body,0)))
      {
	rtx reg = XEXP (body, 0);
        n++;
        if (reg_set)
          set_hard_reg_bits (reg_set, reg);
        if (mode)
	  mode[REGNO (reg)] = GET_MODE (reg);
      }
  return(n);
}

#ifdef CALL_INSN_FUNCTION_USAGE

/* Like record_extra_stack_reg_operands, but scans
   CALL_INSN_FUNCTION_USAGE list rather than preceding USE insns */

static int
record_fusage_stack_reg_operands (insn, reg_set, mode)
    rtx insn;
    HARD_REG_SET *reg_set;
    enum machine_mode *mode;
{
  rtx link;
  int n = 0;

  if (reg_set)
    CLEAR_HARD_REG_SET (*reg_set);

  for (link = CALL_INSN_FUNCTION_USAGE (insn); link; link = XEXP (link, 1))
    {
      rtx body = XEXP (link, 0);
      if (GET_CODE (body) == USE && STACK_REG_P (XEXP (body,0)))
	{
	  rtx reg = XEXP (body, 0);
	  n++;
	  if (reg_set)
	    set_hard_reg_bits (reg_set, reg);
	  if (mode)
	    mode[REGNO (reg)] = GET_MODE (reg);
	}
    }
  return(n);
}
#endif /* CALL_INSN_FUNCTION_USAGE */
    


/* Skip the sequence of USE insns immediately preceding INSN.
   This is used when we want to emit an insn before INSN
   to avoid disturbing the neighborhood of USEs and the INSN. */

static rtx
skip_uses (insn)
  rtx insn;
{
  rtx prev;
  while ((prev = PREV_INSN (insn))
          && GET_CODE (prev) == INSN
          && GET_CODE (PATTERN (prev)) == USE)
    insn = prev;
  return insn;
}

/* If PAT is a tablejump insn's pattern, find and return the table.
   Otherwise, return 0. */

static rtx
find_jump_table (pat)
  rtx pat;
{
  rtx label, table;

  if (GET_CODE (pat) == LABEL_REF)
    {
      label = XEXP (pat, 0);
      if (GET_CODE (label) != CODE_LABEL)
        abort ();
      table = NEXT_INSN (label);
      if (GET_CODE (table) == JUMP_INSN
          && (GET_CODE (PATTERN (table)) == ADDR_DIFF_VEC
              || GET_CODE (PATTERN (table)) == ADDR_VEC))
        return table;
    }
  else
    {
      int i, j;
      char *fmt = GET_RTX_FORMAT (GET_CODE (pat));

      for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= 0; i--)
        {
          if (fmt[i] == 'e')
            if (table = find_jump_table (XEXP (pat, i)))
              return table;
          if (fmt[i] == 'E')
            for (j = 0; j < XVECLEN (pat, i); j++)
              if (table = find_jump_table (XVECEXP (pat, i, j)))
                return table;
        }
    }
  return 0;
}



/* Scan INSN, which is in BLOCK, and record the life & death of stack
   registers in REGSTACK.  This function is called to process insns from
   the last insn in a block to the first.  The actual scanning is done in
   record_reg_life_pat.

   ??? the comment is outdated...
   If a register is live after a CALL_INSN, but is not a value return
   register for that CALL_INSN, then code is emitted to initialize that
   register.  The block_end[] data is kept accurate.

   Existing death and unset notes for stack registers are deleted
   before processing the insn. */

static void
record_reg_life (insn, regstack)
     rtx insn;
     stack regstack;
{
  /* Skip non-insns and CLOBBER insns: those do not affect register life.
     Skip USE insns, since the respective death note must not be attached
     here, but rather to the real insn (CALL_INSN) that follows.  Such USE
     will be taken care of when processing the CALL_INSN.  */
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i'
      || GET_CODE (PATTERN (insn)) == USE
      || GET_CODE (PATTERN (insn)) == CLOBBER)
    return;

  /* Strip old death notes for stack regs from this insn */
  {
    rtx note;
    rtx *note_link = &REG_NOTES(insn);
    for (note = *note_link; note; note = XEXP (note, 1))
      if (STACK_REG_P (XEXP (note, 0))
          && (REG_NOTE_KIND (note) == REG_DEAD
              || REG_NOTE_KIND (note) == REG_UNUSED))
        *note_link = XEXP (note, 1);
      else
        note_link = &XEXP (note, 1);
  }

  /* Process all patterns in the insn. */
  {
    struct reg_life_data data;
    int regno;
    int k;

    CLEAR_HARD_REG_SET (data.src);
    CLEAR_HARD_REG_SET (data.dest);
    CLEAR_HARD_REG_SET (data.clobbered);
    data.regstack = regstack;

    /* CALLs and RETURNs may have extra operands. Those count for SRC. */
    if (GET_CODE (insn) == CALL_INSN
        || GET_CODE (PATTERN (insn)) == RETURN)
      record_extra_stack_reg_operands (insn, &data.src, regstack->mode);

    /* GCC >2.6.0 describes extra operands in a special field in the
       insn, instead of attaching separate USE insns before CALL insn.
       Count them for SRC as well.  */
#ifdef CALL_INSN_FUNCTION_USAGE
    if (GET_CODE (insn) == CALL_INSN)
      record_fusage_stack_reg_operands (insn, &data.src, regstack->mode);
#endif /* CALL_INSN_FUNCTION_USAGE */

    if (GET_CODE (insn) == CALL_INSN)
      IOR_HARD_REG_SET (data.clobbered, call_used_reg_set);

    record_reg_life_pat (PATTERN (insn), &data);

    for (k = 0; k < STACK_REG_NSTACKS; ++k)
      for (regno = STACK_REG_FIRST(k); regno <= STACK_REG_LAST(k); ++regno)
        if (! TEST_HARD_REG_BIT (regstack->reg_set, regno))
          {
            if (TEST_HARD_REG_BIT (data.src, regno)
                && ! TEST_HARD_REG_BIT (data.dest, regno))
              REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_DEAD,
                  /* We want to indicate the death of a *single* hard
                     register, not a multi-register value. Therefore
                     we use QImode here, supposing HARD_REGNO_NREGS
                     (called from find_regno_note) will always return
                     1 for it. */
                                          hard_reg[regno][(int) QImode],
                                          REG_NOTES (insn));
            else if (TEST_HARD_REG_BIT (data.dest, regno))
              REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_UNUSED,
                                          hard_reg[regno][(int) QImode],
                                          REG_NOTES (insn));
            if (TEST_HARD_REG_BIT (data.clobbered, regno))
              REG_NOTES (insn) = gen_rtx (EXPR_LIST, REG_UNUSED,
                                          hard_reg[regno][(int) QImode],
                                          REG_NOTES (insn));
          }

    AND_COMPL_HARD_REG_SET (regstack->reg_set, data.dest);


    /* There might be a reg that is live after an insn that clobbers
       it in some way.  Emit zero-initializetion for such regs.
       Do not bother if no regs live across this insn, though.  */

    GO_IF_HARD_REG_EQUAL (regstack->reg_set, reg_class_contents[NO_REGS], m);

    {
      rtx next_insn = NEXT_INSN (insn);
      HARD_REG_SET init_set;

      ior_set_arg = &data.clobbered;
      note_popped_inputs (insn, ior_set);

      COPY_HARD_REG_SET (init_set, regstack->reg_set);
      AND_HARD_REG_SET (init_set, data.clobbered);
      emit_zero_init_before (&init_set, regstack, next_insn);

      /* If the INSN was the end of a block, move the
         block_end to point to the new insn. */

      if (insn == block_end[BLOCK_NUM (insn)])
        block_end[BLOCK_NUM (insn)] = PREV_INSN (next_insn);
    }

  m:
    ;
    IOR_HARD_REG_SET (regstack->reg_set, data.src);
  }
}

/* Find all basic blocks of the function, which starts with FIRST.
   For each JUMP_INSN, build the chain of LABEL_REFS on each CODE_LABEL. */

static void
find_blocks (first)
     rtx first;
{
  register rtx insn;
  register int block;
  register RTX_CODE prev_code = BARRIER;
  register RTX_CODE code;
  rtx label_value_list = 0;

  /* Record where all the blocks start and end.
     Record which basic blocks control can drop in to. */

  block = -1;
  for (insn = first; insn; insn = NEXT_INSN (insn))
    {
      /* Note that this loop must select the same block boundaries
	 as code in reg_to_stack, but that these are not the same
	 as those selected in flow.c.  */

      code = GET_CODE (insn);

      switch (code)
        {
          case JUMP_INSN:
            {
              /* Treat tablejump insn and the table following it as
                 a whole; don't allow the table to fall into
                 a separate block. */
              rtx insn2 = find_jump_table (PATTERN (insn));
              if (insn2)
                {
                  insn = insn2;
                  if (GET_CODE (PREV_INSN (insn)) != CODE_LABEL)
                    abort ();
                  /* delete old chain */
                  LABEL_REFS (PREV_INSN (insn)) = PREV_INSN (insn);
                }
            }
            /* FALL THROUGH */

          case INSN:
          case CALL_INSN:
            if (prev_code != JUMP_INSN && prev_code != BARRIER)
              {
                block_end[block] = insn;
                break;
              }
            /* FALL THROUGH */

          case CODE_LABEL:
            block_begin[++block] = insn;
            block_end[block] = insn;
            block_drops_in[block] = prev_code != BARRIER;
	    break;
        }

      if (GET_RTX_CLASS (code) == 'i')
	{
	  rtx note;

	  /* Make a list of all labels referred to other than by jumps.  */
	  for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	    if (REG_NOTE_KIND (note) == REG_LABEL)
	      label_value_list = gen_rtx (EXPR_LIST, VOIDmode, XEXP (note, 0),
					  label_value_list);
	}

      block_number[INSN_UID (insn)] = block;

      if (code != NOTE)
        prev_code = code;
    }

  if (block + 1 != blocks)
    abort ();

  /* generate all label references to the corresponding jump insn */
  for (block = 0; block < blocks; block++)
    {
      insn = block_end[block];

      if (GET_CODE (insn) == JUMP_INSN)
	{
	  rtx pat = PATTERN (insn);
	  int computed_jump = 0;
	  rtx x;

	  if (GET_CODE (pat) == PARALLEL)
	    {
	      int len = XVECLEN (pat, 0);
	      int has_use_labelref = 0;
	      int i;

	      for (i = len - 1; i >= 0; i--)
		if (GET_CODE (XVECEXP (pat, 0, i)) == USE
		    && GET_CODE (XEXP (XVECEXP (pat, 0, i), 0)) == LABEL_REF)
		  has_use_labelref = 1;

	      if (! has_use_labelref)
		for (i = len - 1; i >= 0; i--)
		  if (GET_CODE (XVECEXP (pat, 0, i)) == SET
		      && SET_DEST (XVECEXP (pat, 0, i)) == pc_rtx
		      && uses_reg_or_mem (SET_SRC (XVECEXP (pat, 0, i))))
		    computed_jump = 1;
	    }
	  else if (GET_CODE (pat) == SET
		   && SET_DEST (pat) == pc_rtx
		   && uses_reg_or_mem (SET_SRC (pat)))
	    computed_jump = 1;
		    
	  if (computed_jump)
	    {
	      for (x = label_value_list; x; x = XEXP (x, 1))
		record_label_references (insn,
					 gen_rtx (LABEL_REF, VOIDmode,
						  XEXP (x, 0)));

	      for (x = forced_labels; x; x = XEXP (x, 1))
		record_label_references (insn,
					 gen_rtx (LABEL_REF, VOIDmode,
						  XEXP (x, 0)));
	    }

	  record_label_references (insn, pat);
	}
    }
}

/* Return 1 if X contain a REG or MEM that is not in the constant pool.  */

static int
uses_reg_or_mem (x)
     rtx x;
{
  enum rtx_code code = GET_CODE (x);
  int i, j;
  char *fmt;

  if (code == REG
      || (code == MEM
	  && ! (GET_CODE (XEXP (x, 0)) == SYMBOL_REF
		&& CONSTANT_POOL_ADDRESS_P (XEXP (x, 0)))))
    return 1;

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e'
	  && uses_reg_or_mem (XEXP (x, i)))
	return 1;

      if (fmt[i] == 'E')
	for (j = 0; j < XVECLEN (x, i); j++)
	  if (uses_reg_or_mem (XVECEXP (x, i, j)))
	    return 1;
    }

  return 0;
}

/* If current function returns its result in a stack register,
   return the register rtx.  Otherwise return NULL_RTX.  */

static rtx
stack_result (decl)
     tree decl;
{
  rtx result = DECL_RTL (DECL_RESULT (decl));

  if (result == 0)
    return NULL_RTX;

  if (!(GET_CODE (result) == REG
        && REGNO (result) < FIRST_PSEUDO_REGISTER))
    {
#ifdef FUNCTION_OUTGOING_VALUE
      result
        = FUNCTION_OUTGOING_VALUE (TREE_TYPE (DECL_RESULT (decl)), decl);
#else
      result = FUNCTION_VALUE (TREE_TYPE (DECL_RESULT (decl)), decl);
#endif
    }

  return STACK_REG_P (result) ? result : NULL_RTX;
}


/* Determine which registers are live at the start of each basic
   block of the function whose first insn is FIRST.

   First, if the function returns a real_type, mark the function
   return type as live at each return point, as the RTL may not give any
   hint that the register is live.

   Then, start with the last block and work back to the first block.
   Similarly, work backwards within each block, insn by insn, recording
   which regs are dead and which are used (and therefore live) in the
   hard reg set of block_stack_in[].

   After processing each basic block, if there is a label at the start
   of the block, propagate the live registers to all jumps to this block.

   As a special case, if there are regs live in this block, that are
   not live in a block containing a jump to this label, and the block
   containing the jump has already been processed, we must propagate this
   block's entry register life back to the block containing the jump, and
   restart life analysis from there.

   In the worst case, this function may traverse the insns
   REG_STACK_SIZE times.  This is necessary, since a jump towards the end
   of the insns may not know that a reg is live at a target that is early
   in the insns.  So we back up and start over with the new reg live.

   If there are registers that are live at the start of the function,
   insns are emitted to initialize these registers.  Something similar is
   done after CALL_INSNs in record_reg_life. */

static void
stack_reg_life_analysis (first)
     rtx first;
{
  int block, i, k;
  struct stack_def reg_stack;
  rtx return_rtx = stack_result (current_function_decl);

  if (return_rtx)
    {
      /* Find all RETURN insns and mark them. */

      int value_regno = REGNO (return_rtx);
      int value_nregs = HARD_REGNO_NREGS (value_regno, GET_MODE (return_rtx));

      for (block = blocks - 1; block >= 0; block--)
        if (GET_CODE (block_end[block]) == JUMP_INSN
            && GET_CODE (PATTERN (block_end[block])) == RETURN)
          {
            /* See if this RETURN insn is preceded by (USE return_rtx).
               If not, it is a void return from a non-void function, i.e.
               the function returns a random value (with a warning).
               For STACK_REG_STRICT machines, it is necessary to assign
               some value to the return register; it's the best to do
               this right before the RETURN insn, or else we'll have
               to do this later, maybe in more than one control path.
               For non-strict machines, it's OK to return with empty
               reg-stack, so just don't mark return register live to
               avoid unecessary bother.  */
            int voidret = !n_extra_stack_reg_operands (block_end[block]);

#ifndef STACK_REG_STRICT
            if (!voidret)
#endif
            for (i = value_regno + value_nregs; --i >= value_regno; )
              {
                SET_HARD_REG_BIT (block_out_reg_set[block], i);
                block_out_reg_mode[block][i] = GET_MODE (return_rtx);
              }
#ifdef STACK_REG_STRICT
            if (voidret)
              emit_zero_init_before (&reg_stack.reg_set, reg_stack,
                                     block_end[block]);
#endif
          }

#ifdef FUNCTION_EPILOGUE
      /* Mark of the end of last block if we "fall off" the end of the
         function into the epilogue.
         The case of void return is hard to detect here, so do nothing
         with it but expect to deal with live regs at function start,
         after calls, etc.  */

      if (NEXT_INSN (block_end[blocks-1]) == 0
          || GET_CODE (NEXT_INSN (block_end[blocks-1])) != BARRIER)
        for (i = value_regno + value_nregs; --i >= value_regno; )
          {
            SET_HARD_REG_BIT (block_out_reg_set[blocks-1], i);
            block_out_reg_mode[blocks-1][i] = GET_MODE (return_rtx);
          }
#endif
    }

  /* now scan all blocks backward for stack register use */

  block = blocks - 1;
  while (block >= 0)
    {
      register rtx insn, prev;

      /* current register status at last instruction */

      COPY_HARD_REG_SET (reg_stack.reg_set, block_out_reg_set[block]);
      bcopy (block_out_reg_mode[block], reg_stack.mode, sizeof(reg_stack.mode));

      prev = block_end[block];
      do
        {
          insn = prev;
          prev = PREV_INSN (insn);

          /* Bother processing only if the insn can affect the
             stack-register's life.  Insns that mention stack-regs can
             affect the life, as are the insns that have a potential of
             clobbering some stack registers (eg CALL_INSN).
             ??? To be completely honest, we'd have to additionally consult
             INSN_CLOBBERS_REGNO_P here, but this macro is relatively
             costly on T800, and returns true only for insns that mention
             stack registers.  So it is omitted for now.  */

          if (GET_MODE (insn) == QImode || GET_CODE (insn) == CALL_INSN)
            record_reg_life (insn, &reg_stack);

        } while (insn != block_begin[block]);

      /* Set the state at the start of the block.  Mark that no
         register mapping information known yet. */

      COPY_HARD_REG_SET (block_stack_in[block].reg_set, reg_stack.reg_set);
      bcopy (reg_stack.mode, block_stack_in[block].mode, sizeof(reg_stack.mode));
      block_stack_in[block].top[0] = -2;

      /* If there is a label, propagate our register life to all jumps
         to this label. */

      if (GET_CODE (insn) == CODE_LABEL)
        {
          register rtx label;
          int must_restart = 0;

          for (label = LABEL_REFS (insn); label != insn;
               label = LABEL_NEXTREF (label))
            {
              int jump_block = BLOCK_NUM (CONTAINING_INSN (label));

              if (jump_block < block)
                ior_regset_and_mode (&block_out_reg_set[jump_block],
                                     block_out_reg_mode[jump_block],
                                     &block_stack_in[block]);
              else
                {
                  /* The block containing the jump has already been
                     processed.  If there are registers that were not known
                     to be live then, but are live now, we must back up
                     and restart life analysis from that point with the new
                     life information. */

                  GO_IF_HARD_REG_SUBSET (block_stack_in[block].reg_set,
                                         block_out_reg_set[jump_block],
                                         win);

                  ior_regset_and_mode (&block_out_reg_set[jump_block],
                                       block_out_reg_mode[jump_block],
                                       &block_stack_in[block]);

                  block = jump_block;
                  must_restart = 1;

                win:
                  ;
                }
            }
          if (must_restart)
            continue;
        }

      if (block_drops_in[block])
        ior_regset_and_mode (&block_out_reg_set[block-1],
                             block_out_reg_mode[block-1],
                             &block_stack_in[block]);
      block -= 1;
    }

  /* If any reg is live at the start of the first block of a
     function, then we must guarantee that the reg holds some value by
     generating our own "load" of that register.  Otherwise a 387 would
     fault trying to access an empty register. */

  block_begin[0] = emit_zero_init_before (&reg_stack.reg_set, &reg_stack,
                                          block_begin[0]);
}


/* For all the regs marked live in regstack, set the respective bit
   in set and set mode from regstack->mode.  */

static void
ior_regset_and_mode (set, mode, regstack)
     HARD_REG_SET *set;
     enum machine_mode *mode;
     stack regstack;
{
  int i,k;

  IOR_HARD_REG_SET (*set, regstack->reg_set);
  for (k = 0; k < STACK_REG_NSTACKS; k++)
    for (i = STACK_REG_FIRST (k); i <= STACK_REG_LAST (k); i++)
      {
        if (TEST_HARD_REG_BIT (regstack->reg_set, i))
          mode[i] = regstack->mode[i];
      }
}


/* Load zero into the regs in init_set.  This is called when we learn,
   during the life analysis, that some reg is live after an insn that
   clobbers it, or at the function start.

   (I never came across this on T800, but it seems reasonable to support.)
   
   The fact that a register appears live at the function start does
   not necessarily imply an error in the user program: it merely means
   that we could not determine that there wasn't such an error, just
   as -Wunused sometimes gives "incorrect" warnings.  In those cases,
   these initializations will do no harm.  */

static rtx
emit_zero_init_before (init_set, regstack, insn)
  HARD_REG_SET *init_set;
  stack regstack;
  rtx insn;
{
  int k, i;

  GO_IF_HARD_REG_EQUAL (*init_set, reg_class_contents[NO_REGS], end);
  
  /* Note that we are inserting virtual register references here:
     these insns must be processed by convert_regs later.  Also, these
     insns will not be in block_number, so BLOCK_NUM() will fail for them. */

  start_sequence ();
  for (k=0; k < STACK_REG_NSTACKS; k++)
    for (i = STACK_REG_FIRST (k); i <= STACK_REG_LAST (k); i++)
      if (TEST_HARD_REG_BIT (*init_set, i))
        {
          enum machine_mode mode = regstack->mode[i];
          rtx insn2;

          /* We may not initialize in a multi-register mode, for maybe
             not all parts of the multi-register are in init_set.  */
          if (HARD_REGNO_NREGS (i, mode) > 1)
            mode = GET_CLASS_NARROWEST_MODE (GET_MODE_CLASS (mode));
          if (HARD_REGNO_NREGS (i, mode) > 1)
            abort ();

          insn2 = emit_move_insn (hard_reg[i][(int) mode], CONST0_RTX (mode));
          PUT_MODE (insn2, QImode);
          CLEAR_HARD_REG_BIT (regstack->reg_set, i);
        }
  insn = emit_insn_before (gen_sequence (), insn);
  end_sequence ();
  
end:
  return insn;
}


/*****************************************************************************
   This section deals with stack register substitution, and forms the second
   pass over the RTL.
 *****************************************************************************/

/* Replace REG, which is a pointer to a stack reg RTX, with an RTX for
   the desired hard REGNO. */

static void
replace_reg (reg, regno)
     rtx *reg;
     int regno;
{
  if (! TEST_HARD_REG_BIT (reg_class_contents[STACK_REGS], regno)
      || ! STACK_REG_P (*reg))
    abort ();

  *reg = hard_reg[regno][(int) GET_MODE (*reg)];
}


/* Find the hard register number of virtual register REG in REGSTACK.
   The hard register number is relative to the top of the stack.  -1 is
   returned if the register is not currently on stack, ie is not live. */

static int
get_hard_regnum (regstack, reg)
     stack regstack;
     rtx reg;
{
  int i, k;

  if (! STACK_REG_P (reg))
    abort ();

  k = STACK_REG_STACKNO (REGNO (reg));
  for (i = regstack->top[k]; i <= STACK_REG_LAST(k); ++i)
    if (regstack->reg[i] == REGNO (reg))
      return (STACK_REG_FIRST(k) + (i - regstack->top[k]));

  return (-1);
}

int
stacks_equal (stack1, stack2)
     stack stack1;
     stack stack2;
{
  int k;
  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    {
      int r = stack1->top[k];
      if (r != stack2->top[k])
        return(0);
      for (  ; r <= STACK_REG_LAST(k); ++r)
        if (stack1->reg[r] != stack2->reg[r])
          return(0);
    }
  return(1);
}

/* Initialize stack order. REG_SET field should be set up before calling.  */

void
init_stack (bstack)
     stack bstack;
{
  int k,r;

  /* First, bring all stacks to empty state */
  /* Then, push live regs in default order */

  for (k=0; k < STACK_REG_NSTACKS; ++k)
    {
      bstack->top[k] = STACK_REG_LAST(k) + 1;
      for (r = STACK_REG_LAST (k); r >= STACK_REG_FIRST (k); --r)
        if (TEST_HARD_REG_BIT (bstack->reg_set, r))
          bstack->reg[--bstack->top[k]] = r;
    }
}

print_stack(s)
     stack s;
{
  int i,k;

  printf("%08x\n", s->reg_set);
  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    {
      printf("stack %d: top %02d: [", k, s->top[k]);
      for (i = s->top[k]; i <= STACK_REG_LAST(k); ++i)
        {
          printf("%02d ", s->reg[i]);
        }
      printf("]\n");
    }
}

/* Delete INSN from the RTL.  Mark the insn, but don't remove it from
   the chain of insns.  Doing so could confuse block_begin and block_end
   if this were the only insn in the block. */

static void
delete_insn_for_stacker (insn)
     rtx insn;
{
  PUT_CODE (insn, NOTE);
  NOTE_LINE_NUMBER (insn) = NOTE_INSN_DELETED;
  NOTE_SOURCE_FILE (insn) = 0;
}

/* See if the INSN is a redundant stack-reg move that should be
   deleted to avoid confusing output patterns.
   If it is, delete it and return 1. Otherwise, return 0.
   REGSTACK is the current stack, which is modified to reflect
   the effect of deleting insn. */

static int
del_stack_reg_move (insn, regstack)
     rtx insn;
     stack regstack;
{
  rtx pat = PATTERN (insn);
  rtx src = SET_SRC (pat);
  rtx dest = SET_DEST (pat);

  if (STACK_REG_P (src) && STACK_REG_P (dest)
      && STACK_REG_STACKNO (REGNO (src)) == STACK_REG_STACKNO (REGNO (dest)))
    {
      /* Write from one stack reg to another. */
      /* Our actions depend on the three conditions:
           (1) whether SRC dies in this insn
           (2) whether DEST is unused after this insn
           (3) whether SRC and DEST are the same reg. */

      int i;
      enum { SRC_DIES=1, DEST_UNUSED=2, SAME_REG=4 } cond;

      cond = 0;
      if (find_regno_note (insn, REG_DEAD, REGNO (src)))
        cond |= SRC_DIES;
      if (find_regno_note (insn, REG_UNUSED, REGNO (dest)))
        cond |= DEST_UNUSED;
      if (REGNO (src) == REGNO (dest))
        cond |= SAME_REG;

      /* The code below cannot handle multi-register moves.
         I hope never see them here */

      if (HARD_REGNO_NREGS (REGNO (dest), GET_MODE (dest)) != 1)
          abort ();

      switch (cond)
        {
          case 0:

            /* SRC and DST are distinct registers, both used later.
               This is not a regundant move, but true DUP insn */

            return 0;

          case SRC_DIES:

            /* SRC dies here, but DEST is not UNUSED. Just change the
               register mapping and delete the insn. */

            i = get_hard_regnum (regstack, src);
            if (i < 0)
              abort ();
            REG_BY_HARD_REGNO (regstack, i) = REGNO (dest);
            SET_HARD_REG_BIT (regstack->reg_set, REGNO (dest));
            CLEAR_HARD_REG_BIT (regstack->reg_set, REGNO (src));
            delete_insn_for_stacker (insn);
            return (1);

            /* Life anlysis never produces the former combination, but
               it may appear as a result of atom_force_single_reg().
               It's simpler to ignore this here than to bother removing
               the note there, so treat this as bare SAME_REG.  */

          case SAME_REG+SRC_DIES:
          case DEST_UNUSED:
          case SAME_REG:

            /* Move to unused reg or no-op move. These are to be
               simply deleted. */

            delete_insn_for_stacker (insn);
            return (1);

          case SAME_REG+DEST_UNUSED:
          case SRC_DIES+DEST_UNUSED:
          case SAME_REG+SRC_DIES+DEST_UNUSED:

            /* Both SRC and DEST are not used after this insn.
               Delete this move and pop SRC off the stack. */

            {
              HARD_REG_SET drop_set;
              CLEAR_HARD_REG_SET (drop_set);
              SET_HARD_REG_BIT (drop_set, REGNO (src));
              start_sequence ();
              STACK_REG_EMIT_DROPS (regstack, drop_set);
              emit_insn_before (gen_sequence (), insn);
              end_sequence ();
              delete_insn_for_stacker (insn);
              return (1);
            }

	  default:
            abort ();
        }
    }

  /* This move should not be deleted */
  return 0;
}


/* Undo the action of place_reg().  */
static void
displace_reg (regstack, place, reg)
  stack regstack;
  int place;
  rtx reg;
{
  int last = place + HARD_REGNO_NREGS (place, GET_MODE (reg)) - 1;
  int i;

  for (i = place; i <= last; ++i)
    regstack->reg[i] = -1;
}


/* Place a reg at specified PLACE in REGSTACK.
   Return 0 if the place is already occupied or is not large enough */
int
place_reg (regstack, place, reg)
  stack regstack;
  int place;
  rtx reg;
{
  int last = place + HARD_REGNO_NREGS (place, GET_MODE (reg)) - 1;
  int r = REGNO (reg);
  int i;

  if (last > STACK_REG_LAST (STACK_REG_STACKNO (place)))
    return 0;

  for (i = place; i <= last; ++i)
    {
      if (regstack->reg[i] != -1)
        {
          while ( --i >= place )
            regstack->reg[i] = -1;
          return 0;
        }
      regstack->reg[i] = r++;
    }

  return 1;
}


/* Reorder reg-stack if necessary, so as to place input operands
   where they are required to be by the constraints.
   It is made by exhaustive testing of all possible placements.
   This probably needs to be redone for machines with deeper stacks,
   as the worst case complexity is O(n!/(n-m)!), where m is the number
   of stack-reg operands and n is the stack depth.

   Recursive call is not the best but easiest way to implement
   the exhaustive testing. There was also a powerful temptation to make
   all operands_* stuff static, but this implies much corrections
   elsewhere. */

static int
place_inputs (i, oldstack, newstack, info)
    int i;
    stack newstack;
    stack oldstack;
    struct insn_info *info;
{
  int sz = info->in_last - &info->in[-1];
  for (  ; i < sz; ++i)
    {
      int k = STACK_REG_STACKNO (REGNO (info->in[i].reg));
      /* PLACE is an index in stack->reg[].
         REGNO is the hard register number of the operand.
         Conversions:
           regno = STACK_REG_FIRST(k) + place - stack->top[k];
           place = regno - STACK_REG_FIRST(k) + stack->top[k];
      */
      int oldregno = get_hard_regnum (oldstack, info->in[i].reg);
      int oldplace = oldregno - STACK_REG_FIRST(k) + oldstack->top[k];
      int place;

      /* If this reg is in a suitable place, and this place
         is still vacant in the new stack, try this variant first */

      if (TEST_HARD_REG_BIT (reg_class_contents[info->in[i].class], oldregno)
          && place_reg (newstack, oldplace, info->in[i].reg))
        {
          if (place_inputs (i+1, oldstack, newstack, info))
            return (1);
          displace_reg (newstack, oldplace, info->in[i].reg);
        }

      /* Try all vacant places in order */
      for (place = newstack->top[k]; place <= STACK_REG_LAST (k); ++place)
        if (place != oldplace
            && TEST_HARD_REG_BIT (reg_class_contents[info->in[i].class],
                            STACK_REG_FIRST(k) + place - newstack->top[k])
            && place_reg (newstack, place, info->in[i].reg))
          {
            if (place_inputs (i+1, oldstack, newstack, info))
              return (1);
            displace_reg (newstack, place, info->in[i].reg);
          }

      /* Cannot find a place for this reg. This means that the 
         constraints are contradictory. The caller should report
         an error */

      return (0);
    }

  /* No regs left -- the placement is done. The last thing
     to check is the rule #2: All implicitly popped input regs must be
     closer to the top of the reg-stack than any input that is
     not implicitly popped. If this is not satisfied, do not accept
     this placement */

  return check_rule_2 (newstack, info);
}


static int
place_regs (oldstack, newstack, info)
    stack newstack;
    stack oldstack;
    struct insn_info *info;
{
  int i,j,k;
  HARD_REG_SET placed_regs;

  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    for (i = STACK_REG_FIRST (k); i <= STACK_REG_LAST (k); ++i)
      newstack->reg[i] = -1;

  if (!place_inputs (0, oldstack, newstack, info))
    return 0;

  /* OK, newstack->reg contains the input regs in proper places.
     Now we are to place all other live regs, which need not be
     in particular places. */

  /* Set bits for placed input regs in placed_regs */

  CLEAR_HARD_REG_SET (placed_regs);
  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    for (i = STACK_REG_FIRST (k); i <= STACK_REG_LAST (k); ++i)
      if (newstack->reg[i] != -1)
        SET_HARD_REG_BIT (placed_regs, newstack->reg[i]);

  /* A reg can be simply left in its present place, if the place
     is not occupied by an already-placed reg. Do such regs... */

  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    for (i = oldstack->top[k]; i <= STACK_REG_LAST(k); ++i)
      if (newstack->reg[i] == -1
          && !TEST_HARD_REG_BIT (placed_regs, oldstack->reg[i]))
        {
          newstack->reg[i] = oldstack->reg[i];
          SET_HARD_REG_BIT (placed_regs, newstack->reg[i]);
        }

  /* Place all yet nonplaced regs in an arbitrary order */

  for (k = 0; k < STACK_REG_NSTACKS; ++k)
    {
      j = newstack->top[k];
      for (i = oldstack->top[k]; i <= STACK_REG_LAST(k); ++i)
        if (!TEST_HARD_REG_BIT (placed_regs, oldstack->reg[i]))
          {
            while (newstack->reg[j] != -1)
              {
                ++j;
                if (j > STACK_REG_LAST(k))
                  abort ();
              }
            newstack->reg[j] = oldstack->reg[i];
            SET_HARD_REG_BIT (placed_regs, newstack->reg[j]);
          }
    }

  /* All live regs must be in PLACED_REGS by now */

  GO_IF_HARD_REG_EQUAL (newstack->reg_set, placed_regs, win);
    abort ();
win:
  return 1;
}

void
set_hard_reg_bits (set, reg)
     HARD_REG_SET *set;
     rtx reg;
{
  int r = REGNO (reg);
  int nr = HARD_REGNO_NREGS (r, GET_MODE (reg));

  while (nr--)
    SET_HARD_REG_BIT (*set, r + nr);
}

void
set_hard_reg_bits_3 (set, r, mode)
     HARD_REG_SET *set;
     int r;
     enum machine_mode mode;
{
  int nr = HARD_REGNO_NREGS (r, mode);

  while (nr--)
    SET_HARD_REG_BIT (*set, r + nr);
}

void
clear_hard_reg_bits (set, reg)
     HARD_REG_SET *set;
     rtx reg;
{
  int r = REGNO (reg);
  int nr = HARD_REGNO_NREGS (r, GET_MODE (reg));

  while (nr--)
    CLEAR_HARD_REG_BIT (*set, r + nr);
}

/* Traverse the insn, changing virtual register numbers into hard reg
   numbers, which are the offsets from the appropriate reg-stack top.  */

#ifndef STACK_REGS_SUBSTITUTION_UNNECESSARY
static void
subst_regs_recursively (loc, regstack, busy_regs)
     rtx *loc;
     stack regstack;
     HARD_REG_SET *busy_regs;
{
  rtx x = *loc;
  int in_clobber = 0;

  if (!x) return;

  if (GET_CODE (x) == CLOBBER)
    {
      in_clobber = 1;
      loc = &XEXP (x,0);
      x = *loc;
    }

  if (STACK_REG_P (x))
    {
      int regnum = get_hard_regnum (regstack, x);
      
      if (regnum < 0)
        /* The reg is not live before this insn.  This can happen
           if the reg either is a pushed output or is clobbered
           in this insn.  */
        if (in_clobber)
          {
            /* It's OK for a CLOBBER to reference a reg that is not
               live. However, we must make sure it does not conflict
               with other operands of this insn.  Otherwise,
               constrain_operands called while final pass will fail.
               We will select a reg which is dead both before and
               after this insn. */
    
            int k = STACK_REG_STACKNO (REGNO (x));
    
            regnum = STACK_REG_FIRST (k);
            while (TEST_HARD_REG_BIT (*busy_regs, regnum))
              if (++regnum > STACK_REG_LAST (k))
                /* Cannot find a reg? There must be at least one... */
                abort ();
          }
        else
          {
            /* For pushed outputs, do nothing, since pushed outputs
               (probably) never appear explicitly in the written form
               of an insn and thus final.c will not use the register
               numbers anyway.  */
            set_hard_reg_bits (busy_regs, x);
            return;
          }

      replace_reg (loc, regnum);
      set_hard_reg_bits (busy_regs, *loc);
      return;
    }

  /* Traverse subexpressions in the natural order so as to process
     CLOBBERS in the end, after BUSY_REGS set is completely formed.  */
  {
    register char *fmt = GET_RTX_FORMAT (GET_CODE (x));
    register int i,j;

    /* In EXPR_LISTs, avoid anything but REG_{DEAD,UNUSED}:
       the things like REG_EQUIV may refer to other insns.  */
    if (GET_CODE (x) == EXPR_LIST
        && REG_NOTE_KIND (x) != REG_DEAD
        && REG_NOTE_KIND (x) != REG_UNUSED)
        fmt = "xe";

    for (i = 0; i < GET_RTX_LENGTH (GET_CODE (x)); i++)
      if (fmt[i] == 'E')
        for (j = 0; j < XVECLEN (x, i); j++)
          subst_regs_recursively (&XVECEXP (x, i, j), regstack, busy_regs);
      else if (fmt[i] == 'e')
        subst_regs_recursively (&XEXP (x, i), regstack, busy_regs);
  }
}
#endif /* STACK_REGS_SUBSTITUTION_UNNECESSARY */


void
print_insn_info(info)
    struct insn_info *info;
{
  struct insn_info_reg *p;
  static char *reg_class_names[] = REG_CLASS_NAMES;

  printf("<is_asm = %d\n", info->is_asm);

  for (p = info->in; p <= info->in_last; p++)
    {
      printf(" in[%d] : ", p - info->in);
      print_rtl(stdout, p->reg);
      printf(" flags %02x class %s\n", p->flags, reg_class_names[p->class]);
    }
  for (p = info->out; p <= info->out_last; p++)
    {
      printf(" out[%d] : ", p - info->out);
      print_rtl(stdout, p->reg);
      printf(" flags %02x class %s\n", p->flags, reg_class_names[p->class]);
    }
  printf(">\n");
}


static void
analyse_insn (insn, info)
    rtx insn;
    struct insn_info *info;
{
  int n_operands;
  int i;

#if 0  /* it is in recog.h -- redundant here. */
  /* insn_extract() interface */
  extern rtx recog_operand[];
  extern rtx *recog_operand_loc[];
#endif

  rtx *operands = recog_operand;
  char *const *constraints;


  info->in_last  = &info->in[-1];
  info->out_last = &info->out[-1];
  info->is_asm = ((n_operands = asm_noperands (PATTERN (insn))) >= 0);

  if (info->is_asm)
    {
      rtx body = PATTERN (insn);
      constraints = (char **) alloca (n_operands * sizeof (char *));
      decode_asm_operands (body, operands, recog_operand_loc,
                           (char **)constraints, NULL_PTR);
      /* We will decide which operand is input and which is output
         later in usual way (from constraints). expand_asm_operands()
         guarantees that '='s are set properly */
    }
  else
    {
      int insn_code = recog_memoized (insn);
      if (insn_code < 0)
        fatal_insn_not_found (insn);

      n_operands = insn_n_operands[insn_code];
      constraints = insn_operand_constraint[insn_code];
      insn_extract (insn);

#ifdef CALL_INSN_FUNCTION_USAGE
      if (GET_CODE (insn) == CALL_INSN)
	{
	  rtx link;
	  for (link = CALL_INSN_FUNCTION_USAGE (insn); link;
	       link = XEXP (link, 1))
	    {
	      rtx body = XEXP (link, 0);
	      if (GET_CODE (body) == USE && STACK_REG_P (XEXP (body,0)))
		{
		  rtx reg = XEXP (body, 0);
		  info->in_last++;
		  info->in_last->reg = reg;

		  /* Extra operands have no constraints; so consult
		     machine-dependent macros to find CLASS and FLAGS */

		  info->in_last->flags =
		    STACK_REG_EXTRA_OPERAND_FLAGS (insn, info->in_last->reg);
		  info->in_last->class =
		    STACK_REG_EXTRA_OPERAND_CLASS (insn, info->in_last->reg);
		}
	    }
	}
#endif /* CALL_INSN_FUNCTION_USAGE */

      if (GET_CODE (insn) == CALL_INSN
#ifdef FUNCTION_EPILOGUE
          || GET_CODE (PATTERN (insn)) == CONST_INT     /* nop */
#endif
          || GET_CODE (PATTERN (insn)) == RETURN)
        {
          /* Those insns may have extra operands ... */
          int n = n_extra_stack_reg_operands (insn);
          if (n > 0)
            {
              rtx prev = insn;
              while (n--)
                {
                  rtx reg = XEXP (PATTERN (prev = PREV_INSN (prev)), 0);

                  if (STACK_REG_P (reg))
                    {
                      info->in_last++;
                      info->in_last->reg = reg;
                      /* Extra operands have no constraints; so,
                         consult macros to find CLASS and FLAGS... */
                      info->in_last->flags = STACK_REG_EXTRA_OPERAND_FLAGS (insn, info->in_last->reg);
                      info->in_last->class = STACK_REG_EXTRA_OPERAND_CLASS (insn, info->in_last->reg);
                    }
                }
            }
        }
    }

  /* Most info is provided by constraints; extract it.  If no constraint
     alternative matches, that is a compiler bug: we should have caught
     such an insn during reload.  */
  if (analyse_constraints (insn, n_operands, operands, constraints, info) < 0)
    abort ();
}


/* Helper function for SORT_INSN_INFO_{IN,OUT} */

static int
sort_insn_info_cmp(r1, r2)
    struct insn_info_reg *r1, *r2;
{
  return STACK_REG_LOAD_ORDER (r1->class) - STACK_REG_LOAD_ORDER (r2->class);
}


/* Sort IN array in an INSN_INFO structure by HARD regno, ie in the   
   order the virtual regs should be pushed. */

static void
sort_insn_info_in(info)
    register struct insn_info *info;
{
  int sz = info->in_last - &info->in[-1];

  if (sz > 1)
    qsort (info->in, sz, sizeof (struct insn_info_reg), sort_insn_info_cmp);
}

/* Likewise for OUT */

static void
sort_insn_info_out(info)
    register struct insn_info *info;
{
  int sz = info->out_last - &info->out[-1];

  if (sz > 1)
    qsort (info->out, sz, sizeof (struct insn_info_reg), sort_insn_info_cmp);
}


/* Remove from REGSTACK any inputs that the asm implicitly popped.
   This can be called to process either POPPED or JUMP_POPPED inputs */

static void
pop_inputs (regstack, info, popped)
  stack regstack;
  struct insn_info *info;
  char popped;              /* either POPPED or JUMP_POPPED */
{
  int i;
  struct insn_info_reg *p;

  for (p = info->in; p <= info->in_last; p++)
    if ((p->flags & (popped|INPUT)) == (popped|INPUT))
      {
        /* p->reg might not be at the top of stack.  But that's OK,
           because all we need to do is pop the right number of regs
           off of the top of a reg-stack.  check_rules() guarantees
           that all implicitly popped regs are grouped at the top of
           the reg-stack. */

        int k = STACK_REG_STACKNO (REGNO (p->reg));
        int nr = HARD_REGNO_NREGS (REGNO (p->reg), GET_MODE (p->reg));
        while (nr--)
          {
            CLEAR_HARD_REG_BIT (regstack->reg_set,
                                regstack->reg[regstack->top[k]]);
            regstack->top[k]++;
          }
      }
}

/* Substitute stack hard reg numbers for stack virtual registers in
   INSN.  Non-stack register numbers are not changed.  REGSTACK is the
   current stack contents. Insns may be emitted as needed to arrange the
   register stack based on the contents of the insn. */

static void
subst_stack_regs (insn, regstack, jumpstack)
     rtx insn;
     stack regstack;
     stack jumpstack;
{
  register int i,k;

  rtx body = PATTERN (insn);
  rtx prev;

  struct stack_def stack_before;  /* reg-stack state just before insn */
  struct insn_info inf;


  if (GET_MODE (insn) != QImode)
    return;

  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    abort();

  if (GET_CODE (body) == USE)
    /* Do not substitute hard regs; this may be revisited during
       the scan for extra operands */
    return;

  if (GET_CODE (body) == CLOBBER)
	/* A special kind of insn that recog doesn't recognize; skip it too */
    return;

  /* See if this is a MOVE insn that can be deleted */

  if (GET_CODE (body) == SET && del_stack_reg_move (insn, regstack) > 0)
    return;

  analyse_insn (insn, &inf);

  /* Reorder stack so as the input regs take the places where
     the insn expects to see them */

  bcopy ((char *) regstack, &stack_before, sizeof (stack_before));

  if (! place_regs (&stack_before, regstack, &inf))
    {
      CHECK_ERROR (insn,
       "Wrong constraints; unable to arrange stack before insn", 0);
      return; 
    }

  /* Now that the regs are placed, we can check rules */
  if (check_rules (insn, regstack, &inf) < 0)
    return;

  /* emit insns before INSN to make sure the reg-stack is in the right
     order.  */

  change_stack (insn, &stack_before, regstack, emit_insn_before);

  /* Now remove from REGSTACK any inputs that the insn implicitly popped.
     Note that old state of REGSTACK is kept in TEMP_STACK, thus retaining
     the info necessary for hard reg numbers' substitution */

  if (GET_CODE (insn) == JUMP_INSN)
    {
      /* Jump insn on some machines pops the stack diffrently when the
         jump is taken and when it is not. Here we account for popping
         in the `jump taken' case. */
      bcopy (regstack, jumpstack, sizeof (*jumpstack));
      pop_inputs (jumpstack, &inf, JUMP_POPPED);
    }

  pop_inputs (regstack, &inf, POPPED);

  /* Add to REGSTACK any outputs that the insn implicitly pushed.
     We currently suppose jump insns to output identically both
     when jump is taken and when it is not. */

  {
    struct insn_info_reg *p;

    sort_insn_info_out(&inf);

    /* Regs with higher regno should be pushed first */
    for (p = inf.out_last; p >= inf.out; p--)
      {
        int vregno = REGNO (p->reg);
        int k = STACK_REG_STACKNO (vregno);
        int r = vregno + HARD_REGNO_NREGS (vregno, GET_MODE (p->reg));

        while (--r >= vregno)
          {
            regstack->reg[--regstack->top[k]] = r;
            SET_HARD_REG_BIT (regstack->reg_set, r);
            regstack->mode[r] = GET_MODE (p->reg);

            if (GET_CODE (insn) == JUMP_INSN)
              {
                jumpstack->reg[--jumpstack->top[k]] = r;
                SET_HARD_REG_BIT (jumpstack->reg_set, r);
                jumpstack->mode[r] = GET_MODE (p->reg);
              }
          }
      }
  }

  /* Find what regs are to be dropped due to being REG_UNUSED ouput
     or REG_DEAD input that the insn didn't implicitly pop.  If the insn
     didn't implicitly pop an input reg, that reg will still be live. */

  /* We will not do any dropping for JUMP_STACK here; this is in the
     competence of CONVERT_REGS. */

  {
    HARD_REG_SET drop_set;
    CLEAR_HARD_REG_SET (drop_set);
    {
      rtx note;
      for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
        {
          int r;

	  /* Ignore things like (expr_list:REG_UNUSED (scratch:SI) */
          if (GET_CODE (XEXP (note, 0)) != REG)
	    continue;

	  r = REGNO (XEXP (note, 0));
          if (TEST_HARD_REG_BIT (regstack->reg_set, r)
              && (REG_NOTE_KIND (note) == REG_UNUSED
                  || REG_NOTE_KIND (note) == REG_DEAD
                     && !TEST_HARD_REG_BIT (inf.out_regs, r)))
            set_hard_reg_bits (&drop_set, XEXP (note, 0));
        }
    }

    /* Emit insns to drop the regs that we've just found. The place to
      insert insns is after the INSN except when the insn is a TABLEJUMP, in
      which case the place is just after the jump table.
      ??? optimization: skip dropping if the tablejump is followed by a BARRIER  */

    {
      rtx drop_place = 0;       /* the insn to insert the drops after */

      if (GET_CODE (insn) != JUMP_INSN
          || !(drop_place = find_jump_table (PATTERN (insn))))
        drop_place = insn;

      start_sequence ();
      STACK_REG_EMIT_DROPS (regstack, drop_set);
      emit_insn_after (gen_sequence (), drop_place);
      end_sequence ();
    }
  }

#ifndef STACK_REGS_SUBSTITUTION_UNNECESSARY
  /* Now that we aren't going to use find_regno_note() anymore, we can
     finally substitute hard reg numbers. */

  /* ??? subst_regs_recursively this currently does wrong for output
     registers (it takes registrer numbers from stack_before, which is
     ok for input regs, but not for output.  Probably fixing it only
     requires to pass stack_after as an additional parameter, but on
     t800 substitution is not needed at all, so leave it to the future  */

  {
    HARD_REG_SET busy_regs;
    CLEAR_HARD_REG_SET (busy_regs);

    subst_regs_recursively (&PATTERN (insn), &stack_before, &busy_regs);
    subst_regs_recursively (&REG_NOTES (insn), &stack_before, &busy_regs);
  }
#endif /* STACK_REGS_SUBSTITUTION_UNNECESSARY */
}


/* Change the organization of the stack so that it fits a new basic
   block.  Some registers might have to be popped, but there can never be
   a register live in the new block that is not now live.

   Insert any needed insns before or after INSN.  WHEN is emit_insn_before
   or emit_insn_after. OLD is the original stack layout, and NEW is
   the desired form.  OLD is updated to reflect the code emitted, ie, it
   will be the same as NEW upon return.

   This function will not preserve block_end[].  But that information
   is no longer needed once this has executed. */

static void
change_stack (insn, old, new, when)
     rtx insn;
     stack old;
     stack new;
     rtx (*when)();
{
  int reg;
  HARD_REG_SET drop_set;

  start_sequence ();

  /* Pop any registers that are not needed in the new block. */

  COPY_HARD_REG_SET (drop_set, old->reg_set);
  AND_COMPL_HARD_REG_SET (drop_set, new->reg_set);

  STACK_REG_EMIT_DROPS (old, drop_set);

  if (new->top[0] < 0)
    {
      /* If the new block has never been processed, then it can inherit
         the old stack order. */

      bcopy (old, new, sizeof (*new));
    }
  else
    {
      /* This block has been entered before, and we must match the
         previously selected stack order. */

      STACK_REG_EMIT_SWAPS (old, new);

      /* At this point there must be no differences. */

      if (!stacks_equal (old,new))
        abort ();
    }

    if (when == emit_insn_before)
      insn = skip_uses (insn);
    (*when) (gen_sequence (), insn);
    end_sequence ();
}

/* Check PAT, which points to RTL in INSN, for a LABEL_REF.  If it is
   found, ensure that a jump from INSN to the code_label to which the
   label_ref points ends up with the same stack as that at the
   code_label.  Do this by inserting insns just before the code_label to
   pop and rotate the stack until it is in the correct order.  REGSTACK
   is the order of the register stack in INSN.

   Any code that is emitted here must not be later processed as part
   of any block, as it will already contain hard register numbers. */

static void
goto_block_pat (insn, regstack, pat)
     rtx insn;
     stack regstack;
     rtx pat;
{
  rtx label;
  rtx insn_before_label;
  rtx new_label;
  int label_block;
  stack label_stack;
  struct stack_def temp_stack;

  if (GET_CODE (pat) != LABEL_REF)
    {
      int i, j;
      char *fmt = GET_RTX_FORMAT (GET_CODE (pat));

      /* When dealing with ADDR_DIFF_VEC, skip XEXP(0), which is
         the label at the jump table start; it need not be processed
         since control never gets there. */
      int lim = (GET_CODE (pat) == ADDR_DIFF_VEC);

      for (i = GET_RTX_LENGTH (GET_CODE (pat)) - 1; i >= lim; i--)
        {
          if (fmt[i] == 'e')
            goto_block_pat (insn, regstack, XEXP (pat, i));
          if (fmt[i] == 'E')
            for (j = 0; j < XVECLEN (pat, i); j++)
              goto_block_pat (insn, regstack, XVECEXP (pat, i, j));
        }
      return;
    }

  label = XEXP (pat, 0);
  if (GET_CODE (label) != CODE_LABEL)
    abort ();

  if (INSN_UID (label) <= 0)
    return;

  insn_before_label = PREV_INSN (label);
  label_block = BLOCK_NUM (label);
  label_stack = &block_stack_in[label_block];

  /* Emit the agreeing insn sequence.  Note that CHANGE_STACK will
     initialize LABEL_STACK if it is not yet initialized. */

  bcopy (regstack, &temp_stack, sizeof (temp_stack));
  change_stack (label, &temp_stack, label_stack, emit_insn_before);

  /* If no insns were emitted, i.e. the stacks are identical, we are done. */

  if (NEXT_INSN (insn_before_label) == label)
    return;

  /* Emit a label for the new code, and point the original insn at
     this new label. We can't use redirect_jump here, because we're using
     fld[4] of the code labels as LABEL_REF chains, no NUSES counters. */

  new_label = gen_label_rtx ();
  emit_label_after (new_label, insn_before_label);
  LABEL_REFS (new_label) = new_label;

  /* The old label_ref will no longer point to the code_label it now uses,
     so strip the label_ref from the code_label's chain of references. */
  {
    rtx *ref;
    for (ref = &LABEL_REFS (label); *ref != pat; ref = &LABEL_NEXTREF (*ref))
      if (*ref == label)
        abort ();
    *ref = LABEL_NEXTREF (*ref);
  }

  XEXP (pat, 0) = new_label;
  record_label_references (insn, PATTERN (insn));

  if (JUMP_LABEL (insn) == label)
    JUMP_LABEL (insn) = new_label;

  /* If this block is entered at top, we need to insert a jump to go
     round the agreeing sequence just emitted.  */
  {
    int this_block_drops_in = block_drops_in[label_block];

    /* Indicate that now control drops into the block from the
       agreeing sequence we have just emitted.  */
    block_drops_in[label_block] |= 2;
    if (!this_block_drops_in)
      return;
  }

#ifdef JUMP_CLOBBERS_REGS
  /* On some machines, we cannot use jump to go around the agreeing
     sequence (e.g. on T800 the jump insn changes reg-stack).  In
     this case, an alternative technique is used: emit the sequence
     of insns which makes stack the same as it was just after jump to
     this block, in order to neutralize the effect of agreeing
     sequence we're going to emit. */
  {
    HARD_REG_SET push_set;

    bcopy (label_stack, &temp_stack, sizeof (temp_stack));

    /* REGSTACK may have some regs live which are not in LABEL_STACK.
       Initialize those to zero. */
    COPY_HARD_REG_SET (push_set, regstack->reg_set);
    AND_COMPL_HARD_REG_SET (push_set, temp_stack.reg_set);
    start_sequence ();
    STACK_REG_EMIT_PUSHES (&temp_stack, push_set);
    emit_insn_after (gen_sequence (), insn_before_label);
    end_sequence ();
    
    change_stack (new_label, &temp_stack, regstack, emit_insn_before);
  }
#else
  {
    rtx new_jump = emit_jump_insn_after (gen_jump (label), insn_before_label);
    record_label_references (new_jump, PATTERN (new_jump));
    JUMP_LABEL (new_jump) = label;
    emit_barrier_after (new_jump);
  }
#endif

}


/* Perform stack register substitution for insns between INSN and
   LAST_INSN.  Return LAST_INSN  */

static rtx
subst_stack_regs_from_to (insn, last_insn, regstack, jumpstack)
    rtx insn;
    rtx last_insn;
    stack regstack;
    stack jumpstack;
{
  rtx next_insn = insn;
  do
    {
      /* Remember the NEXT_INSN, because subst_stack_regs emits new
	 insns after INSN */
      insn = next_insn;
      next_insn = NEXT_INSN (insn);
      subst_stack_regs (insn, regstack, jumpstack);
    }
  while (insn != last_insn);

  return last_insn;
}


/* Do stack register substitution within a *group* of insns. The 
   purpose of this function is to try reg-stack loading optimization
   prior to actual stack reg substitution. Example (T800):
   the expression
        x - 5
   might get compiled into
        ldc 5; ldl x; sub;
   which would be transformed while reg substitution into
        ldc 5; ldl x; rev; sub;
   but is optimized by SUBST_STACK_REGS_IN_GROUP to
        ldl x; ldc 5; sub;

   Insns like ldl,ldc are recognized to be `load atoms'. A load
   atom makes no side effect and takes no stack register input; its
   sole action is to load a value at the top of a reg-stack.

   Insn group is considered to consist of zero or more load atoms
   followed by a regular insn.

   This function takes a group starting from INSN1 and spanning at
   most to END_INSN, does register substitution in the group, and
   returns the last insn processed.  REGSTACK on entry reflect the
   stack state before INSN1.  Upon return REGSTACK shows the stack
   state at the last insn in the group processed, and if that insn is
   a JUMP_INSN, JUMPSTACK shows the stack state at the label this insn
   jumps to if jump is taken.  */

static rtx
subst_stack_regs_in_group (insn1, end_insn, regstack, jumpstack)
    rtx insn1, end_insn;
    stack regstack;
    stack jumpstack;
{
  rtx insn;
  rtx general_insn = 0;
  rtx insn0 = PREV_INSN (insn1);
  rtx insert_before;

  struct atom {
    rtx start;          /* first insn of the atom */
    rtx end;            /* last insn of the atom */
    rtx reg;            /* the reg this atom loads */
    struct atom *next;
  } *atoms = 0;

  /* Find the general insn that ends the group, collecting the load
     atoms meanwhile */

  insn = insn1;
  do
    {
      rtx atom_end;

      /* Allow atoms to be interspersed with NOTEs and CLOBBERs.  It
	 seems that it will not harm if we reorder the CLOBBERs
	 occasionally--unlike USEs which must be kept close to the
	 insn they precede, as they may supply extra operands to the
	 insn. */

      if (GET_CODE (insn) == NOTE)
	continue;
      if (GET_CODE (insn) == INSN && GET_CODE (PATTERN (insn)) == CLOBBER)
	continue;

      atom_end = recognize_load_atom (insn, end_insn);
      if (atom_end)
        {
          struct atom *new_atom = (struct atom *) alloca (sizeof (struct atom));

          new_atom->start = insn;
          new_atom->end = atom_end;
          new_atom->reg = SET_DEST (PATTERN (atom_end));
          if (GET_CODE (new_atom->reg) != REG) abort();
          new_atom->next = atoms;
          atoms = new_atom;
          if (new_atom->start != new_atom->end)
            atom_force_single_reg (new_atom->start, new_atom->end, new_atom->reg);
          insn = atom_end;
        }
      else
	{
	  /* INSN is not an atom. */

	  insert_before = insn;

	  /* Skep USEs which may precede the real general insn */

	  do
	    {
	      if (GET_CODE (insn) == INSN
		  && GET_CODE (PATTERN (insn)) == USE)
		continue;
	      general_insn = insn;
	      break;
	    }
	  while (insn != end_insn && (insn = NEXT_INSN (insn)));

	  break;
        }
    }
  while (insn != end_insn && (insn = NEXT_INSN (insn)));

  /* If we hit end_insn before finding the general insn, or the
     general insn we found is not a real insn, or if is doesn't
     mention stack registers, it imposes no stack layout requirements.
     Then we have no reason to reorder preceding atoms; just process
     this group as it is.  */

  if (! general_insn
      || GET_RTX_CLASS (GET_CODE (general_insn)) != 'i'
      || GET_MODE (general_insn) != QImode
      || GET_CODE (PATTERN (general_insn)) == USE)
    return subst_stack_regs_from_to (insn1, insn, regstack, jumpstack);

  /* For now, do the simplest transformation possible: reorder the
     load atoms so as to save some stack shuffling afterwards. */
  {
    struct insn_info inf;
    struct insn_info_reg *p;
    struct atom *a;

    analyse_insn (general_insn, &inf);
    sort_insn_info_in(&inf);

    for (p = inf.in; p <= inf.in_last; p++)
      for (a = atoms; a; a = a->next)
        if (REGNO (a->reg) == REGNO (p->reg))
          {
            rtx insert_after = PREV_INSN (insert_before);
            if (insert_after != a->end)
              reorder_insns (a->start, a->end, insert_after);
            insert_before = a->start;
            break;
          }
  }

  /* INSN1 might go invalid because of the reordering; use INSN0 instead */
  return subst_stack_regs_from_to (NEXT_INSN (insn0), general_insn,
                                   regstack, jumpstack);
}


/* Tell if the INSN is a start of `load atom', which is an insn or insn
   sequence with a sole effect of loading a value onto a reg stack. If
   it is, the last insn in the atom is returned.  */

static rtx
recognize_load_atom (insn, end_insn)
    rtx insn;
    rtx end_insn;
{
  rtx body = PATTERN (insn);
  rtx last_atom_insn;
  rtx loaded_reg;

  if (GET_CODE (insn) != INSN)
    return 0;

  /* See if the insn has no effect except loading a value onto a
     reg-stack. CLOBBERs are not allowed. */

  if (GET_CODE (body) == SET)
    {
      rtx src = SET_SRC (body);
      rtx dest = SET_DEST (body);

      /* Destination must be a plain stack reg.  Source may be
         anything not mentioning stack regs.  */
      if (! STACK_REG_P (dest)
          || stack_reg_mentioned (src))
        return 0;
    }
  else
    return 0;

  /* Yes, INSN is valid for atom start.  Now look if the insn that
     follows has no effect except taking the value loaded by INSN off
     the stack and loading another value instead; eg, the first insn
     loads a memory address and the second replaces it with the
     contents of the memory cell at this address.  */

  last_atom_insn = insn;
  while (insn != end_insn)
    {
      insn = NEXT_INSN (insn);
      body = PATTERN (insn);

      /* Allow NOTEs inside atoms: eg, NOTE_INSN_DELETED */

      if (GET_CODE (insn) == NOTE)
	continue;

      /* Even CALL_INSN cannot belong to a load atom, since it may clobber
	 certain regs. JUMPs, LABELs and BARRIERs are out too.  */

      if (GET_CODE (insn) != INSN)
        break;

      /* Ignore CLOBBERs that precede no-conflict blocks  */

      if (GET_CODE (body) == CLOBBER)
	continue;

      if (GET_CODE (body) == SET)
        {
          rtx src = SET_SRC (body);
          rtx dest = SET_DEST (body);
	  loaded_reg = SET_DEST (PATTERN (last_atom_insn));

          /* Destination must be a stack reg of the same reg-stack as
             the reg loaded by INSN.  Source may be anything
             mentioning the only stack reg, namely the one loaded by
             INSN.  This reg must either be set or die here.  */

          if (! STACK_REG_P (dest)
              || STACK_REG_STACKNO (REGNO (dest))
                 != STACK_REG_STACKNO (REGNO (loaded_reg))
              || HARD_REGNO_NREGS (REGNO (loaded_reg), GET_MODE (loaded_reg))
                 > HARD_REGNO_NREGS (REGNO (dest), GET_MODE (dest))
              /* This check unnecessarily misses if dest is wider than
                 loaded_reg and actually covers it, but has a different
                 regno.  But that probably does not worth the bother... */
              || !(REGNO (loaded_reg) == REGNO (dest)
                   || find_regno_note (insn, REG_DEAD, REGNO (loaded_reg)))
                 /* I think the presense of the death note makes the
                    check for presense of the reg in SRC redundant; so
                    check only that no other stack reg is mentioned.  */
              || stack_reg_but_one_mentioned (src, loaded_reg)
                 /* But do the check if we haven't seen the death note.  */
              || (REGNO (loaded_reg) == REGNO (dest)
                  && !stack_reg_mentioned (src)))
            break;
        }
      else
        break;

      /* INSN2 proved to be a valid continuation for the atom. */
      last_atom_insn = insn;
    }

  /* Now INSN is the last insn in the atom found.  */
  return last_atom_insn;
}


/* This procedure fixes up multi-insn atoms to make them use the
   only stack register, namely the one which is the value of the atom.
   Otherwise, we may abort during processing reordered atoms because of
   the atom's internal use of a stack reg which has already been
   loaded by another atom reinserted ahead of this one.
   Example:
        ldl 0; ldnl 0
   Suppose `ldl' sets the virtual register Areg and `ldnl' sets the
   virtual register Breg. This procedure will change Areg in both
   SET_DEST of `ldl' and memory address of `ldnl' to Areg, so that
   the atom will use the only stack register Breg.

   Note that atom_force_single_reg and atom_force_single_reg_1 take
   shortcuts assuming the insn they operate upon are valid atoms */

static void
atom_force_single_reg (start, end, reg)
    rtx start, end, reg;
{
  rtx insn = PREV_INSN (start);
  do {
    insn = NEXT_INSN (insn);
    if (GET_RTX_CLASS (GET_CODE (insn)) == 'i')
      {
	rtx note;

	/* REG_UNUSED notes require special handling when only a part
	   of the loaded value is UNUSED.  */

	for (note = REG_NOTES (insn); note; note = XEXP (note, 1))
	  switch (REG_NOTE_KIND (note))
	    {
	    case REG_UNUSED:
	      {
		rtx *loc = &XEXP (note, 0);
		rtx dest = SET_DEST (PATTERN (insn));

		if (STACK_REG_P (*loc))
		  {
		    /* We can assume the notes are for one-word-wide
		       registers -- we know how record_reg_life works.  */
		    int subword = REGNO (*loc) - REGNO (dest);
		    *loc = hard_reg[REGNO (reg) + subword][GET_MODE (*loc)];
		  }
	      }
	      break;

	    case REG_DEAD:
	      {
		rtx *loc = &XEXP (note, 0);
		if (STACK_REG_P (*loc))
		  *loc = hard_reg[REGNO (reg)][GET_MODE (*loc)];
	      }
	      break;
	    }

	/* Change all mentions of stack registers within the PATTERN */

        atom_force_single_reg_1 (&PATTERN (insn), reg);
      }
  } while (insn != end);
}

static void
atom_force_single_reg_1 (loc, reg)
    rtx *loc, reg;
{
  if (!*loc)
    return;

  if (STACK_REG_P (*loc))
    *loc = hard_reg[REGNO (reg)][GET_MODE (*loc)];
  else
    {
      register char *fmt = GET_RTX_FORMAT (GET_CODE (*loc));
      register int i,j;

      for (i = GET_RTX_LENGTH (GET_CODE (*loc)) - 1; i >= 0; i--)
        if (fmt[i] == 'E')
          for (j = XVECLEN (*loc, i) - 1; j >= 0; j--)
            atom_force_single_reg_1 (&XVECEXP (*loc, i, j), reg);
        else if (fmt[i] == 'e')
          atom_force_single_reg_1 (&XEXP (*loc, i), reg);
    }
}


/* Traverse all basic blocks in a function, converting the register
   references in each insn from the "flat" register file that gcc uses, to
   the stack-like registers the 387 uses. */

static void
convert_regs ()
{
  register int block, reg;
  register rtx insn, next;
  struct stack_def reg_stack;
  struct stack_def jump_stack;

  for (block = 0; block < blocks; block++)
    {
      stack bstack = &block_stack_in[block];

      /* If this block has not been previously encountered, choose a
         default mapping for any stack regs live on entry */

      if (bstack->top[0] < 0)
        init_stack (bstack);

      /* Process all insns in this block.  Keep track of `next' here,
         so that we don't process any insns emitted while making
         substitutions in INSN. */

      bcopy (&block_stack_in[block], &reg_stack, sizeof (reg_stack));
      insn = block_begin[block];
      do
        {
          if (GET_MODE (insn) == QImode)
            insn = subst_stack_regs_in_group (insn, block_end[block],
					      &reg_stack, &jump_stack);
        }
      while (insn != block_end[block] && (insn = NEXT_INSN (insn)));

      /* Something failed if the stack life doesn't match. */

      GO_IF_HARD_REG_EQUAL (reg_stack.reg_set, block_out_reg_set[block], win);
        abort ();

    win:
      /* If the block ends with a JUMP_INSN, call GOTO_BLOCK_PAT to
         make the stack at the jump target block agree with JUMP_STACK. */

      if (GET_CODE (block_end[block]) == JUMP_INSN)
        goto_block_pat (block_end[block], &jump_stack,
                        PATTERN (block_end[block]));

      /* Likewise handle the case where we fall into the next block. */

      if (block < blocks - 1 && block_drops_in[block+1] & 1)
        change_stack (insn, &reg_stack, &block_stack_in[block+1],
                      emit_insn_after);
    }

#ifdef FUNCTION_EPILOGUE
  /* There is a couple of things to worry about if the last block
     "falls off into epilogue."
     
     Beware! This code is untested.  */

  if (insn == 0 || GET_CODE (insn) != BARRIER)
    {
      rtx return_rtx = stack_result (current_function_decl);
  
#if STACK_REG_STRICT
      /* If the last basic block is the end of a loop, and that loop has
         regs live at its start, then the last basic block will have
         regs live at its end that need to be popped before the
         function returns.  */

      HARD_REG_SET drop_set;
  
      COPY_HARD_REG_SET (drop_set, regstack->reg_set);
      if (return_rtx)
        clear_hard_reg_bits (&drop_set, return_rtx)
      start_sequence ();
      STACK_REG_EMIT_DROPS (regstack, drop_set);
      emit_insn_after (gen_sequence (), insn);
      end_sequence ();
#endif
  
      /* In case of a multi-register return value, we may need to reorder
         regstack for registers to appear in the right order on return.
         We do this by making a dummy insn sequence that can be processed
         by the code that knows how to reorder regstack before a RETURN
         insn.  */
      if (return_rtx)
        {
          insn = emit_insn_after (gen_rtx (USE, VOIDmode, return_rtx), insn);
          insn = emit_insn_after (gen_nop (), insn);
          PUT_MODE (insn, QImode);
          subst_stack_regs (insn, &reg_stack, &jump_stack);

          /* OK, now the stack shuffling sequence (if any) is emitted
             before the nop.  No more need for the nop... */
          delete_insn_for_stacker (insn);
        }
    }
#endif
}

/* Check expression PAT, which is in INSN, for label references.  if
   one is found, print the block number of destination to FILE. */

static void
print_blocks (file, insn, pat)
     FILE *file;
     rtx insn, pat;
{
  register RTX_CODE code = GET_CODE (pat);
  register int i;
  register char *fmt;

  if (code == LABEL_REF)
    {
      register rtx label = XEXP (pat, 0);

      if (GET_CODE (label) != CODE_LABEL)
        abort ();

      fprintf (file, " %d", BLOCK_NUM (label));

      return;
    }

  fmt = GET_RTX_FORMAT (code);
  for (i = GET_RTX_LENGTH (code) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'e')
        print_blocks (file, insn, XEXP (pat, i));
      if (fmt[i] == 'E')
        {
          register int j;
          for (j = 0; j < XVECLEN (pat, i); j++)
            print_blocks (file, insn, XVECEXP (pat, i, j));
        }
    }
}

/* Write information about stack registers and stack blocks into FILE.
   This is part of making a debugging dump.  */
static void
dump_stack_info (file)
     FILE *file;
{
  register int block;

  fprintf (file, "\n%d stack blocks.\n", blocks);
  for (block = 0; block < blocks; block++)
    {
      register rtx head, jump, end;
      register int regno;

      fprintf (file, "\nStack block %d: first insn %d, last %d.\n",
               block, INSN_UID (block_begin[block]),
               INSN_UID (block_end[block]));

      head = block_begin[block];

      fprintf (file, "Reached from blocks: ");
      if (GET_CODE (head) == CODE_LABEL)
        for (jump = LABEL_REFS (head);
             jump != head;
             jump = LABEL_NEXTREF (jump))
          {
            register int from_block = BLOCK_NUM (CONTAINING_INSN (jump));
            fprintf (file, " %d", from_block);
          }
      if (block_drops_in[block])
        fprintf (file, " previous");

      fprintf (file, "\nlive stack registers on block entry: ");
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
        {
          if (TEST_HARD_REG_BIT (block_stack_in[block].reg_set, regno))
            fprintf (file, "%d ", regno);
        }

      fprintf (file, "\nlive stack registers on block exit: ");
      for (regno = 0; regno < FIRST_PSEUDO_REGISTER; regno++)
        {
          if (TEST_HARD_REG_BIT (block_out_reg_set[block], regno))
            fprintf (file, "%d ", regno);
        }

      end = block_end[block];

      fprintf (file, "\nJumps to blocks: ");
      if (GET_CODE (end) == JUMP_INSN)
        print_blocks (file, end, PATTERN (end));

      if (block + 1 < blocks && block_drops_in[block+1])
        fprintf (file, " next");
      else if (block + 1 == blocks
               || (GET_CODE (end) == JUMP_INSN
                   && GET_CODE (PATTERN (end)) == RETURN))
        fprintf (file, " return");

      fprintf (file, "\n");
    }
}

/* Do alter_subreg on all the SUBREGs contained in X.  Like
   walk_alter_subreg in final.c, but traverses an arbitrary rtx.  */

static rtx
walk_alter_subreg (x)
     rtx x;
{
  register char *fmt;
  register int i;

  if (GET_CODE (x) == SUBREG)
    return alter_subreg (x);

  fmt = GET_RTX_FORMAT (GET_CODE (x));
  for (i = GET_RTX_LENGTH (GET_CODE (x)) - 1; i >= 0; i--)
    {
      if (fmt[i] == 'E')
        {
          register int j;

          for (j = XVECLEN (x, i) - 1; j >= 0; j--)
            XVECEXP (x, i, j) = walk_alter_subreg (XVECEXP (x, i, j));
        }
      else if (fmt[i] == 'e')
        XEXP (x, i) = walk_alter_subreg (XEXP (x, i));
    }

  return x;
}


/* Call FUN for every reg which is a popped input for INSN.
   The value returned by NOTE_POPPED_INPUTS is the first non-zero
   value returned from the call to FUN, or 0 if all FUN invocations
   returned zero. */

int
note_popped_inputs (insn, fun)
     register rtx insn;
     int (*fun) ();
{
  int popped_inputs;
  int i;
  int result;
  int n_operands;
  char *const *constraints;
  
  if (GET_RTX_CLASS (GET_CODE (insn)) != 'i')
    return 0;

  switch (GET_CODE (PATTERN (insn)))
    {
    case USE:
    case CLOBBER:
    case ASM_INPUT:
    case UNSPEC:
    case UNSPEC_VOLATILE:
    case ADDR_VEC:
    case ADDR_DIFF_VEC:
      /* These won't recognize -- but they cannot pop anything anyway. */
      return 0;
    }

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

      popped_inputs = 0;
      
      i = n_operands;
      while (--i >= 0)
	{
	  if (index (constraints[i], 'P'))
	    popped_inputs |= 1<<i;
	}
      
      if (!popped_inputs)
	return 0;
    }
  else
    {
      /* Ordinary insn */

      int insn_code = recog_memoized (insn);
      if (insn_code < 0)
        fatal_insn_not_found (insn);

      popped_inputs = get_attr_popped_inputs (insn);

#ifdef HAVE_ATTR_popped_inputs_on_jump
      popped_inputs |= get_attr_popped_inputs_on_jump (insn);
#endif
      
      if (!popped_inputs)
	return 0;

      insn_extract (insn);
      n_operands = insn_n_operands [INSN_CODE (insn)];
    }

  i = n_operands;
  while (--i >= 0)
    if (popped_inputs & (1<<i))
      {
        rtx pat = recog_operand[i];

        if (GET_CODE (pat) != REG && GET_CODE (pat) != SUBREG)
          pat = stack_reg_mentioned (pat);

        if (pat && (GET_CODE (pat) == REG || GET_CODE (pat) == SUBREG))
          if (result = (*fun)(insn, pat))
            return result;  /* FUN says stop scanning this insn */
      }

  return 0;
}

#endif /* STACK_REGS */

