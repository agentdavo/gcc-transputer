/* Declarations for reg-stack pass necessary for the implementation
   of STACK_REG_EMIT_{DROPS,SWAPS}();
   Copyright (C) 1992 Free Software Foundation, Inc.

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


#ifndef NULL_PTR
#define NULL_PTR 0
#endif


#define FIRST_NONSTACK_REGISTER     (STACK_REG_LAST (STACK_REG_NSTACKS-1))

/* This structure describes the state of all reg-stacks. TOP[k] is an
   index into REG[] such that REG[TOP[k]] is the top of k-th stack.
   TOP[k] > STACK_REG_LAST(k) means the stack is empty.

                             +0  +1  +2             <<< hard regnos
    +---+---+---+---+---+---+---+---+---+---+---
      ^STACK_REG_FIRST(k)     ^TOP[k] ^STACK_REG_LAST(k)

   If TOP[0] is less than 0, the stack is not yet initialized: reg_set
   indicates which registers are live.  Stack initialization consists of
   placing each live reg in array REG and setting TOP appropriately.

   Size of REG is larger than actually needed. Non-stack-reg entries
   are unused, but simplify the life slightly.
   
   The array MODE, indexed by a virtual regno, shows the mode of the
   value contained in the virtual reg.  Entries for non-live registers
   contain a garbage.  This information needs not to be very accurate,
   since it is used only to find out the proper mode when emitting
   stack shuffling insns.  It is necessary because on T800 there are
   distinct instructions for loading/storing SF and DF values; the
   manual says the result of the insn is undefined if the insn does
   not match the value currently held in the register which is
   loaded/stored.
   
   `Inaccurate' means that each register composing, say, DImode value
   gets marked with DImode, although the proper value would be SImode.  */

typedef
struct stack_def
{
  HARD_REG_SET reg_set;             /* set of live virtual registers */
  int top[STACK_REG_NSTACKS];
  int reg[FIRST_PSEUDO_REGISTER];
  enum machine_mode mode[FIRST_PSEUDO_REGISTER];
} *stack;

/* Given a hard (not virtual) regno, provide access to corresponding
   entry of the REG array in above structure.  */
#define REG_BY_HARD_REGNO(stack, regno) \
  (stack)->reg[(stack)->top[STACK_REG_STACKNO (regno)]          \
               + (regno)                                        \
               - STACK_REG_FIRST (STACK_REG_STACKNO (regno))]

/* Get the depth of the given stack.  */
#define STACK_DEPTH(STACK, STACKNO) \
  (STACK_REG_LAST (STACKNO) + 1 - (STACK)->top[STACKNO])


/* The structure containing miscellaneous useful information about an insn.
   It is filled by ANALYSE_INSN. The info is associated mostly with
   the stack registers the insn acts on. 

     IN contains info on the stack regs that are input for the insn.
   The regs that are both input and output (matching constraint) are
   there also, because they need the same treatment as the pure
   inputs.

     OUT contains info on the regs that are "pushed outputs" of the
   insn; this need to be in a separate array as the same reg may act
   as both input and output, in different classes and modes.

     Dimensions of IN and OUT are choosen so as to be enough for any
   insn--rather wasteful, though.  */

struct insn_info {
  int is_asm;

  /* The set of virtual stack-regs that are outputs of the insn.
     If the insn has a multi-register output operand, all subregs
     of the compound register are in this set. */
  HARD_REG_SET out_regs;

  struct insn_info_reg {
    rtx reg;
    int flags;
    enum reg_class class;
  } in[FIRST_NONSTACK_REGISTER], *in_last,
   out[FIRST_NONSTACK_REGISTER], *out_last;
};

enum {
  INPUT         = 0x01,         /* reg used as input */
  OUTPUT        = 0x02,         /* reg used as output */
  EARLYCLOBBER  = 0x04,         /* '&' presents in operand's constraint */
  POPPED        = 0x20,         /* input reg is implicitly popped */
  PUSHED        = POPPED,       /* output reg is implicitly pushed */
  JUMP_POPPED   = 0x40,         /* input reg is implicitly popped by jump
                                   insn when the jump is taken */
};


/* This is the register file for all register after conversion */
extern rtx hard_reg[FIRST_PSEUDO_REGISTER][(int) MAX_MACHINE_MODE];

/* Declarations for some functions from reg-stack.c */
int print_stack     PROTO((stack));

