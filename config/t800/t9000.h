/* Definitions of target machine for GNU compiler for IMST800.
   Copyright (C) 1995 Free Software Foundation, Inc.

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
the Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.  */


/* We define some processor features here which can be tested with
   #ifdef in the generic t800.h.  One might think that using macros
   derived automatically from md file would be a better way, but those
   macros are not available here because insn-flags.h is always
   included *after* config.h  */

/* Have 16-bit memory access instructions (ls/ss)  */

#define HAVE_HALFWORD_LOAD_STORE
#define HAVE_POP

#include "t800/t800.h"
