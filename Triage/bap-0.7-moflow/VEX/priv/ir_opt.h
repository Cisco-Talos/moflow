
/*---------------------------------------------------------------*/
/*--- begin                                          ir_opt.h ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
      info@open-works.net

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.

   The GNU General Public License is contained in the file COPYING.

   Neither the names of the U.S. Department of Energy nor the
   University of California nor the names of its contributors may be
   used to endorse or promote products derived from this software
   without prior written permission.
*/

#ifndef __VEX_IR_OPT_H
#define __VEX_IR_OPT_H

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"

/* Top level optimiser entry point.  Returns a new BB.  Operates
   under the control of the global "vex_control" struct. */
extern 
IRSB* do_iropt_BB(
         IRSB* bb,
         IRExpr* (*specHelper) (HChar*, IRExpr**, IRStmt**, Int),
         Bool (*preciseMemExnsFn)(Int,Int),
         Addr64 guest_addr,
         VexArch guest_arch
      );

/* Do a constant folding/propagation pass. */
extern
IRSB* cprop_BB ( IRSB* );

/* Do a dead-code removal pass.  bb is destructively modified. */
extern
void do_deadcode_BB ( IRSB* bb );

/* The tree-builder.  Make (approximately) maximal safe trees.  bb is
   destructively modified. */
extern
void ado_treebuild_BB ( IRSB* bb );

#endif /* ndef __VEX_IR_OPT_H */

/*---------------------------------------------------------------*/
/*--- end                                            ir_opt.h ---*/
/*---------------------------------------------------------------*/
