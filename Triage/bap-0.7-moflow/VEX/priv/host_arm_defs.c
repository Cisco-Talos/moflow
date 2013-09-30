
/*---------------------------------------------------------------*/
/*--- begin                                   host_arm_defs.c ---*/
/*---------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2004-2010 OpenWorks LLP
      info@open-works.net

   NEON support is
   Copyright (C) 2010-2010 Samsung Electronics
   contributed by Dmitry Zhurikhin <zhur@ispras.ru>
              and Kirill Batuzov <batuzovk@ispras.ru>

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
*/

#include "libvex_basictypes.h"
#include "libvex.h"
#include "libvex_trc_values.h"

#include "main_util.h"
#include "host_generic_regs.h"
#include "host_arm_defs.h"

UInt arm_hwcaps = 0;


/* --------- Registers. --------- */

/* The usual HReg abstraction.
   There are 16 general purpose regs.
*/

void ppHRegARM ( HReg reg )  {
   Int r;
   /* Be generic for all virtual regs. */
   if (hregIsVirtual(reg)) {
      ppHReg(reg);
      return;
   }
   /* But specific for real regs. */
   switch (hregClass(reg)) {
      case HRcInt32:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("r%d", r);
         return;
      case HRcFlt64:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("d%d", r);
         return;
      case HRcFlt32:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 32);
         vex_printf("s%d", r);
         return;
      case HRcVec128:
         r = hregNumber(reg);
         vassert(r >= 0 && r < 16);
         vex_printf("q%d", r);
         return;
      default:
         vpanic("ppHRegARM");
   }
}

HReg hregARM_R0  ( void ) { return mkHReg(0,  HRcInt32, False); }
HReg hregARM_R1  ( void ) { return mkHReg(1,  HRcInt32, False); }
HReg hregARM_R2  ( void ) { return mkHReg(2,  HRcInt32, False); }
HReg hregARM_R3  ( void ) { return mkHReg(3,  HRcInt32, False); }
HReg hregARM_R4  ( void ) { return mkHReg(4,  HRcInt32, False); }
HReg hregARM_R5  ( void ) { return mkHReg(5,  HRcInt32, False); }
HReg hregARM_R6  ( void ) { return mkHReg(6,  HRcInt32, False); }
HReg hregARM_R7  ( void ) { return mkHReg(7,  HRcInt32, False); }
HReg hregARM_R8  ( void ) { return mkHReg(8,  HRcInt32, False); }
HReg hregARM_R9  ( void ) { return mkHReg(9,  HRcInt32, False); }
HReg hregARM_R10 ( void ) { return mkHReg(10, HRcInt32, False); }
HReg hregARM_R11 ( void ) { return mkHReg(11, HRcInt32, False); }
HReg hregARM_R12 ( void ) { return mkHReg(12, HRcInt32, False); }
HReg hregARM_R13 ( void ) { return mkHReg(13, HRcInt32, False); }
HReg hregARM_R14 ( void ) { return mkHReg(14, HRcInt32, False); }
HReg hregARM_R15 ( void ) { return mkHReg(15, HRcInt32, False); }
HReg hregARM_D8  ( void ) { return mkHReg(8,  HRcFlt64, False); }
HReg hregARM_D9  ( void ) { return mkHReg(9,  HRcFlt64, False); }
HReg hregARM_D10 ( void ) { return mkHReg(10, HRcFlt64, False); }
HReg hregARM_D11 ( void ) { return mkHReg(11, HRcFlt64, False); }
HReg hregARM_D12 ( void ) { return mkHReg(12, HRcFlt64, False); }
HReg hregARM_S26 ( void ) { return mkHReg(26, HRcFlt32, False); }
HReg hregARM_S27 ( void ) { return mkHReg(27, HRcFlt32, False); }
HReg hregARM_S28 ( void ) { return mkHReg(28, HRcFlt32, False); }
HReg hregARM_S29 ( void ) { return mkHReg(29, HRcFlt32, False); }
HReg hregARM_S30 ( void ) { return mkHReg(30, HRcFlt32, False); }
HReg hregARM_Q8  ( void ) { return mkHReg(8,  HRcVec128, False); }
HReg hregARM_Q9  ( void ) { return mkHReg(9,  HRcVec128, False); }
HReg hregARM_Q10 ( void ) { return mkHReg(10, HRcVec128, False); }
HReg hregARM_Q11 ( void ) { return mkHReg(11, HRcVec128, False); }
HReg hregARM_Q12 ( void ) { return mkHReg(12, HRcVec128, False); }
HReg hregARM_Q13 ( void ) { return mkHReg(13, HRcVec128, False); }
HReg hregARM_Q14 ( void ) { return mkHReg(14, HRcVec128, False); }
HReg hregARM_Q15 ( void ) { return mkHReg(15, HRcVec128, False); }

void getAllocableRegs_ARM ( Int* nregs, HReg** arr )
{
   Int i = 0;
   *nregs = 26;
   *arr = LibVEX_Alloc(*nregs * sizeof(HReg));
   // callee saves ones are listed first, since we prefer them
   // if they're available
   (*arr)[i++] = hregARM_R4();
   (*arr)[i++] = hregARM_R5();
   (*arr)[i++] = hregARM_R6();
   (*arr)[i++] = hregARM_R7();
   (*arr)[i++] = hregARM_R10();
   (*arr)[i++] = hregARM_R11();
   // otherwise we'll have to slum it out with caller-saves ones
   (*arr)[i++] = hregARM_R0();
   (*arr)[i++] = hregARM_R1();
   (*arr)[i++] = hregARM_R2();
   (*arr)[i++] = hregARM_R3();
   (*arr)[i++] = hregARM_R9();
   // FP hreegisters.  Note: these are all callee-save.  Yay!
   // Hence we don't need to mention them as trashed in
   // getHRegUsage for ARMInstr_Call.
   (*arr)[i++] = hregARM_D8();
   (*arr)[i++] = hregARM_D9();
   (*arr)[i++] = hregARM_D10();
   (*arr)[i++] = hregARM_D11();
   (*arr)[i++] = hregARM_D12();
   (*arr)[i++] = hregARM_S26();
   (*arr)[i++] = hregARM_S27();
   (*arr)[i++] = hregARM_S28();
   (*arr)[i++] = hregARM_S29();
   (*arr)[i++] = hregARM_S30();

   (*arr)[i++] = hregARM_Q8();
   (*arr)[i++] = hregARM_Q9();
   (*arr)[i++] = hregARM_Q10();
   (*arr)[i++] = hregARM_Q11();
   (*arr)[i++] = hregARM_Q12();

   //(*arr)[i++] = hregARM_Q13();
   //(*arr)[i++] = hregARM_Q14();
   //(*arr)[i++] = hregARM_Q15();

   // unavail: r8 as GSP
   // r12 is used as a spill/reload temporary
   // r13 as SP
   // r14 as LR
   // r15 as PC
   //
   // All in all, we have 11 allocatable integer registers:
   // 0 1 2 3 4 5 6 7 9 10 11, with r8 dedicated as GSP
   // and r12 dedicated as a spill temporary.
   // 13 14 and 15 are not under the allocator's control.
   //
   // Hence for the allocatable registers we have:
   //
   // callee-saved: 4 5 6 7 (8) 9 10 11
   // caller-saved: 0 1 2 3
   // Note 9 is ambiguous: the base EABI does not give an e/r-saved
   // designation for it, but the Linux instantiation of the ABI
   // specifies it as callee-saved.
   //
   // If the set of available registers changes or if the e/r status
   // changes, be sure to re-check/sync the definition of
   // getHRegUsage for ARMInstr_Call too.
   vassert(i == *nregs);
}



/* --------- Condition codes, ARM encoding. --------- */

HChar* showARMCondCode ( ARMCondCode cond ) {
   switch (cond) {
       case ARMcc_EQ:  return "eq";
       case ARMcc_NE:  return "ne";
       case ARMcc_HS:  return "hs";
       case ARMcc_LO:  return "lo";
       case ARMcc_MI:  return "mi";
       case ARMcc_PL:  return "pl";
       case ARMcc_VS:  return "vs";
       case ARMcc_VC:  return "vc";
       case ARMcc_HI:  return "hi";
       case ARMcc_LS:  return "ls";
       case ARMcc_GE:  return "ge";
       case ARMcc_LT:  return "lt";
       case ARMcc_GT:  return "gt";
       case ARMcc_LE:  return "le";
       case ARMcc_AL:  return "al"; // default
       case ARMcc_NV:  return "nv";
       default: vpanic("showARMCondCode");
   }
}


/* --------- Mem AModes: Addressing Mode 1 --------- */

ARMAMode1* ARMAMode1_RI  ( HReg reg, Int simm13 ) {
   ARMAMode1* am        = LibVEX_Alloc(sizeof(ARMAMode1));
   am->tag              = ARMam1_RI;
   am->ARMam1.RI.reg    = reg;
   am->ARMam1.RI.simm13 = simm13;
   vassert(-4095 <= simm13 && simm13 <= 4095);
   return am;
}
ARMAMode1* ARMAMode1_RRS ( HReg base, HReg index, UInt shift ) {
   ARMAMode1* am        = LibVEX_Alloc(sizeof(ARMAMode1));
   am->tag              = ARMam1_RRS;
   am->ARMam1.RRS.base  = base;
   am->ARMam1.RRS.index = index;
   am->ARMam1.RRS.shift = shift;
   vassert(0 <= shift && shift <= 3);
   return am;
}

void ppARMAMode1 ( ARMAMode1* am ) {
   switch (am->tag) {
      case ARMam1_RI:
         vex_printf("%d(", am->ARMam1.RI.simm13);
         ppHRegARM(am->ARMam1.RI.reg);
         vex_printf(")");
         break;
      case ARMam1_RRS:
         vex_printf("(");
         ppHRegARM(am->ARMam1.RRS.base);
         vex_printf(",");
         ppHRegARM(am->ARMam1.RRS.index);
         vex_printf(",%u)", am->ARMam1.RRS.shift);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARMAMode1 ( HRegUsage* u, ARMAMode1* am ) {
   switch (am->tag) {
      case ARMam1_RI:
         addHRegUse(u, HRmRead, am->ARMam1.RI.reg);
         return;
      case ARMam1_RRS:
         //    addHRegUse(u, HRmRead, am->ARMam1.RRS.base);
         //    addHRegUse(u, HRmRead, am->ARMam1.RRS.index);
         //   return;
      default:
         vpanic("addRegUsage_ARMAmode1");
   }
}

static void mapRegs_ARMAMode1 ( HRegRemap* m, ARMAMode1* am ) {
   switch (am->tag) {
      case ARMam1_RI:
         am->ARMam1.RI.reg = lookupHRegRemap(m, am->ARMam1.RI.reg);
         return;
      case ARMam1_RRS:
         //am->ARMam1.RR.base =lookupHRegRemap(m, am->ARMam1.RR.base);
         //am->ARMam1.RR.index = lookupHRegRemap(m, am->ARMam1.RR.index);
         //return;
      default:
         vpanic("mapRegs_ARMAmode1");
   }
}


/* --------- Mem AModes: Addressing Mode 2 --------- */

ARMAMode2* ARMAMode2_RI ( HReg reg, Int simm9 ) {
   ARMAMode2* am       = LibVEX_Alloc(sizeof(ARMAMode2));
   am->tag             = ARMam2_RI;
   am->ARMam2.RI.reg   = reg;
   am->ARMam2.RI.simm9 = simm9;
   vassert(-255 <= simm9 && simm9 <= 255);
   return am;
}
ARMAMode2* ARMAMode2_RR ( HReg base, HReg index ) {
   ARMAMode2* am       = LibVEX_Alloc(sizeof(ARMAMode2));
   am->tag             = ARMam2_RR;
   am->ARMam2.RR.base  = base;
   am->ARMam2.RR.index = index;
   return am;
}

void ppARMAMode2 ( ARMAMode2* am ) {
   switch (am->tag) {
      case ARMam2_RI:
         vex_printf("%d(", am->ARMam2.RI.simm9);
         ppHRegARM(am->ARMam2.RI.reg);
         vex_printf(")");
         break;
      case ARMam2_RR:
         vex_printf("(");
         ppHRegARM(am->ARMam2.RR.base);
         vex_printf(",");
         ppHRegARM(am->ARMam2.RR.index);
         vex_printf(")");
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARMAMode2 ( HRegUsage* u, ARMAMode2* am ) {
   switch (am->tag) {
      case ARMam2_RI:
         addHRegUse(u, HRmRead, am->ARMam2.RI.reg);
         return;
      case ARMam2_RR:
         //    addHRegUse(u, HRmRead, am->ARMam2.RR.base);
         //    addHRegUse(u, HRmRead, am->ARMam2.RR.index);
         //   return;
      default:
         vpanic("addRegUsage_ARMAmode2");
   }
}

static void mapRegs_ARMAMode2 ( HRegRemap* m, ARMAMode2* am ) {
   switch (am->tag) {
      case ARMam2_RI:
         am->ARMam2.RI.reg = lookupHRegRemap(m, am->ARMam2.RI.reg);
         return;
      case ARMam2_RR:
         //am->ARMam2.RR.base =lookupHRegRemap(m, am->ARMam2.RR.base);
         //am->ARMam2.RR.index = lookupHRegRemap(m, am->ARMam2.RR.index);
         //return;
      default:
         vpanic("mapRegs_ARMAmode2");
   }
}


/* --------- Mem AModes: Addressing Mode VFP --------- */

ARMAModeV* mkARMAModeV ( HReg reg, Int simm11 ) {
   ARMAModeV* am = LibVEX_Alloc(sizeof(ARMAModeV));
   vassert(simm11 >= -1020 && simm11 <= 1020);
   vassert(0 == (simm11 & 3));
   am->reg    = reg;
   am->simm11 = simm11;
   return am;
}

void ppARMAModeV ( ARMAModeV* am ) {
   vex_printf("%d(", am->simm11);
   ppHRegARM(am->reg);
   vex_printf(")");
}

static void addRegUsage_ARMAModeV ( HRegUsage* u, ARMAModeV* am ) {
   addHRegUse(u, HRmRead, am->reg);
}

static void mapRegs_ARMAModeV ( HRegRemap* m, ARMAModeV* am ) {
   am->reg = lookupHRegRemap(m, am->reg);
}


/* --------- Mem AModes: Addressing Mode Neon ------- */

ARMAModeN *mkARMAModeN_RR ( HReg rN, HReg rM ) {
   ARMAModeN* am = LibVEX_Alloc(sizeof(ARMAModeN));
   am->tag = ARMamN_RR;
   am->ARMamN.RR.rN = rN;
   am->ARMamN.RR.rM = rM;
   return am;
}

ARMAModeN *mkARMAModeN_R ( HReg rN ) {
   ARMAModeN* am = LibVEX_Alloc(sizeof(ARMAModeN));
   am->tag = ARMamN_R;
   am->ARMamN.R.rN = rN;
   return am;
}

static void addRegUsage_ARMAModeN ( HRegUsage* u, ARMAModeN* am ) {
   if (am->tag == ARMamN_R) {
      addHRegUse(u, HRmRead, am->ARMamN.R.rN);
   } else {
      addHRegUse(u, HRmRead, am->ARMamN.RR.rN);
      addHRegUse(u, HRmRead, am->ARMamN.RR.rM);
   }
}

static void mapRegs_ARMAModeN ( HRegRemap* m, ARMAModeN* am ) {
   if (am->tag == ARMamN_R) {
      am->ARMamN.R.rN = lookupHRegRemap(m, am->ARMamN.R.rN);
   } else {
      am->ARMamN.RR.rN = lookupHRegRemap(m, am->ARMamN.RR.rN);
      am->ARMamN.RR.rM = lookupHRegRemap(m, am->ARMamN.RR.rM);
   }
}

void ppARMAModeN ( ARMAModeN* am ) {
   vex_printf("[");
   if (am->tag == ARMamN_R) {
      ppHRegARM(am->ARMamN.R.rN);
   } else {
      ppHRegARM(am->ARMamN.RR.rN);
   }
   vex_printf("]");
   if (am->tag == ARMamN_RR) {
      vex_printf(", ");
      ppHRegARM(am->ARMamN.RR.rM);
   }
}


/* --------- Reg or imm-8x4 operands --------- */

static UInt ROR32 ( UInt x, UInt sh ) {
   vassert(sh >= 0 && sh < 32);
   if (sh == 0)
      return x;
   else
      return (x << (32-sh)) | (x >> sh);
}

ARMRI84* ARMRI84_I84 ( UShort imm8, UShort imm4 ) {
   ARMRI84* ri84          = LibVEX_Alloc(sizeof(ARMRI84));
   ri84->tag              = ARMri84_I84;
   ri84->ARMri84.I84.imm8 = imm8;
   ri84->ARMri84.I84.imm4 = imm4;
   vassert(imm8 >= 0 && imm8 <= 255);
   vassert(imm4 >= 0 && imm4 <= 15);
   return ri84;
}
ARMRI84* ARMRI84_R ( HReg reg ) {
   ARMRI84* ri84       = LibVEX_Alloc(sizeof(ARMRI84));
   ri84->tag           = ARMri84_R;
   ri84->ARMri84.R.reg = reg;
   return ri84;
}

void ppARMRI84 ( ARMRI84* ri84 ) {
   switch (ri84->tag) {
      case ARMri84_I84:
         vex_printf("0x%x", ROR32(ri84->ARMri84.I84.imm8,
                                  2 * ri84->ARMri84.I84.imm4));
         break;
      case ARMri84_R:
         ppHRegARM(ri84->ARMri84.R.reg);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARMRI84 ( HRegUsage* u, ARMRI84* ri84 ) {
   switch (ri84->tag) {
      case ARMri84_I84:
         return;
      case ARMri84_R:
         addHRegUse(u, HRmRead, ri84->ARMri84.R.reg);
         return;
      default:
         vpanic("addRegUsage_ARMRI84");
   }
}

static void mapRegs_ARMRI84 ( HRegRemap* m, ARMRI84* ri84 ) {
   switch (ri84->tag) {
      case ARMri84_I84:
         return;
      case ARMri84_R:
         ri84->ARMri84.R.reg = lookupHRegRemap(m, ri84->ARMri84.R.reg);
         return;
      default:
         vpanic("mapRegs_ARMRI84");
   }
}


/* --------- Reg or imm5 operands --------- */

ARMRI5* ARMRI5_I5 ( UInt imm5 ) {
   ARMRI5* ri5         = LibVEX_Alloc(sizeof(ARMRI5));
   ri5->tag            = ARMri5_I5;
   ri5->ARMri5.I5.imm5 = imm5;
   vassert(imm5 > 0 && imm5 <= 31); // zero is not allowed
   return ri5;
}
ARMRI5* ARMRI5_R ( HReg reg ) {
   ARMRI5* ri5       = LibVEX_Alloc(sizeof(ARMRI5));
   ri5->tag          = ARMri5_R;
   ri5->ARMri5.R.reg = reg;
   return ri5;
}

void ppARMRI5 ( ARMRI5* ri5 ) {
   switch (ri5->tag) {
      case ARMri5_I5:
         vex_printf("%u", ri5->ARMri5.I5.imm5);
         break;
      case ARMri5_R:
         ppHRegARM(ri5->ARMri5.R.reg);
         break;
      default:
         vassert(0);
   }
}

static void addRegUsage_ARMRI5 ( HRegUsage* u, ARMRI5* ri5 ) {
   switch (ri5->tag) {
      case ARMri5_I5:
         return;
      case ARMri5_R:
         addHRegUse(u, HRmRead, ri5->ARMri5.R.reg);
         return;
      default:
         vpanic("addRegUsage_ARMRI5");
   }
}

static void mapRegs_ARMRI5 ( HRegRemap* m, ARMRI5* ri5 ) {
   switch (ri5->tag) {
      case ARMri5_I5:
         return;
      case ARMri5_R:
         ri5->ARMri5.R.reg = lookupHRegRemap(m, ri5->ARMri5.R.reg);
         return;
      default:
         vpanic("mapRegs_ARMRI5");
   }
}

/* -------- Neon Immediate operatnd --------- */

ARMNImm* ARMNImm_TI ( UInt type, UInt imm8 ) {
   ARMNImm* i = LibVEX_Alloc(sizeof(ARMNImm));
   i->type = type;
   i->imm8 = imm8;
   return i;
}

ULong ARMNImm_to_Imm64 ( ARMNImm* imm ) {
   int i, j;
   ULong y, x = imm->imm8;
   switch (imm->type) {
      case 3:
         x = x << 8;
      case 2:
         x = x << 8;
      case 1:
         x = x << 8;
      case 0:
         return (x << 32) | x;
      case 5:
      case 6:
         if (imm->type == 5)
            x = x << 8;
         else
            x = (x << 8) | x;
      case 4:
         x = (x << 16) | x;
         return (x << 32) | x;
      case 8:
         x = (x << 8) | 0xFF;
      case 7:
         x = (x << 8) | 0xFF;
         return (x << 32) | x;
      case 9:
         x = 0;
         for (i = 7; i >= 0; i--) {
            y = ((ULong)imm->imm8 >> i) & 1;
            for (j = 0; j < 8; j++) {
               x = (x << 1) | y;
            }
         }
         return x;
      case 10:
         x |= (x & 0x80) << 5;
         x |= (~x & 0x40) << 5;
         x &= 0x187F; /* 0001 1000 0111 1111 */
         x |= (x & 0x40) << 4;
         x |= (x & 0x40) << 3;
         x |= (x & 0x40) << 2;
         x |= (x & 0x40) << 1;
         x = x << 19;
         x = (x << 32) | x;
         return x;
      default:
         vpanic("ARMNImm_to_Imm64");
   }
}

ARMNImm* Imm64_to_ARMNImm ( ULong x ) {
   ARMNImm tmp;
   if ((x & 0xFFFFFFFF) == (x >> 32)) {
      if ((x & 0xFFFFFF00) == 0)
         return ARMNImm_TI(0, x & 0xFF);
      if ((x & 0xFFFF00FF) == 0)
         return ARMNImm_TI(1, (x >> 8) & 0xFF);
      if ((x & 0xFF00FFFF) == 0)
         return ARMNImm_TI(2, (x >> 16) & 0xFF);
      if ((x & 0x00FFFFFF) == 0)
         return ARMNImm_TI(3, (x >> 24) & 0xFF);
      if ((x & 0xFFFF00FF) == 0xFF)
         return ARMNImm_TI(7, (x >> 8) & 0xFF);
      if ((x & 0xFF00FFFF) == 0xFFFF)
         return ARMNImm_TI(8, (x >> 16) & 0xFF);
      if ((x & 0xFFFF) == ((x >> 16) & 0xFFFF)) {
         if ((x & 0xFF00) == 0)
            return ARMNImm_TI(4, x & 0xFF);
         if ((x & 0x00FF) == 0)
            return ARMNImm_TI(5, (x >> 8) & 0xFF);
         if ((x & 0xFF) == ((x >> 8) & 0xFF))
            return ARMNImm_TI(6, x & 0xFF);
      }
      if ((x & 0x7FFFF) == 0) {
         tmp.type = 10;
         tmp.imm8 = ((x >> 19) & 0x7F) | ((x >> 24) & 0x80);
         if (ARMNImm_to_Imm64(&tmp) == x)
            return ARMNImm_TI(tmp.type, tmp.imm8);
      }
   } else {
      /* This can only be type 9. */
      tmp.imm8 = (((x >> 56) & 1) << 7)
               | (((x >> 48) & 1) << 6)
               | (((x >> 40) & 1) << 5)
               | (((x >> 32) & 1) << 4)
               | (((x >> 24) & 1) << 3)
               | (((x >> 16) & 1) << 2)
               | (((x >>  8) & 1) << 1)
               | (((x >>  0) & 1) << 0);
      tmp.type = 9;
      if (ARMNImm_to_Imm64 (&tmp) == x)
         return ARMNImm_TI(tmp.type, tmp.imm8);
   }
   return NULL;
}

void ppARMNImm (ARMNImm* i) {
   ULong x = ARMNImm_to_Imm64(i);
   vex_printf("0x%llX%llX", x, x);
}

/* -- Register or scalar operand --- */

ARMNRS* mkARMNRS(ARMNRS_tag tag, HReg reg, UInt index)
{
   ARMNRS *p = LibVEX_Alloc(sizeof(ARMNRS));
   p->tag = tag;
   p->reg = reg;
   p->index = index;
   return p;
}

void ppARMNRS(ARMNRS *p)
{
   ppHRegARM(p->reg);
   if (p->tag == ARMNRS_Scalar) {
      vex_printf("[%d]", p->index);
   }
}

/* --------- Instructions. --------- */

HChar* showARMAluOp ( ARMAluOp op ) {
   switch (op) {
      case ARMalu_ADD:  return "add";
      case ARMalu_ADDS: return "adds";
      case ARMalu_ADC:  return "adc";
      case ARMalu_SUB:  return "sub";
      case ARMalu_SUBS: return "subs";
      case ARMalu_SBC:  return "sbc";
      case ARMalu_AND:  return "and";
      case ARMalu_BIC:  return "bic";
      case ARMalu_OR:   return "orr";
      case ARMalu_XOR:  return "xor";
      default: vpanic("showARMAluOp");
   }
}

HChar* showARMShiftOp ( ARMShiftOp op ) {
   switch (op) {
      case ARMsh_SHL: return "shl";
      case ARMsh_SHR: return "shr";
      case ARMsh_SAR: return "sar";
      default: vpanic("showARMShiftOp");
   }
}

HChar* showARMUnaryOp ( ARMUnaryOp op ) {
   switch (op) {
      case ARMun_NEG: return "neg";
      case ARMun_NOT: return "not";
      case ARMun_CLZ: return "clz";
      default: vpanic("showARMUnaryOp");
   }
}

HChar* showARMMulOp ( ARMMulOp op ) {
   switch (op) {
      case ARMmul_PLAIN: return "mul";
      case ARMmul_ZX:    return "umull";
      case ARMmul_SX:    return "smull";
      default: vpanic("showARMMulOp");
   }
}

HChar* showARMVfpOp ( ARMVfpOp op ) {
   switch (op) {
      case ARMvfp_ADD: return "add";
      case ARMvfp_SUB: return "sub";
      case ARMvfp_MUL: return "mul";
      case ARMvfp_DIV: return "div";
      default: vpanic("showARMVfpOp");
   }
}

HChar* showARMVfpUnaryOp ( ARMVfpUnaryOp op ) {
   switch (op) {
      case ARMvfpu_COPY: return "cpy";
      case ARMvfpu_NEG:  return "neg";
      case ARMvfpu_ABS:  return "abs";
      case ARMvfpu_SQRT: return "sqrt";
      default: vpanic("showARMVfpUnaryOp");
   }
}

HChar* showARMNeonBinOp ( ARMNeonBinOp op ) {
   switch (op) {
      case ARMneon_VAND: return "vand";
      case ARMneon_VORR: return "vorr";
      case ARMneon_VXOR: return "veor";
      case ARMneon_VADD: return "vadd";
      case ARMneon_VRHADDS: return "vrhadd";
      case ARMneon_VRHADDU: return "vrhadd";
      case ARMneon_VADDFP: return "vadd";
      case ARMneon_VPADDFP: return "vpadd";
      case ARMneon_VABDFP: return "vabd";
      case ARMneon_VSUB: return "vsub";
      case ARMneon_VSUBFP: return "vsub";
      case ARMneon_VMINU: return "vmin";
      case ARMneon_VMINS: return "vmin";
      case ARMneon_VMINF: return "vmin";
      case ARMneon_VMAXU: return "vmax";
      case ARMneon_VMAXS: return "vmax";
      case ARMneon_VMAXF: return "vmax";
      case ARMneon_VQADDU: return "vqadd";
      case ARMneon_VQADDS: return "vqadd";
      case ARMneon_VQSUBU: return "vqsub";
      case ARMneon_VQSUBS: return "vqsub";
      case ARMneon_VCGTU:  return "vcgt";
      case ARMneon_VCGTS:  return "vcgt";
      case ARMneon_VCGTF:  return "vcgt";
      case ARMneon_VCGEF:  return "vcgt";
      case ARMneon_VCGEU:  return "vcge";
      case ARMneon_VCGES:  return "vcge";
      case ARMneon_VCEQ:  return "vceq";
      case ARMneon_VCEQF:  return "vceq";
      case ARMneon_VPADD:   return "vpadd";
      case ARMneon_VPMINU:   return "vpmin";
      case ARMneon_VPMINS:   return "vpmin";
      case ARMneon_VPMINF:   return "vpmin";
      case ARMneon_VPMAXU:   return "vpmax";
      case ARMneon_VPMAXS:   return "vpmax";
      case ARMneon_VPMAXF:   return "vpmax";
      case ARMneon_VEXT:   return "vext";
      case ARMneon_VMUL:   return "vmuli";
      case ARMneon_VMULLU:   return "vmull";
      case ARMneon_VMULLS:   return "vmull";
      case ARMneon_VMULP:  return "vmul";
      case ARMneon_VMULFP:  return "vmul";
      case ARMneon_VMULLP:  return "vmul";
      case ARMneon_VQDMULH: return "vqdmulh";
      case ARMneon_VQRDMULH: return "vqrdmulh";
      case ARMneon_VQDMULL: return "vqdmull";
      case ARMneon_VTBL: return "vtbl";
      case ARMneon_VRECPS: return "vrecps";
      case ARMneon_VRSQRTS: return "vrecps";
      /* ... */
      default: vpanic("showARMNeonBinOp");
   }
}

HChar* showARMNeonBinOpDataType ( ARMNeonBinOp op ) {
   switch (op) {
      case ARMneon_VAND:
      case ARMneon_VORR:
      case ARMneon_VXOR:
         return "";
      case ARMneon_VADD:
      case ARMneon_VSUB:
      case ARMneon_VEXT:
      case ARMneon_VMUL:
      case ARMneon_VPADD:
      case ARMneon_VTBL:
      case ARMneon_VCEQ:
         return ".i";
      case ARMneon_VRHADDU:
      case ARMneon_VMINU:
      case ARMneon_VMAXU:
      case ARMneon_VQADDU:
      case ARMneon_VQSUBU:
      case ARMneon_VCGTU:
      case ARMneon_VCGEU:
      case ARMneon_VMULLU:
      case ARMneon_VPMINU:
      case ARMneon_VPMAXU:
         return ".u";
      case ARMneon_VRHADDS:
      case ARMneon_VMINS:
      case ARMneon_VMAXS:
      case ARMneon_VQADDS:
      case ARMneon_VQSUBS:
      case ARMneon_VCGTS:
      case ARMneon_VCGES:
      case ARMneon_VQDMULL:
      case ARMneon_VMULLS:
      case ARMneon_VPMINS:
      case ARMneon_VPMAXS:
      case ARMneon_VQDMULH:
      case ARMneon_VQRDMULH:
         return ".s";
      case ARMneon_VMULP:
      case ARMneon_VMULLP:
         return ".p";
      case ARMneon_VADDFP:
      case ARMneon_VABDFP:
      case ARMneon_VPADDFP:
      case ARMneon_VSUBFP:
      case ARMneon_VMULFP:
      case ARMneon_VMINF:
      case ARMneon_VMAXF:
      case ARMneon_VPMINF:
      case ARMneon_VPMAXF:
      case ARMneon_VCGTF:
      case ARMneon_VCGEF:
      case ARMneon_VCEQF:
      case ARMneon_VRECPS:
      case ARMneon_VRSQRTS:
         return ".f";
      /* ... */
      default: vpanic("showARMNeonBinOpDataType");
   }
}

HChar* showARMNeonUnOp ( ARMNeonUnOp op ) {
   switch (op) {
      case ARMneon_COPY: return "vmov";
      case ARMneon_COPYLS: return "vmov";
      case ARMneon_COPYLU: return "vmov";
      case ARMneon_COPYN: return "vmov";
      case ARMneon_COPYQNSS: return "vqmovn";
      case ARMneon_COPYQNUS: return "vqmovun";
      case ARMneon_COPYQNUU: return "vqmovn";
      case ARMneon_NOT: return "vmvn";
      case ARMneon_EQZ: return "vceq";
      case ARMneon_CNT: return "vcnt";
      case ARMneon_CLS: return "vcls";
      case ARMneon_CLZ: return "vclz";
      case ARMneon_DUP: return "vdup";
      case ARMneon_PADDLS: return "vpaddl";
      case ARMneon_PADDLU: return "vpaddl";
      case ARMneon_VQSHLNSS: return "vqshl";
      case ARMneon_VQSHLNUU: return "vqshl";
      case ARMneon_VQSHLNUS: return "vqshlu";
      case ARMneon_REV16: return "vrev16";
      case ARMneon_REV32: return "vrev32";
      case ARMneon_REV64: return "vrev64";
      case ARMneon_VCVTFtoU: return "vcvt";
      case ARMneon_VCVTFtoS: return "vcvt";
      case ARMneon_VCVTUtoF: return "vcvt";
      case ARMneon_VCVTStoF: return "vcvt";
      case ARMneon_VCVTFtoFixedU: return "vcvt";
      case ARMneon_VCVTFtoFixedS: return "vcvt";
      case ARMneon_VCVTFixedUtoF: return "vcvt";
      case ARMneon_VCVTFixedStoF: return "vcvt";
      case ARMneon_VCVTF32toF16: return "vcvt";
      case ARMneon_VCVTF16toF32: return "vcvt";
      case ARMneon_VRECIP: return "vrecip";
      case ARMneon_VRECIPF: return "vrecipf";
      case ARMneon_VNEGF: return "vneg";
      case ARMneon_ABS: return "vabs";
      case ARMneon_VABSFP: return "vabsfp";
      case ARMneon_VRSQRTEFP: return "vrsqrtefp";
      case ARMneon_VRSQRTE: return "vrsqrte";
      /* ... */
      default: vpanic("showARMNeonUnOp");
   }
}

HChar* showARMNeonUnOpDataType ( ARMNeonUnOp op ) {
   switch (op) {
      case ARMneon_COPY:
      case ARMneon_NOT:
         return "";
      case ARMneon_COPYN:
      case ARMneon_EQZ:
      case ARMneon_CNT:
      case ARMneon_DUP:
      case ARMneon_REV16:
      case ARMneon_REV32:
      case ARMneon_REV64:
         return ".i";
      case ARMneon_COPYLU:
      case ARMneon_PADDLU:
      case ARMneon_COPYQNUU:
      case ARMneon_VQSHLNUU:
      case ARMneon_VRECIP:
      case ARMneon_VRSQRTE:
         return ".u";
      case ARMneon_CLS:
      case ARMneon_CLZ:
      case ARMneon_COPYLS:
      case ARMneon_PADDLS:
      case ARMneon_COPYQNSS:
      case ARMneon_COPYQNUS:
      case ARMneon_VQSHLNSS:
      case ARMneon_VQSHLNUS:
      case ARMneon_ABS:
         return ".s";
      case ARMneon_VRECIPF:
      case ARMneon_VNEGF:
      case ARMneon_VABSFP:
      case ARMneon_VRSQRTEFP:
         return ".f";
      case ARMneon_VCVTFtoU: return ".u32.f32";
      case ARMneon_VCVTFtoS: return ".s32.f32";
      case ARMneon_VCVTUtoF: return ".f32.u32";
      case ARMneon_VCVTStoF: return ".f32.s32";
      case ARMneon_VCVTF16toF32: return ".f32.f16";
      case ARMneon_VCVTF32toF16: return ".f16.f32";
      case ARMneon_VCVTFtoFixedU: return ".u32.f32";
      case ARMneon_VCVTFtoFixedS: return ".s32.f32";
      case ARMneon_VCVTFixedUtoF: return ".f32.u32";
      case ARMneon_VCVTFixedStoF: return ".f32.s32";
      /* ... */
      default: vpanic("showARMNeonUnOpDataType");
   }
}

HChar* showARMNeonUnOpS ( ARMNeonUnOpS op ) {
   switch (op) {
      case ARMneon_SETELEM: return "vmov";
      case ARMneon_GETELEMU: return "vmov";
      case ARMneon_GETELEMS: return "vmov";
      case ARMneon_VDUP: return "vdup";
      /* ... */
      default: vpanic("showARMNeonUnarySOp");
   }
}

HChar* showARMNeonUnOpSDataType ( ARMNeonUnOpS op ) {
   switch (op) {
      case ARMneon_SETELEM:
      case ARMneon_VDUP:
         return ".i";
      case ARMneon_GETELEMS:
         return ".s";
      case ARMneon_GETELEMU:
         return ".u";
      /* ... */
      default: vpanic("showARMNeonUnarySOp");
   }
}

HChar* showARMNeonShiftOp ( ARMNeonShiftOp op ) {
   switch (op) {
      case ARMneon_VSHL: return "vshl";
      case ARMneon_VSAL: return "vshl";
      case ARMneon_VQSHL: return "vqshl";
      case ARMneon_VQSAL: return "vqshl";
      /* ... */
      default: vpanic("showARMNeonShiftOp");
   }
}

HChar* showARMNeonShiftOpDataType ( ARMNeonShiftOp op ) {
   switch (op) {
      case ARMneon_VSHL:
      case ARMneon_VQSHL:
         return ".u";
      case ARMneon_VSAL:
      case ARMneon_VQSAL:
         return ".s";
      /* ... */
      default: vpanic("showARMNeonShiftOpDataType");
   }
}

HChar* showARMNeonDualOp ( ARMNeonDualOp op ) {
   switch (op) {
      case ARMneon_TRN: return "vtrn";
      case ARMneon_ZIP: return "vzip";
      case ARMneon_UZP: return "vuzp";
      /* ... */
      default: vpanic("showARMNeonDualOp");
   }
}

HChar* showARMNeonDualOpDataType ( ARMNeonDualOp op ) {
   switch (op) {
      case ARMneon_TRN:
      case ARMneon_ZIP:
      case ARMneon_UZP:
         return "i";
      /* ... */
      default: vpanic("showARMNeonDualOp");
   }
}

static HChar* showARMNeonDataSize_wrk ( UInt size )
{
   switch (size) {
      case 0: return "8";
      case 1: return "16";
      case 2: return "32";
      case 3: return "64";
      default: vpanic("showARMNeonDataSize");
   }
}

static HChar* showARMNeonDataSize ( ARMInstr* i )
{
   switch (i->tag) {
      case ARMin_NBinary:
         if (i->ARMin.NBinary.op == ARMneon_VEXT)
            return "8";
         if (i->ARMin.NBinary.op == ARMneon_VAND ||
             i->ARMin.NBinary.op == ARMneon_VORR ||
             i->ARMin.NBinary.op == ARMneon_VXOR)
            return "";
         return showARMNeonDataSize_wrk(i->ARMin.NBinary.size);
      case ARMin_NUnary:
         if (i->ARMin.NUnary.op == ARMneon_COPY ||
             i->ARMin.NUnary.op == ARMneon_NOT ||
             i->ARMin.NUnary.op == ARMneon_VCVTF32toF16||
             i->ARMin.NUnary.op == ARMneon_VCVTF16toF32||
             i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedS ||
             i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedU ||
             i->ARMin.NUnary.op == ARMneon_VCVTFixedStoF ||
             i->ARMin.NUnary.op == ARMneon_VCVTFixedUtoF ||
             i->ARMin.NUnary.op == ARMneon_VCVTFtoS ||
             i->ARMin.NUnary.op == ARMneon_VCVTFtoU ||
             i->ARMin.NUnary.op == ARMneon_VCVTStoF ||
             i->ARMin.NUnary.op == ARMneon_VCVTUtoF)
            return "";
         if (i->ARMin.NUnary.op == ARMneon_VQSHLNSS ||
             i->ARMin.NUnary.op == ARMneon_VQSHLNUU ||
             i->ARMin.NUnary.op == ARMneon_VQSHLNUS) {
            UInt size;
            size = i->ARMin.NUnary.size;
            if (size & 0x40)
               return "64";
            if (size & 0x20)
               return "32";
            if (size & 0x10)
               return "16";
            if (size & 0x08)
               return "8";
            vpanic("showARMNeonDataSize");
         }
         return showARMNeonDataSize_wrk(i->ARMin.NUnary.size);
      case ARMin_NUnaryS:
         if (i->ARMin.NUnaryS.op == ARMneon_VDUP) {
            int size;
            size = i->ARMin.NUnaryS.size;
            if ((size & 1) == 1)
               return "8";
            if ((size & 3) == 2)
               return "16";
            if ((size & 7) == 4)
               return "32";
            vpanic("showARMNeonDataSize");
         }
         return showARMNeonDataSize_wrk(i->ARMin.NUnaryS.size);
      case ARMin_NShift:
         return showARMNeonDataSize_wrk(i->ARMin.NShift.size);
      case ARMin_NDual:
         return showARMNeonDataSize_wrk(i->ARMin.NDual.size);
      default:
         vpanic("showARMNeonDataSize");
   }
}

ARMInstr* ARMInstr_Alu ( ARMAluOp op,
                         HReg dst, HReg argL, ARMRI84* argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag            = ARMin_Alu;
   i->ARMin.Alu.op   = op;
   i->ARMin.Alu.dst  = dst;
   i->ARMin.Alu.argL = argL;
   i->ARMin.Alu.argR = argR;
   return i;
}
ARMInstr* ARMInstr_Shift  ( ARMShiftOp op,
                            HReg dst, HReg argL, ARMRI5* argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_Shift;
   i->ARMin.Shift.op   = op;
   i->ARMin.Shift.dst  = dst;
   i->ARMin.Shift.argL = argL;
   i->ARMin.Shift.argR = argR;
   return i;
}
ARMInstr* ARMInstr_Unary ( ARMUnaryOp op, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag             = ARMin_Unary;
   i->ARMin.Unary.op  = op;
   i->ARMin.Unary.dst = dst;
   i->ARMin.Unary.src = src;
   return i;
}
ARMInstr* ARMInstr_CmpOrTst ( Bool isCmp, HReg argL, ARMRI84* argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                  = ARMin_CmpOrTst;
   i->ARMin.CmpOrTst.isCmp = isCmp;
   i->ARMin.CmpOrTst.argL  = argL;
   i->ARMin.CmpOrTst.argR  = argR;
   return i;
}
ARMInstr* ARMInstr_Mov ( HReg dst, ARMRI84* src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag           = ARMin_Mov;
   i->ARMin.Mov.dst = dst;
   i->ARMin.Mov.src = src;
   return i;
}
ARMInstr* ARMInstr_Imm32  ( HReg dst, UInt imm32 ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_Imm32;
   i->ARMin.Imm32.dst   = dst;
   i->ARMin.Imm32.imm32 = imm32;
   return i;
}
ARMInstr* ARMInstr_LdSt32 ( Bool isLoad, HReg rD, ARMAMode1* amode ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_LdSt32;
   i->ARMin.LdSt32.isLoad = isLoad;
   i->ARMin.LdSt32.rD     = rD;
   i->ARMin.LdSt32.amode  = amode;
   return i;
}
ARMInstr* ARMInstr_LdSt16 ( Bool isLoad, Bool signedLoad,
                            HReg rD, ARMAMode2* amode ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                     = ARMin_LdSt16;
   i->ARMin.LdSt16.isLoad     = isLoad;
   i->ARMin.LdSt16.signedLoad = signedLoad;
   i->ARMin.LdSt16.rD         = rD;
   i->ARMin.LdSt16.amode      = amode;
   return i;
}
ARMInstr* ARMInstr_LdSt8U ( Bool isLoad, HReg rD, ARMAMode1* amode ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_LdSt8U;
   i->ARMin.LdSt8U.isLoad = isLoad;
   i->ARMin.LdSt8U.rD     = rD;
   i->ARMin.LdSt8U.amode  = amode;
   return i;
}
//extern ARMInstr* ARMInstr_Ld8S   ( HReg, ARMAMode2* );
ARMInstr* ARMInstr_Goto ( IRJumpKind jk, ARMCondCode cond, HReg gnext ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_Goto;
   i->ARMin.Goto.jk    = jk;
   i->ARMin.Goto.cond  = cond;
   i->ARMin.Goto.gnext = gnext;
   return i;
}
ARMInstr* ARMInstr_CMov ( ARMCondCode cond, HReg dst, ARMRI84* src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag             = ARMin_CMov;
   i->ARMin.CMov.cond = cond;
   i->ARMin.CMov.dst  = dst;
   i->ARMin.CMov.src  = src;
   vassert(cond != ARMcc_AL);
   return i;
}
ARMInstr* ARMInstr_Call ( ARMCondCode cond, HWord target, Int nArgRegs ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_Call;
   i->ARMin.Call.cond     = cond;
   i->ARMin.Call.target   = target;
   i->ARMin.Call.nArgRegs = nArgRegs;
   return i;
}
ARMInstr* ARMInstr_Mul ( ARMMulOp op ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag          = ARMin_Mul;
   i->ARMin.Mul.op = op;
   return i;
}
ARMInstr* ARMInstr_LdrEX ( Int szB ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag             = ARMin_LdrEX;
   i->ARMin.LdrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARMInstr* ARMInstr_StrEX ( Int szB ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag             = ARMin_StrEX;
   i->ARMin.StrEX.szB = szB;
   vassert(szB == 8 || szB == 4 || szB == 2 || szB == 1);
   return i;
}
ARMInstr* ARMInstr_VLdStD ( Bool isLoad, HReg dD, ARMAModeV* am ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_VLdStD;
   i->ARMin.VLdStD.isLoad = isLoad;
   i->ARMin.VLdStD.dD     = dD;
   i->ARMin.VLdStD.amode  = am;
   return i;
}
ARMInstr* ARMInstr_VLdStS ( Bool isLoad, HReg fD, ARMAModeV* am ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_VLdStS;
   i->ARMin.VLdStS.isLoad = isLoad;
   i->ARMin.VLdStS.fD     = fD;
   i->ARMin.VLdStS.amode  = am;
   return i;
}
ARMInstr* ARMInstr_VAluD ( ARMVfpOp op, HReg dst, HReg argL, HReg argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_VAluD;
   i->ARMin.VAluD.op   = op;
   i->ARMin.VAluD.dst  = dst;
   i->ARMin.VAluD.argL = argL;
   i->ARMin.VAluD.argR = argR;
   return i;
}
ARMInstr* ARMInstr_VAluS ( ARMVfpOp op, HReg dst, HReg argL, HReg argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_VAluS;
   i->ARMin.VAluS.op   = op;
   i->ARMin.VAluS.dst  = dst;
   i->ARMin.VAluS.argL = argL;
   i->ARMin.VAluS.argR = argR;
   return i;
}
ARMInstr* ARMInstr_VUnaryD ( ARMVfpUnaryOp op, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_VUnaryD;
   i->ARMin.VUnaryD.op  = op;
   i->ARMin.VUnaryD.dst = dst;
   i->ARMin.VUnaryD.src = src;
   return i;
}
ARMInstr* ARMInstr_VUnaryS ( ARMVfpUnaryOp op, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_VUnaryS;
   i->ARMin.VUnaryS.op  = op;
   i->ARMin.VUnaryS.dst = dst;
   i->ARMin.VUnaryS.src = src;
   return i;
}
ARMInstr* ARMInstr_VCmpD ( HReg argL, HReg argR ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_VCmpD;
   i->ARMin.VCmpD.argL = argL;
   i->ARMin.VCmpD.argR = argR;
   return i;
}
ARMInstr* ARMInstr_VCMovD ( ARMCondCode cond, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_VCMovD;
   i->ARMin.VCMovD.cond = cond;
   i->ARMin.VCMovD.dst  = dst;
   i->ARMin.VCMovD.src  = src;
   vassert(cond != ARMcc_AL);
   return i;
}
ARMInstr* ARMInstr_VCMovS ( ARMCondCode cond, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_VCMovS;
   i->ARMin.VCMovS.cond = cond;
   i->ARMin.VCMovS.dst  = dst;
   i->ARMin.VCMovS.src  = src;
   vassert(cond != ARMcc_AL);
   return i;
}
ARMInstr* ARMInstr_VCvtSD ( Bool sToD, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_VCvtSD;
   i->ARMin.VCvtSD.sToD = sToD;
   i->ARMin.VCvtSD.dst  = dst;
   i->ARMin.VCvtSD.src  = src;
   return i;
}
ARMInstr* ARMInstr_VXferD ( Bool toD, HReg dD, HReg rHi, HReg rLo ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_VXferD;
   i->ARMin.VXferD.toD = toD;
   i->ARMin.VXferD.dD  = dD;
   i->ARMin.VXferD.rHi = rHi;
   i->ARMin.VXferD.rLo = rLo;
   return i;
}
ARMInstr* ARMInstr_VXferS ( Bool toS, HReg fD, HReg rLo ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag              = ARMin_VXferS;
   i->ARMin.VXferS.toS = toS;
   i->ARMin.VXferS.fD  = fD;
   i->ARMin.VXferS.rLo = rLo;
   return i;
}
ARMInstr* ARMInstr_VCvtID ( Bool iToD, Bool syned,
                            HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_VCvtID;
   i->ARMin.VCvtID.iToD  = iToD;
   i->ARMin.VCvtID.syned = syned;
   i->ARMin.VCvtID.dst   = dst;
   i->ARMin.VCvtID.src   = src;
   return i;
}
ARMInstr* ARMInstr_FPSCR ( Bool toFPSCR, HReg iReg ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                 = ARMin_FPSCR;
   i->ARMin.FPSCR.toFPSCR = toFPSCR;
   i->ARMin.FPSCR.iReg    = iReg;
   return i;
}
ARMInstr* ARMInstr_MFence ( void ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag      = ARMin_MFence;
   return i;
}

ARMInstr* ARMInstr_NLdStQ ( Bool isLoad, HReg dQ, ARMAModeN *amode ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                  = ARMin_NLdStQ;
   i->ARMin.NLdStQ.isLoad  = isLoad;
   i->ARMin.NLdStQ.dQ      = dQ;
   i->ARMin.NLdStQ.amode   = amode;
   return i;
}

ARMInstr* ARMInstr_NLdStD ( Bool isLoad, HReg dD, ARMAModeN *amode ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                  = ARMin_NLdStD;
   i->ARMin.NLdStD.isLoad  = isLoad;
   i->ARMin.NLdStD.dD      = dD;
   i->ARMin.NLdStD.amode   = amode;
   return i;
}

ARMInstr* ARMInstr_NUnary ( ARMNeonUnOp op, HReg dQ, HReg nQ,
                            UInt size, Bool Q ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_NUnary;
   i->ARMin.NUnary.op   = op;
   i->ARMin.NUnary.src  = nQ;
   i->ARMin.NUnary.dst  = dQ;
   i->ARMin.NUnary.size = size;
   i->ARMin.NUnary.Q    = Q;
   return i;
}

ARMInstr* ARMInstr_NUnaryS ( ARMNeonUnOpS op, ARMNRS* dst, ARMNRS* src,
                             UInt size, Bool Q ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_NUnaryS;
   i->ARMin.NUnaryS.op   = op;
   i->ARMin.NUnaryS.src  = src;
   i->ARMin.NUnaryS.dst  = dst;
   i->ARMin.NUnaryS.size = size;
   i->ARMin.NUnaryS.Q    = Q;
   return i;
}

ARMInstr* ARMInstr_NDual ( ARMNeonDualOp op, HReg nQ, HReg mQ,
                           UInt size, Bool Q ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_NDual;
   i->ARMin.NDual.op   = op;
   i->ARMin.NDual.arg1 = nQ;
   i->ARMin.NDual.arg2 = mQ;
   i->ARMin.NDual.size = size;
   i->ARMin.NDual.Q    = Q;
   return i;
}

ARMInstr* ARMInstr_NBinary ( ARMNeonBinOp op,
                             HReg dst, HReg argL, HReg argR,
                             UInt size, Bool Q ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_NBinary;
   i->ARMin.NBinary.op   = op;
   i->ARMin.NBinary.argL = argL;
   i->ARMin.NBinary.argR = argR;
   i->ARMin.NBinary.dst  = dst;
   i->ARMin.NBinary.size = size;
   i->ARMin.NBinary.Q    = Q;
   return i;
}

ARMInstr* ARMInstr_NeonImm (HReg dst, ARMNImm* imm ) {
   ARMInstr *i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag         = ARMin_NeonImm;
   i->ARMin.NeonImm.dst = dst;
   i->ARMin.NeonImm.imm = imm;
   return i;
}

ARMInstr* ARMInstr_NCMovQ ( ARMCondCode cond, HReg dst, HReg src ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag               = ARMin_NCMovQ;
   i->ARMin.NCMovQ.cond = cond;
   i->ARMin.NCMovQ.dst  = dst;
   i->ARMin.NCMovQ.src  = src;
   vassert(cond != ARMcc_AL);
   return i;
}

ARMInstr* ARMInstr_NShift ( ARMNeonShiftOp op,
                            HReg dst, HReg argL, HReg argR,
                            UInt size, Bool Q ) {
   ARMInstr* i = LibVEX_Alloc(sizeof(ARMInstr));
   i->tag                = ARMin_NShift;
   i->ARMin.NShift.op   = op;
   i->ARMin.NShift.argL = argL;
   i->ARMin.NShift.argR = argR;
   i->ARMin.NShift.dst  = dst;
   i->ARMin.NShift.size = size;
   i->ARMin.NShift.Q    = Q;
   return i;
}

/* Helper copy-pasted from isel.c */
static Bool fitsIn8x4 ( UInt* u8, UInt* u4, UInt u )
{
   UInt i;
   for (i = 0; i < 16; i++) {
      if (0 == (u & 0xFFFFFF00)) {
         *u8 = u;
         *u4 = i;
         return True;
      }
      u = ROR32(u, 30);
   }
   vassert(i == 16);
   return False;
}

ARMInstr* ARMInstr_Add32 ( HReg rD, HReg rN, UInt imm32 ) {
   UInt u8, u4;
   ARMInstr *i = LibVEX_Alloc(sizeof(ARMInstr));
   /* Try to generate single ADD if possible */
   if (fitsIn8x4(&u8, &u4, imm32)) {
      i->tag            = ARMin_Alu;
      i->ARMin.Alu.op   = ARMalu_ADD;
      i->ARMin.Alu.dst  = rD;
      i->ARMin.Alu.argL = rN;
      i->ARMin.Alu.argR = ARMRI84_I84(u8, u4);
   } else {
      i->tag               = ARMin_Add32;
      i->ARMin.Add32.rD    = rD;
      i->ARMin.Add32.rN    = rN;
      i->ARMin.Add32.imm32 = imm32;
   }
   return i;
}

/* ... */

void ppARMInstr ( ARMInstr* i ) {
   switch (i->tag) {
      case ARMin_Alu:
         vex_printf("%-4s  ", showARMAluOp(i->ARMin.Alu.op));
         ppHRegARM(i->ARMin.Alu.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.Alu.argL);
         vex_printf(", ");
         ppARMRI84(i->ARMin.Alu.argR);
         return;
      case ARMin_Shift:
         vex_printf("%s   ", showARMShiftOp(i->ARMin.Shift.op));
         ppHRegARM(i->ARMin.Shift.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.Shift.argL);
         vex_printf(", ");
         ppARMRI5(i->ARMin.Shift.argR);
         return;
      case ARMin_Unary:
         vex_printf("%s   ", showARMUnaryOp(i->ARMin.Unary.op));
         ppHRegARM(i->ARMin.Unary.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.Unary.src);
         return;
      case ARMin_CmpOrTst:
         vex_printf("%s   ", i->ARMin.CmpOrTst.isCmp ? "cmp" : "tst");
         ppHRegARM(i->ARMin.CmpOrTst.argL);
         vex_printf(", ");
         ppARMRI84(i->ARMin.CmpOrTst.argR);
         return;
      case ARMin_Mov:
         vex_printf("mov   ");
         ppHRegARM(i->ARMin.Mov.dst);
         vex_printf(", ");
         ppARMRI84(i->ARMin.Mov.src);
         return;
      case ARMin_Imm32:
         vex_printf("imm   ");
         ppHRegARM(i->ARMin.Imm32.dst);
         vex_printf(", 0x%x", i->ARMin.Imm32.imm32);
         return;
      case ARMin_LdSt32:
         if (i->ARMin.LdSt32.isLoad) {
            vex_printf("ldr   ");
            ppHRegARM(i->ARMin.LdSt32.rD);
            vex_printf(", ");
            ppARMAMode1(i->ARMin.LdSt32.amode);
         } else {
            vex_printf("str   ");
            ppARMAMode1(i->ARMin.LdSt32.amode);
            vex_printf(", ");
            ppHRegARM(i->ARMin.LdSt32.rD);
         }
         return;
      case ARMin_LdSt16:
         if (i->ARMin.LdSt16.isLoad) {
            vex_printf("%s", i->ARMin.LdSt16.signedLoad 
                                ? "ldrsh " : "ldrh  " );
            ppHRegARM(i->ARMin.LdSt16.rD);
            vex_printf(", ");
            ppARMAMode2(i->ARMin.LdSt16.amode);
         } else {
            vex_printf("strh  ");
            ppARMAMode2(i->ARMin.LdSt16.amode);
            vex_printf(", ");
            ppHRegARM(i->ARMin.LdSt16.rD);
         }
         return;
      case ARMin_LdSt8U:
         if (i->ARMin.LdSt8U.isLoad) {
            vex_printf("ldrb  ");
            ppHRegARM(i->ARMin.LdSt8U.rD);
            vex_printf(", ");
            ppARMAMode1(i->ARMin.LdSt8U.amode);
         } else {
            vex_printf("strb  ");
            ppARMAMode1(i->ARMin.LdSt8U.amode);
            vex_printf(", ");
            ppHRegARM(i->ARMin.LdSt8U.rD);
         }
         return;
      case ARMin_Ld8S:
         goto unhandled;
      case ARMin_Goto:
         if (i->ARMin.Goto.cond != ARMcc_AL) {
            vex_printf("if (%%cpsr.%s) { ",
                       showARMCondCode(i->ARMin.Goto.cond));
         } else {
            vex_printf("if (1) { ");
         }
         if (i->ARMin.Goto.jk != Ijk_Boring
             && i->ARMin.Goto.jk != Ijk_Call
             && i->ARMin.Goto.jk != Ijk_Ret) {
            vex_printf("mov r8, $");
            ppIRJumpKind(i->ARMin.Goto.jk);
            vex_printf(" ; ");
         }
         vex_printf("mov r0, ");
         ppHRegARM(i->ARMin.Goto.gnext);
         vex_printf(" ; bx r14");
         if (i->ARMin.Goto.cond != ARMcc_AL) {
            vex_printf(" }");
         } else {
            vex_printf(" }");
         }
         return;
      case ARMin_CMov:
         vex_printf("mov%s ", showARMCondCode(i->ARMin.CMov.cond));
         ppHRegARM(i->ARMin.CMov.dst);
         vex_printf(", ");
         ppARMRI84(i->ARMin.CMov.src);
         return;
      case ARMin_Call:
         vex_printf("call%s  ",
                    i->ARMin.Call.cond==ARMcc_AL
                       ? "" : showARMCondCode(i->ARMin.Call.cond));
         vex_printf("0x%lx [nArgRegs=%d]",
                    i->ARMin.Call.target, i->ARMin.Call.nArgRegs);
         return;
      case ARMin_Mul:
         vex_printf("%-5s ", showARMMulOp(i->ARMin.Mul.op));
         if (i->ARMin.Mul.op == ARMmul_PLAIN) {
            vex_printf("r0, r2, r3");
         } else {
            vex_printf("r1:r0, r2, r3");
         }
         return;
      case ARMin_LdrEX: {
         HChar* sz = "";
         switch (i->ARMin.LdrEX.szB) {
            case 1: sz = "b"; break; case 2: sz = "h"; break;
            case 8: sz = "d"; break; case 4: break;
            default: vassert(0);
         }      
         vex_printf("ldrex%s %sr2, [r4]",
                    sz, i->ARMin.LdrEX.szB == 8 ? "r3:" : "");
         return;
      }
      case ARMin_StrEX: {
         HChar* sz = "";
         switch (i->ARMin.StrEX.szB) {
            case 1: sz = "b"; break; case 2: sz = "h"; break;
            case 8: sz = "d"; break; case 4: break;
            default: vassert(0);
         }      
         vex_printf("strex%s r0, %sr2, [r4]",
                    sz, i->ARMin.StrEX.szB == 8 ? "r3:" : "");
         return;
      }
      case ARMin_VLdStD:
         if (i->ARMin.VLdStD.isLoad) {
            vex_printf("fldd  ");
            ppHRegARM(i->ARMin.VLdStD.dD);
            vex_printf(", ");
            ppARMAModeV(i->ARMin.VLdStD.amode);
         } else {
            vex_printf("fstd  ");
            ppARMAModeV(i->ARMin.VLdStD.amode);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VLdStD.dD);
         }
         return;
      case ARMin_VLdStS:
         if (i->ARMin.VLdStS.isLoad) {
            vex_printf("flds  ");
            ppHRegARM(i->ARMin.VLdStS.fD);
            vex_printf(", ");
            ppARMAModeV(i->ARMin.VLdStS.amode);
         } else {
            vex_printf("fsts  ");
            ppARMAModeV(i->ARMin.VLdStS.amode);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VLdStS.fD);
         }
         return;
      case ARMin_VAluD:
         vex_printf("f%-3sd ", showARMVfpOp(i->ARMin.VAluD.op));
         ppHRegARM(i->ARMin.VAluD.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VAluD.argL);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VAluD.argR);
         return;
      case ARMin_VAluS:
         vex_printf("f%-3ss ", showARMVfpOp(i->ARMin.VAluS.op));
         ppHRegARM(i->ARMin.VAluS.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VAluS.argL);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VAluS.argR);
         return;
      case ARMin_VUnaryD:
         vex_printf("f%-3sd ", showARMVfpUnaryOp(i->ARMin.VUnaryD.op));
         ppHRegARM(i->ARMin.VUnaryD.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VUnaryD.src);
         return;
      case ARMin_VUnaryS:
         vex_printf("f%-3ss ", showARMVfpUnaryOp(i->ARMin.VUnaryS.op));
         ppHRegARM(i->ARMin.VUnaryS.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VUnaryS.src);
         return;
      case ARMin_VCmpD:
         vex_printf("fcmpd ");
         ppHRegARM(i->ARMin.VCmpD.argL);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VCmpD.argR);
         vex_printf(" ; fmstat");
         return;
      case ARMin_VCMovD:
         vex_printf("fcpyd%s ", showARMCondCode(i->ARMin.VCMovD.cond));
         ppHRegARM(i->ARMin.VCMovD.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VCMovD.src);
         return;
      case ARMin_VCMovS:
         vex_printf("fcpys%s ", showARMCondCode(i->ARMin.VCMovS.cond));
         ppHRegARM(i->ARMin.VCMovS.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VCMovS.src);
         return;
      case ARMin_VCvtSD:
         vex_printf("fcvt%s ", i->ARMin.VCvtSD.sToD ? "ds" : "sd");
         ppHRegARM(i->ARMin.VCvtSD.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VCvtSD.src);
         return;
      case ARMin_VXferD:
         vex_printf("vmov  ");
         if (i->ARMin.VXferD.toD) {
            ppHRegARM(i->ARMin.VXferD.dD);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferD.rLo);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferD.rHi);
         } else {
            ppHRegARM(i->ARMin.VXferD.rLo);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferD.rHi);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferD.dD);
         }
         return;
      case ARMin_VXferS:
         vex_printf("vmov  ");
         if (i->ARMin.VXferS.toS) {
            ppHRegARM(i->ARMin.VXferS.fD);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferS.rLo);
         } else {
            ppHRegARM(i->ARMin.VXferS.rLo);
            vex_printf(", ");
            ppHRegARM(i->ARMin.VXferS.fD);
         }
         return;
      case ARMin_VCvtID: {
         HChar* nm = "?";
         if (i->ARMin.VCvtID.iToD) {
            nm = i->ARMin.VCvtID.syned ? "fsitod" : "fuitod";
         } else {
            nm = i->ARMin.VCvtID.syned ? "ftosid" : "ftouid";
         }
         vex_printf("%s ", nm);
         ppHRegARM(i->ARMin.VCvtID.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.VCvtID.src);
         return;
      }
      case ARMin_FPSCR:
         if (i->ARMin.FPSCR.toFPSCR) {
            vex_printf("fmxr  fpscr, ");
            ppHRegARM(i->ARMin.FPSCR.iReg);
         } else {
            vex_printf("fmrx  ");
            ppHRegARM(i->ARMin.FPSCR.iReg);
            vex_printf(", fpscr");
         }
         return;
      case ARMin_MFence:
         vex_printf("mfence (mcr 15,0,r0,c7,c10,4; 15,0,r0,c7,c10,5; "
                    "15,0,r0,c7,c5,4)");
         return;
      case ARMin_NLdStQ:
         if (i->ARMin.NLdStQ.isLoad)
            vex_printf("vld1.32 {");
         else
            vex_printf("vst1.32 {");
         ppHRegARM(i->ARMin.NLdStQ.dQ);
         vex_printf("} ");
         ppARMAModeN(i->ARMin.NLdStQ.amode);
         return;
      case ARMin_NLdStD:
         if (i->ARMin.NLdStD.isLoad)
            vex_printf("vld1.32 {");
         else
            vex_printf("vst1.32 {");
         ppHRegARM(i->ARMin.NLdStD.dD);
         vex_printf("} ");
         ppARMAModeN(i->ARMin.NLdStD.amode);
         return;
      case ARMin_NUnary:
         vex_printf("%s%s%s  ",
                    showARMNeonUnOp(i->ARMin.NUnary.op),
                    showARMNeonUnOpDataType(i->ARMin.NUnary.op),
                    showARMNeonDataSize(i));
         ppHRegARM(i->ARMin.NUnary.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NUnary.src);
         if (i->ARMin.NUnary.op == ARMneon_EQZ)
            vex_printf(", #0");
         if (i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedS ||
             i->ARMin.NUnary.op == ARMneon_VCVTFtoFixedU ||
             i->ARMin.NUnary.op == ARMneon_VCVTFixedStoF ||
             i->ARMin.NUnary.op == ARMneon_VCVTFixedUtoF) {
            vex_printf(", #%d", i->ARMin.NUnary.size);
         }
         if (i->ARMin.NUnary.op == ARMneon_VQSHLNSS ||
             i->ARMin.NUnary.op == ARMneon_VQSHLNUU ||
             i->ARMin.NUnary.op == ARMneon_VQSHLNUS) {
            UInt size;
            size = i->ARMin.NUnary.size;
            if (size & 0x40) {
               vex_printf(", #%d", size - 64);
            } else if (size & 0x20) {
               vex_printf(", #%d", size - 32);
            } else if (size & 0x10) {
               vex_printf(", #%d", size - 16);
            } else if (size & 0x08) {
               vex_printf(", #%d", size - 8);
            }
         }
         return;
      case ARMin_NUnaryS:
         vex_printf("%s%s%s  ",
                    showARMNeonUnOp(i->ARMin.NUnary.op),
                    showARMNeonUnOpDataType(i->ARMin.NUnary.op),
                    showARMNeonDataSize(i));
         ppARMNRS(i->ARMin.NUnaryS.dst);
         vex_printf(", ");
         ppARMNRS(i->ARMin.NUnaryS.src);
         return;
      case ARMin_NShift:
         vex_printf("%s%s%s  ",
                    showARMNeonShiftOp(i->ARMin.NShift.op),
                    showARMNeonShiftOpDataType(i->ARMin.NShift.op),
                    showARMNeonDataSize(i));
         ppHRegARM(i->ARMin.NShift.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NShift.argL);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NShift.argR);
         return;
      case ARMin_NDual:
         vex_printf("%s%s%s  ",
                    showARMNeonDualOp(i->ARMin.NDual.op),
                    showARMNeonDualOpDataType(i->ARMin.NDual.op),
                    showARMNeonDataSize(i));
         ppHRegARM(i->ARMin.NDual.arg1);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NDual.arg2);
         return;
      case ARMin_NBinary:
         vex_printf("%s%s%s",
                    showARMNeonBinOp(i->ARMin.NBinary.op),
                    showARMNeonBinOpDataType(i->ARMin.NBinary.op),
                    showARMNeonDataSize(i));
         vex_printf("  ");
         ppHRegARM(i->ARMin.NBinary.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NBinary.argL);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NBinary.argR);
         return;
      case ARMin_NeonImm:
         vex_printf("vmov  ");
         ppHRegARM(i->ARMin.NeonImm.dst);
         vex_printf(", ");
         ppARMNImm(i->ARMin.NeonImm.imm);
         return;
      case ARMin_NCMovQ:
         vex_printf("vmov%s ", showARMCondCode(i->ARMin.NCMovQ.cond));
         ppHRegARM(i->ARMin.NCMovQ.dst);
         vex_printf(", ");
         ppHRegARM(i->ARMin.NCMovQ.src);
         return;
      case ARMin_Add32:
         vex_printf("add32 ");
         ppHRegARM(i->ARMin.Add32.rD);
         vex_printf(", ");
         ppHRegARM(i->ARMin.Add32.rN);
         vex_printf(", ");
         vex_printf("%d", i->ARMin.Add32.imm32);
         return;
      default:
      unhandled:
         vex_printf("ppARMInstr: unhandled case (tag %d)", (Int)i->tag);
         vpanic("ppARMInstr(1)");
         return;
   }
}


/* --------- Helpers for register allocation. --------- */

void getRegUsage_ARMInstr ( HRegUsage* u, ARMInstr* i, Bool mode64 )
{
   vassert(mode64 == False);
   initHRegUsage(u);
   switch (i->tag) {
      case ARMin_Alu:
         addHRegUse(u, HRmWrite, i->ARMin.Alu.dst);
         addHRegUse(u, HRmRead, i->ARMin.Alu.argL);
         addRegUsage_ARMRI84(u, i->ARMin.Alu.argR);
         return;
      case ARMin_Shift:
         addHRegUse(u, HRmWrite, i->ARMin.Shift.dst);
         addHRegUse(u, HRmRead, i->ARMin.Shift.argL);
         addRegUsage_ARMRI5(u, i->ARMin.Shift.argR);
         return;
      case ARMin_Unary:
         addHRegUse(u, HRmWrite, i->ARMin.Unary.dst);
         addHRegUse(u, HRmRead, i->ARMin.Unary.src);
         return;
      case ARMin_CmpOrTst:
         addHRegUse(u, HRmRead, i->ARMin.CmpOrTst.argL);
         addRegUsage_ARMRI84(u, i->ARMin.CmpOrTst.argR);
         return;
      case ARMin_Mov:
         addHRegUse(u, HRmWrite, i->ARMin.Mov.dst);
         addRegUsage_ARMRI84(u, i->ARMin.Mov.src);
         return;
      case ARMin_Imm32:
         addHRegUse(u, HRmWrite, i->ARMin.Imm32.dst);
         return;
      case ARMin_LdSt32:
         addRegUsage_ARMAMode1(u, i->ARMin.LdSt32.amode);
         if (i->ARMin.LdSt32.isLoad) {
            addHRegUse(u, HRmWrite, i->ARMin.LdSt32.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARMin.LdSt32.rD);
         }
         return;
      case ARMin_LdSt16:
         addRegUsage_ARMAMode2(u, i->ARMin.LdSt16.amode);
         if (i->ARMin.LdSt16.isLoad) {
            addHRegUse(u, HRmWrite, i->ARMin.LdSt16.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARMin.LdSt16.rD);
         }
         return;
      case ARMin_LdSt8U:
         addRegUsage_ARMAMode1(u, i->ARMin.LdSt8U.amode);
         if (i->ARMin.LdSt8U.isLoad) {
            addHRegUse(u, HRmWrite, i->ARMin.LdSt8U.rD);
         } else {
            addHRegUse(u, HRmRead, i->ARMin.LdSt8U.rD);
         }
         return;
      case ARMin_Ld8S:
         goto unhandled;
      case ARMin_Goto:
         /* reads the reg holding the next guest addr */
         addHRegUse(u, HRmRead, i->ARMin.Goto.gnext);
         /* writes it to the standard integer return register */
         addHRegUse(u, HRmWrite, hregARM_R0());
         /* possibly messes with the baseblock pointer */
         if (i->ARMin.Goto.jk != Ijk_Boring
             && i->ARMin.Goto.jk != Ijk_Call
             && i->ARMin.Goto.jk != Ijk_Ret)
            /* note, this is irrelevant since r8 is not actually
               available to the allocator.  But still .. */
            addHRegUse(u, HRmWrite, hregARM_R8());
         return;
      case ARMin_CMov:
         addHRegUse(u, HRmWrite, i->ARMin.CMov.dst);
         addHRegUse(u, HRmRead,  i->ARMin.CMov.dst);
         addRegUsage_ARMRI84(u, i->ARMin.CMov.src);
         return;
      case ARMin_Call:
         /* logic and comments copied/modified from x86 back end */
         /* This is a bit subtle. */
         /* First off, claim it trashes all the caller-saved regs
            which fall within the register allocator's jurisdiction.
            These I believe to be r0,1,2,3.  If it turns out that r9
            is also caller-saved, then we'll have to add that here
            too. */
         addHRegUse(u, HRmWrite, hregARM_R0());
         addHRegUse(u, HRmWrite, hregARM_R1());
         addHRegUse(u, HRmWrite, hregARM_R2());
         addHRegUse(u, HRmWrite, hregARM_R3());
         /* Now we have to state any parameter-carrying registers
            which might be read.  This depends on nArgRegs. */
         switch (i->ARMin.Call.nArgRegs) {
            case 4: addHRegUse(u, HRmRead, hregARM_R3()); /*fallthru*/
            case 3: addHRegUse(u, HRmRead, hregARM_R2()); /*fallthru*/
            case 2: addHRegUse(u, HRmRead, hregARM_R1()); /*fallthru*/
            case 1: addHRegUse(u, HRmRead, hregARM_R0()); break;
            case 0: break;
            default: vpanic("getRegUsage_ARM:Call:regparms");
         }
         /* Finally, there is the issue that the insn trashes a
            register because the literal target address has to be
            loaded into a register.  Fortunately, for the nArgRegs=
            0/1/2/3 case, we can use r0, r1, r2 or r3 respectively, so
            this does not cause any further damage.  For the
            nArgRegs=4 case, we'll have to choose another register
            arbitrarily since all the caller saved regs are used for
            parameters, and so we might as well choose r11.
            */
         if (i->ARMin.Call.nArgRegs == 4)
            addHRegUse(u, HRmWrite, hregARM_R11());
         /* Upshot of this is that the assembler really must observe
            the here-stated convention of which register to use as an
            address temporary, depending on nArgRegs: 0==r0,
            1==r1, 2==r2, 3==r3, 4==r11 */
         return;
      case ARMin_Mul:
         addHRegUse(u, HRmRead, hregARM_R2());
         addHRegUse(u, HRmRead, hregARM_R3());
         addHRegUse(u, HRmWrite, hregARM_R0());
         if (i->ARMin.Mul.op != ARMmul_PLAIN)
            addHRegUse(u, HRmWrite, hregARM_R1());
         return;
      case ARMin_LdrEX:
         addHRegUse(u, HRmRead, hregARM_R4());
         addHRegUse(u, HRmWrite, hregARM_R2());
         if (i->ARMin.LdrEX.szB == 8)
            addHRegUse(u, HRmWrite, hregARM_R3());
         return;
      case ARMin_StrEX:
         addHRegUse(u, HRmRead, hregARM_R4());
         addHRegUse(u, HRmWrite, hregARM_R0());
         addHRegUse(u, HRmRead, hregARM_R2());
         if (i->ARMin.StrEX.szB == 8)
            addHRegUse(u, HRmRead, hregARM_R3());
         return;
      case ARMin_VLdStD:
         addRegUsage_ARMAModeV(u, i->ARMin.VLdStD.amode);
         if (i->ARMin.VLdStD.isLoad) {
            addHRegUse(u, HRmWrite, i->ARMin.VLdStD.dD);
         } else {
            addHRegUse(u, HRmRead, i->ARMin.VLdStD.dD);
         }
         return;
      case ARMin_VLdStS:
         addRegUsage_ARMAModeV(u, i->ARMin.VLdStS.amode);
         if (i->ARMin.VLdStS.isLoad) {
            addHRegUse(u, HRmWrite, i->ARMin.VLdStS.fD);
         } else {
            addHRegUse(u, HRmRead, i->ARMin.VLdStS.fD);
         }
         return;
      case ARMin_VAluD:
         addHRegUse(u, HRmWrite, i->ARMin.VAluD.dst);
         addHRegUse(u, HRmRead, i->ARMin.VAluD.argL);
         addHRegUse(u, HRmRead, i->ARMin.VAluD.argR);
         return;
      case ARMin_VAluS:
         addHRegUse(u, HRmWrite, i->ARMin.VAluS.dst);
         addHRegUse(u, HRmRead, i->ARMin.VAluS.argL);
         addHRegUse(u, HRmRead, i->ARMin.VAluS.argR);
         return;
      case ARMin_VUnaryD:
         addHRegUse(u, HRmWrite, i->ARMin.VUnaryD.dst);
         addHRegUse(u, HRmRead, i->ARMin.VUnaryD.src);
         return;
      case ARMin_VUnaryS:
         addHRegUse(u, HRmWrite, i->ARMin.VUnaryS.dst);
         addHRegUse(u, HRmRead, i->ARMin.VUnaryS.src);
         return;
      case ARMin_VCmpD:
         addHRegUse(u, HRmRead, i->ARMin.VCmpD.argL);
         addHRegUse(u, HRmRead, i->ARMin.VCmpD.argR);
         return;
      case ARMin_VCMovD:
         addHRegUse(u, HRmWrite, i->ARMin.VCMovD.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCMovD.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCMovD.src);
         return;
      case ARMin_VCMovS:
         addHRegUse(u, HRmWrite, i->ARMin.VCMovS.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCMovS.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCMovS.src);
         return;
      case ARMin_VCvtSD:
         addHRegUse(u, HRmWrite, i->ARMin.VCvtSD.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCvtSD.src);
         return;
      case ARMin_VXferD:
         if (i->ARMin.VXferD.toD) {
            addHRegUse(u, HRmWrite, i->ARMin.VXferD.dD);
            addHRegUse(u, HRmRead,  i->ARMin.VXferD.rHi);
            addHRegUse(u, HRmRead,  i->ARMin.VXferD.rLo);
         } else {
            addHRegUse(u, HRmRead,  i->ARMin.VXferD.dD);
            addHRegUse(u, HRmWrite, i->ARMin.VXferD.rHi);
            addHRegUse(u, HRmWrite, i->ARMin.VXferD.rLo);
         }
         return;
      case ARMin_VXferS:
         if (i->ARMin.VXferS.toS) {
            addHRegUse(u, HRmWrite, i->ARMin.VXferS.fD);
            addHRegUse(u, HRmRead,  i->ARMin.VXferS.rLo);
         } else {
            addHRegUse(u, HRmRead,  i->ARMin.VXferS.fD);
            addHRegUse(u, HRmWrite, i->ARMin.VXferS.rLo);
         }
         return;
      case ARMin_VCvtID:
         addHRegUse(u, HRmWrite, i->ARMin.VCvtID.dst);
         addHRegUse(u, HRmRead,  i->ARMin.VCvtID.src);
         return;
      case ARMin_FPSCR:
         if (i->ARMin.FPSCR.toFPSCR)
            addHRegUse(u, HRmRead, i->ARMin.FPSCR.iReg);
         else
            addHRegUse(u, HRmWrite, i->ARMin.FPSCR.iReg);
         return;
      case ARMin_MFence:
         return;
      case ARMin_NLdStQ:
         if (i->ARMin.NLdStQ.isLoad)
            addHRegUse(u, HRmWrite, i->ARMin.NLdStQ.dQ);
         else
            addHRegUse(u, HRmRead, i->ARMin.NLdStQ.dQ);
         addRegUsage_ARMAModeN(u, i->ARMin.NLdStQ.amode);
         return;
      case ARMin_NLdStD:
         if (i->ARMin.NLdStD.isLoad)
            addHRegUse(u, HRmWrite, i->ARMin.NLdStD.dD);
         else
            addHRegUse(u, HRmRead, i->ARMin.NLdStD.dD);
         addRegUsage_ARMAModeN(u, i->ARMin.NLdStD.amode);
         return;
      case ARMin_NUnary:
         addHRegUse(u, HRmWrite, i->ARMin.NUnary.dst);
         addHRegUse(u, HRmRead, i->ARMin.NUnary.src);
         return;
      case ARMin_NUnaryS:
         addHRegUse(u, HRmWrite, i->ARMin.NUnaryS.dst->reg);
         addHRegUse(u, HRmRead, i->ARMin.NUnaryS.src->reg);
         return;
      case ARMin_NShift:
         addHRegUse(u, HRmWrite, i->ARMin.NShift.dst);
         addHRegUse(u, HRmRead, i->ARMin.NShift.argL);
         addHRegUse(u, HRmRead, i->ARMin.NShift.argR);
         return;
      case ARMin_NDual:
         addHRegUse(u, HRmWrite, i->ARMin.NDual.arg1);
         addHRegUse(u, HRmWrite, i->ARMin.NDual.arg2);
         addHRegUse(u, HRmRead, i->ARMin.NDual.arg1);
         addHRegUse(u, HRmRead, i->ARMin.NDual.arg2);
         return;
      case ARMin_NBinary:
         addHRegUse(u, HRmWrite, i->ARMin.NBinary.dst);
         /* TODO: sometimes dst is also being read! */
         // XXX fix this
         addHRegUse(u, HRmRead, i->ARMin.NBinary.argL);
         addHRegUse(u, HRmRead, i->ARMin.NBinary.argR);
         return;
      case ARMin_NeonImm:
         addHRegUse(u, HRmWrite, i->ARMin.NeonImm.dst);
         return;
      case ARMin_NCMovQ:
         addHRegUse(u, HRmWrite, i->ARMin.NCMovQ.dst);
         addHRegUse(u, HRmRead,  i->ARMin.NCMovQ.dst);
         addHRegUse(u, HRmRead,  i->ARMin.NCMovQ.src);
         return;
      case ARMin_Add32:
         addHRegUse(u, HRmWrite, i->ARMin.Add32.rD);
         addHRegUse(u, HRmRead, i->ARMin.Add32.rN);
         return;
      unhandled:
      default:
         ppARMInstr(i);
         vpanic("getRegUsage_ARMInstr");
   }
}


void mapRegs_ARMInstr ( HRegRemap* m, ARMInstr* i, Bool mode64 )
{
   vassert(mode64 == False);
   switch (i->tag) {
      case ARMin_Alu:
         i->ARMin.Alu.dst = lookupHRegRemap(m, i->ARMin.Alu.dst);
         i->ARMin.Alu.argL = lookupHRegRemap(m, i->ARMin.Alu.argL);
         mapRegs_ARMRI84(m, i->ARMin.Alu.argR);
         return;
      case ARMin_Shift:
         i->ARMin.Shift.dst = lookupHRegRemap(m, i->ARMin.Shift.dst);
         i->ARMin.Shift.argL = lookupHRegRemap(m, i->ARMin.Shift.argL);
         mapRegs_ARMRI5(m, i->ARMin.Shift.argR);
         return;
      case ARMin_Unary:
         i->ARMin.Unary.dst = lookupHRegRemap(m, i->ARMin.Unary.dst);
         i->ARMin.Unary.src = lookupHRegRemap(m, i->ARMin.Unary.src);
         return;
      case ARMin_CmpOrTst:
         i->ARMin.CmpOrTst.argL = lookupHRegRemap(m, i->ARMin.CmpOrTst.argL);
         mapRegs_ARMRI84(m, i->ARMin.CmpOrTst.argR);
         return;
      case ARMin_Mov:
         i->ARMin.Mov.dst = lookupHRegRemap(m, i->ARMin.Mov.dst);
         mapRegs_ARMRI84(m, i->ARMin.Mov.src);
         return;
      case ARMin_Imm32:
         i->ARMin.Imm32.dst = lookupHRegRemap(m, i->ARMin.Imm32.dst);
         return;
      case ARMin_LdSt32:
         i->ARMin.LdSt32.rD = lookupHRegRemap(m, i->ARMin.LdSt32.rD);
         mapRegs_ARMAMode1(m, i->ARMin.LdSt32.amode);
         return;
      case ARMin_LdSt16:
         i->ARMin.LdSt16.rD = lookupHRegRemap(m, i->ARMin.LdSt16.rD);
         mapRegs_ARMAMode2(m, i->ARMin.LdSt16.amode);
         return;
      case ARMin_LdSt8U:
         i->ARMin.LdSt8U.rD = lookupHRegRemap(m, i->ARMin.LdSt8U.rD);
         mapRegs_ARMAMode1(m, i->ARMin.LdSt8U.amode);
         return;
      case ARMin_Ld8S:
         goto unhandled;
      case ARMin_Goto:
         i->ARMin.Goto.gnext = lookupHRegRemap(m, i->ARMin.Goto.gnext);
         return;
      case ARMin_CMov:
         i->ARMin.CMov.dst = lookupHRegRemap(m, i->ARMin.CMov.dst);
         mapRegs_ARMRI84(m, i->ARMin.CMov.src);
         return;
      case ARMin_Call:
         return;
      case ARMin_Mul:
         return;
      case ARMin_LdrEX:
         return;
      case ARMin_StrEX:
         return;
      case ARMin_VLdStD:
         i->ARMin.VLdStD.dD = lookupHRegRemap(m, i->ARMin.VLdStD.dD);
         mapRegs_ARMAModeV(m, i->ARMin.VLdStD.amode);
         return;
      case ARMin_VLdStS:
         i->ARMin.VLdStS.fD = lookupHRegRemap(m, i->ARMin.VLdStS.fD);
         mapRegs_ARMAModeV(m, i->ARMin.VLdStS.amode);
         return;
      case ARMin_VAluD:
         i->ARMin.VAluD.dst  = lookupHRegRemap(m, i->ARMin.VAluD.dst);
         i->ARMin.VAluD.argL = lookupHRegRemap(m, i->ARMin.VAluD.argL);
         i->ARMin.VAluD.argR = lookupHRegRemap(m, i->ARMin.VAluD.argR);
         return;
      case ARMin_VAluS:
         i->ARMin.VAluS.dst  = lookupHRegRemap(m, i->ARMin.VAluS.dst);
         i->ARMin.VAluS.argL = lookupHRegRemap(m, i->ARMin.VAluS.argL);
         i->ARMin.VAluS.argR = lookupHRegRemap(m, i->ARMin.VAluS.argR);
         return;
      case ARMin_VUnaryD:
         i->ARMin.VUnaryD.dst = lookupHRegRemap(m, i->ARMin.VUnaryD.dst);
         i->ARMin.VUnaryD.src = lookupHRegRemap(m, i->ARMin.VUnaryD.src);
         return;
      case ARMin_VUnaryS:
         i->ARMin.VUnaryS.dst = lookupHRegRemap(m, i->ARMin.VUnaryS.dst);
         i->ARMin.VUnaryS.src = lookupHRegRemap(m, i->ARMin.VUnaryS.src);
         return;
      case ARMin_VCmpD:
         i->ARMin.VCmpD.argL = lookupHRegRemap(m, i->ARMin.VCmpD.argL);
         i->ARMin.VCmpD.argR = lookupHRegRemap(m, i->ARMin.VCmpD.argR);
         return;
      case ARMin_VCMovD:
         i->ARMin.VCMovD.dst = lookupHRegRemap(m, i->ARMin.VCMovD.dst);
         i->ARMin.VCMovD.src = lookupHRegRemap(m, i->ARMin.VCMovD.src);
         return;
      case ARMin_VCMovS:
         i->ARMin.VCMovS.dst = lookupHRegRemap(m, i->ARMin.VCMovS.dst);
         i->ARMin.VCMovS.src = lookupHRegRemap(m, i->ARMin.VCMovS.src);
         return;
      case ARMin_VCvtSD:
         i->ARMin.VCvtSD.dst = lookupHRegRemap(m, i->ARMin.VCvtSD.dst);
         i->ARMin.VCvtSD.src = lookupHRegRemap(m, i->ARMin.VCvtSD.src);
         return;
      case ARMin_VXferD:
         i->ARMin.VXferD.dD  = lookupHRegRemap(m, i->ARMin.VXferD.dD);
         i->ARMin.VXferD.rHi = lookupHRegRemap(m, i->ARMin.VXferD.rHi);
         i->ARMin.VXferD.rLo = lookupHRegRemap(m, i->ARMin.VXferD.rLo);
         return;
      case ARMin_VXferS:
         i->ARMin.VXferS.fD  = lookupHRegRemap(m, i->ARMin.VXferS.fD);
         i->ARMin.VXferS.rLo = lookupHRegRemap(m, i->ARMin.VXferS.rLo);
         return;
      case ARMin_VCvtID:
         i->ARMin.VCvtID.dst = lookupHRegRemap(m, i->ARMin.VCvtID.dst);
         i->ARMin.VCvtID.src = lookupHRegRemap(m, i->ARMin.VCvtID.src);
         return;
      case ARMin_FPSCR:
         i->ARMin.FPSCR.iReg = lookupHRegRemap(m, i->ARMin.FPSCR.iReg);
         return;
      case ARMin_MFence:
         return;
      case ARMin_NLdStQ:
         i->ARMin.NLdStQ.dQ = lookupHRegRemap(m, i->ARMin.NLdStQ.dQ);
         mapRegs_ARMAModeN(m, i->ARMin.NLdStQ.amode);
         return;
      case ARMin_NLdStD:
         i->ARMin.NLdStD.dD = lookupHRegRemap(m, i->ARMin.NLdStD.dD);
         mapRegs_ARMAModeN(m, i->ARMin.NLdStD.amode);
         return;
      case ARMin_NUnary:
         i->ARMin.NUnary.src = lookupHRegRemap(m, i->ARMin.NUnary.src);
         i->ARMin.NUnary.dst = lookupHRegRemap(m, i->ARMin.NUnary.dst);
         return;
      case ARMin_NUnaryS:
         i->ARMin.NUnaryS.src->reg
            = lookupHRegRemap(m, i->ARMin.NUnaryS.src->reg);
         i->ARMin.NUnaryS.dst->reg
            = lookupHRegRemap(m, i->ARMin.NUnaryS.dst->reg);
         return;
      case ARMin_NShift:
         i->ARMin.NShift.dst = lookupHRegRemap(m, i->ARMin.NShift.dst);
         i->ARMin.NShift.argL = lookupHRegRemap(m, i->ARMin.NShift.argL);
         i->ARMin.NShift.argR = lookupHRegRemap(m, i->ARMin.NShift.argR);
         return;
      case ARMin_NDual:
         i->ARMin.NDual.arg1 = lookupHRegRemap(m, i->ARMin.NDual.arg1);
         i->ARMin.NDual.arg2 = lookupHRegRemap(m, i->ARMin.NDual.arg2);
         return;
      case ARMin_NBinary:
         i->ARMin.NBinary.argL = lookupHRegRemap(m, i->ARMin.NBinary.argL);
         i->ARMin.NBinary.argR = lookupHRegRemap(m, i->ARMin.NBinary.argR);
         i->ARMin.NBinary.dst  = lookupHRegRemap(m, i->ARMin.NBinary.dst);
         return;
      case ARMin_NeonImm:
         i->ARMin.NeonImm.dst = lookupHRegRemap(m, i->ARMin.NeonImm.dst);
         return;
      case ARMin_NCMovQ:
         i->ARMin.NCMovQ.dst = lookupHRegRemap(m, i->ARMin.NCMovQ.dst);
         i->ARMin.NCMovQ.src = lookupHRegRemap(m, i->ARMin.NCMovQ.src);
         return;
      case ARMin_Add32:
         i->ARMin.Add32.rD = lookupHRegRemap(m, i->ARMin.Add32.rD);
         i->ARMin.Add32.rN = lookupHRegRemap(m, i->ARMin.Add32.rN);
      unhandled:
      default:
         ppARMInstr(i);
         vpanic("mapRegs_ARMInstr");
   }
}

/* Figure out if i represents a reg-reg move, and if so assign the
   source and destination to *src and *dst.  If in doubt say No.  Used
   by the register allocator to do move coalescing. 
*/
Bool isMove_ARMInstr ( ARMInstr* i, HReg* src, HReg* dst )
{
   /* Moves between integer regs */
   switch (i->tag) {
      case ARMin_Mov:
         if (i->ARMin.Mov.src->tag == ARMri84_R) {
            *src = i->ARMin.Mov.src->ARMri84.R.reg;
            *dst = i->ARMin.Mov.dst;
            return True;
         }
         break;
      case ARMin_VUnaryD:
         if (i->ARMin.VUnaryD.op == ARMvfpu_COPY) {
            *src = i->ARMin.VUnaryD.src;
            *dst = i->ARMin.VUnaryD.dst;
            return True;
         }
         break;
      case ARMin_VUnaryS:
         if (i->ARMin.VUnaryS.op == ARMvfpu_COPY) {
            *src = i->ARMin.VUnaryS.src;
            *dst = i->ARMin.VUnaryS.dst;
            return True;
         }
         break;
      default:
         break;
   }

   // todo: float, vector moves
   return False;
}


/* Generate arm spill/reload instructions under the direction of the
   register allocator.  Note it's critical these don't write the
   condition codes. */

void genSpill_ARM ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                    HReg rreg, Int offsetB, Bool mode64 )
{
   HRegClass rclass;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == False);
   *i1 = *i2 = NULL;
   rclass = hregClass(rreg);
   switch (rclass) {
      case HRcInt32:
         vassert(offsetB <= 4095);
         *i1 = ARMInstr_LdSt32( False/*!isLoad*/, 
                                rreg, 
                                ARMAMode1_RI(hregARM_R8(), offsetB) );
         return;
      case HRcFlt32:
      case HRcFlt64: {
         HReg r8   = hregARM_R8();  /* baseblock */
         HReg r12  = hregARM_R12(); /* spill temp */
         HReg base = r8;
         vassert(0 == (offsetB & 3));
         if (offsetB >= 1024) {
            Int offsetKB = offsetB / 1024;
            /* r12 = r8 + (1024 * offsetKB) */
            *i1 = ARMInstr_Alu(ARMalu_ADD, r12, r8,
                               ARMRI84_I84(offsetKB, 11));
            offsetB -= (1024 * offsetKB);
            base = r12;
         }
         vassert(offsetB <= 1020);
         if (rclass == HRcFlt32) {
            *i2 = ARMInstr_VLdStS( False/*!isLoad*/,
                                   rreg,
                                   mkARMAModeV(base, offsetB) );
         } else {
            *i2 = ARMInstr_VLdStD( False/*!isLoad*/,
                                   rreg,
                                   mkARMAModeV(base, offsetB) );
         }
         return;
      }
      case HRcVec128: {
         HReg r8  = hregARM_R8();
         HReg r12 = hregARM_R12();
         *i1 = ARMInstr_Add32(r12, r8, offsetB);
         *i2 = ARMInstr_NLdStQ(False, rreg, mkARMAModeN_R(r12));
         return;
      }
      default:
         ppHRegClass(rclass);
         vpanic("genSpill_ARM: unimplemented regclass");
   }
}

void genReload_ARM ( /*OUT*/HInstr** i1, /*OUT*/HInstr** i2,
                     HReg rreg, Int offsetB, Bool mode64 )
{
   HRegClass rclass;
   vassert(offsetB >= 0);
   vassert(!hregIsVirtual(rreg));
   vassert(mode64 == False);
   *i1 = *i2 = NULL;
   rclass = hregClass(rreg);
   switch (rclass) {
      case HRcInt32:
         vassert(offsetB <= 4095);
         *i1 = ARMInstr_LdSt32( True/*isLoad*/, 
                                rreg, 
                                ARMAMode1_RI(hregARM_R8(), offsetB) );
         return;
      case HRcFlt32:
      case HRcFlt64: {
         HReg r8   = hregARM_R8();  /* baseblock */
         HReg r12  = hregARM_R12(); /* spill temp */
         HReg base = r8;
         vassert(0 == (offsetB & 3));
         if (offsetB >= 1024) {
            Int offsetKB = offsetB / 1024;
            /* r12 = r8 + (1024 * offsetKB) */
            *i1 = ARMInstr_Alu(ARMalu_ADD, r12, r8,
                               ARMRI84_I84(offsetKB, 11));
            offsetB -= (1024 * offsetKB);
            base = r12;
         }
         vassert(offsetB <= 1020);
         if (rclass == HRcFlt32) {
            *i2 = ARMInstr_VLdStS( True/*isLoad*/,
                                   rreg,
                                   mkARMAModeV(base, offsetB) );
         } else {
            *i2 = ARMInstr_VLdStD( True/*isLoad*/,
                                   rreg,
                                   mkARMAModeV(base, offsetB) );
         }
         return;
      }
      case HRcVec128: {
         HReg r8  = hregARM_R8();
         HReg r12 = hregARM_R12();
         *i1 = ARMInstr_Add32(r12, r8, offsetB);
         *i2 = ARMInstr_NLdStQ(True, rreg, mkARMAModeN_R(r12));
         return;
      }
      default:
         ppHRegClass(rclass);
         vpanic("genReload_ARM: unimplemented regclass");
   }
}


/* Emit an instruction into buf and return the number of bytes used.
   Note that buf is not the insn's final place, and therefore it is
   imperative to emit position-independent code. */

static inline UChar iregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcInt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return toUChar(n);
}

static inline UChar dregNo ( HReg r )
{
   UInt n;
   if (hregClass(r) != HRcFlt64)
      ppHRegClass(hregClass(r));
   vassert(hregClass(r) == HRcFlt64);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return toUChar(n);
}

static inline UChar fregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcFlt32);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 31);
   return toUChar(n);
}

static inline UChar qregNo ( HReg r )
{
   UInt n;
   vassert(hregClass(r) == HRcVec128);
   vassert(!hregIsVirtual(r));
   n = hregNumber(r);
   vassert(n <= 15);
   return toUChar(n);
}

#define BITS4(zzb3,zzb2,zzb1,zzb0) \
   (((zzb3) << 3) | ((zzb2) << 2) | ((zzb1) << 1) | (zzb0))
#define X0000  BITS4(0,0,0,0)
#define X0001  BITS4(0,0,0,1)
#define X0010  BITS4(0,0,1,0)
#define X0011  BITS4(0,0,1,1)
#define X0100  BITS4(0,1,0,0)
#define X0101  BITS4(0,1,0,1)
#define X0110  BITS4(0,1,1,0)
#define X0111  BITS4(0,1,1,1)
#define X1000  BITS4(1,0,0,0)
#define X1001  BITS4(1,0,0,1)
#define X1010  BITS4(1,0,1,0)
#define X1011  BITS4(1,0,1,1)
#define X1100  BITS4(1,1,0,0)
#define X1101  BITS4(1,1,0,1)
#define X1110  BITS4(1,1,1,0)
#define X1111  BITS4(1,1,1,1)

#define XXXXX___(zzx7,zzx6,zzx5,zzx4,zzx3) \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12))

#define XXXXXX__(zzx7,zzx6,zzx5,zzx4,zzx3,zzx2)        \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx2) & 0xF) <<  8))

#define XXXXX__X(zzx7,zzx6,zzx5,zzx4,zzx3,zzx0)        \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx0) & 0xF) <<  0))

#define XXX___XX(zzx7,zzx6,zzx5,zzx1,zzx0) \
  ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) | \
   (((zzx5) & 0xF) << 20) | (((zzx1) & 0xF) << 4) | \
   (((zzx0) & 0xF) << 0))

#define XXXXXXXX(zzx7,zzx6,zzx5,zzx4,zzx3,zzx2,zzx1,zzx0)  \
   ((((zzx7) & 0xF) << 28) | (((zzx6) & 0xF) << 24) |  \
    (((zzx5) & 0xF) << 20) | (((zzx4) & 0xF) << 16) |  \
    (((zzx3) & 0xF) << 12) | (((zzx2) & 0xF) <<  8) |  \
    (((zzx1) & 0xF) <<  4) | (((zzx0) & 0xF) <<  0))

/* Generate a skeletal insn that involves an a RI84 shifter operand.
   Returns a word which is all zeroes apart from bits 25 and 11..0,
   since it is those that encode the shifter operand (at least to the
   extent that we care about it.) */
static UInt skeletal_RI84 ( ARMRI84* ri )
{
   UInt instr;
   if (ri->tag == ARMri84_I84) {
      vassert(0 == (ri->ARMri84.I84.imm4 & ~0x0F));
      vassert(0 == (ri->ARMri84.I84.imm8 & ~0xFF));
      instr = 1 << 25;
      instr |= (ri->ARMri84.I84.imm4 << 8);
      instr |= ri->ARMri84.I84.imm8;
   } else {
      instr = 0 << 25;
      instr |= iregNo(ri->ARMri84.R.reg);
   }
   return instr;
}

/* Ditto for RI5.  Resulting word is zeroes apart from bit 4 and bits
   11..7. */
static UInt skeletal_RI5 ( ARMRI5* ri )
{
   UInt instr;
   if (ri->tag == ARMri5_I5) {
      UInt imm5 = ri->ARMri5.I5.imm5;
      vassert(imm5 >= 1 && imm5 <= 31);
      instr = 0 << 4;
      instr |= imm5 << 7;
   } else {
      instr = 1 << 4;
      instr |= iregNo(ri->ARMri5.R.reg) << 8;
   }
   return instr;
}


/* Get an immediate into a register, using only that 
   register.  (very lame..) */
static UInt* imm32_to_iregNo ( UInt* p, Int rD, UInt imm32 )
{
   UInt instr;
   vassert(rD >= 0 && rD <= 14); // r15 not good to mess with!
#if 0
   if (0 == (imm32 & ~0xFF)) {
      /* mov with a immediate shifter operand of (0, imm32) (??) */
      instr = XXXXXX__(X1110,X0011,X1010,X0000,rD,X0000);
      instr |= imm32;
      *p++ = instr;
   } else {
      // this is very bad; causes Dcache pollution
      // ldr  rD, [pc]
      instr = XXXXX___(X1110,X0101,X1001,X1111,rD);
      *p++ = instr;
      // b .+8
      instr = 0xEA000000;
      *p++ = instr;
      // .word imm32
      *p++ = imm32;
   }
#else
   if (VEX_ARM_ARCHLEVEL(arm_hwcaps) > 6) {
      /* Generate movw rD, #low16.  Then, if the high 16 are
         nonzero, generate movt rD, #high16. */
      UInt lo16 = imm32 & 0xFFFF;
      UInt hi16 = (imm32 >> 16) & 0xFFFF;
      instr = XXXXXXXX(0xE, 0x3, 0x0, (lo16 >> 12) & 0xF, rD,
                       (lo16 >> 8) & 0xF, (lo16 >> 4) & 0xF,
                       lo16 & 0xF);
      *p++ = instr;
      if (hi16 != 0) {
         instr = XXXXXXXX(0xE, 0x3, 0x4, (hi16 >> 12) & 0xF, rD,
                          (hi16 >> 8) & 0xF, (hi16 >> 4) & 0xF,
                          hi16 & 0xF);
         *p++ = instr;
      }
   } else {
      UInt imm, rot;
      UInt op = X1010;
      UInt rN = 0;
      if ((imm32 & 0xFF) || (imm32 == 0)) {
         imm = imm32 & 0xFF;
         rot = 0;
         instr = XXXXXXXX(0xE, 0x3, op, rN, rD, rot, imm >> 4, imm & 0xF);
         *p++ = instr;
         op = X1000;
         rN = rD;
      }
      if (imm32 & 0xFF000000) {
         imm = (imm32 >> 24) & 0xFF;
         rot = 4;
         instr = XXXXXXXX(0xE, 0x3, op, rN, rD, rot, imm >> 4, imm & 0xF);
         *p++ = instr;
         op = X1000;
         rN = rD;
      }
      if (imm32 & 0xFF0000) {
         imm = (imm32 >> 16) & 0xFF;
         rot = 8;
         instr = XXXXXXXX(0xE, 0x3, op, rN, rD, rot, imm >> 4, imm & 0xF);
         *p++ = instr;
         op = X1000;
         rN = rD;
      }
      if (imm32 & 0xFF00) {
         imm = (imm32 >> 8) & 0xFF;
         rot = 12;
         instr = XXXXXXXX(0xE, 0x3, op, rN, rD, rot, imm >> 4, imm & 0xF);
         *p++ = instr;
         op = X1000;
         rN = rD;
      }
   }
#endif
   return p;
}


Int emit_ARMInstr ( UChar* buf, Int nbuf, ARMInstr* i,
                    Bool mode64,
                    void* dispatch_unassisted, void* dispatch_assisted ) 
{
   UInt* p = (UInt*)buf;
   vassert(nbuf >= 32);
   vassert(mode64 == False);
   vassert(0 == (((HWord)buf) & 3));

   switch (i->tag) {
      case ARMin_Alu: {
         UInt     instr, subopc;
         UInt     rD   = iregNo(i->ARMin.Alu.dst);
         UInt     rN   = iregNo(i->ARMin.Alu.argL);
         ARMRI84* argR = i->ARMin.Alu.argR;
         switch (i->ARMin.Alu.op) {
            case ARMalu_ADDS: /* fallthru */
            case ARMalu_ADD:  subopc = X0100; break;
            case ARMalu_ADC:  subopc = X0101; break;
            case ARMalu_SUBS: /* fallthru */
            case ARMalu_SUB:  subopc = X0010; break;
            case ARMalu_SBC:  subopc = X0110; break;
            case ARMalu_AND:  subopc = X0000; break;
            case ARMalu_BIC:  subopc = X1110; break;
            case ARMalu_OR:   subopc = X1100; break;
            case ARMalu_XOR:  subopc = X0001; break;
            default: goto bad;
         }
         instr = skeletal_RI84(argR);
         instr |= XXXXX___(X1110, (1 & (subopc >> 3)),
                           (subopc << 1) & 0xF, rN, rD);
         if (i->ARMin.Alu.op == ARMalu_ADDS
             || i->ARMin.Alu.op == ARMalu_SUBS) {
            instr |= 1<<20;  /* set the S bit */
         }
         *p++ = instr;
         goto done;
      }
      case ARMin_Shift: {
         UInt    instr, subopc;
         HReg    rD   = iregNo(i->ARMin.Shift.dst);
         HReg    rM   = iregNo(i->ARMin.Shift.argL);
         ARMRI5* argR = i->ARMin.Shift.argR;
         switch (i->ARMin.Shift.op) {
            case ARMsh_SHL: subopc = X0000; break;
            case ARMsh_SHR: subopc = X0001; break;
            case ARMsh_SAR: subopc = X0010; break;
            default: goto bad;
         }
         instr = skeletal_RI5(argR);
         instr |= XXXXX__X(X1110,X0001,X1010,X0000,rD, /* _ _ */ rM);
         instr |= (subopc & 3) << 5;
         *p++ = instr;
         goto done;
      }
      case ARMin_Unary: {
         UInt instr;
         HReg rDst = iregNo(i->ARMin.Unary.dst);
         HReg rSrc = iregNo(i->ARMin.Unary.src);
         switch (i->ARMin.Unary.op) {
            case ARMun_CLZ:
               instr = XXXXXXXX(X1110,X0001,X0110,X1111,
                                rDst,X1111,X0001,rSrc);
               *p++ = instr;
               goto done;
            case ARMun_NEG: /* RSB rD,rS,#0 */
               instr = XXXXX___(X1110,0x2,0x6,rSrc,rDst);
               *p++ = instr;
               goto done;
            case ARMun_NOT: {
               UInt subopc = X1111; /* MVN */
               instr = rSrc;
               instr |= XXXXX___(X1110, (1 & (subopc >> 3)),
                                 (subopc << 1) & 0xF, 0, rDst);
               *p++ = instr;
               goto done;
            }
            default:
               break;
         }
         goto bad;
      }
      case ARMin_CmpOrTst: {
         UInt instr  = skeletal_RI84(i->ARMin.CmpOrTst.argR);
         UInt subopc = i->ARMin.CmpOrTst.isCmp ? X1010 : X1000;
         UInt SBZ    = 0;
         instr |= XXXXX___(X1110, (1 & (subopc >> 3)),
                           ((subopc << 1) & 0xF) | 1,
                           i->ARMin.CmpOrTst.argL, SBZ );
         *p++ = instr;
         goto done;
      }
      case ARMin_Mov: {
         UInt instr  = skeletal_RI84(i->ARMin.Mov.src);
         UInt subopc = X1101; /* MOV */
         UInt SBZ    = 0;
         instr |= XXXXX___(X1110, (1 & (subopc >> 3)),
                           (subopc << 1) & 0xF, SBZ, i->ARMin.Mov.dst);
         *p++ = instr;
         goto done;
      }
      case ARMin_Imm32: {
         p = imm32_to_iregNo( (UInt*)p, iregNo(i->ARMin.Imm32.dst),
                                        i->ARMin.Imm32.imm32 );
         goto done;
      }
      case ARMin_LdSt32:
      case ARMin_LdSt8U: {
         UInt       bL, bB;
         HReg       rD;
         ARMAMode1* am;
         if (i->tag == ARMin_LdSt32) {
            bB = 0;
            bL = i->ARMin.LdSt32.isLoad ? 1 : 0;
            am = i->ARMin.LdSt32.amode;
            rD = i->ARMin.LdSt32.rD;
         } else {
            bB = 1;
            bL = i->ARMin.LdSt8U.isLoad ? 1 : 0;
            am = i->ARMin.LdSt8U.amode;
            rD = i->ARMin.LdSt8U.rD;
         }
         if (am->tag == ARMam1_RI) {
            Int  simm12;
            UInt instr, bP;
            if (am->ARMam1.RI.simm13 < 0) {
               bP = 0;
               simm12 = -am->ARMam1.RI.simm13;
            } else {
               bP = 1;
               simm12 = am->ARMam1.RI.simm13;
            }
            vassert(simm12 >= 0 && simm12 <= 4095);
            instr = XXXXX___(X1110,X0101,BITS4(bP,bB,0,bL),
                             iregNo(am->ARMam1.RI.reg),
                             iregNo(rD));
            instr |= simm12;
            *p++ = instr;
            goto done;
         } else {
            // RR case
            goto bad;
         }
      }
      case ARMin_LdSt16: {
         HReg       rD = i->ARMin.LdSt16.rD;
         UInt       bS = i->ARMin.LdSt16.signedLoad ? 1 : 0;
         UInt       bL = i->ARMin.LdSt16.isLoad ? 1 : 0;
         ARMAMode2* am = i->ARMin.LdSt16.amode;
         if (am->tag == ARMam2_RI) {
            HReg rN = am->ARMam2.RI.reg;
            Int  simm8;
            UInt bP, imm8hi, imm8lo, instr;
            if (am->ARMam2.RI.simm9 < 0) {
               bP = 0;
               simm8 = -am->ARMam2.RI.simm9;
            } else {
               bP = 1;
               simm8 = am->ARMam2.RI.simm9;
            }
            vassert(simm8 >= 0 && simm8 <= 255);
            imm8hi = (simm8 >> 4) & 0xF;
            imm8lo = simm8 & 0xF;
            vassert(!(bL == 0 && bS == 1)); // "! signed store"
            /**/ if (bL == 0 && bS == 0) {
               // strh
               instr = XXXXXXXX(X1110,X0001, BITS4(bP,1,0,0), iregNo(rN),
                                iregNo(rD), imm8hi, X1011, imm8lo);
               *p++ = instr;
               goto done;
            }
            else if (bL == 1 && bS == 0) {
               // ldrh
               instr = XXXXXXXX(X1110,X0001, BITS4(bP,1,0,1), iregNo(rN),
                                iregNo(rD), imm8hi, X1011, imm8lo);
               *p++ = instr;
               goto done;
            }
            else if (bL == 1 && bS == 1) {
               goto bad;
            }
            else vassert(0); // ill-constructed insn
         } else {
            // RR case
            goto bad;
         }
      }
      case ARMin_Ld8S:
         goto bad;
      case ARMin_Goto: {
         UInt        instr;
         IRJumpKind  jk    = i->ARMin.Goto.jk;
         ARMCondCode cond  = i->ARMin.Goto.cond;
         UInt        rnext = iregNo(i->ARMin.Goto.gnext);
         Int         trc   = -1;
         /* since we branch to lr(r13) to get back to dispatch: */
         vassert(dispatch_unassisted == NULL);
         vassert(dispatch_assisted == NULL);
         switch (jk) {
            case Ijk_Ret: case Ijk_Call: case Ijk_Boring:
               break; /* no need to set GST in these common cases */
            case Ijk_ClientReq:
               trc = VEX_TRC_JMP_CLIENTREQ; break;
            case Ijk_Sys_int128:
            case Ijk_Sys_int129:
            case Ijk_Sys_int130:
            case Ijk_Yield:
            case Ijk_EmWarn:
            case Ijk_MapFail:
               goto unhandled_jk;
            case Ijk_NoDecode:
               trc = VEX_TRC_JMP_NODECODE; break;
            case Ijk_TInval:
               trc = VEX_TRC_JMP_TINVAL; break;
            case Ijk_NoRedir:
               trc = VEX_TRC_JMP_NOREDIR; break;
            case Ijk_Sys_sysenter:
            case Ijk_SigTRAP:
            case Ijk_SigSEGV:
               goto unhandled_jk;
            case Ijk_Sys_syscall:
               trc = VEX_TRC_JMP_SYS_SYSCALL; break;
            unhandled_jk:
            default:
               goto bad;
         }
         if (trc != -1) {
            // mov{cond} r8, #trc
            vassert(trc >= 0 && trc <= 255);
            instr = (cond << 28) | 0x03A08000 | (0xFF & (UInt)trc);
            *p++ = instr;
         }
         // mov{cond} r0, rnext
         if (rnext != 0) {
            instr = (cond << 28) | 0x01A00000 | rnext;
            *p++ = instr;
         }
         // bx{cond} r14
         instr =(cond << 28) | 0x012FFF1E;
         *p++ = instr;
         goto done;
      }
      case ARMin_CMov: {
         UInt instr  = skeletal_RI84(i->ARMin.CMov.src);
         UInt subopc = X1101; /* MOV */
         UInt SBZ    = 0;
         instr |= XXXXX___(i->ARMin.CMov.cond, (1 & (subopc >> 3)),
                           (subopc << 1) & 0xF, SBZ, i->ARMin.CMov.dst);
         *p++ = instr;
         goto done;
      }
      case ARMin_Call: {
         UInt instr;
         /* Decide on a scratch reg used to hold to the call address.
            This has to be done as per the comments in getRegUsage. */
         Int scratchNo;
         switch (i->ARMin.Call.nArgRegs) {
            case 0:  scratchNo = 0;  break;
            case 1:  scratchNo = 1;  break;
            case 2:  scratchNo = 2;  break;
            case 3:  scratchNo = 3;  break;
            case 4:  scratchNo = 11; break;
            default: vassert(0);
         }
         // r"scratchNo" = &target
         p = imm32_to_iregNo( (UInt*)p,
                              scratchNo, (UInt)i->ARMin.Call.target );
         // blx{cond} r"scratchNo"
         instr = XXX___XX(i->ARMin.Call.cond, X0001, X0010, /*___*/
                          X0011, scratchNo);
         instr |= 0xFFF << 8; // stick in the SBOnes
         *p++ = instr;
         goto done;
      }
      case ARMin_Mul: {
         /* E0000392   mul     r0, r2, r3
            E0810392   umull   r0(LO), r1(HI), r2, r3
            E0C10392   smull   r0(LO), r1(HI), r2, r3
         */
         switch (i->ARMin.Mul.op) {
            case ARMmul_PLAIN: *p++ = 0xE0000392; goto done;
            case ARMmul_ZX:    *p++ = 0xE0810392; goto done;
            case ARMmul_SX:    *p++ = 0xE0C10392; goto done;
            default: vassert(0);
         }
         goto bad;
      }
      case ARMin_LdrEX: {
         /* E1D42F9F   ldrexb r2, [r4]
            E1F42F9F   ldrexh r2, [r4]
            E1942F9F   ldrex  r2, [r4]
            E1B42F9F   ldrexd r2, r3, [r4]
         */
         switch (i->ARMin.LdrEX.szB) {
            case 1: *p++ = 0xE1D42F9F; goto done;
            case 2: *p++ = 0xE1F42F9F; goto done;
            case 4: *p++ = 0xE1942F9F; goto done;
            case 8: *p++ = 0xE1B42F9F; goto done;
            default: break;
         }
         goto bad;
      }
      case ARMin_StrEX: {
         /* E1C40F92   strexb r0, r2, [r4]
            E1E40F92   strexh r0, r2, [r4]
            E1840F92   strex  r0, r2, [r4]
            E1A40F92   strexd r0, r2, r3, [r4]
         */
         switch (i->ARMin.StrEX.szB) {
            case 1: *p++ = 0xE1C40F92; goto done;
            case 2: *p++ = 0xE1E40F92; goto done;
            case 4: *p++ = 0xE1840F92; goto done;
            case 8: *p++ = 0xE1A40F92; goto done;
            default: break;
         }
         goto bad;
      }
      case ARMin_VLdStD: {
         UInt dD     = dregNo(i->ARMin.VLdStD.dD);
         UInt rN     = iregNo(i->ARMin.VLdStD.amode->reg);
         Int  simm11 = i->ARMin.VLdStD.amode->simm11;
         UInt off8   = simm11 >= 0 ? simm11 : ((UInt)(-simm11));
         UInt bU     = simm11 >= 0 ? 1 : 0;
         UInt bL     = i->ARMin.VLdStD.isLoad ? 1 : 0;
         UInt insn;
         vassert(0 == (off8 & 3));
         off8 >>= 2;
         vassert(0 == (off8 & 0xFFFFFF00));
         insn = XXXXXX__(0xE,X1101,BITS4(bU,0,0,bL),rN,dD,X1011);
         insn |= off8;
         *p++ = insn;
         goto done;
      }
      case ARMin_VLdStS: {
         UInt fD     = fregNo(i->ARMin.VLdStS.fD);
         UInt rN     = iregNo(i->ARMin.VLdStS.amode->reg);
         Int  simm11 = i->ARMin.VLdStS.amode->simm11;
         UInt off8   = simm11 >= 0 ? simm11 : ((UInt)(-simm11));
         UInt bU     = simm11 >= 0 ? 1 : 0;
         UInt bL     = i->ARMin.VLdStS.isLoad ? 1 : 0;
         UInt bD     = fD & 1;
         UInt insn;
         vassert(0 == (off8 & 3));
         off8 >>= 2;
         vassert(0 == (off8 & 0xFFFFFF00));
         insn = XXXXXX__(0xE,X1101,BITS4(bU,bD,0,bL),rN, (fD >> 1), X1010);
         insn |= off8;
         *p++ = insn;
         goto done;
      }
      case ARMin_VAluD: {
         UInt dN = dregNo(i->ARMin.VAluD.argL);
         UInt dD = dregNo(i->ARMin.VAluD.dst);
         UInt dM = dregNo(i->ARMin.VAluD.argR);
         UInt pqrs = X1111; /* undefined */
         switch (i->ARMin.VAluD.op) {
            case ARMvfp_ADD: pqrs = X0110; break;
            case ARMvfp_SUB: pqrs = X0111; break;
            case ARMvfp_MUL: pqrs = X0100; break;
            case ARMvfp_DIV: pqrs = X1000; break;
            default: goto bad;
         }
         vassert(pqrs != X1111);
         UInt bP  = (pqrs >> 3) & 1;
         UInt bQ  = (pqrs >> 2) & 1;
         UInt bR  = (pqrs >> 1) & 1;
         UInt bS  = (pqrs >> 0) & 1;
         UInt insn = XXXXXXXX(0xE, X1110, BITS4(bP,0,bQ,bR), dN, dD,
                              X1011, BITS4(0,bS,0,0), dM);
         *p++ = insn;
         goto done;
      }
      case ARMin_VAluS: {
         UInt dN = fregNo(i->ARMin.VAluS.argL);
         UInt dD = fregNo(i->ARMin.VAluS.dst);
         UInt dM = fregNo(i->ARMin.VAluS.argR);
         UInt bN = dN & 1;
         UInt bD = dD & 1;
         UInt bM = dM & 1;
         UInt pqrs = X1111; /* undefined */
         switch (i->ARMin.VAluS.op) {
            case ARMvfp_ADD: pqrs = X0110; break;
            case ARMvfp_SUB: pqrs = X0111; break;
            case ARMvfp_MUL: pqrs = X0100; break;
            case ARMvfp_DIV: pqrs = X1000; break;
            default: goto bad;
         }
         vassert(pqrs != X1111);
         UInt bP  = (pqrs >> 3) & 1;
         UInt bQ  = (pqrs >> 2) & 1;
         UInt bR  = (pqrs >> 1) & 1;
         UInt bS  = (pqrs >> 0) & 1;
         UInt insn = XXXXXXXX(0xE, X1110, BITS4(bP,bD,bQ,bR),
                              (dN >> 1), (dD >> 1),
                              X1010, BITS4(bN,bS,bM,0), (dM >> 1));
         *p++ = insn;
         goto done;
      }
      case ARMin_VUnaryD: {
         UInt dD   = dregNo(i->ARMin.VUnaryD.dst);
         UInt dM   = dregNo(i->ARMin.VUnaryD.src);
         UInt insn = 0;
         switch (i->ARMin.VUnaryD.op) {
            case ARMvfpu_COPY:
               insn = XXXXXXXX(0xE, X1110,X1011,X0000,dD,X1011,X0100,dM);
               break;
            case ARMvfpu_ABS:
               insn = XXXXXXXX(0xE, X1110,X1011,X0000,dD,X1011,X1100,dM);
               break;
            case ARMvfpu_NEG:
               insn = XXXXXXXX(0xE, X1110,X1011,X0001,dD,X1011,X0100,dM);
               break;
            case ARMvfpu_SQRT:
               insn = XXXXXXXX(0xE, X1110,X1011,X0001,dD,X1011,X1100,dM);
               break;
            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_VUnaryS: {
         UInt fD   = fregNo(i->ARMin.VUnaryS.dst);
         UInt fM   = fregNo(i->ARMin.VUnaryS.src);
         UInt insn = 0;
         switch (i->ARMin.VUnaryS.op) {
            case ARMvfpu_COPY:
               insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0000,
                               (fD >> 1), X1010, BITS4(0,1,(fM & 1),0),
                               (fM >> 1));
               break;
            case ARMvfpu_ABS:
               insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0000,
                               (fD >> 1), X1010, BITS4(1,1,(fM & 1),0),
                               (fM >> 1));
               break;
            case ARMvfpu_NEG:
               insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0001,
                               (fD >> 1), X1010, BITS4(0,1,(fM & 1),0),
                               (fM >> 1));
               break;
            case ARMvfpu_SQRT:
               insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1), X0001,
                               (fD >> 1), X1010, BITS4(1,1,(fM & 1),0),
                               (fM >> 1));
               break;
            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_VCmpD: {
         UInt dD   = dregNo(i->ARMin.VCmpD.argL);
         UInt dM   = dregNo(i->ARMin.VCmpD.argR);
         UInt insn = XXXXXXXX(0xE, X1110, X1011, X0100, dD, X1011, X0100, dM);
         *p++ = insn;       /* FCMPD dD, dM */
         *p++ = 0xEEF1FA10; /* FMSTAT */
         goto done;
      }
      case ARMin_VCMovD: {
         UInt cc = (UInt)i->ARMin.VCMovD.cond;
         UInt dD = dregNo(i->ARMin.VCMovD.dst);
         UInt dM = dregNo(i->ARMin.VCMovD.src);
         vassert(cc < 16 && cc != ARMcc_AL);
         UInt insn = XXXXXXXX(cc, X1110,X1011,X0000,dD,X1011,X0100,dM);
         *p++ = insn;
         goto done;
      }
      case ARMin_VCMovS: {
         UInt cc = (UInt)i->ARMin.VCMovS.cond;
         UInt fD = fregNo(i->ARMin.VCMovS.dst);
         UInt fM = fregNo(i->ARMin.VCMovS.src);
         vassert(cc < 16 && cc != ARMcc_AL);
         UInt insn = XXXXXXXX(cc, X1110, BITS4(1,(fD & 1),1,1),
                              X0000,(fD >> 1),X1010,
                              BITS4(0,1,(fM & 1),0), (fM >> 1));
         *p++ = insn;
         goto done;
      }
      case ARMin_VCvtSD: {
         if (i->ARMin.VCvtSD.sToD) {
            UInt dD = dregNo(i->ARMin.VCvtSD.dst);
            UInt fM = fregNo(i->ARMin.VCvtSD.src);
            UInt insn = XXXXXXXX(0xE, X1110, X1011, X0111, dD, X1010,
                                 BITS4(1,1, (fM & 1), 0),
                                 (fM >> 1));
            *p++ = insn;
            goto done;
         } else {
            UInt fD = fregNo(i->ARMin.VCvtSD.dst);
            UInt dM = dregNo(i->ARMin.VCvtSD.src);
            UInt insn = XXXXXXXX(0xE, X1110, BITS4(1,(fD & 1),1,1),
                                 X0111, (fD >> 1),
                                 X1011, X1100, dM);
            *p++ = insn;
            goto done;
         }
         goto bad;
      }
      case ARMin_VXferD: {
         UInt dD  = dregNo(i->ARMin.VXferD.dD);
         UInt rHi = iregNo(i->ARMin.VXferD.rHi);
         UInt rLo = iregNo(i->ARMin.VXferD.rLo);
         /* vmov dD, rLo, rHi is
            E C 4 rHi rLo B (0,0,dD[4],1) dD[3:0]
            vmov rLo, rHi, dD is
            E C 5 rHi rLo B (0,0,dD[4],1) dD[3:0]
         */
         UInt insn
            = XXXXXXXX(0xE, 0xC, i->ARMin.VXferD.toD ? 4 : 5,
                       rHi, rLo, 0xB,
                       BITS4(0,0, ((dD >> 4) & 1), 1), (dD & 0xF));
         *p++ = insn;
         goto done;
      }
      case ARMin_VXferS: {
         UInt fD  = fregNo(i->ARMin.VXferS.fD);
         UInt rLo = iregNo(i->ARMin.VXferS.rLo);
         /* vmov fD, rLo is
            E E 0 fD[4:1] rLo A (fD[0],0,0,1) 0
            vmov rLo, fD is
            E E 1 fD[4:1] rLo A (fD[0],0,0,1) 0
         */
         UInt insn
            = XXXXXXXX(0xE, 0xE, i->ARMin.VXferS.toS ? 0 : 1,
                       (fD >> 1) & 0xF, rLo, 0xA, 
                       BITS4((fD & 1),0,0,1), 0);
         *p++ = insn;
         goto done;
      }
      case ARMin_VCvtID: {
         Bool iToD = i->ARMin.VCvtID.iToD;
         Bool syned = i->ARMin.VCvtID.syned;
         if (iToD && syned) {
            // FSITOD: I32S-in-freg to F64-in-dreg
            UInt regF = fregNo(i->ARMin.VCvtID.src);
            UInt regD = dregNo(i->ARMin.VCvtID.dst);
            UInt insn = XXXXXXXX(0xE, X1110, X1011, X1000, regD,
                                 X1011, BITS4(1,1,(regF & 1),0),
                                 (regF >> 1) & 0xF);
            *p++ = insn;
            goto done;
         }
         if (iToD && (!syned)) {
            // FUITOD: I32U-in-freg to F64-in-dreg
            UInt regF = fregNo(i->ARMin.VCvtID.src);
            UInt regD = dregNo(i->ARMin.VCvtID.dst);
            UInt insn = XXXXXXXX(0xE, X1110, X1011, X1000, regD,
                                 X1011, BITS4(0,1,(regF & 1),0),
                                 (regF >> 1) & 0xF);
            *p++ = insn;
            goto done;
         }
         if ((!iToD) && syned) {
            // FTOSID: F64-in-dreg to I32S-in-freg
            UInt regD = dregNo(i->ARMin.VCvtID.src);
            UInt regF = fregNo(i->ARMin.VCvtID.dst);
            UInt insn = XXXXXXXX(0xE, X1110, BITS4(1,(regF & 1),1,1),
                                 X1101, (regF >> 1) & 0xF,
                                 X1011, X0100, regD);
            *p++ = insn;
            goto done;
         }
         if ((!iToD) && (!syned)) {
            // FTOUID: F64-in-dreg to I32U-in-freg
            UInt regD = dregNo(i->ARMin.VCvtID.src);
            UInt regF = fregNo(i->ARMin.VCvtID.dst);
            UInt insn = XXXXXXXX(0xE, X1110, BITS4(1,(regF & 1),1,1),
                                 X1100, (regF >> 1) & 0xF,
                                 X1011, X0100, regD);
            *p++ = insn;
            goto done;
         }
         /*UNREACHED*/
         vassert(0);
      }
      case ARMin_FPSCR: {
         Bool toFPSCR = i->ARMin.FPSCR.toFPSCR;
         HReg iReg    = iregNo(i->ARMin.FPSCR.iReg);
         if (toFPSCR) {
            /* fmxr fpscr, iReg is EEE1 iReg A10 */
            *p++ = 0xEEE10A10 | ((iReg & 0xF) << 12);
            goto done;
         }
         goto bad; // FPSCR -> iReg case currently ATC
      }
      case ARMin_MFence: {
         *p++ = 0xEE070F9A; /* mcr 15,0,r0,c7,c10,4 (DSB) */
         *p++ = 0xEE070FBA; /* mcr 15,0,r0,c7,c10,5 (DMB) */
         *p++ = 0xEE070F95; /* mcr 15,0,r0,c7,c5,4  (ISB) */
         goto done;
      }
      case ARMin_NLdStQ: {
         UInt regD = qregNo(i->ARMin.NLdStQ.dQ) << 1;
         UInt regN, regM;
         UInt D = regD >> 4;
         UInt bL = i->ARMin.NLdStQ.isLoad ? 1 : 0;
         UInt insn;
         vassert(hregClass(i->ARMin.NLdStQ.dQ) == HRcVec128);
         regD &= 0xF;
         if (i->ARMin.NLdStQ.amode->tag == ARMamN_RR) {
            regN = iregNo(i->ARMin.NLdStQ.amode->ARMamN.RR.rN);
            regM = iregNo(i->ARMin.NLdStQ.amode->ARMamN.RR.rM);
         } else {
            regN = iregNo(i->ARMin.NLdStQ.amode->ARMamN.R.rN);
            regM = 15;
         }
         insn = XXXXXXXX(0xF, X0100, BITS4(0, D, bL, 0),
                              regN, regD, X1010, X1000, regM);
         *p++ = insn;
         goto done;
      }
      case ARMin_NLdStD: {
         UInt regD = dregNo(i->ARMin.NLdStD.dD);
         UInt regN, regM;
         UInt D = regD >> 4;
         UInt bL = i->ARMin.NLdStD.isLoad ? 1 : 0;
         UInt insn;
         vassert(hregClass(i->ARMin.NLdStD.dD) == HRcFlt64);
         regD &= 0xF;
         if (i->ARMin.NLdStD.amode->tag == ARMamN_RR) {
            regN = iregNo(i->ARMin.NLdStD.amode->ARMamN.RR.rN);
            regM = iregNo(i->ARMin.NLdStD.amode->ARMamN.RR.rM);
         } else {
            regN = iregNo(i->ARMin.NLdStD.amode->ARMamN.R.rN);
            regM = 15;
         }
         insn = XXXXXXXX(0xF, X0100, BITS4(0, D, bL, 0),
                              regN, regD, X0111, X1000, regM);
         *p++ = insn;
         goto done;
      }
      case ARMin_NUnaryS: {
         UInt Q = i->ARMin.NUnaryS.Q ? 1 : 0;
         UInt regD, D;
         UInt regM, M;
         UInt size = i->ARMin.NUnaryS.size;
         UInt insn;
         UInt opc, opc1, opc2;
         switch (i->ARMin.NUnaryS.op) {
	    case ARMneon_VDUP:
               if (i->ARMin.NUnaryS.size >= 16)
                  goto bad;
               if (i->ARMin.NUnaryS.dst->tag != ARMNRS_Reg)
                  goto bad;
               if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
                  goto bad;
               regD = (hregClass(i->ARMin.NUnaryS.dst->reg) == HRcVec128)
                        ? (qregNo(i->ARMin.NUnaryS.dst->reg) << 1)
                        : dregNo(i->ARMin.NUnaryS.dst->reg);
               regM = (hregClass(i->ARMin.NUnaryS.src->reg) == HRcVec128)
                        ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1)
                        : dregNo(i->ARMin.NUnaryS.src->reg);
               D = regD >> 4;
               M = regM >> 4;
               regD &= 0xf;
               regM &= 0xf;
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1),
                               (i->ARMin.NUnaryS.size & 0xf), regD,
                               X1100, BITS4(0,Q,M,0), regM);
               *p++ = insn;
               goto done; 
            case ARMneon_SETELEM:
               regD = Q ? (qregNo(i->ARMin.NUnaryS.dst->reg) << 1) :
                                dregNo(i->ARMin.NUnaryS.dst->reg);
               regM = iregNo(i->ARMin.NUnaryS.src->reg);
               M = regM >> 4;
               D = regD >> 4;
               regM &= 0xF;
               regD &= 0xF;
               if (i->ARMin.NUnaryS.dst->tag != ARMNRS_Scalar)
                  goto bad;
               switch (size) {
                  case 0:
                     if (i->ARMin.NUnaryS.dst->index > 7)
                        goto bad;
                     opc = X1000 | i->ARMin.NUnaryS.dst->index;
                     break;
                  case 1:
                     if (i->ARMin.NUnaryS.dst->index > 3)
                        goto bad;
                     opc = X0001 | (i->ARMin.NUnaryS.dst->index << 1);
                     break;
                  case 2:
                     if (i->ARMin.NUnaryS.dst->index > 1)
                        goto bad;
                     opc = X0000 | (i->ARMin.NUnaryS.dst->index << 2);
                     break;
                  default:
                     goto bad;
               }
               opc1 = (opc >> 2) & 3;
               opc2 = opc & 3;
               insn = XXXXXXXX(0xE, X1110, BITS4(0,(opc1 >> 1),(opc1 & 1),0),
                               regD, regM, X1011,
                               BITS4(D,(opc2 >> 1),(opc2 & 1),1), X0000);
               *p++ = insn;
               goto done;
            case ARMneon_GETELEMU:
               regM = Q ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1) :
                                dregNo(i->ARMin.NUnaryS.src->reg);
               regD = iregNo(i->ARMin.NUnaryS.dst->reg);
               M = regM >> 4;
               D = regD >> 4;
               regM &= 0xF;
               regD &= 0xF;
               if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
                  goto bad;
               switch (size) {
                  case 0:
                     if (Q && i->ARMin.NUnaryS.src->index > 7) {
                        regM++;
                        i->ARMin.NUnaryS.src->index -= 8;
                     }
                     if (i->ARMin.NUnaryS.src->index > 7)
                        goto bad;
                     opc = X1000 | i->ARMin.NUnaryS.src->index;
                     break;
                  case 1:
                     if (Q && i->ARMin.NUnaryS.src->index > 3) {
                        regM++;
                        i->ARMin.NUnaryS.src->index -= 4;
                     }
                     if (i->ARMin.NUnaryS.src->index > 3)
                        goto bad;
                     opc = X0001 | (i->ARMin.NUnaryS.src->index << 1);
                     break;
                  case 2:
                     goto bad;
                  default:
                     goto bad;
               }
               opc1 = (opc >> 2) & 3;
               opc2 = opc & 3;
               insn = XXXXXXXX(0xE, X1110, BITS4(1,(opc1 >> 1),(opc1 & 1),1),
                               regM, regD, X1011,
                               BITS4(M,(opc2 >> 1),(opc2 & 1),1), X0000);
               *p++ = insn;
               goto done;
            case ARMneon_GETELEMS:
               regM = Q ? (qregNo(i->ARMin.NUnaryS.src->reg) << 1) :
                                dregNo(i->ARMin.NUnaryS.src->reg);
               regD = iregNo(i->ARMin.NUnaryS.dst->reg);
               M = regM >> 4;
               D = regD >> 4;
               regM &= 0xF;
               regD &= 0xF;
               if (i->ARMin.NUnaryS.src->tag != ARMNRS_Scalar)
                  goto bad;
               switch (size) {
                  case 0:
                     if (Q && i->ARMin.NUnaryS.src->index > 7) {
                        regM++;
                        i->ARMin.NUnaryS.src->index -= 8;
                     }
                     if (i->ARMin.NUnaryS.src->index > 7)
                        goto bad;
                     opc = X1000 | i->ARMin.NUnaryS.src->index;
                     break;
                  case 1:
                     if (Q && i->ARMin.NUnaryS.src->index > 3) {
                        regM++;
                        i->ARMin.NUnaryS.src->index -= 4;
                     }
                     if (i->ARMin.NUnaryS.src->index > 3)
                        goto bad;
                     opc = X0001 | (i->ARMin.NUnaryS.src->index << 1);
                     break;
                  case 2:
                     if (Q && i->ARMin.NUnaryS.src->index > 1) {
                        regM++;
                        i->ARMin.NUnaryS.src->index -= 2;
                     }
                     if (i->ARMin.NUnaryS.src->index > 1)
                        goto bad;
                     opc = X0000 | (i->ARMin.NUnaryS.src->index << 2);
                     break;
                  default:
                     goto bad;
               }
               opc1 = (opc >> 2) & 3;
               opc2 = opc & 3;
               insn = XXXXXXXX(0xE, X1110, BITS4(0,(opc1 >> 1),(opc1 & 1),1),
                               regM, regD, X1011,
                               BITS4(M,(opc2 >> 1),(opc2 & 1),1), X0000);
               *p++ = insn;
               goto done;
            default:
               goto bad;
         }
      }
      case ARMin_NUnary: {
         UInt Q = i->ARMin.NUnary.Q ? 1 : 0;
         UInt regD = (hregClass(i->ARMin.NUnary.dst) == HRcVec128)
                       ? (qregNo(i->ARMin.NUnary.dst) << 1)
                       : dregNo(i->ARMin.NUnary.dst);
         UInt regM, M;
         UInt D = regD >> 4;
         UInt sz1 = i->ARMin.NUnary.size >> 1;
         UInt sz2 = i->ARMin.NUnary.size & 1;
         UInt sz = i->ARMin.NUnary.size;
         UInt insn;
         UInt F = 0; /* TODO: floating point EQZ ??? */
         if (i->ARMin.NUnary.op != ARMneon_DUP) {
            regM = (hregClass(i->ARMin.NUnary.src) == HRcVec128) 
                     ? (qregNo(i->ARMin.NUnary.src) << 1)
                     : dregNo(i->ARMin.NUnary.src);
            M = regM >> 4;
         } else {
            regM = iregNo(i->ARMin.NUnary.src);
            M = regM >> 4;
         }
         regD &= 0xF;
         regM &= 0xF;
         switch (i->ARMin.NUnary.op) {
            case ARMneon_COPY: /* VMOV reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regM, regD, X0001,
                               BITS4(M,Q,M,1), regM);
               break;
            case ARMneon_COPYN: /* VMOVN regD, regQ */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0010, BITS4(0,0,M,0), regM);
               break;
            case ARMneon_COPYQNSS: /* VQMOVN regD, regQ */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0010, BITS4(1,0,M,0), regM);
               break;
            case ARMneon_COPYQNUS: /* VQMOVUN regD, regQ */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0010, BITS4(0,1,M,0), regM);
               break;
            case ARMneon_COPYQNUU: /* VQMOVN regD, regQ */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0010, BITS4(1,1,M,0), regM);
               break;
            case ARMneon_COPYLS: /* VMOVL regQ, regD */
               if (sz >= 3)
                  goto bad;
               insn = XXXXXXXX(0xF, X0010,
                               BITS4(1,D,(sz == 2) ? 1 : 0,(sz == 1) ? 1 : 0),
                               BITS4((sz == 0) ? 1 : 0,0,0,0),
                               regD, X1010, BITS4(0,0,M,1), regM);
               break;
            case ARMneon_COPYLU: /* VMOVL regQ, regD */
               if (sz >= 3)
                  goto bad;
               insn = XXXXXXXX(0xF, X0011,
                               BITS4(1,D,(sz == 2) ? 1 : 0,(sz == 1) ? 1 : 0),
                               BITS4((sz == 0) ? 1 : 0,0,0,0),
                               regD, X1010, BITS4(0,0,M,1), regM);
               break;
            case ARMneon_NOT: /* VMVN reg, reg*/
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0000, regD, X0101,
                               BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_EQZ:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,1),
                               regD, BITS4(0,F,0,1), BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_CNT:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0000, regD, X0101,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_CLZ:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, X0100, BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_CLS:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, X0100, BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_ABS:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,1),
                               regD, X0011, BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_DUP:
               sz1 = i->ARMin.NUnary.size == 0 ? 1 : 0;
               sz2 = i->ARMin.NUnary.size == 1 ? 1 : 0;
               vassert(sz1 + sz2 < 2);
               insn = XXXXXXXX(0xE, X1110, BITS4(1, sz1, Q, 0), regD, regM,
                               X1011, BITS4(D,0,sz2,1), X0000);
               break;
            case ARMneon_REV16:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, BITS4(0,0,0,1), BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_REV32:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, BITS4(0,0,0,0), BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_REV64:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, BITS4(0,0,0,0), BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_PADDLU:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, X0010, BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_PADDLS:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,0,0),
                               regD, X0010, BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VQSHLNUU:
               insn = XXXXXXXX(0xF, X0011,
                               (1 << 3) | (D << 2) | ((sz >> 4) & 3),
                               sz & 0xf, regD, X0111,
                               BITS4(sz >> 6,Q,M,1), regM);
               break;
            case ARMneon_VQSHLNSS:
               insn = XXXXXXXX(0xF, X0010,
                               (1 << 3) | (D << 2) | ((sz >> 4) & 3),
                               sz & 0xf, regD, X0111,
                               BITS4(sz >> 6,Q,M,1), regM);
               break;
            case ARMneon_VQSHLNUS:
               insn = XXXXXXXX(0xF, X0011,
                               (1 << 3) | (D << 2) | ((sz >> 4) & 3),
                               sz & 0xf, regD, X0110,
                               BITS4(sz >> 6,Q,M,1), regM);
               break;
            case ARMneon_VCVTFtoS:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0111,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VCVTFtoU:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0111,
                               BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_VCVTStoF:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0110,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VCVTUtoF:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0110,
                               BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_VCVTFtoFixedU:
               sz1 = (sz >> 5) & 1;
               sz2 = (sz >> 4) & 1;
               sz &= 0xf;
               insn = XXXXXXXX(0xF, X0011,
                               BITS4(1,D,sz1,sz2), sz, regD, X1111,
                               BITS4(0,Q,M,1), regM);
               break;
            case ARMneon_VCVTFtoFixedS:
               sz1 = (sz >> 5) & 1;
               sz2 = (sz >> 4) & 1;
               sz &= 0xf;
               insn = XXXXXXXX(0xF, X0010,
                               BITS4(1,D,sz1,sz2), sz, regD, X1111,
                               BITS4(0,Q,M,1), regM);
               break;
            case ARMneon_VCVTFixedUtoF:
               sz1 = (sz >> 5) & 1;
               sz2 = (sz >> 4) & 1;
               sz &= 0xf;
               insn = XXXXXXXX(0xF, X0011,
                               BITS4(1,D,sz1,sz2), sz, regD, X1110,
                               BITS4(0,Q,M,1), regM);
               break;
            case ARMneon_VCVTFixedStoF:
               sz1 = (sz >> 5) & 1;
               sz2 = (sz >> 4) & 1;
               sz &= 0xf;
               insn = XXXXXXXX(0xF, X0010,
                               BITS4(1,D,sz1,sz2), sz, regD, X1110,
                               BITS4(0,Q,M,1), regM);
               break;
            case ARMneon_VCVTF32toF16:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0110, regD, X0110,
                               BITS4(0,0,M,0), regM);
               break;
            case ARMneon_VCVTF16toF32:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X0110, regD, X0111,
                               BITS4(0,0,M,0), regM);
               break;
            case ARMneon_VRECIP:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0100,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VRECIPF:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0101,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VABSFP:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1001, regD, X0111,
                               BITS4(0,Q,M,0), regM);
               break;
            case ARMneon_VRSQRTEFP:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0101,
                               BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_VRSQRTE:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1011, regD, X0100,
                               BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_VNEGF:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), X1001, regD, X0111,
                               BITS4(1,Q,M,0), regM);
               break;

            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_NDual: {
         UInt Q = i->ARMin.NDual.Q ? 1 : 0;
         UInt regD = (hregClass(i->ARMin.NDual.arg1) == HRcVec128)
                       ? (qregNo(i->ARMin.NDual.arg1) << 1)
                       : dregNo(i->ARMin.NDual.arg1);
         UInt regM = (hregClass(i->ARMin.NDual.arg2) == HRcVec128)
                       ? (qregNo(i->ARMin.NDual.arg2) << 1)
                       : dregNo(i->ARMin.NDual.arg2);
         UInt D = regD >> 4;
         UInt M = regM >> 4;
         UInt sz1 = i->ARMin.NDual.size >> 1;
         UInt sz2 = i->ARMin.NDual.size & 1;
         UInt insn;
         regD &= 0xF;
         regM &= 0xF;
         switch (i->ARMin.NDual.op) {
            case ARMneon_TRN: /* VTRN reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0000, BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_ZIP: /* VZIP reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0001, BITS4(1,Q,M,0), regM);
               break;
            case ARMneon_UZP: /* VUZP reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), BITS4(sz1,sz2,1,0),
                               regD, X0001, BITS4(0,Q,M,0), regM);
               break;
            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_NBinary: {
         UInt Q = i->ARMin.NBinary.Q ? 1 : 0;
         UInt regD = (hregClass(i->ARMin.NBinary.dst) == HRcVec128)
                       ? (qregNo(i->ARMin.NBinary.dst) << 1)
                       : dregNo(i->ARMin.NBinary.dst);
         UInt regN = (hregClass(i->ARMin.NBinary.argL) == HRcVec128)
                       ? (qregNo(i->ARMin.NBinary.argL) << 1)
                       : dregNo(i->ARMin.NBinary.argL);
         UInt regM = (hregClass(i->ARMin.NBinary.argR) == HRcVec128)
                       ? (qregNo(i->ARMin.NBinary.argR) << 1)
                       : dregNo(i->ARMin.NBinary.argR);
         UInt sz1 = i->ARMin.NBinary.size >> 1;
         UInt sz2 = i->ARMin.NBinary.size & 1;
         UInt D = regD >> 4;
         UInt N = regN >> 4;
         UInt M = regM >> 4;
         UInt insn;
         regD &= 0xF;
         regM &= 0xF;
         regN &= 0xF;
         switch (i->ARMin.NBinary.op) {
            case ARMneon_VAND: /* VAND reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X0001,
                               BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VORR: /* VORR reg, reg, reg*/
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD, X0001,
                               BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VXOR: /* VEOR reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD, X0001,
                               BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VADD: /* VADD reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1000, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VSUB: /* VSUB reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1000, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VMINU: /* VMIN.Uxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0110, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VMINS: /* VMIN.Sxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0110, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VMAXU: /* VMAX.Uxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0110, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VMAXS: /* VMAX.Sxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0110, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VRHADDS: /* VRHADD.Sxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0001, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VRHADDU: /* VRHADD.Uxx reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0001, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VQADDU: /* VQADD unsigned reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0000, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VQADDS: /* VQADD signed reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0000, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VQSUBU: /* VQSUB unsigned reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0010, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VQSUBS: /* VQSUB signed reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0010, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VCGTU: /* VCGT unsigned reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0011, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VCGTS: /* VCGT signed reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0011, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VCGEU: /* VCGE unsigned reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0011, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VCGES: /* VCGE signed reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0011, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VCEQ: /* VCEQ reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1000, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VEXT: /* VEXT.8 reg, reg, #imm4*/
               if (i->ARMin.NBinary.size >= 16)
                  goto bad;
               insn = XXXXXXXX(0xF, X0010, BITS4(1,D,1,1), regN, regD,
                               i->ARMin.NBinary.size & 0xf, BITS4(N,Q,M,0),
                               regM);
               break;
            case ARMneon_VMUL:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1001, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VMULLU:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,sz1,sz2), regN, regD,
                               X1100, BITS4(N,0,M,0), regM);
               break;
            case ARMneon_VMULLS:
               insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
                               X1100, BITS4(N,0,M,0), regM);
               break;
            case ARMneon_VMULP:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1001, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VMULFP:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
                               X1101, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VMULLP:
               insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
                               X1110, BITS4(N,0,M,0), regM);
               break;
            case ARMneon_VQDMULH:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1011, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VQRDMULH:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1011, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VQDMULL:
               insn = XXXXXXXX(0xF, X0010, BITS4(1,D,sz1,sz2), regN, regD,
                               X1101, BITS4(N,0,M,0), regM);
               break;
            case ARMneon_VTBL:
               insn = XXXXXXXX(0xF, X0011, BITS4(1,D,1,1), regN, regD,
                               X1000, BITS4(N,0,M,0), regM);
               break;
            case ARMneon_VPADD:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1011, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VPADDFP:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
                               X1101, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VPMINU:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1010, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VPMINS:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1010, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VPMAXU:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X1010, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VPMAXS:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X1010, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VADDFP: /* VADD reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD,
                               X1101, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VSUBFP: /* VADD reg, reg, reg */
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD,
                               X1101, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VABDFP: /* VABD reg, reg, reg */
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD,
                               X1101, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VMINF:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD,
                               X1111, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VMAXF:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD,
                               X1111, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VPMINF:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD,
                               X1111, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VPMAXF:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD,
                               X1111, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VRECPS:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X1111,
                               BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VCGTF:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,1,0), regN, regD, X1110,
                               BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VCGEF:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,0,0), regN, regD, X1110,
                               BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VCEQF:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,0,0), regN, regD, X1110,
                               BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VRSQRTS:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,1,0), regN, regD, X1111,
                               BITS4(N,Q,M,1), regM);
               break;
            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_NShift: {
         UInt Q = i->ARMin.NShift.Q ? 1 : 0;
         UInt regD = (hregClass(i->ARMin.NShift.dst) == HRcVec128)
                       ? (qregNo(i->ARMin.NShift.dst) << 1)
                       : dregNo(i->ARMin.NShift.dst);
         UInt regM = (hregClass(i->ARMin.NShift.argL) == HRcVec128)
                       ? (qregNo(i->ARMin.NShift.argL) << 1)
                       : dregNo(i->ARMin.NShift.argL);
         UInt regN = (hregClass(i->ARMin.NShift.argR) == HRcVec128)
                       ? (qregNo(i->ARMin.NShift.argR) << 1)
                       : dregNo(i->ARMin.NShift.argR);
         UInt sz1 = i->ARMin.NShift.size >> 1;
         UInt sz2 = i->ARMin.NShift.size & 1;
         UInt D = regD >> 4;
         UInt N = regN >> 4;
         UInt M = regM >> 4;
         UInt insn;
         regD &= 0xF;
         regM &= 0xF;
         regN &= 0xF;
         switch (i->ARMin.NShift.op) {
            case ARMneon_VSHL:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0100, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VSAL:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0100, BITS4(N,Q,M,0), regM);
               break;
            case ARMneon_VQSHL:
               insn = XXXXXXXX(0xF, X0011, BITS4(0,D,sz1,sz2), regN, regD,
                               X0100, BITS4(N,Q,M,1), regM);
               break;
            case ARMneon_VQSAL:
               insn = XXXXXXXX(0xF, X0010, BITS4(0,D,sz1,sz2), regN, regD,
                               X0100, BITS4(N,Q,M,1), regM);
               break;
            default:
               goto bad;
         }
         *p++ = insn;
         goto done;
      }
      case ARMin_NeonImm: {
         UInt Q = (hregClass(i->ARMin.NeonImm.dst) == HRcVec128) ? 1 : 0;
         UInt regD = Q ? (qregNo(i->ARMin.NeonImm.dst) << 1) :
                          dregNo(i->ARMin.NeonImm.dst);
         UInt D = regD >> 4;
         UInt imm = i->ARMin.NeonImm.imm->imm8;
         UInt tp = i->ARMin.NeonImm.imm->type;
         UInt j = imm >> 7;
         UInt imm3 = (imm >> 4) & 0x7;
         UInt imm4 = imm & 0xF;
         UInt cmode, op;
         UInt insn;
         regD &= 0xF;
         if (tp == 9)
            op = 1;
         else
            op = 0;
         switch (tp) {
            case 0:
            case 1:
            case 2:
            case 3:
            case 4:
            case 5:
               cmode = tp << 1;
               break;
            case 9:
            case 6:
               cmode = 14;
               break;
            case 7:
               cmode = 12;
               break;
            case 8:
               cmode = 13;
               break;
            case 10:
               cmode = 15;
               break;
            default:
               vpanic("ARMin_NeonImm");

         }
         insn = XXXXXXXX(0xF, BITS4(0,0,1,j), BITS4(1,D,0,0), imm3, regD,
                         cmode, BITS4(0,Q,op,1), imm4);
         *p++ = insn;
         goto done;
      }
      case ARMin_NCMovQ: {
         UInt cc = (UInt)i->ARMin.NCMovQ.cond;
         UInt qM = qregNo(i->ARMin.NCMovQ.src) << 1;
         UInt qD = qregNo(i->ARMin.NCMovQ.dst) << 1;
         UInt vM = qM & 0xF;
         UInt vD = qD & 0xF;
         UInt M  = (qM >> 4) & 1;
         UInt D  = (qD >> 4) & 1;
         vassert(cc < 16 && cc != ARMcc_AL && cc != ARMcc_NV);
         /* b!cc here+8: !cc A00 0000 */
         UInt insn = XXXXXXXX(cc ^ 1, 0xA, 0x0, 0x0, 0x0, 0x0, 0x0, 0x0);
         *p++ = insn;
         /* vmov qD, qM */
         insn = XXXXXXXX(0xF, 0x2, BITS4(0,D,1,0),
                         vM, vD, BITS4(0,0,0,1), BITS4(M,1,M,1), vM);
         *p++ = insn;
         goto done;
      }
      case ARMin_Add32: {
         UInt regD = iregNo(i->ARMin.Add32.rD);
         UInt regN = iregNo(i->ARMin.Add32.rN);
         UInt imm32 = i->ARMin.Add32.imm32;
         vassert(regD != regN);
         /* MOV regD, imm32 */
         p = imm32_to_iregNo((UInt *)p, regD, imm32);
         /* ADD regD, regN, regD */
         UInt insn = XXXXXXXX(0xE, 0, X1000, regN, regD, 0, 0, regD);
         *p++ = insn;
         goto done;
      }
      /* ... */
      default: 
         goto bad;
    }

  bad:
   ppARMInstr(i);
   vpanic("emit_ARMInstr");
   /*NOTREACHED*/

  done:
   vassert(((UChar*)p) - &buf[0] <= 32);
   return ((UChar*)p) - &buf[0];
}

#undef BITS4
#undef X0000
#undef X0001
#undef X0010
#undef X0011
#undef X0100
#undef X0101
#undef X0110
#undef X0111
#undef X1000
#undef X1001
#undef X1010
#undef X1011
#undef X1100
#undef X1101
#undef X1110
#undef X1111
#undef XXXXX___
#undef XXXXXX__
#undef XXX___XX
#undef XXXXX__X
#undef XXXXXXXX

/*---------------------------------------------------------------*/
/*--- end                                     host_arm_defs.c ---*/
/*---------------------------------------------------------------*/
