(* $Id$
 *
 * Copyright (c) 2008 Timothy Bourke (University of NSW and NICTA)
 * All rights reserved.
 *
 * This program is free software; you can redistribute it and/or modify it
 * under the terms of the "BSD License" which is distributed with the
 * software in the file LICENSE.
 *
 * This program is distributed in the hope that it will be useful, but
 * WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the BSD
 * License for more details.
 *
 * Derived from the Intel MCS-51 Instruction Set Summary
 *
 *)

signature MCS51_INSTRUCTION = sig
  
  datatype reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  datatype reg0or1 = Reg0 | Reg1

  type direct
   and data
   and data16
   and addr11
   and addr16
   and bit
   and rel

  datatype t = (* arithmetic *)
               ADD_reg of reg
             | ADD_dir of direct
             | ADD_ind of reg0or1
             | ADD_imm of data

             | ADDC_reg of reg
             | ADDC_dir of direct
             | ADDC_ind of reg0or1
             | ADDC_imm of data

             | SUBB_reg of reg
             | SUBB_dir of direct
             | SUBB_ind of reg0or1
             | SUBB_imm of data

             | INC_acc
             | INC_reg of reg
             | INC_dir of direct
             | INC_ind of reg0or1

             | DEC_acc
             | DEC_reg of reg
             | DEC_dir of direct
             | DEC_ind of reg0or1

             | INC_DPTR
             | MUL
             | DIV
             | DA
               (* logical *)
             | ANL_reg of reg
             | ANL_dir of direct
             | ANL_ind of reg0or1
             | ANL_imm of data
             | ANL_A_dir of direct
             | ANL_dir_imm of direct * data

             | ORL_reg of reg
             | ORL_dir of direct
             | ORL_ind of reg0or1
             | ORL_imm of data
             | ORL_A_dir of direct
             | ORL_dir_imm of direct * data

             | XRL_reg of reg
             | XRL_dir of direct
             | XRL_ind of reg0or1
             | XRL_imm of data
             | XRL_A_dir of direct
             | XRL_dir_imm of direct * data

             | CLR
             | CPL
             | RL
             | RLC
             | RR
             | RRC
             | SWAP
               (* data transfer *)
             | MOV_regToA of reg
             | MOV_dirToA of direct
             | MOV_indToA of reg0or1
             | MOV_immToA of data

             | MOV_AToReg of reg
             | MOV_dirToReg of reg * direct
             | MOV_immToReg of reg * data

             | MOV_AToDir of direct
             | MOV_regToDir of direct * reg
             | MOV_dirToDir of direct * direct
             | MOV_indToDir of direct * reg0or1
             | MOV_immToDir of direct * data

             | MOV_AToInd of reg0or1
             | MOV_dirToInd of reg0or1 * direct
             | MOV_immToInd of reg0or1 * data

             | MOVDPTR of data16

             | MOVC_DPTR
             | MOVC_PC

             | MOVX_From8 of reg0or1
             | MOVX_From16
             | MOVX_To8 of reg0or1
             | MOVX_To16

             | PUSH of direct
             | POP of direct

             | XCH_rn of reg
             | XCH_dir of direct
             | XCH_ind of reg0or1

             | XCHD_ind of reg0or1

             (* Boolean Variable Manipulation *)
             | CLR_c
             | CLR_bit of bit
             | SETB_c
             | SETB_bit of bit
             | CPL_c
             | CPL_bit of bit

             | ANL_bit of bit
             | ANL_cbit of bit
             | ORL_bit of bit
             | ORL_cbit of bit

             | MOV_cToBit of bit
             | MOV_bitToC of bit

             (* Program and Machine Control *)
             | ACALL of addr11
             | LCALL of addr16
             | RET
             | RETI
             | AJMP of addr11
             | LJMP of addr16
             | SJMP of rel
             | JMP_DPTR
             | JZ of rel
             | JNZ of rel
             | JC of rel
             | JNC of rel
             | JB of bit * rel
             | JNB of bit * rel
             | JBC of bit * rel

             | CJNE_dirToA of direct * rel
             | CJNE_immToA of data * rel
             | CJNE_immToReg of reg * data * rel
             | CJNE_immToInd of reg0or1 * data * rel

             | DJNZ_reg of reg * rel
             | DJNZ_dir of direct * rel

             | NOP

  val regToString         : reg -> string
  val indToString         : reg0or1 -> string
  val immToString         : data -> string

  val directToString      : direct -> string
  val dataToString        : data -> string
  val data16ToString      : data16 -> string
  val addr11ToString      : addr11 -> string
  val addr16ToString      : addr16 -> string
  val bitToString         : bit -> string
  val relToString         : rel -> string

  val toString            : t -> string
  val numCycles           : t -> int
  val numBytes            : t -> int

  val stringToDirect      : string -> direct
  val stringToData        : string -> data
  val stringToData16      : string -> data16
  val stringToAddr11      : string -> addr11
  val stringToAddr16      : string -> addr16
  val stringToBit         : string -> bit
  val stringToRel         : string -> rel
end

