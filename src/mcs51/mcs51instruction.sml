(* $Id$ *)

structure MCS51Instruction :> MCS51_INSTRUCTION = struct
  
  datatype reg = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7
  datatype reg0or1 = Reg0 | Reg1

  type direct = string
   and data   = string
   and data16 = string
   and addr11 = string
   and addr16 = string
   and bit    = string
   and rel    = string

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

  fun regToString R0 = "R0" | regToString R1 = "R1"
    | regToString R2 = "R2" | regToString R3 = "R3"
    | regToString R4 = "R4" | regToString R5 = "R5"
    | regToString R6 = "R6" | regToString R7 = "R7"

  fun reg0or1ToString Reg0 = "R0" | reg0or1ToString Reg1 = "R1"

  fun directToString direct = direct
  fun dataToString data  = data
  fun indToString ri     = "@"^(reg0or1ToString ri)
  fun immToString data   = "#"^(dataToString data)

  fun data16ToString data16 = data16
  fun addr11ToString addr11 = addr11
  fun addr16ToString addr16 = addr16
  fun bitToString bit       = bit
  fun relToString rel       = rel

  fun toString (ADD_reg r)       = "ADD A,"^regToString r
    | toString (ADD_dir d)       = "ADD A,"^directToString d
    | toString (ADD_ind r)       = "ADD A,"^indToString r
    | toString (ADD_imm data)    = "ADD A,"^immToString data

    | toString (ADDC_reg r)      = "ADDC A,"^regToString r
    | toString (ADDC_dir d)      = "ADDC A,"^directToString d
    | toString (ADDC_ind r)      = "ADDC A,"^indToString r
    | toString (ADDC_imm data)   = "ADDC A,"^immToString data

    | toString (SUBB_reg r)      = "SUBB A,"^regToString r
    | toString (SUBB_dir d)      = "SUBB A,"^directToString d
    | toString (SUBB_ind r)      = "SUBB A,"^indToString r
    | toString (SUBB_imm data)   = "SUBB A,"^immToString data

    | toString (INC_acc)         = "INC A"
    | toString (INC_reg r)       = "INC "^regToString r
    | toString (INC_dir d)       = "INC "^directToString d
    | toString (INC_ind r)       = "INC "^indToString r

    | toString (DEC_acc)         = "DEC A"
    | toString (DEC_reg r)       = "DEC "^regToString r
    | toString (DEC_dir d)       = "DEC "^directToString d
    | toString (DEC_ind r)       = "DEC "^indToString r

    | toString (INC_DPTR)        = "INC DPTR"
    | toString (MUL)             = "MUL AB"
    | toString (DIV)             = "DIV AB"
    | toString (DA)              = "DA A"

    | toString (ANL_reg r)       = "ANL A,"^regToString r
    | toString (ANL_dir d)       = "ANL A,"^directToString d
    | toString (ANL_ind r)       = "ANL A,"^indToString r
    | toString (ANL_imm data)    = "ANL A,"^immToString data
    | toString (ANL_A_dir d)     = "ANL "^directToString d^",A"
    | toString (ANL_dir_imm (d, data))
              = concat ["ANL ", directToString d, ",", immToString data]

    | toString (ORL_reg r)       = "ORL A,"^regToString r
    | toString (ORL_dir d)       = "ORL A,"^directToString d
    | toString (ORL_ind r)       = "ORL A,"^indToString r
    | toString (ORL_imm data)    = "ORL A,"^immToString data
    | toString (ORL_A_dir d)     = "ORL "^directToString d^",A"
    | toString (ORL_dir_imm (d, data))
              = concat ["ORL ", directToString d, ",", immToString data]

    | toString (XRL_reg r)       = "XRL A,"^regToString r
    | toString (XRL_dir d)       = "XRL A,"^directToString d
    | toString (XRL_ind r)       = "XRL A,"^indToString r
    | toString (XRL_imm data)    = ""^immToString data
    | toString (XRL_A_dir d)     = "XRL "^directToString d^",A"
    | toString (XRL_dir_imm (d, data))
              = concat ["XRL ", directToString d, ",", immToString data]

    | toString (CLR)             = "CLR A"
    | toString (CPL)             = "CPL A"
    | toString (RL)              = "RL A"
    | toString (RLC)             = "RLC A"
    | toString (RR)              = "RR A"
    | toString (RRC)             = "RRC A"
    | toString (SWAP)            = "SWAP A"
    | toString (MOV_regToA r)    = "MOV A,"^regToString r
    | toString (MOV_dirToA d)    = "MOV A,"^directToString d
    | toString (MOV_indToA r)    = "MOV A,"^indToString r
    | toString (MOV_immToA data) = "MOV A,"^immToString data
    | toString (MOV_AToReg r)    = "MOV "^regToString r^",A"
    | toString (MOV_dirToReg (r, d))
              = concat ["MOV ", regToString r, ",", directToString d]
    | toString (MOV_immToReg (r, data))
              = concat ["MOV ", regToString r, ",", immToString data]
    | toString (MOV_AToDir d)    = "MOV "^directToString d^",A"
    | toString (MOV_regToDir (d, r))
              = concat ["MOV ", directToString d, ",", regToString r]
    | toString (MOV_dirToDir (dd, ds))
              = concat ["MOV ", directToString dd, ",", directToString ds]
    | toString (MOV_indToDir (d, r))
              = concat ["MOV ", directToString d, ",", indToString r]
    | toString (MOV_immToDir (d, data))
              = concat ["MOV ", directToString d, ",", dataToString data]
    | toString (MOV_AToInd r)    = "MOV "^indToString r^",A"
    | toString (MOV_dirToInd (r, d))
              = concat ["MOV ", indToString r, ",", directToString d]
    | toString (MOV_immToInd (r, data))
              = concat ["MOV ", indToString r, ",", immToString data]
    | toString (MOVDPTR data16)  = "MOV DPTR,"^data16ToString data16
    | toString (MOVC_DPTR)       = "MOVC A,@A+DPTR"
    | toString (MOVC_PC)         = "MOVC A,@A+PC"

    | toString (MOVX_From8 r)    = "MOVX A,"^indToString r
    | toString (MOVX_From16)     = "MOVX A,@DTPR"
    | toString (MOVX_To8 r)      = "MOVX "  ^indToString r^",A"
    | toString (MOVX_To16)       = "MOVX @DPTR,A"

    | toString (PUSH d)          = "PUSH "  ^directToString d
    | toString (POP d)           = "POP "   ^directToString d
    | toString (XCH_rn r)        = "XCH A," ^regToString r
    | toString (XCH_dir d)       = "XCH A," ^directToString d
    | toString (XCH_ind r)       = "XCH A," ^indToString r
    | toString (XCHD_ind r)      = "XCHD A,"^indToString r

    | toString (CLR_c)           = "CLR C"
    | toString (CLR_bit b)       = "CLR " ^bitToString b
    | toString (SETB_c)          = "SETB C"
    | toString (SETB_bit b)      = "SETB "^bitToString b
    | toString (CPL_c)           = "CPL C"
    | toString (CPL_bit b)       = "CPL " ^bitToString b

    | toString (ANL_bit b)       = "ANL " ^bitToString b
    | toString (ANL_cbit b)      = "ANL /"^bitToString b
    | toString (ORL_bit b)       = "ORL " ^bitToString b
    | toString (ORL_cbit b)      = "ORL /"^bitToString b
    | toString (MOV_cToBit b)    = "MOV C,"^bitToString b
    | toString (MOV_bitToC b)    = "MOV "^bitToString b^",C"
    | toString (ACALL addr11)    = "ACALL "^addr11ToString addr11
    | toString (LCALL addr16)    = "LCALL "^addr16ToString addr16
    | toString (RET)             = "RET"
    | toString (RETI)            = "RETI"
    | toString (AJMP addr11)     = "AJMP"^addr11ToString addr11
    | toString (LJMP addr16)     = "LJMP"^addr16ToString addr16
    | toString (SJMP rel)        = "SJMP"^relToString rel
    | toString (JMP_DPTR)        = "JMP @A+DPTR"
    | toString (JZ rel)          = "JZ " ^relToString rel
    | toString (JNZ rel)         = "JNZ "^relToString rel
    | toString (JC rel)          = "JC " ^relToString rel
    | toString (JNC rel)         = "JNC "^relToString rel
    | toString (JB (b, rl))
              = concat ["JB " , bitToString b, ",", relToString rl]
    | toString (JNB (b, rl))
              = concat ["JNB ", bitToString b, ",", relToString rl]
    | toString (JBC (b, rl))
              = concat ["JBC ", bitToString b, ",", relToString rl]

    | toString (CJNE_dirToA (d, rl))
              = concat ["CJNE A,", directToString d, ",", relToString rl]
    | toString (CJNE_immToA (data, rl))
              = concat ["CJNE A,", immToString data, ",", relToString rl]
    | toString (CJNE_immToReg (r, data, rl))
              = concat ["CJNE ", regToString r, ",", immToString data,
                        ",", relToString rl]
    | toString (CJNE_immToInd (r, data, rl))
              = concat ["CJNE ", indToString r, ",", immToString data,
                        ",", relToString rl]

    | toString (DJNZ_reg (r, rl))
              = concat ["DJNZ ", regToString r, ",", relToString rl]
    | toString (DJNZ_dir (d, rl))
              = concat ["DJNZ ", directToString d, ",", relToString rl]

    | toString (NOP) = "NOP"

  fun numCycles (INC_DPTR)            = 2
    | numCycles (MUL)                 = 4
    | numCycles (DIV)                 = 4
    | numCycles (ANL_dir_imm _)       = 2
    | numCycles (ORL_dir_imm _)       = 2
    | numCycles (XRL_dir_imm _)       = 2
    | numCycles (MOV_dirToReg _)      = 2
    | numCycles (MOV_regToDir _)      = 2
    | numCycles (MOV_dirToDir _)      = 2
    | numCycles (MOV_indToDir _)      = 2
    | numCycles (MOV_immToDir _)      = 2
    | numCycles (MOV_dirToInd _)      = 2
    | numCycles (MOVC_DPTR)           = 2
    | numCycles (MOVC_PC)             = 2
    | numCycles (MOVX_From8 _)        = 2
    | numCycles (MOVX_From16)         = 2
    | numCycles (MOVX_To8 _)          = 2
    | numCycles (MOVX_To16)           = 2
    | numCycles (PUSH _)              = 2
    | numCycles (POP _)               = 2
    | numCycles (ANL_bit _)           = 2
    | numCycles (ANL_cbit _)          = 2
    | numCycles (ORL_bit _)           = 2
    | numCycles (ORL_cbit _)          = 2
    | numCycles (MOV_bitToC _)        = 2
    | numCycles (ACALL _)             = 2
    | numCycles (LCALL _)             = 2
    | numCycles (RET)                 = 2
    | numCycles (RETI)                = 2
    | numCycles (AJMP _)              = 2
    | numCycles (LJMP _)              = 2
    | numCycles (SJMP _)              = 2
    | numCycles (JMP_DPTR)            = 2
    | numCycles (JZ _)                = 2
    | numCycles (JNZ _)               = 2
    | numCycles (JC _)                = 2
    | numCycles (JNC _)               = 2
    | numCycles (JB _)                = 2
    | numCycles (JNB _)               = 2
    | numCycles (JBC _)               = 2
    | numCycles (CJNE_dirToA _)       = 2
    | numCycles (CJNE_immToA _)       = 2
    | numCycles (CJNE_immToReg _)     = 2
    | numCycles (CJNE_immToInd _)     = 2
    | numCycles (DJNZ_reg _)          = 2
    | numCycles (DJNZ_dir _)          = 2
    | numCycles _                     = 1

  fun numBytes (ADD_reg _)            = 1
    | numBytes (ADD_dir _)            = 2
    | numBytes (ADD_ind _)            = 1
    | numBytes (ADD_imm _)            = 2
    | numBytes (ADDC_reg _)           = 1
    | numBytes (ADDC_dir _)           = 2
    | numBytes (ADDC_ind _)           = 1
    | numBytes (ADDC_imm _)           = 2
    | numBytes (SUBB_reg _)           = 1
    | numBytes (SUBB_dir _)           = 2
    | numBytes (SUBB_ind _)           = 1
    | numBytes (SUBB_imm _)           = 2
    | numBytes (INC_acc)              = 1
    | numBytes (INC_reg _)            = 1
    | numBytes (INC_dir _)            = 2
    | numBytes (INC_ind _)            = 1
    | numBytes (DEC_acc)              = 1
    | numBytes (DEC_reg _)            = 1
    | numBytes (DEC_dir _)            = 2
    | numBytes (DEC_ind _)            = 1
    | numBytes (INC_DPTR)             = 1
    | numBytes (MUL)                  = 1
    | numBytes (DIV)                  = 1
    | numBytes (DA)                   = 1
    | numBytes (ANL_reg _)            = 1
    | numBytes (ANL_dir _)            = 2
    | numBytes (ANL_ind _)            = 1
    | numBytes (ANL_imm _)            = 2
    | numBytes (ANL_A_dir _)          = 2
    | numBytes (ANL_dir_imm _)        = 3
    | numBytes (ORL_reg _)            = 1
    | numBytes (ORL_dir _)            = 2
    | numBytes (ORL_ind _)            = 1
    | numBytes (ORL_imm _)            = 2
    | numBytes (ORL_A_dir _)          = 2
    | numBytes (ORL_dir_imm _)        = 3
    | numBytes (XRL_reg _)            = 1
    | numBytes (XRL_dir _)            = 2
    | numBytes (XRL_ind _)            = 1
    | numBytes (XRL_imm _)            = 2
    | numBytes (XRL_A_dir _)          = 2
    | numBytes (XRL_dir_imm _)        = 3
    | numBytes (CLR)                  = 1
    | numBytes (CPL)                  = 1
    | numBytes (RL)                   = 1
    | numBytes (RLC)                  = 1
    | numBytes (RR)                   = 1
    | numBytes (RRC)                  = 1
    | numBytes (SWAP)                 = 1
    | numBytes (MOV_regToA _)         = 1
    | numBytes (MOV_dirToA _)         = 2
    | numBytes (MOV_indToA _)         = 1
    | numBytes (MOV_immToA _)         = 2
    | numBytes (MOV_AToReg _)         = 1
    | numBytes (MOV_dirToReg _)       = 2
    | numBytes (MOV_immToReg _)       = 2
    | numBytes (MOV_AToDir _)         = 2
    | numBytes (MOV_regToDir _)       = 2
    | numBytes (MOV_dirToDir _)       = 3
    | numBytes (MOV_indToDir _)       = 2
    | numBytes (MOV_immToDir _)       = 3
    | numBytes (MOV_AToInd _)         = 1
    | numBytes (MOV_dirToInd _)       = 2
    | numBytes (MOV_immToInd _)       = 2
    | numBytes (MOVDPTR _)            = 2
    | numBytes (MOVC_DPTR)            = 1
    | numBytes (MOVC_PC)              = 1
    | numBytes (MOVX_From8 _)         = 1
    | numBytes (MOVX_From16)          = 1
    | numBytes (MOVX_To8 _)           = 1
    | numBytes (MOVX_To16)            = 1
    | numBytes (PUSH _)               = 2
    | numBytes (POP _)                = 2
    | numBytes (XCH_rn _)             = 1
    | numBytes (XCH_dir _)            = 2
    | numBytes (XCH_ind _)            = 1
    | numBytes (XCHD_ind _)           = 1
    | numBytes (CLR_c)                = 1
    | numBytes (CLR_bit _)            = 2
    | numBytes (SETB_c)               = 1
    | numBytes (SETB_bit _)           = 2
    | numBytes (CPL_c)                = 1
    | numBytes (CPL_bit _)            = 2
    | numBytes (ANL_bit _)            = 2
    | numBytes (ANL_cbit _)           = 2
    | numBytes (ORL_bit _)            = 2
    | numBytes (ORL_cbit _)           = 2
    | numBytes (MOV_cToBit _)         = 2
    | numBytes (MOV_bitToC _)         = 2
    | numBytes (ACALL _)              = 2
    | numBytes (LCALL _)              = 3
    | numBytes (RET)                  = 1
    | numBytes (RETI)                 = 1
    | numBytes (AJMP _)               = 2
    | numBytes (LJMP _)               = 3
    | numBytes (SJMP _)               = 2
    | numBytes (JMP_DPTR)             = 1
    | numBytes (JZ _)                 = 2
    | numBytes (JNZ _)                = 2
    | numBytes (JC _)                 = 2
    | numBytes (JNC _)                = 2
    | numBytes (JB _)                 = 3
    | numBytes (JNB _)                = 3
    | numBytes (JBC _)                = 3
    | numBytes (CJNE_dirToA _)        = 3
    | numBytes (CJNE_immToA _)        = 3
    | numBytes (CJNE_immToReg _)      = 3
    | numBytes (CJNE_immToInd _)      = 3
    | numBytes (DJNZ_reg _)           = 2
    | numBytes (DJNZ_dir _)           = 3
    | numBytes (NOP)                  = 1

  fun stringToDirect s = s
  fun stringToData s   = s
  fun stringToData16 s = s
  fun stringToAddr11 s = s
  fun stringToAddr16 s = s
  fun stringToBit s    = s
  fun stringToRel s    = s

end

