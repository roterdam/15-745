{-
  The datatype for X86-64 assembly
-}

module Compile.Types.XAsm where

import Data.Int (Int32)
import Data.List (intercalate)
import qualified Compile.Types.Common as Common

{-
  Represents a full XAsm program
-}
data XAsm = Prog [Line]

{-
  Represents a line of X86-64 assembly code
-}
data Line = Ins Ins
          | Comment String
          | Pseudo String [String]
          | Symbol String

{-
  Supported subset of X86-64 instruction set.

  Each of the following instructions (with the exception of Mov2Mem) maps directly to an x86-64
  instructions.

  We generate only a small subset of the x86-64 instruction set. And within this subset, we do not
  generate each instruction in all possible forms (i.e. with all possible addressing modes).

  Instead, we restrict the addressing modes used each instruction to a subset of the possible
  addressing modes for that instruction.

  NOTE: The one expection to these rules is Mov2Mem. It is the only supported instruction that can
  move values to memory, and is kept around for that reason. It is a special case of Mov.

  NOTE: We must be careful that any instructions we add to this datatype only take of datatypes
  that correspond to legal addressing modes.

  Below, for reference, we group supported x86-64 instructions by possible addressing modes. Note
  that these are the addressing modes that are possible in x86-64, and that we will not generate
  that uses all of them (as per the rules above):

    mov    i -> r
    /add   i -> m
    /sub   r -> r
    /and   r -> m
    /or    m -> r
    /xor
    /cmp

    cmov   r -> r
           m -> r

    imul   i -> r
           r -> r
           m -> r

    idiv   r
           m

    sal    r
    /sar   m
           i -> r
           i -> m
-}
data Ins = -- Arithmetic
           Add   Common.Size Src Common.Reg
         | Sub   Common.Size Src Common.Reg
         | IMul  Common.Size Src Common.Reg
         | IDiv  Common.Size Common.Reg
         | Ext   Common.Size
           -- Bitwise
         | And   Common.Size Src Common.Reg
         | Or    Common.Size Src Common.Reg
         | Xor   Common.Size Src Common.Reg
         | SaL   Common.Size Common.Reg
         | SaR   Common.Size Common.Reg
           -- Memory
         | Mov   Common.Size Src Common.Reg
--         | MovS  Common.Size Common.Size Src Common.Reg
           -- Comparisons (<, <=, ==, >=, >, !=)
         | Cmp   Common.Size Src Common.Reg
         | Test  Common.Size Src Common.Reg
         | CMov  Common.Cmp Common.Size Common.Reg Common.Reg
           -- Control Flow
         | Jmpcc Common.Cmp String
         | Jmp   String
         | Call  String
         | CallPtr Src
         | Ret
           -- Other
         | Mov2Mem Common.Size Common.Reg Ind
         | LoadLbl String Common.Reg
          {-
            TODO: document LoadLbl case better
          -}

-- TODO: should Imm's actually be Int32's? What about Ind's?
-- Actually, the best solution may involve letting Imms have a few types depending on what the
-- size of what we're trying to move is.

{-
  A valid src argument for an instruction (a constant, register, or memory)
-}
data Src = Reg Common.Reg | Mem Ind | Imm Int32

{-
  An indirect reference, given as a register and an offset from that register's value
-}
data Ind = Ind Common.Reg Int32

{- Convenience methods -}
dstToSrc :: Common.Reg -> Src
dstToSrc = Reg

{- Show implementations -}
instance Show XAsm where
  show (Prog lines) = concatMap show lines

-- Possible lines of x86-64 code
instance Show Line where
  show (Ins ins) = "  " ++ show ins ++ "\n"
  show (Comment s) = "# " ++ s ++ "\n"
  show (Pseudo op args) = "." ++ op ++ " " ++ intercalate "," args ++ "\n"
  show (Symbol symbol) = symbol ++ ":\n"

instance Show Ins where
  show (Add sz src dst) = "add" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (Sub sz src dst) = "sub" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (IMul sz src dst) = "imul" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (IDiv sz src) = "idiv" ++ show sz ++ " " ++ show src
  show (Ext sz) = case sz of
    Common.Byte -> "cbw"
    Common.Long -> "cdq"
    Common.Quad -> "cqo"

  show (And sz src dst) = "and" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (Or sz src dst) = "or" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (Xor sz src dst) = "xor" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (SaL sz dst) = "sal" ++ show sz ++ " %cl, " ++ show dst
  show (SaR sz dst) = "sar" ++ show sz ++ " %cl, " ++ show dst

  show (Mov sz src dst) = "mov" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
--  show (Mov sz1 sz2 src dst) =
--    "movs" ++ show sz1 ++ show sz2 ++ " " ++ show src ++ ", " ++ show dst

  show (Cmp sz src dst) = "cmp" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (Test sz src dst) = "test" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (CMov cmpop sz src dst) = "cmov" ++ show cmpop ++ show sz ++ " " ++ show src ++ ", " ++ show dst

  show (Jmpcc cmpop l) = "j" ++ show cmpop ++ " " ++ l
  show (Jmp l) = "jmp " ++ l
  show (Call l) = "call " ++ l
  show (CallPtr src) = "call *" ++ show src
  show Ret = "ret"

  show (Mov2Mem sz src dst) = "mov" ++ show sz ++ " " ++ show src ++ ", " ++ show dst
  show (LoadLbl lbl reg) = "lea " ++ lbl ++ "(%rip), " ++ show reg

instance Show Src where
  show (Reg r) = show r
  show (Mem ind) = show ind
  show (Imm x) = "$" ++ Common.toHexStr x

instance Show Ind where
  show (Ind r offset) = Common.toHexStr offset ++ "(" ++ show r ++ ")"
