{-
  This file contains pre-processing functions for the 2Asm -> XAsm conversion.
-}

-- TODO: stack offsets shouldn't actually be Int32s. Maybe Int64 or integer?

module Compile.CodeGen.GenX86.Calling where

import qualified Job
import qualified Compile.Types.XAsm as XAsm
import qualified Compile.Types.Common as Common
import Compile.CodeGen.GenX86.Common (Assignment(..), TransMaps, FnList)

import Data.Bits ((.|.))
import Data.Int (Int32)
import qualified Data.Maybe as Maybe
import qualified Data.Map as Map
import qualified Data.List as List

{-
  Gets the size of a function's stack frame, by adding together the maximum spill index from its
  temp assignments and the number of callee-save registers being pushed to the stack (currently,
  all of them).
-}
getFrameSize :: [Common.RName] -> TransMaps -> Int32
getFrameSize toSave tMaps =
  let spillSlots = numSpillSlots tMaps
      calleeSlots = toInteger $ length toSave
      totalSlots = makeOdd (spillSlots + calleeSlots)
      slotSize = Common.bytesForSize Common.Quad
  in  fromIntegral $ totalSlots * slotSize
  where makeOdd :: (Integral n) => n -> n
        makeOdd n = if mod n 2 == 0 then n + 1 else n

        numSpillSlots :: TransMaps -> Integer
        numSpillSlots (aMap, rMap, _) =
          case (Maybe.mapMaybe getIndex $ Map.elems aMap ++ Map.elems rMap) of
            [] -> 0
            is -> 1 + (maximum is)

        getIndex :: Assignment -> Maybe Integer
        getIndex (Spill i) = Just i
        getIndex _ = Nothing

-- Gets a list of callee-saved regs that need to be saved for a function,
-- given the assignments for that function.
getRegsToSave :: TransMaps -> [Common.RName]
getRegsToSave (aMap, rMap, _) = List.intersect Common.calleeSaveRegs (getRegs aMap ++ getRegs rMap)
  where getRegs assns = Maybe.mapMaybe getReg $ Map.elems assns
        getReg (Reg rName) = Just rName
        getReg (Spill _) = Nothing

-- Gets a header and footer to wrap a translated function body.
-- These handle stack discipline, specifically storing/restoring callee-saved registers.
fnHeaderFooter :: [Common.RName] -> Int32 -> String -> ([XAsm.Line], [XAsm.Line])
fnHeaderFooter toSave frameSize fnName =
  let header =
        [ XAsm.Pseudo "globl" [fnName],
          XAsm.Symbol fnName,
          XAsm.Ins $ XAsm.Sub Common.Quad (XAsm.Imm frameSize) Common.rsp ] ++
          map save (enum toSaveQuads)
      footer =
        [ XAsm.Symbol $ retLabel fnName ] ++
          map restore (enum toSaveQuads) ++
        [ XAsm.Ins $ XAsm.Add Common.Quad (XAsm.Imm frameSize) Common.rsp,
          XAsm.Ins XAsm.Ret ]
  in  (header, footer)
  where enum = zip [0..]
        toSaveQuads = map (Common.R Common.Quad) toSave
        save (index, reg) =
          let ind = XAsm.Ind Common.rsp $ calleeSaveOffset index frameSize
          in  XAsm.Ins $ XAsm.Mov2Mem Common.Quad reg ind
        restore (index, reg) =
          let ind = XAsm.Ind Common.rsp $ calleeSaveOffset index frameSize
          in  XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Mem ind) reg

-- Gets the footer for a fully translated XAsm program.
-- This contains common blocks for error-throwing.
progFooter :: Job.Job -> FnList -> [XAsm.Line]
progFooter job fnList =
  let arithErrorBlock =
        [ XAsm.Symbol arithErrorLabel,
          XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 8) Common.rdi,
          XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 0) Common.rax,
          XAsm.Ins $ XAsm.Call $ Common.wrapFnName "raise" ]
      memErrorBlock =
        [ XAsm.Symbol memErrorLabel,
          XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 11) Common.rdi,
          XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 0) Common.rax,
          XAsm.Ins $ XAsm.Call $ Common.wrapFnName "raise" ]
      abortBlock =
        [ XAsm.Symbol abortLabel,
          XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 0) Common.rax,
          XAsm.Ins $ XAsm.Call $ Common.wrapFnName "abort" ]
      fnTable =
        [ XAsm.Pseudo "data" [],
          XAsm.Symbol fnTableLabel,
          XAsm.Pseudo "quad" ["0"] ] -- NOTE: dummy element
        ++ map (\fnName -> XAsm.Pseudo "quad" [fnName]) fnList
      errBlocks = if Job.safe job then arithErrorBlock ++ memErrorBlock else []
  in  errBlocks ++ abortBlock ++ fnTable

-- Labels for destinations in the header and footers.
arithErrorLabel :: String
arithErrorLabel = "LArithError"
memErrorLabel :: String
memErrorLabel = "LMemError"
abortLabel :: String
abortLabel = "LAbort"
retLabel :: String -> String
retLabel fnName = fnName ++ "_LRet"
fnTableLabel :: String
fnTableLabel = "LFnTable"

getLabelFor :: Common.Error -> String
getLabelFor Common.ArithError = "LArithError"
getLabelFor Common.MemError = "LMemError"
getLabelFor Common.AssertError = "LAbort"


-- Given the spill index of an arg which we wish to pass to a funciton from our current function,
-- gets its offset from %rsp.
spilledArgOffset :: Integer -> Int32
spilledArgOffset = spilledTmpOffset

-- Given the spill index of a tmp in our current function, gets its offset from %rsp.
spilledTmpOffset :: Integer -> Int32
spilledTmpOffset index = fromIntegral $ index * (Common.bytesForSize Common.Quad)

-- Given the spill index of a param passed to our current function and the current function's frame
-- size, gets its offset from %rsp.
spilledParamOffset :: Integer -> Int32 -> Int32
spilledParamOffset index frameSize =
  let rtnAddrSize = Common.bytesForSize Common.Quad
  in  (spilledTmpOffset index) + frameSize + rtnAddrSize

-- Given the index in the callee-saved registers list of a callee-saved register and the current
-- funciton's frame size, gets its offset from %rsp.
calleeSaveOffset :: Integer -> Int32 -> Int32
calleeSaveOffset index frameSize =
  -- TODO: can we base this on prev funcitons?
  frameSize - (fromIntegral $ index + 1) * (Common.bytesForSize Common.Quad)
