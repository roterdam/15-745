{-
  Transforms 2-argument abstract assembly into a representation of x86-64
  assembly (which can easily be stringified into the contents of a full-fledged
  x86-64 assembly file).
-}

-- TODO: stack offsets shouldn't actually be Int32s. Maybe Int64 or integer?

{-
  TODO: function pointers
  * make sure all calls to transfn get access to a map from function labels to IDS.
    this should be really easy to generate (just by enumerating the list of function idents)
  * use same map to generate the jump table at the end
  * replace fnptr src's with IDS
  * replace callptrs with calls into our jump table (we'll need to work out the mechanics of this)
-}

module Compile.CodeGen.GenX86.GenX86 where

import qualified Job
import qualified Compile.Types.B2Asm as B2Asm
import qualified Compile.Types.XAsm as XAsm
import qualified Compile.Types.Common as Common
import qualified Compile.CodeGen.GenX86.Assign as Assign
import qualified Compile.CodeGen.GenX86.Calling as Calling
import Compile.CodeGen.GenX86.Common (Assignment(..), TransMaps, FnList)

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Int (Int32)
import Data.Word (Word8)

import Debug.Trace (trace)
trace' msg x = trace (msg ++ show x) x

{-
  Converts B2Asm code to XAsm.
  Note that we pass around a list where a function's index is an integer ident, and that there is a
  dummy element at the start of this list.
-}
twoToX86 :: Job.Job -> B2Asm.B2Asm -> XAsm.XAsm
twoToX86 job (B2Asm.Prog fns) =
  let fnList = [""] ++ List.nub (concatMap getNamesFn fns)
      body = concatMap (transFn job fnList) fns
  in  XAsm.Prog $ body ++ Calling.progFooter job fnList
  where -- Gets the names of functions referenced (with the & operator) in a function.
        getNamesFn :: B2Asm.Fn -> FnList
        getNamesFn (B2Asm.Fn _ bMap) = concatMap getNamesBlock $ Map.elems bMap

        getNamesBlock :: B2Asm.Block -> FnList
        getNamesBlock (B2Asm.BBlock ss _) = concatMap getNamesSIns ss

        getNamesSIns :: B2Asm.SIns -> FnList
        getNamesSIns (B2Asm.Asop _ _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.Set _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.ToReserved _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.FromReserved _ _ _) = []
        getNamesSIns (B2Asm.Load _ _ _) = []
        getNamesSIns (B2Asm.Store _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.Cmp _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.Test _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.CSet _ _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.Memcpy _ src) = getNamesSrc src
        getNamesSIns (B2Asm.Call _) = []
        getNamesSIns (B2Asm.CallPtr src) = getNamesSrc src
        getNamesSIns (B2Asm.CheckNonZero _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.CheckNonNeg _ src _) = getNamesSrc src
        getNamesSIns (B2Asm.CheckEqual _ src1 src2 _) = getNamesSrc src1 ++ getNamesSrc src2
        getNamesSIns (B2Asm.CheckDivOverflow src1 src2) = getNamesSrc src1 ++ getNamesSrc src2
        getNamesSIns (B2Asm.CheckShiftMax src) = getNamesSrc src
        getNamesSIns (B2Asm.CheckArrEnd src1 src2) = getNamesSrc src1 ++ getNamesSrc src2

        getNamesSrc :: B2Asm.Src -> FnList
        getNamesSrc (B2Asm.STmp _) = []
        getNamesSrc (B2Asm.Imm _) = []
        getNamesSrc (B2Asm.FnPtr fnName) = [fnName]

{-
  Translates one function from B2Asm to XAsm.

  Steps:
    - Build a map assigning each tmp to a register or location on the stack.
    - Calculate the frame size.
    - Traverse the B2Asm code, replacing tmps with their assigns regs.
    - Add calling convention code.
-}
transFn :: Job.Job -> FnList -> B2Asm.Fn -> [XAsm.Line]
transFn job fnList (B2Asm.Fn fnName bMap) =
  let (bMap', aMap, rMap) = Assign.getAssignments job (B2Asm.Fn fnName bMap)
      tMaps = (aMap, rMap, fnList)
      toSave = Calling.getRegsToSave tMaps
      frameSize = Calling.getFrameSize toSave tMaps
      body = concatMap (transBlock tMaps frameSize) $ Map.assocs bMap'
      (header, footer) = Calling.fnHeaderFooter toSave frameSize fnName
  in  header ++ body ++ footer
  where transBlock :: TransMaps -> Int32 -> (Common.BlockIdent, B2Asm.Block) -> [XAsm.Line]
        transBlock tMaps frameSize (bNum, B2Asm.BBlock ss j) =
          let labelIns = XAsm.Symbol $ labelStr fnName bNum
              ssss = concatMap (transSIns tMaps frameSize) ss
              ssj = transJIns j
          in [labelIns] ++ ssss ++ ssj

        transSIns :: TransMaps -> Int32 -> B2Asm.SIns -> [XAsm.Line]
        transSIns tMaps _ (B2Asm.Asop sz op src dst) =
          let newS = transTmpSrc tMaps sz src
              (loadD, newD, storeD) = transTmpDst tMaps sz dst
              scratchReg = getScratchReg sz
              axReg = Common.R sz Common.AX
              cxReg = Common.R sz Common.CX
              dxReg = Common.R sz Common.DX
              code = case op of
                Common.AAdd -> [XAsm.Ins $ XAsm.Add sz newS newD]
                Common.ASub -> [XAsm.Ins $ XAsm.Sub sz newS newD]
                Common.AMul -> [XAsm.Ins $ XAsm.IMul sz newS newD]
                Common.AAnd -> [XAsm.Ins $ XAsm.And sz newS newD]
                Common.AOr -> [XAsm.Ins $ XAsm.Or sz newS newD]
                Common.AXor -> [XAsm.Ins $ XAsm.Xor sz newS newD]
                Common.ADiv -> [
                  XAsm.Ins $ XAsm.Mov sz (XAsm.dstToSrc newD) axReg,
                  XAsm.Ins $ XAsm.Ext sz,
                  XAsm.Ins $ XAsm.Mov sz newS scratchReg,
                  XAsm.Ins $ XAsm.IDiv sz scratchReg,
                  XAsm.Ins $ XAsm.Mov sz (XAsm.Reg axReg) newD]
                Common.AMod -> [
                  XAsm.Ins $ XAsm.Mov sz (XAsm.dstToSrc newD) axReg,
                  XAsm.Ins $ XAsm.Ext sz,
                  XAsm.Ins $ XAsm.Mov sz newS scratchReg,
                  XAsm.Ins $ XAsm.IDiv sz scratchReg,
                  XAsm.Ins $ XAsm.Mov sz (XAsm.Reg dxReg) newD]
                Common.AShL -> [
                    XAsm.Ins $ XAsm.Mov sz newS cxReg,
                    XAsm.Ins $ XAsm.SaL sz newD]
                Common.AShR -> [
                    XAsm.Ins $ XAsm.Mov sz newS cxReg,
                    XAsm.Ins $ XAsm.SaR sz newD]
          in loadD ++ code ++ storeD
        transSIns tMaps _ (B2Asm.Set sz src dst) =
          let newS = transTmpSrc tMaps sz src
              (_, newD, storeD) = transTmpDst tMaps sz dst
              code = [XAsm.Ins $ XAsm.Mov sz newS newD]
          in code ++ storeD
        transSIns tMaps _ (B2Asm.ToReserved sz src rDst) =
          let newS = transTmpSrc tMaps sz src
              (storeD, newD) = transArgDst tMaps sz rDst
              code = [XAsm.Ins $ XAsm.Mov sz newS newD]
          in code ++ storeD
        transSIns tMaps frameSize (B2Asm.FromReserved sz rSrc dst) =
          let newS = transParamSrc tMaps frameSize sz rSrc
              (_, newD, storeD) = transTmpDst tMaps sz dst
              code = [XAsm.Ins $ XAsm.Mov sz newS newD]
          in code ++ storeD
        transSIns tMaps _ (B2Asm.Load sz mem dst) =
          let (memSs, memInd) = transMem tMaps mem
              (_, newD, storeD) = transTmpDst tMaps sz dst
              code = [XAsm.Ins $ XAsm.Mov sz (XAsm.Mem memInd) newD]
          in memSs ++ code ++ storeD
        transSIns tMaps _ (B2Asm.Store sz src mem) =
          let newS = transTmpSrc tMaps sz src
              (memSs, memInd) = transMem tMaps mem
              scratchReg2 = getSpillReg sz
              code = [XAsm.Ins $ XAsm.Mov sz newS scratchReg2,
                      XAsm.Ins $ XAsm.Mov2Mem sz scratchReg2 memInd]
          in memSs ++ code
        transSIns tMaps _ (B2Asm.Cmp sz src dst) =
          let newS = transTmpSrc tMaps sz src
              (loadD, newD, _) = transTmpDst tMaps sz dst
              code = [XAsm.Ins $ XAsm.Cmp sz newS newD]
          in loadD ++ code
        transSIns tMaps _ (B2Asm.Test sz src dst) =
          let newS = transTmpSrc tMaps sz src
              (loadD, newD, _) = transTmpDst tMaps sz dst
              scratchReg = getScratchReg sz
              code = [XAsm.Ins $ XAsm.Mov sz (XAsm.Reg newD) scratchReg,
                      XAsm.Ins $ XAsm.Xor sz newS scratchReg,
                      XAsm.Ins $ XAsm.Test sz (XAsm.Reg scratchReg) scratchReg]
          in loadD ++ code
        -- Bytes have to be treated differently because CMov doesn't exist for Byte registers.
        transSIns tMaps _ (B2Asm.CSet Common.Byte cmp src dst) =
          let newS = transTmpSrc tMaps Common.Byte src
              (loadD, newD, storeD) = transTmpDst tMaps Common.Byte dst
              code = [XAsm.Ins $ XAsm.Mov Common.Byte newS scratchByte,
                      XAsm.Ins $ XAsm.Mov Common.Byte (XAsm.Reg newD) scratchByte2,
                      XAsm.Ins $ XAsm.CMov cmp Common.Long scratchLong scratchLong2,
                      XAsm.Ins $ XAsm.Mov Common.Byte (XAsm.Reg scratchByte2) newD]
          in loadD ++ code ++ storeD
        transSIns tMaps _ (B2Asm.CSet sz cmp src dst) =
          let newS = transTmpSrc tMaps sz src
              (loadD, newD, storeD) = transTmpDst tMaps sz dst
              scratchReg = getScratchReg sz
              code = [XAsm.Ins $ XAsm.Mov sz newS scratchReg,
                      XAsm.Ins $ XAsm.CMov cmp sz scratchReg newD]
          in loadD ++ code ++ storeD
        transSIns tMaps _ (B2Asm.Memcpy bytes src) =
          let ptrSrc = transTmpSrc tMaps Common.Quad src
              loadS = [XAsm.Ins $ XAsm.Mov Common.Quad ptrSrc scratchQuad]
          in loadS ++ (concatMap (getCopyInss scratchQuad) $ zip bytes [0..])
          where getCopyInss :: Common.Reg -> (Word8, Int32) -> [XAsm.Line]
                getCopyInss ptrReg (x, offset) =
                  [XAsm.Ins $ XAsm.Mov Common.Byte (XAsm.Imm $ fromIntegral x) scratchByte2,
                   XAsm.Ins $ XAsm.Mov2Mem Common.Byte scratchByte2 $ XAsm.Ind ptrReg offset]
        transSIns tMaps _ (B2Asm.Call name) =
          [XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Imm 0) Common.rax,
           XAsm.Ins $ XAsm.Call name]

        -- TODO: should CallPtr here take a size?
        transSIns tMaps _ (B2Asm.CallPtr src) =
          let fnPtrSrc = transTmpSrc tMaps Common.Quad src
          in [XAsm.Ins $ XAsm.Mov Common.Quad fnPtrSrc scratchQuad,
              XAsm.Ins $ XAsm.IMul Common.Quad (XAsm.Imm $ Common.bytesForSize Common.Quad) scratchQuad,
              XAsm.Ins $ XAsm.LoadLbl Calling.fnTableLabel scratchQuad2,
              XAsm.Ins $ XAsm.Add Common.Quad (XAsm.Reg scratchQuad) scratchQuad2,
              XAsm.Ins $ XAsm.Mov Common.Quad (XAsm.Mem $ XAsm.Ind scratchQuad2 0) scratchQuad,
              XAsm.Ins $ XAsm.CallPtr $ XAsm.Reg scratchQuad]
        transSIns tMaps _ (B2Asm.CheckNonZero sz src err)
          | Job.safe job =
            let (scratchReg, errorLabel) = (getScratchReg sz, Calling.getLabelFor err)
            in [XAsm.Ins $ XAsm.Mov sz (transTmpSrc tMaps sz src) scratchReg,
                XAsm.Ins $ XAsm.Test sz (XAsm.Reg scratchReg) scratchReg,
                XAsm.Ins $ XAsm.Jmpcc Common.E errorLabel]
          | otherwise = []
        transSIns tMaps _ (B2Asm.CheckNonNeg sz src err)
          | Job.safe job =
            let (scratchReg, errorLabel) = (getScratchReg sz, Calling.getLabelFor err)
            in [XAsm.Ins $ XAsm.Mov sz (XAsm.Imm 0) scratchReg,
                XAsm.Ins $ XAsm.Cmp sz (transTmpSrc tMaps sz src) scratchReg,
                XAsm.Ins $ XAsm.Jmpcc Common.G errorLabel]
          | otherwise = []
        transSIns tMaps _ (B2Asm.CheckEqual sz src1 src2 err)
          | Job.safe job =
            let (newS1, newS2) = (transTmpSrc tMaps sz src1, transTmpSrc tMaps sz src2)
                (scratchReg, errorLabel) = (getScratchReg sz, Calling.getLabelFor err)
            in [XAsm.Ins $ XAsm.Mov sz newS1 scratchReg,
                XAsm.Ins $ XAsm.Cmp sz newS2 scratchReg,
                XAsm.Ins $ XAsm.Jmpcc Common.NE errorLabel]
          | otherwise = []
        transSIns tMaps _ (B2Asm.CheckDivOverflow l r)
          | Job.safe job =
              let (l', r') = (transTmpSrc tMaps Common.Long l, transTmpSrc tMaps Common.Long r)
              in [XAsm.Ins $ XAsm.Mov Common.Long r' scratchLong,
                  XAsm.Ins $ XAsm.Mov Common.Long (XAsm.Imm 0) scratchLong2,
                  XAsm.Ins $ XAsm.Cmp Common.Long (XAsm.Imm (-1)) scratchLong,
                  XAsm.Ins $ XAsm.CMov Common.NE Common.Long scratchLong2 scratchLong,
                  XAsm.Ins $ XAsm.And Common.Long l' scratchLong,
                  XAsm.Ins $ XAsm.Cmp Common.Long (XAsm.Imm minInt) scratchLong,
                  XAsm.Ins $ XAsm.Jmpcc Common.E $ Calling.arithErrorLabel]
          | otherwise = []
        transSIns tMaps _ (B2Asm.CheckShiftMax src)
          | Job.safe job =
            let newS = transTmpSrc tMaps Common.Long src
            in [XAsm.Ins $ XAsm.Mov Common.Long newS scratchLong,
                XAsm.Ins $ XAsm.Cmp Common.Long (XAsm.Imm 32) scratchLong,
                XAsm.Ins $ XAsm.Jmpcc Common.GE $ Calling.arithErrorLabel]
          | otherwise = []
        transSIns tMaps _ (B2Asm.CheckArrEnd addrSrc idxSrc)
          | Job.safe job =
            let newAddrSrc = transTmpSrc tMaps Common.Quad addrSrc
                newIdxSrc = transTmpSrc tMaps Common.Long idxSrc
            in [XAsm.Ins $ XAsm.Mov Common.Quad newAddrSrc scratchQuad,
                XAsm.Ins $ XAsm.Mov Common.Long (XAsm.Mem $ XAsm.Ind scratchQuad 0) scratchLong,
                XAsm.Ins $ XAsm.Cmp Common.Long newIdxSrc scratchLong,
                XAsm.Ins $ XAsm.Jmpcc Common.LE Calling.memErrorLabel]
          | otherwise = []

        transJIns :: B2Asm.JIns -> [XAsm.Line]
        transJIns B2Asm.Ret =
          [XAsm.Ins $ XAsm.Jmp $ Calling.retLabel fnName]
        transJIns (B2Asm.Jmp b) =
          [XAsm.Ins $ XAsm.Jmp $ labelStr fnName b]
        transJIns (B2Asm.Jmpcc cmp bt bf) =
          [XAsm.Ins $ XAsm.Jmpcc cmp $ labelStr fnName bt,
           XAsm.Ins $ XAsm.Jmp $ labelStr fnName bf]
        transJIns (B2Asm.Raise err) =
          [XAsm.Ins $ XAsm.Jmp $ Calling.getLabelFor err]

        {-
          Given a reserved temp acting as a Src, gets it's XAsm Src.

          NOTE: This will only occur with a Spill when we're in a *callee*, fetching Spilled
                args from our caller's stack frame. It may also occur with a reg in this case.

          It may also occur with a reg when we're manipulating %rax.
        -}
        transParamSrc :: TransMaps -> Int32 -> Common.Size -> B2Asm.RTmp -> XAsm.Src
        transParamSrc (_, rMap, _) frameSize sz (B2Asm.RTmp t) =
          case (rMap Map.! t) of
            Spill index ->
              XAsm.Mem $ XAsm.Ind Common.rsp $ Calling.spilledParamOffset index frameSize
            Reg rName ->
              XAsm.Reg $ Common.R sz rName

        {-
          Given a reserved temp acting as a Dst, returns:

            (storeD, newDQ)

          where
            - newDQ = an apparent location for the value
            - storeD = code to run to store the value in newDQ to its real location

          NOTE: This will only occur with a Spill when we're in a *caller*, placing spilled
                args into our own stack frame. It may also occur with a reg in this case.

          It may also occur with a reg when we're manipulating %rax.
        -}
        -- TODO: share more code with transTmpDst?
        transArgDst :: TransMaps -> Common.Size -> B2Asm.RTmp -> ([XAsm.Line], Common.Reg)
        transArgDst (_, rMap, _) sz (B2Asm.RTmp t) =
          case (rMap Map.! t) of
            Spill index ->
              let ind = XAsm.Ind Common.rsp $ Calling.spilledArgOffset index
                  spillReg = getSpillReg sz
                  store = [XAsm.Ins $ XAsm.Mov2Mem sz spillReg ind]
              in (store, spillReg)
            Reg rName -> ([], Common.R sz rName)

        {-
          Given a src tmp, gets its XAsm src.
        -}
        transTmpSrc :: TransMaps -> Common.Size -> B2Asm.Src -> XAsm.Src
        transTmpSrc _ sz (B2Asm.Imm x) = XAsm.Imm $ truncateToSize sz x
        transTmpSrc (aMap, _, _) sz (B2Asm.STmp t) =
          case (aMap Map.! t) of
            Spill index ->
              XAsm.Mem $ XAsm.Ind Common.rsp $ Calling.spilledTmpOffset index
            Reg rName -> XAsm.Reg $ Common.R sz rName
        transTmpSrc (_, _, fnList) _ (B2Asm.FnPtr fnName) =
          let fnIdent = fromIntegral $ Maybe.fromJust $ List.elemIndex fnName fnList
          in  XAsm.Imm fnIdent

        {-
          Given a 4-byte immediate and a target size, truncates the given immediate to that
          size by using mod.
        -}
        truncateToSize :: Common.Size -> Int32 -> Int32
        truncateToSize Common.Byte x = x `mod` 256
        truncateToSize Common.Long x = x
        truncateToSize Common.Quad x = x

        {-
          Given a dest tmp, gets:

            (load, new, store)

            where:
              - new = an apparent location for the value
              - load = code to load the value into new
              - store = code to store the value in the apparent location in the actual location
        -}
        transTmpDst :: TransMaps -> Common.Size -> B2Asm.Dst
                    -> ([XAsm.Line], Common.Reg, [XAsm.Line])
        transTmpDst (aMap, _, _) sz (B2Asm.DTmp t) =
          case (aMap Map.! t) of
            (Spill index) ->
              let ind = XAsm.Ind Common.rsp $ Calling.spilledTmpOffset index
                  spillReg = getSpillReg sz
                  load = [XAsm.Ins $ XAsm.Mov sz (XAsm.Mem ind) spillReg]
                  store = [XAsm.Ins $ XAsm.Mov2Mem sz spillReg ind]
              in  (load, spillReg, store)
            (Reg rName) -> ([], Common.R sz rName, [])

        {-
          Generates the instructions needed to turn the given complex memory access into an
          indirect offset, and returns those instructions along with the offset.
        -}
        transMem :: TransMaps -> B2Asm.Mem -> ([XAsm.Line], XAsm.Ind)
        transMem tMaps (B2Asm.ArrMem esz addrSrc idxSrc offset) =
          let (newAddrSrc, newIdxSrc) = (transTmpSrc tMaps Common.Quad addrSrc, transTmpSrc tMaps Common.Long idxSrc)
          in ([XAsm.Ins $ XAsm.Mov Common.Long newIdxSrc scratchLong,
               XAsm.Ins $ XAsm.IMul Common.Long (XAsm.Imm esz) scratchLong,
               XAsm.Ins $ XAsm.Add Common.Quad newAddrSrc scratchQuad],
              XAsm.Ind scratchQuad (offset + 8))

        transMem tMaps (B2Asm.PtrMem addrSrc offset) =
          let newAddrSrc = transTmpSrc tMaps Common.Quad addrSrc
          in ([XAsm.Ins $ XAsm.Mov Common.Quad newAddrSrc scratchQuad],
              XAsm.Ind scratchQuad offset)


getScratchReg :: Common.Size -> Common.Reg
getScratchReg sz = Common.R sz Common.scratchReg

getSpillReg :: Common.Size -> Common.Reg
getSpillReg sz = Common.R sz Common.spillReg

scratchQuad = getScratchReg Common.Quad
scratchQuad2 = getSpillReg Common.Quad
scratchLong = getScratchReg Common.Long
scratchLong2 = getSpillReg Common.Long
scratchByte = getScratchReg Common.Byte
scratchByte2 = getSpillReg Common.Byte



shiftMin = 0
shiftMax = 32
minInt = minBound :: Int32
labelStr fnName n = fnName ++ "_L" ++ show n
