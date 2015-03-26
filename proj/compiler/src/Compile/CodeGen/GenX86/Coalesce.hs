{-
	This file contains algorithms for register coalescing and self-move elimination.
-}

module Compile.CodeGen.GenX86.Coalesce where

import qualified Job
import qualified Compile.Types.B2Asm as B2Asm
import qualified Compile.Types.Common as Common
import qualified Compile.CodeGen.GenX86.Common as XCommon

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Maybe as Maybe
import qualified Data.List as List

{-
  TODO:
    * can we combine rMap and aMap? It would make a lot of this more straitforward.
    * should we be coalescing/trimming the assignments from rMap as well as those from aMap?
    * standardize order in which we pass args here

    * should we convert some of the Map.Lookups back to Map.!'s to ensure correctness (if we know that
      something *should* be in the map)?
-}

type TmpIdent = Common.TmpIdent
type Move = (TmpIdent, TmpIdent)
type BlockMap = B2Asm.BlockMap
type TmpAssn = XCommon.TmpAssignments
type Interference = Map.Map TmpIdent (Set.Set TmpIdent, Set.Set XCommon.Assignment)

{-
  Implements Pereira and Palsbers's register coalescing algorithm, described here:
  \url{http://www.cs.ucla.edu/~palsberg/paper/aplas05.pdf}

  TODO: coalesce updates the Interfernece graph, but doesn't return the updated version
  because it's not used afterward. This is a little fragile.
-}
coalesce :: Job.Job -> BlockMap -> TmpAssn -> TmpAssn -> Interference -> (BlockMap, TmpAssn, TmpAssn)
coalesce job bMap aMap rMap interference =
  if Job.lvl job < 2
    then (bMap, aMap, rMap)
    else  let (bMap', aMap') = greedyCoalesce bMap aMap interference
              bMap'' = trimSelfMoves aMap' rMap bMap'
          in  (bMap'', aMap', rMap)

-- TODO: should this be preformed on rMap as well?
-- TODO: when we filter the moves from BMap, we drop info about their location in BMap.
-- Could we make this more efficient?
greedyCoalesce :: BlockMap -> TmpAssn -> Interference -> (BlockMap, TmpAssn)
greedyCoalesce bMap aMap interference =
  let moves = getMoves bMap
      (_, bMap', aMap') = foldl tryCoalesceMove (interference, bMap, aMap) moves
  in  (bMap', aMap')
  where -- Gets a list of (srcTmp, dstTmp) pairs that we move between.
        -- Does not consider reserved tmps.
        -- TODO: consider reserved tmps?
        getMoves :: BlockMap -> [Move]
        getMoves bMap = concatMap (\(B2Asm.BBlock ss _) -> Maybe.mapMaybe getMove ss) $ Map.elems bMap

        -- If an instruction is a move, gets (srcTmp, dstTmp).
        getMove :: B2Asm.SIns -> Maybe Move
        getMove (B2Asm.Set _ (B2Asm.STmp src) (B2Asm.DTmp dst)) = Just (src, dst)
        getMove _ = Nothing

        -- Checks if two non-reserved tmps interfere.
        interfere :: Interference -> TmpIdent -> TmpIdent -> Bool
        interfere interference t1 t2 =
          let (n1, _) = interference Map.! t1
              (n2, _) = interference Map.! t2
          in  Set.member t1 n2 || Set.member t2 n1

        -- Get's t's neighbors' colors in the interference graph.
        nColors :: Interference -> TmpAssn -> TmpIdent -> [XCommon.Assignment]
        nColors interference aMap t =
          let (n, rColors) = interference Map.! t
          in  Set.elems rColors ++ map ((Map.!) aMap) (Set.elems n)

        -- Attempts to coalesce the tmps involved in a move.
        -- If successful, returns the modified code and assignments.
        -- Otherwise, passes the old code and assignments through.
        tryCoalesceMove :: (Interference, BlockMap, TmpAssn) -> Move
                        -> (Interference, BlockMap, TmpAssn)
        tryCoalesceMove maps@(interference, bMap, aMap) (src, dst) =
          -- TODO: should we also check here to see if src -> dst is a self-move?
          if (Map.notMember src interference || Map.notMember dst interference ||
              interfere interference src dst)
          then maps
          else (let nc = nColors interference aMap src ++ nColors interference aMap dst
                in  (case validRegs List.\\ nc of
                      [] -> maps
                      color : _ -> coalesceMove maps (src, dst) color))

        {-
          Once we've found a color with which the coalesced tmps (src, dst) can be colored, do the
          coalescing. Pereira and Palsberg specify that src and dst should be condensed into a new
          tmp. For simplicity, we simply contract dst into src.

          Then, coalescing involves these steps:
            1.  In the interference graph, contract src and dst into src, and wire up all of dst's
                neighbors to src.
            2.  In the block map, replace dst with src.
            3.  In aMap, remove dst and map src to color.
        -}
        coalesceMove :: (Interference, BlockMap, TmpAssn) -> Move -> XCommon.Assignment -> (Interference, BlockMap, TmpAssn)
        coalesceMove (interference, bMap, aMap) (src, dst) color =
          let interference' = replaceInInterference interference dst src
              bMap' = replaceInBMap bMap dst src
              aMap' = Map.insert src color $ Map.delete dst aMap
          in  (interference', bMap', aMap')

        -- Replace all occurences of an old tmp in the interference graph with a new one.
        replaceInInterference :: Interference -> TmpIdent -> TmpIdent -> Interference
        replaceInInterference interference old new =
          let interference' =
                (case (Map.lookup old interference, Map.lookup new interference) of
                    (Nothing, _) -> interference
                    (Just (n, r), Nothing) ->
                      Map.insert new (n, r) $ Map.delete old interference
                    (Just (nOld, rOld), Just (nNew, rNew)) ->
                      let (n', r') = (Set.union nOld nNew, Set.union rOld rNew)
                      in Map.insert new (n', r') $ Map.delete old interference)
          in  Map.map (\(n1, r1) -> (replaceInSet n1 old new, r1)) interference'
          where replaceInSet :: (Ord a) => Set.Set a -> a -> a -> Set.Set a
                replaceInSet set old new = Set.insert new $ Set.delete old set

        -- Replace all occurences of an old tmp in the block map with a new one.
        replaceInBMap :: BlockMap -> TmpIdent -> TmpIdent -> BlockMap
        replaceInBMap bMap old new = Map.map (replaceInBlock old new) bMap
          where replaceInBlock :: TmpIdent -> TmpIdent -> B2Asm.Block -> B2Asm.Block
                replaceInBlock old new (B2Asm.BBlock ss j) =
                  B2Asm.BBlock (map (replaceInSIns old new) ss) j

                replaceInSIns :: TmpIdent -> TmpIdent -> B2Asm.SIns -> B2Asm.SIns
                replaceInSIns old new (B2Asm.Asop sz asop src dst) =
                  B2Asm.Asop sz asop (replaceInSrc old new src) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.Set sz src dst) =
                  B2Asm.Set sz (replaceInSrc old new src) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.ToReserved sz src rTmp) =
                  B2Asm.ToReserved sz (replaceInSrc old new src) rTmp
                replaceInSIns old new (B2Asm.FromReserved sz rTmp dst) =
                  B2Asm.FromReserved sz rTmp (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.Load sz mem dst) =
                  B2Asm.Load sz (replaceInMem old new mem) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.Store sz src mem) =
                  B2Asm.Store sz (replaceInSrc old new src) (replaceInMem old new mem)
                replaceInSIns old new (B2Asm.Cmp sz src dst) =
                  B2Asm.Cmp sz (replaceInSrc old new src) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.Test sz src dst) =
                  B2Asm.Test sz (replaceInSrc old new src) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.CSet sz cmp src dst) =
                  B2Asm.CSet sz cmp (replaceInSrc old new src) (replaceInDst old new dst)
                replaceInSIns old new (B2Asm.Memcpy bytes src) =
                  B2Asm.Memcpy bytes (replaceInSrc old new src)
                replaceInSIns old new ins@(B2Asm.Call fnName) =
                  ins
                replaceInSIns old new (B2Asm.CallPtr src) =
                  B2Asm.CallPtr (replaceInSrc old new src)
                replaceInSIns old new (B2Asm.CheckNonZero sz src err) =
                  B2Asm.CheckNonZero sz (replaceInSrc old new src) err
                replaceInSIns old new (B2Asm.CheckNonNeg sz src err) =
                  B2Asm.CheckNonNeg sz (replaceInSrc old new src) err
                replaceInSIns old new (B2Asm.CheckEqual sz s1 s2 err) =
                  B2Asm.CheckEqual sz (replaceInSrc old new s1) (replaceInSrc old new s2) err
                replaceInSIns old new (B2Asm.CheckDivOverflow s1 s2) =
                  B2Asm.CheckDivOverflow (replaceInSrc old new s1) (replaceInSrc old new s2)
                replaceInSIns old new (B2Asm.CheckShiftMax src) =
                  B2Asm.CheckShiftMax (replaceInSrc old new src)
                replaceInSIns old new (B2Asm.CheckArrEnd s1 s2) =
                  B2Asm.CheckArrEnd (replaceInSrc old new s1) (replaceInSrc old new s2)

                replaceInMem :: TmpIdent -> TmpIdent -> B2Asm.Mem -> B2Asm.Mem
                replaceInMem old new (B2Asm.ArrMem esz s1 s2 offset) =
                  B2Asm.ArrMem esz (replaceInSrc old new s1) (replaceInSrc old new s2) offset
                replaceInMem old new (B2Asm.PtrMem src offset) =
                  B2Asm.PtrMem (replaceInSrc old new src) offset

                replaceInSrc :: TmpIdent -> TmpIdent -> B2Asm.Src -> B2Asm.Src
                replaceInSrc old new (B2Asm.STmp t) = B2Asm.STmp $ if t == old then new else t
                replaceInSrc old new src@(B2Asm.Imm _) = src
                replaceInSrc old new src@(B2Asm.FnPtr _) = src

                replaceInDst :: TmpIdent -> TmpIdent -> B2Asm.Dst -> B2Asm.Dst
                replaceInDst old new (B2Asm.DTmp t) = B2Asm.DTmp $ if t == old then new else t

        -- Valid registers for assigning to tmps.
        validRegs :: [XCommon.Assignment]
        validRegs = map XCommon.Reg (Common.callerSaveRegs ++ Common.calleeSaveRegs)

trimSelfMoves :: TmpAssn -> TmpAssn -> B2Asm.BlockMap -> B2Asm.BlockMap
trimSelfMoves aMap rMap bMap = Map.map (trimBlock aMap rMap) bMap
  where trimBlock :: TmpAssn -> TmpAssn -> B2Asm.Block -> B2Asm.Block
        trimBlock aMap rMap (B2Asm.BBlock ss j) = B2Asm.BBlock (filter (not . isSelfMove aMap rMap) ss) j

        isSelfMove :: TmpAssn -> TmpAssn -> B2Asm.SIns -> Bool
        isSelfMove aMap _ (B2Asm.Set _ (B2Asm.STmp tSrc) (B2Asm.DTmp tDst)) =
          case (aMap Map.! tSrc, aMap Map.! tDst) of
            (XCommon.Reg r1, XCommon.Reg r2) -> (r1 == r2)
            (XCommon.Spill i1, XCommon.Spill i2) -> (i1 == i2)
            _ -> False
        isSelfMove aMap rMap (B2Asm.ToReserved _ (B2Asm.STmp tSrc) (B2Asm.RTmp tR)) =
          case (aMap Map.! tSrc, rMap Map.! tR) of
            (XCommon.Reg r1, XCommon.Reg r2) -> (r1 == r2)
            (XCommon.Spill i1, XCommon.Spill i2) -> (i1 == i2)
            _ -> False
        isSelfMove aMap rMap (B2Asm.FromReserved _ (B2Asm.RTmp tR) (B2Asm.DTmp tDst)) =
          case (rMap Map.! tR, aMap Map.! tDst) of
            (XCommon.Reg r1, XCommon.Reg r2) -> (r1 == r2)
            (XCommon.Spill i1, XCommon.Spill i2) -> False -- TODO: reexamine this line
            _ -> False
        isSelfMove _ _ _ = False
        -- TODO: do we need to add more cases to deal with B2Asm.Ref?
