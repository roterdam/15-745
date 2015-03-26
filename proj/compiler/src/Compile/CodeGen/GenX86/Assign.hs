{-
  Gets assignments for every tmp in the given abstract assembly, where an assignment is either
  a register or a location on the stack.
-}

module Compile.CodeGen.GenX86.Assign where

import qualified Job
import qualified Compile.Types.B2Asm as B2Asm
import qualified Compile.Types.Common as Common
import qualified Compile.Graph.Graph as Graph
import qualified Compile.Graph.GraphAlgs as GraphAlgs
import qualified Compile.CodeGen.GenX86.Coalesce as Coalesce
import qualified Compile.CodeGen.GenX86.Common as XCommon

import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Data.List

type BlockMap = B2Asm.BlockMap

type LineNum = Integer

type TmpIdent = Common.TmpIdent
type TmpSet = Set.Set TmpIdent
type DefSet = TmpSet
type LiveSet = TmpSet
type AssignSet = Set.Set XCommon.Assignment

type LiveMap = Map.Map BlockIdent LiveSet
type DefMap = Map.Map BlockIdent DefSet
type PredecessorMap = Map.Map BlockIdent [BlockIdent]

{-
  (ss,
   j,
   <vars defined in block>,
   <vars locally known to be live at block start>,
   <vars known via propogation to be live at block start>,
-}
type BlockInfo = ([B2Asm.SIns], B2Asm.JIns, DefSet, LiveSet, LiveSet)
type BlockIdent = Common.BlockIdent

type BlockNode = (Set.Set BlockIdent, Set.Set BlockIdent, BlockInfo)
type BlockGraph = Graph.Graph BlockIdent BlockInfo

type Usages = Map.Map TmpIdent LineNum
type Liveness = Map.Map TmpIdent [LineNum]
type Interference = Map.Map TmpIdent (TmpSet, AssignSet)

{-
  Generates assignments for every non-reserved temp in the given assembly, given that reserved
  temps are pre-colored and some instructions require certain registers.

  Notable special instructions:
    shl/shr require ecx
    idiv requires eax and edx
    call implicitly requires all caller-save registers, since the called fn might modify them
  Note that even though many instruction require spill registers, we can't know which until
    after we register-allocate, so to avoid backtracking we don't give out spill registers for
    general use.

  Current algorithm:
    Build interference graph between unreserved temps, where each unreserved temp also has a set
      of registers/stack allocations it's know to conflict with because of reserved registers
      and special instructions
    Greedily assign based off of this graph (no particular temp order), in the order:
      caller-save registers
      callee-save registers
      stack space
-}
getAssignments :: Job.Job -> B2Asm.Fn -> (BlockMap, XCommon.TmpAssignments, XCommon.TmpAssignments)
getAssignments job (B2Asm.Fn name bMap) =
  let rTmpAssignments = getRTmpAssignments bMap
      numRTmps = toInteger $ Map.size rTmpAssignments
  in if numRTmps > maxRTmps
      then (bMap, getDefaultAssignments numRTmps bMap, rTmpAssignments)
      else  let interference = getInterferenceGraph bMap
                tmpAssignments = assignFromGraph interference
            in  Coalesce.coalesce job bMap tmpAssignments rTmpAssignments interference
  where maxRTmps = 32

        getDefaultAssignments :: Integer -> BlockMap -> XCommon.TmpAssignments
        getDefaultAssignments numRTmps bMap =
          let tmpSet = Map.foldl Set.union Set.empty $ Map.map getTmpSet bMap
              validAssignments = map XCommon.Spill [numRTmps..]
          in Map.fromList $ zip (Set.toList tmpSet) validAssignments

        getTmpSet :: B2Asm.Block -> TmpSet
        getTmpSet (B2Asm.BBlock ss _) = foldl processSIns Set.empty ss

        processSIns :: TmpSet -> B2Asm.SIns -> TmpSet
        processSIns tSet (B2Asm.Asop _ _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.Set _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.ToReserved _ _ _) = tSet
        processSIns tSet (B2Asm.FromReserved _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.Load _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.Store _ _ _) = tSet
        processSIns tSet (B2Asm.Cmp _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.Test _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.CSet _ _ _ (B2Asm.DTmp tDst)) = Set.insert tDst tSet
        processSIns tSet (B2Asm.Memcpy _ _) = tSet
        processSIns tSet (B2Asm.Call _) = tSet
        processSIns tSet (B2Asm.CallPtr _) = tSet
        processSIns tSet (B2Asm.CheckNonZero _ _ _) = tSet
        processSIns tSet (B2Asm.CheckNonNeg _ _ _) = tSet
        processSIns tSet (B2Asm.CheckEqual _ _ _ _) = tSet
        processSIns tSet (B2Asm.CheckDivOverflow _ _) = tSet
        processSIns tSet (B2Asm.CheckShiftMax _) = tSet
        processSIns tSet (B2Asm.CheckArrEnd _ _) = tSet

{-
  Generates an interference graph on the tmps in the given assembly. The basic idea is to extract
  all the needed information from the straight-line code at the start, and then work with the
  graph of blocks instead.

  WARNING: this doesn't bother computing the liveness information of reserved temps, since they
           only show up at the start and end of control flow paths, and just around function
           calls. If this ever stops being true, this code might have to be re-examined.

  Steps:
    1. Loop upwards through each block independently, and generate:
        a. The set of variables defined (i.e. written to) in the block.
        b. The set of variables live at the start of the block, assuming no liveness information
           propogated between blocks.
        c. The predecessors of the block.
    2. For each block, for each variable at that block, propogate its liveness to as many blocks
       as possible, where liveness propogates to the top of a block if it was live in a successor
       block and not defined in the current block.
    3. For each block, generate the set of temps live at the end of that block and loop upwards
       to get the interference generated by all the code in that block.
    4. For each v newly propogated to the top of a block, and each u written to in that block,
       add (u, v) to that block's interference graph.
    5. Merge all the interference graphs together to get the final result.
-}
getInterferenceGraph :: BlockMap -> Interference
getInterferenceGraph bMap =
  let bGraph = getBlockGraph bMap
      defaultInterference = getDefaultInterference bGraph
      bGraph' = propogateLiveness bGraph
      interference = getLocalInterference bGraph'
  in mergeInterferenceGraphs [defaultInterference, interference]
  where {-
          Extracts information from each block to build a graph over blocks. Note that the
          propSet is empty because propogation hasn't been done yet.
        -}
        getBlockGraph :: BlockMap -> BlockGraph
        getBlockGraph bMap = Graph.fromMapWith processBlock getSuccessors bMap
        {-
          Unions all the def sets to find the full set of temps used by the program,
          and makes a dummy interference graph with no edges out of them.
        -}
        getDefaultInterference :: BlockGraph -> Interference
        getDefaultInterference bg =
          let tmpSet = Graph.fold (\s -> (\(_, _, dSet, _, _s) -> Set.union dSet s)) Set.empty bg
          in Map.fromSet (\_ -> (Set.empty, Set.empty)) tmpSet
        {-
          Gets the successor blocks of the given block.
        -}
        getSuccessors :: B2Asm.Block -> [BlockIdent]
        getSuccessors (B2Asm.BBlock ss B2Asm.Ret) = []
        getSuccessors (B2Asm.BBlock ss (B2Asm.Jmp l)) = [l]
        getSuccessors (B2Asm.BBlock ss (B2Asm.Jmpcc _ l1 l2)) = [l1, l2]
        getSuccessors (B2Asm.BBlock ss (B2Asm.Raise _)) = []

{-
  Given a block of straight-line assembly terminated by a jump/ret, computes the following:
  1. A set of all the temps defined in the current block.
  2. A set of all the temps definitely live at the start of the block (doesn't account for
     variables live in subsequent blocks whose liveness gets propogated upwards).
  3. A list of the successor blocks of the current block.

  NOTE: since this is a local processing operation, the computed propSet is empty.
  NOTE2: the liveness of reserved temps isn't tracked, because our GenTwo should keep it minimal
-}
processBlock :: B2Asm.Block -> BlockInfo
processBlock (B2Asm.BBlock ss j) =
  let (dSet, lSet) = foldr processSIns (Set.empty, Set.empty) ss
  in (ss, j, dSet, lSet, Set.empty)
  where {-
          The first tuple represents the state just below the given instruction, the return
          triple represents the state just above.
          Reserved temps aren't tracked, so s -> r is just a read of s, and s <- r is just a
          write to s.
        -}
        processSIns :: B2Asm.SIns -> (DefSet, LiveSet) -> (DefSet, LiveSet)
        processSIns (B2Asm.Asop _ _ src (B2Asm.DTmp tDst)) (dSet, lSet) =
          (Set.insert tDst dSet, addIfTmp src $ Set.delete tDst lSet)
        processSIns (B2Asm.Set _ src (B2Asm.DTmp tDst)) (dSet, lSet) =
          (Set.insert tDst dSet, addIfTmp src $ Set.delete tDst lSet)
        processSIns (B2Asm.ToReserved _ src _) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.FromReserved _ _ (B2Asm.DTmp tDst)) (dSet, lSet) =
          (Set.insert tDst dSet, Set.delete tDst lSet)
        processSIns (B2Asm.Load _ mem (B2Asm.DTmp tDst)) (dSet, lSet) =
          (Set.insert tDst dSet, addMemTmps mem $ Set.delete tDst lSet)
        processSIns (B2Asm.Store _ src mem) (dSet, lSet) =
          (dSet, addIfTmp src $ addMemTmps mem lSet)
        processSIns (B2Asm.Cmp _ src (B2Asm.DTmp tDst)) (dSet, lSet) =
          (dSet, addIfTmp src $ Set.insert tDst lSet)
        processSIns (B2Asm.Test _ src (B2Asm.DTmp tDst)) (dSet, lSet) =
          (dSet, addIfTmp src $ Set.insert tDst lSet)
        processSIns (B2Asm.CSet _ _ src (B2Asm.DTmp tDst)) (dSet, lSet) =
          (Set.insert tDst dSet, addIfTmp src $ Set.delete tDst lSet)
        processSIns (B2Asm.Memcpy _ src) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.Call _) (dSet, lSet) =
          (dSet, lSet)
        processSIns (B2Asm.CallPtr src) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.CheckNonZero sz src err) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.CheckNonNeg sz src err) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.CheckEqual sz src1 src2 err) (dSet, lSet) =
          (dSet, addIfTmp src1 $ addIfTmp src2 lSet)
        processSIns (B2Asm.CheckDivOverflow l r) (dSet, lSet) =
          (dSet, addIfTmp l $ addIfTmp r lSet)
        processSIns (B2Asm.CheckShiftMax src) (dSet, lSet) =
          (dSet, addIfTmp src lSet)
        processSIns (B2Asm.CheckArrEnd addrSrc idxSrc) (dSet, lSet) =
          (dSet, addIfTmp addrSrc $ addIfTmp idxSrc lSet)
        {-
          Given a src and a set, if the src is a temp it adds it to the set, otherwise it does
          nothing.
        -}
        addIfTmp :: B2Asm.Src -> TmpSet -> TmpSet
        addIfTmp (B2Asm.STmp t) s = Set.insert t s
        addIfTmp _ s = s

        {-
          Returns the list of successors of the given JIns.
        -}
        getSuccsFromJ :: B2Asm.JIns -> [BlockIdent]
        getSuccsFromJ B2Asm.Ret = []
        getSuccsFromJ (B2Asm.Jmp l) = [l]
        getSuccsFromJ (B2Asm.Jmpcc _ l1 l2) = [l1, l2]
        getSuccsFromJ (B2Asm.Raise _) = []
        {-
          Given a set of variables and a memory location, adds any temps read as a part of
          evaluating the memory location into the set.
        -}
        addMemTmps :: B2Asm.Mem -> TmpSet -> TmpSet
        addMemTmps (B2Asm.ArrMem _ addrSrc indSrc _) s = addIfTmp addrSrc $ addIfTmp indSrc s
        addMemTmps (B2Asm.PtrMem addrSrc _) s = addIfTmp addrSrc s


{-
  Given the set of temps known to be live at the start of each block, and the set of temps
  defined within that block, propogates liveness as much as possible to find the full set of
  temps live at the start of each block. For later convenience it separates out those newly
  propogated temps from the ones that were live at the top of the block before propogation.

  Propogation rules: a variable is live at the start of a block if:
    1. It is locally known to be live at the start of that block.
    2. It is live at the start of a successor block, and not defined in the current block.
  Rule 1 has already been applied to saturation; the purpose of this function is to use that
  information to saturate rule 2.
-}
propogateLiveness :: BlockGraph -> BlockGraph
propogateLiveness bg =
  Graph.addMapData updatePropSet (GraphAlgs.propogateAllBackwards getLiveVars canPropVar bg) bg
  where getLiveVars :: BlockInfo -> [TmpIdent]
        getLiveVars (_, _, _, lSet, _) = Set.toList lSet
        canPropVar :: TmpIdent -> BlockInfo -> Bool
        canPropVar t (_, _, dSet, lSet, _) = not (t `Set.member` dSet || t `Set.member` lSet)
        updatePropSet :: LiveSet -> BlockInfo -> BlockInfo
        updatePropSet pSet (ss, j, dSet, lSet, _) = (ss, j, dSet, lSet, pSet)

{-
  Gets the interference graph resulting from looping upwards through each block, starting each
  block with the union of the live sets for all its successor blocks, and doing the lab1
  interference algorithm for straight-line code. Note that things are a little bit more
  complicated now, because both special instructions and precolored temps are managed
  explicitly:
  --> To/FromReserved act like Set, but interference with a reserved temp is interference with
      a specific (predefined) register.
  --> Call implicilty writes to all caller-save registers.
  --> a /= b and a %= b generate extra interference with %eax and %edx
  --> a <<= b and a >>= b generate extra interference with %ecx
-}
getLocalInterference :: BlockGraph -> Interference
getLocalInterference bg =
  let blockInterference = map (getBlockInterference bg) $ Graph.nodes bg
  in mergeInterferenceGraphs $ blockInterference
  where getBlockInterference :: BlockGraph -> BlockNode -> Interference
        getBlockInterference bg (_, succs, (ss, j, _, _, pSet)) =
          let lSet = Set.unions $ map (getBlockLiveness bg) $ Set.toList succs
              (_, _, interference) = foldr processSIns (lSet, Set.empty, Map.empty) ss
          in interference

        getBlockLiveness :: BlockGraph -> BlockIdent -> LiveSet
        getBlockLiveness bg b = -- TODO: this is doing extra work; I should fix that
          let (_, _, _, liveSet, propSet) = bg Graph.! b
          in Set.union liveSet propSet

        processSIns :: B2Asm.SIns -> (LiveSet, AssignSet, Interference)
                    -> (LiveSet, AssignSet, Interference)
        processSIns (B2Asm.Asop _ Common.ADiv src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          idivCase src tDst (lSet, aSet, ig)
        processSIns (B2Asm.Asop _ Common.AMod src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          idivCase src tDst (lSet, aSet, ig)
        processSIns (B2Asm.Asop _ Common.AShL src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          shiftCase src tDst (lSet, aSet, ig)
        processSIns (B2Asm.Asop _ Common.AShR src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          shiftCase src tDst (lSet, aSet, ig)
        processSIns (B2Asm.Asop _ _ src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          let ig' = addConflictsWithTmp tDst (Set.delete tDst lSet) ig
              ig'' = addConflictWithAssignments aSet tDst ig'
          in (Set.insert tDst $ addIfTmp src lSet, aSet, ig'')
        processSIns (B2Asm.Set _ src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          let lSet' = Set.delete tDst lSet
              ig' = addConflictsWithTmp tDst (removeIfTmp src lSet') ig
              ig'' = addConflictWithAssignments aSet tDst ig'
          in (addIfTmp src lSet', aSet, ig'')
        processSIns (B2Asm.ToReserved _ src (B2Asm.RTmp r)) (lSet, aSet, ig) =
          let ig' = addConflictsWithAssignment (removeIfTmp src lSet) ig (assignRTmp r)
          in (addIfTmp src lSet, Set.delete (assignRTmp r) aSet, ig')
        processSIns (B2Asm.FromReserved _ (B2Asm.RTmp r) (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          let lSet' = Set.delete tDst lSet
              ig' = addConflictsWithTmp tDst lSet' ig
              ig'' = addConflictWithAssignments aSet tDst ig'
          in (lSet', Set.insert (assignRTmp r) aSet, ig'')
        processSIns (B2Asm.Load _ mem (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          let lSet' = Set.delete tDst lSet
              ig' = addConflictsWithTmp tDst lSet' ig
              ig'' = addConflictWithAssignments aSet tDst ig'
          in (addMemTmps mem lSet', aSet, ig'')
        processSIns (B2Asm.Store _ src mem) (lSet, aSet, ig) =
          (addIfTmp src $ addMemTmps mem lSet, aSet, ig)
        processSIns (B2Asm.Cmp _ src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          (addIfTmp src $ Set.insert tDst lSet, aSet, ig)
        processSIns (B2Asm.Test _ src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          (addIfTmp src $ Set.insert tDst lSet, aSet, ig)
        processSIns (B2Asm.CSet _ _ src (B2Asm.DTmp tDst)) (lSet, aSet, ig) =
          let ig' = addConflictsWithTmp tDst (Set.delete tDst lSet) ig
              ig'' = addConflictWithAssignments aSet tDst ig'
          in (Set.insert tDst $ addIfTmp src lSet, aSet, ig'')
        processSIns (B2Asm.Memcpy _ src) (lSet, aSet, ig) =
          (addIfTmp src lSet, aSet, ig)
        processSIns (B2Asm.Call _) (lSet, aSet, ig) =
          let ig' = foldl (addConflictsWithAssignment lSet) ig
                          (map XCommon.Reg Common.callerSaveRegs)
          in (lSet, Set.empty, ig')
        processSIns (B2Asm.CallPtr src) (lSet, aSet, ig) =
          let ig' = foldl (addConflictsWithAssignment lSet) ig
                          (map XCommon.Reg Common.callerSaveRegs)
          in (addIfTmp src lSet, Set.empty, ig')
        processSIns (B2Asm.CheckNonZero _ src _) (lSet, aSet, ig) =
          (addIfTmp src lSet, aSet, ig)
        processSIns (B2Asm.CheckNonNeg _ src _) (lSet, aSet, ig) =
          (addIfTmp src lSet, aSet, ig)
        processSIns (B2Asm.CheckEqual _ src1 src2 _) (lSet, aSet, ig) =
          (addIfTmp src1 $ addIfTmp src2 lSet, aSet, ig)
        processSIns (B2Asm.CheckDivOverflow l r) (lSet, aSet, ig) =
          (addIfTmp l $ addIfTmp r lSet, aSet, ig)
        processSIns (B2Asm.CheckShiftMax src) (lSet, aSet, ig) =
          (addIfTmp src lSet, aSet, ig)
        processSIns (B2Asm.CheckArrEnd addrSrc indSrc) (lSet, aSet, ig) =
          (addIfTmp addrSrc $ addIfTmp indSrc lSet, aSet, ig)

        {-
          a /= b produces the following conflicts:
            anything live on the following line that isn't a conflicts with a, eax, edx
            b conflicts with edx
            if b != a, b also conflicts with eax
        -}
        idivCase :: B2Asm.Src -> TmpIdent -> (LiveSet, AssignSet, Interference)
                 -> (LiveSet, AssignSet, Interference)
        idivCase src tDst (lSet, aSet, ig) =
          let lSet' = Set.delete tDst lSet
              ig' = addConflictsWithTmp tDst lSet' ig
              ig'' = addConflictsWithAssignment lSet' ig' (XCommon.Reg Common.AX)
              ig''' = addConflictsWithAssignment lSet' ig'' (XCommon.Reg Common.DX)
              ig'''' = (case src of
                          (B2Asm.Imm _) -> ig'''
                          (B2Asm.STmp tSrc) ->
                            let ig'''' = addConflictWithAssignment (XCommon.Reg Common.DX) tSrc ig'''
                            in (if tSrc == tDst
                                then ig''''
                                else addConflictWithAssignment (XCommon.Reg Common.AX) tSrc ig'''')
                          (B2Asm.FnPtr fnName) -> error "Assign: division by function pointer")
              ig''''' = (if (((XCommon.Reg Common.AX) `Set.member` aSet) ||
                             ((XCommon.Reg Common.DX) `Set.member` aSet))
                         then error "eax or edx required live at time of idiv call"
                         else addConflictWithAssignments aSet tDst ig'''')
          in (Set.insert tDst $ addIfTmp src lSet, aSet, ig''''')

        {-
          A <<= b (or a >>= b) produces the following conflicts:
            anything live on the following line that isn't a conflicts with a, ecx.
            if a != b, a conflicts with ecx.
        -}
        shiftCase :: B2Asm.Src -> TmpIdent -> (LiveSet, AssignSet, Interference)
                  -> (LiveSet, AssignSet, Interference)
        shiftCase src tDst (lSet, aSet, ig) =
          let lSet' = Set.delete tDst lSet
              ig' = addConflictsWithTmp tDst lSet' ig
              ig'' = addConflictsWithAssignment lSet' ig' (XCommon.Reg Common.CX)
              ig''' = (case src of
                          (B2Asm.Imm _) -> addConflictWithAssignment (XCommon.Reg Common.CX) tDst ig''
                          (B2Asm.STmp tSrc) ->
                            (if tSrc == tDst
                             then ig''
                             else addConflictWithAssignment (XCommon.Reg Common.CX) tDst ig'')
                          (B2Asm.FnPtr _) -> error "Assign: shift by function pointer")
              ig'''' = (if ((XCommon.Reg Common.CX) `Set.member` aSet)
                        then error "ecx required live at time of shift call"
                        else addConflictWithAssignments aSet tDst ig''')
          in (Set.insert tDst $ addIfTmp src lSet, aSet, ig'''')

        {-
          Given a set of temps lSet, an interference graph ig, and an assignment a, bans that
          assignment from all temps in lSet.
        -}
        addConflictsWithAssignment :: LiveSet -> Interference -> XCommon.Assignment
                                   -> Interference
        addConflictsWithAssignment lSet ig a =
          Set.fold (addConflictWithAssignment a) ig lSet

        {-
          Given an assignment, temp, and interference graph, bans t from being assigned a in
          the interference graph.
        -}
        addConflictWithAssignment :: XCommon.Assignment -> TmpIdent -> Interference
                                  -> Interference
        addConflictWithAssignment a t ig =
          Map.insertWith merge t (Set.empty, Set.singleton a) ig

        {-
          Given a set of assignments and a temp t, forces t to conflict with everything in it.
        -}
        addConflictWithAssignments :: AssignSet -> TmpIdent -> Interference
                                   -> Interference
        addConflictWithAssignments aSet t ig =
          Map.insertWith merge t (Set.empty, aSet) ig

        {-
          Given a src and a set, if the src is a temp it adds it to the set, otherwise it does
          nothing.
        -}
        addIfTmp :: B2Asm.Src -> TmpSet -> TmpSet
        addIfTmp (B2Asm.STmp t) s = Set.insert t s
        addIfTmp _ s = s
        {-
          Given a src and a set, if the src is a temp it removes it from the set, otherwise it
          does nothing.
        -}
        removeIfTmp :: B2Asm.Src -> TmpSet -> TmpSet
        removeIfTmp (B2Asm.STmp t) s = Set.delete t s
        removeIfTmp _ s = s
        {-
          For every vertex t1 in the live set, adds the undirected edge (t, t1) to the graph.
        -}
        addConflictsWithTmp :: TmpIdent -> TmpSet -> Interference -> Interference
        addConflictsWithTmp t lSet ig =
          Set.fold (addEdge t) ig lSet
        {-
          Given t1, t2, and a graph, adds the undirected edge (t1, t2) to the graph.
        -}
        addEdge :: TmpIdent -> TmpIdent -> Interference -> Interference
        addEdge t1 t2 g =
          let g' = Map.insertWith merge t1 (Set.singleton t2, Set.empty) g
          in Map.insertWith merge t2 (Set.singleton t1, Set.empty) g'

        merge (a1, b1) (a2, b2) = (Set.union a1 a2, Set.union b1 b2)

        {-
          Given a set of variables and a memory location, adds any temps read as a part of
          evaluating the memory location into the set.
        -}
        addMemTmps :: B2Asm.Mem -> TmpSet -> TmpSet
        addMemTmps (B2Asm.ArrMem _ addrSrc indSrc _) s = addIfTmp addrSrc $ addIfTmp indSrc s
        addMemTmps (B2Asm.PtrMem addrSrc _) s = addIfTmp addrSrc s


{-
  Merges the given list of interference graphs into a single graph.
-}
mergeInterferenceGraphs :: [Interference] -> Interference
mergeInterferenceGraphs igs =
  Map.unionsWith (\(a1, b1) -> (\(a2, b2) -> (Set.union a1 a2, Set.union b1 b2))) igs

{-
  Generates assignments for every temp in the input interference graph. Does so by greedily
  choosing the first available color for each temp in turn.

  NOTE: r10 and r11 are never allocated to any temps, because they're needed for spilling.
-}
assignFromGraph :: Interference -> XCommon.TmpAssignments
assignFromGraph interference =
  let tmps = seo interference
  in foldl (addAssignment interference) Map.empty tmps
  where addAssignment :: Interference -> XCommon.TmpAssignments -> Integer
                      -> XCommon.TmpAssignments
        addAssignment ig assignments t =
          let (nbrs, as) = ig Map.! t
              nbrAssignments = Set.map fromJust $ Set.filter isJust $ Set.map (`Map.lookup` assignments) nbrs
              badAssignments = Set.union as nbrAssignments
              assignment = fromJust $ find (`Set.notMember` badAssignments) validAssignments
          in Map.insert t assignment assignments

        validAssignments :: [XCommon.Assignment]
        validAssignments =
          map XCommon.Reg (Common.callerSaveRegs ++ Common.calleeSaveRegs)
          ++ map XCommon.Spill [0..]

        -- Returns the tmps of the interference graph, in a a simplicial elimination ordering.
        -- TODO: I think that this is currently O(V^2) in the size of the interference graph.
        -- Apparently, it can be done in O(V + E).
        seo :: Interference -> [TmpIdent]
        seo interference =
          let weights = Map.map (\_ -> 0) interference
          in  seo' weights
          where {-
                  Gets the heaviest remaining tmp in weights, and updates the weight map by:
                    1. Removing the heaviest tmp.
                    2. Incrementing the weights of the heaviest tmp's neighbors.
                  Then, finishes the seo with the updated weights.
                -}
                seo' :: Map.Map TmpIdent Integer -> [TmpIdent]
                seo' weights
                  | Map.null weights = []
                  | otherwise =
                      let h = heaviest weights
                          weights' = Map.delete h weights
                          n = neighbors interference h
                          weights'' = foldl (\w t -> Map.adjust (+ 1) t w) weights' n
                      in  h : (seo' weights'')

                -- Gets a tmp's neighbors in the interference graph.
                neighbors :: Interference -> TmpIdent -> [TmpIdent]
                neighbors interference t =
                  let (n, _) = interference Map.! t
                  in Set.elems n

                -- Finds the heaviest tmp in weights.
                heaviest :: Map.Map TmpIdent Integer -> TmpIdent
                heaviest weights =
                  let tmps = Map.keys weights
                  in  maximumBy (\a b -> compare (weights Map.! a) (weights Map.! b)) tmps

getRTmpAssignments :: B2Asm.BlockMap -> XCommon.TmpAssignments
getRTmpAssignments bMap =
  let maxRTmp = Map.foldl max (-1) $ Map.map getMaxRTmp bMap
  in (if maxRTmp == -1
      then Map.empty
      else
        (let rTmps = [0..maxRTmp]
         in Map.fromList $ map (\r -> (r, assignRTmp r)) rTmps))
  where getMaxRTmp :: B2Asm.Block -> TmpIdent
        getMaxRTmp (B2Asm.BBlock ss _) = foldl processSIns (-1) ss

        processSIns :: TmpIdent -> B2Asm.SIns -> TmpIdent
        processSIns a (B2Asm.ToReserved _ _ (B2Asm.RTmp a')) = max a a'
        processSIns a (B2Asm.FromReserved _ (B2Asm.RTmp a') _) = max a a'
        processSIns a _ = a

assignRTmp :: TmpIdent -> XCommon.Assignment
assignRTmp r =
  if r < 7
  then XCommon.Reg $ genericIndex Common.callerSaveRegs r
  else XCommon.Spill $ r - 7
