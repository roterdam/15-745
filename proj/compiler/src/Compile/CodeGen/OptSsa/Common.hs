{-
  Holds types/functions used by multiple optimizations.
-}

module Compile.CodeGen.OptSsa.Common where

import Compile.Types.S3Asm
import qualified Compile.Graph.Graph as Graph
import Compile.Types.Common (TmpIdent, BlockIdent)

import qualified Data.Map as Map
import qualified Data.Set as Set

{-
  Maps temps to the values they take on. Used for minimization/propogation/cfg shrinking
-}
type ValueMap = Map.Map TmpIdent Src

{-
  Represents a graph on blocks, with arbitrary information at the nodes.
-}
type BlockGraph = Graph.Graph BlockIdent


{-
  Helper function used for creating block graphs from S3Asm BlockMaps.
-}
bgFromBMap :: (Block -> s) -> BlockMap -> BlockGraph s
bgFromBMap f bMap = Graph.fromMapWith f succsOf bMap
  where succsOf :: Block -> [BlockIdent]
        succsOf (BBlock _ _ (Ret _ _)) = []
        succsOf (BBlock _ _ (Jmp (Label l _))) = [l]
        succsOf (BBlock _ _ (CmpJmp _ _ _ _ (Label a _) (Label b _))) =
          [a, b]
        succsOf (BBlock _ _ (Raise _)) = []

{-
  Given a block map, generates a map from block idents to their predecessors. 
-}       
getPredecessorMap :: BlockMap -> Map.Map BlockIdent [BlockIdent]
getPredecessorMap bMap =
  Map.map (\(ps, _, _) -> Set.toList ps) $ Graph.toMapWithEdges $ bgFromBMap id bMap
