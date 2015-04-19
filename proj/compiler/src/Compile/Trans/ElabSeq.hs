{-
  Contains the C implementation of sequences, as a list of CTree GDecls.
-}

module Compile.Trans.ElabSeq where

import qualified Compile.Trans.Conc as Conc
import qualified Compile.Types.Common as Common
import Compile.Types.CTree

import qualified Data.Set as Set

{-
  Contains:
    list of element types used in sequences.
    list of (fnName, retType) used in tabulate calls.
    list of (fnName, inType, outType) used in map calls.
    list of (fnName, type) used in reduce calls.
    list of (fnName, inType) used in filter calls.
    list of (fnName, inType1, inType2, outType) used in combine calls.
-}
type SeqInfo = (Set.Set (Conc.Conc),
                Set.Set (Common.Ident, Conc.Conc),
                Set.Set (Common.Ident, Conc.Conc, Conc.Conc),
                Set.Set (Common.Ident, Conc.Conc),
                Set.Set (Common.Ident, Conc.Conc),
                Set.Set (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc))
emptySeqInfo :: SeqInfo
emptySeqInfo = (Set.empty, Set.empty, Set.empty, Set.empty, Set.empty,
                Set.empty)
addSeqType :: SeqInfo -> Conc.Conc -> SeqInfo
addSeqType (s1, s2, s3, s4, s5, s6) c =
  (Set.insert c s1, s2, s3, s4, s5, s6)
addTabulateCall :: SeqInfo -> Common.Ident -> Conc.Conc -> SeqInfo
addTabulateCall (s1, s2, s3, s4, s5, s6) f c =
  (s1, Set.insert (f, c) s2, s3, s4, s5, s6)
addMapCall :: SeqInfo -> Common.Ident -> Conc.Conc -> Conc.Conc -> SeqInfo
addMapCall (s1, s2, s3, s4, s5, s6) f c1 c2 =
  (s1, s2, Set.insert (f, c1, c2) s3, s4, s5, s6)
addReduceCall :: SeqInfo -> Common.Ident -> Conc.Conc -> SeqInfo
addReduceCall (s1, s2, s3, s4, s5, s6) f c =
  (s1, s2, s3, Set.insert (f, c) s4, s5, s6)
addFilterCall :: SeqInfo -> Common.Ident -> Conc.Conc -> SeqInfo
addFilterCall (s1, s2, s3, s4, s5, s6) f c =
  (s1, s2, s3, s4, Set.insert (f, c) s5, s6)
addCombineCall :: SeqInfo ->
                  Common.Ident -> Conc.Conc -> Conc.Conc -> Conc.Conc -> SeqInfo
addCombineCall (s1, s2, s3, s4, s5, s6) f c1 c2 c3 =
  (s1, s2, s3, s4, s5, Set.insert (f, c1, c2, c3) s6)


{-
  Goes before user code, to make sure gcc knows about the functions/structs.
-}
getSeqDecls :: SeqInfo -> [GDecl]
getSeqDecls (seqTypes, tabulates, maps, reduces, filters, combines) =
  concat [getStructDefns seqTypes,
          getStructTypedefs seqTypes,
          getTabulateDecls tabulates,
          getMapDecls maps,
          getReduceDecls reduces,
          getFilterDecls filters,
          getCombineDecls combines]
          

{-
  Goes after user code, to make sure all user-defined functions are available
  for use.
-}
getSeqDefns :: SeqInfo -> [GDecl]
getSeqDefns (seqTypes, tabulates, maps, reduces, filters, combines) =
  concat [getTabulateDefns tabulates,
          getMapDefns maps,
          getReduceDefns reduces,
          getFilterDefns filters,
          getCombineDefns combines]




{-
  struct __libseq_seq {
    T *arr;
    int len;
  };
-}
getStructDefns :: Set.Set Conc.Conc -> [GDecl]
getStructDefns s = map getStructDefn $ Set.elems s
  where getStructDefn :: Conc.Conc -> GDecl
        getStructDefn c =
          SDefn (seqTypeName c) [CParam (CPtrT $ cTypeFor c) arrFieldName,
                                 CParam CIntT lenFieldName]

{-
  typedef struct <name> <name>;
-}
getStructTypedefs :: Set.Set Conc.Conc -> [GDecl]
getStructTypedefs s = map getStructTypedef $ Set.elems s
  where getStructTypedef :: Conc.Conc -> GDecl
        getStructTypedef c =
          let sName = seqTypeName c
          in Typedef (CStructT sName) sName

getTabulateDecls :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getTabulateDecls s = map getTabulateDecl $ Set.elems s
  where getTabulateDecl :: (Common.Ident, Conc.Conc) -> GDecl
        getTabulateDecl (fName, outC) =
          FDecl (seqType outC) (libFnName "tabulate" fName) [CParam CIntT nName]

getMapDecls :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc) -> [GDecl]
getMapDecls s = map getMapDecl $ Set.elems s
  where getMapDecl :: (Common.Ident, Conc.Conc, Conc.Conc) -> GDecl
        getMapDecl (fName, inC, outC) =
          FDecl (seqType outC)
                (libFnName "map" fName)
                [CParam (seqType inC) s1Name]

getReduceDecls :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getReduceDecls s = map getReduceDecl $ Set.elems s
  where getReduceDecl :: (Common.Ident, Conc.Conc) -> GDecl
        getReduceDecl (fName, inC) = 
          FDecl (getCType inC)
                (libFnName "reduce" fName)
                [CParam (getCType inC) bName, CParam (seqType inC) s1Name]

getFilterDecls :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getFilterDecls s = map getFilterDecl $ Set.elems s
  where getFilterDecl :: (Common.Ident, Conc.Conc) -> GDecl
        getFilterDecl (fName, inC) =
          FDecl (seqType inC)
                (libFnName "filter" fName)
                [CParam (seqType inC) s1Name]

getCombineDecls :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc)
                -> [GDecl]
getCombineDecls s = map getCombineDecl $ Set.elems s
  where getCombineDecl :: (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc)
                       -> GDecl
        getCombineDecl (fName, inC1, inC2, outC) =
          FDecl (seqType outC)
                (libFnName "combine" fName)
                [CParam (seqType inC1) s1Name, CParam (seqType inC2) s2Name]





getTabulateDefns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getTabulateDefns s = map getTabulateDefn $ Set.elems s
  where getTabulateDefn :: (Common.Ident, Conc.Conc) -> GDecl
        getTabulateDefn (fName, outC) =
          FDefn (seqType outC)
                (libFnName "tabulate" fName)
                [CParam CIntT "n"]
                (buildSeq outC $
                 buildLoop $
                 getLoopStmts fName)

        getLoopStmts :: Common.Ident -> [Stmt]
        getLoopStmts fName =
          let iE = Ident iName
              idxLV = LIndex (LDot (LStar $ LIdent sName) arrFieldName) iE
          in  [Assn Common.Set idxLV $ Call (Ident fName) [iE]]


getMapDefns :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc) -> [GDecl]
getMapDefns s = map getMapDefn $ Set.elems s
  where getMapDefn :: (Common.Ident, Conc.Conc, Conc.Conc) -> GDecl
        getMapDefn (fName, inC, outC) =
          FDefn (seqType outC)
                (libFnName "map" fName)
                [CParam (seqType inC) s1Name]
                (getSizeFromSeq $
                 buildSeq outC $
                 buildLoop $
                 getLoopStmts fName)

        getLoopStmts :: Common.Ident -> [Stmt]
        getLoopStmts fName =
          let iE = Ident iName
              idxLV = LIndex (LDot (LStar $ LIdent sName) arrFieldName) iE
              idxE = Index (Dot (Star $ Ident s1Name) arrFieldName) iE
          in  [Assn Common.Set idxLV $ Call (Ident fName) [idxE]]


getReduceDefns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getReduceDefns s = map getReduceDefn $ Set.elems s
  where getReduceDefn :: (Common.Ident, Conc.Conc) -> GDecl
        getReduceDefn (fName, inC) = 
          FDefn (getCType inC)
                (libFnName "reduce" fName)
                [CParam (getCType inC) bName, CParam (seqType inC) s1Name]
                ((getSizeFromSeq $
                  buildLoop $
                  getLoopStmts fName) ++
                 [Return $ Just $ Ident bName])

        getLoopStmts :: Common.Ident -> [Stmt]
        getLoopStmts fName =
          let val = Index (Dot (Star $ Ident s1Name) arrFieldName) $ Ident iName
          in [Assn Common.Set (LIdent bName) $
              Call (Ident fName) [Ident bName, val]]


getFilterDefns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getFilterDefns s = map getFilterDefn $ Set.elems s
  where getFilterDefn :: (Common.Ident, Conc.Conc) -> GDecl
        getFilterDefn (fName, inC) =
          FDefn (seqType inC)
                (libFnName "filter" fName)
                [CParam (seqType inC) s1Name]
                (getSizeFromSeq $
                 buildSeq inC $
                 adjustSize $
                 buildLoop $
                 getLoopStmts fName)

        getLoopStmts :: Common.Ident -> [Stmt]
        getLoopStmts fName =
          let s1IdxE = Index (Dot (Star $ Ident s1Name) arrFieldName)
                             (Ident iName)
              sIdxLV = LIndex (LDot (LStar $ LIdent sName) arrFieldName)
                              (Ident bName)
          in [If (Call (Ident fName) [s1IdxE])
               [Assn Common.Set sIdxLV s1IdxE,
                Assn Common.Set (LIdent bName)
                                (Binop (ArithOp Common.AAdd)
                                       (Ident bName)
                                       (IntLit $ Common.Dec 1))]
               []
             ]


getCombineDefns :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc)
              -> [GDecl]
getCombineDefns s = map getCombineDefn $ Set.elems s
  where getCombineDefn :: (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc)
                       -> GDecl
        getCombineDefn (fName, inC1, inC2, outC) =
          FDefn (seqType outC)
                (libFnName "combine" fName)
                [CParam (seqType inC1) s1Name, CParam (seqType inC2) s2Name]
                (getSizeFromSeqs $
                 buildSeq outC $
                 buildLoop $
                 getLoopStmts fName)

        getLoopStmts :: Common.Ident -> [Stmt]
        getLoopStmts fName =
          let iE = Ident iName
              lv = LIndex (LDot (LStar $ LIdent sName) arrFieldName) iE
              s1E = Index (Dot (Star $ Ident s1Name) arrFieldName) iE
              s2E = Index (Dot (Star $ Ident s2Name) arrFieldName) iE
          in  [Assn Common.Set lv $ Call (Ident fName) [s1E, s2E]]




{-
  Wraps the given statements in a declaration of n, computed as seq1->len.
-}
getSizeFromSeq :: [Stmt] -> [Stmt]
getSizeFromSeq stmts =
  [Decl CIntT nName (Just $ Dot (Star $ Ident s1Name) lenFieldName) stmts]

{-
  Wraps the given statements in an assert that seq1->len == seq2->len, and
  declares a variable n to have this value.
-}
getSizeFromSeqs :: [Stmt] -> [Stmt]
getSizeFromSeqs stmts =
  [Exp $ Call (Ident "assert") [Binop (CmpOp $ Common.CmpOp Common.E)
                                      (Dot (Star $ Ident s1Name) lenFieldName)
                                      (Dot (Star $ Ident s2Name) lenFieldName)],
   Decl CIntT nName (Just $ Dot (Star $ Ident s1Name) lenFieldName) stmts]

{-
  Wraps the given code in a declaration and return of a sequence.
-}
buildSeq :: Conc.Conc -> [Stmt] -> [Stmt]
buildSeq elemC stmts =
  let sT@(CPtrT bigT) = seqType elemC
      eT = getCType elemC
      callocSeqExp = Call (Ident "calloc") [Sizeof bigT, IntLit $ Common.Dec 1]
      callocArrExp = Call (Ident "calloc") [Sizeof eT, Ident nName]
      arrLV = LDot (LStar $ LIdent sName) arrFieldName
      lenLV = LDot (LStar $ LIdent sName) lenFieldName
  in  [Decl sT sName (Just $ callocSeqExp)
       ([Assn Common.Set arrLV callocArrExp,
         Assn Common.Set lenLV (Ident nName)] ++
         stmts ++
         [Return $ Just $ Ident sName])]

{-
  Wraps the given code in a declaration of a variable, which is initialized to 0
  at the start and stored as the sequence's length at the end.
-}
adjustSize :: [Stmt] -> [Stmt]
adjustSize stmts =
  [Decl CIntT bName (Just $ IntLit $ Common.Dec 0) $
   stmts ++ [Assn Common.Set (LDot (LStar $ LIdent sName) lenFieldName)
                             (Ident bName)]]

{-
  Wraps the given statements in a while loop from 0 to n-1.
-}
buildLoop :: [Stmt] -> [Stmt]
buildLoop stmts =
  [Decl CIntT iName (Just $ IntLit $ Common.Dec 0) [
   While (Binop (CmpOp $ Common.CmpOp Common.L) (Ident iName) (Ident nName)) $
     stmts ++ 
     [Assn Common.Set (LIdent iName)
                      (Binop (ArithOp Common.AAdd) (Ident iName)
                                                   (IntLit $ Common.Dec 1))]
   ]
  ]


{-
  Helpers.
-}
nName :: Common.Ident
nName = "n"

iName :: Common.Ident
iName = "i"

bName :: Common.Ident
bName = "b"

sName :: Common.Ident
sName = "s"

s1Name :: Common.Ident
s1Name = "s1"

s2Name :: Common.Ident
s2Name = "s2"

arrFieldName :: Common.Ident
arrFieldName = "arr"

lenFieldName :: Common.Ident
lenFieldName = "len"

seqType :: Conc.Conc -> CType
seqType c = CPtrT $ CDefT $ seqTypeName c

getCType :: Conc.Conc -> CType
getCType (Conc.BoolC) = CBoolT
getCType (Conc.IntC) = CIntT
getCType _ = error "unexpected sequence element type"

libFnName :: Common.Ident -> Common.Ident -> Common.Ident
libFnName operatorName fnName = "__libseq_" ++ operatorName ++ "_" ++ fnName

seqTypeName :: Conc.Conc -> Common.Ident
seqTypeName c = "__libseq_seq_" ++ (suffixFor c)

suffixFor :: Conc.Conc -> String
suffixFor Conc.IntC = "i"
suffixFor Conc.BoolC = "b"

cTypeFor :: Conc.Conc -> CType
cTypeFor Conc.IntC = CIntT
cTypeFor Conc.BoolC = CBoolT
