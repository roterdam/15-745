{-
  Contains the C implementation of sequences, as a list of CTree GDecls.
-}

module Compile.Trans.LibSeq where

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



getSeqIncludes :: SeqInfo -> [GDecl]
getSeqIncludes (seqTypes, tabulates, maps, reduces, filters, combines) =
  concat [getStructDefinitions seqTypes,
          getStructTypedefs seqTypes,
          getTabulateFns tabulates,
          getMapFns maps,
          getReduceFns reduces,
          getFilterFns filters,
          getCombineFns combines]


{-
  struct __libseq_seq {
    T *arr;
    int len;
  };
-}
getStructDefinitions :: Set.Set Conc.Conc -> [GDecl]
getStructDefinitions s = map getStructDefn $ Set.elems s
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


{-
  How do I achieve code reuse between the following library functions?

  seq<T> *tabulate_<f>(int n) {
    seq<T> *newSeq = calloc(sizeof(seq<T>), 1);
    newSeq->arr = calloc(sizeof(T), n);
    newSeq->len = n;
    int i = 0;
    while (i < n) {
      newSeq->arr[i] = f(i);
      i = i + 1;
    }
    return newSeq;
  }

  seq<T> *map_<f>(seq<T2> *inSeq) {
    int n = inSeq->len;
    seq<T> *newSeq = calloc(sizeof(seq<T>), 1);
    newSeq->arr = calloc(sizeof(T), n);
    int i = 0;
    while (i < n) {
      newSeq->arr[i] = f(inSeq->arr[i]);
      i = i + 1;
    }
    return newSeq;
  }

  T reduce_<f>(T baseVal, seq<T> *inSeq) {
    int n = inSeq->len;
    T result = baseVal;
    int i = 0;
    while (i < n) {
      result = f(result, inSeq->arr[i]);
      i = i + 1;
    }
    return newSeq;
  }

  seq<T> *filter_<f>(seq<T> *inSeq) {
    int n = inSeq->len;
    seq<T> *newSeq = calloc(sizeof(seq<T>), 1);
    newSeq->arr = calloc(sizeof(T), n);
    int i = 0;
    int outI = 0;
    while (i < n) {
      if (f(inSeq->arr[i])) {
        newSeq->arr[outI] = inSeq->arr[i];
        outI = outI + 1;
      }
      i = i + 1;
    }
    return newSeq;
  }

  seq<T> *combine_<f>(seq<T> *s1, seq<T> *s2) {
    assert(s1->len == s2->len);
    int n = s1->len;
    seq<T> *newSeq = calloc(szeof(seq<T>), 1);
    newSeq->arr = calloc(sizeof(T), n);
    newSeq->len = n;
    int i = 0;
    while (i < n) {
      newSeq->arr[i] = f(s1->arr[i], s2->arr[i]);
      i = i + 1;
    }
    return newSeq;
  }
-}
getTabulateFns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getTabulateFns s = map getTabulateFn $ Set.elems s
  where getTabulateFn :: (Common.Ident, Conc.Conc) -> GDecl
        getTabulateFn (f, outC) =
          FDefn (seqType outC)
                (libFnName "tabulate" $ nameSuffix outC)
                [CParam CIntT "n"]
                (seqBuilderTemplate outC (\i -> Call (Ident f) [Ident i]))


seqBuilderTemplate :: Conc.Conc -> (Common.Ident -> Exp) -> [Stmt]
seqBuilderTemplate elemC bodyExpBuilder =
  let sT = seqType elemC
      sN = "s"
      sLV = LIdent sN
      sE = Ident sN
      idxName = "i"
      idxE = Ident idxName
      idxLV = LIdent idxName
      nName = "n"
      nE = Ident nName
      arrLV = LDot (LStar sLV) arrFieldName
      lenLV = LDot (LStar sLV) lenFieldName
      ltCmp = CmpOp $ Common.CmpOp Common.L
      oneE = IntLit $ Common.Dec 1
  in  [Decl sT sN (Just $ callocSeqExp sT) [
       Assn Common.Set arrLV (callocArrExp sT nE),
       Assn Common.Set lenLV nE,
       Decl CIntT idxName (Just $ IntLit $ Common.Dec 0) [
       While (Binop ltCmp idxE nE) [
         Assn Common.Set (LIndex arrLV idxE) $ bodyExpBuilder idxName,
         Assn Common.Set idxLV (Binop (ArithOp Common.AAdd) idxE oneE)
       ]],
       Return $ Just sE
       ]
      ]

getMapFns :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc) -> [GDecl]
getMapFns s = map getMapFn $ Set.elems s
  where getMapFn :: (Common.Ident, Conc.Conc, Conc.Conc) -> GDecl
        getMapFn = error "unimplemented"

getReduceFns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getReduceFns s = map getReduceFn $ Set.elems s
  where getReduceFn :: (Common.Ident, Conc.Conc) -> GDecl
        getReduceFn = error "unimplemented"

getFilterFns :: Set.Set (Common.Ident, Conc.Conc) -> [GDecl]
getFilterFns s = map getFilterFn $ Set.elems s
  where getFilterFn :: (Common.Ident, Conc.Conc) -> GDecl
        getFilterFn = error "unimplemented"

getCombineFns :: Set.Set (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc) -> [GDecl]
getCombineFns s = map getCombineFn $ Set.elems s
  where getCombineFn :: (Common.Ident, Conc.Conc, Conc.Conc, Conc.Conc) -> GDecl
        getCombineFn = error "unimplemented"



{-
  Helpers.
-}
arrFieldName :: Common.Ident
arrFieldName = "arr"

lenFieldName :: Common.Ident
lenFieldName = "len"

seqType :: Conc.Conc -> CType
seqType = error "unimplemented"

nameSuffix :: Conc.Conc -> Common.Ident
nameSuffix = error "unimplemented"

libFnName :: Common.Ident -> Common.Ident -> Common.Ident
libFnName = error "unimplemented"

callocSeqExp :: CType -> Exp
callocSeqExp = error "unimplemented"

callocArrExp :: CType -> Exp -> Exp
callocArrExp = error "unimplemented"

seqTypeName :: Conc.Conc -> Common.Ident
seqTypeName c = "__libseq_seq_" ++ (suffixFor c)

suffixFor :: Conc.Conc -> String
suffixFor Conc.IntC = "i"
suffixFor Conc.BoolC = "b"

cTypeFor :: Conc.Conc -> CType
cTypeFor Conc.IntC = CIntT
cTypeFor Conc.BoolC = CBoolT
