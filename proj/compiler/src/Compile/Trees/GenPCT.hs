{-
  Converts an AST into a PCT. Broadly, the goal is to drop all type information from the tree,
  and simplify it as much as possible before the conversion to IRT. The main piece of this is
  turning the string-based Star/Index/Dot memory model into an offset-based Star/Index one;
  this is more annoying than it seems because it mandates passing a lot of type information
  around, so it's worth doing in a separate pass for clarity's sake.
  TODO: in the future, it'd be cool to get rid of Decls entirely. Is this feasible?
-}

module Compile.Trees.GenPCT where

import Compile.Types.Common (Size(..), Tag)
import qualified Compile.Types.Common as Common
import qualified Compile.Types.AST as AST
import qualified Compile.Types.PCT as PCT
import qualified Compile.Trees.Conc as Conc
import qualified Job
import Compile.Trees.CheckState (GlobalState(..))

import qualified Data.Set as Set
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.List as List
import Data.Int (Int32)
import Data.Char (ord)
import Data.Bits (xor, shiftL, shiftR, (.|.), (.&.))

{-
  Maps old function names to their names in PCT.
-}
type NameMap = Map.Map String String

{-
  Maps variables to their types.
-}
type Context = Map.Map String Conc.Conc

{-
  Maps struct names to lists of their fields (name and type).
-}
type FieldListMap = Map.Map String [Conc.CParam]

{-
  Maps struct names to their sizes, alignments, and the offset from the start of each of their
  fields.
-}
type FieldInfoMap = Map.Map String (Conc.Conc, PCT.Offset)
type StructInfo = (PCT.ElemSize, Size, FieldInfoMap)
type StructInfoMap = Map.Map String StructInfo

type TagMap = Map.Map Conc.Conc Tag
type TaggedSet = Set.Set Conc.Conc

{-
  The state used to translate AST function to PCT functions.
-}
type TransState = (NameMap, Context, GlobalState, StructInfoMap, TagMap)

astToPCT :: Job.Job -> GlobalState -> AST.AST -> PCT.PCT
astToPCT _ gs (AST.Prog gDecls) =
  let nMap = Map.fromList $ Maybe.mapMaybe getNamePair gDecls
      siMap = buildSIMap (structs gs)
      tMap = buildTagMap gs siMap gDecls
  in PCT.Prog $ Maybe.mapMaybe (transGDecl nMap gs siMap tMap) gDecls
  where {-
          Gets an (oldName, newName) pair for each function declaraion or external function.
        -}
        getNamePair :: AST.GDecl -> Maybe (String, String)
        getNamePair (AST.FDefn _ ident _ _ ) =
          Just (ident, Common.wrapFnName $ mangleFnName ident)
        getNamePair (AST.FExt _ ident _) = Just (ident, Common.wrapFnName ident)
        getNamePair _ = Nothing

        {-
          Builds a map from struct names to their sizes, alignments, and the offsets of their
          fields.
        -}
        buildSIMap :: FieldListMap -> StructInfoMap
        buildSIMap flMap = foldl (resolveStruct flMap) Map.empty $ Map.keys flMap
          where {-
                  Adds the struct info (size, alignment, field offsets) for a single struct to
                  the given struct map.
                -}
                resolveStruct :: FieldListMap -> StructInfoMap -> String -> StructInfoMap
                resolveStruct flMap siMap sName
                  | sName `Map.member` siMap = siMap
                  | otherwise =
                    let fields = flMap Map.! sName
                        siMap' = foldl (resolveField flMap) siMap fields
                        (sz, alignment, fiMap) = foldl (addField siMap') (0, Long, Map.empty) fields
                    in Map.insert sName (upToMultipleOf sz alignment, alignment, fiMap) siMap'

                {-
                  If given a struct field, recursively resolves it and updates the sMap. If
                  given any other kind of field, does nothing b/c the sMap doens't need to be
                  updated.
                -}
                resolveField :: FieldListMap -> StructInfoMap -> Conc.CParam -> StructInfoMap
                resolveField flMap siMap (Conc.CParam (Conc.StructC sName) fName) =
                  resolveStruct flMap siMap sName
                resolveField _ siMap _ = siMap

                {-
                  Given a field and information about a struct, computes new information for if
                  the field were added to the end of the struct. Used to inductively build up
                  struct information.
                -}
                addField :: StructInfoMap -> StructInfo -> Conc.CParam -> StructInfo
                addField siMap (sz, alignment, fiMap) f@(Conc.CParam fTp fName) =
                  let (fsz, fAlignment) = getFieldInfo siMap fTp
                      offset = upToMultipleOf sz fAlignment
                      fiMap' = Map.insert fName (fTp, offset) fiMap
                  in (offset + fsz, max alignment fAlignment, fiMap')

                getFieldInfo :: StructInfoMap -> Conc.Conc -> (PCT.ElemSize, Size)
                getFieldInfo _ (Conc.ArrC _) = (8, Quad)
                getFieldInfo _ (Conc.PtrC (Conc.AnyFn _)) = error "GenPCT.getFieldInfo: unexpected AnyFn in struct field"
                getFieldInfo _ (Conc.PtrC (Conc.One (Conc.FnC _))) = (8, Quad)
                getFieldInfo _ (Conc.PtrC _) = (8, Quad)
                getFieldInfo siMap (Conc.StructC sName) =
                  let (sz, alignment, _) = siMap Map.! sName
                  in (sz, alignment)
                getFieldInfo _ (Conc.FnC _) = error "GenPCT.getFieldInfo: unexpected function in struct field"
                getFieldInfo _ Conc.VoidC = (4, Long)
                getFieldInfo _ Conc.BoolC = (1, Byte)
                getFieldInfo _ Conc.IntC = (4, Long)
                getFieldInfo _ Conc.CharC = (1, Byte)
                getFieldInfo _ Conc.StringC = (8, Quad)

                upToMultipleOf :: PCT.ElemSize -> Size -> PCT.ElemSize
                upToMultipleOf sz Byte = sz
                upToMultipleOf sz Long = ((sz + 3) `div` 4) * 4
                upToMultipleOf sz Quad = ((sz + 7) `div` 8) * 8

        {-
          Traverses the entire program to build a set of the concrete types that are ever cast
          to void*, and maps each one to a unique integer in (0..n-1).
          Very similar code to transFn/Stmt/Exp below, because both have to propogate the types
          around.
          TODO: could this duplication be avoided elegantly somehow?
        -}
        buildTagMap :: GlobalState -> StructInfoMap -> [AST.GDecl] -> TagMap
        buildTagMap gs siMap gDecls =
          let tSet = Set.unions $ map (getTaggedSet gs siMap) gDecls
          in Map.fromList $ zip (Set.toList tSet) [0..]
          where getTaggedSet :: GlobalState -> StructInfoMap -> AST.GDecl -> TaggedSet
                getTaggedSet gs siMap (AST.FDefn _ _ params stmts) =
                  let state = (Map.empty, Map.empty, gs, siMap, Map.empty)
                      state' = fst $ List.mapAccumL processParam state params
                  in getTaggedInStmts state' stmts
                getTaggedSet _ _ _ = Set.empty

                getTaggedInStmts :: TransState -> [AST.Stmt] -> TaggedSet
                getTaggedInStmts state =
                  Set.unions . map (getTaggedInStmt state)

                getTaggedInStmt :: TransState -> AST.Stmt -> TaggedSet
                getTaggedInStmt state (AST.Assn _ lv e) =
                  Set.union (getTaggedInLValue state lv) (getTaggedInExp state e)
                getTaggedInStmt state (AST.If e tstmts fstmts) =
                  Set.unions [getTaggedInExp state e, getTaggedInStmts state tstmts,
                              getTaggedInStmts state fstmts]
                getTaggedInStmt state (AST.While e stmts) =
                  Set.union (getTaggedInExp state e) (getTaggedInStmts state stmts)
                getTaggedInStmt state (AST.Return eOpt) =
                  getTaggedInEOpt state eOpt
                getTaggedInStmt state@(nMap, ctx, gs, siMap, tMap) (AST.Decl tp v eOpt stmts) =
                  let ctx' = Map.insert v (resolveType state tp) ctx
                      state' = (nMap, ctx', gs, siMap, tMap)
                  in Set.union (getTaggedInEOpt state' eOpt) (getTaggedInStmts state' stmts)
                getTaggedInStmt state (AST.Assert e) =
                  getTaggedInExp state e
                getTaggedInStmt state (AST.Exp e) =
                  getTaggedInExp state e

                getTaggedInEOpt :: TransState -> Maybe AST.Exp -> TaggedSet
                getTaggedInEOpt state eOpt =
                  case eOpt of
                    Nothing -> Set.empty
                    Just e -> getTaggedInExp state e

                getTaggedInLValue :: TransState -> AST.LValue -> TaggedSet
                getTaggedInLValue _ (AST.LIdent _) = Set.empty
                getTaggedInLValue state (AST.LStar lv) = getTaggedInLValue state lv
                getTaggedInLValue state (AST.LDot lv _) = getTaggedInLValue state lv
                getTaggedInLValue state (AST.LIndex lv e) =
                  Set.union (getTaggedInLValue state lv) (getTaggedInExp state e)

                getTaggedInExp :: TransState -> AST.Exp -> TaggedSet
                getTaggedInExp state e = snd $ gtRec state e

                gtRec :: TransState -> AST.Exp -> (Conc.Conc, TaggedSet)
                gtRec _ (AST.IntLit _) = (Conc.IntC, Set.empty)
                gtRec _ (AST.BoolLit _) = (Conc.BoolC, Set.empty)
                gtRec _ (AST.CharLit _) = (Conc.CharC, Set.empty)
                gtRec _ (AST.StringLit _) = (Conc.StringC, Set.empty)
                gtRec (_, ctx, _, _, _) (AST.Ident v) = (ctx Map.! v, Set.empty)
                gtRec state (AST.Binop op l r) =
                  let (lTp, lSet) = gtRec state l
                      (rTp, rSet) = gtRec state r
                      tp = (case op of
                              (AST.CmpOp _) -> Conc.BoolC
                              _ -> lTp)
                  in (tp, Set.union lSet rSet)
                gtRec state (AST.Unop _ e) =
                  gtRec state e
                gtRec state (AST.Cond e1 e2 e3) =
                  let (_, tSet1) = gtRec state e1
                      (tp2, tSet2) = gtRec state e2
                      (tp3, tSet3) = gtRec state e3
                      tp = (case (tp2, tp3) of
                              (Conc.PtrC Conc.Any, _) -> tp3
                              (_, _) -> tp2)
                  in (tp, Set.unions [tSet1, tSet2, tSet3])
                gtRec state (AST.Call (AST.Ident fName) args) =
                  let tSets = map (getTaggedInExp state) args
                      (_, (resTp, _)) = (fnSigs gs) Map.! fName
                  in (resTp, Set.unions tSets)
                gtRec state@(_, _, gs, _, _) (AST.Call (AST.Star e) args) =
                  let tSets = map (getTaggedInExp state) args
                      (Conc.PtrC ptd, tSet) = gtRec state e
                      resTp = case ptd of
                                (Conc.AnyFn (rTp, _)) -> rTp
                                (Conc.One (Conc.FnC fTpName)) ->
                                  fst $ (sigdefs gs) Map.! fTpName
                  in (resTp, Set.union tSet (Set.unions tSets))
                gtRec _ (AST.Call _ _) = error "GenPCT.transExp.gtRec: invalid Call"
                gtRec state (AST.Alloc tp) =
                  (Conc.PtrC $ Conc.One $ resolveType state tp, Set.empty)
                gtRec state (AST.AllocArray tp numE) =
                  (Conc.ArrC $ resolveType state tp, snd $ gtRec state numE)
                gtRec state (AST.Index arrE idxE) =
                  let (Conc.ArrC arrTp, arrTSet) = gtRec state arrE
                      (_, idxTSet) = gtRec state idxE
                  in (arrTp, Set.union arrTSet idxTSet)
                gtRec state (AST.Star addrE) =
                  let (Conc.PtrC (Conc.One tp), tSet) = gtRec state addrE
                  in (tp, tSet)
                gtRec state@(_, _, _, siMap, _) (AST.Dot structE field) =
                  let (Conc.StructC structIdent, structTSet) = gtRec state structE
                      (_, _, fiMap) = siMap Map.! structIdent
                      (fieldTp, _) = fiMap Map.! field
                  in (fieldTp, structTSet)
                gtRec (_, _, gs, _, _) (AST.Amp fName) =
                  (Conc.PtrC $ Conc.AnyFn $ snd $ (fnSigs gs) Map.! fName, Set.empty)
                gtRec state (AST.Cast tp e) =
                  let tp' = resolveType state tp
                      (eTp, tSet) = gtRec state e
                  in (case tp' of
                        (Conc.PtrC (Conc.One Conc.VoidC)) -> (tp', Set.insert eTp tSet)
                        _ -> (tp', Set.insert tp' tSet))
                gtRec state AST.Null = (Conc.PtrC Conc.Any, Set.empty)



        {-
          Translates a function definition from AST to PCT, using the given name map and state.
          Returns Just (translated_function), or Nothing if it was some other kind of GDecl.
        -}
        transGDecl :: NameMap -> GlobalState -> StructInfoMap -> TagMap -> AST.GDecl
                   -> Maybe PCT.Fn
        transGDecl nMap gs siMap tMap (AST.FDefn t ident params stmts) =
          let state = (nMap, Map.empty, gs, siMap, tMap)
              (state', params') = List.mapAccumL processParam state params
              stmts' = transStmts state' (stmts ++ [rtn])
          in  Just (PCT.Fn (nMap Map.! ident) params' stmts')
        transGDecl _ _ _ _ _ = Nothing

        {-
          Adds a parameter declaration to the context (just like the Decl case below).
        -}
        processParam :: TransState -> Common.Param -> (TransState, (PCT.Ident, Size))
        processParam state@(nMap, context, gs, siMap, tMap) (Common.Param tp p) =
          let tp' = resolveType state tp
          in  ((nMap, Map.insert p tp' context, gs, siMap, tMap), (p, concSize tp'))

        -- Renames an internally defined function name.
        mangleFnName :: String -> String
        mangleFnName = ("_c0_" ++)

        rtn :: AST.Stmt
        rtn = AST.Return $ Just $ AST.IntLit $ Common.Dec 0

{-
  Translates the given list of statements into PCT form, stopping when it hits a return.
  Note that bare returns are elaborated to "return 0".
-}
transStmts :: TransState -> [AST.Stmt] -> [PCT.Stmt]
transStmts state = map (transStmt state)

{-
  Translates a single statement from AST to PCT form, given a translation state.
-}
transStmt :: TransState -> AST.Stmt -> PCT.Stmt
transStmt state (AST.Assn op lv e) = PCT.Assn op (transLValue state lv) (transExp state e)
transStmt state (AST.If e ss1 ss2) =
  PCT.If (transExp state e) (transStmts state ss1) (transStmts state ss2)
transStmt state (AST.While e ss) = PCT.While (transExp state e) (transStmts state ss)
transStmt _ (AST.Return Nothing) = PCT.Return (PCT.NumLit 0)
transStmt state (AST.Return (Just e)) = PCT.Return (transExp state e)
transStmt state@(nMap, context, gs, siMap, tMap) (AST.Decl tp v eOpt ss) =
  let ss' = case eOpt of
        Nothing -> ss
        Just e -> (AST.Assn Common.Set (AST.LIdent v) e) : ss
      tp' = resolveType state tp
      context' = Map.insert v tp' context
      state' = (nMap, context', gs, siMap, tMap)
  in PCT.Decl v (concSize tp') (transStmts state' ss')
transStmt state (AST.Assert e) = PCT.Assert (transExp state e)
transStmt state (AST.Exp e) = PCT.Exp (transExp state e)

{-
  Translates an AST lValue into a PCT lValue by flattening the Dots into offsets.
-}
transLValue :: TransState -> AST.LValue -> PCT.LValue
transLValue state lv =
  snd $ tLVRec state lv
  where {-
          Translates an lvalue from AST to PCT form, also returning the type to help with
          translating possible enclosing lvalues.
        -}
        tLVRec :: TransState -> AST.LValue -> (Conc.Conc, PCT.LValue)
        tLVRec (_, ctx, _, _, _) (AST.LIdent ident) = (ctx Map.! ident, PCT.LIdent ident)
        tLVRec state (AST.LStar lv) =
          let (Conc.PtrC (Conc.One tp), lv') = tLVRec state lv
          in (tp, PCT.LStar lv' Nothing)
        tLVRec state (AST.LIndex lv idxE) =
          let (Conc.ArrC arrTp, lv') = tLVRec state lv
              esz = concSpace state arrTp
          in (arrTp, PCT.LIndex esz lv' (transExp state idxE) 0)
        tLVRec state (AST.LDot lv field) =
          let (Conc.StructC structIdent, lv') = tLVRec state lv
              (fieldTp, fieldOffset) = getFieldConcAndOffset state structIdent field
              lv'' = addOffset lv' fieldOffset
          in (fieldTp, lv'')

        {-
          Adds the given offset of the preexisting offset for the given lvalue.
        -}
        addOffset :: PCT.LValue -> PCT.Offset -> PCT.LValue
        addOffset (PCT.LIdent _) _ = error "GenPCT.transLValue.addOffset: unexpected IDENT"
        addOffset (PCT.LStar lv Nothing) o' = PCT.LStar lv (Just o')
        addOffset (PCT.LStar lv (Just o)) o' = PCT.LStar lv (Just (o + o'))
        addOffset (PCT.LIndex esz lv idxE o) o' = PCT.LIndex esz lv idxE (o + o')

{-
  Translates an AST expr into a PCT exp by flattening out chains of Dots into offset memory.
-}
transExp :: TransState -> AST.Exp -> PCT.Exp
transExp state e =
  snd $ tERec state e
  where {-
          Translates a given expression from AST to PCT form, also returning its type for use
          in the parent call.
        -}
        tERec :: TransState -> AST.Exp -> (Conc.Conc, PCT.Exp)
        tERec _ (AST.IntLit n) =
          (Conc.IntC, PCT.NumLit (transIntLit n))
        tERec _ (AST.BoolLit b) =
          (Conc.BoolC, PCT.BoolLit b)
        tERec _ (AST.CharLit c) =
          (Conc.CharC, PCT.CharLit $ fromIntegral $ ord c)
        tERec _ (AST.StringLit s) =
          (Conc.StringC, PCT.StringLit s)
        tERec (_, context, _, _, _) (AST.Ident v) =
          (context Map.! v, PCT.Ident v)
        tERec _ AST.Null =
          (Conc.PtrC Conc.Any, PCT.Null)
        tERec state (AST.Unop op e) =
          let (tp, e') = tERec state e
          in  (tp, PCT.Unop op e')
        tERec state (AST.Binop op eL eR) =
          let (tpL, eL') = tERec state eL
              (tpR, eR') = tERec state eR
          in (case (op, tpL, tpR) of
                (AST.ArithOp op, _, _) -> (tpL, PCT.Binop (PCT.ArithOp op) eL' eR')
                (AST.LogOp op, _, _) -> (tpL, PCT.Binop (PCT.LogOp op) eL' eR')
                (AST.CmpOp op, Conc.PtrC (Conc.One Conc.VoidC), _) ->
                  (Conc.BoolC, PCT.CompareTagged op eL' eR')
                (AST.CmpOp op, _, Conc.PtrC (Conc.One Conc.VoidC)) ->
                  (Conc.BoolC, PCT.CompareTagged op eL' eR')
                (AST.CmpOp op, _, _) -> (Conc.BoolC, PCT.Binop (PCT.CmpOp op) eL' eR'))
        tERec state (AST.Cond e1 e2 e3) =
          let (_, e1') = tERec state e1
              (tpL, e2') = tERec state e2
              (tpR, e3') = tERec state e3
              tp = (case (tpL, tpR) of
                      (Conc.PtrC Conc.Any, _) -> tpR
                      (_, _) -> tpL)
          in (tp, PCT.Cond e1' e2' e3')
        tERec state (AST.Call (AST.Ident fName) args) =
          let args' = map (transExp state) args
              (fName', (resTp, _)) = transFnName state fName
          in (resTp, PCT.Call (concSize resTp) fName' args')
        tERec state@(_, _, gs, _, _) (AST.Call (AST.Star e) args) =
          let args' = map (transExp state) args
              (Conc.PtrC ptd, e') = tERec state e
              resTp = case ptd of
                        (Conc.AnyFn (rTp, _)) -> rTp
                        (Conc.One (Conc.FnC fTypName)) ->
                          let (rTp, _) = (sigdefs gs) Map.! fTypName
                          in rTp
          in (resTp, PCT.CallPtr (concSize resTp) e' args')
        tERec _ (AST.Call _ _) = error "GenPCT.transExp.tERec: invalid Call"
        tERec state (AST.Alloc tp) =
          let tp' = resolveType state tp
              sz = concSpace state tp'
          in (Conc.PtrC (Conc.One tp'), PCT.Alloc sz)
        tERec state (AST.AllocArray tp numE) =
          let tp' = resolveType state tp
              sz = concSpace state tp'
              numE' = transExp state numE
          in (Conc.ArrC tp', PCT.AllocArray sz numE')
        tERec state (AST.Star addrE) =
          let (Conc.PtrC (Conc.One tp), addrE') = tERec state addrE
          in (tp, PCT.Star (concSize tp) addrE' 0)
        tERec state (AST.Index arrE idxE) =
          let (Conc.ArrC arrTp, arrE') = tERec state arrE
              (_, idxE') = tERec state idxE
              (sz, esz) = (concSize arrTp, concSpace state arrTp)
          in (arrTp, PCT.Index sz esz arrE' idxE' 0)
        tERec state (AST.Dot structE field) =
          let (Conc.StructC structIdent, structE') = tERec state structE
              (fieldTp, fieldOffset) = getFieldConcAndOffset state structIdent field
              fieldSz = concSize fieldTp
              structE'' = updateMemExp fieldSz fieldOffset structE'
          in (fieldTp, structE'')
        tERec state (AST.Amp fName) =
          let (fName', fnSig) = transFnName state fName
          in (Conc.PtrC (Conc.AnyFn fnSig), PCT.Amp fName')
        tERec state@(_, _, _, _, tMap) (AST.Cast tp e) =
          let tp' = resolveType state tp
              (eTp, e') = tERec state e
          in (case tp' of
                (Conc.PtrC (Conc.One Conc.VoidC)) -> (tp', PCT.Tag e' $ tMap Map.! eTp)
                _ -> (tp', PCT.Untag e' $ tMap Map.! tp'))

        {-
          Translates a numeric literal into an int32.
        -}
        transIntLit :: Common.IntLit -> Int32
        transIntLit (Common.Hex x) = fromIntegral $ x
        transIntLit (Common.Dec n) = fromIntegral $ n

        {-
          From a function name, computes the mangled name and function signature.
        -}
        transFnName :: TransState -> String -> (String, Conc.CSig)
        transFnName (nMap, _, gs, _, _) fName =
          let fName' = nMap Map.! fName
              (_, sig) = (fnSigs gs) Map.! fName
          in (fName', sig)

        {-
          Updates the offset and output size for the given memory expression. Throws an error
          if it's not a memory expression.
        -}
        updateMemExp :: Size -> PCT.Offset -> PCT.Exp -> PCT.Exp
        updateMemExp sz o (PCT.Star _ lv o') = PCT.Star sz lv (o' + o)
        updateMemExp sz o (PCT.Index _ esz lv idxE o') = PCT.Index sz esz lv idxE (o' + o)
        updateMemExp _ _ e =
          error $ "GenPCT.transExp.updateMemExp: got non-memory expression: " ++ show e



{-
  Returns the size of the given concrete type as a Size object.
  NOTE: even though there are no values of type void, we still have to assign it a size, so
        that void functions can be turned into normal functions.
-}
concSize :: Conc.Conc -> Size
concSize (Conc.ArrC _) = Quad
concSize (Conc.PtrC (Conc.AnyFn _)) = Quad
concSize (Conc.PtrC (Conc.One (Conc.FnC _))) = Quad
concSize (Conc.PtrC _) = Quad
concSize (Conc.StructC _) = error "GenPCT.concSize: unexpected large type (struct)"
concSize (Conc.FnC _) = error "GenPCT.concSize: unexpected large type (function)"
concSize Conc.VoidC = Long
concSize Conc.BoolC = Byte
concSize Conc.IntC = Long
concSize Conc.CharC = Byte
concSize Conc.StringC = Quad


{-
  Returns the size of the given concrete type as an integer.
  NOTE: voids are forbidden here (but not in concSize) because this function is only used for
        struct/array size computations, and voids are forbidden in those contexts.
-}
concSpace :: TransState -> Conc.Conc -> PCT.ElemSize
concSpace _ (Conc.ArrC _) = q
concSpace _ (Conc.PtrC (Conc.AnyFn _)) = q
concSpace _ (Conc.PtrC (Conc.One (Conc.FnC _))) = q
concSpace _ (Conc.PtrC _) = q
concSpace (_, _, _, siMap, _) (Conc.StructC sName) =
  let (sz, _, _) = siMap Map.! sName
  in sz
concSpace _ Conc.VoidC = error "GenPCT.ConcSpace: unexpected VOID"
concSpace _ Conc.BoolC = b
concSpace _ Conc.IntC = l
concSpace _ Conc.CharC = b
concSpace _ Conc.StringC = q


{-
  Gets the concrete type and offset for a field of a struct.
-}
getFieldConcAndOffset :: TransState -> String -> String -> (Conc.Conc, PCT.ElemSize)
getFieldConcAndOffset state@(_, _, _, siMap, _) sName fName =
  let (_, _, fiMap) = siMap Map.! sName
  in fiMap Map.! fName

{-
  The sizes of the Common.Size types.
-}
q = Common.bytesForSize Common.Quad
l = Common.bytesForSize Common.Long
b = Common.bytesForSize Common.Byte

{-
  Resolves an AST type to a concrete type.
-}
resolveType :: TransState -> Common.Type -> Conc.Conc
resolveType (_, _, gs, _, _) (Common.DefT ident) = (typedefs gs) Map.! ident
resolveType _ (Common.StructT ident) = Conc.StructC ident
resolveType _ (Common.FnT fnTpName) = Conc.FnC fnTpName
resolveType state (Common.ArrT t) = Conc.ArrC $ resolveType state t
resolveType state (Common.PtrT t) = Conc.PtrC $ Conc.One $ resolveType state t
resolveType _ Common.IntT = Conc.IntC
resolveType _ Common.BoolT = Conc.BoolC
resolveType _ Common.VoidT = Conc.VoidC
resolveType _ Common.CharT = Conc.CharC
resolveType _ Common.StringT = Conc.StringC
