{-# LANGUAGE
        TemplateHaskell,
        UndecidableInstances
  #-}

{-# OPTIONS_GHC
        -fno-warn-incomplete-patterns
        -fno-warn-missing-signatures
        -fno-warn-name-shadowing
        -fno-warn-unused-matches
  #-}

{- This module provides the template to automatically derive a
   Trie implementation from a data type, which uses this data type
   as key. The template generates a trie data type and an instance of the
   KeyMap class for every given key, as described in the paper
   "Efficient, Modular Tries" by Sebastian Fischer and Frank Huch is generated
   using Template Haskell

   Usage: $(deriveTrie [''<keytypename1>,''<keytypename2>,..])
 -}
module Data.Derive.Trie
                   (
                     deriveTrie,
                     tidy, ensureTrie, trieToMaybe -- used in generated code
                    )  where

import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.Maybe (fromMaybe,isJust,fromJust)
import Control.Monad (foldM)
import Data.List (nub,nubBy,find)
import Debug.Trace
import qualified Data.KeyMap as KeyMap
import qualified Data.Map
import qualified Data.IntMap
import Data.Array


-- trie types for some primitive types like Int,Char, ..
prim2trie :: [(Type,Type)]
prim2trie = [(ConT ''Int,    ConT ''Data.IntMap.IntMap),
             (ConT ''Char,   AppT (ConT ''Data.Map.Map) (ConT ''Char)),
             (ConT ''Float,  AppT (ConT ''Data.Map.Map) (ConT ''Float)),
             (ConT ''Double, AppT (ConT ''Data.Map.Map) (ConT ''Double)),
             (ConT ''Array,  AppT (ConT ''Data.Map.Map) (ConT ''Array))
            ]
-- names for the built-in types, whose names would be invalid in user code,
-- e.g. '[]Trie' is a illegal name
nonStandardTrieNamesForKeys :: [(Name,String)]
nonStandardTrieNamesForKeys =
    [(''(),"UnitTrie"),(''[],"ListTrie"),(''(,),"T2Trie"),(''(,,),"T3Trie"),
     (''(,,,),"T4Trie"),(''(,,,,),"T5Trie"),(''(,,,,,),"T6Trie"),
     (''(,,,,,,),"T7Trie")
    ]


-- list of the classes names, which shall occur in the 'deriving' clause
-- empty, because compiling 'deriving' with flag
-- '-fallow-undecidable-instances' traps in ghc-bug (ghc6.6)
classesToDeriveFrom :: [Name]
classesToDeriveFrom = [] --[''Show,''Eq,''Ord]

-- for (Show a, Eq a,..)
standardCxt :: [Name] -> Cxt
standardCxt typevarnames =
  concatMap ((flip mkCxtForClass) typevarnames) classesToDeriveFrom

-- mkCxtForClass ''Eq [a,b] -> (Eq a, Eq b)
mkCxtForClass :: Name -> [Name] -> Cxt
mkCxtForClass _ [] = []
mkCxtForClass classname (t:ts) =
    ClassP classname [VarT t] : mkCxtForClass classname ts

-- main template of this module, for each name given, it builds the data and
-- instance declaration for a trie with the data type noted by the name as key.
-- Further, some helper functions are brought into scope.
-- This template must be called only once within a module, otherwise name
-- clashes are inevitable.
deriveTrie :: [Name] -> Q [Dec]
deriveTrie [] = return []
deriveTrie names = do
  keyDecs <- mapM getDecOfName names
  trieAndInstanceDecs <- mapM deriveTrie' keyDecs
  ns      <- mapM getMaxN keyDecs
  let maxn = maximum ns
  lookupnDecs  <- mapM lookupnD  [0..maxn]
  alternDecs   <- mapM alternD   [0..maxn]
  combinenDecs <- mapM combinenD [0..maxn]
  mapMaybeWithKeynDecs <- mapM mapMaybeWithKeynD [0..maxn]
  toListnDecs  <- mapM toListnD   [0..maxn]
  return (nubBy eqDec (concat trieAndInstanceDecs) ++
          lookupnDecs ++ alternDecs ++ concat combinenDecs ++
          mapMaybeWithKeynDecs ++ toListnDecs)

-- get name's type declaration
getDecOfName :: Name -> Q Dec
getDecOfName name = do
  info <- reify name
  case info of
    TyConI dec -> return dec
    _          -> error "getDecOfName: type constructor expected!"

-- generates the data and instance declaration for a trie with keyDec as key
deriveTrie' :: Dec -> Q [Dec]
deriveTrie' keyDec = do -- Q Monade
       --trace ("derive: "++ nameBase (getNameOfDec keyDec)) (return ())
       (key2trie,trie2keyDecs) <- genTrieDatastructure prim2trie [] keyDec
       let trieDecs = map fst trie2keyDecs
           keyDecs  = map snd trie2keyDecs
           -- sometimes, e.g. when building a Pattern, a dataCon is needed
           -- with type synonyms you may need reify on recently generated types
           -- because this is not possible in TH, the list knownDecs is needed
           -- it is used by getConstrsOfDataDec
           knownDecs = trieDecs ++ keyDecs
           -- log messages to see, what has been generated.
           -- fixxme: does not show used built-in tries
           keyNames = map (nameBase . getNameOfDec . snd) trie2keyDecs
           triePPrints = map (pprint . fst) trie2keyDecs
           loglines = map (\ (t,k) -> k ++ " --> " ++ t)
                          (zip triePPrints keyNames)
       trace (unlines loglines) (return ())
       --trace ("\n" ++ unlines (map (show . fst) trie2keyDecs)) (return ())
       --trace ("\n" ++ unlines (map (show . snd) trie2keyDecs)) (return ())
       instanceDecs <- mapM (uncurry (genKeyMapInstanceDec knownDecs key2trie))
                            trie2keyDecs
       return (trieDecs ++ concat instanceDecs )
               -- ++ lookupnDecs ++ alternDecs ++ (concat combinenDecs))

-- get the maximal arity of all data constructors. is needed to determine max
-- argument of lookupn, altern and combinen.
getMaxN :: Dec -> Q Int
getMaxN dec = getMaxN' [] 0 dec

getMaxN' :: [Name] -> Int -> Dec -> Q Int
getMaxN' visiteds n dec = do
 let name = getNameOfDec dec
 if elem name visiteds
   then return n
   else do cons <- getConstrsOfDataDec [] dec
           let types = map getTypesInCon cons
               maxn  = maximum (n : map length types)
               conTypes =
                   filter isConT (nub (concatMap getBaseTypes (concat types)))
           mconDecs <- mapM (getDecOfType []) conTypes
           let conDecs = map fromJust (filter isJust mconDecs)
           maxns <- mapM (getMaxN' (name : visiteds) maxn) conDecs
           return (maximum (maxn:maxns))

-- get arities of constructors
getNs :: Dec -> Q [Int]
getNs keyDec = do cons <- getConstrsOfDataDec [] keyDec
                  return (map length (map getTypesInCon cons))


-- generates the trie for the given key.
-- key2trie maps keys to already generated tries,
-- knownTrieDecs lists known tries, for lookup in case they need to be reified
genTrieDatastructure :: [(Type,Type)] -> [Dec] -> Dec
                     -> Q ([(Type,Type)],[(Dec,Dec)])
genTrieDatastructure key2trie knownTrieDecs keyDec =
 --trace ("genTrieDatastructure:\n " ++ show key2trie ++ "\n " ++ show knownTrieDecs ++ "\n "++ show keyDec ++ "\n\n") $
 case keyDec of
   TySynD keyName keyTypeVarNames keyType -> do
     if isJust (lookup (ConT keyName) key2trie)
       then return (key2trie,[])
       else do
           let trieBaseName = mkTrieBaseName keyName
           (key2trie',decs,names) <-
               genTrieDataHelper key2trie knownTrieDecs keyName keyTypeVarNames
                                 [[keyType]]
           let knownTrieDecs' = map fst decs ++ knownTrieDecs
           trieDec <- mkTrieNewtypeDec knownTrieDecs' key2trie' trieBaseName
                                       names keyType
           return (key2trie', (trieDec,keyDec) : decs)

   NewtypeD _ keyName keyTypeVarNames con _ -> do
    if isJust (lookup (ConT keyName) key2trie)
      then return (key2trie,[])
      else do
       let [keyType]    = getTypesInCon con
           trieBaseName = mkTrieBaseName keyName
           --trieName = mkName trieBaseName
       (key2trie',decs,names) <-
         genTrieDataHelper key2trie knownTrieDecs keyName keyTypeVarNames
                           [[keyType]]
       let knownTrieDecs' = map fst decs ++ knownTrieDecs
       trieDec <- mkTrieNewtypeDec knownTrieDecs' key2trie' trieBaseName names
                                   keyType
       return (key2trie',(trieDec,keyDec) : decs)

   DataD _ keyName keyTypeVarNames constrs _ ->
    if isJust (lookup (ConT keyName) key2trie)
      then return (key2trie,[])
      else do
       let types    = map getTypesInCon constrs
           trieBaseName = mkTrieBaseName keyName
           trieName = mkName trieBaseName
           knownTrieDecs' = dataDStub trieName : knownTrieDecs
       (key2trie',decs,names) <-
        genTrieDataHelper key2trie knownTrieDecs' keyName keyTypeVarNames types
       let knownTrieDecs'' = map fst decs ++ knownTrieDecs'
       trieDec <- mkTrieDataDec knownTrieDecs'' key2trie' trieBaseName names
                                types
       return (key2trie',(trieDec,keyDec) : decs)
   _ -> error "Can only derive from type, newtype or data declarations!"

-- an empty data declaration
dataDStub :: Name -> Dec
dataDStub name = DataD [] name [] [] []


-- helper for generating the trie datastructure, does the stuff common for
-- data declarations. typesynonyms and newtyps, namely:
-- returns the tries name,typevariables, decs of subtype's tries
genTrieDataHelper :: [(Type,Type)] -> [Dec] -> Name -> [TyVarBndr] -> [[Type]]
                     -> Q ([(Type,Type)],[(Dec,Dec)],(Name,[Name]))
genTrieDataHelper key2trie knownTrieDecs keyName keyTypeVarBndrs constrTypes =
     do
        --trace ("genTrieHelper: " ++ show key2trie ++ "\n" ++ show knownTrieDecs ++"\n" ++nameBase keyName ++ "\n") (return ())
        let keyType = ConT keyName
            trieBaseName = mkTrieBaseName keyName
            trieType = ConT (mkName trieBaseName)
           -- for every type variable of the original type, a type variable for
           -- the corresponding map is needed in trie type declaration (plus a
           -- type variable for values stored in trie)
        valName <- newName "val"
        let keyTypeVarNames = map getNameFromBndr keyTypeVarBndrs
            trieTypeVarNames = map getTrieTypeVar keyTypeVarNames
            var2trie = zip keyTypeVarNames trieTypeVarNames
            key2trie' =
                [(keyType,trieType)] ++
                 key2trie ++ (map (\(x,y) -> (VarT x,VarT y)) var2trie)
            -- generate the tries for the suptypes
            -- baseTypes should only contain VarTs and ConTs
            baseTypes =
               nub (concatMap getBaseTypes (nub (concat constrTypes)))
        mbaseTypeDecs <- mapM (getDecOfType knownTrieDecs)
                              (filter isConT baseTypes)
        let baseTypeDecs = map fromJust (filter isJust mbaseTypeDecs)
        --trace ("genTrieHelper: " ++ show baseTypeDecs++ "\n") (return ())
        (key2trie'',decs) <-
           foldM (\(k2t,ds) d ->do let ktd = knownTrieDecs ++ map fst ds
                                   (k2t',ds') <- genTrieDatastructure k2t ktd d
                                   return (k2t',ds' ++ ds))
                 (key2trie',[])
                 baseTypeDecs
        return (key2trie'',decs,(valName,trieTypeVarNames))

-- using 'show' instead of 'nameBase' is here important,because:
-- show     keyTypeVarName -> a_822037354
-- nameBase keyTypeVarName -> a
-- using mkName instead of newName is also important, because the Decs of
-- built-in types like [] and (,) use identical typevars (a_822083586), which
-- can corrupt the key2trie lists:
--    [..(a_822083586,mapa_1627440534),(a_822083586,mapa_1627440537),..]
getTrieTypeVar :: Name -> Name
getTrieTypeVar keyTypeVarName = mkName ("map" ++ show keyTypeVarName)

-- builds the basename of the trie for the given keyname
-- (usually (keyname ++"Trie))
-- special, non standard key names,like [],(,),.. are replaced by special
-- trienames (ListTrie,T2Trie,..)
mkTrieBaseName :: Name -> String
mkTrieBaseName keyname =
    let nonStandardName = lookup keyname nonStandardTrieNamesForKeys
     in if isJust nonStandardName
         then fromJust nonStandardName
         else nameBase keyname ++ "Trie"

-- builds from several data created before the trie's data declaration
mkTrieDataDec :: [Dec]->[(Type,Type)]-> String -> (Name,[Name]) -> [[Type]]
              -> Q Dec
mkTrieDataDec knownTrieDecs key2trie trieBaseName (valName,trieTypeVarNames)
              constrTypes = do
 conFields <- mapM (mkConField knownTrieDecs valName key2trie) constrTypes
 return (DataD (standardCxt (trieTypeVarNames ++ [valName]))
               (mkName trieBaseName)
               (map PlainTV $ trieTypeVarNames ++ [valName])
               [NormalC (mkName ("No" ++ trieBaseName)) [],
                NormalC (mkName trieBaseName) conFields]
               classesToDeriveFrom)

-- builds from several data created before the trie's type synonym declaration
mkTrieTySynDec :: [Dec] ->[(Type,Type)] -> String -> (Name,[Name]) -> Type
               -> Q Dec
mkTrieTySynDec knownTrieDecs key2trie trieBaseName (valName,trieTypeVarNames)
               keyType = do
    let unAppTrieTypeWithTySyns = replaceKeyByTrie key2trie keyType
    unAppTrieType <-
        replaceTySynTypesByDataType knownTrieDecs unAppTrieTypeWithTySyns
    return (TySynD (mkName trieBaseName)
                   (map PlainTV $ trieTypeVarNames ++ [valName])
                   (AppT unAppTrieType (VarT valName)))

-- builds from several data created before the trie's newtype declaration
-- currently not in use
mkTrieNewtypeDec :: [Dec] ->[(Type,Type)] -> String -> (Name,[Name]) -> Type
             -> Q Dec
mkTrieNewtypeDec knownTrieDecs key2trie trieBaseName (valName,trieTypeVarNames)
                 keyType = do
    let unAppTrieTypeWithTySyns = replaceKeyByTrie key2trie keyType
    unAppTrieType <-
        replaceTySynTypesByDataType knownTrieDecs unAppTrieTypeWithTySyns
    return (NewtypeD []
                     (mkName trieBaseName)
                     (map PlainTV $ trieTypeVarNames ++ [valName])
                     (RecC (mkName trieBaseName)
                           [(mkName ("un"++trieBaseName),
                             NotStrict,
                             AppT unAppTrieType (VarT valName))])
                     [])

-- in the given type, all type synonyms are replaced by the underlying data
-- types
replaceTySynTypesByDataType :: [Dec] -> Type -> Q Type
replaceTySynTypesByDataType knownTrieDecs t = do
  let ot = getOutermostTypeOfType t
      otArgs = getTypeArgs t
  otArgs' <- mapM (replaceTySynTypesByDataType knownTrieDecs) otArgs
  case ot of
   ConT name -> do
     let knownDec = find ((==name).getNameOfDec) knownTrieDecs
     dec <- if isJust knownDec
             then return (fromJust knownDec)
             else do
              info <- reify name
              case info of
               TyConI d -> return d
               i -> error ("replaceTySynTypesByDataType: TyConI expected!\n"++
                           show i)
     case dec of
       TySynD _ tvbndrs (AppT t' val) ->
          let tvlist = map getNameFromBndr tvbndrs
              tv2arg = zip (map VarT tvlist) otArgs'
           in return (replaceArgs tv2arg t')
       TySynD _ _ _ ->
          error "replaceTySynTypesByDataType: invalid trie type synonym!"
       _ -> return (applyTypes ot otArgs')
   v -> do --trace ("in: "++ show t ++ "\nout: " ++ show (applyTypes ot otArgs') ++ "\n") (return ())
           return (applyTypes ot otArgs')


-- if the given type is an application from one type to some args, getTypeArgs
-- returns these args
getTypeArgs :: Type -> [Type]
getTypeArgs (AppT t1 t2) = getTypeArgs t1 ++ [t2]
getTypeArgs _            = []

-- applies type to args
applyTypes :: Type -> [Type] -> Type
applyTypes t [] = t
applyTypes t' (t:ts) = applyTypes (AppT t' t) ts

-- replaces recursively types as specified in the assocList
replaceArgs :: [(Type,Type)] -> Type -> Type
replaceArgs assocList (AppT t1 t2) =
  AppT (replaceArgs assocList t1) (replaceArgs assocList t2)
replaceArgs assocList t =
  let t' = lookup t assocList
  in if isJust t'
      then fromJust t'
      else t

isConT :: Type -> Bool
isConT (ConT _) = True
isConT _        = False

-- if the given type is a type constructor, it's declaration is returned,
-- otherwise Nothing
getDecOfType :: [Dec] -> Type -> Q (Maybe Dec)
getDecOfType knownDecs (ConT name) = do
 let knownDec = find ((==name).getNameOfDec) knownDecs
 if isJust knownDec
   then return knownDec
   else do
     info <- reify name
     case info of
       TyConI tdec -> return (Just tdec)
       _           -> return Nothing
getDecOfType _ _ = return Nothing


-- takes constructor of original datatype and returns corresponding field
-- for trie datatype
mkConField :: [Dec] -> Name -> [(Type,Type)] -> [Type] -> Q StrictType
mkConField knownTrieDecs valname key2trie types = do
 --trace ("mkConField: " ++ show (mkConFieldType valname key2trie types))
  t <- mkConFieldType knownTrieDecs valname key2trie types
  return (IsStrict,t)

mkConFieldType :: [Dec] -> Name -> [(Type,Type)] -> [Type] -> Q Type
mkConFieldType knownTrieDecs valname key2trie types = do
 let replace =
      (replaceTySynTypesByDataType knownTrieDecs) . (replaceKeyByTrie key2trie)
 trieTypesWithoutVal <- mapM replace types
 let trieTypes = addVal trieTypesWithoutVal (VarT valname)
     revTrieTypes = reverse trieTypes
     fieldType = applyTypesAcc (tail revTrieTypes) (head revTrieTypes)
  --trace ("mkConFieldType: " ++ show valname ++ " " ++ show types ++ "\n")
     --replaceTySynTypesByDataType knownTrieDecs fieldType
 return fieldType



isAppT :: Type -> Bool
isAppT (AppT _ _) = True
isAppT _          = False

-- replaces recursively every key type by the corresponding trie type, as
-- specified in key2trie
replaceKeyByTrie :: [(Type,Type)] -> Type -> Type
replaceKeyByTrie key2trie (AppT t1 t2) =
   AppT (replaceKeyByTrie key2trie t1) (replaceKeyByTrie key2trie t2)
replaceKeyByTrie key2trie keyType =
   fromMaybe (AppT (ConT ''Data.Map.Map) keyType) (lookup keyType key2trie)

getTypesInCon :: Con -> [Type]
getTypesInCon (NormalC _ strictTypes) = map snd strictTypes
getTypesInCon (InfixC (_,t1) _ (_,t2)) = [t1,t2]
getTypesInCon (RecC _ varstrictTypes) = map (\(_,_,t) -> t) varstrictTypes
getTypesInCon (ForallC _ _ _) =
  error "Error:getTypesInCon: forallT not supported"


-- returns the undividable types contained in given type. Like removing all
-- AppTs and collecting the single types in a list.
getBaseTypes :: Type -> [Type]
getBaseTypes (ForallT _ _ t) =
    error "Error:getBaseTypes: forallT not supported"
getBaseTypes (AppT t1 t2) = getBaseTypes t1 ++ getBaseTypes t2
getBaseTypes ListT = [ConT ''[]]
getBaseTypes (TupleT _) = [ConT ''(,)]
getBaseTypes ArrowT = error ("Error:getBaseTypes: ArrowT not supported" ++
                             "have you tried to use functions as keys?")
getBaseTypes t = [t]

getNameFromBndr :: TyVarBndr -> Name
getNameFromBndr (PlainTV name) = name
getNameFromBndr (KindedTV name _) = name

-- generates the KeyMap-instance-declaration for given key and trie
genKeyMapInstanceDec :: [Dec] -> [(Type,Type)] -> Dec -> Dec -> Q [Dec]
genKeyMapInstanceDec knownDecs key2trie trieDec keyDec = do
   --trace ("instance: " ++ pprint trieDec) (return ())
 --  let trie2key = map (\ (a,b) -> (b,a)) key2trie
   g_empty   <- gen_empty   knownDecs trieDec
   g_null    <- gen_null    knownDecs trieDec
   g_lookup  <- gen_lookup  knownDecs keyDec trieDec
   g_alter   <- gen_alter   knownDecs keyDec trieDec
   g_combine <- gen_combine knownDecs keyDec trieDec
   g_mapMaybeWithKey <- gen_mapMaybeWithKey knownDecs keyDec trieDec
   g_toList  <- gen_toList  knownDecs keyDec trieDec
   let methods = [ g_empty
                 , g_null
                 , g_lookup
                 , g_alter
                 , g_combine
                 , g_mapMaybeWithKey
                 , g_toList
                 ]

   case trieDec of
     DataD _ triename tvarbndrs _ _ -> do
       let tvarnames = map getNameFromBndr tvarbndrs
       return (mkKeyMapInstanceDec key2trie triename tvarnames methods)
     NewtypeD _ triename tvarbndrs _ _ -> do
       let tvarnames = map getNameFromBndr tvarbndrs
       return (mkKeyMapInstanceDec key2trie triename tvarnames methods)
     _  -> return []
-- builds the instance declaration
mkKeyMapInstanceDec :: [(Type,Type)] -> Name -> [Name] -> [Dec] -> [Dec]
mkKeyMapInstanceDec key2trie triename tvarnames methods =
  let trie2key = map (\ (a,b) -> (b,a)) key2trie
      tvarnamesWithoutVal = take (length tvarnames - 1) tvarnames
      keyMapCxt  = map (mkKeyMapCxt trie2key) tvarnamesWithoutVal
      keyType    = mkKeyType trie2key tvarnamesWithoutVal triename
      trieType   = mkTrieType key2trie keyType
      keyMapType = AppT (AppT (ConT ''KeyMap.KeyMap) keyType) trieType
  in [InstanceD keyMapCxt keyMapType methods]

-- builds the context of the KeyMap-instance-declaration for a given type
-- variable
mkKeyMapCxt :: [(Type,Type)] -> Name -> Pred
mkKeyMapCxt trie2key tvarname =
    let keytvar = fromJust (lookup (VarT tvarname) trie2key)
     in  ClassP ''KeyMap.KeyMap [keytvar, VarT tvarname]

-- builds the key type needed for the KeyMap-instance-declaration
-- variable
mkKeyType :: [(Type,Type)] -> [Name] -> Name -> Type
mkKeyType trie2key tvarnames triename =
    let keytvars = map (fromJust . ((flip lookup) trie2key) . VarT) tvarnames
        keyTypeCon = fromJust (lookup (ConT triename) trie2key)
      in foldl AppT keyTypeCon keytvars

-- builds the trie type needed for the KeyMap-instance-declaration
-- variable
mkTrieType :: [(Type,Type)] -> Type -> Type
mkTrieType key2trie keyType =
    replaceKeyByTrie key2trie keyType

-- generates the empty-method of KeyMap
gen_empty :: [Dec] -> Dec -> Q Dec
gen_empty knownDecs (NewtypeD _ _ _ con _) = do
    let dataconE = conE (getNameOfCon con)
    funD (mkName "empty") [clause [] (normalB [| $dataconE KeyMap.empty|]) []]
gen_empty knownDecs trieDec@(DataD _ _ _ _ _) = do
    noTrieCon <- getNoTrieCon knownDecs trieDec
    funD (mkName "empty")
         [clause [] (normalB (conE (getNameOfCon noTrieCon))) []]


-- generates the null-method of KeyMap
gen_null :: [Dec] -> Dec -> Q Dec
gen_null knownDecs (NewtypeD _ _ _ con _) = do
    (triepat,[varname]) <- mkConPattern con
    let m = varE varname
    funD (mkName "null") [clause [triepat] (normalB [| KeyMap.null ($m)|]) []]
gen_null knownDecs triedec@(DataD _ _ _ _ _) = do
   (emptyTrieConPattern,_) <-
      getNoTrieCon knownDecs triedec >>= mkConPattern
   (nonEmptyTrieConPattern,nonEmptyTrieVarNames) <-
      getNonEmptyTrieCon knownDecs triedec >>= mkConNullPattern knownDecs
   funD (mkName "null")
        [clause [emptyTrieConPattern] (normalB (conE 'True)) [],
         clause [nonEmptyTrieConPattern]
                (nullBody knownDecs triedec nonEmptyTrieVarNames) [],
         clause [wildP] (normalB (conE 'False)) []]

 where mkConNullPattern :: [Dec] -> Con -> Q (PatQ,[Name])
       mkConNullPattern knownDecs con = do
          let types = getTypesInCon con
          patternWithNames <- mapM (getNullPatternForType knownDecs) types
          return (conP (getNameOfCon con) (map fst patternWithNames),
                  concatMap snd patternWithNames)

       getNullPatternForType :: [Dec] ->Type -> Q (PatQ,[Name])
       getNullPatternForType knownDecs t = do
          let ot = getOutermostTypeOfType t
              mdec = find ((== (getNameOfType ot)).getNameOfDec) knownDecs
          if isJust mdec
             then do
               let dec = fromJust mdec
               if isNewtypeD (fromJust mdec)
                then do [con] <- getConstrsOfDataDec knownDecs dec
                        varname <- newName "m"
                        return (conP (getNameOfCon con) [varP varname],
                                [varname])
                else do con <- getNoTrieCon knownDecs (fromJust mdec)
                        return (conP (getNameOfCon con) [],[])
             else do let conName = getNameOfType ot
                     if (conName == ''Maybe)
                      then do  return (conP 'Nothing [],[])
                      else do vname <- newName "x"
                              return (varP vname,[vname])

       nullBody :: [Dec] -> Dec -> [Name] -> BodyQ
       nullBody _ _ [] = normalB (conE 'True)  -- for noTrieCon
       nullBody knownDecs triedec names = do
         con <- getNonEmptyTrieCon knownDecs triedec
         let types = getTypesInCon con
         normalB (appE (varE 'and)
                       (listE (zipWith appE
                                      (map (const (varE 'KeyMap.null)) types)
                                      (map varE names))))



getNameOfType :: Type -> Name
getNameOfType (VarT name) = name
getNameOfType (ConT name) = name
getNameOfType t = error ("Error: getNameOfType: " ++ show t ++ "has no name")

-- returns the data constructors of the given declaration. If the declaration
-- is one of a type synonym, the constructors of the underlying data type are
-- returned
getConstrsOfDataDec :: [Dec] -> Dec -> Q [Con]
getConstrsOfDataDec knownDecs dec =
  case dec of
    DataD _ _ _ cons _ -> return cons
    NewtypeD _ _ _ con _ -> return [con]
    TySynD name _ t    -> do
       let (ConT newname) = getOutermostTypeOfType t
       tdec <- doReify knownDecs newname
       getConstrsOfDataDec knownDecs tdec
    _                 -> error "Error:getConstrsOfDataDec: not implemented!"
 where doReify :: [Dec] -> Name -> Q Dec
       doReify knownDecs name = do
        let knownDec = find ((==name).getNameOfDec) knownDecs
        if isJust knownDec
          then return (fromJust knownDec)
          else do
           info <- reify name
           case info of
             TyConI tdec -> return tdec
             _               -> error "doReify: TyConI expected"

getOutermostTypeOfType :: Type -> Type
getOutermostTypeOfType (AppT t1 t2) = getOutermostTypeOfType t1
getOutermostTypeOfType t = t

-- returns the constructor for the empty trie
-- Assumption: a trie has two cons and the first of them is the one for the
-- empty trie
getNoTrieCon :: [Dec] -> Dec -> Q Con
getNoTrieCon knownDecs triedec = do
  cons <- getConstrsOfDataDec knownDecs triedec
  return (head cons)

-- returns the constructor for the nonempty trie
-- Assumption: a trie has two cons and the first of them is the one for the
-- empty trie
getNonEmptyTrieCon :: [Dec] -> Dec -> Q Con
getNonEmptyTrieCon knownDecs dec = do
  cons <-getConstrsOfDataDec knownDecs dec
  return  (cons !! 1)

-- generates the lookup-method of KeyMap
gen_lookup :: [Dec] -> Dec -> Dec -> Q Dec
gen_lookup knownDecs (NewtypeD _ _ _ keycon _) (NewtypeD _ _ _ triecon _) = do
  (triepat,[trievarname]) <- mkConPattern triecon
  (keypat,[keyvarname])   <- mkConPattern keycon
  let m = varE trievarname
      k = varE keyvarname
  funD (mkName "lookup") [clause [keypat,triepat]
                                 (normalB [| KeyMap.lookup $k $m|])
                                 []]

gen_lookup knownDecs keyDec (NewtypeD _ _ _ triecon _) = do
  (triepat,[trievarname]) <- mkConPattern triecon
  keyvarname <- newName "k"
  let m = varE trievarname
      k = varE keyvarname
  funD (mkName "lookup") [clause [varP keyvarname,triepat]
                                 (normalB [| KeyMap.lookup $k $m|])
                                 []]
gen_lookup knownDecs keyDec trieDec@(DataD _ _ _ _ _) = do
   noTrieCon       <- getNoTrieCon knownDecs trieDec
   nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
   keyCons         <- getConstrsOfDataDec knownDecs keyDec

   emptyTrieClause <- gen_lookupClause noTrieCon Nothing
   nonEmptyTrieClauses <-
      mapM (gen_lookupClause nonEmptyTrieCon)
                             (map Just (zip keyCons
                                            [0..]))
   return (FunD (mkName "lookup") (emptyTrieClause : nonEmptyTrieClauses))


-- generates a clause for the lookup-method of KeyMap
-- (con,n) = key's nth data con => recursively call lookup on nth field of
--                                 nonempty triecon
gen_lookupClause :: Con -> Maybe (Con,Int) -> Q Clause
gen_lookupClause trieCon@(NormalC triename _) mkeyCon = do
  case mkeyCon of
    Nothing -> -- triecon is con for empty trie,no key pattern needed
      clause [wildP, conP triename []] (normalB (conE 'Nothing)) []
    Just (keyCon,n) -> do
      (keyPat,keyVarNames)   <- mkConPattern keyCon
      (triePat,trieVarNames) <- mkConPattern trieCon
      let k = length keyVarNames
      clause [keyPat,triePat]
             (normalB (apply (varE (mkName ("lookup" ++ show k)))
                             (map varE keyVarNames ++
                              [varE (trieVarNames !! n)])))
             []
gen_lookupClause tc _ =
    error ("Error:gen_lookupClause: malformed trie constructor: " ++ show tc)


-- returns a pattern for the given con and a list of the names of the variables
-- for the pattern's fields
mkConPattern ::  Con -> Q (PatQ,[Name])
mkConPattern (NormalC name types) = do
  varPNames <- mapM newName (map (const "x") types)
  return (conP name (map varP varPNames),varPNames)
mkConPattern (RecC name types) = do
  varPNames <- mapM newName (map (const "x") types)
  return (conP name (map varP varPNames),varPNames)
mkConPattern (InfixC t1 name t2) = do
  let types = [t1,t2]
  [varPName1,varPName2] <- mapM newName (map (const "x") types)
  return (infixP (varP varPName1) name (varP varPName2),[varPName1,varPName2])
mkConPattern (ForallC _ _ _) =
  error "Error:mkKeyPattern: ForallC not supported!"


 -- generates the alter-method of KeyMap
gen_alter :: [Dec] -> Dec -> Dec -> Q Dec
gen_alter knownDecs (NewtypeD _ _ _ keycon _) (NewtypeD _ _ _ triecon _) = do
  (triepat,[trievarname]) <- mkConPattern triecon
  (keypat,[keyvarname]) <- mkConPattern keycon
  fvarname   <-newName "f"
  let m = varE trievarname
      f = varE fvarname
      k = varE keyvarname
      c = conE (getNameOfCon triecon)
  funD (mkName "alter")
       [clause [keypat,varP fvarname,triepat]
               (normalB [| $c (KeyMap.alter $k $f $m)|])
               []]

gen_alter knownDecs keyDec (NewtypeD _ _ _ con _) = do
  (triepat,[trievarname]) <- mkConPattern con
  fvarname   <-newName "f"
  keyvarname <- newName "k"
  let m = varE trievarname
      f = varE fvarname
      k = varE keyvarname
      c = conE (getNameOfCon con)
  funD (mkName "alter")
       [clause [varP keyvarname,varP fvarname,triepat]
               (normalB [| $c (KeyMap.alter $k $f $m)|])
               []]
gen_alter knownDecs keyDec trieDec@(DataD _ _ _ _ _) = do
   noTrieCon       <- getNoTrieCon knownDecs trieDec
   nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
   keyCons         <- getConstrsOfDataDec knownDecs keyDec

   emptyTrieClauses <- mapM (gen_alterClause knownDecs trieDec noTrieCon)
                            (zip keyCons [0..])
   nonEmptyTrieClauses <-
      mapM (gen_alterClause knownDecs trieDec nonEmptyTrieCon)
           (zip keyCons [0..])
   return (FunD (mkName "alter") (emptyTrieClauses ++ nonEmptyTrieClauses))

-- generates a clause for the alter-method of KeyMap
-- (con,n) = key's nth data con
gen_alterClause :: [Dec] -> Dec -> Con -> (Con,Int) -> Q Clause
gen_alterClause knownDecs trieDec trieCon@(NormalC triename _) (keyCon,n) =
 --trace ("genalterclause: " ++ show keyCon ++ "\n" ++ show trieCon) $
 do
  nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
  (keyPat,keyVarNames)   <- mkConPattern keyCon
  (triePat,trieVarNames) <- mkConPattern trieCon
  fVarName <- newName "f"

  let emptyTrieFields = mkEmptyTrieFields nonEmptyTrieCon
      oldFields = if trieVarNames == []
                   then map return emptyTrieFields
                   else map varE trieVarNames
      fieldToChange = oldFields !! n
      newField = apply (varE (mkName ("alter" ++ show (length keyVarNames))))
                        (map varE keyVarNames ++ [varE fVarName,fieldToChange])
  clause [keyPat,varP fVarName,triePat]
         (normalB (appE (varE 'tidy)
                        (apply (conE (getConName nonEmptyTrieCon))
                               (take n oldFields ++
                                (newField : drop (n+1) oldFields)))))
         []
gen_alterClause _ _ _ _ =
    error "Error:gen_alterClause: malformed trie constructor!"


getConName :: Con -> Name
getConName (NormalC name _)  = name
getConName (RecC name _)     = name
getConName (InfixC _ name _) = name
getConName _ = error "Error: getConName:Forall not supported"

-- builds default fields for the nonempty triecon
mkEmptyTrieFields :: Con -> [Exp]
mkEmptyTrieFields trieCon =
  map type2empty (getTypesInCon trieCon)
 where type2empty :: Type -> Exp
       type2empty (AppT t _)
           | t == ConT ''Maybe = ConE 'Nothing
           | otherwise = VarE 'KeyMap.empty
       type2empty _                = VarE 'KeyMap.empty


-- generates the combine-method for KeyMap
gen_combine :: [Dec] -> Dec -> Dec -> Q Dec
gen_combine knownDecs keyDec (NewtypeD _ _ _ con _) = do
    (triepat1,[trievarname1]) <- mkConPattern con
    (triepat2,[trievarname2]) <- mkConPattern con
    fname <- newName "f"
    let m1 = varE trievarname1
        m2 = varE trievarname2
        f  = varE fname
        c  = conE (getNameOfCon con)
    funD (mkName "combine")
         [clause [varP fname, triepat1,triepat2]
                 (normalB [| $c (KeyMap.combine $f $m1 $m2)|])
                 []]
gen_combine knownDecs keyDec trieDec@(DataD _ _ _ _ _) = do
   fName <- newName "f"
   ns <- getNs keyDec
   noTrieConName   <- getNoTrieCon knownDecs trieDec >>= (return . getConName)
   nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
   let nonEmptyTrieConFields = mkEmptyTrieFields nonEmptyTrieCon

       nonEmptyTrieConName   = getConName nonEmptyTrieCon
       -- emptyTrie = apply (conE nonEmptyTrieConName)
       --                   (map return nonEmptyTrieConFields)
       noTrieConP = conP noTrieConName []
   (nonEmptyTrieConP1,nonEmptyTrieConNames1) <- mkConPattern nonEmptyTrieCon
   (nonEmptyTrieConP2,nonEmptyTrieConNames2) <- mkConPattern nonEmptyTrieCon
  --combinenDecs <- mapM combinenD ns

   emptyEmptyClause <- clause [wildP,noTrieConP,noTrieConP]
                              (normalB (conE noTrieConName))
                              []
   emptyNonEmptyClause <-
       clause [varP fName,noTrieConP,nonEmptyTrieConP2]
              (normalB (appE (varE 'tidy)
                              (apply (conE nonEmptyTrieConName)
                                     (map (combineField (varE fName))
                                          (zip3 ns
                                                (map return
                                                     nonEmptyTrieConFields)
                                                 (map varE
                                                      nonEmptyTrieConNames2)))
                              )))
              []
   nonEmptyEmptyClause <-
      clause [varP fName,nonEmptyTrieConP1,noTrieConP]
             (normalB (appE (varE 'tidy)
                            (apply (conE nonEmptyTrieConName)
                                   (map (combineField (varE fName))
                                        (zip3 ns
                                              (map varE nonEmptyTrieConNames1)
                                              (map return nonEmptyTrieConFields)
                                        )))))
             []
   nonEmptyNonEmptyClause <-
      clause [varP fName,nonEmptyTrieConP1,nonEmptyTrieConP2]
             (normalB (appE (varE 'tidy)
                            (apply (conE nonEmptyTrieConName)
                                   (map (combineField (varE fName))
                                        (zip3 ns
                                              (map varE nonEmptyTrieConNames1)
                                              (map varE nonEmptyTrieConNames2))
                                   ))))
             []

   return (FunD (mkName "combine") [emptyEmptyClause,emptyNonEmptyClause,
                                   nonEmptyEmptyClause,nonEmptyNonEmptyClause])
 where combineField :: ExpQ -> (Int,ExpQ,ExpQ) -> ExpQ
       combineField f (0,a,b) = apply (varE (mkName "combine0")) [f,a,b]
       combineField f (n,a,b) =
          {- appE (varE 'ensureTrie)
                (apply (varE (mkName ("combine"++show n)))
                       [f,appE (varE 'trieToMaybe) a,
                          appE (varE 'trieToMaybe) b])-}
                apply (varE (mkName ("combine"++show n))) [f, a, b]

gen_mapMaybeWithKey :: [Dec] -> Dec -> Dec -> Q Dec
gen_mapMaybeWithKey knownDecs (NewtypeD _ _ _ keycon _)
                              (NewtypeD _ _ _ triecon _) = do
  (triepat,[trievarname]) <- mkConPattern triecon
  fvarname   <-newName "f"
  let m = varE trievarname
      f = varE fvarname
      c = conE (getNameOfCon triecon)
  funD (mkName "mapMaybeWithKey")
       [clause [varP fvarname,triepat]
               (normalB [| $c (KeyMap.mapMaybeWithKey $f $m)|])
               []]

gen_mapMaybeWithKey knownDecs keyDec (NewtypeD _ _ _ con _) = do
  (triepat,[trievarname]) <- mkConPattern con
  fvarname   <-newName "f"
  let m = varE trievarname
      f = varE fvarname
      c = conE (getNameOfCon con)
  funD (mkName "mapMaybeWithKey")
       [clause [varP fvarname,triepat]
               (normalB [| $c (KeyMap.mapMaybeWithKey $f $m)|])
               []]
gen_mapMaybeWithKey knownDecs keyDec trieDec@(DataD _ _ _ _ _) = do
   noTrieCon       <- getNoTrieCon knownDecs trieDec
   let noTrieName = getNameOfCon noTrieCon
   nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
   fvarname   <-newName "f"

   emptyTrieClause <- clause [varP fvarname, conP noTrieName []]
                             (normalB $ conE noTrieName) []
   nonEmptyTrieClause <-
      gen_mapMaybeWithKeyClause knownDecs keyDec trieDec nonEmptyTrieCon
   return (FunD (mkName "mapMaybeWithKey")
                [emptyTrieClause, nonEmptyTrieClause])

gen_mapMaybeWithKeyClause :: [Dec] -> Dec -> Dec -> Con -> Q Clause
gen_mapMaybeWithKeyClause knownDecs keyDec trieDec trieCon@(NormalC triename _) =
 do
  nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
  (triePat,trieVarNames) <- mkConPattern trieCon
  fVarName <- newName "f"
  ns <- getNs keyDec
  cons <- getConstrsOfDataDec knownDecs keyDec

  let oldFields = map varE trieVarNames
      newFields = zipWith3 (\ n con o -> apply (varE 
                                                (mkName $
                                                 "mapMaybeWithKey" ++ show n))
                                               [conE $ getNameOfCon con, o,
                                                    varE fVarName])
                           ns cons oldFields
  clause [varP fVarName,triePat]
         (normalB (appE (varE 'tidy)
                        (apply (conE (getConName nonEmptyTrieCon)) newFields)))
         []
gen_mapMaybeWithKeyClause _ _ _ _ =
    error "Error:gen_mapMaybeWithKeyClause: malformed trie constructor!"

{-
nth :: String -> Int -> [a] -> a
nth descr n list = --trace ("nth " ++ show n ++ " " ++ descr ++ "\n")
                   (nth' descr n list)

nth' :: String -> Int -> [a] -> a
nth' d _ [] = error (d ++ ": nth: Index to large!")
nth' _ 0 (x:_) = x
nth' d n (_:xs) = nth' d (n-1) xs

fromJust' :: String -> Maybe a -> a
fromJust' d Nothing = error d
fromJust' _ (Just a) = a

reify' :: String -> Name -> Q Info
reify' str name = do
  --trace (str++ " " ++ nameBase name) (return ())
  reify name

-}
getNameOfDec :: Dec -> Name
getNameOfDec (FunD name _)           = name
getNameOfDec (DataD _ name _ _ _)    = name
getNameOfDec (NewtypeD _ name _ _ _) = name
getNameOfDec (TySynD name _ _)       = name
getNameOfDec (ClassD _ name _ _ _)   = name
getNameOfDec (SigD name _)           = name
getNameOfDec dec = error ("getNameOfDec: " ++ show dec ++ " has no name!")

-- equality test for decs, (==) is not suitable,because the same decs with
-- only different named variables are supposed to be different
eqDec :: Dec -> Dec -> Bool
eqDec (InstanceD _ t1 _) (InstanceD _ t2 _) = eqTypeIgnoreVarNames t1 t2
eqDec (InstanceD _ t1 _) d2 = False
eqDec d1 (InstanceD _ t1 _) = False
eqDec d1 d2 = getNameOfDec d1 == getNameOfDec d2

-- equality test for Types, (==) is not suitable,because the same Types with
-- only different named variables are supposed to be different
eqTypeIgnoreVarNames :: Type -> Type -> Bool
eqTypeIgnoreVarNames (AppT t1 t2) (AppT t1' t2') =
    eqTypeIgnoreVarNames t1 t1' && eqTypeIgnoreVarNames t2 t2'
eqTypeIgnoreVarNames (ForallT names cxt t) (ForallT names' cxt' t') =
  eqTypeIgnoreVarNames t t'  -- for use here, this is sufficient
eqTypeIgnoreVarNames (VarT _) (VarT _) = True
eqTypeIgnoreVarNames t1 t2 = t1 == t2

getNameOfCon :: Con -> Name
getNameOfCon (NormalC name _)  = name
getNameOfCon (RecC name _)     = name
getNameOfCon (InfixC _ name _) = name
getNameOfCon (ForallC _ _ con) = getNameOfCon con

-- makes expression, where f is applied to args
apply :: ExpQ -> [ExpQ] -> ExpQ
apply f args = foldl appE f args


applyTypesAcc :: [Type] -> Type -> Type
applyTypesAcc [] acc = acc
applyTypesAcc (t:ts) acc = applyTypesAcc ts (AppT t acc)

-- when building fields of trie, one needs to add the val - variable
-- this is done by this helper function
addVal :: [Type] -> Type -> [Type]
addVal [] valtype     = [AppT (ConT ''Maybe) valtype]
addVal [t] valtype    = [AppT t valtype]
addVal (t:ts) valtype = t : addVal ts valtype



-- this function lifts the type (map val -> map val) to
-- (Maybe (map val) -> Maybe (map val))
-- for use with alter-continuation
lift1 :: KeyMap.KeyMap key map
      => (map val -> map val) -> Maybe (map val) -> Maybe (map val)
lift1 f =  trieToMaybe . f . maybe KeyMap.empty id


-- this function lifts the type (map val -> map val' -> map val'') to
-- (Maybe (map val) -> Maybe (map val') -> Maybe (map val''))
-- for use with combine
lift2 :: KeyMap.KeyMap key map
      => (map val -> map val' -> map val'')
      -> Maybe (map val) -> Maybe (map val') -> Maybe (map val'')
lift2 f mx my
  = maybe (my >>= trieToMaybe . f KeyMap.empty)
          (trieToMaybe . flip f (maybe KeyMap.empty id my))
          mx



--tidym :: KeyMap.KeyMap key map => map val -> Maybe (map val)
--tidym t = if KeyMap.null t then Nothing else Just t

tidy :: KeyMap.KeyMap key map => map val -> map val
tidy m = if KeyMap.null m then KeyMap.empty else m

trieToMaybe :: KeyMap.KeyMap key map => map val -> Maybe (map val)
trieToMaybe t = if KeyMap.null t then Nothing else Just t



ensureTrie :: KeyMap.KeyMap key map => Maybe (map val) -> map val
ensureTrie m = fromMaybe KeyMap.empty m

maybe2trie :: ExpQ
maybe2trie = [| \ mt -> fromMaybe KeyMap.empty mt |]


-- generates the declaration for the helper-function lookup1,lookup2,..
-- according to n
lookupnD :: Int -> DecQ
lookupnD 0 = do
 let lookupName = mkName "lookup0"
 (funD lookupName [clause [] (normalB (varE 'id)) []])
lookupnD n = do
  kvarNames <- mapM newName (replicate n "key")
  mvarName <- newName "m"
  tmpvarNames <- mapM newName (take (n-1) (repeat "x"))
  let argNames = kvarNames ++ [mvarName]
      args = map varP argNames
      lookups =
          map (\ (n1,n2,k) -> bindS (varP n2) (apply (varE 'KeyMap.lookup)
                                                     [varE k,varE n1]))
              (zip3 (mvarName:tmpvarNames) tmpvarNames kvarNames)
      lookupName = mkName ("lookup" ++ show n)
  (funD lookupName
        [clause args
                (normalB (doE (lookups ++
                               [noBindS (apply (varE 'KeyMap.lookup)
                                               [varE (last kvarNames),
                                                varE (last (mvarName:tmpvarNames))])]))) []])


-- generates the declaration for the helper-function alter1,alter2,..
-- according to n
alternD :: Int -> DecQ
alternD 0 = do
    -- let alterName = mkName "alter0"
    [alterDec] <- [d| alter0 = id |]
    return alterDec
alternD 1 = do
  [alterDec] <- [d| alter1 k f m = {-tidy-} (KeyMap.alter k f m) |]
  return alterDec
alternD n = do
  let alterName = mkName ("alter"++show n)
      --alter1Name = mkName "alter1"
      alternMinus1Name = mkName ("alter"++show (n-1))
  kvarNames <- mapM newName (replicate n "key")
  mvarName <- newName "m"
  fvarName <- newName "f"
  let argNames = kvarNames ++ [fvarName,mvarName]
      args = map varP argNames
      kvars = map varE kvarNames
      continuation = [| trieToMaybe .
                        $(apply (varE 'KeyMap.alter)
                                [last kvars,varE fvarName])
                        . ensureTrie |]
  (funD alterName
        [clause args (normalB (apply (varE alternMinus1Name)
                                     (take (n-1) kvars ++
                                      [continuation,varE mvarName])))  []])


-- generates the declaration for the helper-function combine1,combine2,..
-- according to n
combinenD :: Int -> Q [Dec]
combinenD 0 =
  [d| combine0 :: (Maybe val -> Maybe val' -> Maybe val'') -> Maybe val -> Maybe val' -> Maybe val'';combine0 f = f |]

combinenD 1 = do
  [d| combine1 :: KeyMap.KeyMap key map => (Maybe val -> Maybe val' -> Maybe val'') ->  (map val) -> (map val') -> (map val'');combine1 f ma mb = {-tidy-} (KeyMap.combine f ma mb) |]

combinenD n =
  do
   let combineName = mkName ("combine"++show n)
   valNames <- mapM newName (replicate 3 "val")
   keyNames <- mapM newName (replicate n "key")
   mapNames <- mapM newName (replicate n "map")

   fname  <- newName "f"
   maname <- newName "ma"
   mbname <-  newName "mb"
   --trace ("valNames: " ++ show valNames) (return ())
   let context = map (\ (k,m) -> (ClassP ''KeyMap.KeyMap [k, m]))
                     (zip (map VarT keyNames) (map VarT mapNames))
  --   f = (Maybe val -> Maybe val' -> Maybe val'')
       f = AppT (AppT ArrowT (AppT (ConT ''Maybe) (VarT (valNames!!0))))
                (AppT (AppT ArrowT (AppT (ConT ''Maybe) (VarT (valNames!!1))))
 	             (AppT (ConT ''Maybe) (VarT (valNames!!2))))
       args = map (mkArg mapNames) valNames
       singleTypes = addVal (map (AppT ArrowT) (take 2 args)) (args!!2)
       sigType =
        AppT (AppT ArrowT f)
             (applyTypesAcc (tail (reverse singleTypes)) (head (reverse singleTypes)))
       sig  = SigD combineName
                   (ForallT (map PlainTV $ keyNames ++ mapNames ++ valNames) context sigType)
       [fvar,mavar,mbvar] = [varE fname,varE maname,varE mbname]
       continuation =
           [| \ ma mb -> trieToMaybe ($(varE 'KeyMap.combine) $fvar
                                                              (ensureTrie ma)
                                                              (ensureTrie mb))|]
   def <- (funD combineName
                [clause [varP fname,varP maname,varP mbname]
                        (normalB [| $(combinen (n-1)) $continuation $mavar
                                                                    $mbvar|])
                        []])
   return [sig,def]
  where mkArg :: [Name] -> Name -> Type
        mkArg mapNames valName =
           let singleTypes = addVal (map VarT mapNames) (VarT valName)
            in (applyTypesAcc (tail (reverse singleTypes))
                              (head (reverse singleTypes)))


combinen :: Int -> ExpQ
combinen n = varE (mkName ("combine" ++ show n))

mapMaybeWithKeynD :: Int -> DecQ
mapMaybeWithKeynD 0 = do
  [dec] <- [d|
    mapMaybeWithKey0 _ Nothing  _ = Nothing
    mapMaybeWithKey0 k (Just v) f = f k v
    |]
  return dec
mapMaybeWithKeynD n = do
  let fname = mkName $ "mapMaybeWithKey" ++ show n
  kf    <- newName "kf"
  t     <- newName "t"
  f     <- newName "f"

  let e = mapMaybeWithKeynD' n (varE t) (varE f) (varE kf) []

  funD fname [clause [varP kf, varP t, varP f] (normalB e) []]

mapMaybeWithKeynD' 1 t f kf ks =
  [| KeyMap.mapMaybeWithKey (\ k v -> $f ($(apply kf $ reverse ks) k) v) $t
    |]
mapMaybeWithKeynD' m t f kf ks = do
  k <- newName "k"
  v <- newName "v"
  let kp = varP k
      ke = varE k
      vp = varP v
      ve = varE v

  apply [| KeyMap.mapMaybeWithKey |]
        [lamE [kp, vp] $ appE [| Just |] $
                mapMaybeWithKeynD' (m - 1) ve f kf $ ke : ks, t]
        

{-
  [|  (\ k $vp -> Just $ $(mapMaybeWithKeynD (m - 1) $v $f $kf))
    |]
-}

-- generates the toList-method of KeyMap
gen_toList :: [Dec] -> Dec -> Dec -> Q Dec
gen_toList knownDecs keyDec (NewtypeD _ _ _ con _) = do
    (triepat,[trievarname]) <- mkConPattern con
    let m = varE trievarname
    funD (mkName "toList") [clause [triepat] (normalB [| KeyMap.toList $m|]) []]
gen_toList knownDecs keyDec trieDec@(DataD _ _ _ _ _) = do
   noTrieCon       <- getNoTrieCon knownDecs trieDec
   nonEmptyTrieCon <- getNonEmptyTrieCon knownDecs trieDec
   ns <- getNs keyDec
   emptyTrieClause <- clause [conP (getNameOfCon noTrieCon) []]
                             (normalB (conE '[])) []
   nonEmptyTrieClause <- gen_toListClause nonEmptyTrieCon keyDec
   return (FunD (mkName "toList") [emptyTrieClause,nonEmptyTrieClause])


gen_toListClause :: Con -> Dec -> Q Clause
gen_toListClause trieCon@(NormalC triename _) keyDec  = do
      (triePat,trieVarNames) <- mkConPattern trieCon
      ns <- getNs keyDec
      let toLists = map (varE . mkName . ("toList"++) . show) ns
          recCalls = zipWith appE toLists (map varE trieVarNames)

      clause [triePat]
             (normalB (foldr appE
                             (last recCalls)
                             (init (map (appE (varE '(++))) recCalls))))
             []
gen_toListClause tc _ =
    error ("Error:gen_toListClause: malformed trie constructor: " ++ show tc)

{-
toListn2 :: (KeyMap.KeyMap ak am, KeyMap.KeyMap bk bm) => (am (bm val)) -> [val]
toListn2 m = concatMap KeyMap.toList (KeyMap.toList m)

toListn3 m = concatMap KeyMap.toList (concatMap KeyMap.toList (KeyMap.toList m))
-}
-- generates the declaration for the helper-function toList1,toList2,..
-- according to n
toListnD :: Int ->  DecQ
toListnD 0 = do [toList0Dec] <- [d| toList0   = maybe [] (\mx -> [mx]) |]
                return toList0Dec
toListnD 1 = do [toList1Dec] <- [d| toList1 m = KeyMap.toList m |]
                return toList1Dec

toListnD n = do
  mName <- newName "m"
  let toListName = mkName ("toList" ++ show n)
      args = [varP mName]
      concatMaps = replicate (n-1) (appE (varE 'concatMap)
                                         (varE 'KeyMap.toList))
      body = normalB (foldr appE
                            (appE (varE (mkName "toList1")) (varE mName))
                            concatMaps)
  funDec <- funD toListName [clause args body []]
  return funDec



isNewtypeD :: Dec -> Bool
isNewtypeD (NewtypeD _ _ _ _ _) = True
isNewtypeD _ = False


