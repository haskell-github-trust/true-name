{-# LANGUAGE CPP #-}

-- | Refer to <https://github.com/liyang/true-name/blob/master/sanity.hs these examples>.

module Unsafe.TrueName (summon, quasiName) where

import Prelude
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif
import Data.List (nub)
import Language.Haskell.TH.Ppr
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax

conNames :: Con -> [Name]
conNames con = case con of
    NormalC name _ -> [name]
    RecC name fields -> name : concat
        [ fname : typNames typ | (fname, _, typ) <- fields ]
    InfixC _ name _ -> [name]
    ForallC _ _ con' -> conNames con'

decNames :: Dec -> [Name]
decNames dec = case dec of
    FunD _ _ -> []
    ValD _ _ _ -> []
    DataD _ _ _ cons _ -> conNames =<< cons
    NewtypeD _ _ _ con _ -> conNames con
    TySynD _ _ typ -> typNames typ
    ClassD _ _ _ _ decs -> decNames =<< decs
    InstanceD cxt typ decs -> (predNames =<< cxt)
        ++ typNames typ ++ (decNames =<< decs)
    SigD name typ -> name : typNames typ
    ForeignD frgn -> case frgn of
        ImportF _ _ _ name t -> name : typNames t
        ExportF _ _ name t -> name : typNames t
#if MIN_VERSION_template_haskell(2,8,0)
    InfixD _ _ -> []
#endif
    PragmaD _ -> []
    FamilyD _ _ _ _ -> []
    DataInstD cxt _ _ cons names -> (conNames =<< cons)
        ++ names ++ (predNames =<< cxt)
    NewtypeInstD cxt _ _ con names -> conNames con
        ++ names ++ (predNames =<< cxt)
#if !MIN_VERSION_template_haskell(2,9,0)
    TySynInstD _ ts t -> (typNames =<< ts) ++ typNames t
#else
    TySynInstD _ tse -> tseNames tse
    ClosedTypeFamilyD _ _ _ tses -> tseNames =<< tses
    RoleAnnotD _ _ -> []
#endif
#if MIN_VERSION_template_haskell(2,10,0)
    StandaloneDerivD cxt typ -> (predNames =<< cxt) ++ typNames typ
    DefaultSigD _ _ -> []
#endif

#if MIN_VERSION_template_haskell(2,9,0)
tseNames :: TySynEqn -> [Name]
tseNames (TySynEqn ts t) = (typNames =<< ts) ++ typNames t
#endif

predNames :: Pred -> [Name]
#if !MIN_VERSION_template_haskell(2,10,0)
predNames p = case p of
    ClassP n ts -> n : (typNames =<< ts)
    EqualP s t -> typNames s ++ typNames t
#else
predNames = typNames
#endif

typNames :: Type -> [Name]
typNames typ = case typ of
    ForallT _ c t -> (predNames =<< c) ++ typNames t
    AppT s t -> typNames s ++ typNames t
    SigT t _ -> typNames t
    VarT _ -> []
    ConT name -> [name]
    TupleT _ -> []
    UnboxedTupleT _ -> []
    ArrowT -> []
    ListT -> []
#if MIN_VERSION_template_haskell(2,8,0)
    PromotedT _ -> []
    PromotedTupleT _ -> []
    PromotedNilT -> []
    PromotedConsT -> []
    StarT -> []
    ConstraintT -> []
    LitT _ -> []
#endif
#if MIN_VERSION_template_haskell(2,10,0)
    EqualityT -> []
#endif

infoNames :: Info -> [Name]
infoNames info = case info of
    ClassI dec _ -> decNames dec
    ClassOpI _ typ _ _ -> typNames typ
    TyConI dec -> decNames dec
    FamilyI _ decs -> decNames =<< decs
    DataConI _ typ parent _ -> parent : typNames typ
    VarI _ typ _ _ -> typNames typ
    PrimTyConI _ _ _ -> []
    TyVarI _ typ -> typNames typ

-- | Summons a 'Name' using @template-haskell@'s 'reify' function.
--
-- The first argument is a 'String' matching the 'Name' we want: either its
-- 'nameBase', or qualified with its module. The second argument gives the
-- 'Name' to 'reify'.
--
-- If no match is found or there is some ambiguity, 'summon' will fail with
-- a list of 'Name's found, along with the output of 'reify' for reference.
--
-- Suppose we are given a module @M@ that exports a function @s@, but not
-- the type @T@, the constrcutor @C@, nor the field @f@:
--
-- > module M (s) where
-- > newtype T = C { f :: Int }
-- > s :: T -> T
-- > s = C . succ . f
--
-- In our own module we have no legitimate way of passing @s@ an argument of
-- type @T@. We can get around this in a type-safe way with 'summon':
--
-- >{-# LANGUAGE TemplateHaskell #-}
-- >module Main where
-- >import Language.Haskell.TH.Syntax
-- >import Unsafe.TrueName
-- >import M
-- >
-- >type T = $(fmap ConT $ summon "T" 's)
-- >mkC :: Int -> T; unC :: T -> Int; f :: T -> Int
-- >mkC = $(fmap ConE $ summon "C" =<< summon "T" 's)
-- >unC $(fmap (`ConP` [VarP $ mkName "n"]) $ summon "C" =<< summon "T" 's) = n
-- >f = $(fmap VarE $ summon "f" =<< summon "T" 's)
-- >
-- >main :: IO ()
-- >main = print (unC t, n) where
-- >    t = s (mkC 42 :: T)
-- >    n = f (s t)
--
-- Note that 'summon' cannot obtain the 'Name' for an unexported function,
-- since GHC <http://hackage.haskell.org/package/template-haskell/docs/Language-Haskell-TH.html#v:VarI does not currently return the RHS of function definitons>.
-- The only workaround is to copypasta the definition. D:
summon :: String -> Name -> Q Name
summon name thing = do
    info <- reify thing
    let ns = nub (infoNames info)
    case filter (\ n -> name == nameBase n || name == show n) ns of
        [n] -> return n
        _ -> fail $ "summon: you wanted " ++ show name ++ ", but I have:\n"
            ++ unlines ((++) "        " . namespace <$> ns)
            ++ "    reify " ++ show thing ++ " returned:\n"
            ++ show (nest 8 $ ppr info)
  where
    namespace n@(Name _ flavour) = show n ++ case flavour of
        NameG VarName _ _ -> " (var)"
        NameG DataName _ _ -> " (cons)"
        NameG TcClsName _ _ -> " (type)"
        _ -> " (?)"

-- | 'QuasiQuoter' interface to 'summon'. Accepts two or more
-- corresponding argument tokens: first should be sans @""@-quotes; the
-- namespace for the second is denoted in the usual TH syntax of either
-- a single @'@ or double @''@ prefix.
--
-- Extra tokens are assigned as variable names in a 'Pat' context. 'Exp' and
-- 'Type' are always created with 'ConE' and 'ConT' respectively, so this is
-- not quite as flexible as 'summon'.
quasiName :: QuasiQuoter
quasiName = QuasiQuoter
    { quoteExp = fmap (ConE . fst) . nameVars
    , quotePat = fmap (uncurry ConP) . nameVars
    , quoteType = fmap (ConT . fst) . nameVars
    , quoteDec = \ _ -> fail "quasiName: I'm not sure how this works."
    } where
    nameVars spec = do
        (name, extra, (things, m'thing)) <- case words spec of
            name : s0 : extra -> (,,) name extra <$> case s0 of
                '\'' : s1 -> case s1 of
                    '\'' : s2 -> (,) s2 <$> lookupTypeName s2
                    _ -> (,) s1 <$> lookupValueName s1
                _ -> return (s0, Just $ mkName s0) -- unhygenic, says TH docs
            _ -> fail $ "quasiName: can't parse spec: " ++ spec
        let nope = fail $ "quasiName: not in scope: " ++ things
        flip (,) (pat <$> extra) <$> maybe nope (summon name) m'thing
    pat n = case n of
        "_" -> WildP
        '!' : ns -> BangP (pat ns)
        '~' : ns -> TildeP (pat ns)
        _ -> VarP (mkName n)

