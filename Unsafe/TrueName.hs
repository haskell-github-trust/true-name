{-# LANGUAGE CPP #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Unsafe.TrueName (trueName, quasiName) where

import Control.Applicative
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
#if MIN_VERSION_template_haskell(2,8,0)
    hiding (trueName)
#endif

concatMapM :: (Functor m, Monad m) => (a -> m [b]) -> [a] -> m [b]
concatMapM f xs = concat <$> mapM f xs

conNames :: Con -> [Name]
conNames con = case con of
    NormalC name _ -> [name]
    RecC name fields -> name : map (\ (fname, _, _) -> fname) fields
    InfixC _ name _ -> [name]
    ForallC _ _ con' -> conNames con'

decNames :: Dec -> Q [Name]
decNames dec = case dec of
    FunD _ _ -> return []
    ValD _ _ _ -> return []
    DataD _ _ _ cons _ -> return (conNames =<< cons)
    NewtypeD _ _ _ con _ -> return (conNames con)
    TySynD _ _ typ -> typNames typ
    ClassD _ _ _ _ decs -> concatMapM decNames decs
    InstanceD cxt typ decs -> (++) <$> concatMapM predNames cxt
        <*> ((++) <$> typNames typ <*> concatMapM decNames decs)
    SigD name typ -> (:) name <$> typNames typ
    ForeignD frgn -> case frgn of
        ImportF _ _ _ name t -> (:) name <$> typNames t
        ExportF _ _ name t -> (:) name <$> typNames t
#if MIN_VERSION_template_haskell(2,8,0)
    InfixD _ _ -> return []
#endif
    PragmaD _ -> return []
    FamilyD _ _ _ _ -> return []
    DataInstD cxt _ _ cons names -> (++) (conNames =<< cons)
        . (++) names <$> concatMapM predNames cxt
    NewtypeInstD cxt _ _ con names -> (++) (conNames con)
        . (++) names <$> concatMapM predNames cxt
#if MIN_VERSION_template_haskell(2,9,0)
    TySynInstD _ tse -> tseNames tse
    ClosedTypeFamilyD _ _ _ tses -> concatMapM tseNames tses
    RoleAnnotD _ _ -> return []

tseNames :: TySynEqn -> Q [Name]
tseNames (TySynEqn ts t) = (++) <$> concatMapM typNames ts <*> typNames t
#else
    TySynInstD _ ts t -> (++) <$> concatMapM typNames ts <*> typNames t
#endif

predNames :: Pred -> Q [Name]
predNames p = case p of
    ClassP n ts -> (:) n <$> concatMapM typNames ts
    EqualP s t -> (++) <$> typNames s <*> typNames t

typNames :: Type -> Q [Name]
typNames typ = case typ of
    ForallT _ c t -> (++) <$> concatMapM predNames c <*> typNames t
    AppT s t -> (++) <$> typNames s <*> typNames t
    SigT t _ -> typNames t
    VarT _ -> return []
    ConT name -> return [name]
    TupleT _ -> return []
    UnboxedTupleT _ -> return []
    ArrowT -> return []
    ListT -> return []
#if MIN_VERSION_template_haskell(2,8,0)
    PromotedT _ -> return []
    PromotedTupleT _ -> return []
    PromotedNilT -> return []
    PromotedConsT -> return []
    StarT -> return []
    ConstraintT -> return []
    LitT _ -> return []
#endif

infoNames :: Info -> Q [Name]
infoNames info = case info of
    ClassI dec _ -> decNames dec
    ClassOpI _ typ _ _ -> typNames typ
    TyConI dec -> decNames dec
    FamilyI _ decs -> concatMapM decNames decs
    DataConI _ typ parent _ -> (:) parent <$> typNames typ
    VarI _ typ _ _ -> typNames typ
    PrimTyConI _ _ _ -> return []
    TyVarI _ typ -> typNames typ

-- | Scrapes a qualified 'Name' out from a point-of-entry that you do have
-- access to. First argument is the 'nameBase' part of the required 'Name',
-- while the second is some other 'Name' that contains the required 'Name'
-- in its type or declaration.
--
-- Check the
-- <https://github.com/liyang/true-name/blob/master/sanity.hs included examples>.
trueName :: String -> Name -> Q Name
trueName base thing = do
    cons <- infoNames =<< reify thing
    case filter ((==) base . nameBase) cons of
        [name] -> return name
        _ -> fail $ "trueName: you wanted " ++ show base
            ++ ", but I only have " ++ show (map nameBase cons)

-- | 'QuasiQuoter' interface to 'trueName'. Accepts two or more
-- corresponding argument tokens: first should be sans @""@-quotes; the
-- namespace for the second is denoted in the usual TH syntax of either
-- a single @'@ or double @''@ prefix.
--
-- Extra tokens are assigned as variable names in a 'Pat' context. 'Exp' and
-- 'Type' are always created with 'ConE' and 'ConT' respectively, so this is
-- not quite as flexible as 'trueName'.
quasiName :: QuasiQuoter
quasiName = QuasiQuoter
    { quoteExp = fmap (ConE . snd) . name
    , quotePat = fmap (uncurry $ flip ConP) . name
    , quoteType = fmap (ConT . snd) . name
    , quoteDec = \ _ -> fail "quasiName: I'm not sure how this works."
    } where
    name spec = do
        (base, extra, (things, m'thing)) <- case words spec of
            base : s0 : extra -> (,,) base extra <$> case s0 of
                '\'' : s1 -> case s1 of
                    '\'' : s2 -> (,) s2 <$> lookupTypeName s2
                    _ -> (,) s1 <$> lookupValueName s1
                _ -> return (s0, Just $ mkName s0) -- unhygenic, says TH docs
            _ -> fail $ "quasiName: can't parse spec: " ++ spec
        let nope = fail $ "quasiName: not in scope: " ++ things
        (,) (pat <$> extra) <$> maybe nope (trueName base) m'thing
    pat n = case n of
        "_" -> WildP
        _ -> VarP (mkName n)

