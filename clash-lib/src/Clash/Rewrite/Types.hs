{-|
  Copyright  :  (C) 2012-2016, University of Twente,
                    2016     , Myrtle Software Ltd,
                    2017     , Google Inc.,
                    2021     , QBayLogic B.V.
  License    :  BSD2 (see the file LICENSE)
  Maintainer :  QBayLogic B.V. <devops@qbaylogic.com>

  Type and instance definitions for Rewrite modules
-}

{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Clash.Rewrite.Types where

import Control.Concurrent.Supply             (Supply, freshId)
import Control.DeepSeq                       (NFData)
import Control.Lens                          (Lens', use, (.=))
import qualified Control.Lens as Lens
#if !MIN_VERSION_base(4,13,0)
import Control.Monad.Fail                    (MonadFail(fail))
#endif
import Control.Monad.Fix                     (MonadFix (..), fix)
import Control.Monad.Reader                  (MonadReader (..))
import Control.Monad.State                   (MonadState (..))
import Control.Monad.State.Strict            (State)
import Control.Monad.Writer                  (MonadWriter (..))
import Data.Binary                           (Binary)
import Data.HashMap.Strict                   (HashMap)
import Data.IntMap.Strict                    (IntMap)
import Data.Monoid                           (Any)
import Data.Text                             (Text)
import GHC.Generics

import Clash.Core.PartialEval as PE          (Evaluator)
import Clash.Core.Evaluator.Types as WHNF    (Evaluator, PrimHeap)

import Clash.Core.Term           (Term, Context)
import Clash.Core.Type           (Type)
import Clash.Core.TyCon          (TyConName, TyConMap)
import Clash.Core.Var            (Id)
import Clash.Core.VarEnv         (InScopeSet, VarSet, VarEnv)
import Clash.Driver.Types        (BindingMap, DebugOpts)
import Clash.Netlist.Types       (FilteredHWType, HWMap)
import Clash.Rewrite.WorkFree    (isWorkFree)
import Clash.Util

import Clash.Annotations.BitRepresentation.Internal (CustomReprs)

-- | State used by the inspection mechanism for recording rewrite steps.
data RewriteStep
  = RewriteStep
  { t_ctx    :: Context
  -- ^ current context
  , t_name   :: String
  -- ^ Name of the transformation
  , t_bndrS  :: String
  -- ^ Name of the current binder
  , t_before :: Term
  -- ^ Term before `apply`
  , t_after  :: Term
  -- ^ Term after `apply`
  } deriving (Show, Generic, NFData, Binary)

-- | State of a rewriting session
data RewriteState extra
  = RewriteState
    -- TODO Given we now keep transformCounters, this should just be 'fold'
    -- over that map, otherwise the two counts could fall out of sync.
  { _transformCounter :: {-# UNPACK #-} !Word
  -- ^ Total number of applied transformations
  , _transformCounters :: HashMap Text Word
  -- ^ Map that tracks how many times each transformation is applied
  , _bindings         :: !BindingMap
  -- ^ Global binders
  , _uniqSupply       :: !Supply
  -- ^ Supply of unique numbers
  , _curFun           :: (Id,SrcSpan) -- Initially set to undefined: no strictness annotation
  -- ^ Function which is currently normalized
  , _nameCounter      :: {-# UNPACK #-} !Int
  -- ^ Used for 'Fresh'
  , _globalHeap       :: PrimHeap
  -- ^ Used as a heap for compile-time evaluation of primitives that live in I/O
  , _workFreeBinders  :: VarEnv Bool
  -- ^ Map telling whether a binder's definition is work-free
  , _extra            :: !extra
  -- ^ Additional state
  }

Lens.makeLenses ''RewriteState

-- | Read-only environment of a rewriting session
data RewriteEnv
  = RewriteEnv
  { _debugOpts      :: DebugOpts
  -- ^ Options for debugging during rewriting
  , _aggressiveXOpt :: Bool
  -- ^ Transformations to print debugging info for
  , _typeTranslator :: CustomReprs
                    -> TyConMap
                    -> Type
                    -> State HWMap (Maybe (Either String FilteredHWType))
  -- ^ Hardcode Type -> FilteredHWType translator
  , _tcCache        :: TyConMap
  -- ^ TyCon cache
  , _tupleTcCache   :: IntMap TyConName
  -- ^ Tuple TyCon cache
  , _peEvaluator    :: PE.Evaluator
  -- ^ Hardcoded evaluator for partial evaluation
  , _evaluator      :: WHNF.Evaluator
  -- ^ Hardcoded evaluator for WHNF (old evaluator)
  , _topEntities    :: VarSet
  -- ^ Functions that are considered TopEntities
  , _customReprs    :: CustomReprs
  -- ^ Custom bit representations
  , _fuelLimit      :: Word
  -- ^ Maximum amount of fuel for the evaluator
  }

Lens.makeLenses ''RewriteEnv

-- | Monad that keeps track how many transformations have been applied and can
-- generate fresh variables and unique identifiers. In addition, it keeps track
-- if a transformation/rewrite has been successfully applied.
newtype RewriteMonad extra a = R
  { unR :: RewriteEnv -> RewriteState extra -> Any -> (a,RewriteState extra,Any) }

-- | Run the computation in the RewriteMonad
runR
  :: RewriteMonad extra a
  -> RewriteEnv
  -> RewriteState extra
  -> (a, RewriteState extra, Any)
runR m r s = unR m r s mempty

instance MonadFail (RewriteMonad extra) where
  fail err = error ("RewriteMonad.fail: " ++ err)

instance Functor (RewriteMonad extra) where
  fmap f m = R $ \ r s w -> case unR m r s w of (a, s', w') -> (f a, s', w')
  {-# INLINE fmap #-}

instance Applicative (RewriteMonad extra) where
  pure a = R $ \ _ s w -> (a, s, w)
  {-# INLINE pure #-}
  R mf <*> R mx = R $ \ r s w -> case mf r s w of
    (f,s',w') -> case mx r s' w' of
      (x,s'',w'') -> (f x, s'', w'')
  {-# INLINE (<*>) #-}

instance Monad (RewriteMonad extra) where
  return a = R $ \ _ s w -> (a, s, w)
  {-# INLINE return #-}
  m >>= k  =
    R $ \ r s w -> case unR m r s w of
      (a,s',w') -> unR (k a) r s' w'
  {-# INLINE (>>=) #-}


instance MonadState (RewriteState extra) (RewriteMonad extra) where
  get = R $ \_ s w -> (s,s,w)
  {-# INLINE get #-}
  put s = R $ \_ _ w -> ((),s,w)
  {-# INLINE put #-}
  state f = R $ \_ s w -> case f s of (a,s') -> (a,s',w)
  {-# INLINE state #-}

instance MonadUnique (RewriteMonad extra) where
  getUniqueM = do
    sup <- use uniqSupply
    let (a,sup') = freshId sup
    uniqSupply .= sup'
    a `seq` return a

instance MonadWriter Any (RewriteMonad extra) where
  writer (a,w') = R $ \_ s w -> let wt = w `mappend` w' in wt `seq` (a,s,wt)
  {-# INLINE writer #-}
  tell w' = R $ \_ s w -> let wt = w `mappend` w' in wt `seq` ((),s,wt)
  {-# INLINE tell #-}
  listen m = R $ \r s w -> case runR m r s of
    (a,s',w') -> let wt = w `mappend` w' in wt `seq` ((a,w'),s',wt)
  {-# INLINE listen #-}
  pass m = R $ \r s w -> case runR m r s of
    ((a,f),s',w') -> let wt = w `mappend` f w' in wt `seq` (a, s', wt)
  {-# INLINE pass #-}

censor :: (Any -> Any) -> RewriteMonad extra a -> RewriteMonad extra a
censor f m = R $ \r s w -> case runR m r s of
  (a,s',w') -> let wt = w `mappend` f w' in wt `seq` (a, s', wt)
{-# INLINE censor #-}

instance MonadReader RewriteEnv (RewriteMonad extra) where
   ask = R $ \r s w -> (r,s,w)
   {-# INLINE ask #-}
   local f m = R $ \r s w -> unR m (f r) s w
   {-# INLINE local #-}
   reader f = R $ \r s w -> (f r,s,w)
   {-# INLINE reader #-}

instance MonadFix (RewriteMonad extra) where
  mfix f = R $ \r s w -> fix $ \ ~(a,_,_) -> unR (f a) r s w
  {-# INLINE mfix #-}

data TransformContext
  = TransformContext
  { tfInScope :: !InScopeSet
  , tfContext :: Context
  }

-- | Monadic action that transforms a term given a certain context
type Transform m = TransformContext -> Term -> m Term

-- | A 'Transform' action in the context of the 'RewriteMonad'
type Rewrite extra = Transform (RewriteMonad extra)

-- Moved into Clash.Rewrite.WorkFree
{-# SPECIALIZE isWorkFree
      :: Lens' (RewriteState extra) (VarEnv Bool)
      -> BindingMap
      -> Term
      -> RewriteMonad extra Bool
  #-}
