{-# language PolyKinds #-}
{-# language TypeFamilies #-}
{-# language TypeOperators #-}
{-# language KindSignatures #-}
{-# language ConstraintKinds #-}
{-# language DefaultSignatures #-}
{-# language FlexibleInstances #-}
{-# language ScopedTypeVariables #-}
{-# language UndecidableInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}

module Control.Lens.Constraints where

import Control.Category
import Control.Exception
import Control.Exception.Lens (exception)
import Control.Lens
import Data.Constraint
import Data.Constraint.Unsafe(unsafeCoerceConstraint)
import Data.Functor.Apply
import Data.Proxy
import Prelude hiding ((.),id)

class (TraversalConstraint p f, Contravariant f) => FoldConstraint p f
class (Traversal1Constraint p f, Contravariant f) => Fold1Constraint p f
class (LensConstraint p f, Contravariant f) => GetterConstraint p f
class (p ~ (->), Settable f) => SetterConstraint p f
class (Traversal1Constraint p f, Applicative f) => TraversalConstraint p f
class (LensConstraint p f, Apply f) => Traversal1Constraint p f
class (p ~ (->), Functor f) => LensConstraint p f
class (Choice p, Applicative f) => PrismConstraint p f
class (Choice p, Bifunctor p, Settable f) => ReviewConstraint p f
class (Profunctor p, Functor f) => IsoConstraint p f
class EqualityConstraint (p :: * -> * -> *) (f :: * -> *)
class (Indexable i p, IndexedTraversalConstraint i p f, Contravariant f) => IndexedFoldConstraint i p f
class (Indexable i p, IndexedTraversal1Constraint i p f, Contravariant f) => IndexedFold1Constraint i p f
class (Indexable i p, IndexedLensConstraint i p f, Contravariant f) => IndexedGetterConstraint i p f
class (Indexable i p, p ~ (->), Settable f) => IndexedSetterConstraint i p f 
class (Indexable i p, IndexedTraversal1Constraint i p f, Applicative f) => IndexedTraversalConstraint i p f
class (Indexable i p, IndexedLensConstraint i p f, Apply f) => IndexedTraversal1Constraint i p f 
class (Indexable i p, p ~ (->), Functor f) => IndexedLensConstraint i p f

instance (TraversalConstraint p f, Contravariant f) => FoldConstraint p f
instance (Traversal1Constraint p f, Contravariant f) => Fold1Constraint p f
instance (LensConstraint p f, Contravariant f) => GetterConstraint p f
instance (p ~ (->), Settable f) => SetterConstraint p f
instance (Traversal1Constraint p f, Applicative f) => TraversalConstraint p f
instance (LensConstraint p f, Apply f) => Traversal1Constraint p f
instance (p ~ (->), Functor f) => LensConstraint p f
instance (Choice p, Applicative f) => PrismConstraint p f
instance (Choice p, Bifunctor p, Settable f) => ReviewConstraint p f
instance (Profunctor p, Functor f) => IsoConstraint p f
instance EqualityConstraint p f 
instance (Indexable i p, IndexedTraversalConstraint i p f, Contravariant f) => IndexedFoldConstraint i p f
instance (Indexable i p, IndexedTraversal1Constraint i p f, Contravariant f) => IndexedFold1Constraint i p f
instance (Indexable i p, IndexedLensConstraint i p f, Contravariant f) => IndexedGetterConstraint i p f
instance (Indexable i p, p ~ (->), Settable f) => IndexedSetterConstraint i p f 
instance (Indexable i p, IndexedTraversal1Constraint i p f, Applicative f) => IndexedTraversalConstraint i p f
instance (Indexable i p, IndexedLensConstraint i p f, Apply f) => IndexedTraversal1Constraint i p f 
instance (Indexable i p, p ~ (->), Functor f) => IndexedLensConstraint i p f

class (s :: (* -> * -> *) -> (* -> *) -> Constraint) :< (t :: (* -> * -> *) -> (* -> *) -> Constraint) where
  impl :: t p f :- s p f

instance FoldConstraint :< FoldConstraint where impl = id
instance Fold1Constraint :< FoldConstraint where 
  impl = Sub go where
    go :: forall p f. FoldConstraint p f => Dict (Fold1Constraint p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance Fold1Constraint :< Fold1Constraint where impl = id
instance GetterConstraint :< FoldConstraint where impl = Sub Dict
instance GetterConstraint :< Fold1Constraint where impl = Sub Dict
instance GetterConstraint :< GetterConstraint where impl = id
instance SetterConstraint :< SetterConstraint where impl = id
instance TraversalConstraint :< FoldConstraint where impl = Sub Dict
instance TraversalConstraint :< SetterConstraint where
  impl = Sub go where
    go :: forall p f. SetterConstraint p f => Dict (TraversalConstraint p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance TraversalConstraint :< TraversalConstraint where impl = Sub Dict
instance Traversal1Constraint :< FoldConstraint where
  impl = Sub go where
    go :: forall p f. FoldConstraint p f => Dict (Traversal1Constraint p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance Traversal1Constraint :< Fold1Constraint where impl = Sub Dict
instance Traversal1Constraint :< SetterConstraint where
  impl = Sub go where
    go :: forall p f. SetterConstraint p f => Dict (Traversal1Constraint p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance Traversal1Constraint :< TraversalConstraint where
  impl = Sub go where
    go :: forall p f. TraversalConstraint p f => Dict (Traversal1Constraint p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict  
instance Traversal1Constraint :< Traversal1Constraint where impl = Sub Dict
instance LensConstraint :< FoldConstraint where impl = Sub Dict
instance LensConstraint :< Fold1Constraint where impl = Sub Dict
instance LensConstraint :< GetterConstraint where impl = Sub Dict
instance LensConstraint :< SetterConstraint where impl = Sub Dict
instance LensConstraint :< TraversalConstraint where impl = Sub Dict
instance LensConstraint :< Traversal1Constraint where impl = Sub Dict
instance LensConstraint :< LensConstraint where impl = id
instance PrismConstraint :< FoldConstraint where impl = Sub Dict
instance PrismConstraint :< SetterConstraint where impl = Sub Dict
instance PrismConstraint :< TraversalConstraint where impl = Sub Dict 
instance PrismConstraint :< PrismConstraint where impl = id
instance PrismConstraint :< ReviewConstraint where impl = Sub Dict
instance ReviewConstraint :< ReviewConstraint where impl = id
instance IsoConstraint :< FoldConstraint where impl = Sub Dict
instance IsoConstraint :< Fold1Constraint where impl = Sub Dict
instance IsoConstraint :< GetterConstraint where impl = Sub Dict
instance IsoConstraint :< SetterConstraint where impl = Sub Dict
instance IsoConstraint :< TraversalConstraint where impl = Sub Dict
instance IsoConstraint :< Traversal1Constraint where impl = Sub Dict
instance IsoConstraint :< LensConstraint where impl = Sub Dict
instance IsoConstraint :< PrismConstraint where impl = Sub Dict
instance IsoConstraint :< ReviewConstraint where impl = Sub Dict
instance IsoConstraint :< IsoConstraint where impl = id
instance EqualityConstraint :< FoldConstraint where impl = Sub Dict
instance EqualityConstraint :< Fold1Constraint where impl = Sub Dict
instance EqualityConstraint :< GetterConstraint where impl = Sub Dict
instance EqualityConstraint :< SetterConstraint where impl = Sub Dict
instance EqualityConstraint :< TraversalConstraint where impl = Sub Dict
instance EqualityConstraint :< Traversal1Constraint where impl = Sub Dict
instance EqualityConstraint :< LensConstraint where impl = Sub Dict
instance EqualityConstraint :< PrismConstraint where impl = Sub Dict
instance EqualityConstraint :< ReviewConstraint where impl = Sub Dict
instance EqualityConstraint :< IsoConstraint where impl = Sub Dict
instance EqualityConstraint :< EqualityConstraint where impl = id

class (s :: k -> (* -> * -> *) -> (* -> *) -> Constraint) *:< (t :: k -> (* -> * -> *) -> (* -> *) -> Constraint) where
  impli :: t i p f :- s i p f

instance IndexedFoldConstraint *:< IndexedFoldConstraint where impli = id
instance IndexedFold1Constraint *:< IndexedFoldConstraint where
  impli = Sub go where
    go :: forall i p f. IndexedFoldConstraint i p f => Dict (IndexedFold1Constraint i p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance IndexedFold1Constraint *:< IndexedFold1Constraint where impli = Sub Dict
instance IndexedGetterConstraint *:< IndexedFoldConstraint where impli = Sub Dict
instance IndexedGetterConstraint *:< IndexedFold1Constraint where impli = Sub Dict
instance IndexedGetterConstraint *:< IndexedGetterConstraint where impli = id
instance IndexedSetterConstraint *:< IndexedSetterConstraint where impli = id
instance IndexedTraversalConstraint *:< IndexedFoldConstraint where impli = Sub Dict
instance IndexedTraversalConstraint *:< IndexedSetterConstraint where
  impli = Sub go where
    go :: forall i p f. IndexedSetterConstraint i p f => Dict (IndexedTraversalConstraint i p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance IndexedTraversalConstraint *:< IndexedTraversalConstraint where impli = Sub Dict
instance IndexedTraversal1Constraint *:< IndexedFoldConstraint where
  impli = Sub go where
    go :: forall i p f. IndexedFoldConstraint i p f => Dict (IndexedTraversal1Constraint i p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance IndexedTraversal1Constraint *:< IndexedFold1Constraint where impli = Sub Dict
instance IndexedTraversal1Constraint *:< IndexedSetterConstraint where
  impli = Sub go where
    go :: forall i p f. IndexedSetterConstraint i p f => Dict (IndexedTraversal1Constraint i p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict
instance IndexedTraversal1Constraint *:< IndexedTraversalConstraint where
  impli = Sub go where
    go :: forall i p f. IndexedTraversalConstraint i p f => Dict (IndexedTraversal1Constraint i p f)
    go = case unsafeApply (Proxy :: Proxy f) of Sub Dict -> Dict  
instance IndexedTraversal1Constraint *:< IndexedTraversal1Constraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedFoldConstraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedFold1Constraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedGetterConstraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedSetterConstraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedTraversalConstraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedTraversal1Constraint where impli = Sub Dict
instance IndexedLensConstraint *:< IndexedLensConstraint where impli = id

class (s :< u, t :< u) => Join s t u | s t -> u where

instance Join FoldConstraint FoldConstraint FoldConstraint
instance Join FoldConstraint Fold1Constraint FoldConstraint
instance Join FoldConstraint GetterConstraint FoldConstraint
instance Join FoldConstraint TraversalConstraint FoldConstraint
instance Join FoldConstraint Traversal1Constraint FoldConstraint
instance Join FoldConstraint LensConstraint FoldConstraint
instance Join FoldConstraint PrismConstraint FoldConstraint
instance Join FoldConstraint IsoConstraint FoldConstraint
instance Join FoldConstraint EqualityConstraint FoldConstraint
instance Join Fold1Constraint FoldConstraint FoldConstraint
instance Join Fold1Constraint Fold1Constraint Fold1Constraint
instance Join Fold1Constraint GetterConstraint Fold1Constraint
instance Join Fold1Constraint TraversalConstraint FoldConstraint
instance Join Fold1Constraint Traversal1Constraint Fold1Constraint
instance Join Fold1Constraint LensConstraint Fold1Constraint
instance Join Fold1Constraint PrismConstraint FoldConstraint
instance Join Fold1Constraint IsoConstraint Fold1Constraint
instance Join Fold1Constraint EqualityConstraint Fold1Constraint
instance Join GetterConstraint FoldConstraint FoldConstraint
instance Join GetterConstraint Fold1Constraint Fold1Constraint
instance Join GetterConstraint GetterConstraint GetterConstraint
instance Join GetterConstraint TraversalConstraint FoldConstraint
instance Join GetterConstraint Traversal1Constraint Fold1Constraint
instance Join GetterConstraint PrismConstraint FoldConstraint
instance Join GetterConstraint IsoConstraint GetterConstraint
instance Join GetterConstraint EqualityConstraint GetterConstraint
instance Join SetterConstraint SetterConstraint SetterConstraint
instance Join SetterConstraint TraversalConstraint SetterConstraint
instance Join SetterConstraint LensConstraint SetterConstraint
instance Join SetterConstraint PrismConstraint SetterConstraint
instance Join SetterConstraint IsoConstraint SetterConstraint
instance Join SetterConstraint EqualityConstraint SetterConstraint
instance Join TraversalConstraint FoldConstraint FoldConstraint
instance Join TraversalConstraint Fold1Constraint FoldConstraint
instance Join TraversalConstraint GetterConstraint FoldConstraint
instance Join TraversalConstraint SetterConstraint SetterConstraint
instance Join TraversalConstraint TraversalConstraint TraversalConstraint
instance Join TraversalConstraint Traversal1Constraint TraversalConstraint
instance Join TraversalConstraint LensConstraint TraversalConstraint
instance Join TraversalConstraint PrismConstraint TraversalConstraint
instance Join TraversalConstraint IsoConstraint TraversalConstraint
instance Join TraversalConstraint EqualityConstraint TraversalConstraint
instance Join Traversal1Constraint FoldConstraint FoldConstraint
instance Join Traversal1Constraint Fold1Constraint Fold1Constraint
instance Join Traversal1Constraint GetterConstraint Fold1Constraint
instance Join Traversal1Constraint TraversalConstraint TraversalConstraint
instance Join Traversal1Constraint Traversal1Constraint Traversal1Constraint
instance Join Traversal1Constraint LensConstraint Traversal1Constraint
instance Join Traversal1Constraint PrismConstraint TraversalConstraint
instance Join Traversal1Constraint IsoConstraint Traversal1Constraint
instance Join Traversal1Constraint EqualityConstraint Traversal1Constraint
instance Join LensConstraint FoldConstraint FoldConstraint
instance Join LensConstraint Fold1Constraint Fold1Constraint
instance Join LensConstraint GetterConstraint GetterConstraint
instance Join LensConstraint SetterConstraint SetterConstraint
instance Join LensConstraint TraversalConstraint TraversalConstraint
instance Join LensConstraint Traversal1Constraint Traversal1Constraint
instance Join LensConstraint LensConstraint LensConstraint
instance Join LensConstraint PrismConstraint TraversalConstraint
instance Join LensConstraint IsoConstraint TraversalConstraint
instance Join LensConstraint EqualityConstraint TraversalConstraint
instance Join PrismConstraint FoldConstraint FoldConstraint
instance Join PrismConstraint Fold1Constraint FoldConstraint
instance Join PrismConstraint GetterConstraint FoldConstraint
instance Join PrismConstraint SetterConstraint SetterConstraint
instance Join PrismConstraint TraversalConstraint TraversalConstraint
instance Join PrismConstraint Traversal1Constraint TraversalConstraint
instance Join PrismConstraint LensConstraint TraversalConstraint
instance Join PrismConstraint PrismConstraint PrismConstraint
instance Join PrismConstraint ReviewConstraint ReviewConstraint
instance Join PrismConstraint IsoConstraint PrismConstraint
instance Join PrismConstraint EqualityConstraint PrismConstraint
instance Join ReviewConstraint ReviewConstraint ReviewConstraint
instance Join ReviewConstraint PrismConstraint ReviewConstraint
instance Join ReviewConstraint IsoConstraint ReviewConstraint
instance Join ReviewConstraint EqualityConstraint ReviewConstraint
instance Join IsoConstraint FoldConstraint FoldConstraint
instance Join IsoConstraint Fold1Constraint Fold1Constraint
instance Join IsoConstraint GetterConstraint GetterConstraint
instance Join IsoConstraint SetterConstraint SetterConstraint
instance Join IsoConstraint TraversalConstraint TraversalConstraint
instance Join IsoConstraint Traversal1Constraint Traversal1Constraint
instance Join IsoConstraint LensConstraint LensConstraint
instance Join IsoConstraint PrismConstraint PrismConstraint
instance Join IsoConstraint ReviewConstraint ReviewConstraint
instance Join IsoConstraint IsoConstraint IsoConstraint
instance Join EqualityConstraint FoldConstraint FoldConstraint
instance Join EqualityConstraint Fold1Constraint Fold1Constraint
instance Join EqualityConstraint GetterConstraint GetterConstraint
instance Join EqualityConstraint SetterConstraint SetterConstraint
instance Join EqualityConstraint TraversalConstraint TraversalConstraint
instance Join EqualityConstraint Traversal1Constraint Traversal1Constraint
instance Join EqualityConstraint LensConstraint LensConstraint
instance Join EqualityConstraint PrismConstraint PrismConstraint
instance Join EqualityConstraint ReviewConstraint ReviewConstraint
instance Join EqualityConstraint IsoConstraint IsoConstraint
instance Join EqualityConstraint EqualityConstraint EqualityConstraint

class (s *:< u, t *:< u) => Joini s t u | s t -> u where

instance Joini IndexedFoldConstraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedFoldConstraint IndexedFold1Constraint IndexedFoldConstraint
instance Joini IndexedFoldConstraint IndexedGetterConstraint IndexedFoldConstraint
instance Joini IndexedFoldConstraint IndexedTraversalConstraint IndexedFoldConstraint
instance Joini IndexedFoldConstraint IndexedTraversal1Constraint IndexedFoldConstraint
instance Joini IndexedFoldConstraint IndexedLensConstraint IndexedFoldConstraint
instance Joini IndexedFold1Constraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedFold1Constraint IndexedFold1Constraint IndexedFold1Constraint
instance Joini IndexedFold1Constraint IndexedGetterConstraint IndexedFold1Constraint
instance Joini IndexedFold1Constraint IndexedTraversalConstraint IndexedFoldConstraint
instance Joini IndexedFold1Constraint IndexedTraversal1Constraint IndexedFold1Constraint
instance Joini IndexedFold1Constraint IndexedLensConstraint IndexedFold1Constraint
instance Joini IndexedGetterConstraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedGetterConstraint IndexedFold1Constraint IndexedFold1Constraint
instance Joini IndexedGetterConstraint IndexedGetterConstraint IndexedGetterConstraint
instance Joini IndexedGetterConstraint IndexedTraversalConstraint IndexedFoldConstraint
instance Joini IndexedGetterConstraint IndexedTraversal1Constraint IndexedFold1Constraint
instance Joini IndexedSetterConstraint IndexedSetterConstraint IndexedSetterConstraint
instance Joini IndexedSetterConstraint IndexedTraversalConstraint IndexedSetterConstraint
instance Joini IndexedSetterConstraint IndexedLensConstraint IndexedSetterConstraint
instance Joini IndexedTraversalConstraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedTraversalConstraint IndexedFold1Constraint IndexedFoldConstraint
instance Joini IndexedTraversalConstraint IndexedGetterConstraint IndexedFoldConstraint
instance Joini IndexedTraversalConstraint IndexedSetterConstraint IndexedSetterConstraint
instance Joini IndexedTraversalConstraint IndexedTraversalConstraint IndexedTraversalConstraint
instance Joini IndexedTraversalConstraint IndexedTraversal1Constraint IndexedTraversalConstraint
instance Joini IndexedTraversalConstraint IndexedLensConstraint IndexedTraversalConstraint
instance Joini IndexedTraversal1Constraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedTraversal1Constraint IndexedFold1Constraint IndexedFold1Constraint
instance Joini IndexedTraversal1Constraint IndexedGetterConstraint IndexedFold1Constraint
instance Joini IndexedTraversal1Constraint IndexedTraversalConstraint IndexedTraversalConstraint
instance Joini IndexedTraversal1Constraint IndexedTraversal1Constraint IndexedTraversal1Constraint
instance Joini IndexedTraversal1Constraint IndexedLensConstraint IndexedTraversal1Constraint
instance Joini IndexedLensConstraint IndexedFoldConstraint IndexedFoldConstraint
instance Joini IndexedLensConstraint IndexedFold1Constraint IndexedFold1Constraint
instance Joini IndexedLensConstraint IndexedGetterConstraint IndexedGetterConstraint
instance Joini IndexedLensConstraint IndexedSetterConstraint IndexedSetterConstraint
instance Joini IndexedLensConstraint IndexedTraversalConstraint IndexedTraversalConstraint
instance Joini IndexedLensConstraint IndexedTraversal1Constraint IndexedTraversal1Constraint
instance Joini IndexedLensConstraint IndexedLensConstraint IndexedLensConstraint

unsafeApply :: forall proxy f. proxy f -> Applicative f :- Apply f
unsafeApply _ = trans (unsafeCoerceConstraint :: Apply (WrappedApplicative m) :- Apply m) (Sub Dict :: Applicative f :- Apply (WrappedApplicative f))

class AsArithException t where
  type AsArithExceptionConstraint t :: (* -> * -> *) -> (* -> *) -> Constraint
  type AsArithExceptionConstraint t = PrismConstraint

  _AsArithException' :: AsArithExceptionConstraint t p f => Optic' p f t ArithException


  _AsArithExceptionIsPrism :: proxy t -> Dict (AsArithExceptionConstraint t :< PrismConstraint)
  default _AsArithExceptionIsPrism :: (AsArithExceptionConstraint t ~ PrismConstraint) => proxy t -> Dict (AsArithExceptionConstraint t :< PrismConstraint)
  _AsArithExceptionIsPrism _ = Dict
  

_AsArithException :: forall t p f. (AsArithException t, Choice p, Applicative f) => Optic' p f t ArithException
_AsArithException = case _AsArithExceptionIsPrism (Proxy :: Proxy t) of
  Dict -> case impl :: PrismConstraint p f :- AsArithExceptionConstraint t p f of
    Sub Dict -> _AsArithException' 
   
instance AsArithException ArithException where
  type AsArithExceptionConstraint ArithException = EqualityConstraint
  _AsArithExceptionIsPrism _ = Dict
  _AsArithException' = id

instance AsArithException SomeException where
  _AsArithException' = exception

classdd Conz s t a b | s -> a, t -> b, s b -> t, t a -> s where
  _Conz :: Prism s t (a, s) (b, t)
