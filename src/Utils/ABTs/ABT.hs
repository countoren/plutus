{-# OPTIONS -Wall #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveGeneric #-}





-- | This module defines a kind of abstract binding tree (ABT). It uses a type
-- similar to the 'Fix' type, but with an added constructor for variables, and
-- an intermediate type 'Scope' for representing (possibly empty) binders.
-- For uniformity, every argument to a construct is a 'Scope', even args that
-- normally aren't seen as scopes. For example, whereas normally you might
-- expect that a pair has the form @pair(M;N)@ (using the PFPL notation), with
-- these ABTs, it has the form @pair([].M;[].N)@ where the pair elements are
-- binders with an empty list of bound variables. This avoids the need to make
-- two different types available to the class of constructions, one for terms
-- and one for scopes.

module Utils.ABTs.ABT where

import Utils.ABTs.Vars

import Data.List (elemIndex)
import GHC.Generics







-- * Main types



-- | 'ABT' is like 'Fix', but with the addition of tools for binders. Like
-- 'Fix', we have a parameter 'f' for functors that define the shape of the
-- constructors for the type of interest. In this sense, then, the subset
-- of elements that have no variables and non-binding 'Scope's is just a
-- kind of least fixed point, in the F-algebra sense. The addition of
-- variables and scopes simply introduces new constructions in the right
-- places to represent binding. It's similar to the free monad construction,
-- but the variable parameter is fixed to be 'Variable'.
--
-- The particular choices for 'f' can be simple polynomial functors, such as
-- 
-- > data LC a = Pair a a | Fst a | Snd a | Lam a | App a a
--
-- as for the a simple lambda calculus with pairs and functions, or it can be
-- more complex, such as
--
-- > data LC a = ... | Con String [a] | Case a [(Pattern, a)]
--
-- which has constructed data (eg @Con "True" []@ for @True@), as well as
-- case expressions with a list of clauses, represented by pairs of patterns
-- and associated clause bodies.
--
-- The choice to represent ABTs as functors was to make this kind of
-- representation possible, without simultaneously forcing every kind of
-- construct (lists, clauses, etc.) into the ABT type.

data ABT sig = Var Variable
             | sig :$: [Scope sig]
  deriving (Show,Generic)





-- | A @Scope f@ is a list of bound variable names used for both display
-- purposes and to track how many bound variables there are, along with a
-- @ABT f@ for the body of the scope. a value @Scope ["x","y"] m@ corresponds
-- to a PFPL scope of the form @x,y.m@

data Scope sig
  = Scope
      { names :: [String]
      , freeNames :: [FreeVar]
      , body :: ABT sig
      }
  deriving (Show,Generic)







-- * Free variables in an ABT



-- | A term has some specified free variables. We can compute these whenever
-- the constructions form a 'Foldable' instance.

freeVars :: ABT sig -> [FreeVar]
freeVars (Var (Free n)) = [n]
freeVars (Var _) = []
freeVars (_ :$: xs) = xs >>= freeVarsScope

freeVarsScope :: Scope sig -> [FreeVar]
freeVarsScope (Scope _ _ x) = freeVars x



-- | 'freeVarNames' just gives back the free names for the free vars.

freeVarNames :: ABT sig -> [String]
freeVarNames = map (\(FreeVar n) -> n) . freeVars







-- * Shifting bound variables



-- | The 'shift' function appropriately increments a term's free variables,
-- to handle substitution under binders. Any bound variable inside a term
-- that gets substituted under a binder needs to still point to its own
-- binder higher up, or else it'll be captured. The @l@ argument of 'shift'
-- represents how many bound variables in the substituted term the 'shift' has
-- passed under, while the @i@ represents how many new bound variables there
-- are in the scope that's being substituted into. We use @l-1@ in the
-- condition because if there are @l@ many bound vars, than the index for the
-- binders are the numbers in the range @[0..l-1]@, so any bound var above that
-- range points to a higher binder.
--
-- For example, consider the function term @λx. (λy.λz.y) x@. This can
-- be normalized to @λx.λz.x@ by beta reducing the inner application.
-- To do this, we need to substitute @x@ for @y@ in @λz.y@. But @x@ is a
-- bound variable, bound by the outer lambda, so we need to avoid capture, by
-- shifting it appropriately. With de Bruijn indices, want to turn the term
-- @λ.(λ.λ.1)0@ into the term @λ.λ.1@. The index for @x@ has to shift from @0@
-- to @1@ because it's being used under the binder for @z@. This is what the
-- @i@ argument to 'shift' represents.
--
-- Similarly, if we had also put a binder around @x@, as in the term
-- @λx. (λy.λz.y) (λw.x)@ we need to track that as well. This should normalize
-- to @λx.λz.λw.x@. With de Bruijn indices, @λ. (λ.λ.1) (λ.1)@ should become
-- the term @λ.λ.λ.2@. The variable @x@ initially was coded as @1@, but shifts
-- to @2@, as expected. However, if we had normalized @λx. (λy.λz.y) (λw.w)@
-- which with de Bruijn indexes is @λ. (λ.λ.1) (λ.0)@, we expect to get back
-- @λx.λz.λw.w@ which with de Bruin indexes is @λ.λ.λ.0@. Notice that although
-- the variable @w@ corresponds to the index @0@, the 'shift' function must
-- leave it unchanged. So not all bound variables are shifted, only those that
-- were bound outside of any new binders that 'shift' passes under. This is
-- what the variable @l@ represents in 'shift'.

shift :: Int -> Int -> ABT sig -> ABT sig
shift l i (Var (Bound n (BoundVar v))) | v > l-1 =
  Var (Bound n (BoundVar (v+i)))
shift _ _ (Var v) = Var v
shift l i (c :$: xs) = c :$: map (shiftScope l i) xs



-- | When shifting a scope, we keep track of the number of new variables that
-- are brought into scope, so the number of variables bound by the scope is
-- added to the current value of @l@ in the recursive call.

shiftScope :: Int -> Int -> Scope sig -> Scope sig
shiftScope l i (Scope ns fns x) = Scope ns fns (shift (l+length ns) i x)







-- * Unshifting bound variables



-- | Just as we need to shift, sometimes we also need to unshift. In this
-- case, for evaluation under binders so that a variable bound outside its
-- nearest binder still points appropriately. For example, consider
-- @λx. (λy.x) c@, which corresponds to the de Bruijn term @λ. (λ.1) c@. If we
-- evaluate under the binder, we expect to get @λx.x@ because @[c/y]x = x@,
-- which is the de Bruijn term @λ.0@. The index @1@ in the original had to be
-- unshifted down to @0@ because its enclosing binder was removed.
--
-- In the function 'unshift', the argument @l@ indicates the number of bound
-- variables that were in scope before the relevant outer scope was removed.
-- This includes the original variables bound by the now-removed scope, and
-- any variables bound by binders inside that scope that have been passed
-- under by 'unshift'. A variable in the relevant range will be left
-- un-altered, because either it's been instantiated out of existence or its
-- binder is still present. A variable outside the range will be reduced by
-- @i@, which represents the number of variables bound by the now-removed
-- binder.

unshift :: Int -> Int -> ABT sig -> ABT sig
unshift l i (Var (Bound n (BoundVar v))) | v > l-1 =
  Var (Bound n (BoundVar (v-i)))
unshift _ _ (Var v) = Var v
unshift l i (c :$: xs) = c :$: map (unshiftScope l i) xs



-- | When unshifting a scope, we keep track of the number of new variables
-- that are brought into scope, so the number of variables bound by the scope
-- is added to the current value of @l@ in the recursive call.

unshiftScope :: Int -> Int -> Scope sig -> Scope sig
unshiftScope l i (Scope ns fns x) = Scope ns fns (unshift (l+length ns) i x)







-- * Binding a free variable



-- | We bind variables by replacing them with an appropriately named 'Bound'.
-- The argument @l@ tracks how many binders we've recursed under.

bind :: Int -> [FreeVar] -> ABT sig -> ABT sig
bind _ [] x = x
bind l ns (Var v@(Free n)) =
  case elemIndex n ns of
    Nothing -> Var v
    Just i -> Var (Bound (name v) (BoundVar (l + i)))
bind _ _ (Var v) = Var v
bind l ns (c :$: xs) = c :$: map (bindScope l ns) xs



-- | We also can bind scopes. As before, @l@ tracks new variables.

bindScope :: Int -> [FreeVar] -> Scope sig -> Scope sig
bindScope _ [] sc = sc
bindScope l ns (Scope ns' _ b) =
  Scope ns' fv b'
  where
    b' = bind (l + length ns') ns b
    fv = freeVars b'







-- * Unbinding a bound variable



-- | We unbind by doing the opposite of binding, replacing bound variables
-- with free variables.

unbind :: Int -> [FreeVar] -> ABT sig -> ABT sig
unbind _ [] x = x
unbind l ns (Var (Bound n (BoundVar i))) =
  if i < l  -- i is a locally bound variable
  then Var (Bound n (BoundVar i))
  else if i >= l + length ns  -- i is bound outside the variables to replace
  then Var (Bound n (BoundVar i))
  else Var (Free (ns !! (i - l)))
unbind _ _ (Var v) = Var v
unbind l ns (c :$: xs) = c :$: map (unbindScope l ns) xs



-- | We also can unbind scopes.

unbindScope :: Int -> [FreeVar] -> Scope sig -> Scope sig
unbindScope l ns (Scope ns' _ b) =
  Scope ns' fv b'
  where
    b' = unbind (l + length ns') ns b
    fv = freeVars b'







-- * Smart constructors



-- | A smart constructor that creates a @Scope f@ while also performing actual
-- binding of free variables. This also calculates the remaining free
-- variables in the body of the scope and stores them.

scope :: [String] -> ABT sig -> Scope sig
scope ns b = Scope { names = ns
                   , freeNames = freeVars b'
                   , body = b'
                   }
  where b' = bind 0 (map FreeVar ns) b


-- | Descoping a scope involves reversing the actions of 'scope' that made it.
-- However, because a scope's names might conflict with the free variables in
-- the scope body, we need to rename them first, before unbinding. 'descope'
-- also returns the fresh names.

descope :: Scope sig -> ([String], ABT sig)
descope (Scope ns fns b) = (freshNs, unbind 0 (map FreeVar freshNs) b)
  where freshNs = freshen [ n | FreeVar n <- fns ] ns


-- | Opening a scope is very similar to descoping, except that it produces new
-- variable names. The primary difference is that descoping exists to serve
-- the instantiation process, wherein the descoped variables are immediately
-- substituted asay, while opening a scope exists to serve the process of
-- performing actions under a scope, where the variables still exist and are
-- now in some variable context to be referenced.

openScope :: [String] -> Scope sig -> ([String], ABT sig)
openScope oldNs sc =
  let ns = names sc
      newNames = freshen oldNs ns
      newVars = map (Var . Free . FreeVar) newNames
      m = instantiate sc newVars
  in (newNames, m)







-- * Substitution



-- | Substitution on ABTs just looks up variables, and recurses into scopes.
-- We do however need to track how many binders we've passed under in order to
-- shift free variables.

subst :: Int -> [(FreeVar, ABT sig)] -> ABT sig -> ABT sig
subst l subs (Var (Free n)) =
  case lookup n subs of
    Nothing -> Var (Free n)
    Just x -> shift 0 l x
subst _ _ (Var v) = Var v
subst l subs (c :$: xs) = c :$: map (substScope l subs) xs



-- | Substitution for scopes is similarly simple.

substScope :: Int -> [(FreeVar, ABT sig)] -> Scope sig -> Scope sig
substScope l subs (Scope ns _ b) =
  Scope ns (freeVars b') b'
  where b' = subst (l + length ns) subs b







-- * Instantiation of scopes



-- | Instantiating a scope simply means substituting an appropriate number
-- of terms in for the variables bound by the scope. The resulting term has to
-- be unshifted by the number of variables now removed by instantiation,
-- because some of the terms inside the result might be variables bound by a
-- scope higher than the one being instantiated.

instantiate :: Scope sig -> [ABT sig] -> ABT sig
instantiate (Scope ns fns b) xs
  | length ns /= length xs =
      error "Cannot instantiate along differing numbers of arguments."
  | null ns = b
  | otherwise = subst 0 subs (unshift l l b')
    where
      l = length xs
      (freshNs, b') = descope (Scope ns fns b)
      subs = zip (map FreeVar freshNs) xs


-- | A convenience function for instantiating at exactly no arguments.

instantiate0 :: Scope sig -> ABT sig
instantiate0 a = instantiate a []







-- * Utility functions



-- | We can apply functions under a binder without disturbing the binding.

under :: (ABT sig -> ABT sig) -> Scope sig -> Scope sig
under f (Scope ns fns b) = Scope ns fns (f b)



-- | A convenience function that makes it easier to do iterated binding.

helperFold :: (a -> b -> b) -> [a] -> b -> b
helperFold c xs n = foldr c n xs



-- | It's desirable to distinguish between free variables and names for
-- defined terms. To do this for a "complete" term, one which is about to be
-- elaborated, we can replace all free variables in the term at once, by
-- applying a function that wraps them with a supplied function. The function
-- should basically turn @Free n@ into @In (Defined n)@ or some equivalent
-- term that represents the name of a defined term.

freeToDefined :: (String -> ABT sig) -> ABT sig -> ABT sig
freeToDefined d (Var (Free (FreeVar n))) = d n
freeToDefined _ (Var v) = Var v
freeToDefined d (c :$: xs) = c :$: fmap (freeToDefinedScope d) xs


-- | Similarly, we can swap out the free variables in scopes.

freeToDefinedScope :: (String -> ABT sig) -> Scope sig -> Scope sig
freeToDefinedScope d (Scope ns _ b) =
  Scope ns [] (freeToDefined d b)







-- * Operations with Metavariables



-- | Just as free variables can be substituted for, metasvariables can too.
-- Since metavariables are in some sense always free, their substitution is
-- much simpler.

substMetas :: [(MetaVar, ABT sig)] -> ABT sig -> ABT sig
substMetas [] x = x
substMetas subs (Var (Meta m)) =
  case lookup m subs of
    Nothing -> Var (Meta m)
    Just x -> x
substMetas _ (Var v) =
  Var v
substMetas subs (c :$: xs) =
  c :$: map (substMetasScope subs) xs


-- | We need to also be able to substitute metavariables in scopes.

substMetasScope :: [(MetaVar, ABT sig)] -> Scope sig -> Scope sig
substMetasScope subs sc = substMetas subs `under` sc


-- | We can perform occurs checks on ABTs by using the generic ABT fold.

occurs :: MetaVar -> ABT sig -> Bool
occurs m (Var (Meta m')) = m == m'
occurs _ (Var _) = False
occurs m (_ :$: xs) = any (occursScope m) xs

occursScope :: MetaVar -> Scope sig -> Bool
occursScope m (Scope _ _ x) = occurs m x


-- | We can get a list of the metavars in an ABT.

metaVars :: ABT sig -> [MetaVar]
metaVars (Var (Meta m)) = [m]
metaVars (Var _) = []
metaVars (_ :$: xs) = xs >>= metaVarsScope

metaVarsScope :: Scope sig -> [MetaVar]
metaVarsScope (Scope _ _ x) = metaVars x






-- * Equality

-- | Equality on ABTs is pretty straight forward structural equality. Because
-- the signature provides construct names, we just need to ensure that the
-- equated constructs have the same name, and equal argument lists.

instance Eq sig => Eq (ABT sig) where
  Var x == Var y = x == y
  c :$: xs == c' :$: xs' = c == c' && xs == xs'
  _ == _ = False


-- | The equality for scopes is slightly more involved though, because scopes
-- store some information about names for convenience, and also use De Bruijn
-- indices. So to equate scopes, we just need them to bind the same number of
-- variables, for for their bodies to be equal.

instance Eq sig => Eq (Scope sig) where
  Scope ns _ x == Scope ns' _ y =
    length ns == length ns' && x == y