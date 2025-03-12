{-# LANGUAGE LambdaCase #-}
module Language.Plume.Frontend.Typechecker.Unification where

import qualified Language.Plume.Syntax.HLIR as HLIR
import qualified Language.Plume.Frontend.Typechecker.Monad as M

-- Check to see if a TVar (the first argument) occurs in the type
-- given as the second argument. Fail if it does.
-- At the same time, update the levels of all encountered free
-- variables to be the min of variable's current level and
-- the level of the given variable tvr.
doesOccurB :: IORef HLIR.TyVar -> HLIR.Type -> IO Bool
doesOccurB tvr (HLIR.MkTyVar tv') = do
  tvr' <- readIORef tvr
  tvr'' <- readIORef tv'
  case tvr'' of
    HLIR.Link t -> doesOccurB tvr t
    HLIR.Unbound name lvl -> do
      let newMinLvl = case tvr' of
            HLIR.Link _ -> lvl
            HLIR.Unbound _ lvl' -> min lvl' lvl
      writeIORef tv' (HLIR.Unbound name newMinLvl)
      pure (tvr == tv')
doesOccurB tv (HLIR.MkTyApp t1 t2) = do
  b <- doesOccurB tv t1
  if b
    then pure True
    else or <$> traverse (doesOccurB tv) t2
doesOccurB _ _ = pure False

-- Unify two types
-- Type unification is the process of making two types equal by
-- substituting type variables with other types.
-- The unification algorithm is based on the Hindley-Milner type
-- inference algorithm.
unifiesWith :: M.MonadChecker m => HLIR.Type -> HLIR.Type -> m ()
unifiesWith t t' = do
  t1 <- HLIR.simplify t
  t2 <- HLIR.simplify t'
  if t1 == t2
    then pure ()
    else case (t1, t2) of
      (HLIR.MkTyVar tv1, _) -> readIORef tv1 >>= \case
        HLIR.Link tl -> unifiesWith tl t2
        HLIR.Unbound _ _ -> do
          whenM (liftIO $ doesOccurB tv1 t2) $ do
            M.throw (M.UnificationFail t1 t2)
          writeIORef tv1 (HLIR.Link t2)
      (_, HLIR.MkTyVar _) -> unifiesWith t2 t1
      (HLIR.MkTyQuantified qv1, HLIR.MkTyQuantified qv2) | qv1 == qv2 -> pure ()
      (HLIR.MkTyQuantified _, _) -> pure ()
      (_, HLIR.MkTyQuantified _) -> pure ()
      (HLIR.MkTyApp t1a t1b, HLIR.MkTyApp t2a t2b) | length t1b == length t2b -> do
        unifiesWith t1a t2a
        zipWithM_ unifiesWith t1b t2b
      (HLIR.MkTyId n, HLIR.MkTyId n') | n == n' -> pure ()
      _ -> M.throw (M.UnificationFail t1 t2)

-- | Check to see if two types can be unified without
-- | altering the types of any type variables.
doesUnifyWith :: M.MonadChecker m => HLIR.Type -> HLIR.Type -> m Bool
doesUnifyWith t t' = runExceptT (unifiesWith t t') >>= \case
  Left _ -> pure False
  Right _ -> pure True