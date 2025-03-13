module Language.Plume.Frontend.Typechecker.Checker where

import Language.Plume.Frontend.Typechecker.Monad qualified as M
import Language.Plume.Frontend.Typechecker.Unification qualified as M
import Language.Plume.Syntax.HLIR qualified as HLIR
import Control.Monad.Result qualified as Err
import qualified Data.Map as Map
import qualified Data.Foldable as Foldable
import qualified Data.Set as Set

check :: M.MonadChecker m => HLIR.HLIR "Expression" -> HLIR.Type -> m (HLIR.TLIR "Expression")
check (HLIR.MkExprVariable var) ty = do
  vars <- readIORef M.checkerState <&> M.variables

  case Map.lookup var.name vars of
    Just scheme -> do
      ty' <- M.instantiate scheme
      M.unifiesWith ty ty'
      pure $ HLIR.MkExprVariable var { HLIR.value = Identity ty' }
    Nothing -> M.throw (Err.VariableNotFound var.name (Just ty))
check (HLIR.MkExprApplication f args) fTy@(argsTys HLIR.:->: _) = do
  f' <- check f fTy
  args' <- zipWithM check args argsTys
  pure $ HLIR.MkExprApplication f' args'
check (HLIR.MkExprLambda args ty body) (argsTys HLIR.:->: retTy) = do
  let anns = zipWith (\(HLIR.MkAnnotation name ty') expectedTy -> case ty' of
            Just foundTy -> HLIR.MkAnnotation name (Identity foundTy)
            Nothing -> HLIR.MkAnnotation name (Identity expectedTy)
          ) args argsTys

  let vars = Map.fromList $ zip (map HLIR.name anns) (map HLIR.value anns)
  let vars' = Map.map (HLIR.Forall [] . runIdentity) vars

  M.with M.checkerState (\s -> s { M.variables = Map.union vars' s.variables }) $ do
    body' <- check body retTy

    Foldable.for_ ty (`M.unifiesWith` retTy)

    pure $ HLIR.MkExprLambda anns (Identity retTy) body'
check (HLIR.MkExprLet var expr body) ty = do
  varTy <- M.fresh
  expr' <- M.with M.checkerState (\s -> s { M.variables = Map.insert var.name (HLIR.Forall [] varTy) s.variables }) $ check expr varTy

  body' <- check body ty

  pure $ HLIR.MkExprLet var { HLIR.value = Identity varTy } expr' body'
check (HLIR.MkExprMatch expr cases) ty = do
  expr' <- check expr ty
  cases' <- traverse (\(pat, body) -> do
    (pat', env) <- checkPattern pat ty
    body' <- M.with
      M.checkerState
      (\s -> s { M.variables = env <> s.variables }) $
        check body ty
    pure (pat', body')
    ) cases

  pure $ HLIR.MkExprMatch expr' cases'
check (HLIR.MkExprIf cond then' else') ty = do
  cond' <- check cond HLIR.MkTyBool
  then'' <- check then' ty
  else'' <- check else' ty

  pure $ HLIR.MkExprIf cond' then'' else''
check (HLIR.MkExprLoc pos expr) ty = do
  expr' <- check expr ty
  pure $ HLIR.MkExprLoc pos expr'
check (HLIR.MkExprIs {}) _ = Err.compilerError "Typechecking of 'is' expression not implemented yet"
check (HLIR.MkExprLiteral l) ty = do
  let lTy = typeOfLit l

  M.unifiesWith ty lTy

  pure $ HLIR.MkExprLiteral l
check (HLIR.MkExprList es) (HLIR.MkTyList ty) = do
  es' <- mapM (`check` ty) es

  pure $ HLIR.MkExprList es'
check (HLIR.MkExprRecordAccess e k) ty = do
  r <- M.fresh

  e' <- check e (HLIR.MkTyRecord $ HLIR.MkTyRowExtend k ty r)

  pure $ HLIR.MkExprRecordAccess e' k
check (HLIR.MkExprRecordRestrict e k) ty = do
  a <- M.fresh

  e' <- check e (HLIR.MkTyRecord $ HLIR.MkTyRowExtend k a ty)

  pure $ HLIR.MkExprRecordRestrict e' k
-- check (HLIR.MkExprRecordExtend e k v) ty = do
--   a <- M.fresh

--   (e', recTy) <- synthesize e

--   v' <- check v a

--   print (recTy, a, ty)

--   M.unifiesWith (HLIR.MkTyRecord $ HLIR.MkTyRowExtend k a recTy) ty

--   pure $ HLIR.MkExprRecordExtend e' k v'
check e ty = do
  (e', eTy) <- synthesize e
  M.unifiesWith eTy ty

  pure e'

synthesize :: M.MonadChecker m => HLIR.HLIR "Expression" -> m (HLIR.TLIR "Expression", HLIR.Type)
synthesize (HLIR.MkExprVariable var) = do
  vars <- readIORef M.checkerState <&> M.variables

  case Map.lookup var.name vars of
    Just scheme -> do
      ty <- M.instantiate scheme
      pure (HLIR.MkExprVariable var { HLIR.value = Identity ty }, ty)
    Nothing -> M.throw (Err.VariableNotFound var.name Nothing)
synthesize (HLIR.MkExprApplication f args) = do
  (f', fTy) <- synthesize f

  case fTy of
    argsTys HLIR.:->: retTy -> do
      args' <- zipWithM check args argsTys
      pure (HLIR.MkExprApplication f' args', retTy)
    _ -> do
      (args', argsTys) <- mapAndUnzipM synthesize args

      retTy <- M.fresh
      M.unifiesWith fTy (argsTys HLIR.:->: retTy)

      pure (HLIR.MkExprApplication f' args', retTy)
synthesize (HLIR.MkExprLambda args mTy body) = do
  anns <- mapM (\(HLIR.MkAnnotation name ty') -> case ty' of
            Just foundTy -> pure $ HLIR.MkAnnotation name (Identity foundTy)
            Nothing -> HLIR.MkAnnotation name . Identity <$> M.fresh
          ) args

  let vars = Map.fromList $ zip (map HLIR.name anns) (map HLIR.value anns)
  let vars' = Map.map (HLIR.Forall [] . runIdentity) vars

  M.with M.checkerState (\s -> s { M.variables = Map.union vars' s.variables }) $ do
    case mTy of
      Just ty -> do
        body' <- check body ty
        pure (HLIR.MkExprLambda anns (Identity ty) body', ty)
      Nothing -> do
        (body', ty) <- synthesize body
        pure (HLIR.MkExprLambda anns (Identity ty) body', ty)
synthesize (HLIR.MkExprLet var expr body) = do
  varTy <- M.fresh
  (expr', exprTy) <- M.with M.checkerState (\s -> s { M.variables = Map.insert var.name (HLIR.Forall [] varTy) s.variables }) $ synthesize expr

  M.unifiesWith varTy exprTy

  (body', ty) <- M.with
    M.checkerState
    (\s -> s { M.variables = Map.insert var.name (HLIR.Forall [] exprTy) s.variables }) $
      synthesize body

  pure (HLIR.MkExprLet var { HLIR.value = Identity varTy } expr' body', ty)
synthesize (HLIR.MkExprMatch expr cases) = do
  (expr', ty) <- synthesize expr
  (_, cs) <- M.mapMWithAcc (\acc (pat, e) -> do
      (pat', env) <- checkPattern pat ty

      M.with M.checkerState (\st -> st { M.variables = env <> st.variables }) $
        case acc of
          Just accTy -> do
            e' <- check e accTy

            pure (acc, ((pat', e'), accTy))
          Nothing -> do
            (e', eTy) <- synthesize e

            pure (Just eTy, ((pat', e'), eTy))
    ) Nothing cases


  let (cases', tys) = unzip cs

  (exprTy, exprTys) <- case tys of
    [] -> M.throw M.EmptyMatch
    (x : xs'') -> return (x, xs'')

  -- Unify the return type with the type of the case expressions
  forM_ exprTys $ M.unifiesWith exprTy

  pure (HLIR.MkExprMatch expr' cases', ty)
synthesize (HLIR.MkExprIf cond then' else') = do
  cond' <- check cond HLIR.MkTyBool
  (then'', ty) <- synthesize then'
  else'' <- check else' ty

  pure (HLIR.MkExprIf cond' then'' else'', ty)
synthesize (HLIR.MkExprLoc pos expr) = do
  HLIR.pushPosition pos
  (expr', ty) <- synthesize expr
  void HLIR.popPosition
  pure (HLIR.MkExprLoc pos expr', ty)
synthesize (HLIR.MkExprIs {}) = Err.compilerError "Typechecking of 'is' expression not implemented yet"
synthesize (HLIR.MkExprLiteral l) = do
  let lTy = typeOfLit l

  pure (HLIR.MkExprLiteral l, lTy)
synthesize (HLIR.MkExprList es) = do
  case es of
    [] -> do
      ty <- M.fresh
      pure (HLIR.MkExprList [], HLIR.MkTyList ty)
    (x : xs) -> do
      (x', ty) <- synthesize x
      xs' <- mapM (`check` ty) xs

      pure (HLIR.MkExprList (x' : xs'), HLIR.MkTyList ty)
synthesize (HLIR.MkExprRecordAccess e k) = do
  (e', ty) <- synthesize e

  a <- M.fresh
  r <- M.fresh

  let funTy = [HLIR.MkTyRecord $ HLIR.MkTyRowExtend k a r] HLIR.:->: a

  ret <- M.fresh

  M.unifiesWith ([ty] HLIR.:->: ret) funTy

  pure (HLIR.MkExprRecordAccess e' k, ret)
synthesize (HLIR.MkExprRecordRestrict e k) = do
  a <- M.fresh
  r <- M.fresh

  let funTy = [HLIR.MkTyRecord $ HLIR.MkTyRowExtend k a r] HLIR.:->: HLIR.MkTyRecord r

  (recExpr, recTy) <- synthesize e

  ret <- M.fresh

  M.unifiesWith ([recTy] HLIR.:->: ret) funTy

  pure (HLIR.MkExprRecordRestrict recExpr k, ret)
synthesize (HLIR.MkExprRecordExtend e k v) = do
  (e', ty) <- synthesize e
  (v', vTy) <- synthesize v

  a <- M.fresh
  r <- M.fresh

  let funTy = [a, HLIR.MkTyRecord r] HLIR.:->: HLIR.MkTyRecord (HLIR.MkTyRowExtend k a r)

  ret <- M.fresh

  M.unifiesWith ([vTy, ty] HLIR.:->: ret) funTy

  pure (HLIR.MkExprRecordExtend e' k v', ret)
synthesize HLIR.MkExprRecordEmpty = 
  pure (HLIR.MkExprRecordEmpty, HLIR.MkTyRecord HLIR.MkTyRowEmpty)

typeOfLit :: HLIR.Literal -> HLIR.Type
typeOfLit (HLIR.MkLitInt _) = HLIR.MkTyInt
typeOfLit (HLIR.MkLitFloat _) = HLIR.MkTyFloat
typeOfLit (HLIR.MkLitString _) = HLIR.MkTyString
typeOfLit (HLIR.MkLitChar _) = HLIR.MkTyChar
typeOfLit (HLIR.MkLitBool _) = HLIR.MkTyBool

checkPattern :: M.MonadChecker m => HLIR.HLIR "Pattern" -> HLIR.Type -> m (HLIR.TLIR "Pattern", Map.Map Text HLIR.Scheme)
checkPattern (HLIR.MkPatLiteral l) ty = do
  let lTy = typeOfLit l

  M.unifiesWith ty lTy

  pure (HLIR.MkPatLiteral l, Map.empty)
checkPattern (HLIR.MkPatVariable var) ty = do
  pure (HLIR.MkPatVariable var { HLIR.value = Identity ty }, Map.singleton var.name (HLIR.Forall [] ty))
checkPattern (HLIR.MkPatDataVariant t) ty = do
  vars <- readIORef M.checkerState <&> M.variables

  case Map.lookup t vars of
    Just scheme -> do
      ty' <- HLIR.simplify =<< M.instantiate scheme
      M.unifiesWith ty ty'

      pure (HLIR.MkPatDataVariant t, Map.empty)
    Nothing -> M.throw (Err.InvalidConstructor t)
checkPattern (HLIR.MkPatDataConstructor t ps) ty = do
  vars <- readIORef M.checkerState <&> M.variables

  case Map.lookup t vars of
    Just scheme -> do
      ty' <- HLIR.simplify =<< M.instantiate scheme

      case ty' of
        patArgsTys HLIR.:->: retTy -> do
          ps' <- zipWithM checkPattern ps patArgsTys
          M.unifiesWith retTy ty

          pure (HLIR.MkPatDataConstructor t (map fst ps'), Map.unions (map snd ps'))
        _ -> M.throw (Err.InvalidConstructor t)
    Nothing -> M.throw (Err.InvalidConstructor t)
checkPattern (HLIR.MkPatLoc pos pat) ty = do
  HLIR.pushPosition pos
  (pat', env) <- checkPattern pat ty
  void HLIR.popPosition
  pure (HLIR.MkPatLoc pos pat', env)
checkPattern HLIR.MkPatWildcard _ = pure (HLIR.MkPatWildcard, Map.empty)

checkToplevel :: M.MonadChecker m => HLIR.HLIR "Toplevel" -> m (HLIR.TLIR "Toplevel")
checkToplevel (HLIR.MkTopExpr (HLIR.MkExprLoc p e)) = do
  HLIR.pushPosition p
  e' <- checkToplevel (HLIR.MkTopExpr e)
  void HLIR.popPosition
  pure $ HLIR.MkTopLoc p e'
checkToplevel (HLIR.MkTopLet var expr body) = do
  varTy <- M.fresh
  (expr', exprTy) <- M.with M.checkerState (\s -> s { M.variables = Map.insert var.name (HLIR.Forall [] varTy) s.variables }) $ synthesize expr

  M.unifiesWith varTy exprTy

  modifyIORef'
    M.checkerState
    (\s -> s { M.variables = Map.insert var.name (HLIR.Forall [] exprTy) s.variables })

  (body', _) <- synthesize body

  pure (HLIR.MkTopLet var { HLIR.value = Identity varTy } expr' body')
checkToplevel (HLIR.MkTopExpr expr) = do
  (expr', _) <- synthesize expr
  pure $ HLIR.MkTopExpr expr'
checkToplevel (HLIR.MkTopData t cs) = do
  let header
        | null t.value = HLIR.MkTyId t.name
        | otherwise = HLIR.MkTyApp (HLIR.MkTyId t.name) (map HLIR.MkTyQuantified t.value)
  let generics = Set.fromList t.value

  let css = map (`checkDataCs` header) cs

  let schemes = map (second (HLIR.Forall (Set.toList generics))) css

  modifyIORef
    M.checkerState
    (\s -> s { M.variables = Map.union (Map.fromList schemes) s.variables })

  pure $ HLIR.MkTopData t cs

  where
    checkDataCs (HLIR.MkDataVariable n) h = (n, h)
    checkDataCs (HLIR.MkDataConstructor n ts) h = (n, ts HLIR.:->: h)
checkToplevel (HLIR.MkTopFunction generics ann args body) = do
  let generics' = Set.toList generics
  args' <- mapM (\(HLIR.MkAnnotation name ty) -> do
    ty' <- maybe M.fresh pure ty

    pure $ HLIR.MkAnnotation name (Identity ty')
    ) args

  let vars = Map.fromList $ zip (map HLIR.name args') (map HLIR.value args')
  let vars' = Map.map (HLIR.Forall [] . runIdentity) vars

  (finalExpr, ty) <- M.with
    M.checkerState
    (\s -> s { M.variables = Map.union vars' s.variables })
    $ do
      (body', ty) <- case ann.value of
        Just t -> (,t) <$> check body t
        Nothing -> synthesize body

      pure (HLIR.MkTopFunction generics ann { HLIR.value = Identity ty } args' body', ty)

  let finalTy = HLIR.Forall generics' $ map (runIdentity . HLIR.value) args' HLIR.:->: ty

  modifyIORef'
    M.checkerState
    (\s -> s { M.variables = Map.insert ann.name finalTy s.variables })

  pure finalExpr
checkToplevel (HLIR.MkTopLoc pos t) = do
  HLIR.pushPosition pos
  t' <- checkToplevel t
  void HLIR.popPosition
  pure $ HLIR.MkTopLoc pos t'
checkToplevel (HLIR.MkTopType {}) = Err.compilerError "Typechecking of type declarations not implemented yet"
checkToplevel (HLIR.MkTopNative ann args retTy code) = do
  let generics = Set.toList ann.value
  let scheme = HLIR.Forall generics $ map HLIR.value args HLIR.:->: retTy

  modifyIORef'
    M.checkerState
    (\s -> s { M.variables = Map.insert ann.name scheme s.variables })

  pure $ HLIR.MkTopNative ann args retTy code

runTypecheckingPass ::
  MonadIO m =>
  [HLIR.HLIR "Toplevel"] ->
  m (Either M.Error [HLIR.TLIR "Toplevel"])
runTypecheckingPass toplevels = do
  let initialState = M.MkCheckerState { M.variables = Map.empty }

  writeIORef M.checkerState initialState
  writeIORef M.currentLevel 0
  writeIORef M.typeCounter 0

  runExceptT (mapM checkToplevel toplevels)
