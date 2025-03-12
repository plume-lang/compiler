{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PatternSynonyms #-}
module Language.Plume.Syntax.HLIR (
  -- * Re-exports
  module Ty,
  module Pos,
  module Ann,
  module Lit,

  -- * Exported types
  Expression(..),
  Pattern(..),
  Toplevel(..),
  DataConstructor(..),

  -- * Type family
  HLIR,

  -- * Pattern synonyms
  pattern MkExprBinary
) where

import Language.Plume.Syntax.Internal.Type as Ty
import Language.Plume.Syntax.Internal.Position as Pos
import Language.Plume.Syntax.Internal.Annotation as Ann
import Language.Plume.Syntax.Internal.Literal as Lit
import GHC.TypeLits (Symbol, TypeError, ErrorMessage(..))
import GHC.Show qualified as S

-- | Expression type
data Expression f t 
  = MkExprLiteral Lit.Literal
  | MkExprVariable (Ann.Annotation (f t))
  | MkExprApplication (Expression f t) [Expression f t]
  | MkExprLambda [Ann.Annotation (f t)] (f t) (Expression f t)
  | MkExprLet (Ann.Annotation (f t)) (Expression f t) (Expression f t)
  | MkExprMatch (Expression f t) [(Pattern f t, Expression f t)]
  | MkExprIf (Expression f t) (Expression f t) (Expression f t)
  | MkExprLoc Pos.Position (Expression f t)
  | MkExprIs (Expression f t) (Pattern f t)

data Toplevel f t
  = MkTopFunction (Set Ty.QuVar) (Ann.Annotation (f t)) [Ann.Annotation (f t)] (Expression f t)
  | MkTopType (Ann.Annotation (f t)) (f t)
  | MkTopData (Ann.Annotation [Ty.QuVar]) [DataConstructor f t]
  | MkTopLoc Pos.Position (Toplevel f t)
  | MkTopExpr (Expression f t)

data DataConstructor f t
  = MkDataVariable Text
  | MkDataConstructor Text [Ty.Type]

-- | Pattern type
data Pattern f t 
  = MkPatLiteral Lit.Literal
  | MkPatVariable (Ann.Annotation (f t))
  | MkPatDataVariant Text
  | MkPatDataConstructor Text [Pattern f t]
  | MkPatLoc Pos.Position (Pattern f t)

type family HLIR (symbol :: Symbol) where
  HLIR "Expression" = Expression Maybe Ty.Type
  HLIR "Pattern" = Pattern Maybe Ty.Type
  HLIR "Type" = Maybe Ty.Type
  HLIR "Toplevel" = Toplevel Maybe Ty.Type
  HLIR "DataConstructor" = DataConstructor Maybe Ty.Type
  HLIR _ = TypeError ('Text "Unknown HLIR type")
  
pattern MkExprBinary :: Text -> Expression Maybe t -> Expression Maybe t -> Expression Maybe t
pattern MkExprBinary op l r = MkExprApplication (MkExprVariable (MkAnnotation op Nothing)) [l, r]

-- LOCATE INSTANCES

instance Locate (Expression f t) where
  locate e p = MkExprLoc p e

instance Locate (Pattern f t) where
  locate p pos = MkPatLoc pos p

instance Locate (Toplevel f t) where
  locate t pos = MkTopLoc pos t

-- SHOW INSTANCES

instance (Show (f t), Show t) => Show (Expression f t) where
  show (MkExprLiteral l) = show l
  show (MkExprVariable a) = toString a.name
  show (MkExprApplication e es) = show e <> "(" <> intercalate ", " (map show es) <> ")"
  show (MkExprLambda as _ e) = "(" <> intercalate ", " (map show as) <> ") -> " <> show e
  show (MkExprLet a e1 e2) = show a <> " = " <> show e1 <> " in " <> show e2 
  show (MkExprMatch e ps) = "match " <> show e <> " with " <> intercalate " | " (map (\(p, b) -> show p <> " -> " <> show b) ps)
  show (MkExprIf e1 e2 e3) = "if " <> show e1 <> " then " <> show e2 <> " else " <> show e3
  show (MkExprLoc _ e) = show e
  show (MkExprIs e p) = show e <> " is " <> show p

instance (Show (f t), Show t) => Show (Pattern f t) where
  show (MkPatLiteral l) = show l
  show (MkPatVariable a) = "let " <> toString a.name
  show (MkPatDataVariant t) = toString t
  show (MkPatDataConstructor t ps) = show t <> "(" <> intercalate ", " (map show ps) <> ")"
  show (MkPatLoc _ e) = show e

instance (Show (f t), Show t) => Show (DataConstructor f t) where
  show (MkDataVariable t) = toString t
  show (MkDataConstructor t ps) = show t <> "(" <> intercalate ", " (map show ps) <> ")"

instance (Show (f t), Show t) => Show (Toplevel f t) where
  show (MkTopFunction gens a as e) = "fn " <> toString a.name <> "[" <> intercalate ", " (map toString (toList gens)) <> "](" <> intercalate "," (map show as) <> ") = " <> show e
  show (MkTopType a t) = "type " <> show a <> " = " <> show t
  show (MkTopData a cs) = "data " <> show a <> " = " <> intercalate " | " (map show cs)
  show (MkTopLoc _ t) = show t
  show (MkTopExpr e) = show e

-- EQUALITY INSTANCES

instance (Eq (f t), Eq t) => Eq (Expression f t) where
  MkExprLiteral l1 == MkExprLiteral l2 = l1 == l2
  MkExprVariable a1 == MkExprVariable a2 = a1 == a2
  MkExprApplication e1 es1 == MkExprApplication e2 es2 = e1 == e2 && es1 == es2
  MkExprLambda as1 _ e1 == MkExprLambda as2 _ e2 = as1 == as2 && e1 == e2
  MkExprLet a1 e1 b1 == MkExprLet a2 e2 b2 = a1 == a2 && e1 == e2 && b1 == b2
  MkExprMatch e1 ps1 == MkExprMatch e2 ps2 = e1 == e2 && ps1 == ps2
  MkExprIf c1 t1 e1 == MkExprIf c2 t2 e2 =  c1 == c2 && t1 == t2 && e1 == e2
  MkExprLoc _ e1 == MkExprLoc _ e2 = e1 == e2
  MkExprIs e1 p1 == MkExprIs e2 p2 = e1 == e2 && p1 == p2
  _ == _ = False

instance (Eq (f t), Eq t) => Eq (Pattern f t) where
  MkPatLiteral l1 == MkPatLiteral l2 = l1 == l2
  MkPatVariable a1 == MkPatVariable a2 = a1 == a2
  MkPatDataVariant t1 == MkPatDataVariant t2 = t1 == t2
  MkPatDataConstructor t1 ps1 == MkPatDataConstructor t2 ps2 = t1 == t2 && ps1 == ps2
  MkPatLoc _ e1 == MkPatLoc _ e2 = e1 == e2
  _ == _ = False

instance (Eq (f t), Eq t) => Eq (DataConstructor f t) where
  MkDataVariable t1 == MkDataVariable t2 = t1 == t2
  MkDataConstructor t1 ps1 == MkDataConstructor t2 ps2 = t1 == t2 && ps1 == ps2
  _ == _ = False

instance (Eq (f t), Eq t) => Eq (Toplevel f t) where
  MkTopFunction gens1 a1 as1 e1 == MkTopFunction gens2 a2 as2 e2 = gens1 == gens2 && a1 == a2 && as1 == as2 && e1 == e2
  MkTopType a1 t1 == MkTopType a2 t2 = a1 == a2 && t1 == t2
  MkTopData a1 cs1 == MkTopData a2 cs2 = a1 == a2 && cs1 == cs2
  MkTopLoc _ t1 == MkTopLoc _ t2 = t1 == t2
  MkTopExpr e1 == MkTopExpr e2 = e1 == e2
  _ == _ = False
