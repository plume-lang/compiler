module Language.Plume.Frontend.Parser.Expression where

import qualified Language.Plume.Frontend.Parser as P
import qualified Control.Monad.Combinators.Expr as P
import qualified Text.Megaparsec.Char as P
import qualified Language.Plume.Frontend.Parser.Lexer as Lex
import qualified Language.Plume.Frontend.Parser.Internal.Literal as Lit
import qualified Language.Plume.Syntax.HLIR as HLIR
import qualified Language.Plume.Frontend.Parser.Internal.Type as Typ
import qualified Data.Set as Set

-- | PARSE ANNOTATION
-- Parse an annotation. An annotation is used to attach metadata to an AST node.
-- In this context, an annotation is an identifier followed by an optional type, 
-- consisting of the following:
--
-- - name (":" type)?
parseAnnotation :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation (Maybe a))
parseAnnotation p = P.choice [
    P.try $ do
      name <- Lex.identifier
      ty <- P.optional (Lex.symbol ":" *> p)

      pure $ HLIR.MkAnnotation name ty,
    HLIR.MkAnnotation <$> Lex.identifier <*> pure Nothing
  ]

-- | PARSE ANNOTATION'
-- Parse an annotation. An annotation is used to attach metadata to an AST node.
-- In this context, an annotation is an identifier followed by a type,
-- consisting of the following:
--
-- - name ":" type
parseAnnotation' :: MonadIO m => P.Parser m a -> P.Parser m (HLIR.Annotation a)
parseAnnotation' p = HLIR.MkAnnotation <$> Lex.identifier <*> (Lex.symbol ":" *> p)

-- | LOCALIZE
-- Localize an AST node by attaching a position to it.
-- This is used to attach a position to an AST node after it has been parsed.
-- It makes use of the Locate typeclass to attach a position to an AST node 
-- programmatically.
localize :: (MonadIO m, HLIR.Locate a) => P.Parser m a -> P.Parser m a
localize p = do
  startP <- P.getSourcePos
  x <- p
  endP <- P.getSourcePos

  pure $ HLIR.locate x (startP, endP)

-- | PARSE LITERAL
-- Parse a literal. Literals are the most basic form of data in a programming language.
-- They are also called atomic or primitive values. Literals are used to represent
-- numbers, strings, characters, booleans, and other basic types.
parseExprLiteral :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprLiteral = localize . Lex.lexeme $ HLIR.MkExprLiteral <$> Lit.parseLiteral

-- | PARSE VARIABLE
-- Parse a variable. A variable is a name that represents a value. It is used to
-- store data that can be referenced and manipulated in a program.
parseExprVariable :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprVariable = HLIR.MkExprVariable <$>
  (HLIR.MkAnnotation <$> Lex.identifier <*> pure Nothing)

-- | PARSE LAMBDA
-- Parse a lambda. A lambda is an anonymous function that can be used to define
-- a computation. It consists of the following:
--
-- - "(" (name ":" type ("," name ":" type)*)? ")" "=>" expression
-- - "(" (name ":" type ("," name ":" type)*)? ")" "{" block "}"
parseExprLambda :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprLambda = localize $ do
  params <- Lex.parens $ P.sepBy (parseAnnotation Typ.parseType) Lex.comma

  body <- P.choice [
      Lex.symbol "=" *> parseExprFull,
      Lex.braces parseBlock
    ]

  pure $ HLIR.MkExprLambda params Nothing body

-- | PARSE CONDITION
-- Parse a condition. A condition is a boolean expression that is used to control
-- the flow of a program. It consists of the following:
--
-- - "if" expression "then" expression "else" expression
parseExprIf :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprIf = localize $ do
  void $ Lex.reserved "if"
  cond <- parseExprFull
  void $ Lex.reserved "then"
  thenE <- parseExprFull
  void $ Lex.reserved "else"

  elseE <- parseExprFull

  case getExprIs cond of
    Just (e, p) -> 
      pure $ 
        HLIR.MkExprMatch e [(p, thenE), (HLIR.MkPatWildcard, elseE)]
    Nothing -> 
      pure $ 
        HLIR.MkExprIf cond thenE elseE

  where
    getExprIs :: HLIR.HLIR "Expression" -> Maybe (HLIR.HLIR "Expression", HLIR.HLIR "Pattern")
    getExprIs (HLIR.MkExprIs e p) = Just (e, p)
    getExprIs (HLIR.MkExprLoc _ e) = getExprIs e
    getExprIs _ = Nothing

-- | PARSE PATTERN
-- Parse a pattern. A pattern is used to match values in a match expression.
-- It can be a literal, a variable, a data variant, or a data constructor.
parsePattern :: (MonadIO m) => P.Parser m (HLIR.HLIR "Pattern")
parsePattern = localize $ P.choice [
    P.try $ do
      name <- Lex.identifier
      args <- Lex.parens $ P.sepBy1 parsePattern Lex.comma

      pure $ HLIR.MkPatDataConstructor name args,
    HLIR.MkPatVariable <$> do
      void $ Lex.reserved "let"
      name <- Lex.identifier

      pure $ HLIR.MkAnnotation name Nothing,
    P.try $ HLIR.MkPatDataVariant <$> Lex.identifier,
    HLIR.MkPatLiteral <$> Lex.lexeme Lit.parseLiteral
  ]

-- | PARSE MATCH
-- Parse a match expression. A match expression is used to match a value against
-- a series of patterns and execute the corresponding branch.
-- It consists of the following:
--
-- - switch expression { case pattern "->" expression (case pattern "->" expression)* }
parseExprMatch :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprMatch = localize $ do
  void $ Lex.reserved "switch"
  e <- parseExprFull

  cases <- Lex.braces $ P.many (do
    void $ Lex.reserved "case"
    pat <- parsePattern
    void $ Lex.symbol "="
    expr <- parseExprFull

    pure (pat, expr))

  pure $ HLIR.MkExprMatch e cases

-- | PARSE EXPRESSION TERM
-- Parse an expression term. An expression term is a basic building block of an expression.
-- It can be a literal, a variable, a lambda, or a parenthesized expression.
parseExprTerm :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprTerm = P.choice [
    parseExprLambda,
    parseExprIf,
    Lex.braces parseBlock,
    parseExprLiteral,
    parseExprVariable,
    Lex.parens parseExprFull
  ]

-- | PARSE EXPRESSION
-- Parse an expression. An expression is a combination of terms and operators that
-- represent a computation. It can be a literal, a variable, a lambda, a parenthesized
-- expression, or a binary operation.
parseExprFull :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseExprFull = P.makeExprParser parseExprTerm table
  where
    table = [
        [
          P.Postfix $ do
            void $ Lex.reserved "is"
            pat <- parsePattern

            pure $ \e -> HLIR.MkExprIs e pat
        ],
        [
          P.Postfix . Lex.makeUnaryOp $ do
            args <- Lex.parens (P.sepBy parseExprFull Lex.comma)
            pure $ \e -> HLIR.MkExprApplication e args
        ],
        [
            P.Postfix . Lex.makeUnaryOp $ do
              field <- P.char '.' *> Lex.nonLexedID <* Lex.scn
              args <- P.option [] $ Lex.parens (P.sepBy parseExprFull Lex.comma)
              let var = HLIR.MkExprVariable (HLIR.MkAnnotation field Nothing)
              pure $ \e -> HLIR.MkExprApplication var (e:args)
        ],
        [
          P.Postfix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "["
            idx <- parseExprFull
            void $ Lex.symbol "]"

            pure $ \e -> HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "getIndex" Nothing)) [e, idx]
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "*"
            pure $ \a b -> HLIR.MkExprBinary "*" a b,
          P.InfixL $ do
            void $ Lex.symbol "/"
            pure $ \a b -> HLIR.MkExprBinary "/" a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "+"
            pure $ \a b -> HLIR.MkExprBinary "+" a b,
          P.InfixL $ do
            void $ Lex.symbol "-"
            pure $ \a b -> HLIR.MkExprBinary "-" a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol "=="
            pure $ \a b -> HLIR.MkExprBinary "==" a b,
          P.InfixN $ do
            void $ Lex.symbol "!="
            pure $ \a b -> HLIR.MkExprBinary "!=" a b
        ],
        [
          P.InfixN $ do
            void $ Lex.symbol ">="
            pure $ \a b -> HLIR.MkExprBinary ">=" a b,
          P.InfixN $ do
            void $ Lex.symbol "<="
            pure $ \a b -> HLIR.MkExprBinary "<=" a b,
          P.InfixN $ do
            void $ Lex.symbol ">"
            pure $ \a b -> HLIR.MkExprBinary ">" a b,
          P.InfixN $ do
            void $ Lex.symbol "<"
            pure $ \a b -> HLIR.MkExprBinary "<" a b
        ],
        [
          P.InfixL $ do
            void $ Lex.symbol "and"
            pure $ \a b -> HLIR.MkExprBinary "and" a b,
          P.InfixL $ do
            void $ Lex.symbol "or"
            pure $ \a b -> HLIR.MkExprBinary "or" a b
        ],
        [
          P.Prefix . Lex.makeUnaryOp $ do
            void $ Lex.symbol "!"
            pure $ \a -> HLIR.MkExprApplication (HLIR.MkExprVariable (HLIR.MkAnnotation "!" Nothing)) [a]
        ],
        [
          P.InfixL $ do
            op <- Lex.operator
            pure $ \a b -> HLIR.MkExprBinary op a b
        ]
      ]

-- | PARSE BLOCK
-- Parse a block. A block is a sequence of statements enclosed in braces.
-- It is used to group multiple statements together and execute
-- them as a single unit.
parseBlock :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseBlock = localize $ do
  stmts <- P.sepBy parseStmtFull (Lex.symbol ";")

  pure (reduceS stmts)

  where
    reduceS :: [HLIR.HLIR "Expression"] -> HLIR.HLIR "Expression"
    reduceS (HLIR.MkExprLet ann e _ : xs) = HLIR.MkExprLet ann e (reduceS xs)
    reduceS [x] = x
    reduceS (e : xs) = HLIR.MkExprLet (HLIR.MkAnnotation "_" Nothing) e (reduceS xs)
    reduceS [] = HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing)

-- | PARSE STATEMENT
-- Parse a statement. A statement is a single instruction that performs an action.
-- It can be a let binding, an expression, or a block.
parseStmtFull :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseStmtFull = P.choice [
    parseStmtLet,
    parseExprFull
  ]

-- | PARSE LET
-- Parse a let binding. A let binding is used to bind a value to a name.
-- It consists of the following:
--
-- - let name ":" type = expression
parseStmtLet :: (MonadIO m) => P.Parser m (HLIR.HLIR "Expression")
parseStmtLet = localize $ do
  void $ Lex.reserved "let"
  ann <- parseAnnotation Typ.parseType
  void $ Lex.symbol "="
  e <- parseExprFull

  pure $ HLIR.MkExprLet ann e (HLIR.MkExprVariable (HLIR.MkAnnotation "unit" Nothing))

parseToplevel :: (MonadIO m) => P.Parser m [HLIR.HLIR "Toplevel"]
parseToplevel = Lex.scn *> P.sepEndBy parseTopFull Lex.scn <* P.eof

parseTopFunction :: (MonadIO m) => P.Parser m (HLIR.HLIR "Toplevel")
parseTopFunction = localize $ do
  void $ Lex.reserved "fn"
  name <- Lex.identifier
  generics <- Lex.brackets $ P.sepBy Lex.identifier Lex.comma

  params <- Lex.parens $ P.sepBy (parseAnnotation Typ.parseType) Lex.comma

  returnType <- P.optional $ Lex.symbol ":" *> Typ.parseType

  body <- P.choice [
      Lex.symbol "=" *> parseExprFull,
      Lex.braces parseBlock
    ]

  pure $ HLIR.MkTopFunction (Set.fromList generics) (HLIR.MkAnnotation name returnType) params body

parseTopData :: (MonadIO m) => P.Parser m (HLIR.HLIR "Toplevel")
parseTopData = localize $ do
  void $ Lex.reserved "type"
  name <- Lex.identifier
  generics <- P.option [] $ Lex.brackets (P.sepBy Lex.identifier Lex.comma)

  constructors <- Lex.braces $ P.many parseDataConstructor

  pure $ HLIR.MkTopData (HLIR.MkAnnotation name generics) constructors
  
  where
    parseDataConstructor :: (MonadIO m) => P.Parser m (HLIR.HLIR "DataConstructor")
    parseDataConstructor = P.choice [
        P.try $ do
          name <- Lex.identifier
          args <- Lex.parens $ P.sepBy Typ.parseType Lex.comma

          pure $ HLIR.MkDataConstructor name args,
        HLIR.MkDataVariable <$> Lex.identifier
      ]


parseTopFull :: (MonadIO m) => P.Parser m (HLIR.HLIR "Toplevel")
parseTopFull = localize $ P.choice [
    parseTopFunction,
    parseTopData,
    HLIR.MkTopExpr <$> parseStmtFull
  ]