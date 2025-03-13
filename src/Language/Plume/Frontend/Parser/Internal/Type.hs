module Language.Plume.Frontend.Parser.Internal.Type where
import qualified Language.Plume.Frontend.Parser as P
import qualified Language.Plume.Syntax.HLIR as HLIR
import qualified Language.Plume.Frontend.Parser.Lexer as Lex
import qualified Data.List as List

-- | TYPE
-- | Parse a type.
parseType :: (MonadIO m) => P.Parser m HLIR.Type
parseType =
  P.choice [
    -- Function type constructor
    -- Defined as the following:
    --
    -- "fn" "(" type ("," type)* ")" ":" type
    do
      tys <- Lex.parens $ P.sepBy parseType Lex.comma
      ret <- Lex.symbol "=" *> parseType

      pure $ tys HLIR.:->: ret,

    -- Mutable type constructor
    -- Defined as the following:
    --
    -- "mut" type
    do
      void $ Lex.reserved "mut"
      HLIR.MkTyMutable <$> parseType,

    -- Tuple type constructor
    -- Defined as the following:
    --
    -- "(" type "," type ")"
    Lex.parens $ do
      x <- parseType
      void $ Lex.reserved ","
      HLIR.MkTyTuple x <$> parseType,

    -- Type application constructor
    -- Defined as the following:
    --
    -- identifier "<" type ("," type)* ">"
    P.try $ do
      idt <- Lex.identifier
      tys <- Lex.brackets $ P.sepBy1 parseType Lex.comma

      pure $ HLIR.MkTyApp (HLIR.MkTyId idt) tys,

    -- Records
    -- Defined as the following:
    --
    -- "{" (identifier ":" type ("," identifier ":" type)*)? ("|" type)? "}"
    P.try $ do
      void $ Lex.symbol "{"
      fields <- P.option [] $ P.sepBy1 parseField Lex.comma

      rest <- P.optional $ do
        void $ Lex.symbol ","
        void $ Lex.symbol "..."
        parseType
      
      void $ Lex.symbol "}"

      pure . HLIR.MkTyRecord $ List.foldl 
        (\acc (idt, ty) -> HLIR.MkTyRowExtend idt ty acc) 
        (fromMaybe HLIR.MkTyRowEmpty rest)
        fields,
    
    Lex.identifier <&> HLIR.MkTyId
  ]

  where
    parseField = do
      idt <- Lex.identifier
      void $ Lex.symbol ":"
      ty <- parseType
      pure (idt, ty)