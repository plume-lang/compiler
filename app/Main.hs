module Main where
import Language.Plume.Frontend.Parser (parsePlumeFile)
import Language.Plume.Frontend.Parser.Expression (parseToplevel)
import Control.Monad.Result (showError, handle, parseError)
import Language.Plume.Frontend.Typechecker.Checker (runTypecheckingPass)

main :: IO ()
main = do
  let filename = "example/main.plm"
  fileContent <- decodeUtf8 <$> readFileBS filename

  res <- parsePlumeFile filename fileContent parseToplevel

  case res of
    Left err -> parseError err filename (Just fileContent)
    Right ast -> do
      typedAST <- runTypecheckingPass ast

      handle typedAST $ \typed -> do
        mapM_ print typed