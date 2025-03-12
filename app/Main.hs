module Main where
import Language.Plume.Frontend.Parser (parsePlumeFile)
import Language.Plume.Frontend.Parser.Expression (parseToplevel)
import Control.Monad.Result (showError)

main :: IO ()
main = do
  let filename = "example/main.plm"
  fileContent <- decodeUtf8 <$> readFileBS filename

  res <- parsePlumeFile filename fileContent parseToplevel

  case res of
    Left err -> putStrLn (showError err)
    Right ast -> mapM_ print ast