
import Options.Applicative
import Data.Monoid ((<>))


data Cmd = TreeCmd deriving (Show)


cmd :: Parser Cmd 
cmd = hsubparser $
 command "tree" $ info (pure TreeCmd) (progDesc "Show the project tree")

main :: IO ()
main = do 
  opts <- execParser (info cmd $ progDesc "~ Unfinished Project ~")
  print opts
