module Vinyl.Class.Example where
import Vinyl.Class()

{-|
@
stack build && stack exec -- vinyl-class-example
@
-}
main :: IO ()
main = do
 putStrLn ""
 print $ "Vinyl.Class"
