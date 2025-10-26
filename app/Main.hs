module Main where

import Data.String
import Data.Text (Text)
import Data.Text qualified

main :: IO ()
main = putStrLn $ 
    interpolateStuff
    [
        Left "aa",
        Right Foo,
        Left "bb"
    ]

class Interpolate a where
    interpolate :: a -> String

interpolateStuff :: 
    (Interpolate a, Monoid s, IsString s) =>
    [Either String a] ->
    s
interpolateStuff = 
    foldr 
    (\e rest ->
        case e of
            Left s -> fromString s
            Right a -> fromString (interpolate a) 
        <> rest
    ) 
    (fromString "")

data Foo = Foo 

instance Interpolate Foo where
    interpolate _ = "this is foo"

fooToText :: Foo -> Text
fooToText _ = Data.Text.pack "this is foo"