module Main where

import Data.String
import Data.Text (Text)
import Data.Text qualified

main :: IO ()
main = do
    i :: Int <- readLn 
    print @Text $ 
                    interpolateStuff
                    [
                        Left "aa",
                        Right (Foo i),
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

newtype Foo = Foo Int

instance Interpolate Foo where
    interpolate = fooToString

{-# RULES "noIntermediateStringForText" forall foo. Data.Text.pack (fooToString foo) = fooToText foo #-}

fooToText :: Foo -> Text
fooToText (Foo i) = if i > 5 then Data.Text.pack "Foo" else Data.Text.pack "Bar"

fooToString :: Foo -> String
fooToString (Foo i) = if i > 5 then "FooS" else "BarS"
{-# NOINLINE fooToString #-}