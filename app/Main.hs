module Main where

import Data.String
import Data.Text (Text)
import Data.Text qualified

main :: IO ()
main = do
  i :: Int <- readLn
  print @Text $
    -- Notice that we are creating a 'Text' value,
    -- not a 'String'.
    interpolateStuff
      [ Left "aa",
        Right (Foo i),
        Left "bb"
      ]

-- | We aren't using any interpolation syntax, the list of 'Either's is enough
-- for the example.
interpolateStuff ::
  (Interpolate a, Monoid s, IsString s) =>
  [Either String a] ->
  s
interpolateStuff =
  foldr
    ( \e rest ->
        case e of
          Left s -> fromString s
          Right a -> fromString (interpolate a)
          <> rest
    )
    (fromString "")

class Interpolate a where
  -- | We always render into a 'String'! 
  -- If 'interpolateStuff' generates a 'Text', and the
  -- type @a@ has some associated 
  -- 'Text'-generating render function, 
  -- we would like to use that instead!
  -- That's the motivation of the rewrite rule below.
  interpolate :: a -> String

newtype Foo = Foo Int

instance Interpolate Foo where
  interpolate = fooToString

{-# RULES "noIntermediateStringForText" forall foo . Data.Text.pack (fooToString foo) = fooToText foo #-}

-- We need the NOINLINE, otherwise 'fooToString' might be
-- inlined too eagerly, preventing the rewrite rule from firing.
fooToString :: Foo -> String
fooToString (Foo i) = if i > 5 then "FooS" else "BarS"
{-# NOINLINE fooToString #-}

-- | We assume of course that 'fooToString' and 'fooToText'
-- generate compatible strings.
fooToText :: Foo -> Text
fooToText (Foo i) = if i > 5 then Data.Text.pack "Foo" else Data.Text.pack "Bar"
