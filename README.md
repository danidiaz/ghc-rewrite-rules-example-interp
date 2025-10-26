# Playing with GHC rewrite rules

This is an experiment prompted by the proposed [`-XStringInterpolation`](https://discourse.haskell.org/t/updates-for-string-related-ghc-proposals/13160) extension.

The current version of the proposal relies on the `interpolateString` function and the `Interpolate` typeclass:

```
interpolateString ::
  (IsString s, Monoid s) =>
  ( (forall a. Interpolate a => a -> s)
    -> (s -> s)
    -> (s -> s -> s)
    -> s
    -> s
  )
  -> s
interpolateString f = mconcat $ f (fromString . interpolate) id (:) []
{-# INLINE interpolateString #-}

class Interpolate a where
  {-# MINIMAL interpolate | interpolateS #-}

  interpolate :: a -> String
  interpolate x = interpolateS x ""

  interpolateS :: a -> ShowS
  interpolateS x s = interpolate x <> s
```

Onde small annoyance of having an `Interpolate a` typeclass that always renders to `String` is that, if we are interpolating some value into a `Text`, we will first convert it to `String` and then into a `Text` (this last conversion using the `IsString` instance of `Text`). Even if there is a perfectly useful "render to text" function for the value somewhere! 

On the other hand, having a two-parameter typeclass like 
`Interpolate a s` could be annoying as well, because of the 
possible proliferation of instances that, in reality, would be doing largely isomorphic things. 🤔 `Interpolate Foo String` and `Interpolate Foo Text` should be consistent, shouldn't they?

Can we remove the `String` overhead caused by `Interpolate a` in another way? Perhaps with GHC [rewrite rules](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/rewrite_rules.html)!

The idea is that, if we have a type `Foo` with functions `fooToText` and `fooToString`, we can tell GHC to rewrite occurrences of `Data.Text.pack (fooToString foo)` into `fooToText foo`! But, will it work? 😲

# Without the rewrite rule

The `Foo` value is converted to `String` and then `pack`ed again. A wasteful detour.

```
   (text-2.1.2-ecdb:Data.Text.Show.$fShowText_$cshow
      (case ipv3 of { ghc-prim:GHC.Types.I# x ->
       case ghc-prim:GHC.Prim.># x 5# of {
         __DEFAULT -> Main.main2 Main.fooToString3;
         1# -> Main.main2 Main.fooToString1
       }
       }))

...

Main.main2 :: String -> Text
 Main.main2
   = \ (arg :: String) ->
       case text-2.1.2-ecdb:Data.Text.Show.$wunpackCStringAscii#
              Main.main4
       of
       { (# ww, ww1, ww2 #) ->
       case Data.Text.pack arg of { Data.Text.Internal.Text ww4 ww5 ww6 ->
       case text-2.1.2-ecdb:Data.Text.Show.$wunpackCStringAscii#
              Main.main3
       of
       { (# ww7, ww8, ww9 #) ->
       case Data.Text.empty of { Data.Text.Internal.Text ww3 ww10 ww11 ->
       case Data.Text.Internal.$wappend ww7 ww8 ww9 ww3 ww10 ww11 of
       { (# ww12, ww13, ww14 #) ->
       case Data.Text.Internal.$wappend ww4 ww5 ww6 ww12 ww13 ww14 of
       { (# ww15, ww16, ww17 #) ->
       case Data.Text.Internal.$wappend ww ww1 ww2 ww15 ww16 ww17 of
       { (# ww18, ww19, ww20 #) ->
       Data.Text.Internal.Text ww18 ww19 ww20
       }
       }
       }
       }
       }
       }
       }
```

# When the rule fires

We detect that we are packing after unpacking, and we remove the detour, instead rendering directly to `Text`:

```
Rule fired
   Rule: noIntermediateStringForText
   Module: (Main)
   Before: Data.Text.Internal.pack ValArg Main.fooToString a
   After:  (\ (foo :: Main.Foo) -> Main.fooToText foo) a
   Cont:   Stop[BoringCtxt] Data.Text.Internal.Text
```

Relevant part of the generated core (note the invocations to `fooToText1` and `fooToText2`):

```
    join {
       $j
         :: ghc-prim:GHC.Prim.ByteArray#
            -> ghc-prim:GHC.Prim.Int# -> ghc-prim:GHC.Prim.Int# -> Text
       $j (ww4 :: ghc-prim:GHC.Prim.ByteArray#)
          (ww5 :: ghc-prim:GHC.Prim.Int#)
          (ww6 :: ghc-prim:GHC.Prim.Int#)
         = case text-2.1.2-ecdb:Data.Text.Show.$wunpackCStringAscii#
                  Main.main2
           of
           { (# ww7, ww8, ww9 #) ->
           case Data.Text.empty of { Data.Text.Internal.Text ww3 ww10 ww11 ->
           case Data.Text.Internal.$wappend ww7 ww8 ww9 ww3 ww10 ww11 of
           { (# ww12, ww13, ww14 #) ->
           case Data.Text.Internal.$wappend ww4 ww5 ww6 ww12 ww13 ww14 of
           { (# ww15, ww16, ww17 #) ->
           case Data.Text.Internal.$wappend ww ww1 ww2 ww15 ww16 ww17 of
           { (# ww18, ww19, ww20 #) ->
           Data.Text.Internal.Text ww18 ww19 ww20
           }
           }
           }
           }
           } } in
     case ghc-prim:GHC.Prim.># x 5# of {
       __DEFAULT ->
         case Main.fooToText2 of { Data.Text.Internal.Text ww4 ww5 ww6 ->
         jump $j ww4 ww5 ww6
         };
       1# ->
         case Main.fooToText1 of { Data.Text.Internal.Text ww4 ww5 ww6 ->
         jump $j ww4 ww5 ww6
         }
     }
```