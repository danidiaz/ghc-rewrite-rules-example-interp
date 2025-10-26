
# Without the rewrite rule

# When the rule fires

```
Rule fired
   Rule: noIntermediateStringForText
   Module: (Main)
   Before: Data.Text.Internal.pack ValArg Main.fooToString a
   After:  (\ (foo :: Main.Foo) -> Main.fooToText foo) a
   Cont:   Stop[BoringCtxt] Data.Text.Internal.Text
```

Relevant part of the generated core (note the invotations to `fooToText1` and `fooToText2`):

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