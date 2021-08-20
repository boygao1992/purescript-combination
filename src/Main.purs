module Main where

import Prelude
import Combination.AST as Combination.AST
import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console

main :: Effect Unit
main = do
  Effect.Class.Console.logShow
    $ Combination.AST.evalExpr
        ( Combination.AST.Divide
            ( Combination.AST.Sum { from: 1, to: 3 } \i ->
                Combination.AST.Multiply
                  (Combination.AST.Binomial 3 i)
                  (Combination.AST.Binomial 37 (5 - i))
            )
            (Combination.AST.Binomial 40 5)
        )
