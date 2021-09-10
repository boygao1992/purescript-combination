module Main where

import Prelude
import Combination.AST as Combination.AST
import Data.Array as Data.Array
import Data.Decimal as Data.Decimal
import Data.Maybe as Data.Maybe
import Data.String as Data.String
import Effect (Effect)
import Effect.Class.Console as Effect.Class.Console

main :: Effect Unit
main = do
  _1_2
  _1_3

_1_2 :: Effect Unit
_1_2 = do
  let
    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    $ printTable "n \\ m"
    $ flip map (Data.Array.range 1 10) \n ->
        flip map (Data.Array.range 1 10) \m ->
          _1_2_equation deck hand n m <> "%"

_1_3 :: Effect Unit
_1_3 = do
  let
    v :: Int
    v = 6

    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    $ printTable "n \\ u"
    $ flip map (Data.Array.range 1 10) \n ->
        flip map (Data.Array.range 1 3) \u ->
          _1_3_equation deck hand n v u <> "%"

printTable :: String -> Array (Array String) -> String
printTable legend xss =
  [ printHeader legend (Data.Maybe.maybe 0 Data.Array.length (Data.Array.head xss))
  , printBody
      $ Data.Array.zipWith Data.Array.cons
          (show <$> Data.Array.range 1 10)
      $ xss
  ]
    # Data.Array.fold

printHeader :: String -> Int -> String
printHeader legend n =
  [ printRow ([ legend ] <> (show <$> Data.Array.range 1 n))
  , printRow (Data.Array.replicate (n + 1) "---")
  ]
    # Data.Array.fold

printBody :: Array (Array String) -> String
printBody xss = Data.Array.fold (printRow <$> xss)

printRow :: Array String -> String
printRow xs =
  [ "| "
  , Data.String.joinWith " | " xs
  , " |"
  , "\n"
  ]
    # Data.Array.fold

_1_2_equation :: Int -> Int -> Int -> Int -> String
_1_2_equation deck hand n m =
  Data.Decimal.toFixed 2
    $ Combination.AST.evalExpr
        ( Combination.AST.Multiply
            (Combination.AST.Int 100)
            ( Combination.AST.Divide
                ( Combination.AST.Sum { from: 1, to: min n hand } \i ->
                    Combination.AST.Multiply
                      (Combination.AST.Binomial n i)
                      ( Combination.AST.Sum { from: 1, to: min m (hand - i) } \j ->
                          Combination.AST.Multiply
                            (Combination.AST.Binomial m j)
                            (Combination.AST.Binomial (deck - n - m) (hand - i - j))
                      )
                )
                (Combination.AST.Binomial deck hand)
            )
        )

_1_3_equation :: Int -> Int -> Int -> Int -> Int -> String
_1_3_equation deck hand n v u =
  Data.Decimal.toFixed 2
    $ Combination.AST.evalExpr
        ( Combination.AST.Multiply
            (Combination.AST.Int 100)
            ( Combination.AST.Divide
                ( Combination.AST.Sum { from: 1, to: min u hand } \i ->
                    Combination.AST.Multiply
                      (Combination.AST.Binomial u i)
                      ( Combination.AST.Multiply
                          (Combination.AST.Binomial (deck - n - u) (hand - i))
                          ( Combination.AST.Sum { from: 1, to: min n v } \j ->
                              Combination.AST.Multiply
                                (Combination.AST.Binomial n j)
                                (Combination.AST.Binomial (deck - hand - n) (v - j))
                          )
                      )
                )
                ( Combination.AST.Multiply
                    (Combination.AST.Binomial deck hand)
                    (Combination.AST.Binomial (deck - hand) v)
                )
            )
        )
