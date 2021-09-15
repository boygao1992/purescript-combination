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
  pure unit

_1_1 :: Effect Unit
_1_1 = do
  let
    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    $ print1DTable "n" "prob"
    $ flip map (Data.Array.range 1 10) \n ->
        _1_1_equation deck hand n <> "%"

_1_2 :: Effect Unit
_1_2 = do
  let
    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    $ print2DTable "n \\ m"
    $ flip map (Data.Array.range 1 10) \n ->
        flip map (Data.Array.range 1 10) \m ->
          _1_2_equation deck hand n m <> "%"

_1_3 :: Int -> Effect Unit
_1_3 v = do
  let
    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    ( [ printHeader
          ( [ [ "n \\ u" ]
            , Data.Array.range 0 3 <#> show
            ]
              # Data.Array.fold
          )
      , printBody
          $ flip map (Data.Array.range 1 10) \n ->
              [ [ [ "$", _1_1_equation deck hand n, "\\%", "$" ]
                    # Data.Array.fold
                ]
              , flip map (Data.Array.range 1 3) \u ->
                  [ "$", "+", _1_3_equation deck hand n v u, "\\%", "$" ]
                    # Data.Array.fold
              ]
                # Data.Array.fold
      ]
        # Data.Array.fold
    )

_1_4 :: Int -> Int -> Effect Unit
_1_4 v u = do
  let
    deck :: Int
    deck = 40

    hand :: Int
    hand = 5
  Effect.Class.Console.log
    $ print2DTable "n \\ m"
    $ flip map (Data.Array.range 1 10) \n ->
        flip map (Data.Array.range 1 10) \m ->
          [ "$", "+", _1_4_equation deck hand n m v u, "\\%", "$" ]
            # Data.Array.fold

print1DTable :: String -> String -> Array String -> String
print1DTable h1 h2 xs =
  [ printHeader [ h1, h2 ]
  , printBody (pure <$> xs)
  ]
    # Data.Array.fold

print2DTable :: String -> Array (Array String) -> String
print2DTable legend xss =
  [ printHeader
      ( [ [ legend ]
        , ( Data.Array.range 1
              (Data.Maybe.maybe 0 Data.Array.length (Data.Array.head xss))
              <#> show
          )
        ]
          # Data.Array.fold
      )
  , printBody xss
  ]
    # Data.Array.fold

printHeader :: Array String -> String
printHeader hs =
  [ printRow hs
  , printDashes (Data.Array.length hs)
  ]
    # Data.Array.fold

printBody :: Array (Array String) -> String
printBody xss =
  printRows
    $ Data.Array.zipWith Data.Array.cons
        (show <$> Data.Array.range 1 (Data.Array.length xss))
    $ xss

printDashes :: Int -> String
printDashes n = printRow (Data.Array.replicate n "---")

printRows :: Array (Array String) -> String
printRows xss = Data.Array.fold (printRow <$> xss)

printRow :: Array String -> String
printRow xs =
  [ "| "
  , Data.String.joinWith " | " xs
  , " |"
  , "\n"
  ]
    # Data.Array.fold

_1_1_equation :: Int -> Int -> Int -> String
_1_1_equation deck hand n =
  Data.Decimal.toFixed 2 <<< Combination.AST.evalExpr
    $ Combination.AST.Multiply
        (Combination.AST.Int 100)
        ( Combination.AST.Divide
            ( Combination.AST.Sum { from: 1, to: min n hand } \i ->
                Combination.AST.Multiply
                  (Combination.AST.Binomial n i)
                  (Combination.AST.Binomial (deck - n) (hand - i))
            )
            (Combination.AST.Binomial deck hand)
        )

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

_1_4_equation :: Int -> Int -> Int -> Int -> Int -> Int -> String
_1_4_equation deck hand n m v u =
  Data.Decimal.toFixed 2
    $ Combination.AST.evalExpr
        ( Combination.AST.Multiply
            (Combination.AST.Int 100)
            ( Combination.AST.Divide
                ( Combination.AST.Plus
                    ( Combination.AST.Sum { from: 1, to: min u hand } \i ->
                        Combination.AST.Multiply
                          (Combination.AST.Binomial u i)
                          ( Combination.AST.Sum { from: 1, to: min m (hand - i) } \j ->
                              Combination.AST.Multiply
                                (Combination.AST.Binomial m j)
                                ( Combination.AST.Multiply
                                    (Combination.AST.Binomial (deck - n - m - u) (hand - i - j))
                                    ( Combination.AST.Sum { from: 1, to: min n v } \k ->
                                        Combination.AST.Multiply
                                          (Combination.AST.Binomial n k)
                                          (Combination.AST.Binomial (deck - hand - n) (v - k))
                                    )
                                )
                          )
                    )
                    ( Combination.AST.Sum { from: 1, to: min u hand } \i ->
                        Combination.AST.Multiply
                          (Combination.AST.Binomial u i)
                          ( Combination.AST.Sum { from: 1, to: min n (hand - i) } \j ->
                              Combination.AST.Multiply
                                (Combination.AST.Binomial n j)
                                ( Combination.AST.Multiply
                                    (Combination.AST.Binomial (deck - n - m - u) (hand - i - j))
                                    ( Combination.AST.Sum { from: 1, to: min m v } \k ->
                                        Combination.AST.Multiply
                                          (Combination.AST.Binomial m k)
                                          (Combination.AST.Binomial (deck - hand - m) (v - k))
                                    )
                                )
                          )
                    )
                )
                ( Combination.AST.Multiply
                    (Combination.AST.Binomial deck hand)
                    (Combination.AST.Binomial (deck - hand) v)
                )
            )
        )
