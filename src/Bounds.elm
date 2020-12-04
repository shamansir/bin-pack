module Bounds exposing

    ( Bounds
    , zero
    , multiply, multiplyBy
    , shift
    )

{-| Just rectangular bounds, including position, and some useful operations on them.

# Empty bounds

@docs zero

# Operations

@docs multiply, multiplyBy, shift
-}


{-| Top-left corner position and width/height dimensions, all that simple. -}
type alias Bounds =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| Create empty bounds at `(0, 0)` -}
zero : Bounds
zero =
    { x = 0
    , y = 0
    , width = 0
    , height = 0
    }


{-| Multiply both bounds and top left position by given proportions on X and Y. -}
multiply : { x : Float, y : Float } -> Bounds -> Bounds
multiply mult bounds =
    { x = mult.x * bounds.x
    , y = mult.y * bounds.y
    , width = mult.x * bounds.width
    , height = mult.y * bounds.height
    }


{-| Multiply dimensions of the bounds by the same value, the same as `multiply n n`. -}
multiplyBy : Float -> Bounds -> Bounds
multiplyBy n =
    multiply { x = n, y = n }


{-| Shift bounds top left corner by given position. -}
shift : { a | x : Float, y : Float } -> Bounds -> Bounds
shift by bounds =
    { x = bounds.x + by.x
    , y = bounds.y + by.y
    , width = bounds.width
    , height = bounds.height
    }
