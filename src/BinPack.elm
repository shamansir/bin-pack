module BinPack exposing

    ( BinPack
    , container
    , pack, carelessPack, packAll
    , find
    , fold, foldGeometry
    , toList
    , map
    )

{-| Simple bin packing for rectangles.

Based on [the version in Haskell](https://github.com/bflyblue/binpack/blob/master/Data/BinPack/R2.hs).

[The demo of a similar algorithm](https://observablehq.com/@mourner/simple-rectangle-packing).

[The article on how it works](https://codeincomplete.com/articles/bin-packing/).

Create one with `container <width> <height>` and then add rectangles and values using `pack` or `carelessPack`:

    -- :: BinPack Color
    BinPack.container 300 250
        |> pack ( { width = 10, height = 30 }, Color.black )
        |> pack ( { width = 20, height = 15 }, Color.red )
        |> pack ( { width = 5, height = 25 }, Color.blue )
        |> ...

    -- :: BinPack String
    BinPack.container 300 250
        |> pack ( { width = 10, height = 30 }, "Martha" )
        |> pack ( { width = 20, height = 15 }, "Ben" )
        |> pack ( { width = 5, height = 25 }, "John" )
        |> ...

# Core type

@docs BinPack

# Bounds

@docs Bounds

# Create container

@docs container

# Packing

@docs pack, carelessPack, packAll

# Search

@docs find

# Folding

@docs fold, foldGeometry

# Lists

@docs toList

# Mapping

@docs map

-}


{-| The bounds, top left corner and width/height. -}
type alias Bounds =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


{-| `BinPack a`, where `a` is the type of what every rectangle is associated with (what lies in every cell). For example, it could be `Color`.
-}
type BinPack a
    = Node

        { width : Float
        , height : Float
        }

        { right : BinPack a
        , below : BinPack a
        }

        a

    | Free

        { width : Float
        , height : Float
        }


{-| Substitute all of the cells with different ones, using the previous ones as the source. -}
map : ( a -> b ) -> BinPack a -> BinPack b
map f bp =
    case bp of
        Node size { right, below } v ->
            Node size
                { right = map f right
                , below = map f below
                }
                (f v)
        Free size -> Free size


{-| Fold the `BinPack a` to any other type, for example:

    BinPack.container 300 250
        |> pack ( { width = 10, height = 30 }, Color.black )
        |> pack ( { width = 20, height = 15 }, Color.red )
        |> pack ( { width = 5, height = 25 }, Color.blue )
        |> fold (::) []

    -- == [ Color.black, Color.red, Color.blue ]

 -}
fold : ( a -> b -> b ) -> b -> BinPack a -> b
fold f =
    fold1
        (\bp prev ->
            case bp of
                Node _ _ v -> f v prev
                Free _ -> prev
        )


{-| Fold the `BinPack` using the information about if it's a free space or a node.
 -}
fold1 : ( BinPack a -> b -> b ) -> b -> BinPack a -> b
fold1 f i bp =
    case bp of
        Node _ { right, below } _ ->
            let
                current = f bp i
                fromRight = fold1 f current right
                fromBelow = fold1 f fromRight below
            in fromBelow
        Free _ -> f bp i


{-| Fold the structure, using both the values and their bounds:

    BinPack.container 20 100
        |> pack ( { width = 10, height = 30 }, Color.black )
        |> pack ( { width = 20, height = 15 }, Color.red )
        |> pack ( { width = 5, height = 25 }, Color.blue )
        |> pack ( { width = 12, height = 25 }, Color.green )
        |> foldGeometry (::) []

    -- ==
    --    [ ( Color.black, { x = 0, y = 0, width = 10, height = 30 } )
    --    , ( Color.red, { x = 0, y = 30, width = 20, height = 15 } )
    --    , ( Color.blue, { x = 10, y = 0, width = 5, height = 25 } )
    --    , ( Color.green, { x = 0, y = 45, width = 12, height = 25 } )
    --    ]
 -}
foldGeometry : ( ( a, Bounds ) -> k -> k ) -> k -> BinPack a -> k
foldGeometry f =
    let
        helper x y v bp =
           case bp of
               Free _ -> v
               Node r n i ->
                   f ( i,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    <| helper x (y + r.height) (helper (x + r.width) y v n.right) n.below
    in helper 0 0


{-| Fold with the information if it's a free space or a node, including bounds.
 -}
foldGeometry1 : ( ( BinPack a, Bounds ) -> k -> k ) -> k -> BinPack a -> k
foldGeometry1 f =
    let
        helper x y v bp =
           case bp of
               Free r ->
                    f ( bp,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    v
               Node r n i ->
                   f ( bp,
                        { x = x, y = y
                        , width = r.width
                        , height = r.height
                        }
                      )
                    <| helper x (y + r.height) (helper (x + r.width) y v n.right) n.below
    in helper 0 0


{-| Convert the structure to the list of values and their bounds
-}
toList : BinPack a -> List (a, Bounds)
toList = foldGeometry (::) []


{-| Convert the structure to the list of values and their bounds + information if it's a free space or a node.
-}
toList1 : BinPack a -> List (BinPack a, Bounds)
toList1 = foldGeometry1 (::) []


{-| Try to pack all the values with given dimensions in a `BinPack` container with given width and height, ignore the item when it doesn't fit.
-}
packAll : Float -> Float -> List ( { width : Float, height : Float }, a ) -> BinPack a
packAll w h = List.foldl carelessPack <| container w h


{-| Create an empty container with given height and width.
-}
container : Float -> Float -> BinPack a
container w h = Free { width = w, height = h }


{-| Create a node with given width, height, value, and other packs below and right
-}
node : Float -> Float -> BinPack a -> BinPack a -> a -> BinPack a
node w h r b a =
    Node
        { width = w
        , height = h
        }
        { right = r
        , below = b
        }
        a

{-| Try to pack the value in a rectangle with given width and height. If the rect doesn't fit, `Nothing` is returned.
-}
pack : ( { width : Float, height : Float }, a ) -> BinPack a -> Maybe (BinPack a)
pack ( rect, value ) bp =
    case bp of
        Free f ->
            if rect.width <= f.width && rect.height <= f.height
            then
                Just
                    <| let
                        pright = container (f.width - rect.width) rect.height
                        pbelow = container f.width (f.height - rect.height)
                    in node rect.width rect.height pright pbelow value
            else Nothing
        Node r n nodeValue ->
            case pack ( rect, value ) n.right of
                Just newRight ->
                    Just <| node r.width r.height newRight n.below nodeValue
                Nothing ->
                    case pack ( rect, value ) n.below of
                        Just newBelow ->
                            Just <| node r.width r.height n.right newBelow nodeValue
                        Nothing -> Nothing


{-| Try to pack the value in a rectangle with given width and height. If the rectangle doesn't fit, ignore that fact and return previous condition of `BinPack`. -}
carelessPack : ( { width : Float, height : Float }, a ) -> BinPack a -> BinPack a
carelessPack ( rect, value ) bp =
    pack ( rect, value ) bp |> Maybe.withDefault bp


{-| Try to find a value in a structure using given coordinates. -}
find : { x : Float, y : Float } -> BinPack a -> Maybe ( a, Bounds )
find pos =
    foldGeometry
        (\ ( v, bounds ) foundBefore ->
            case foundBefore of
                Just _ -> foundBefore
                Nothing ->
                    if (pos.x >= bounds.x)
                    && (pos.y >= bounds.y)
                    && (pos.x < bounds.x + bounds.width)
                    && (pos.y < bounds.y + bounds.height)
                        then Just ( v, bounds )
                        else Nothing
        )
        Nothing
