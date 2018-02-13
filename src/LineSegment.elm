module LineSegment exposing (LineSegment, affineTransform, draw, moveTo, moveBy, scaleBy)

{-| This module exposes the lineSegment type. Values of this type
can be manipulated mathematically and rendered in to SVG.


# API

@docs LineSegment, affineTransform, draw, moveTo, moveBy, scaleBy

-}

import Affine
import ColorRecord exposing (..)
import Svg as S exposing (..)
import Svg.Attributes exposing (..)
import Vector exposing (Vector)


{-| A record which models a line segment with
endpoints a and b.
-}
type alias LineSegment =
    { a : Vector
    , b : Vector
    , width : Float
    , strokeColor : ColorRecord
    , fillColor : ColorRecord
    }


{-| Apply an affine transform to a lineSegment.
-}
affineTransform : Affine.Coefficients -> LineSegment -> LineSegment
affineTransform coefficients lineSegment =
    let
        newA =
            Affine.affineTransform coefficients lineSegment.a

        newB =
            Affine.affineTransform coefficients lineSegment.b
    in
        { lineSegment | a = newA, b = newB }


{-| Produce an SVG representation of a lineSegment.
-}
draw : LineSegment -> S.Svg msg
draw segment =
    S.line (lineAttributes segment) []


{-| Move a lineSegment to a new position: translante
it so that its first endpoint is at the given position.
-}
moveTo : Vector -> LineSegment -> LineSegment
moveTo position lineSegment =
    let
        displacement =
            Vector.sub lineSegment.b lineSegment.a

        newB =
            Vector.add displacement position
    in
        { lineSegment | a = position, b = newB }


{-| Translate a lineSegment.
-}
moveBy : Vector -> LineSegment -> LineSegment
moveBy displacement lineSegment =
    let
        newA =
            Vector.add displacement lineSegment.a

        newB =
            Vector.add displacement lineSegment.b
    in
        { lineSegment | a = newA, b = newB }


{-| Scale a line segment by a factor. That is,
dilate with center at the origin.
-}
scaleBy : Float -> LineSegment -> LineSegment
scaleBy factor lineSegment =
    let
        newA =
            Vector.mul factor lineSegment.a

        newB =
            Vector.mul factor lineSegment.b
    in
        { lineSegment | a = newA, b = newB }


lineAttributes : LineSegment -> List (Attribute msg)
lineAttributes lineSegment =
    [ fill (rgba lineSegment.fillColor)
    , stroke (rgba lineSegment.fillColor)
    , x1 (toString lineSegment.a.x)
    , y1 (toString lineSegment.a.y)
    , x2 (toString lineSegment.b.x)
    , y2 (toString lineSegment.b.y)
    , strokeWidth (toString lineSegment.width)
    ]
