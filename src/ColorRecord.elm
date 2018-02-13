module ColorRecord exposing (ColorRecord, rgba, mix)


type alias ColorRecord =
    { r : Int, g : Int, b : Int, a : Float }


mix : ColorRecord -> ColorRecord -> ColorRecord
mix a b =
    let
        rr =
            intAverage a.r b.r

        gg =
            intAverage a.g b.g

        bb =
            intAverage a.b b.b

        aa =
            (a.a + b.a) / 2.0
    in
        ColorRecord rr gg bb aa


intAverage i j =
    let
        sum =
            toFloat <| i + j

        average =
            sum / 2.0
    in
        round average


rgba : ColorRecord -> String
rgba color =
    "rgba(" ++ toString color.r ++ "," ++ toString color.g ++ "," ++ toString color.b ++ "," ++ toString color.a ++ ")"
