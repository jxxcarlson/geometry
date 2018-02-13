

Geometry
========

This package defines two top level geometric structures, LineSegment and Shape
that can be manipulated and rendered in to SVG.  A shape can be
a Rect or an Ellipse:


```
type Shape
    = Rect ShapeData
    | Ellipse ShapeData


type alias ShapeData =
    { center : Vector
    , dimensions : Vector
    , strokeColor : ColorRecord
    , fillColor : ColorRecord
    }
```

Here is how a typical Shape is created:

```
redColor = ColorRecord 255 0 0 1.0

ellipse = Ellipse (ShapeData (Vector 1 0) (Vector 2 2) redColor redColor)
```

One can move and rescale shapes:

```
moveTo (Vector 10 5) ellipse

scaleBy 2 ellipse
```

Here is how to render an ellipse into SVG:

```
draw ellipse
```

See the examples folder for demo code.
