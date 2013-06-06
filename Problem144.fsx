let nearlyEqual x y = 
    abs(x - y) < 0.0001 // good enough for govt work.

[<StructuredFormatDisplay("{format}")>]
type Point(x:float, y:float) =
    member this.X = x
    member this.Y = y
    member this.format = sprintf "(%g, %g)" this.X this.Y
    member this.Equals (other:Point) =
        let xEquals = nearlyEqual this.X other.X
        let yEquals = nearlyEqual this.Y other.Y
        xEquals && yEquals

[<StructuredFormatDisplay("{format}")>]
type Line(slope:float, intercept:float) = 
    member this.Slope = slope
    member this.Intercept = intercept
    member this.format = sprintf "y = %gx + %g" this.Slope this.Intercept      
    
    static member FromPointSlope(point:Point, slope:float) =
        let intercept = point.Y - (slope * point.X)
        Line(slope, intercept)

    static member FromTwoPoints(first:Point, second:Point) =
        let slope = (second.Y - first.Y) / (second.X - first.X)
        Line.FromPointSlope(first, slope)

    member this.Intersect(other:Line) = 
        let x = (this.Intercept - other.Intercept) / (other.Slope - this.Slope)
        let y = this.Slope * x + this.Intercept;
        Point(x,y)

    // Finds the line that is the reflection of this line around some mirror
    member this.Reflect(mirror:Line) =
        //let slope = (2.0 * normal.Slope) - this.Slope
        let intersect = this.Intersect(mirror)
        // see http://math.stackexchange.com/questions/67024/the-equation-of-a-line-reflected-about-another-line
        let k2 = mirror.Slope
        let k3 = this.Slope
        // solve for k1 http://www.wolframalpha.com/input/?i=solve+%28b+-+a%29+%2F+%281%2Bb*a%29+%3D+%28c-b%29+%2F+%281%2Bc*b%29+for+a
        let k1 = ((-1.0*k2*k2*k3) - (2.0*k2) + k3) / ((k2*k2) - (2.0*k2*k3) - 1.0)
        Line.FromPointSlope(intersect, k1)

// Ellipse in (x/a)^2 + (y/b)^2 = 1
[<StructuredFormatDisplay("{format}")>]
type Ellipse(a:float, b:float) =
    member this.A = a
    member this.B = b
    member this.format = sprintf "(x/%g)^2 + (y/%g)^2 = 1" this.A this.B
    
    // See: http://www.ambrsoft.com/TrigoCalc/Circles2/Ellipse/EllipseLine.htm
    member this.Intersect(line:Line) =
        let m = line.Slope
        let c = line.Intercept
        let denom = (a * a * m * m) + (b * b)
        let inner = sqrt(denom - (c * c))
        let x_left = -1.0 * (a * a) * m * c
        let x_right = a * b * inner
        let x1 = (x_left + x_right) / denom
        let x2 = (x_left - x_right) / denom

        let y_left = b * b * c
        let y_right = a * b * m * inner
        let y1 = (y_left + y_right) / denom
        let y2 = (y_left - y_right) / denom
        
        (Point(x1, y1), Point(x2, y2))

    member this.tangentAtPoint(point:Point) = 
        let coeff = -1.0 * ((this.B / this.A) ** 2.0)
        let slope = (coeff * point.X) / point.Y
        Line.FromPointSlope(point, slope)

    // given a line that intersects the ellipse at the given point:
    // reflects the intersecting point about the tangent at that point
    // and returns a tuple with the reflected line and the next point
    // it intersects the ellipse.
    member this.bounce(intersectingLine:Line, reflectionPoint:Point) =
        let tangent = this.tangentAtPoint(reflectionPoint)
        let reflected = intersectingLine.Reflect(tangent)
        let (first, second) = this.Intersect(reflected)
        let equalsFirst = reflectionPoint.Equals(first)
        let nextPoint = if equalsFirst then second else first
        (reflected, nextPoint)

    // Creates an infinite sequence of the points that given line will bounce off of.
    member this.bounces(intersectingLine:Line, reflectionPoint:Point) =
        Seq.unfold (fun (line, point) -> Some(point, this.bounce(line, point))) (intersectingLine, reflectionPoint)

// 4x^2 + y^2 = 100  ->  (x/5)^2 + (y/10)^2 = 1
let ellipse = Ellipse(5.0, 10.0)

let a = Point(0.0, 10.1)
let b = Point(1.4, -9.6)
let first = Line.FromTwoPoints(a, b)

let solution =
    ellipse.bounces(first, b)
    |> Seq.takeWhile (fun point -> point.Y < 0.0 || point.X > 0.01 || point.X < -0.01)
    |> Seq.length