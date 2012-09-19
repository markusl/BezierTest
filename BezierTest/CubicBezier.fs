﻿module CubicBezier

/// Create bezier curve from p1 to p4, using the control points p2, p3
/// Returns the requested number of segments
let cubic_bezier (p1:vector) (p2:vector) (p3:vector) (p4:vector) segments =
    ([0 .. segments - 1]
        |> List.map(fun i ->
            let t = float i / float segments
            let a =           (1. - t) ** 3.
            let b = 3. * t * ((1. - t) ** 2.)
            let c = 3. * 1. * (t ** 2.) * (1. - t)
            let d =            t ** 3.
            let x = a * p1.[0] + b * p2.[0] + c * p3.[0] + d * p4.[0]
            let y = a * p1.[1] + b * p2.[1] + c * p3.[1] + d * p4.[1]
            vector [x; y])) @ [p4]

/// Create hermite curve from p1 to p4, using the control points p2, p3
/// Returns the requested number of segments
let hermite (p1:vector) (p2:vector) (p3:vector) (p4:vector) segments =
    [0 .. segments - 1]
        |> List.map(fun i ->
            let t = float i / float segments
            // Hermite algorithm in expanded form
            let a = +2. * (t ** 3.) - 3. * (t ** 2.) + 1.
            let b =       (t ** 3.) - (2. * (t ** 2.)) + t
            let c = -2. * (t ** 3.) + 3. * (t ** 2.)
            let d =       (t ** 3.) - (t ** 2.)
            let x = a * p1.[0] + b * p2.[0] + c * p3.[0] + d * p4.[0]
            let y = a * p1.[1] + b * p2.[1] + c * p3.[1] + d * p4.[1]
            vector [x; y])
