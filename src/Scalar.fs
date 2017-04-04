module OpenSolid.Scalar

type Scalar =
    static member equalWithin tolerance firstValue secondValue =
        abs (secondValue - firstValue) <= tolerance

    static member interpolateFrom firstValue secondValue parameter =
        if parameter <= 0.5 then
            firstValue + parameter * (secondValue - firstValue)
        else
            secondValue + (1.0 - parameter) * (firstValue - secondValue)
