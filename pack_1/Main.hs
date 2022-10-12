solver :: Double -> Double -> Double -> (Double, Double)
solver a b c =
  if (b * b -4 * a * c) > 0
    then ( ((-1) * b + (b * b -4 * a * c) ** 0.5) / (2 * a), ((-1) * b - (b * b -4 * a * c) ** 0.5) / (2 * a))
    else
      if (b * b -4 * a * c) == 0
        then (((-1) * b) / (2 * a), ((-1) * b) / (2 * a))
        else error "No solution"
