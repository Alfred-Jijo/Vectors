import Text.Read (readMaybe)

-- Calculate the magnitude of a vector (x, y)
magnitude :: (Double, Double) -> Double
magnitude (x, y) = sqrt (x ^ 2 + y ^ 2)

-- Convert radians to degrees
radToDeg :: Double -> Double
radToDeg rad = rad * (180 / pi)

-- Calculate the direction (angle) of a vector (x, y) from the positive x-axis
-- The result is in radians, in the range [-pi, pi]
direction :: (Double, Double) -> Double
direction (x, y)
  | x > 0 && y >= 0 = atan (y / x)
  | x > 0 && y < 0 = atan (y / x) + 2 * pi
  | x < 0 = atan (y / x) + pi
  | x == 0 && y > 0 = pi / 2
  | x == 0 && y < 0 = 3 * pi / 2
  | otherwise = 0

main :: IO ()
main = do
  putStrLn "Enter the x and y components of the first vector:"
  xStr <- getLine
  yStr <- getLine
  let xMaybe = readMaybe xStr :: Maybe Double
      yMaybe = readMaybe yStr :: Maybe Double

  case (xMaybe, yMaybe) of
    (Just x, Just y) -> do
      putStrLn "Enter the x and y components of the second vector:"
      x2Str <- getLine
      y2Str <- getLine
      let x2Maybe = readMaybe x2Str :: Maybe Double
          y2Maybe = readMaybe y2Str :: Maybe Double

      case (x2Maybe, y2Maybe) of
        (Just x2, Just y2) -> do
          let resultantVector = (x + x2, y + y2)
              mag = magnitude resultantVector
              dir = direction resultantVector
          putStrLn "Do you want the direction in degrees or radians? (Enter 'd' or 'r')"
          unitChoice <- getLine
          case unitChoice of
            "d" -> putStrLn $ "The magnitude of the resulting vector is: " ++ show mag ++ "\nThe direction of the resulting vector from the positive x-axis is: " ++ show (radToDeg dir) ++ " degrees"
            "r" -> putStrLn $ "The magnitude of the resulting vector is: " ++ show mag ++ "\nThe direction of the resulting vector from the positive x-axis is: " ++ show dir ++ " radians"
            _ -> putStrLn "Invalid choice. Please enter 'd' for degrees or 'r' for radians."
        _ -> putStrLn "Invalid input for the second vector."
    _ -> putStrLn "Invalid input for the first vector."