import Text.Read (readMaybe)

-- magnitude calculates the magnitude (length) of a vector (horizontal, vertical)
magnitude :: (Double, Double) -> Double
magnitude (horizontal, vertical) = sqrt (horizontal ^ 2 + vertical ^ 2)

-- radToDeg converts an angle in radians to degrees
radToDeg :: Double -> Double
radToDeg radians = radians * (180 / pi)

-- direction calculates the direction (angle) of a vector (horizontal, vertical) from the positive x-axis
-- in radians, in the range [-pi, pi]
direction :: (Double, Double) -> Double
direction (horizontal, vertical)
  | horizontal > 0 && vertical >= 0 = atan (vertical / horizontal)
  | horizontal > 0 && vertical < 0 = atan (vertical / horizontal) + 2 * pi
  | horizontal < 0 = atan (vertical / horizontal) + pi
  | horizontal == 0 && vertical > 0 = pi / 2
  | horizontal == 0 && vertical < 0 = 3 * pi / 2
  | otherwise = 0

main :: IO ()
main = do
  putStrLn "Enter the horizontal and vertical components of the first vector:"
  horizontalStr <- getLine
  verticalStr <- getLine
  let horizontalMaybe = readMaybe horizontalStr :: Maybe Double
      verticalMaybe = readMaybe verticalStr :: Maybe Double

  -- Check if both components of the first vector are valid
  case (horizontalMaybe, verticalMaybe) of
    (Just horizontal, Just vertical) -> do
      putStrLn "Enter the horizontal and vertical components of the second vector:"
      horizontal2Str <- getLine
      vertical2Str <- getLine
      let horizontal2Maybe = readMaybe horizontal2Str :: Maybe Double
          vertical2Maybe = readMaybe vertical2Str :: Maybe Double

      -- Check if both components of the second vector are valid
      case (horizontal2Maybe, vertical2Maybe) of
        (Just horizontal2, Just vertical2) -> do
          -- Calculate the resultant vector by adding components
          let resultantVector = (horizontal + horizontal2, vertical + vertical2)
              mag = magnitude resultantVector
              dir = direction resultantVector
          putStrLn "Do you want the direction in degrees or radians? (Enter 'd' or 'r')"
          unitChoice <- getLine
          -- Output the magnitude and direction based on user choice
          case unitChoice of
            "d" -> putStrLn $ "The magnitude of the resulting vector is: " ++ show mag ++ "\nThe direction of the resulting vector from the positive x-axis is: " ++ show (radToDeg dir) ++ " degrees"
            "r" -> putStrLn $ "The magnitude of the resulting vector is: " ++ show mag ++ "\nThe direction of the resulting vector from the positive x-axis is: " ++ show dir ++ " radians"
            _ -> putStrLn "Invalid choice. Please enter 'd' for degrees or 'r' for radians."
        _ -> putStrLn "Invalid input for the second vector."
    _ -> putStrLn "Invalid input for the first vector."
