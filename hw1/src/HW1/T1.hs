module HW1.T1
  ( Day (..),
    nextDay,
    afterDays,
    isWeekend,
    daysToParty,
  )
where

import Numeric.Natural (Natural)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday

intToDay :: Natural -> Day
intToDay i
  | i `mod` 7 == 0 = Monday
  | i `mod` 7 == 1 = Tuesday
  | i `mod` 7 == 2 = Wednesday
  | i `mod` 7 == 3 = Thursday
  | i `mod` 7 == 4 = Friday
  | i `mod` 7 == 5 = Saturday
  | otherwise = Sunday

dayToInt :: Day -> Natural
dayToInt Monday = 0
dayToInt Tuesday = 1
dayToInt Wednesday = 2
dayToInt Thursday = 3
dayToInt Friday = 4
dayToInt Saturday = 5
dayToInt Sunday = 6

-- | Returns the day that follows the day of the week given as input.
nextDay :: Day -> Day
nextDay day = intToDay (dayToInt day + 1)

-- | Returns the day of the week after a given number of days has passed.
afterDays :: Natural -> Day -> Day
afterDays i day = intToDay (dayToInt day + i)

-- | Checks if the day is on the weekend.
isWeekend :: Day -> Bool
isWeekend day = dayToInt day `div` 5 == 1

-- | Computes the number of days until the next Friday.
daysToParty :: Day -> Natural
daysToParty day = (11 - dayToInt day) `mod` 7
