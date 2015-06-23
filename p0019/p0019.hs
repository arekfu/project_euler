data Date = DMY Int Int Int

instance Show Date where
        show (DMY d m y) = show d ++ " " ++ show m ++ " " ++ show y

isLeapYear :: Int -> Bool
isLeapYear y = y `mod` 4==0 && (y `mod` 100/=0 || y`mod`400==0)

monthLength :: Int -> Int -> Int
monthLength y m
        | m==1 || m==3 || m==5 || m==7 || m==8 || m==10 || m==12 = 31
        | m==4 || m==6 || m==9 || m==11 = 30
        | m==2 && isLeapYear y = 29
        | otherwise = 28

monthLengthDate :: Date -> Int
monthLengthDate (DMY _ m y) = monthLength y m

yearLength y = if isLeapYear y then 366 else 365

nextDay :: Date -> Date
nextDay (DMY d m y) = (DMY d' m' y')
        where lastDayMonth = (d == monthLength y m)
              lastDayYear = (m == 12 && lastDayMonth)
              d' = if lastDayMonth then 1 else d+1
              m' = if lastDayYear then 1 else if lastDayMonth then m+1 else m
              y' = if lastDayYear then y+1 else y

nextMonth :: Date -> Date
nextMonth (DMY d m y) = (DMY d' m' y')
        where lastMonthYear = m == 12
              d' = d
              m' = if lastMonthYear then 1 else m+1
              y' = if lastMonthYear then y+1 else y

genMonthlyDates :: Date -> [Date]
genMonthlyDates today = today : genMonthlyDates (nextMonth today)

datesToMonthLengths :: [Date] -> [Int]
datesToMonthLengths dates = map monthLengthDate dates

shiftDay d l = (d+l) `mod` 7

theDates = takeWhile (\(DMY _ _ y) -> y<=2000) $ genMonthlyDates (DMY 1 1 1901)

theLengths = datesToMonthLengths theDates

startDay = shiftDay 1 $ yearLength 1900

cumulativeLengths = scanl (+) startDay theLengths

daysOfTheWeek = map ((flip mod) 7) cumulativeLengths

nSundays = length $ filter (==0) daysOfTheWeek

