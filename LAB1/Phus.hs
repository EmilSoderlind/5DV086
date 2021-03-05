module Phus where
import Data.List

type ParkingInfo = (String,Bool,(Integer,Integer))
type PTimeInfo = (String,(Integer,Integer))
type TimeInfo = (Integer,Integer)

-- Helper functions
regOfCar :: ParkingInfo -> String
regOfCar (x, _, _) = x

minutesParkingOfCar :: ParkingInfo -> Integer
minutesParkingOfCar (_, _, (h,m)) = ((h*60) + m)

entranceOfCar :: ParkingInfo -> Bool
entranceOfCar (_, b, _) = b

-- Does summariseParkingTime for one car
summariseParkingTimeHelper :: ParkingInfo -> [ParkingInfo] -> Integer
summariseParkingTimeHelper currentCar [] = 0 -- No more cars to compare to
summariseParkingTimeHelper currentCar (x:xs) = 
  if not (entranceOfCar currentCar) -- Comparing the exit tupple -> Return 0
    then 0
    else 
      if ((regOfCar currentCar) == regOfCar x) -- Foxund the matching Exit-Parking
      then ((((minutesParkingOfCar x) - (minutesParkingOfCar currentCar))))
      else summariseParkingTimeHelper currentCar xs

-- Summarise parking time for all cars 
summariseParkingTime :: [ParkingInfo] -> [(String,Integer)]
summariseParkingTime [] = []
summariseParkingTime (x:xs) = ((regOfCar x), summariseParkingTimeHelper x xs) : summariseParkingTime xs

workingCarTimes :: [ParkingInfo] -> [(String, Integer)]
workingCarTimes day = [(reg, num) | (reg,num) <- summariseParkingTime day, num > 0]

-- If we've found more than 1 entrance+exit of a single car. We sum these together
addSeveralVisitsHelper :: (String, Integer) -> [(String, Integer)] -> Integer
addSeveralVisitsHelper currentCar [] = snd currentCar
addSeveralVisitsHelper currentCar (x:xs) =
  if((fst currentCar) == fst x)
  then ((((snd x) + (0)))) + addSeveralVisitsHelper currentCar xs
  else addSeveralVisitsHelper currentCar xs

addSeveralVisits :: [(String, Integer)] -> [(String, Integer)]
addSeveralVisits [] = []
addSeveralVisits (x:xs) = ((fst x), (addSeveralVisitsHelper x xs)) : addSeveralVisits xs

-- Duplicates occur when there is more than 1 entrance+exit of a single car. Removing those.
fixDuplicates :: [(String, Integer)] -> [(String, Integer)]
fixDuplicates xs = nubBy removeSmallestEntry xs
   where removeSmallestEntry ys zs = (fst ys == fst zs) && (snd ys > snd zs)

-- Finding the car that have parked the longest
longestParkedCar :: (String, Integer) -> [(String, Integer)] -> String
longestParkedCar (maxReg,maxParkTime) [] = maxReg 
longestParkedCar (maxReg,maxParkTime) (x:xs) = 
  if (snd x) > maxParkTime
  then longestParkedCar x xs
  else longestParkedCar (maxReg,maxParkTime) xs

-- Converting minutes -> (hours,minutes)
minutesToTimeFormat :: (String, Integer) -> (String, (Integer, Integer))
minutesToTimeFormat (reg, minutes) = (reg, ((minutes `div` 60), (((minutes `mod` 60)))))

fixTimeFormats day = [minutesToTimeFormat x | x <- (fixDuplicates (addSeveralVisits (workingCarTimes day)))]

phusHelper day = (longestParkedCar ("",0) (fixDuplicates (addSeveralVisits (workingCarTimes day))) , fixTimeFormats day)

phus :: [ParkingInfo] -> (String, [(String, TimeInfo)])
phus day = phusHelper day
