module Ch5 where

data TimeMachine = TM { manufactuer :: String, year :: Integer } deriving (Eq, Show)

-- get all the time machines from a year n on

timeMachinesFrom :: String -> Integer -> [TimeMachine]
timeMachinesFrom mf y = TM mf y : timeMachinesFrom mf (y + 1)

-- interesting in place destructing
-- find (\(TM { year = y }) -> y > 2018) timelyIncMachines
timelyIncMachines :: [TimeMachine]
timelyIncMachines = timeMachinesFrom "Timely Inc." 100
