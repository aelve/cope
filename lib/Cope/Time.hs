-- | Utilities for working with time.
module Cope.Time
(
  showTimeDiff,
)
where


import Imports


-- | Show a time diff with better accuracy than 'diffF'.
--
--   * Seconds: *5s”, “1m 10s”, “5m 23s”
--   * Minutes: “10m”, “1h 37m”, “8h 51m”
--   * Hours: “10h”, “1d 3h”, “6d 20h”
--   * Days: “7d”, “20d”, “100d”
--
-- Adds a “-” for negative diffs.
showTimeDiff :: RealFrac a => a -> Text
showTimeDiff x
  | x < 0     = "-" <> showTimeDiff (-x)     -- negative
  | mm == 0   = format "{}s" s               -- < 1 minute
  | mm < 10   = format "{}m {}s" m s         -- < 10 minutes
  | hh == 0   = format "{}m" m               -- < 1 hour
  | hh < 10   = format "{}h {}m" h m         -- < 10 hours
  | dd == 0   = format "{}h" h               -- < 1 day
  | dd < 7    = format "{}d {}h" d h         -- < 7 days
  | otherwise = format "{}d" d               -- any number of days
  where
    ss = round x :: Integer
    
    (mm, s) = divMod ss 60   -- full minutes, seconds
    (hh, m) = divMod mm 60   -- full hours, minutes
    (dd, h) = divMod hh 24   -- full days, hours
    d       = dd             -- days (= full days)
