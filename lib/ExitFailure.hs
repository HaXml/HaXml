module ExitFailure where

import System
exitFailure = exitWith (ExitFailure (-1))
