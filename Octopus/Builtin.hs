module Octopus.Builtin where

import Data.Symbol

combineF = intern "__car__"
combineX = intern "__cdr__"

closureBody = intern "__ast__"
closureEnv = intern "__env__"
closureArg = intern "__arg__"

thunkBody = closureBody
thunkEnv = intern "__caller__"