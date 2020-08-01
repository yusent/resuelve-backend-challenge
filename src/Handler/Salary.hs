module Handler.Salary where

import Import (Handler, Value, requireCheckJsonBody, toJSON)
import Model.Team (fillCompleteSalaries)

postPlayersCompleteSalaryR :: Handler Value
postPlayersCompleteSalaryR = toJSON . fillCompleteSalaries <$> requireCheckJsonBody
