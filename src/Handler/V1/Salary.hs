module Handler.V1.Salary where

import Import (Handler, Value, requireCheckJsonBody, toJSON)
import Model.Team (fillCompleteSalaries)

postV1PlayersCompleteSalaryR :: Handler Value
postV1PlayersCompleteSalaryR = toJSON . fillCompleteSalaries <$> requireCheckJsonBody
