module PropLogicWorkspace where

import PropLogic
import Workspace

type PLWorkspace = DAGWorkspace Statement (WContext statement) Statement
