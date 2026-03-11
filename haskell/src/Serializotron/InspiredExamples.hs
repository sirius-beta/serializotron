{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-missed-specialisations #-}

-- | Example data structures for testing Serializotron
-- Generic business and application domain examples
module Serializotron.InspiredExamples where

import Data.Map.Strict qualified as Map
import Data.Text (Text)
import GHC.Generics
import Serializotron
import Serializotron.Instances -- For Map, Maybe, and other instances

--------------------------------------------------------------------------------
-- Organizational Charts and Workflow Systems
--------------------------------------------------------------------------------

-- | A department or team in an organization
data Department = Department
  { deptId :: Text,
    deptName :: Text,
    reportsTo :: [Text], -- IDs of parent departments
    properties :: Map.Map Text Text
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

-- | An organizational structure with budgets and relationships
data Organization = Organization
  { departments :: Map.Map Text Department,
    budgetAllocations :: Map.Map (Text, Text) Double,
    orgMetrics :: OrgMetrics
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data OrgMetrics = OrgMetrics
  { isFlat :: Bool,
    isConnectedStructure :: Bool,
    maxHierarchyDepth :: Maybe Int,
    totalBudget :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

--------------------------------------------------------------------------------
-- Configuration and Settings Management
--------------------------------------------------------------------------------

-- | Different types of configuration parameters
data ConfigParam
  = ToggleParam Text ConfigConstraint
  | ChoiceParam Text [Text] -- name and possible options
  | RangeParam Text (Double, Double) -- name and (min, max) range
  | DerivedParam Text Formula
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ConfigConstraint
  = BooleanConstraint
  | CategoryConstraint [Text]
  | NumericConstraint Double Double -- min, max
  | TextConstraint (Maybe Int) -- max length
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Formula
  = WeightedSum [(Text, Double)] -- param name and coefficient
  | CombinationRule Operation [Text] -- operator and input parameters
  | ConditionalFormula Text Text Text -- if-param, then-param, else-param
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Operation = AndOp | OrOp | NotOp | XorOp
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

--------------------------------------------------------------------------------
-- Pricing and Scoring Tables
--------------------------------------------------------------------------------

-- | A lookup table mapping conditions to scores or prices
data ScoringTable = ScoringTable
  { tableName :: Text,
    inputFields :: [Text],
    outputFields :: [Text],
    scoringEntries :: [ScoringEntry],
    scalingFactor :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ScoringEntry = ScoringEntry
  { inputCondition :: Map.Map Text FieldValue,
    outputResult :: Map.Map Text FieldValue,
    score :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data FieldValue
  = BoolValue Bool
  | TextValue Text
  | NumValue Double
  | ListValue [FieldValue]
  deriving stock (Generic, Show, Eq, Ord)
  deriving anyclass (ToSZT, FromSZT)

--------------------------------------------------------------------------------
-- Business Process and Workflow Models
--------------------------------------------------------------------------------

-- | A business process with parameters, steps, and metrics
data BusinessProcess = BusinessProcess
  { processName :: Text,
    parameters :: Map.Map Text ConfigParam,
    steps :: [ProcessStep],
    metrics :: Map.Map Text Measurement,
    reports :: [Report],
    processStatus :: QualityCheck
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ProcessStep
  = SequentialStep Text Text Double -- from-step, to-step, duration
  | ConditionalBranch Text [Text] ScoringTable -- target, prerequisites, decision table
  | ServiceCall Text [Text] ServiceTable -- service-name, input-params, lookup table
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Measurement
  = DirectMeasurement Text FieldValue Double -- parameter, value, confidence
  | AggregatedMeasurement Text [Text] Double -- result, source-params, weight
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Report
  = SummaryReport [Text] -- parameters to summarize
  | ComparisonReport [Text] [Text] -- target params, baseline params
  | OptimizationReport [Text] Text -- control params, target metric
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data QualityCheck = QualityCheck
  { isApproved :: Bool,
    isConsistent :: Bool,
    isStreamlined :: Bool,
    qualityIssues :: [QualityIssue],
    recommendations :: [Text]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data QualityIssue
  = CircularDependency [Text] -- cycle of step names
  | InvalidMetric Text Double -- parameter, invalid score
  | MissingStep Text Text -- referenced by, missing step
  | ConflictingMeasurement Text [Text] -- parameter, conflicting measurement sources
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

-- | Service lookup table for external integrations
data ServiceTable = ServiceTable
  { serviceEntries :: Map.Map (Map.Map Text FieldValue) Double,
    defaultResponse :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

--------------------------------------------------------------------------------
-- Tournament and Competition Systems
--------------------------------------------------------------------------------

-- | A tournament or competition event
data Tournament = Tournament
  { tournamentName :: Text,
    participants :: Map.Map Text Participant,
    tournamentRules :: CompetitionRules,
    currentState :: CompetitionState,
    eventHistory :: [CompetitionEvent]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Participant = Participant
  { participantId :: Text,
    participantCategory :: ParticipantType,
    techniques :: [Technique],
    preferences :: Map.Map Text Double, -- preferences about other participants
    scoringProfile :: ServiceTable
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ParticipantType
  = ExpertLevel
  | IntermediateLevel Double -- skill parameter
  | BeginnerLevel
  | CustomLevel Text -- custom skill description
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data Technique = Technique
  { techniqueName :: Text,
    techniqueStyle :: TechniqueStyle,
    settings :: Map.Map Text Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data TechniqueStyle
  = DirectApproach [Text] -- sequence of moves
  | VariableApproach [(Text, Double)] -- move and weight pairs
  | AdaptiveApproach [(CompetitionCondition, Text)] -- condition-move pairs
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionCondition
  = ParticipantMoveCondition Text Text -- participant-id, move
  | ScoreCondition Text FieldValue -- score-category, value
  | TimeCondition [Text] -- sequence of past events
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionRules = CompetitionRules
  { orderOfPlay :: [Text], -- participant IDs
    winConditions :: [WinCondition],
    moveConstraints :: Map.Map Text [MoveConstraint],
    scoringMatrix :: ScoringMatrix
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data WinCondition
  = PointsThreshold Text Double -- participant, threshold
  | MaterialCondition Text Int -- resource type, amount
  | TeamCondition [Text] -- required team
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data MoveConstraint
  = MaxMoves Int
  | RequiredEquipment [(Text, Int)] -- equipment type, amount
  | RoundRestriction Int -- round number restriction
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data ScoringMatrix = ScoringMatrix
  { results :: Map.Map [Text] (Map.Map Text Double), -- move sequence -> participant scores
    baseScore :: Double
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionState = CompetitionState
  { activeParticipant :: Text,
    roundNumber :: Int,
    participantInventory :: Map.Map Text (Map.Map Text Int),
    publicScores :: Map.Map Text FieldValue,
    competitionPhase :: CompetitionPhase
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionPhase
  = Preparation
  | InProgress
  | Deliberation
  | Concluded CompetitionOutcome
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionOutcome
  = Champion Text
  | Tie [Text] -- tied participants
  | Cancelled
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data CompetitionEvent = CompetitionEvent
  { eventId :: Text,
    participantId :: Text,
    eventType :: Text,
    eventDetails :: Map.Map Text FieldValue,
    eventTime :: Int,
    effects :: [EventEffect]
  }
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

data EventEffect
  = InventoryChange Text Int -- item type, delta
  | ScoreUpdate Text FieldValue -- category, new value
  | ParticipantImpact Text Text Double -- target participant, impact type, magnitude
  deriving stock (Generic, Show, Eq)
  deriving anyclass (ToSZT, FromSZT)

--------------------------------------------------------------------------------
-- Example instances for testing
--------------------------------------------------------------------------------

-- | A sample organizational chart
exampleOrganization :: Organization
exampleOrganization =
  Organization
    { departments =
        Map.fromList
          [ ("eng", Department "eng" "Engineering" ["exec"] (Map.fromList [("type", "development")])),
            ("sales", Department "sales" "Sales Team" ["exec"] (Map.fromList [("type", "revenue")])),
            ("marketing", Department "marketing" "Marketing" ["sales"] (Map.fromList [("type", "outreach")])),
            ("exec", Department "exec" "Executive" [] (Map.fromList [("type", "leadership")]))
          ],
      budgetAllocations =
        Map.fromList
          [ (("exec", "eng"), 250000.0),
            (("exec", "sales"), 180000.0),
            (("sales", "marketing"), 80000.0)
          ],
      orgMetrics = OrgMetrics False True (Just 3) 510000.0
    }

-- | A sample business process
exampleBusinessProcess :: BusinessProcess
exampleBusinessProcess =
  BusinessProcess
    { processName = "Customer Onboarding Process",
      parameters =
        Map.fromList
          [ ("approval", ToggleParam "approval" (CategoryConstraint ["pending", "approved", "rejected"])),
            ("priority", ChoiceParam "priority" ["high", "medium", "low"]),
            ("score", RangeParam "score" (0, 100)),
            ("risk_level", DerivedParam "risk_level" (WeightedSum [("score", -0.5), ("priority", 0.3)]))
          ],
      steps =
        [ SequentialStep "intake" "review" 2.5,
          ConditionalBranch "approval" ["intake", "review"] (ScoringTable "approval_logic" ["score"] ["approval"] [] 1.0)
        ],
      metrics =
        Map.fromList
          [ ("completion_rate", DirectMeasurement "approval" (TextValue "approved") 0.85)
          ],
      reports =
        [ SummaryReport ["approval", "score"],
          OptimizationReport ["priority"] "completion_rate"
        ],
      processStatus = QualityCheck True True True [] []
    }

-- | A sample tournament
exampleTournament :: Tournament
exampleTournament =
  Tournament
    { tournamentName = "Regional Chess Championship",
      participants =
        Map.fromList
          [ ( "alice",
              Participant
                "alice"
                ExpertLevel
                [Technique "tactical" (DirectApproach ["opening", "middlegame"]) (Map.singleton "aggression" 0.7)]
                (Map.fromList [("bob", 0.6)])
                (ServiceTable (Map.singleton (Map.singleton "position" (TextValue "advantage")) 8.5) 5.0)
            ),
            ( "bob",
              Participant
                "bob"
                (IntermediateLevel 0.75)
                [Technique "positional" (VariableApproach [("defense", 0.6), ("attack", 0.4)]) (Map.singleton "patience" 0.8)]
                (Map.fromList [("alice", 0.4)])
                (ServiceTable (Map.singleton (Map.singleton "position" (TextValue "equal")) 5.0) 5.0)
            )
          ],
      tournamentRules =
        CompetitionRules
          ["alice", "bob"]
          [PointsThreshold "alice" 3.0, PointsThreshold "bob" 3.0]
          (Map.fromList [("alice", [MaxMoves 40]), ("bob", [MaxMoves 40])])
          (ScoringMatrix (Map.singleton ["e4", "e5"] (Map.fromList [("alice", 0.5), ("bob", 0.5)])) 0.0),
      currentState =
        CompetitionState
          "alice"
          1
          (Map.fromList [("alice", Map.singleton "time" 90), ("bob", Map.singleton "time" 90)])
          (Map.singleton "round" (NumValue 1))
          InProgress,
      eventHistory = []
    }
