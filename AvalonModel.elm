module AvalonModel exposing (..)

questsRaw =
    [ ( ( 5, 2 ), [ ( 2, 1 ), ( 3, 1 ), ( 2, 1 ), ( 3, 1 ), ( 3, 1 ) ] )
    , ( ( 6, 2 ), [ ( 2, 1 ), ( 3, 1 ), ( 4, 1 ), ( 3, 1 ), ( 4, 1 ) ] )
    , ( ( 7, 3 ), [ ( 2, 1 ), ( 3, 1 ), ( 3, 1 ), ( 4, 2 ), ( 4, 1 ) ] )
    , ( ( 8, 3 ), [ ( 3, 1 ), ( 4, 1 ), ( 4, 1 ), ( 5, 2 ), ( 5, 1 ) ] )
    , ( ( 9, 3 ), [ ( 3, 1 ), ( 4, 1 ), ( 4, 1 ), ( 5, 2 ), ( 5, 1 ) ] )
    , ( ( 10, 4 ), [ ( 3, 1 ), ( 4, 1 ), ( 4, 1 ), ( 5, 2 ), ( 5, 1 ) ] )
    ]

type alias Mission =
    { index : Int
    , players : Int
    , fails : Int
    }


type alias Quest =
    { players : Int
    , evilPlayers : Int
    , missions : List Mission
    }

rawQuestToQuest n =
    { players = n |> Tuple.first |> Tuple.first
    , evilPlayers = n |> Tuple.first |> Tuple.second
    , missions =
        List.indexedMap
            (\i n ->
                { index = i
                , players = Tuple.first n
                , fails = Tuple.second n
                }
            )
            (n |> Tuple.second)
    }

quests: List Quest
quests =
    List.map
        rawQuestToQuest
        questsRaw

specialGoodRolesAvailable: List String
specialGoodRolesAvailable =
    [ "Merlin", "Percival" ]

specialEvilRolesAvailable: List String
specialEvilRolesAvailable =
    [ "Mordred", "Morgana", "Oberon", "Assassin" ]



-- MODEL

type MissionStatus 
    = InProgress
    | Success
    | Failure 
    | Blocked
    | BlockedToFailure

type alias Person =
    String

type alias Round = 
    { nominated: List Person
    , rejected: List Person
    , missionFails: Int
    , missionStatus: MissionStatus
    }

type alias Model =
    { pastPlayers : List Person
    , players : List Person
    , goodRolesInPlay : List String
    , evilRolesInPlay : List String
    , ladyOfTheLake : Bool
    , targeting : Bool
    , activeQuest : Maybe Quest
    , activeMission : Maybe Mission
    , evilPlayers : List Person
    , evilMerlinSees : List Person
    , merlin : Maybe Person
    , percival : Maybe Person
    , playersPercivalSees : List Person
    , mordred : Maybe Person
    , morgana : Maybe Person
    , oberon : Maybe Person
    , assassin : Maybe Person
    , nominated : List Person
    , rejected : List Person
    , missionFails : Int
    , missionStatus: MissionStatus
    , completeRounds: List Round
    , view : View
    }


model : Model
model =
    { pastPlayers = [ "Chase", "Jon", "Najeem", "Vishesh", "Jamal", "David" ]
    , players = []
    , goodRolesInPlay = []
    , evilRolesInPlay = []
    , ladyOfTheLake = False
    , targeting = False
    , activeQuest = Nothing
    , activeMission = Nothing
    , evilPlayers = []
    , evilMerlinSees = []
    , merlin = Nothing
    , percival = Nothing
    , playersPercivalSees = []
    , mordred = Nothing
    , morgana = Nothing
    , oberon = Nothing
    , assassin = Nothing
    , nominated = []
    , rejected = []
    , missionFails = 0
    , missionStatus = InProgress
    , completeRounds = []
    , view = PlayerSelection
    }

type View
    = RoleSelection
    | PlayerSelection
    | RuleSelection
    | EvilPlayerReveal
    | MerlinSeesEvil
    | PercivalSeesMerlin
    | Nominate
    | VoteOnNominees
    | MissionResults
    | AssassinationAttempt
    | ReportRoles
    | MissionSummary
    | QuestSummary