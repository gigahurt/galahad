module Main exposing (..)

import AvalonModel exposing (..)
import List
import Maybe
import Html exposing (Html, button, div, text, ul, li, h3)
import Html.Events exposing (onClick)
import Html.Attributes exposing (attribute)
import Tuple
import Maybe
import Set exposing (Set)


main =
    Html.beginnerProgram
        { model = model 
        , view = view
        , update = update
        }


-- UPDATE

type Msg
    = Finish View
    | AddPlayer String
    | RemovePlayer String
    | AddGoodRole String
    | AddEvilRole String
    | RemoveGoodRole String
    | RemoveEvilRole String
    | AddEvilPlayer String
    | RemoveEvilPlayer String
    | AddEvilPlayerMerlinSees String
    | RemoveEvilPlayerMerlinSees String
    | SetMerlin String
    | RemoveMerlin String
    | SetPercival String
    | RemovePercival String
    | AddPlayerPercivalSees String
    | RemovePlayerPercivalSees String
    | NominatePlayer String
    | CancelNominatePlayer String
    | VoteReject String
    | VoteApprove String
    | SetFails Int


diffList : List comparable -> List comparable -> List comparable
diffList a b =
    let
        setA =
            Set.fromList a

        setB =
            Set.fromList b
    in
        Set.diff setA setB |> Set.toList


getActiveQuest : List Quest -> Int -> Maybe Quest
getActiveQuest quests players =
    quests 
        |> List.filter (\n -> n.players == players)
        |> List.head


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddPlayer player ->
            { model | players = player :: model.players }

        RemovePlayer player ->
            { model | players = List.filter (\n -> n /= player) (model.players) }

        AddGoodRole role ->
            { model | goodRolesInPlay = role :: model.goodRolesInPlay }

        AddEvilRole role ->
            { model | evilRolesInPlay = role :: model.evilRolesInPlay }

        AddEvilPlayer player ->
            { model | evilPlayers = player :: model.evilPlayers }

        AddPlayerPercivalSees player ->
            { model | playersPercivalSees = player :: model.playersPercivalSees }

        AddEvilPlayerMerlinSees player ->
            { model | evilMerlinSees = player :: model.evilMerlinSees }

        RemoveGoodRole role ->
            { model | goodRolesInPlay = List.filter (\n -> n /= role) model.goodRolesInPlay }

        RemoveEvilRole role ->
            { model | evilRolesInPlay = List.filter (\n -> n /= role) model.evilRolesInPlay }

        RemoveEvilPlayer player ->
            { model | evilPlayers = List.filter (\n -> n /= player) model.evilPlayers }

        RemoveEvilPlayerMerlinSees player ->
            { model | evilMerlinSees = List.filter (\n -> n /= player) model.evilMerlinSees }

        RemovePlayerPercivalSees player ->
            { model | playersPercivalSees = List.filter (\n -> n /= player) model.playersPercivalSees }

        SetMerlin player ->
            { model | merlin = Just player }

        RemoveMerlin player ->
            { model | merlin = Nothing }

        SetPercival player ->
            { model | percival = Just player }

        RemovePercival player ->
            { model | percival = Nothing }

        NominatePlayer player ->
            { model | nominated = player :: model.nominated }

        CancelNominatePlayer player ->
            { model | nominated = List.filter (\n -> n /= player) model.nominated }

        VoteReject player ->
            { model | rejected = player :: model.rejected }

        VoteApprove player ->
            { model | rejected = List.filter (\n -> n /= player) model.rejected }
 
        SetFails fails ->
            { model | missionFails = fails }

        Finish view ->
            case view of
                PlayerSelection ->
                    { model | view = RoleSelection, activeQuest = getActiveQuest quests (List.length model.players) }

                RoleSelection ->
                    { model | view = EvilPlayerReveal }

                EvilPlayerReveal ->
                    if List.member "Merlin" model.goodRolesInPlay then
                        { model | view = MerlinSeesEvil }
                    else
                        update (Finish MerlinSeesEvil) model

                MerlinSeesEvil ->
                    let
                        m =
                            { model
                                | oberon = (diffList model.evilMerlinSees model.evilPlayers) |> List.head
                                , mordred =
                                    if (List.isEmpty model.evilMerlinSees |> not) then
                                        (diffList model.evilPlayers model.evilMerlinSees) |> List.head
                                    else
                                        Nothing
                            }
                    in
                        if List.member "Percival" model.goodRolesInPlay then
                            { m | view = PercivalSeesMerlin }
                        else
                            update (Finish PercivalSeesMerlin) model

                PercivalSeesMerlin ->
                    { model
                        | morgana = (diffList model.evilPlayers model.playersPercivalSees) |> List.head
                        , view = Nominate
                        } |> startGame

                Nominate ->
                    { model | view = VoteOnNominees }

                VoteOnNominees ->
                    let
                        failThreshold =
                            model.players |> List.length |> toFloat |> (/) 2 |> ceiling
                    in
                        if (List.length model.rejected) >= failThreshold then
                            { model
                                | view = MissionSummary
                                , missionStatus = Blocked
                                }
                        else
                            { model | view = MissionResults }

                MissionResults ->
                    case model.activeMission of 
                        Just mission ->
                            { model 
                                | view = MissionSummary
                                , missionStatus = if model.missionFails < mission.fails then Success else Failure } 

                        Nothing -> 
                            model

                MissionSummary ->
                    { model
                        | view = Nominate
                        } 
                    |> saveCurrentMission
                    |> initializeMissionState
                    |> nextMission

                _ ->
                    model

saveCurrentMission: Model -> Model
saveCurrentMission model = 
    { model
        | completeRounds = 
            { nominated = model.nominated,
                rejected = model.rejected, 
                missionFails = model.missionFails, 
                missionStatus = model.missionStatus } :: model.completeRounds }

initializeMissionState: Model -> Model
initializeMissionState model = 
    { model 
        | nominated = []
        , rejected = [] 
        , missionFails = 0
        , missionStatus = InProgress }

nextMission : Model -> Model
nextMission model = 
    case model.activeQuest of 
        Just quest ->
            if List.isEmpty quest.missions then
                { model 
                    | activeMission = Nothing
                    , view = AssassinationAttempt }
            else
                { model 
                    | activeMission = List.head quest.missions
                    , activeQuest = Just { quest | missions = Maybe.withDefault [] (List.tail quest.missions)}}
        
        Nothing -> 
            model

startGame : Model -> Model
startGame model =
    { model
        | activeMission =
            case model.activeQuest of
                Just quest ->
                    List.head quest.missions

                Nothing ->
                    Nothing
    }
    


-- VIEW


choose label from selected maxSelected selectAction unselectAction =
    let
        maxReached =
            (List.length selected) == maxSelected

        sortedFrom =
            List.sort from
    in
        div []
            [ h3 [] [ text label ]
            , ul []
                (List.map
                    (\n ->
                        let
                            slctd =
                                List.member n selected
                        in
                            li []
                                [ if slctd then
                                    button
                                        [ onClick (unselectAction n)
                                        , attribute "class" "selected"
                                        , attribute "style" "border: 1px solid red"
                                        ]
                                        [ text n ]
                                  else
                                    button
                                        [ onClick (selectAction n)
                                        , attribute
                                            (if maxReached then
                                                "disabled"
                                             else
                                                "enabled"
                                            )
                                            ""
                                        ]
                                        [ text n ]
                                ]
                    )
                    sortedFrom
                )
            ]


filterMaybe : Maybe String -> (String -> Bool)
filterMaybe string =
    case string of
        Just s ->
            \n -> n /= s

        Nothing ->
            \n -> True


percivalCouldBe : Model -> List String
percivalCouldBe model =
    merlinCouldBe model
        |> List.filter (filterMaybe model.merlin)


merlinCouldBe : Model -> List String
merlinCouldBe model =
    diffList model.players model.evilPlayers
        |> List.filter (filterMaybe model.oberon)


view : Model -> Html Msg
view model =
    case model.view of
        PlayerSelection ->
            div []
                [ choose "Choose Players" model.pastPlayers model.players 10 AddPlayer RemovePlayer
                , div []
                    [ button
                        [ onClick (Finish PlayerSelection)
                        , attribute
                            (if List.length model.players < 5 then
                                "disabled"
                             else
                                "enabled"
                            )
                            ""
                        ]
                        [ text "Role Selection" ]
                    ]
                ]

        RoleSelection ->
            case model.activeQuest of
                Just quest ->
                    let
                        maxGood =
                            quest.players - quest.evilPlayers

                        maxEvil =
                            quest.evilPlayers
                    in
                        div []
                            [ choose "Choose Good Roles" specialGoodRolesAvailable model.goodRolesInPlay maxGood AddGoodRole RemoveGoodRole
                            , choose "Choose Evil Roles" specialEvilRolesAvailable model.evilRolesInPlay maxEvil AddEvilRole RemoveEvilRole
                            , button [ onClick (Finish RoleSelection) ] [ text "Night Phase" ]
                            ]

                _ ->
                    div [] [ text "Error" ]

        EvilPlayerReveal ->
            case model.activeQuest of
                Just quest ->
                    div []
                        [ choose "Choose Evil Players" model.players model.evilPlayers quest.evilPlayers AddEvilPlayer RemoveEvilPlayer
                        , button [ onClick (Finish EvilPlayerReveal) ] [ text "Merlin Sees Evil" ]
                        ]

                Nothing ->
                    div [] [ text "Error" ]

        MerlinSeesEvil ->
            case model.activeQuest of
                Just quest ->
                    let
                        possibleMerlin =
                            diffList model.players model.evilPlayers
                    in
                        div []
                            [ choose "Choose Evil Merlin Sees" (List.filter (filterMaybe model.merlin) model.players) model.evilMerlinSees quest.evilPlayers AddEvilPlayerMerlinSees RemoveEvilPlayerMerlinSees
                            , case model.merlin of
                                Just player ->
                                    choose "Choose Merlin" possibleMerlin [ player ] 1 SetMerlin RemoveMerlin

                                Nothing ->
                                    choose "Choose Merlin" possibleMerlin [] 1 SetMerlin RemoveMerlin
                            , button [ onClick (Finish MerlinSeesEvil) ] [ text "Percival Sees Merlin" ]
                            ]

                Nothing ->
                    div [] [ text "Error" ]

        PercivalSeesMerlin ->
            div []
                [ choose "Choose players Percival sees" model.players model.playersPercivalSees 2 AddPlayerPercivalSees RemovePlayerPercivalSees
                , case model.percival of
                    Just player ->
                        choose "Choose Percival" (percivalCouldBe model) [ player ] 1 SetPercival RemovePercival

                    Nothing ->
                        choose "Choose Percival" (percivalCouldBe model) [] 1 SetPercival RemovePercival
                , button [ onClick (Finish PercivalSeesMerlin) ] [ text "Start Game" ]
                ]

        Nominate ->
            case model.activeMission of
                Just mission ->
                    div []
                        [ choose "Nominate players for quest" model.players model.nominated mission.players NominatePlayer CancelNominatePlayer
                        , button [ onClick (Finish Nominate) ] [ text "Nominate" ]
                        ]

                Nothing ->
                    div [] [ text "Error" ]

        VoteOnNominees ->
            div []
                [ choose "Who rejected?" model.players model.rejected (List.length model.players) VoteReject VoteApprove
                , button [ onClick (Finish VoteOnNominees) ] [ text "Go on Quest" ]
                ]

        MissionResults ->
            div []
                [ text "How many fails?"
                , div [] (List.map (\n -> button [ onClick (SetFails n) ] [ text (toString n) ]) (List.range 0 5)) -- TODO max fails should be programmatic
                , button [ onClick (Finish MissionResults) ] [ text "Mission Summary" ]
                ]

        MissionSummary ->
            case model.activeMission of
                Just mission ->
                    ul [] [ li [] [text (model.missionStatus |> toString)]
                          , li [] [text (model.missionFails |> toString)]
                          , li [] [text (model.rejected |> toString)]
                          , button [onClick (Finish MissionSummary)] [text "Finish Mission "]
                          ]

                Nothing ->
                    div [] [ text "error" ]

        _ ->
            div [] [ text (model.view |> toString |> (++) "Unbuilt view ") ]
