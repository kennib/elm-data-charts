import Chart
import Chart.Data as Data

import Html exposing (Html)
import Html.Events
import Http
import Task exposing (Task)
import Result exposing (Result(..))
import Svg exposing (Svg)
import Csv exposing (Csv)
import List.Extra as List
import String

main : Signal Html
main = Signal.map3 view
    divisions
    division.signal
    percentage

view : Result String (List String) -> Maybe String -> Maybe Float -> Html
view divisions division' percentage = Html.div []
    [ case divisions of
        Ok divisions -> Html.div []
            [ Html.select [Html.Events.on "change" Html.Events.targetValue (Signal.message division.address << Just)]
                <| [Html.option [] [Html.text  "Pick a division"]]
                ++ List.map (\division -> Html.option [] [Html.text division]) divisions
            ]
        Err error ->
            Html.p [] [Html.text error]
    , case (division', percentage) of
        (Just division, Just percentage) -> chart division percentage
        (Nothing, _) -> Html.p [] [Html.text "Choose a division"]
        (Just _, Nothing) -> Html.p [] [Html.text "Data is missing"]
    ]

chart : String -> Float -> Svg
chart label percentage = Chart.gauge
    Chart.defaultLayout
    (Data.categoricalString label)
    (Data.float 0, Data.float 100)
    (Data.float percentage)

division : Signal.Mailbox (Maybe String)
division = Signal.mailbox Nothing

data : Signal (Maybe (List (List String)))
data = Signal.map (Result.toMaybe << Result.map (.records)) 
    <| results.signal

divisionData : Signal (Maybe (List String))
divisionData = Signal.map2 (\division data ->
    case (division, data) of
        (Just division, Just data) -> List.find (List.member division) data
        _ -> Nothing)
    division.signal data

percentage : Signal (Maybe Float)
percentage = Signal.map (\divisionData ->
    divisionData
    `Maybe.andThen` (\data -> List.getAt data 5)
    `Maybe.andThen` (Result.toMaybe << String.toFloat))
    <| divisionData

divisions : Signal (Result String (List String))
divisions = Signal.map (Result.map <| (.records) >> List.filterMap List.head)
    <| results.signal

results : Signal.Mailbox (Result String Csv)
results = Signal.mailbox (Err "Results haven't loaded yet")

port electionData : Signal (Task () ())
port electionData = Signal.constant electionRequest
    |> Signal.map (\task -> (task |> Task.mapError toString |> Task.map Csv.parse |> Task.toResult)
        `Task.andThen` Signal.send results.address)

electionRequest : Task Http.Error String
electionRequest = Http.getString "http://results.aec.gov.au/17496/Website/Downloads/HouseTppByDivisionDownload-17496.csv"
    |> Task.map (String.lines >> List.tail >> Maybe.withDefault [] >> String.join "\n")
