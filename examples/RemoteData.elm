import Chart
import Chart.Data as Data
import Chart.Data.Csv exposing ((:=))

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
main = Signal.map2 view
    division.signal
    data.signal

view : Maybe String -> Result String (List (Election)) -> Html
view selectedDivision data = 
    let
        election elections division = List.find (\election -> Data.label election.division == division) elections
    in
        Html.div []
        [ case data of
            Ok elections -> Html.div []
                [ Html.select [Html.Events.on "change" Html.Events.targetValue (Signal.message division.address << Just)]
                    <| [Html.option [] [Html.text  "Pick a division"]]
                    ++ List.map (\election -> Html.option [] [Html.text <| Data.label election.division]) elections
                , case selectedDivision `Maybe.andThen` election elections of
                    Just election -> case election.alp of
                        Just percentage -> chart election.division percentage
                        Nothing -> Html.p [] [Html.text "Results are missing"]
                    Nothing -> Html.p [] [Html.text "Election is missing"]
                ]
            Err error ->
                Html.p [] [Html.text error]
        ]

chart : Data.Categorical String -> Data.Number Float -> Svg
chart label percentage = Chart.gauge
    Chart.defaultLayout
    label
    (Data.float 0, Data.float 100)
    percentage

division : Signal.Mailbox (Maybe String)
division = Signal.mailbox Nothing

csvUrl = "https://raw.githubusercontent.com/jumpinjackie/cesium-dotnet-examples/master/ElectionCzml/2013-HouseTppByDivisionDownload-17496.csv"

port request : Signal (Task () ())
port request = Signal.constant (Chart.Data.Csv.fetch csvUrl elections)
    |> Signal.map (\task -> Task.toResult task `Task.andThen` Signal.send data.address)

data : Signal.Mailbox (Result String (List Election))
data = Signal.mailbox (Err "Data hasn't loaded yet")

elections : Chart.Data.Csv.Decoder Election
elections =
       Chart.Data.Csv.map2 (\percentage data -> {data | alp = percentage})
       ("Australian Labor Party Percentage" :=  (String.toFloat >> Result.toMaybe >> Maybe.map Data.float))
    <| Chart.Data.Csv.map2 (\percentage data -> {data | lnp = percentage})
       ("Liberal/National Coalition Percentage" := (String.toFloat >> Result.toMaybe >> Maybe.map Data.float))
    <| Chart.Data.Csv.map (\division -> {division = division, alp = Nothing, lnp = Nothing})
       ("DivisionNm" := Data.categoricalString)

type alias Election =
    { division : Data.Categorical String
    , alp : Maybe (Data.Number Float)
    , lnp : Maybe (Data.Number Float)
    }
