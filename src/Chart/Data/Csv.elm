module Chart.Data.Csv (Decoder, fetch, decode, (:=), map, map2) where

import Http
import Task exposing (Task)
import Csv exposing (Csv)
import List.Extra as List
import String

import Chart.Data

type Decoder a = RowDecoder (Csv -> Maybe (List a)) | Decoder String (String -> a)

fetch : String -> Decoder a -> Task String (List a)
fetch url decoder = Http.getString url
    |> Task.map (read decoder)
    |> Task.mapError toString
    |> \task -> task `Task.andThen` Task.fromMaybe "No values found"

read : Decoder a -> String -> Maybe (List a)
read decoder string = Csv.parse string
    |> decode decoder

decode : Decoder a -> Csv -> Maybe (List a)
decode decoder csv = case decoder of
    RowDecoder decoder -> decoder csv
    Decoder label decoder ->
        List.elemIndex label csv.headers
        |> Maybe.map (\i ->
            List.filterMap (\line -> List.getAt line i) csv.records
            |> List.map decoder)

(:=) = Decoder 

map : (a -> b) -> Decoder a -> Decoder b
map f decoder = case decoder of
    RowDecoder decoder -> RowDecoder
        <| Maybe.map (List.map f) << decoder
    Decoder label decoder -> Decoder label
        <| f << decoder

map2 : (a -> b -> c) -> Decoder a -> Decoder b -> Decoder c
map2 join d1 d2 =  RowDecoder <| \csv ->
    Maybe.map2 (List.map2 join)
    (decode d1 csv) (decode d2 csv) 
