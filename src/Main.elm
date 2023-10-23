module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, h3, hr, span, text)
import Html.Attributes
import Html.Events exposing (onClick)
import Json.Decode as Decode
import Time



-- MAIN


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- MODEL


type alias Model =
    { score : Int
    , hitsPerSecond : Int
    , message : String
    }


init : Int -> ( Model, Cmd Msg )
init starting_score =
    ( { score = starting_score, hitsPerSecond = 0, message = "Press 'Up' to score a run!" }, Cmd.none )



-- UPDATE


type Msg
    = NumberUp
    | Nothing
    | Tick Time.Posix
    | NewHitter Int Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberUp ->
            ( { model | score = model.score + 1 }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )

        Tick _ ->
            if model.score < 100000 then
                ( { model
                    | score = model.score + model.hitsPerSecond
                    , message =
                        if model.score > 30 then
                            "A player is available -- click on them to hit for you!"

                        else if model.score == 0 then
                            "Press 'Up' to score a run!"

                        else
                            ""
                  }
                , Cmd.none
                )

            else
                ( { score = 100000, hitsPerSecond = 0, message = "You Win!" }, Cmd.none )

        NewHitter cost hits ->
            ( { model | hitsPerSecond = model.hitsPerSecond + hits, score = model.score - cost }, Cmd.none )



-- VIEW


player : String -> Int -> Int -> Int -> Html Msg
player name cost hits score =
    button [ onClick (NewHitter cost hits), Html.Attributes.disabled (score < cost) ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        [ h1 []
            [ span [] [ text (String.fromInt model.score) ] ]
        , hr [] []
        , player "Cedric Mullins" 30 1 model.score
        , player "Jordan Westburg" 100 3 model.score
        , player "Austin Hayes" 200 10 model.score
        , player "Anthony Santander" 400 30 model.score
        , player "Adley Rutschman" 1000 50 model.score
        , h3 [] [ span [] [ text model.message ] ]
        ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyDown decoder
        , Time.every 1000 Tick
        ]


decoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey string =
    case string of
        "ArrowUp" ->
            NumberUp

        _ ->
            Nothing
