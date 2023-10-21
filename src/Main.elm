module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Browser.Events
import Html exposing (Html, button, div, h1, hr, span, text)
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
    }


init : Int -> ( Model, Cmd Msg )
init starting_score =
    ( { score = starting_score, hitsPerSecond = 0 }, Cmd.none )



-- UPDATE


type Msg
    = NumberUp
    | Nothing
    | Tick Time.Posix
    | Mullins
    | Westburg
    | Hayes
    | Santander
    | Rutschman


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NumberUp ->
            ( { score = model.score + 1, hitsPerSecond = model.hitsPerSecond }, Cmd.none )

        Nothing ->
            ( model, Cmd.none )

        Tick _ ->
            if model.score < 100000 then
                ( { score = model.score + model.hitsPerSecond, hitsPerSecond = model.hitsPerSecond }, Cmd.none )

            else
                ( { score = 100000, hitsPerSecond = 0 }, Cmd.none )

        Mullins ->
            ( { score = model.score, hitsPerSecond = model.hitsPerSecond + 1 }, Cmd.none )

        Westburg ->
            ( { model | hitsPerSecond = model.hitsPerSecond + 3 }, Cmd.none )

        Hayes ->
            ( { model | hitsPerSecond = model.hitsPerSecond + 10 }, Cmd.none )

        Santander ->
            ( { model | hitsPerSecond = model.hitsPerSecond + 30 }, Cmd.none )

        Rutschman ->
            ( { model | hitsPerSecond = model.hitsPerSecond + 50 }, Cmd.none )



-- VIEW


player : String -> Int -> Int -> Msg -> Html Msg
player name minimum score msg =
    button [ onClick msg, Html.Attributes.disabled (score < minimum) ] [ text name ]


view : Model -> Html Msg
view model =
    div []
        (if model.score < 100000 then
            [ h1 []
                [ span [] [ text (String.fromInt model.score) ] ]
            , hr [] []
            , player "Cedric Mullins" 30 model.score Mullins
            , player "Jordan Westburg" 100 model.score Westburg
            , player "Austin Hayes" 200 model.score Hayes
            , player "Anthony Santander" 400 model.score Santander
            , player "Adley Rutschman" 1000 model.score Rutschman
            ]

         else
            [ h1 [] [ span [] [ text "You Win!!!" ] ] ]
        )



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
