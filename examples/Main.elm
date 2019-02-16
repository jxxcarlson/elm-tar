module Main exposing (main)

import Browser
import Bytes exposing (Bytes)
import Bytes.Encode as E exposing (encode)
import File.Download as Download
import Hex
import Html exposing (..)
import Tar exposing (Data(..))


-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { data : Bytes
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { data = initialData
      }
    , saveData initialData
    )


initialData : Bytes
initialData =
    let
        metadata =
            Tar.defaultMetadata

        metadata1 =
            { metadata | filename = "a.txt" }

        content1 =
            "jello"

        metadata2 =
            { metadata | filename = "b.binary" }

        content2a =
            Hex.toBytes "AABBFF" |> Maybe.withDefault (encode (E.unsignedInt8 0))

        content2 =
            encode (E.string "jello")
    in
        Tar.encodeFiles
            [ ( metadata1, StringData content1 )
            , ( metadata2, BinaryData content2 )
            ]
            |> encode


saveData : Bytes -> Cmd msg
saveData bytes =
    Download.bytes "test.tar" "application/x-tar" bytes



-- UPDATE


type Msg
    = Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ text "Refresh browser to download test tar file" ]
