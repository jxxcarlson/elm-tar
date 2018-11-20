module Main exposing (main)

import Browser
import Html exposing (..)
import Bytes exposing (Bytes)
import Bytes.Encode exposing (encode)
import Tar
import File.Download as Download


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
        fileRecord_ =
            Tar.defaultFileRecord

        fileRecord1 =
            { fileRecord_ | filename = "a.txt" }

        content1 =
            "One two three\n"

        fileRecord2 =
            { fileRecord_ | filename = "b.txt" }

        content2 =
            "Four five six\n"
    in
        Tar.encodeFiles [ ( fileRecord1, content1 ), ( fileRecord2, content2 ) ] |> encode


saveData : Bytes -> Cmd msg
saveData bytes =
    Download.bytes ("test.tar") "application/x-tar" bytes



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
