module Main exposing (main)

import Browser
import Html exposing (..)
import Bytes exposing (Bytes)
import Bytes.Encode exposing (encode)
import Tar exposing (Data(..))
import File.Download as Download
import Hex


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

        fileRecord3 =
            { fileRecord_ | filename = "c.binary" }

        content3 =
            Hex.toBytes "A0B1C2D3E4" |> Maybe.withDefault (encode (Bytes.Encode.unsignedInt8 0))
    in
        -- Tar.encodeTextFiles [ ( fileRecord1, content1 ), ( fileRecord2, content2 ) ] |> encode
        Tar.encodeFiles
            [ ( fileRecord1, StringData content1 )
            , ( fileRecord2, StringData content2 )
            , ( fileRecord3, BinaryData content3 )
            ]
            |> encode


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
