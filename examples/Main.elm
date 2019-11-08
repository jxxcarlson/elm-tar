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
            -- "jello"
            content

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


content : String
content =
    """
😂 絵文字


    FORMÁLI

Í aldarfarsbók þeirri, er Beda prestur heilagur gerði, er getið eylands þess er Thile heitir og á bókum er sagt, að liggi sex dægra sigling í norður frá Bretlandi; þar sagði hann eigi koma dag á vetur og eigi nótt á sumar, þá er dagur er sem lengstur. Til þess ætla vitrir menn það haft, að Ísland sé Thile kallað, að það er víða á landinu, er sól skín um nætur, þá er dagur er sem lengstur, en það er víða um daga, er sól sér eigi, þá er nótt er sem lengst. En Beda prestur andaðist sjö hundruð þrjátigi og fimm árum eftir holdgan dróttins vors, að því er ritað er, og meir en hundraði ára fyrr en Ísland byggðist af Norðmönnum.

En áður Ísland byggðist af Noregi, voru þar þeir menn, er Norðmenn kalla papa; þeir voru menn kristnir, og hyggja menn, að þeir hafi verið vestan um haf, því að fundust eftir þeim bækur írskar, bjöllur og baglar og enn fleiri hlutir, þeir er það mátti skilja, að þeir voru Vestmenn. Enn er og þess getið á bókum enskum, að í þann tíma var farið milli landanna.




FYRSTI HLUTI


1. kafli

Þá er Ísland fannst og byggðist af Noregi, var Adríánus páfi í Róma og Jóhannes eftir hann, sá er hinn fimmti var með því nafni í postuligu sæti, en Hlöðver Hlöðversson keisari fyrir norðan fjall, en Leó og Alexander son hans yfir Miklagarði; þá var Haraldur hárfagri konungur yfir Noregi, en Eiríkur Eymundarson í Svíþjóð og Björn son hans, en Gormur hinn gamli að Danmörk, en Elfráður hinn ríki í Englandi og Játvarður son hans, en Kjarvalur að Dyflinni, Sigurður jarl hinn ríki í Orkneyjum.

Svo segja vitrir menn, að úr Noregi frá Staði sé sjö dægra sigling í vestur til Horns á Íslandi austanverðu, en frá Snæfellsnesi, þar er skemmst er, er fjögurra dægra haf í vestur til Grænlands. En svo er sagt, ef siglt er úr Björgyn rétt í vestur til Hvarfsins á Grænlandi, að þá mun siglt vera tylft fyrir sunnan Ísland. Frá Reykjanesi á sunnanverðu Íslandi er fimm dægra haf til Jölduhlaups á Írlandi (í suður; en frá Langanesi á norðanverðu Íslandi er) fjögurra dægra haf norður til Svalbarða í hafsbotn.

Svo er sagt, að menn skyldu fara úr Noregi til Færeyja; nefna sumir til Naddodd víking; en þá rak vestur í haf og fundu þar land mikið. Þeir gengu upp í Austfjörðum á fjall eitt hátt og sáust um víða, ef þeir sæju reyki eða nokkur líkindi til þess, að landið væri byggt, og sáu þeir það ekki.

Þeir fóru aftur um haustið til Færeyja; og er þeir sigldu af landinu, féll snær mikill á fjöll, og fyrir það kölluðu þeir landið Snæland. Þeir lofuðu mjög landið.

Þar heitir nú Reyðarfjall í Austfjörðum, er þeir höfðu að komið. Svo sagði Sæmundur prestur hinn fróði.

Maður hét Garðar Svavarsson, sænskur að ætt; hann fór að leita Snælands að tilvísan móður sinnar framsýnnar. Hann kom að landi fyrir austan Horn hið eystra; þar var þá höfn. Garðar sigldi umhverfis landið og vissi, að það var eyland. Hann var um veturinn norður í Húsavík á Skjálfanda og gerði þar hús.

Um vorið, er hann var búinn til hafs, sleit frá honum mann á báti, er hét Náttfari, og þræl og ambátt. Hann byggði þar síðan, er heitir Náttfaravík.

Garðar fór þá til Noregs og lofaði mjög landið. Hann var faðir Una, föður Hróars Tungugoða. Eftir það var landið kallað Garðarshólmur, og var þá skógur milli fjalls og fjöru.
"""

