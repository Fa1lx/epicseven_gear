module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)



-- MODEL


type alias Model =
    { name : String
    , hero : Maybe Hero
    , baseStats : Maybe Stats
    , simulatedStats : Maybe Stats
    , items : List Item
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { name = ""
      , hero = Nothing
      , baseStats = Nothing
      , simulatedStats = Nothing
      , items = []
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Loading
    | GotHero (Result Http.Error Hero)



--| Failure
--| Success
--| UpdateItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loading ->
            ( model, Cmd.none )

        GotHero _ ->
            ( model, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div [] []



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = initialModel
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- TYPE DECLARATIONS TO STORE DATA


type alias Hero =
    { name : String
    , rarity : Int
    , element : String
    , stats : Stats
    , skills : List Skill
    }



-- Basic record to hold the selected hero's stats


type alias Stats =
    { atk : Int -- Attack
    , hp : Int -- Hitpoints
    , spd : Int -- Speed
    , def : Int -- Defense
    , chc : Float -- Critical Hit Chance
    , chd : Float -- Critical Hit Damage
    , eff : Float -- Effectiveness
    , efr : Float -- Effect Resistance
    }



-- Basic template for hero skills


type alias Skill =
    { isPassive : Bool
    , soulburn : Int --cost in souls, 0 means not soulburnable
    , soulburnEffect : Maybe String -- Description what soulburning does, empty if not soulburnable
    , cooldown : Int -- Cooldown in Turns
    , name : String
    , description : Maybe String
    , modifiers : List Modifier --list of all modifiers of the skill
    }



-- Basic template for each possible modifier for a hero's skill


type alias Modifier =
    { category : Maybe Stat -- defines which stat this modifier scales of
    , target : Maybe Origin
    , section : Maybe Section -- defines if the modifier is additive or multiplicative
    , value : Float -- defines the magnitude of the modifier
    , soulburn : Float -- defines the magnitude of the modifier while soulburned(same as value if not soulburnable)
    }


type alias Item =
    { slot : Slot
    , implicit : ( Float, Stat )
    , affixes : List ( Float, Stat )
    }


type Slot
    = Weapon
    | Helmet
    | Armor
    | Necklace
    | Ring
    | Boots


type Stat
    = Atk
    | AtkPercent
    | HP
    | HPPercent
    | Def
    | DefPercent
    | ChC
    | ChD
    | EfF
    | EfR
    | Speed



-- Basic template to define certain key enemies you fight very often in the game to measure your damage against those


type alias Enemy =
    { name : String
    , element : String
    , defValue : Int
    }



--TODO: create record for Wyvern11, Golem11, Banshee11, A11


type Section
    = Additive
    | Multiplicative
    | Pow


type Origin
    = Self
    | Target



-- FUNCTIONS
-- JSON DECODER


heroLoader : String -> Cmd Msg
heroLoader source =
    Http.get
        { url = source
        , expect = Http.expectJson GotHero heroDecoder
        }


heroDecoder : Decoder Hero
heroDecoder =
    Decode.map5
        Hero
        (Decode.at [ "name" ] Decode.string)
        (Decode.at [ "rarity" ] Decode.int)
        (Decode.at [ "element" ] Decode.string)
        (Decode.at [ "stats", "lv60SixStarFullyAwakened" ] statsDecoder)
        (Decode.at [ "skills" ] (Decode.list skillDecoder))


statsDecoder : Decoder Stats
statsDecoder =
    Decode.map8
        Stats
        (Decode.at [ "atk" ] Decode.int)
        (Decode.at [ "hp" ] Decode.int)
        (Decode.at [ "spd" ] Decode.int)
        (Decode.at [ "def" ] Decode.int)
        (Decode.at [ "chc" ] Decode.float)
        (Decode.at [ "chd" ] Decode.float)
        (Decode.at [ "eff" ] Decode.float)
        (Decode.at [ "efr" ] Decode.float)


skillDecoder : Decoder Skill
skillDecoder =
    Decode.map7
        Skill
        (Decode.at [ "isPassive" ] Decode.bool)
        (Decode.at [ "soulBurn" ] Decode.int)
        (Decode.maybe (Decode.at [ "soulBurnEffect" ] Decode.string))
        (Decode.at [ "cooldown" ] Decode.int)
        (Decode.at [ "name" ] Decode.string)
        (Decode.maybe (Decode.at [ "description" ] Decode.string))
        (Decode.at [ "damageModifiers" ] (Decode.list modifierDecoder))


modifierDecoder : Decoder Modifier
modifierDecoder =
    Decode.map5
        Modifier
        (Decode.maybe (Decode.at [ "stat" ] categoryDecoder))
        (Decode.maybe (Decode.at [ "target" ] targetDecoder))
        (Decode.maybe (Decode.at [ "section" ] sectionDecoder))
        (Decode.at [ "value" ] Decode.float)
        (Decode.at [ "soulburn" ] Decode.float)


categoryDecoder : Decoder Stat
categoryDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "atk" ->
                        Decode.succeed Atk

                    "hp" ->
                        Decode.succeed HP

                    "spd" ->
                        Decode.succeed Speed

                    _ ->
                        Decode.succeed Def
            )


targetDecoder : Decoder Origin
targetDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "self" ->
                        Decode.succeed Self

                    _ ->
                        Decode.succeed Target
            )


sectionDecoder : Decoder Section
sectionDecoder =
    Decode.string
        |> Decode.andThen
            (\str ->
                case str of
                    "pow" ->
                        Decode.succeed Pow

                    "additive" ->
                        Decode.succeed Additive

                    _ ->
                        Decode.succeed Multiplicative
            )
