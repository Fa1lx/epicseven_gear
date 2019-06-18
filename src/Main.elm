module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)



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



--| Failure
--| Success
--| UpdateItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loading ->
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
    { name : String
    , isPassive : Bool
    , description : String
    , cooldown : Int -- Cooldown in Turns
    , soulburn : Int --cost in souls, 0 means not soulburnable
    , soulburnEffect : String -- Description what soulburning does, empty if not soulburnable
    , modifiers : List Modifier --list of all modifiers of the skill
    }



-- Basic template for each possible modifier for a hero's skill


type alias Modifier =
    { category : String -- defines which stat this modifier scales of
    , target : Origin
    , section : Section -- defines if the modifier is additive or multiplicative
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


type Origin
    = Self
    | Target



-- FUNCTIONS
