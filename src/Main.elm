module Main exposing (main)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)


type alias Model =
    { count : Int }



-- Basic record for storing the information of the selected hero


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
    , dac : Float -- Dual Attack Chance
    }



-- Basic template for hero skills


type alias Skill =
    { name : String
    , isPassive : Bool
    , description : String
    , cooldown : Int
    , soulburn : Int --cost in souls, 0 means not soulburnable
    , soulburnEffect : String
    , modifiers : List Modifier --list of all modifiers of the skill
    }



-- Basic template for each possible modifier for a hero's skill


type alias Modifier =
    { category : String -- defines which stat this modifier scales of
    , section : Section -- defines if the modifier is additive or multiplicative
    , value : Float -- defines the magnitude of the modifier
    }



-- Basic template to define certain key enemies you fight very often in the game to measure your damage against those


type alias Enemy =
    { name : String
    , element : String
    , defValue : Int
    }


type Section
    = Additive
    | Multiplicative


initialModel : Model
initialModel =
    { count = 0 }


type Msg
    = Increment


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model


view : Model -> Html Msg
view model =
    div [] []


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = view
        , update = update
        }
