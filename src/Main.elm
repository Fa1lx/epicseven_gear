module Main exposing (main)

import Browser
import Html exposing (Attribute, Html, a, button, div, footer, h1, h2, header, i, img, node, p, section, span, table, text, th, thead, tr)
import Html.Attributes exposing (class, coords, href, id, name, shape, src, style, usemap)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import List exposing (..)
import String



-- MODEL


type alias Model =
    { name : String
    , hero : Maybe Hero
    , simulatedStats : Stats
    , items : List Item
    , addHeroButton : AddHeroButtonState
    , imageURL : String
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { name = ""
      , hero = Nothing
      , simulatedStats = initStats
      , items = []
      , addHeroButton = HideButtonMenu
      , imageURL = ""
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Loading String
    | GotHero (Result Http.Error Hero)
    | AddHeroButton AddHeroButtonMsg
    | CloseModal
    | HeroClicked String AddHeroButtonMsg



--| Failure
--| Success
--| UpdateItem


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loading name ->
            ( model, heroLoader (formatNameToURL name) )

        GotHero result ->
            case result of
                Ok loadedHero ->
                    ( { model | hero = Just loadedHero, simulatedStats = calculateStats loadedHero.stats model.items }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        CloseModal ->
            ( model, Cmd.none )

        AddHeroButton addHeroButtonMsg ->
            ( updateAddHeroButton addHeroButtonMsg model, Cmd.none )

        HeroClicked name addHeroButtonMsg ->
            let
                oldModel =
                    updateAddHeroButton addHeroButtonMsg model
            in
            ( { oldModel | name = name }, heroLoader (formatNameToURL name) )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeroButton model
        , applicationHeader
        , image model.name model.simulatedStats
        ]



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


initStats =
    Stats 0 0 0 0 0.0 0.0 0.0 0.0



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
    , affixes : List Affix
    }


type alias Affix =
    { value : Int
    , stat : Stat
    }


type alias CumulativeStats =
    { atkFlat : Int
    , atkPercent : Int
    , hpFlat : Int
    , hpPercent : Int
    , defFlat : Int
    , defPercent : Int
    , chc : Int
    , chd : Int
    , eff : Int
    , efr : Int
    , spd : Int
    }


initCumulativeStats : CumulativeStats
initCumulativeStats =
    CumulativeStats 0 100 0 100 0 100 0 0 0 0 0


type Slot
    = Weapon
    | Helmet
    | Armor
    | Necklace
    | Ring
    | Boots
    | Artifact


type Stat
    = Atk
    | AtkPercent
    | HP
    | HPPercent
    | Def
    | DefPercent
    | ChC
    | ChD
    | Eff
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


formatNameToURL : String -> String
formatNameToURL str =
    "src/hero-json/" ++ formatNameToFileName str ++ ".json"


formatNameToFileName : String -> String
formatNameToFileName str =
    String.toLower (String.replace " & " "-" (String.replace " " "-" str))


calculateStats : Stats -> List Item -> Stats
calculateStats heroStats items =
    let
        cStats =
            foldl
                accumulateStats
                --to normalize the % values
                initCumulativeStats
                (map affixToStats (concatMap (\item -> item.affixes) items))
    in
    { heroStats
        | atk = round (toFloat heroStats.atk * (toFloat cStats.atkPercent / 100)) + cStats.atkFlat
        , hp = round (toFloat heroStats.hp * (toFloat cStats.hpPercent / 100)) + cStats.hpFlat
        , spd = heroStats.spd + cStats.spd
        , def = round (toFloat heroStats.def * (toFloat cStats.defPercent / 100)) + cStats.defFlat
        , chc = heroStats.chc + (toFloat cStats.chc / 100)
        , chd = heroStats.chd + (toFloat cStats.chd / 100)
        , eff = heroStats.eff + (toFloat cStats.eff / 100)
        , efr = heroStats.efr + (toFloat cStats.efr / 100)
    }


accumulateStats : CumulativeStats -> CumulativeStats -> CumulativeStats
accumulateStats aStat bStat =
    { aStat
        | atkFlat = aStat.atkFlat + bStat.atkFlat
        , atkPercent = aStat.atkPercent + bStat.atkPercent
        , hpFlat = aStat.hpFlat + bStat.hpFlat
        , hpPercent = aStat.hpPercent + bStat.hpPercent
        , defFlat = aStat.defFlat + bStat.defFlat
        , defPercent = aStat.defPercent + bStat.defPercent
        , chc = aStat.chc + bStat.chc
        , chd = aStat.chd + bStat.chd
        , eff = aStat.eff + bStat.eff
        , efr = aStat.efr + bStat.efr
        , spd = aStat.spd + bStat.spd
    }


affixToStats : Affix -> CumulativeStats
affixToStats affix =
    let
        cStats =
            initCumulativeStats
    in
    case affix.stat of
        Atk ->
            { cStats | atkFlat = cStats.atkFlat + affix.value }

        AtkPercent ->
            { cStats | atkPercent = cStats.atkPercent + affix.value }

        HP ->
            { cStats | hpFlat = cStats.hpFlat + affix.value }

        HPPercent ->
            { cStats | hpPercent = cStats.hpPercent + affix.value }

        Def ->
            { cStats | defFlat = cStats.defFlat + affix.value }

        DefPercent ->
            { cStats | defPercent = cStats.defPercent + affix.value }

        ChC ->
            { cStats | chc = cStats.chc + affix.value }

        ChD ->
            { cStats | chd = cStats.chd + affix.value }

        Eff ->
            { cStats | eff = cStats.eff + affix.value }

        EfR ->
            { cStats | efr = cStats.efr + affix.value }

        Speed ->
            { cStats | spd = cStats.spd + affix.value }



-- PHIL FUNs


showStats : Stats -> Html Msg
showStats stats =
    div [ class "box statsbox" ]
        [ p [ class "title is-5" ] [ text "Stats" ]
        , table [ class "table", class "table is-narrow is-fullwidth" ]
            [ thead []
                [ tr []
                    [ Html.td [] [ text "Hitpoints" ]
                    , Html.td [] [ text (String.fromInt stats.hp) ]
                    ]
                , tr []
                    [ Html.td [] [ text "Attack" ]
                    , Html.td [] [ text (String.fromInt stats.atk) ]
                    ]
                , tr []
                    [ Html.td [] [ text "Defense" ]
                    , Html.td [] [ text (String.fromInt stats.def) ]
                    ]
                , tr []
                    [ Html.td [] [ text "Speed" ]
                    , Html.td [] [ text (String.fromInt stats.spd) ]
                    ]
                , tr []
                    [ Html.td [] [ text "Critical Hit Chance" ]
                    , Html.td [] [ text (String.fromFloat (stats.chc * 100) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Critical Hit Damage" ]
                    , Html.td [] [ text (String.fromFloat (stats.chd * 100) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Effectiveness" ]
                    , Html.td [] [ text (String.fromFloat (stats.eff * 100) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Effect Resistance" ]
                    , Html.td [] [ text (String.fromFloat (stats.efr * 100) ++ "%") ]
                    ]
                ]
            ]
        ]


imgMap : List (Attribute msg) -> List (Html msg) -> Html msg
imgMap attributes children =
    node "map" attributes children


area : List (Attribute msg) -> List (Html msg) -> Html msg
area attributes children =
    node "area" attributes children


type AddHeroButtonMsg
    = HideAddHeroDropdownMenu
    | ShowAddHeroDropdownMenu


type AddHeroButtonState
    = ShowButtonMenu
    | HideButtonMenu


updateAddHeroButton : AddHeroButtonMsg -> Model -> Model
updateAddHeroButton addHeroButtonMsg model =
    case addHeroButtonMsg of
        ShowAddHeroDropdownMenu ->
            { model | addHeroButton = ShowButtonMenu }

        HideAddHeroDropdownMenu ->
            { model | addHeroButton = HideButtonMenu }


applicationTitle : String.String
applicationTitle =
    "Epic7Seven Gear Calculator"


applicationSubTitle : String.String
applicationSubTitle =
    "lol"


applicationHeader : Html Msg
applicationHeader =
    div [ class "hero" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title" ] [ text applicationTitle ]
                , h2 [ class "subtitle" ] [ text applicationSubTitle ]
                ]
            ]
        ]


image : String -> Stats -> Html Msg
image name stats =
    let
        imageurl =
            if name == "Charles" then
                "src/hero_images/charles.jpg"

            else if name == "Bellona" then
                "src/hero_images/bellona.jpg"

            else if name == "Challenger Dominiel" then
                "src/hero_images/challenger-dominiel.jpg"

            else if name == "Martial Artist Ken" then
                "src/hero_images/martial-artist-ken.jpg"

            else if name == "Yufine" then
                "src/hero_images/yufine.jpg"

            else if name == "Sez" then
                "src/hero_images/sez.jpg"

            else if name == "Haste" then
                "src/hero_images/haste.jpg"

            else if name == "Baal&Sezan" then
                "src/hero_images/baal-sezan.jpg"

            else if name == "Karin" then
                "src/hero_images/Karin.jpg"

            else if name == "Vildred" then
                "src/hero_images/vildred.jpg"

            else
                ""
    in
    section [ class "section" ]
        [ div [ class "container test" ]
            [ img
                [ src imageurl, class "test2" ]
                []
            , div [ class "itemimg" ]
                [ img [ onClick (HeroClicked "" HideAddHeroDropdownMenu), src imageurl ] []
                , imgMap [ Html.Attributes.name "my_map" ] [ area [ shape "Rectangle", coords "0,0,100,100" ] [] ]
                ]
            , if name /= "" then
                showStats stats

              else
                div [] []
            ]
        ]


viewHeroButton : Model -> Html Msg
viewHeroButton model =
    let
        { dropdownText, dropdownActive, dropdownAction, dropdownIcon } =
            case model.addHeroButton of
                ShowButtonMenu ->
                    { dropdownText = "Hero"
                    , dropdownActive = " is-active"
                    , dropdownAction = HideAddHeroDropdownMenu
                    , dropdownIcon = "fa-angle-up"
                    }

                HideButtonMenu ->
                    { dropdownText = "Hero"
                    , dropdownActive = ""
                    , dropdownAction = ShowAddHeroDropdownMenu
                    , dropdownIcon = "fa-angle-down"
                    }
    in
    p [ class "level-item" ]
        [ div
            [ class ("dropdown" ++ dropdownActive)
            , onClick (AddHeroButton dropdownAction)
            ]
            [ div [ class "dropdown-trigger" ]
                [ button
                    [ class "button  is-info is-large is-rounded"
                    , ariaHaspopup "true"
                    , ariaControls "dropdown-menu3"
                    ]
                    [ span [] [ text dropdownText ]
                    , span [ class "icon is-small" ]
                        [ i
                            [ class ("fas " ++ dropdownIcon)
                            , ariaHidden "true"
                            ]
                            []
                        ]
                    ]
                ]
            , div [ class "dropdown-menu", id "dropdown-menu3" ]
                [ div [ class "dropdown-content" ]
                    [ heroDropDownElement "Charles"
                    , heroDropDownElement "Bellona"
                    , heroDropDownElement "Challenger Dominiel"
                    , heroDropDownElement "Martial Artist Ken"
                    , heroDropDownElement "Yufine"
                    , heroDropDownElement "Sez"
                    , heroDropDownElement "Haste"
                    , heroDropDownElement "Baal&Sezan"
                    , heroDropDownElement "Karin"
                    , heroDropDownElement "Vildred"
                    ]
                ]
            ]
        ]


heroDropDownElement : String -> Html Msg
heroDropDownElement name =
    a
        [ href "#"
        , class "dropdown-item"
        , onClickNoBubblingUp (HeroClicked name HideAddHeroDropdownMenu)
        ]
        [ text name ]


onClickNoBubblingUp : msg -> Attribute msg
onClickNoBubblingUp msg =
    Html.Events.stopPropagationOn "click" (Decode.map (\m -> ( m, True )) (Decode.succeed msg))


machwas : Html Msg
machwas =
    div [ class "modal-card" ]
        [ modalHeader "Tabelle der Formen"
        , section [ class "modal-card-body" ]
            [ table [ class "table" ]
                [ thead []
                    [ tr []
                        [ th [] [ text "Laufende Nummer" ]
                        , th [] [ text "Gebietstyp" ]
                        , th [] [ text "Gebietskoordinaten" ]
                        ]
                    ]
                ]
            ]
        , modalFooter []
        ]


modalHeader : String -> Html Msg
modalHeader title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ] [ text title ]
        , button [ class "delete", ariaLabel "close", onClick CloseModal ] []
        ]


modalFooter : List (Html Msg) -> Html Msg
modalFooter modalButtons =
    footer [ class "modal-card-foot" ]
        (modalButtons
            ++ [ button [ class "button is-success", onClick CloseModal ] [ text "Schließen" ]
               ]
        )


ariaLabel : String -> Attribute msg
ariaLabel value =
    Html.Attributes.attribute "aria-label" value


ariaHidden : String -> Attribute msg
ariaHidden value =
    Html.Attributes.attribute "aria-hidden" value


ariaHaspopup : String -> Attribute msg
ariaHaspopup value =
    Html.Attributes.attribute "aria-haspopup" value


ariaControls : String -> Attribute msg
ariaControls value =
    Html.Attributes.attribute "aria-controls" value



-- END PHIL FUNs
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



-- Helper functions


join : List (List a) -> List a
join =
    List.foldr (++) []


flatMap : (a -> List b) -> List a -> List b
flatMap f list =
    List.map f list
        |> join
