module Main exposing (main)

import Array
import Bool.Extra
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, field, string)
import List exposing (..)
import List.Extra
import Maybe exposing (withDefault)
import String exposing (concat, toInt)
import Task



-- MODEL


type alias Model =
    { name : String
    , hero : Maybe Hero
    , simulatedStats : Stats
    , baseStats : Stats
    , items : List Item
    , addHeroButton : AddHeroButtonState
    , imageURL : String
    , modal : Maybe ModalState
    , newItem : Item
    , currentItem : Item
    }


initialModel : () -> ( Model, Cmd Msg )
initialModel _ =
    ( { name = ""
      , hero = Nothing
      , simulatedStats = initStats
      , baseStats = initStats
      , items = []
      , addHeroButton = HideButtonMenu
      , imageURL = ""
      , modal = Nothing
      , newItem = initItem Dummy
      , currentItem = initItem Dummy
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
    | OpenModal ModalMsg Item
    | Add
    | UpdateStats


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Loading name ->
            ( model, heroLoader (formatNameToURL name) )

        GotHero result ->
            case result of
                Ok loadedHero ->
                    ( { model | hero = Just loadedHero, simulatedStats = calculateStats loadedHero.stats model.items, baseStats = loadedHero.stats }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AddHeroButton addHeroButtonMsg ->
            ( updateAddHeroButton addHeroButtonMsg model, Cmd.none )

        HeroClicked name addHeroButtonMsg ->
            let
                oldModel =
                    updateAddHeroButton addHeroButtonMsg model
            in
            ( { oldModel | name = name }, heroLoader (formatNameToURL name) )

        OpenModal modalMsg item ->
            ( updateModal modalMsg item model, Cmd.none )

        Add ->
            ( { model
                | modal = Nothing
                , items =
                    let
                        index =
                            indexOf model.currentItem model.items
                    in
                    if index == -1 then
                        model.items ++ [ model.newItem ]

                    else
                        Array.toList (Array.set index model.newItem (Array.fromList model.items))
                , newItem = initItem Dummy
                , currentItem = initItem Dummy
              }
            , Task.succeed UpdateStats |> Task.perform identity
            )

        CloseModal ->
            ( { model | modal = Nothing }, Cmd.none )

        UpdateStats ->
            ( { model | simulatedStats = calculateStats model.baseStats model.items }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewHeroButton model
        , applicationHeader
        , image model.name model.simulatedStats model.items
        , viewModal model
        , createSkillEntity model.hero model.simulatedStats 0
        , createSkillEntity model.hero model.simulatedStats 1
        , createSkillEntity model.hero model.simulatedStats 2
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


dummySkill =
    Skill True 0 (Just "") 0 "Dummy SKill" (Just "") [ dummyMod ]



-- Basic template for each possible modifier for a hero's skill


type alias Item =
    { slot : Slot
    , atkFlat : Int
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


initItem : Slot -> Item
initItem slot =
    Item slot 0 0 0 0 0 0 0 0 0 0 0


initCumulativeStats : Item
initCumulativeStats =
    Item Weapon 0 100 0 100 0 100 0 0 0 0 0


type Slot
    = Weapon
    | Helmet
    | Armor
    | Necklace
    | Ring
    | Boots
    | Dummy


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


type alias Modifier =
    { category : Maybe Stat -- defines which stat this modifier scales of
    , target : Maybe Origin
    , section : Section -- defines if the modifier is additive or multiplicative
    , value : Float -- defines the magnitude of the modifier
    , soulburn : Float -- defines the magnitude of the modifier while soulburned(same as value if not soulburnable)
    }


dummyMod =
    Modifier Nothing Nothing Pow 0.0 0.0


type Section
    = Additive
    | Multiplicative
    | Pow


type Origin
    = Self
    | Target


type alias SkillModifier =
    { defaultMulti : Float
    , pow : Float
    , ownAtk : Float
    , ownHP : Float
    , ownDef : Float
    , ownSpeed : Float
    , enemyHP : Float
    }


initSkillMod : SkillModifier
initSkillMod =
    SkillModifier 1.871 0.0 0.0 0.0 0.0 0.0 0.0



-- FUNCTIONS


formatNameToURL : String -> String
formatNameToURL str =
    "src/hero-json/" ++ formatNameToFileName str ++ ".json"


formatNameToFileName : String -> String
formatNameToFileName str =
    String.toLower (String.replace "&" "-" (String.replace " " "-" str))


calculateStats : Stats -> List Item -> Stats
calculateStats heroStats items =
    let
        cStats =
            List.foldl accumulateStats initCumulativeStats items
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


accumulateStats : Item -> Item -> Item
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


updateSkillMod : Modifier -> SkillModifier -> SkillModifier
updateSkillMod modifier skillMod =
    case modifier.section of
        Pow ->
            { skillMod | pow = modifier.value }

        Additive ->
            case modifier.target of
                --only HP has a target scaling, I think
                Just Target ->
                    { skillMod | enemyHP = modifier.value }

                _ ->
                    case modifier.category of
                        Just stat ->
                            case stat of
                                Atk ->
                                    { skillMod | ownAtk = modifier.value }

                                HP ->
                                    { skillMod | ownHP = modifier.value }

                                _ ->
                                    { skillMod | ownDef = modifier.value }

                        _ ->
                            initSkillMod

        --only own speed scaling is multiplicative
        Multiplicative ->
            { skillMod | ownSpeed = modifier.value }


convertSkillToSkillMod : Skill -> SkillModifier
convertSkillToSkillMod skill =
    List.foldl updateSkillMod initSkillMod skill.modifiers


calculateDmgOfSkill : Stats -> SkillModifier -> Int
calculateDmgOfSkill heroStats skillMod =
    floor
        ((toFloat heroStats.atk * skillMod.ownAtk + toFloat heroStats.hp * skillMod.ownHP + toFloat heroStats.def * skillMod.ownDef)
            * skillMod.pow
            * skillMod.defaultMulti
            * (1.0 + toFloat heroStats.spd * skillMod.ownSpeed)
            * (1.0
                + (if heroStats.chc >= 1 then
                    heroStats.chd - 1.0

                   else
                    heroStats.chc * (heroStats.chd - 1.0)
                  )
              )
        )


calcSkill : Hero -> Int -> Stats -> Int
calcSkill hero skillId stats =
    case List.Extra.getAt skillId hero.skills of
        Nothing ->
            0

        Just skill ->
            calculateDmgOfSkill stats (convertSkillToSkillMod skill)


getItemFromInventory : Slot -> List Item -> Item
getItemFromInventory slot_ inventory =
    let
        item =
            List.head (List.filter (\item_ -> item_.slot == slot_) inventory)
    in
    case item of
        Just a ->
            a

        Nothing ->
            initItem slot_


updateInventory : Model -> Model
updateInventory model =
    { model
        | items =
            let
                index =
                    indexOf model.currentItem model.items
            in
            if index == -1 then
                model.items ++ [ model.newItem ]

            else
                Array.toList (Array.set index model.newItem (Array.fromList model.items))
    }


indexOf : Item -> List Item -> Int
indexOf item items =
    items
        |> List.indexedMap (\i x -> ( i, x ))
        |> List.filter (\( idx, item_ ) -> item == item_)
        |> List.map Tuple.first
        |> List.minimum
        |> Maybe.withDefault -1



-- PHIL FUNs


showStats : Stats -> String -> Html Msg
showStats stats name =
    div [ class "box statsbox flexauto" ]
        [ p [ class "title is-5" ] [ text (concat [ name, "-Stats" ]) ]
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
                    , Html.td [] [ text (String.fromInt (floor (stats.chc * 100)) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Critical Hit Damage" ]
                    , Html.td [] [ text (String.fromInt (floor (stats.chd * 100)) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Effectiveness" ]
                    , Html.td [] [ text (String.fromInt (floor (stats.eff * 100)) ++ "%") ]
                    ]
                , tr []
                    [ Html.td [] [ text "Effect Resistance" ]
                    , Html.td [] [ text (String.fromInt (floor (stats.efr * 100)) ++ "%") ]
                    ]
                ]
            ]
        ]


showSkill : SkillModifier -> Html Msg
showSkill modifier =
    div [ class "box modifierbox" ]
        [ p [ class "title is-5" ] [ text "Modifier" ]
        , table [ class "table", class "table is-narrow is-fullwidth" ]
            [ thead []
                [ tr []
                    [ Html.td [] [ text "defaultMulti" ]
                    , Html.td [] [ text (String.fromFloat modifier.defaultMulti) ]
                    , td [] [ text "enemyHP" ]
                    , td [] [ text (String.fromFloat modifier.enemyHP) ]
                    ]
                , tr []
                    [ Html.td [] [ text "ownAtk" ]
                    , Html.td [] [ text (String.fromFloat modifier.ownAtk) ]
                    , td [] [ text "ownDef" ]
                    , td [] [ text (String.fromFloat modifier.ownDef) ]
                    ]
                , tr []
                    [ Html.td [] [ text "ownHP" ]
                    , Html.td [] [ text (String.fromFloat modifier.ownHP) ]
                    , td [] [ text "ownSpeed" ]
                    , td [] [ text (String.fromFloat modifier.ownSpeed) ]
                    ]
                , tr []
                    [ Html.td [] [ text "pow" ]
                    , Html.td [] [ text (String.fromFloat modifier.pow) ]
                    ]
                ]
            ]
        ]


createSkillEntity : Maybe Hero -> Stats -> Int -> Html Msg
createSkillEntity hero_ stats skillId =
    case hero_ of
        Nothing ->
            div [] []

        Just hero ->
            let
                skill =
                    case List.Extra.getAt skillId hero.skills of
                        Just a ->
                            a

                        Nothing ->
                            dummySkill
            in
            div [ class "box skillbox" ]
                [ p [ class "title is-5" ] [ text (concat [ "Skill ", String.fromInt (skillId + 1) ]) ]
                , table [ class "table", class "table is-narrow is-fullwidth" ]
                    [ td []
                        [ tr []
                            [ td [] [ text "Skill Name:" ]
                            , td [] [ text skill.name ]
                            ]
                        , tr []
                            [ td [] [ text "Cooldown:" ]
                            , td [] [ text (String.fromInt skill.cooldown) ]
                            ]
                        , tr []
                            [ td [] [ text "isPassive:" ]
                            , td [] [ text (Bool.Extra.toString skill.isPassive) ]
                            ]
                        , tr []
                            [ td [] [ text "Damage:" ]
                            , td [] [ text (String.fromInt (calculateDmgOfSkill stats (convertSkillToSkillMod skill))) ]
                            ]
                        , tr []
                            [ td [] [ text "Description:" ]
                            , td [] [ text (Maybe.withDefault "" skill.description) ]
                            ]
                        ]
                    , td [] [ showSkill (convertSkillToSkillMod skill) ]
                    ]
                ]


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


applicationHeader : Html Msg
applicationHeader =
    div [ class "hero" ]
        [ div [ class "hero-body" ]
            [ div [ class "container" ]
                [ h1 [ class "title has-text-white" ] [ text applicationTitle ]
                ]
            ]
        ]


image : String -> Stats -> List Item -> Html Msg
image name stats inventory =
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
    if name /= "" then
        div [ class "flexbox" ]
            [ div [ class "space" ] []
            , showStats stats name
            , div [ class "space" ] []
            , div [ class "itemimg" ]
                [ img [ onClick (OpenModal OpenInput (getItemFromInventory Weapon inventory)), src "src/item-images/weapon.png", class "rounded" ] []
                , img [ onClick (OpenModal OpenInput (getItemFromInventory Helmet inventory)), src "src/item-images/helmet.png", class "rounded" ] []
                , img [ onClick (OpenModal OpenInput (getItemFromInventory Armor inventory)), src "src/item-images/armor.png", class "rounded" ] []
                ]
            , img
                [ src imageurl, class "heroimg" ]
                []
            , div [ class "flexauto itemimg" ]
                [ img [ onClick (OpenModal OpenInput (getItemFromInventory Necklace inventory)), src "src/item-images/necklace.png", class "rounded" ] []
                , img [ onClick (OpenModal OpenInput (getItemFromInventory Ring inventory)), src "src/item-images/ring.png", class "rounded" ] []
                , img [ onClick (OpenModal OpenInput (getItemFromInventory Boots inventory)), src "src/item-images/boots.png", class "rounded" ] []
                ]
            , div [ class "space" ] []
            ]

    else
        div [] [ img [ src "https://static.smilegatemegaport.com/event/live/epic7/world/brand/images/common/img_share_630.jpg", class "openimage" ] [] ]


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
                    [ class "button  is-black is-large is-rounded"
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


type ModalMsg
    = OpenInput
    | Change


type ModalState
    = InputItem Item


updateModal : ModalMsg -> Item -> Model -> Model
updateModal modalMsg item model =
    case modalMsg of
        OpenInput ->
            { model | modal = Just (InputItem item), currentItem = item }

        Change ->
            { model | modal = Just (InputItem item), newItem = item }


inputItem : Item -> Html Msg
inputItem item =
    let
        changeItemAtk newAtk =
            OpenModal Change (Item item.slot (Maybe.withDefault 0 (String.toInt newAtk)) item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc item.chd item.eff item.efr item.spd)

        changeItemAtkP newAtkP =
            OpenModal Change (Item item.slot item.atkFlat (Maybe.withDefault 0 (String.toInt newAtkP)) item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc item.chd item.eff item.efr item.spd)

        changeItemDef newDef =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent (Maybe.withDefault 0 (String.toInt newDef)) item.defPercent item.chc item.chd item.eff item.efr item.spd)

        changeItemDefP newDefP =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat (Maybe.withDefault 0 (String.toInt newDefP)) item.chc item.chd item.eff item.efr item.spd)

        changeItemHP newHP =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent (Maybe.withDefault 0 (String.toInt newHP)) item.hpPercent item.defFlat item.defPercent item.chc item.chd item.eff item.efr item.spd)

        changeItemHPP newHPP =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat (Maybe.withDefault 0 (String.toInt newHPP)) item.defFlat item.defPercent item.chc item.chd item.eff item.efr item.spd)

        changeItemChc newChc =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent (Maybe.withDefault 0 (String.toInt newChc)) item.chd item.eff item.efr item.spd)

        changeItemChd newChd =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc (Maybe.withDefault 0 (String.toInt newChd)) item.eff item.efr item.spd)

        changeItemEff newEff =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc item.chd (Maybe.withDefault 0 (String.toInt newEff)) item.efr item.spd)

        changeItemEfR newEfR =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc item.chd item.eff (Maybe.withDefault 0 (String.toInt newEfR)) item.spd)

        changeItemSpd newSpd =
            OpenModal Change (Item item.slot item.atkFlat item.atkPercent item.hpFlat item.hpPercent item.defFlat item.defPercent item.chc item.chd item.eff item.efr (Maybe.withDefault 0 (String.toInt newSpd)))
    in
    div [ class "modal-card" ]
        [ modalHeader "" --s
        , section [ class "modal-card-body" ]
            [ fieldset []
                [ div []
                    [ text "Atk"
                    , input [ class "input", type_ "text", value (String.fromInt item.atkFlat), onInput changeItemAtk ] []
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "AtkPercent" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.atkPercent), onInput changeItemAtkP ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "HP" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.hpFlat), onInput changeItemHP ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "HPPercent" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.hpPercent), onInput changeItemHPP ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "Def" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.defFlat), onInput changeItemDef ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "DefPercent" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.defPercent), onInput changeItemDefP ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "Chc" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.chc), onInput changeItemChc ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "Chd" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.chd), onInput changeItemChd ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "Eff" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.eff), onInput changeItemEff ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "EfR" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.efr), onInput changeItemEfR ] []
                        ]
                    ]
                , div [ class "field" ]
                    [ label [ class "label" ] [ text "Speed" ]
                    , div [ class "control" ]
                        [ input [ class "input", type_ "text", value (String.fromInt item.spd), onInput changeItemSpd ] []
                        ]
                    ]
                ]
            , modalFooter
            ]
        ]


viewModal : Model -> Html Msg
viewModal model =
    case model.modal of
        Nothing ->
            span [] []

        Just modalState ->
            div [ class "modal is-active" ]
                [ div [ class "modal-background" ] []
                , case modalState of
                    InputItem item ->
                        inputItem item
                ]


modalHeader : String -> Html Msg
modalHeader title =
    header [ class "modal-card-head" ]
        [ p [ class "modal-card-title" ] [ text title ]
        , button [ class "delete", ariaLabel "close", onClick CloseModal ] []
        ]


modalFooter : Html Msg
modalFooter =
    footer [ class "modal-card-foot" ]
        ([ a [ class "button is-success", onClick Add ] [ text "Add Item" ] ]
            ++ [ button [ class "button is-success", onClick CloseModal ] [ text "Close" ]
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
        (Decode.at [ "section" ] sectionDecoder)
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
