module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Keyboard as Key
import Html.Attributes exposing (..)


{- Model
   Hold (x,y) coordinatees for a circle
-}


tubeTimeToPos : Float -> Float -> Int
tubeTimeToPos time n =
    818 - ((round ((time + n) / 5)) % 818)


type alias Model =
    { pixelPosition : Int, timer : Float, set : Bool, oTime : Float, tubeoTime : Float, oPosition : Int, pressoTime : Float, start : Bool, over : Bool, tubeTime : Float }



{- Wrap current Time as Float -}


type Msg
    = Tick Float
    | KeyMsg Key.KeyCode



{- Define a starting position
   (ends up being irrelavent)
-}


init =
    ( { pixelPosition = 150, timer = 0, set = False, oTime = 0, tubeoTime = 0, oPosition = 300, pressoTime = 0, start = False, over = False, tubeTime = 0 }, Cmd.none )



{- Update
   Change position based on current time
-}


update msg model =
    case msg of
        KeyMsg code ->
            let
                -- posY =
                --     model.pixelPosition - 50
                newoTime =
                    model.timer

                newoPosition =
                    model.pixelPosition

                newpressoTime =
                    model.timer

                modelN =
                    { model | oPosition = newoPosition, oTime = newoTime, pressoTime = newpressoTime, start = True }
            in
                ( modelN, Cmd.none )

        Tick time ->
            if model.start == False then
                ( { model | tubeoTime = time }, Cmd.none )
            else if (abs (model.pressoTime - model.timer)) > 200 then
                let
                    newoTime =
                        if model.set == False then
                            time
                        else
                            model.oTime

                    newtubeTime =
                        time - model.tubeoTime

                    posY =
                        round <| (toFloat (model.oPosition)) + 0.00025 * (time - model.oTime) ^ 2

                    modelN =
                        { model | pixelPosition = posY, timer = time, set = True, oTime = newoTime, tubeTime = newtubeTime }
                in
                    ( modelN, Cmd.none )
            else if (abs ((abs (model.pressoTime - model.timer)) - 200)) < 20 then
                let
                    newoPosition =
                        model.pixelPosition

                    newoTime =
                        model.timer

                    newtubeTime =
                        time - model.tubeoTime

                    modelN =
                        { model | oPosition = newoPosition, timer = time, oTime = newoTime, tubeTime = newtubeTime }
                in
                    ( modelN, Cmd.none )
            else
                let
                    newoTime =
                        if model.set == False then
                            time
                        else
                            model.oTime

                    newtubeTime =
                        time - model.tubeoTime

                    posY =
                        round <| (toFloat (model.oPosition)) + 0.001 * (time - model.oTime) ^ 2 - (time - model.oTime) * 0.5

                    modelN =
                        { model | pixelPosition = posY, timer = time, set = True, oTime = newoTime, tubeTime = newtubeTime }
                in
                    ( modelN, Cmd.none )



{- Subscriptions
   Subscribe to time using AnimationFrame for
   smooth rendering
-}


subscriptions model =
    Sub.batch [ Anim.times Tick, Key.downs KeyMsg ]



{- View
   Render circle based on current position
   held in model
-}


view : Model -> Html.Html Msg
view model =
    case model.start of
        False ->
            Html.body [ Html.Attributes.style [ ( "background-color", "#4dc0c9" ), ( "margin", "0" ) ] ]
                [ Html.h1 [ align "center", Html.Attributes.style [ ( "background-color", "#4dc0c9" ), ( "margin", "0" ) ] ]
                    [ text "Flappy Pixel" ]
                , Html.div []
                    [ Html.img
                        [ src "https://i.imgur.com/B6cjYlr.png", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                        []
                    ]
                , svg [ Svg.Attributes.width "768", Svg.Attributes.height "600", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                    [ circle [ cx "100", cy (toString model.pixelPosition), r "10", fill "red" ] [] ]
                ]

        True ->
            let
                pixY =
                    toString model.pixelPosition

                posX n =
                    tubeTimeToPos model.tubeTime n
            in
                Html.body [ Html.Attributes.style [ ( "background-color", "#4dc0c9" ), ( "margin", "0" ) ] ]
                    [ Html.h1 [ align "center", Html.Attributes.style [ ( "background-color", "#4dc0c9" ), ( "margin", "0" ) ] ]
                        [ text "Flappy Pixel" ]
                    , Html.div []
                        [ Html.img
                            [ src "https://i.imgur.com/B6cjYlr.png", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                            []
                        , svg [ Svg.Attributes.width "818", Svg.Attributes.height "432", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "-50px" ) ] ]
                            [ circle [ cx "150", cy pixY, r "10", fill "red" ] []
                            , rect [ x (toString (posX 0)), y "0", Svg.Attributes.height "331", Svg.Attributes.width "50", fill "green" ] []
                            , rect [ x (toString (posX 800)), y "0", Svg.Attributes.height "331", Svg.Attributes.width "50", fill "green" ] []

                            -- , rect [ x (toString (posX 142 * 2)), y "0", Svg.Attributes.height "331", Svg.Attributes.width "50", fill "green" ] []
                            -- , rect [ x (toString (posX 142 * 3)), y "0", Svg.Attributes.height "331", Svg.Attributes.width "50", fill "green" ] []
                            -- , rect [ x (toString (posX 4000)), y "0", Svg.Attributes.height "331", Svg.Attributes.width "50", fill "green" ] []
                            ]
                        , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "50px" ), ( "left", "50px" ) ] ] [ Html.text (toString model.tubeTime) ]
                        ]
                    ]



-- Html.div
--     []
--     [ Html.text (toString model.timer) ]
{- Main -}


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
