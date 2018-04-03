module SvgAnimation exposing (..)

import Html as Html
import Platform.Cmd as Cmd
import Platform.Sub as Sub
import Svg exposing (..)
import Svg.Attributes exposing (..)
import AnimationFrame as Anim
import Keyboard as Key
import Html.Attributes exposing (..)


tubeTimeToPos : Float -> Float -> Int
tubeTimeToPos time n =
    if (time - n) > 0 then
        818 - ((round ((time - n) / 5)) % 818)
    else
        818



-- A function used to turn tube's timer to their coordinates. The 'n' here is the shift of tubes.


touch : Int -> Int -> Int -> Bool
touch pixp y x =
    ((pixp < y) || (pixp > y + 70)) && ((145 > x - 20) && (145 < x + 50)) || ((pixp) < 10 || (pixp) > 321)



-- A function used to detect if the pixel touches certain tube.


tubePass : Int -> Bool
tubePass x =
    abs (x + 50 - 145) < 5



-- A function used to detect if one tube has passed the pixel. Used to avoid scoring on one tube for more than once.


type alias Model =
    { pixelPosition : Int, timer : Float, set : Bool, oTime : Float, tubeoTime : Float, oPosition : Int, pressoTime : Float, start : Bool, over : Bool, tubeTime : Float, hit : Bool, score : Int, t1 : Bool, t2 : Bool, t3 : Bool, t4 : Bool, t5 : Bool }



-- pixelPosition : Used to record the y-coordinate of the pixel.
-- timer : The pixel's timer. Used to record time, and calculate the pixel's current coordinate.
-- set : A simple boolean to define whether the original time is set or not.
-- oTime : The 'original time' of the pixel, used to calculate the pixel's current position. Changes when the pixel's state changes.
-- tubeoTime : The tubes' original time. Never changes once determined.
-- oPosition : The pixel's 'original position', used to calculate the pixel's current position. Changes when the pixel's state changes.
-- pressoTime : The time when you press a button.
-- start : Determine whether the game has started or not.
-- over : Not used. Replaced by hit, which determines whether the game has ended.
-- tubeTime : The tubes' timer, used to calculate the tubes' position.
-- score : Records score.
-- t1~t5 : A series of variables used to determine whether one tube's score has been added. Used for solving frame-skipping problems.


type Msg
    = Tick Float
    | KeyMsg Key.KeyCode



-- The message can be either time passing or key pressing.


init =
    ( { pixelPosition = 150, timer = 0, set = False, oTime = 0, tubeoTime = 0, oPosition = 300, pressoTime = 0, start = False, over = False, tubeTime = 0, hit = False, score = 0, t1 = False, t2 = False, t3 = False, t4 = False, t5 = False }, Cmd.none )


update msg model =
    case msg of
        KeyMsg code ->
            if model.hit then
                init
                -- If a key is pressed and the game has ended, restart the game.
            else
                let
                    newoTime =
                        model.timer

                    newoPosition =
                        model.pixelPosition

                    newpressoTime =
                        model.timer

                    newHit =
                        ((touch model.pixelPosition 150 (tubeTimeToPos model.tubeTime 0)) || (touch model.pixelPosition 40 (tubeTimeToPos model.tubeTime 810)) || (touch model.pixelPosition 180 (tubeTimeToPos model.tubeTime 1620)) || (touch model.pixelPosition 60 (tubeTimeToPos model.tubeTime 2430)) || (touch model.pixelPosition 120 (tubeTimeToPos model.tubeTime 3240)) || (model.hit)) && (model.tubeTime > 50)

                    -- A series of booleans to find out whether the pixel touches any tube.
                    newScore =
                        if ((tubePass (tubeTimeToPos model.tubeTime 0)) && (not model.t1)) || ((tubePass (tubeTimeToPos model.tubeTime 810)) && (not model.t2)) || ((tubePass (tubeTimeToPos model.tubeTime 1620)) && (not model.t3)) || ((tubePass (tubeTimeToPos model.tubeTime 2430)) && (not model.t4)) || ((tubePass (tubeTimeToPos model.tubeTime 3240)) && (not model.t5)) then
                            --Updates score, avoid the influence of frame-skipping using a special method. It's kind of hard to explain. Try read my code.
                            model.score + 1
                        else
                            model.score

                    newt1 =
                        tubePass (tubeTimeToPos model.tubeTime 0)

                    newt2 =
                        tubePass (tubeTimeToPos model.tubeTime 810)

                    newt3 =
                        tubePass (tubeTimeToPos model.tubeTime 1620)

                    newt4 =
                        tubePass (tubeTimeToPos model.tubeTime 2430)

                    newt5 =
                        tubePass (tubeTimeToPos model.tubeTime 3240)

                    modelN =
                        { model | oPosition = newoPosition, oTime = newoTime, pressoTime = newpressoTime, start = True, hit = newHit, score = newScore, t1 = newt1, t2 = newt2, t3 = newt3, t4 = newt4, t5 = newt5 }
                in
                    ( modelN, Cmd.none )

        Tick time ->
            if model.start == False then
                ( { model | tubeoTime = time }, Cmd.none )
                --If the game hasn't started, don't add to the timer. Keep setting original time as current time to avoid time elapse.
            else if model.hit then
                ( model, Cmd.none )
                --If the game is ended, I don't care about the timer anymore.
            else if (abs (model.pressoTime - model.timer)) > 200 then
                --If the key was pressed in 200ms, then..
                let
                    newoTime =
                        if model.set == False then
                            --If the original time has not been set yet, set it.
                            time
                        else
                            model.oTime

                    --If it's set, update it with model's original time.
                    newtubeTime =
                        time - model.tubeoTime

                    posY =
                        round <| (toFloat (model.oPosition)) + 0.0004 * (time - model.oTime) ^ 2

                    newHit =
                        ((touch model.pixelPosition 150 (tubeTimeToPos model.tubeTime 0)) || (touch model.pixelPosition 40 (tubeTimeToPos model.tubeTime 810)) || (touch model.pixelPosition 180 (tubeTimeToPos model.tubeTime 1620)) || (touch model.pixelPosition 60 (tubeTimeToPos model.tubeTime 2430)) || (touch model.pixelPosition 120 (tubeTimeToPos model.tubeTime 3240)) || (model.hit)) && (model.tubeTime > 50)

                    newScore =
                        if ((tubePass (tubeTimeToPos model.tubeTime 0)) && (not model.t1)) || ((tubePass (tubeTimeToPos model.tubeTime 810)) && (not model.t2)) || ((tubePass (tubeTimeToPos model.tubeTime 1620)) && (not model.t3)) || ((tubePass (tubeTimeToPos model.tubeTime 2430)) && (not model.t4)) || ((tubePass (tubeTimeToPos model.tubeTime 3240)) && (not model.t5)) then
                            model.score + 1
                        else
                            model.score

                    newt1 =
                        tubePass (tubeTimeToPos model.tubeTime 0)

                    newt2 =
                        tubePass (tubeTimeToPos model.tubeTime 810)

                    newt3 =
                        tubePass (tubeTimeToPos model.tubeTime 1620)

                    newt4 =
                        tubePass (tubeTimeToPos model.tubeTime 2430)

                    newt5 =
                        tubePass (tubeTimeToPos model.tubeTime 3240)

                    modelN =
                        { model | pixelPosition = posY, timer = time, set = True, oTime = newoTime, tubeTime = newtubeTime, hit = newHit, score = newScore, t1 = newt1, t2 = newt2, t3 = newt3, t4 = newt4, t5 = newt5 }
                in
                    ( modelN, Cmd.none )
            else if (abs ((abs (model.pressoTime - model.timer)) - 200)) < 20 then
                --When the pixel jumps to the top, its states changes. Need to write a whole new state for it, updating the timers and original positions.
                let
                    newoPosition =
                        model.pixelPosition

                    newoTime =
                        model.timer

                    newtubeTime =
                        time - model.tubeoTime

                    newHit =
                        ((touch model.pixelPosition 150 (tubeTimeToPos model.tubeTime 0)) || (touch model.pixelPosition 40 (tubeTimeToPos model.tubeTime 810)) || (touch model.pixelPosition 180 (tubeTimeToPos model.tubeTime 1620)) || (touch model.pixelPosition 60 (tubeTimeToPos model.tubeTime 2430)) || (touch model.pixelPosition 120 (tubeTimeToPos model.tubeTime 3240)) || (model.hit)) && (model.tubeTime > 50)

                    newScore =
                        if ((tubePass (tubeTimeToPos model.tubeTime 0)) && (not model.t1)) || ((tubePass (tubeTimeToPos model.tubeTime 810)) && (not model.t2)) || ((tubePass (tubeTimeToPos model.tubeTime 1620)) && (not model.t3)) || ((tubePass (tubeTimeToPos model.tubeTime 2430)) && (not model.t4)) || ((tubePass (tubeTimeToPos model.tubeTime 3240)) && (not model.t5)) then
                            model.score + 1
                        else
                            model.score

                    newt1 =
                        tubePass (tubeTimeToPos model.tubeTime 0)

                    newt2 =
                        tubePass (tubeTimeToPos model.tubeTime 810)

                    newt3 =
                        tubePass (tubeTimeToPos model.tubeTime 1620)

                    newt4 =
                        tubePass (tubeTimeToPos model.tubeTime 2430)

                    newt5 =
                        tubePass (tubeTimeToPos model.tubeTime 3240)

                    modelN =
                        { model | oPosition = newoPosition, timer = time, oTime = newoTime, tubeTime = newtubeTime, hit = newHit, score = newScore, t1 = newt1, t2 = newt2, t3 = newt3, t4 = newt4, t5 = newt5 }
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

                    -- s = 1/2*g*t^2 + v0*t, PHYSICS!!
                    newHit =
                        ((touch model.pixelPosition 150 (tubeTimeToPos model.tubeTime 0)) || (touch model.pixelPosition 40 (tubeTimeToPos model.tubeTime 810)) || (touch model.pixelPosition 180 (tubeTimeToPos model.tubeTime 1620)) || (touch model.pixelPosition 60 (tubeTimeToPos model.tubeTime 2430)) || (touch model.pixelPosition 120 (tubeTimeToPos model.tubeTime 3240)) || (model.hit)) && (model.tubeTime > 50)

                    newScore =
                        if ((tubePass (tubeTimeToPos model.tubeTime 0)) && (not model.t1)) || ((tubePass (tubeTimeToPos model.tubeTime 810)) && (not model.t2)) || ((tubePass (tubeTimeToPos model.tubeTime 1620)) && (not model.t3)) || ((tubePass (tubeTimeToPos model.tubeTime 2430)) && (not model.t4)) || ((tubePass (tubeTimeToPos model.tubeTime 3240)) && (not model.t5)) then
                            model.score + 1
                        else
                            model.score

                    newt1 =
                        tubePass (tubeTimeToPos model.tubeTime 0)

                    newt2 =
                        tubePass (tubeTimeToPos model.tubeTime 810)

                    newt3 =
                        tubePass (tubeTimeToPos model.tubeTime 1620)

                    newt4 =
                        tubePass (tubeTimeToPos model.tubeTime 2430)

                    newt5 =
                        tubePass (tubeTimeToPos model.tubeTime 3240)

                    modelN =
                        { model | pixelPosition = posY, timer = time, set = True, oTime = newoTime, tubeTime = newtubeTime, hit = newHit, score = newScore, t1 = newt1, t2 = newt2, t3 = newt3, t4 = newt4, t5 = newt5 }
                in
                    ( modelN, Cmd.none )


subscriptions model =
    Sub.batch [ Anim.times Tick, Key.downs KeyMsg ]



--Subscribe to key downs and time.


view : Model -> Html.Html Msg
view model =
    case model.start of
        False ->
            --The game hasn't started yet. Draw the basics, cover the game scene with a half-transparent white color.
            Html.body [ Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "height", "1000px" ), ( "width", "100%" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
                [ Html.h1 [ align "right", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "font-family", "Comic Sans MS" ) ] ]
                    [ text "==Flappy Pixel==" ]
                , Html.div [ Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ) ] ]
                    [ Html.img
                        [ src "https://i.imgur.com/B6cjYlr.png", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                        []
                    , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "216px" ), ( "left", "500px" ), ( "font-family", "Courier" ) ] ] [ Html.b [] [ Html.text "Press any key to start.." ] ]
                    ]
                , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "100px" ), ( "left", "800px" ) ] ] [ Html.h2 [] [ Html.text "Introduction" ], Html.p [] [ Html.text "This is an Elm copy of the game \"Flappy Bird\". I tried to make the gaming experience as close to the original game as possible. " ], Html.br [] [], Html.h2 [] [ Html.text "Known Issues:" ], Html.ul [] [ Html.li [] [ Html.text "The game is hard. Just as hard as \"Flappy Bird\" is. Don't get mad, it's just a game." ], Html.li [] [ Html.text "The game is tested by my girlfriend and is proved to be playable - theoretically you can reach an infinite score if you are as bored as she is." ], Html.li [] [ Html.text "If your computer or your browser is laggy, the game may skip a few frames from time to time, and that may lead to the system unable to catch your failure. I considered fixing this by making a more strict hitbox system, but.. Why must we hurt each other?" ], Html.li [] [ Html.text "The ", Html.a [ Html.Attributes.href "https://github.com/yunc5/CS1XA3/tree/master/Assign2" ] [ Html.text "code" ], Html.text " itself is a little bit messy. It is because I have to handle many errors caused by frame-skipping and other issues caused by the pysics. I will keep working on the code to make it readable. If you find any part of the code confusing, feel free to contact me." ], Html.li [] [ Html.text "The background may be blank on Safari, but it works fine on any other browser. Apple should take the responsibility." ], Html.li [] [ Html.text "Using spacebar as the control may cause page scrolling, use other keys." ] ] ]
                , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "450px" ), ( "left", "50px" ) ] ] [ Html.h2 [] [ Html.text "Thanks to:" ], Html.ol [] [ Html.li [] [ Html.text "http://www.cas.mcmaster.ca/~dalvescb/Code/SvgAnimation.elm, Curtis D'alves" ], Html.li [] [ Html.text "https://github.com/GuidebeeGameEngine/FlappyBird, guidebee" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ] ] ]
                , svg [ Svg.Attributes.width "768", Svg.Attributes.height "600", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                    [ rect [ x "95", y (toString model.pixelPosition), Svg.Attributes.width "20", Svg.Attributes.height "20", Svg.Attributes.style ("fill:yellow;stroke:orange;stroke-width:3;opacity:0.8") ] []
                    , rect [ x "0", y "0", Svg.Attributes.width "768", Svg.Attributes.height "432", Svg.Attributes.style ("fill:white; opacity:0.5") ] []
                    ]
                ]

        True ->
            --The game starts.
            if model.hit then
                --If it hits the tube, then still draw the whole sh*t, but change the color of the pixel to red and cover the game scene with white color, and the final score.
                let
                    pixY =
                        toString model.pixelPosition

                    posX n =
                        tubeTimeToPos model.tubeTime n
                in
                    Html.body [ Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "height", "1000px" ), ( "width", "100%" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
                        [ Html.h1 [ align "right", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "font-family", "Comic Sans MS" ) ] ]
                            [ text "==Flappy Pixel==" ]
                        , Html.div []
                            [ Html.img
                                [ src "https://i.imgur.com/B6cjYlr.png", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                                []
                            , svg [ Svg.Attributes.width "818", Svg.Attributes.height "432", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "-50px" ) ] ]
                                [ rect [ x "145", y (toString model.pixelPosition), Svg.Attributes.width "20", Svg.Attributes.height "20", Svg.Attributes.style ("fill:red; stroke:orange;stroke-width:3") ] []
                                , rect [ x (toString (posX 0)), y "0", Svg.Attributes.height "150", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 0)), y "240", Svg.Attributes.height "91", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 810)), y "0", Svg.Attributes.height "40", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 810)), y "130", Svg.Attributes.height "201", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 1620)), y "0", Svg.Attributes.height "180", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 1620)), y "270", Svg.Attributes.height "61", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 2430)), y "0", Svg.Attributes.height "60", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 2430)), y "150", Svg.Attributes.height "181", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 3240)), y "0", Svg.Attributes.height "120", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 3240)), y "210", Svg.Attributes.height "121", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x "0", y "0", Svg.Attributes.height "432", Svg.Attributes.width "818", Svg.Attributes.style ("fill:white; opacity:0.6") ] []
                                ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "216px" ), ( "left", "384px" ), ( "font-family", "Courier" ) ] ] [ Html.b [] [ Html.text ("Final Score: " ++ (toString model.score)) ], Html.br [] [], Html.text ("..press any key to reset") ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "100px" ), ( "left", "800px" ) ] ] [ Html.h2 [] [ Html.text "Introduction" ], Html.p [] [ Html.text "This is an Elm copy of the game \"Flappy Bird\". I tried to make the gaming experience as close to the original game as possible. " ], Html.br [] [], Html.h2 [] [ Html.text "Known Issues:" ], Html.ul [] [ Html.li [] [ Html.text "The game is hard. Just as hard as \"Flappy Bird\" is. Don't get mad, it's just a game." ], Html.li [] [ Html.text "The game is tested by my girlfriend and is proved to be playable - theoretically you can reach an infinite score if you are as bored as she is." ], Html.li [] [ Html.text "If your computer or your browser is laggy, the game may skip a few frames from time to time, and that may lead to the system unable to catch your failure. I considered fixing this by making a more strict hitbox system, but.. Why must we hurt each other?" ], Html.li [] [ Html.text "The ", Html.a [ Html.Attributes.href "https://github.com/yunc5/CS1XA3/tree/master/Assign2" ] [ Html.text "code" ], Html.text " itself is a little bit messy. It is because I have to handle many errors caused by frame-skipping and other issues caused by the pysics. I will keep working on the code to make it readable. If you find any part of the code confusing, feel free to contact me." ], Html.li [] [ Html.text "The background may be blank on Safari, but it works fine on any other browser. Apple should take the responsibility." ], Html.li [] [ Html.text "Using spacebar as the control may cause page scrolling, use other keys." ] ] ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "450px" ), ( "left", "50px" ) ] ] [ Html.h2 [] [ Html.text "Thanks to:" ], Html.ol [] [ Html.li [] [ Html.text "http://www.cas.mcmaster.ca/~dalvescb/Code/SvgAnimation.elm, Curtis D'alves" ], Html.li [] [ Html.text "https://github.com/GuidebeeGameEngine/FlappyBird, guidebee" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ] ] ]
                            ]
                        ]
            else
                --If it hasn't hit the tube yet, then draw the whole stuff, print score at bottom.
                let
                    pixY =
                        toString model.pixelPosition

                    posX n =
                        tubeTimeToPos model.tubeTime n
                in
                    Html.body [ Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "height", "1000px" ), ( "width", "100%" ), ( "padding", "0" ), ( "margin", "0" ) ] ]
                        [ Html.h1 [ align "right", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "margin", "0" ), ( "font-family", "Comic Sans MS" ) ] ]
                            [ text "==Flappy Pixel==" ]
                        , Html.div []
                            [ Html.img
                                [ src "https://i.imgur.com/B6cjYlr.png", Html.Attributes.style [ ( "background", "linear-gradient(to bottom, #4dc0c9 0%, #d5d38e 130%)" ), ( "position", "absolute" ), ( "top", "0px" ), ( "left", "0px" ) ] ]
                                []
                            , svg [ Svg.Attributes.width "818", Svg.Attributes.height "432", Html.Attributes.style [ ( "position", "absolute" ), ( "top", "0px" ), ( "left", "-50px" ) ] ]
                                [ rect [ x "145", y (toString model.pixelPosition), Svg.Attributes.width "20", Svg.Attributes.height "20", Svg.Attributes.style ("fill:yellow;stroke:orange;stroke-width:3;opacity:0.8") ] []
                                , rect [ x (toString (posX 0)), y "0", Svg.Attributes.height "150", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 0)), y "240", Svg.Attributes.height "91", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 810)), y "0", Svg.Attributes.height "40", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 810)), y "130", Svg.Attributes.height "201", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 1620)), y "0", Svg.Attributes.height "180", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 1620)), y "270", Svg.Attributes.height "61", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 2430)), y "0", Svg.Attributes.height "60", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 2430)), y "150", Svg.Attributes.height "181", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 3240)), y "0", Svg.Attributes.height "120", Svg.Attributes.width "50", fill "green" ] []
                                , rect [ x (toString (posX 3240)), y "210", Svg.Attributes.height "121", Svg.Attributes.width "50", fill "green" ] []
                                ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "360px" ), ( "left", "50px" ), ( "font-family", "Courier" ) ] ] [ Html.b [] [ Html.text ("Score: " ++ (toString model.score)) ] ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "100px" ), ( "left", "800px" ) ] ] [ Html.h2 [] [ Html.text "Introduction" ], Html.p [] [ Html.text "This is an Elm copy of the game \"Flappy Bird\". I tried to make the gaming experience as close to the original game as possible. " ], Html.br [] [], Html.h2 [] [ Html.text "Known Issues:" ], Html.ul [] [ Html.li [] [ Html.text "The game is hard. Just as hard as \"Flappy Bird\" is. Don't get mad, it's just a game." ], Html.li [] [ Html.text "The game is tested by my girlfriend and is proved to be playable - theoretically you can reach an infinite score if you are as bored as she is." ], Html.li [] [ Html.text "If your computer or your browser is laggy, the game may skip a few frames from time to time, and that may lead to the system unable to catch your failure. I considered fixing this by making a more strict hitbox system, but.. Why must we hurt each other?" ], Html.li [] [ Html.text "The ", Html.a [ Html.Attributes.href "https://github.com/yunc5/CS1XA3/tree/master/Assign2" ] [ Html.text "code" ], Html.text " itself is a little bit messy. It is because I have to handle many errors caused by frame-skipping and other issues caused by the pysics. I will keep working on the code to make it readable. If you find any part of the code confusing, feel free to contact me." ], Html.li [] [ Html.text "The background may be blank on Safari, but it works fine on any other browser. Apple should take the responsibility." ], Html.li [] [ Html.text "Using spacebar as the control may cause page scrolling, use other keys." ] ] ]
                            , Html.div [ Html.Attributes.style [ ( "position", "absolute" ), ( "top", "450px" ), ( "left", "50px" ) ] ] [ Html.h2 [] [ Html.text "Thanks to:" ], Html.ol [] [ Html.li [] [ Html.text "http://www.cas.mcmaster.ca/~dalvescb/Code/SvgAnimation.elm, Curtis D'alves" ], Html.li [] [ Html.text "https://github.com/GuidebeeGameEngine/FlappyBird, guidebee" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ], Html.li [] [ Html.text "https://www.w3schools.com/tags/tag_style.asp" ] ] ]
                            ]
                        ]


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
