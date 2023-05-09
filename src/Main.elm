module Main exposing (main)

import Browser
import Browser.Dom as Dom exposing (..)
import Browser.Events exposing (onAnimationFrame)
import Html exposing (Html, a, div, h1, li, nav, node, option, select, text, textarea, ul)
import Html.Attributes exposing (class, classList, id, spellcheck, style, value)
import Html.Events exposing (on, onInput)
import Html.Lazy
import Json.Decode as Json
import Parser
import SyntaxHighlight as SH
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( initModel, Task.perform (always MeasureFontRequested) (Task.succeed ()) )
        , view = view
        , update = update
        , subscriptions = always (Sub.batch [ onAnimationFrame (always Frame) ])
        }



-- Model


type alias Model =
    { scroll : Scroll
    , languageModel : LanguageModel
    , lineCount : Maybe Int
    , highlight : HighlightModel
    , autoComplete : Bool
    , fontSize : FontSize
    , cursor : Cursor
    }


type alias Cursor =
    { x : Float
    , y : Float
    , col : Int
    , row : Int
    , pos : Int
    }


initModel : Model
initModel =
    { scroll = Scroll 0 0
    , languageModel = initLanguageModel hqlExample
    , lineCount = Just 1
    , highlight = HighlightModel (Just SH.Add) 1 3
    , autoComplete = False
    , fontSize = { width = 10, height = 10 }
    , cursor = { x = 0, y = 0, col = 0, row = 0, pos = 0 }
    }


type alias Scroll =
    { top : Int
    , left : Int
    }


type alias LanguageModel =
    { code : String
    , scroll : Scroll
    , highlight : HighlightModel
    }


initLanguageModel : String -> LanguageModel
initLanguageModel codeStr =
    { code = codeStr
    , scroll = Scroll 0 0
    , highlight = initHighlightModel
    }


type alias HighlightModel =
    { mode : Maybe SH.Highlight
    , start : Int
    , end : Int
    }


initHighlightModel : HighlightModel
initHighlightModel =
    { mode = Nothing
    , start = 0
    , end = 0
    }


hqlExample : String
hqlExample =
    """uid := userid |
    groupBy(uid, function=[
{statuscode = 200 | count(as=status200)},
{statuscode = 500 | count(as=status500)}
])
| test(status500 > status200)
| a or b and c
"""


type alias FontSize =
    { width : Float, height : Float }



-- Update


type Msg
    = NoOp
    | MeasureFontRequested
    | MeasureFontReceived (Result Error FontSize)
    | SetText String
    | OnScroll Scroll
    | Frame
    | AutoCompleteToggled
    | AutoCompleteOff
    | Move Int Int


keyDecoder : Json.Decoder Msg
keyDecoder =
    Json.map2
        (\k c ->
            case ( k, c ) of
                ( " ", True ) ->
                    AutoCompleteToggled

                ( "Escape", _ ) ->
                    AutoCompleteOff

                _ ->
                    NoOp
        )
        (Json.field "key" Json.string)
        (Json.field "ctrlKey" Json.bool)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ languageModel } as model) =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        SetText codeStr ->
            languageModel
                |> (\m -> { m | code = codeStr })
                |> updateLangModel model
                |> (\a -> ( a, Cmd.none ))

        OnScroll scroll ->
            ( { model | scroll = scroll }
            , Cmd.none
            )

        Move start end ->
            let
                cursor_ : Cursor
                cursor_ =
                    cursor model end
            in
            if start == end then
                ( { languageModel
                    | highlight =
                        { mode = Just SH.Highlight
                        , start = cursor_.row - 1
                        , end = cursor_.row
                        }
                  }
                    |> updateLangModel { model | cursor = cursor_ }
                , Cmd.none
                )

            else
                ( { languageModel
                    | highlight =
                        { mode = Nothing
                        , start = 0
                        , end = 0
                        }
                  }
                    |> updateLangModel model
                , Cmd.none
                )

        Frame ->
            languageModel
                |> (\m -> { m | scroll = model.scroll })
                |> updateLangModel model
                |> (\a -> ( a, Cmd.none ))

        AutoCompleteToggled ->
            ( { model | autoComplete = not model.autoComplete }
            , Cmd.none
            )

        MeasureFontRequested ->
            ( model
            , Dom.getElement "font-measure"
                |> Task.map (\{ element } -> { width = element.width, height = element.height })
                |> Task.attempt MeasureFontReceived
            )

        MeasureFontReceived measureResult ->
            case measureResult of
                Ok measuredSize ->
                    ( { model | fontSize = measuredSize }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        AutoCompleteOff ->
            ( { model | autoComplete = False }
            , Cmd.none
            )


updateLangModel : Model -> LanguageModel -> Model
updateLangModel model langModel =
    { model | languageModel = langModel }


cursor : Model -> Int -> Cursor
cursor model position =
    let
        lines =
            model.languageModel.code
                |> String.left position
                |> String.split "\n"

        row =
            lines |> List.length

        y =
            toFloat (row - 2) * model.fontSize.height

        col =
            case List.reverse lines of
                hd :: _ ->
                    String.length hd + 1

                [] ->
                    0

        x =
            toFloat col * model.fontSize.width + 20
    in
    { x = x, y = y, row = row, col = col, pos = position }



-- View


inlineCss : Html msg
inlineCss =
    node "style" [] [ text inlineRawCss ]


view : Model -> Browser.Document Msg
view model =
    { title = "Elm Syntax Highlight - Demo"
    , body =
        [ inlineCss
        , h1 [] [ text "Elm text editor" ]
        , div []
            [ textareaStyle
            , SH.useTheme SH.oneDark
            , viewLanguage toHtmlPython model
            ]
        ]
    }


viewAutoComplete : Cursor -> Bool -> Html msg
viewAutoComplete cursor_ visible =
    if visible then
        select
            [ style "left" (String.fromInt (round cursor_.x) ++ "px" |> Debug.log "x")
            , style "top" (String.fromInt (round cursor_.y) ++ "px" |> Debug.log "y")
            ]
            [ option [] [ text "Mjallo" ]
            , option [] [ text "Dallo" ]
            , option [] [ text "Ballo" ]
            ]
        --nav
        --    [ style "position" "absolute"
        --    , style "z-index" "1000"
        --    , style "left" (String.fromInt (round cursor_.x) ++ "px" |> Debug.log "x")
        --    , style "top" (String.fromInt (round cursor_.y) ++ "px" |> Debug.log "y")
        --    ]
        --    [ ul []
        --        [ li [] [ text "Mjallo" ]
        --        , li [] [ text "Dallo" ]
        --        , li [] [ text "Ballo" ]
        --        ]
        --    ]

    else
        Html.span [] []


textareaStyle : Html msg
textareaStyle =
    let
        style a b =
            Html.node "style"
                []
                [ text
                    (String.join "\n"
                        [ ".textarea {caret-color: " ++ a ++ ";}"
                        , ".textarea::selection { background-color: " ++ b ++ "; }"
                        ]
                    )
                ]
    in
    style "#f8f8f2" "rgba(255,255,255,0.2)"


viewLanguage : (Maybe Int -> String -> HighlightModel -> Html Msg) -> Model -> Html Msg
viewLanguage parser ({ lineCount } as model) =
    div
        [ classList
            [ ( "container", True )
            , ( "elmsh", True )
            ]
        ]
        [ div
            [ class "view-container"
            , style "transform"
                ("translate("
                    ++ String.fromInt -model.languageModel.scroll.left
                    ++ "px, "
                    ++ String.fromInt -model.languageModel.scroll.top
                    ++ "px)"
                )
            , style "will-change" "transform"
            ]
            [ Html.Lazy.lazy3 parser
                lineCount
                model.languageModel.code
                model.languageModel.highlight
            ]
        , viewTextarea model.languageModel.code
        , viewFontMeasure
        , viewAutoComplete model.cursor model.autoComplete
        ]


viewFontMeasure : Html Msg
viewFontMeasure =
    div [ id "font-measure" ] [ text "A" ]


viewTextarea : String -> Html Msg
viewTextarea codeStr =
    textarea
        [ value codeStr
        , classList
            [ ( "textarea", True )
            , ( "textarea-lc", True )
            ]
        , onInput SetText
        , on "keydown" keyDecoder
        , spellcheck False
        , Html.Events.on "click" updateSelection
        , Html.Events.on "keyup" updateSelection

        --, Html.Events.on "select" updateSelection
        , Html.Events.on "scroll"
            (Json.map2 Scroll
                (Json.at [ "target", "scrollTop" ] Json.int)
                (Json.at [ "target", "scrollLeft" ] Json.int)
                |> Json.map OnScroll
            )
        ]
        []


updateSelection : Json.Decoder Msg
updateSelection =
    Json.map2 Move
        (Json.at [ "target", "selectionStart" ] Json.int)
        (Json.at [ "target", "selectionEnd" ] Json.int)



-- Helpers function for Html.Lazy.lazy


toHtmlPython : Maybe Int -> String -> HighlightModel -> Html Msg
toHtmlPython =
    toHtml SH.python


toHtml : (String -> Result (List Parser.DeadEnd) SH.HCode) -> Maybe Int -> String -> HighlightModel -> Html Msg
toHtml parser maybeStart str hlModel =
    parser str
        |> Result.map (SH.highlightLines hlModel.mode hlModel.start hlModel.end)
        |> Result.map (SH.toBlockHtml maybeStart)
        |> Result.mapError Parser.deadEndsToString
        |> (\result ->
                case result of
                    Result.Ok a ->
                        a

                    Result.Err x ->
                        text x
           )



-- Options


inlineRawCss : String
inlineRawCss =
    """
body {
  margin: 0 auto 20px auto;
  max-width: 650px;
  line-height: 1.6;
  font-size: 18px;
  color: #444;
  padding: 0 10px;
  text-align: center;
  background-color: #fafafa;
}

h1,
h2,
h3 {
  line-height: 1.2;
}

h1 {
  padding-bottom: 0;
  margin-bottom: 0;
}

h1 small {
  font-size: 1rem;
  color: #888;
}

.subheading {
  margin-top: 0;
}

ul {
  text-align: left;
}

.container {
  position: relative;
  overflow: hidden;
  padding: 0;
  margin: 0;
  text-align: left;
}

.textarea,
.view-container {
  box-sizing: border-box;
  font-size: 1rem;
  line-height: 1.2;
  width: 100%;
  height: 100%;
  height: 250px;
  font-family: monospace;
  letter-spacing: normal;
  word-spacing: normal;
  padding: 0;
  margin: 0;
  border: 0;
  background: transparent;
  white-space: pre;
}

#font-measure {
  font-family: monospace;
  visibility: hidden;
  position: absolute;
  white-space: pre;
}

.textarea {
  color: rgba(0, 0, 0, 0);
  resize: none;
  z-index: 2;
  position: relative;
  padding: 10px;
}

.textarea-lc {
  padding-left: 70px;
}

.textarea:focus {
  outline: none;
}

.view-container {
  position: absolute;
  top: 0;
  left: 0;
  pointer-events: none;
  z-index: 1;
}

/* Elm Syntax Highlight CSS */
pre.elmsh {
  padding: 10px;
  margin: 0;
  text-align: left;
  overflow: auto;
}

code.elmsh {
  padding: 0;
}

.elmsh-line:before {
  content: attr(data-elmsh-lc);
  display: inline-block;
  text-align: right;
  width: 40px;
  padding: 0 20px 0 0;
  opacity: 0.3;
}

/* Demo specifics */
pre.elmsh {
  overflow: visible;
}

select {
    position: absolute;
    z-index: 1000;
    margin: 10px;
}
"""
