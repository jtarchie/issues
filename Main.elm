module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, string, int, succeed, field, maybe, bool)
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Navigation
import RouteParser.QueryString as QueryString
import Dict


type alias Issue =
    { title : String
    , url : String
    , number : Int
    , closed : Bool
    , labels : List String
    , assignees : List String
    , milestone : Maybe String
    }

type alias Model =
    { issues : List Issue
    , token : String
    , query : String
    , config : Dict.Dict String (List String)
    }


type Msg
    = Init Navigation.Location
    | ListIssues (Result Http.Error (List Issue))
    | KeyDown Int
    | UpdateQuery String


type Story
    = Feature
    | Bug
    | Chore
    | Release


toSearchGHQuery : String -> String
toSearchGHQuery query =
    String.join ""
        [ "{search(first: 100, query:"
        , Json.Encode.encode 0 <| Json.Encode.string query
        , """, type: ISSUE) {
              edges {
                node {
                  ... on Issue {
                    number
                    title
                    url
                    closed
                    labels(first: 10) {
                      edges {
                        node {
                          name
                        }
                      }
                    }
                    assignees(first: 2) {
                      edges {
                        node {
                          login
                        }
                      }
                    }
                    milestone {
                      title
                    }
                  }
                }
              }
            }
          }
      """
        ]


main : Program Never Model Msg
main =
    Navigation.program Init
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


extractParam : (Navigation.Location, String, String) -> String
extractParam (location, name, default) =
    let
        params =
            QueryString.parse location.search
    in
        case Dict.get name params of
            Just tokens ->
                case tokens of
                    token :: others ->
                        Maybe.withDefault default (Http.decodeUri token)

                    _ ->
                        default

            _ ->
                default


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { issues = []
            , query = extractParam (location, "query", "user:concourse type:issue")
            , token = extractParam (location, "token", "")
            , config = Dict.fromList [
                  ("started", ["workflow: in progress", "workflow: discuss", "workflow: go-around"]),
                  ("finished", ["workflow: completed"])
                ]
            }
    in
        ( model, listIssues model )


decodeMilestone : Decode.Decoder (Maybe String)
decodeMilestone =
    Decode.nullable (Decode.at [ "title" ] string)


decodeLabel : Decode.Decoder String
decodeLabel =
    Decode.at [ "node", "name" ] string


decodeAssignee : Decode.Decoder String
decodeAssignee =
    Decode.at [ "node", "login" ] string


decodeIssue : Decode.Decoder Issue
decodeIssue =
    Decode.at [ "node" ]
        (succeed Issue
            |: (field "title" string)
            |: (field "url" string)
            |: (field "number" int)
            |: (field "closed" bool)
            |: (field "labels" (Decode.at [ "edges" ] (Decode.list decodeLabel)))
            |: (field "assignees" (Decode.at [ "edges" ] (Decode.list decodeAssignee)))
            |: (field "milestone" decodeMilestone)
        )


decodeIssues : Decode.Decoder (List Issue)
decodeIssues =
    Decode.at [ "data", "search", "edges" ] (Decode.list decodeIssue)


listIssues : Model -> Cmd Msg
listIssues model =
    let
        search =
            Json.Encode.encode 0 (Json.Encode.string <| toSearchGHQuery model.query)

        request =
            { method = "POST"
            , headers = [ Http.header "Authorization" ("Bearer " ++ model.token) ]
            , url = "https://api.github.com/graphql"
            , body = Http.stringBody "application/json" ("{\"query\":" ++ search ++ "}")
            , expect = expectJson decodeIssues
            , timeout = Nothing
            , withCredentials = False
            }
                |> Http.request
    in
        Http.send ListIssues request


viewMilestone : Issue -> List (Html msg)
viewMilestone issue =
    case issue.milestone of
        Just s ->
            [ li [ class "label milestone" ] [ text s ] ]

        _ ->
            []


storyType : Issue -> Story
storyType issue =
    if List.member "type: bug" issue.labels then
        Bug
    else if List.member "type: chore" issue.labels then
        Chore
    else
        Feature


storyTypeClass : Issue -> String
storyTypeClass issue =
    case storyType issue of
        Bug ->
            "bug"

        Chore ->
            "chore"

        _ ->
            "feature"


viewLabels : Issue -> List (Html msg)
viewLabels issue =
    List.concat
        [ (List.map
            (\l ->
                li [ class "label" ] [ text l ]
            )
            issue.labels
          )
        , viewMilestone issue
        ]


viewAssignees : Issue -> List (Html msg)
viewAssignees issue =
    if List.isEmpty issue.assignees then
        []
    else
        [ span [ class "assignees" ] [ text <| String.join ", " issue.assignees ] ]


onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
    on "keydown" (Decode.map tagger keyCode)

stateClass : Dict.Dict String (List String) ->  Issue -> String
stateClass config issue =
  if issue.closed then
    "accepted"
  else
    let
        possible = List.filter (\(state,labels) ->
          List.any (\label ->
            List.member label issue.labels
          ) labels
        ) <| Dict.toList config
    in
        case possible of
          (x, _) :: xs -> x
          [] -> "unstarted"

view : Model -> Html Msg
view model =
    div []
        [ div [ class "query" ]
            [ input [ value model.query, autocomplete False, spellcheck False, onKeyDown KeyDown, onInput UpdateQuery ] []
            ]
        , ul [ class "stories" ]
            (List.map
                (\issue ->
                    li [ classList [ ( "story", True ), ( storyTypeClass issue, True ), ((stateClass model.config issue), True) ] ] <|
                        [ a [ href issue.url, target "_blank" ] [ text issue.title ] ]
                            ++ (viewAssignees issue)
                            ++ [ ul [ class "labels" ] <| viewLabels issue ]
                )
                model.issues
            )
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown key ->
            if key == 13 then
                ( model, listIssues model )
            else
                ( model, Cmd.none )

        UpdateQuery query ->
            ( { model | query = query }, Cmd.none )

        Init _ ->
            ( model, Cmd.none )

        ListIssues (Ok issues) ->
            let
                _ =
                    Debug.log "OK" (toString issues)
            in
                ( { model | issues = issues }, Cmd.none )

        ListIssues (Err error) ->
            let
                _ =
                    Debug.log "ERROR" error
            in
                ( model, Cmd.none )
