module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, string, int, succeed, field, maybe)
import Json.Decode.Extra exposing ((|:))
import Json.Encode
import Navigation
import RouteParser.QueryString as QueryString
import Dict


type alias Issue =
    { title : String
    , url : String
    , number : Int
    , labels : List String
    , assignees : List String
    , milestone : Maybe String
    }


type alias Query =
    { user : String
    , labels : List String
    , milestone : Maybe String
    }


type alias Model =
    { issues : List Issue
    , token : String
    , query : Query
    }


type Msg
    = Init Navigation.Location
    | ListIssues (Result Http.Error (List Issue))


type Story
    = Feature
    | Bug
    | Chore
    | Release


toSearchString : Query -> String
toSearchString query =
    String.join " "
        ([ "user:" ++ query.user
         , "type:issue"
         ]
            ++ List.map (\label -> "label:" ++ label) query.labels
        )


toSearchGHQuery : Query -> String
toSearchGHQuery query =
    let
        search =
            toSearchString query
    in
        String.join ""
            [ "{search(first: 100, query:\""
            , search
            , """", type: ISSUE) {
              edges {
                node {
                  ... on Issue {
                    number
                    title
                    url
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


extractToken : Navigation.Location -> String
extractToken location =
    let
        params =
            QueryString.parse location.search
    in
        case Dict.get "token" params of
            Just tokens ->
                case tokens of
                    token :: others ->
                        token

                    _ ->
                        ""

            _ ->
                ""


init : Navigation.Location -> ( Model, Cmd Msg )
init location =
    let
        model =
            { issues = []
            , query =
                { user = "concourse"
                , labels = []
                , milestone = Nothing
                }
            , token = extractToken location
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


view : Model -> Html msg
view model =
    ul [ class "stories" ]
        (List.map
            (\issue ->
                li [ classList [ ( "story", True ), ( storyTypeClass issue, True ) ] ] <|
                    [ a [href issue.url, target "_blank" ] [text issue.title] ]
                        ++ (viewAssignees issue)
                        ++ [ ul [ class "labels" ] <| viewLabels issue ]
            )
            model.issues
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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
