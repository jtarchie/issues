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
    , milestone : Maybe String
    }


type Msg
    = Noop Navigation.Location
    | ListIssues (Result Http.Error (List Issue))

type Story = Feature | Bug | Chore | Release

main : Program Never (List Issue) Msg
main =
    Navigation.program Noop
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }


init : Navigation.Location -> ( List Issue, Cmd Msg )
init location =
    let
        params =
            Debug.log "Parsing token" location.search |> QueryString.parse
    in
        case Dict.get "token" params of
            Just tokens ->
                case tokens of
                    token :: others ->
                        ( [], listIssues token )

                    _ ->
                        ( [], Cmd.none )

            _ ->
                ( [], Cmd.none )


decodeMilestone : Decode.Decoder (Maybe String)
decodeMilestone =
    Decode.nullable (Decode.at [ "title" ] string)


decodeLabel : Decode.Decoder String
decodeLabel =
    Decode.at [ "node", "name" ] string


decodeIssue : Decode.Decoder Issue
decodeIssue =
    Decode.at [ "node" ]
        (succeed Issue
            |: (field "title" string)
            |: (field "url" string)
            |: (field "number" int)
            |: (field "labels" (Decode.at [ "edges" ] (Decode.list decodeLabel)))
            |: (field "milestone" decodeMilestone)
        )


decodeIssues : Decode.Decoder (List Issue)
decodeIssues =
    Decode.at [ "data", "search", "edges" ] (Decode.list decodeIssue)


listIssues : String -> Cmd Msg
listIssues token =
    let
        search =
            Json.Encode.encode 0 (Json.Encode.string """{
            search(first: 100, query: "user:concourse type:issue", type: ISSUE) {
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
                    milestone {
                      title
                    }
                  }
                }
              }
            }
          }
      """)

        request =
            { method = "POST"
            , headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
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
    Bug -> "bug"
    Chore -> "chore"
    _ -> "feature"

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


view : List Issue -> Html msg
view issues =
    ul [ class "stories" ]
        (List.map
            (\i ->
                li [ classList [ ("story", True), (storyTypeClass i, True)] ]
                    [ text i.title
                    , ul [ class "labels" ] <| viewLabels i
                    ]
            )
            issues
        )


update : Msg -> List Issue -> ( List Issue, Cmd Msg )
update msg models =
    case msg of
        Noop _ ->
            ( [], Cmd.none )

        ListIssues (Ok issues) ->
            let
                _ =
                    Debug.log "OK" (toString issues)
            in
                ( issues, Cmd.none )

        ListIssues (Err error) ->
            let
                _ =
                    Debug.log "ERROR" error
            in
                ( [], Cmd.none )
