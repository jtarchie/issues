module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, string, int, succeed, field, maybe)
import Json.Decode.Extra exposing ((|:))
import Json.Encode


type alias Issue =
    { title : String
    , url : String
    , number : Int
    , labels : List String
    }


type Msg
    = ListIssues (Result Http.Error (List Issue))


main : Program Never (List Issue) Msg
main =
    Html.program
        { init = init "token"
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


subscriptions : List Issue -> Sub Msg
subscriptions _ =
    Sub.none


init : String -> ( List Issue, Cmd Msg )
init token =
    ( [], listIssues token )


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
        )


decodeIssues : Decode.Decoder (List Issue)
decodeIssues =
    Decode.at [ "data", "search", "edges" ] (Decode.list decodeIssue)


listIssues : String -> Cmd Msg
listIssues token =
    let
        search =
            Json.Encode.encode 0 (Json.Encode.string """{
            search(first: 10, query: "user:concourse type:issue", type: ISSUE) {
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


view : List Issue -> Html msg
view issues =
    ul [ class "stories" ]
        (List.map
            (\i ->
                li [ class "story" ]
                    [ text i.title
                    , ul [ class "labels" ]
                        (List.map
                            (\l ->
                                li [ class "label" ] [ text l ]
                            )
                            i.labels
                        )
                    ]
            )
            issues
        )


update : Msg -> List Issue -> ( List Issue, Cmd Msg )
update msg models =
    case msg of
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
