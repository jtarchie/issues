module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Http exposing (..)
import Json.Decode as Decode exposing (Decoder, string, int, list, succeed, field)
import Json.Decode.Extra exposing ((|:))
import Json.Encode

type alias Issue =
  {title: String
  ,url: String
  ,number: Int
  }

type Msg = ListIssues (Result Http.Error (List Issue))

main =
  Html.program
    { init = init "fa58c00b2fe79f6d9e244507520e2ee9762f55ee"
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

subscriptions : List Issue -> Sub Msg
subscriptions _ = Sub.none

init : String -> (List Issue, Cmd Msg)
init token =
  ([], listIssues token)

decodeIssue : Decode.Decoder Issue
decodeIssue =
  succeed Issue
    |: (field "title" string)
    |: (field "url" string)
    |: (field "number" int)

decodeIssues : Decode.Decoder (List Issue)
decodeIssues =
  Decode.at ["data", "search", "edges"] (Decode.list decodeIssue)

listIssues : String -> Cmd Msg
listIssues token =
  let
      search = Json.Encode.encode 0 (Json.Encode.string """{
            search(first: 10, query: \"user:concourse type:issue\", type: ISSUE) {
              edges {
                node {
                  ... on Issue {
                    number
                    title
                    url
                  }
                }
              }
            }
          }
      """)
      request =
        {method = "POST"
        ,headers = [ Http.header "Authorization" ("Bearer " ++ token) ]
        ,url = "https://api.github.com/graphql"
        ,body = Http.stringBody "application/json" ("{\"query\":" ++ search ++ "}")
        ,expect = expectJson decodeIssues
        ,timeout = Nothing
        ,withCredentials = False
        } |> Http.request
  in
    Http.send ListIssues request

view : List Issue -> Html msg
view issues =
  div [title (toString issues)] [
    div [] (List.map (\i -> text i.title ) issues)
  ]

update : Msg -> List Issue -> ( List Issue, Cmd Msg )
update msg models =
  case msg of
    ListIssues (Ok issues) ->
      let _ = Debug.log "OK" (toString issues)
      in (issues, Cmd.none)
    _ ->
      let _ = Debug.log "ERROR" "something"
      in ( [], Cmd.none )
