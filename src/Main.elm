module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes as Attrs exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , int
        , list
        , map4
        , string
        )
import Json.Decode exposing (null)



-- Model


type alias Model =
    { isRequestsPackageShown : Bool
    , posts : List Post
    , errorMessage : Maybe String
    , requestName : String
    }



-- API information


type alias Post =
    { packageName : String
    , packageReleases : List String
    , relatedUrls : List String
    , dependencies : Maybe (List String)
    }



-- Post Decoder


postDecoder : Decoder Post
postDecoder =
    map4 Post
        (Decode.at [ "info", "name" ] <| Decode.string)
        (Decode.at [ "releases" ] (Decode.keyValuePairs (Decode.succeed ()) |> Decode.map (List.map Tuple.first)))
        (Decode.at [ "info", "project_urls" ] (Decode.keyValuePairs string |> Decode.map (List.map Tuple.second)))
        (Decode.at [ "info", "requires_dist" ] (Decode.maybe (Decode.list Decode.string)))



-- Http call


httpCommand : String -> Cmd Msg
httpCommand name =
    Http.get
        { url = "https://pypi.org/pypi/" ++ name ++ "/json"
        , expect = Http.expectJson DataReceived (Decode.map List.singleton postDecoder)
        }



-- Initial model


init : () -> ( Model, Cmd Msg )
init _ =
    ( { isRequestsPackageShown = False
      , posts = []
      , errorMessage = Nothing
      , requestName = ""
      }
    , Cmd.none
    )



-- Messages


type Msg
    = ShowRequestsPackage String
    | SendHttpRequest
    | DataReceived (Result Http.Error (List Post))



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ShowRequestsPackage name ->
            ( { model | isRequestsPackageShown = True }
            , httpCommand name
            )

        SendHttpRequest ->
            ( model, httpCommand "" )

        DataReceived (Ok posts) ->
            ( { model
                | posts = posts
                , errorMessage = Nothing
              }
            , Cmd.none
            )

        DataReceived (Err httpError) ->
            ( { model
                | errorMessage = Just (buildErrorMessage httpError)
              }
            , Cmd.none
            )



-- Error Messages


buildErrorMessage : Http.Error -> String
buildErrorMessage httpError =
    case httpError of
        Http.BadUrl message ->
            message

        Http.Timeout ->
            "Server is taking too long to respond. Please try again later."

        Http.NetworkError ->
            "Unable to reach server."

        Http.BadStatus statusCode ->
            "Request failed with status code: " ++ String.fromInt statusCode

        Http.BadBody message ->
            message



-- View


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ Attrs.class "packagesWrapper" ]
            [ Html.div [ Attrs.class "package" ]
                [ Html.button [ onClick (ShowRequestsPackage "requests") ] [ Html.text "Requests" ] ]
            , Html.div [ Attrs.class "package" ]
                [ Html.button [ onClick (ShowRequestsPackage "pendulum") ] [ Html.text "Pendulum" ] ]
            , Html.div [ Attrs.class "package" ]
                [ Html.button [ onClick (ShowRequestsPackage "xhtml2pdf") ] [ Html.text "xhtml2pdf" ] ]
            ]
        , Html.div []
            [ Html.h1 [] [ Html.text "Select a package to see the relevant information: " ]
            , if model.isRequestsPackageShown then
                viewPostsOrError model

              else
                Html.h1 [] []
            ]
        ]



-- View Package Infromation


viewPackageInformation : Post -> Html Msg
viewPackageInformation post =
    Html.div []
        [ Html.h2 [] [ Html.text "Package Name: " ]
        , Html.p [] [ Html.text post.packageName ]
        , Html.h2 [] [ Html.text "Available versions: " ]
        , post.packageReleases
            |> List.map (\l -> li [] [ text l ])
            |> ul []
        , Html.h2 [] [ Html.text "Related Links: " ]
        , post.relatedUrls
            |> List.map (\l -> li [] [ text l ])
            |> ul []
        , Html.h2 [] [ Html.text "Dependencies: " ]
        , case post.dependencies of
           Just value ->
                value
                    |> List.map (\l -> li [] [ text l ])
                    |> ul []
           Nothing ->
                text "No dependencies"
        ]



-- View request error


viewPostsOrError : Model -> Html Msg
viewPostsOrError model =
    case model.errorMessage of
        Just message ->
            viewError message

        Nothing ->
            List.map viewPackageInformation model.posts |> Html.div []



-- Error message


viewError : String -> Html Msg
viewError errorMessage =
    let
        errorHeading =
            "Couldn't fetch data at this time."
    in
    div []
        [ h3 [] [ text errorHeading ]
        , text ("Error: " ++ errorMessage)
        ]



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
