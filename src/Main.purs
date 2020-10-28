module Main where

import Prelude
import Concur.Core (Widget)
import Concur.Core.Props (Props)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props (unsafeTargetValue)
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Concur.React.SVG as S
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import Data.String as String
import Data.String.CodeUnits as CodeUnits
import Data.String.Regex (test)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Validation.Semigroup (V, invalid, unV)
import Effect (Effect)
import Effect.Class.Console as Console
import React.DOM.Props as React

---------- Types
data Action
  = NewValue String
  | Submit

type State
  = { name :: String
    , errors :: Array ValidationError
    }

---------- Validation Stuff
data ValidationError
  = TooLong { max :: Int, actual :: Int }
  | TooShort { min :: Int, actual :: Int }
  | Empty
  | ForbiddenChars (Array Char)

instance showValidationError :: Show ValidationError where
  show = case _ of
    TooLong { max } -> "The name cannot contain more than " <> show max <> " characters"
    TooShort { min } -> "The name cannot contain less than " <> show min <> " characters"
    Empty -> "The name cannot be empty"
    ForbiddenChars chars ->
      "The "
        <> g "character" "characters"
        <> " "
        <> joinWith ", " (CodeUnits.singleton <$> chars)
        <> " "
        <> g "is" "are"
        <> " forbidden"
      where
      g s s' = if singular then s else s'

      singular = Array.length chars == 1

type Validator a
  = a -> V (Array ValidationError) a

-- | Fail with a single error
fail :: forall e a. e -> V (Array e) a
fail e = invalid [ e ]

tooLong :: Int -> Validator String
tooLong max s =
  if actual > max then
    fail $ TooLong { max, actual }
  else
    pure s
  where
  actual = String.length s

tooShort :: Int -> Validator String
tooShort min s = case actual of
  0 -> fail Empty
  _
    | actual < min -> fail $ TooShort { min, actual }
    | otherwise -> pure s
  where
  actual = String.length s

-- uIswalnum <<< toCharCode
safeChars :: Validator String
safeChars s = case failed of
  [] -> pure s
  _ -> fail $ ForbiddenChars $ Array.nub failed
  where
  failed =
    flip Array.mapMaybe chars \{ char, result } ->
      if result then
        Nothing
      else
        Just char

  chars =
    CodeUnits.toCharArray s
      <#> \char ->
          { char
          , result: test regex $ CodeUnits.singleton char
          }

  regex = unsafeRegex "^[a-zA-Z0-9_ ]*$" noFlags

validate :: Validator String
validate s = ado
  tooShort 3 s
  tooLong 20 s
  safeChars s
  in s

---------- Ui stuff
-- | I made this using https://mbylstra.github.io/html-to-elm/
playArrow :: forall t. Array (Props React.Props t) -> Widget HTML t
playArrow props =
  S.svg
    ( [ P.viewBox "0 0 24 24"
      , P.className "play-arrow"
      , P.unsafeMkProp "xmlms" "http://www.w3.org/2000/svg"
      ]
        <> props
    )
    [ S.path [ P.d "M0 0h24v24H0z", P.fill "none" ]
        []
    , S.path [ P.d "M8 5v14l11-7z" ]
        []
    ]

app :: forall a. State -> Widget HTML a
app { name, errors } = do
  let
    passing = Array.null errors
  event <-
    D.div [ P._id "page-center" ]
      [ D.p [ P._id "title" ]
          [ D.span [ P._id "lunar" ] [ D.text "Lunar" ]
          , D.span [ P._id "time" ] [ D.text "time" ]
          ]
      , D.p [ P._id "description" ]
          [ D.text "The higher the time & the better the score!"
          ]
      , D.div [ P._id "input-container" ]
          [ D.input
              [ P._id "input"
              , P.placeholder "Enter your name"
              , P.className if passing then "success" else "failure"
              , P.onKeyEnter $> Submit
              , P.onInput <#> unsafeTargetValue <#> NewValue
              ]
          , playArrow
              [ P.onClick $> Submit
              , P._id "play-arrow"
              , P.className if passing then "success" else "failure"
              ]
          ]
      , D.div [ P._id "errors" ] $ errors <#> \err -> D.div [ P.className "error" ] [ D.text $ show err ]
      ]
  newName <- case event of
    Submit -> do
      Console.log $ "Submitted " <> name
      pure name
    NewValue name' -> do
      Console.log name'
      pure name'
  let
    result = validate newName
  app { name: newName, errors: unV identity (const []) result }

---------- Run the app
main :: Effect Unit
main = runWidgetInDom "root" (app { errors: [], name: "" })
