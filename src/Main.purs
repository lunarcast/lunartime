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
import Effect (Effect)
import Effect.Class.Console as Console
import React.DOM.Props as React

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

data TextInput
  = NewValue String
  | Submit

app :: forall a. String -> Widget HTML a
app name = do
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
              , P.onKeyEnter $> Submit
              , P.onInput <#> unsafeTargetValue <#> NewValue
              ]
          , playArrow
              [ P.onClick $> Submit
              ]
          ]
      ]
  newName <- case event of
    Submit -> do
      Console.log $ "Submitted " <> name
      pure name
    NewValue name' -> do
      Console.log name'
      pure name'
  app newName

main :: Effect Unit
main = runWidgetInDom "root" (app "")
