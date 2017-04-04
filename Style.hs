{-# LANGUAGE OverloadedStrings #-}

module Style where

import Clay

invoiceStyle :: Css
invoiceStyle = do

  body ? fontSize (px 30)

  ".invoice-box" ? do
    sym margin auto
    sym padding (pct 20)
    fontSize (pct 100)
    color "#222"
    lineHeight (px 40)
    fontFamily ["Helvetica Neue", "Helvetica"] [sansSerif]

    table ? do
      width (pct 100)
      lineHeight inherit
      textAlign $ alignSide sideLeft

      td ? sym padding (pct 1)

      tr ? do
        td#nthChild "3" ? do
          textAlign $ alignSide sideRight
        td#nthChild "4" ? do
          textAlign $ alignSide sideRight

      tr#".top" ? table ? do

        td#nthChild "2" ? do
          textAlign $ alignSide sideRight
          width (px 400)

        fontSize (pct 80)
        lineHeight (px 30)
        color "#444"
        td ? paddingBottom (px 20)


      tr#".information" ? table ? do
        td ? do
          lineHeight (px 45)
          paddingBottom (pct 40)
        td#nthChild "2" ? do
          textAlign $ alignSide sideRight
          width (px 400)

      tr#".heading" ? td ? do
          backgroundColor "#eee"
          borderBottom solid (px 1) "#ddd"
          fontWeight bold

      tr#".details" ? td ? do
          paddingBottom (pct 20)

      tr#".item" ? td ? do
          lineHeight (pct 80)
          fontSize (pct 80)
          td ? do
            borderBottom solid (px 1) "#eee"

      tr#".item"#".last" ? td ? do
            borderBottom none (px 0) "#000"

      tr#".total" ? td ? do
          borderTop solid (px 2) "#eee"
          fontWeight bold
