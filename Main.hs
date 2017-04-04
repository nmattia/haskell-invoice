{-# LANGUAGE ExtendedDefaultRules       #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ViewPatterns               #-}

module Main where

import Control.Arrow      (second)
import Control.Lens
import Data.List          (intersperse)
import Data.Monoid        ((<>))
import Lucid
import Style              (invoiceStyle)
import System.Environment (getArgs)

import qualified Clay
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map                   as Map
import qualified Data.Text                  as T
import qualified Data.Text.Format           as TF
import qualified Data.Text.Lazy             as TL
import qualified Data.Text.Lazy.Builder     as TL

data Currency = CHF | USD | EUR deriving (Enum, Bounded, Ord, Eq)

newtype Value = Value { val :: Double } deriving (Fractional, Num)

type Item = (T.Text, T.Text, T.Text, T.Text)

data Billed = Billed
  { billedPaid    :: (Currency, Value)
  , billedCharged :: (Currency, Value) }

type Address = [T.Text]
type Amount = (Currency, Value)

type Qty = Int

data Entry = Entry
  { entryQuantity  :: Qty
  , entryDescr     :: T.Text
  , entryUnitPrice :: [Billed] }

data Invoice = Invoice
  { invoiceHeader  :: [T.Text]
  , invoiceFrom    :: Address
  , invoiceTo      :: Address
  , invoiceSummary :: T.Text
  , invoiceEntries :: [Entry] }

invoiceItems :: Invoice -> [Item]
invoiceItems = concatMap entryItems . invoiceEntries

entryCharged :: Entry -> [Amount]
entryCharged e = billedToCharged <$> entryUnitPrice e
  where billedToCharged b = second (\x -> qty * x) (billedCharged b)
        qty = fromIntegral $ entryQuantity e

invoiceTotal :: Invoice -> [Amount]
invoiceTotal inv = mergeAmounts $ concatMap entryCharged (invoiceEntries inv)

mergeAmounts :: [Amount] -> [Amount]
mergeAmounts = Map.toList . Map.fromListWith (+)

tshow :: (Show a) => a -> T.Text
tshow = T.pack . show

printValue :: Value -> T.Text
printValue (Value x) = TL.toStrict $ TL.toLazyText $ TF.fixed 2 x

currencyCode :: Currency -> T.Text
currencyCode CHF = "CHF"
currencyCode USD = "USD"
currencyCode EUR = "EUR"

singleCurrencyBill :: Amount -> Billed
singleCurrencyBill a = Billed {billedCharged = a, billedPaid = a}

main :: IO ()
main = head <$> getArgs >>= \case
  "oct-2016" -> L8.putStrLn $ renderBS invoiceOctober2016
  str -> error $ "No such invoice: " <> str

mkLines, mkAddress, mkHeader:: [T.Text] -> Html ()
mkLines ls = sequence_ . intersperse (br_ []) $ toHtml <$> ls
mkAddress = mkLines
mkHeader = mkLines

mkTds :: [T.Text] -> Html ()
mkTds = mapM_ (td_ . toHtml)

mkItem :: Item -> Html ()
mkItem (a,b,c,d) = tr_ [class_ "item"] . mkTds $ [a,b,c,d]

entryItems :: Entry -> [Item]
entryItems e = items & (ix 0 . ix 0) .~ descr
  where items = toItem <$> entryUnitPrice e
        descr | qty == 1 = entryDescr e
              | otherwise = tshow qty <> " " <> entryDescr e
        toItem Billed {billedPaid, billedCharged} = ( ""
                          , let (c, v) = billedPaid in currencyCode c <> " " <> printValue v
                          , let (c, _v) = billedCharged in currencyCode c
                          , printValue $ fromIntegral qty * snd billedCharged )
        qty = entryQuantity e

mkTotal :: [Amount] -> Html ()
mkTotal (mergeAmounts -> (x:xs)) = do
  tr_ [class_ "total"] $ mkTds [ "Total", "", currencyCode $ fst x, printValue $ snd x]
  mapM_ toRow xs
  where toRow (c,t)= tr_ [class_ "total"] $ mkTds [ "", "", currencyCode c, printValue t]
mkTotal _ =
  tr_ [class_ "total"] $ mkTds [ "Total", "", "", "0"]


ourAddress :: [T.Text]
ourAddress =
  [ "The Consultants"
  , "Bahnhofstrasse 33"
  , "8001 Zurich"
  , "Switzerland" ]

theirAddress :: [T.Text]
theirAddress  =
  [ "The Client"
  , "83 Lansdowne Drive"
  , "14 Aldington Court"
  , "E8 3HB London"
  , "England" ]

renderInvoice :: Invoice -> Html ()
renderInvoice invoice =
  doctypehtml_ $ do
    head_ $ style_ $ TL.toStrict $ Clay.render invoiceStyle
    body_ $
      div_ [class_ "invoice-box"] $ do
        table_ $ do
          tr_ [class_ "top"] $ td_ [colspan_ "2"] $ table_ $
                tr_ $  do
                  td_ $ return ()
                  td_ $ mkHeader $ invoiceHeader invoice

          tr_ [class_ "information"] $ td_ [colspan_ "2"] $ table_ $ tr_ $ do
            td_ $ mkAddress $ invoiceFrom invoice
            td_ $ mkAddress $ invoiceTo invoice
        div_ $ do h3_ "Summary"
                  p_ . toHtml $ invoiceSummary invoice
        table_ $ do
          colgroup_ $
            col_ [span_ "4"]
          tr_ [class_ "heading"] $
            mkTds [ "Item"
                  , "Unit price"
                  , ""
                  , "Price" ]

          mapM_ mkItem $ invoiceItems invoice

          mkTotal $ invoiceTotal invoice

invoiceOctober2016 :: Html ()
invoiceOctober2016 = renderInvoice Invoice
  { invoiceHeader = [ "Invoice #: 5015"
                    , "Created: 7th of November 2016"
                    , "Due: 7th of December 2016" ]
  , invoiceFrom = ourAddress
  , invoiceTo   = theirAddress
  , invoiceSummary = "CHF 2,440 and USD 132 is now due for IT support, maintenance, development, travel expenses and material acquisition during October 2016."
  , invoiceEntries = [expensiveHours, internalHours, hotel] }
  where
    expensiveHours = Entry
      { entryQuantity = 8
      , entryDescr = "expensive hours on brand new system"
      , entryUnitPrice = [singleCurrencyBill (CHF, 240)] }
    internalHours = Entry
      { entryQuantity = 11
      , entryDescr = "cheap hours in America"
      , entryUnitPrice = [singleCurrencyBill  (USD, 12)] }
    hotel = Entry
        { entryQuantity = 1
        , entryDescr = "Night at the hotel"
        , entryUnitPrice = [singleCurrencyBill (CHF, 520.00)] }
