{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Colonnade
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Word (Word32)
import Siphon (encodeCsvUtf8)
import qualified Data.ByteString.Char8 as BC8
import qualified Data.Char as Char
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text

main :: IO ()
main = do
  B.writeFile "../data/bazd.csv"
  $ encodeCsvUtf8 bazdColonnade bazdRows

type County = Text
type City = Text
type ZipCode = Word32
type PlusFour = Text

data USState
  = AK
  | AL
  | AR
  | AS -- ^ American Samoa
  | AZ
  | CA
  | CO
  | CT
  | DC -- ^ district of columbia
  | DE
  | FL
  | GA
  | GU -- ^ Guam
  | HI
  | IA
  | ID
  | IL
  | IN
  | KS
  | KY
  | LA
  | MA
  | MD
  | ME
  | MI
  | MN
  | MO
  | MP -- ^ Northern Mariana Islands
  | MS
  | MT
  | NC
  | ND
  | NE
  | NH
  | NJ
  | NM
  | NV
  | NY
  | OH
  | OK
  | OR
  | PA
  | PR -- ^ Puerto Rico
  | RI
  | SC
  | SD
  | TN
  | TX
  | UM -- ^ U.S. Minor Outlying Islands
  | UT
  | VA
  | VI -- ^ U.S. Virgin Islands
  | VT
  | WA
  | WI
  | WV
  | WY
  deriving stock (Show)

data BazdRow = BazdRow
  { state :: USState
  , county :: County
  , city :: City
  , zip_code :: ZipCode
  , plus_four :: Maybe PlusFour
  }

bazdRows :: [BazdRow]
bazdRows = concat
  [ california
  ]

california :: [BazdRow]
california = map
  (\(county, city, zip_code) -> BazdRow
    { state = CA
    , plus_four = Nothing
    , ..
    }
  )
  allCities
  where
    allCities = concat
      [ santaClara
      ]
    santaClara = map
      (\(c, z) -> ("Santa Clara", c, z))
      [ ("Los Altos", 94022)
      , ("Los Altos Hills", 94022)
      , ("Los Altos", 94023)
      , ("Los Altos", 94024)
      , ("Los Altos Hills", 94024)
      , ("Moffett Field", 94035)
      , ("Mountain View", 94035)
      , ("Mountain View", 94039)
      , ("Mountain View", 94040)
      , ("Mountain View", 94041)
      , ("Mountain View", 94042)
      , ("Mountain View", 94043)
      , ("Sunnyvale", 94085)
      , ("Sunnyvale", 94086)
      , ("Sunnyvale", 94087)
      , ("Onizuka Afb", 94088)
      , ("Sunnyvale", 94088)
      , ("Sunnyvale", 94090)
      , ("Palo Alto", 94301)
      , ("Palo Alto", 94302)
      , ("East Palo Alto", 94303)
      , ("Palo Alto", 94303)
      , ("Palo Alto", 94304)
      , ("Stanford", 94305)
      , ("Palo Alto", 94305)
      , ("Palo Alto", 94306)
      , ("Stanford", 94309)
      , ("Palo Alto", 94309)
      , ("Palo Alto", 94310)
      , ("Alviso", 95002)
      , ("Campbell", 95008)
      , ("Campbell", 95009)
      , ("Campbell", 95008)
      , ("Coyote", 95013)
      , ("Monte Vista", 95014)
      , ("Permanente", 95014)
      , ("Cupertino", 95014)
      , ("Cupertino", 95015)
      , ("Gilroy", 95020)
      , ("Gilroy", 95021)
      , ("Holy City", 95026)
      , ("Los Gatos", 95030)
      , ("Monte Sereno", 95030)
      , ("Los Gatos", 95031)
      , ("Los Gatos", 95032)
      , ("Milpitas", 95035)
      , ("Milpitas", 95036)
      , ("Morgan Hill", 95037)
      , ("Morgan Hill", 95038)
      , ("New Almaden", 95042)
      , ("Redwood Estates", 95044)
      , ("San Martin", 95046)
      , ("Santa Clara", 95050)
      , ("Santa Clara", 95051)
      , ("Santa Clara", 95052)
      , ("Santa Clara", 95054)
      , ("Santa Clara", 95055)
      , ("Santa Clara", 95056)
      , ("Saratoga", 95070)
      , ("Saratoga", 95071)
      , ("San Jose", 95101)
      , ("San Jose", 95102)
      , ("San Jose", 95103)
      , ("San Jose", 95106)
      , ("San Jose", 95108)
      , ("San Jose", 95109)
      , ("San Jose", 95110)
      , ("San Jose", 95111)
      , ("San Jose", 95112)
      , ("San Jose", 95113)
      , ("San Jose", 95114)
      , ("San Jose", 95115)
      , ("San Jose", 95116)
      , ("San Jose", 95117)
      , ("San Jose", 95118)
      , ("San Jose", 95119)
      , ("San Jose", 95120)
      , ("San Jose", 95121)
      , ("San Jose", 95122)
      , ("San Jose", 95123)
      , ("San Jose", 95124)
      , ("San Jose", 95125)
      , ("San Jose", 95126)
      , ("San Jose", 95127)
      , ("San Jose", 95128)
      , ("San Jose", 95129)
      , ("San Jose", 95130)
      , ("San Jose", 95131)
      , ("San Jose", 95132)
      , ("San Jose", 95132)
      , ("San Jose", 95133)
      , ("San Jose", 95134)
      , ("San Jose", 95135)
      , ("San Jose", 95136)
      , ("San Jose", 95137)
      , ("San Jose", 95138)
      , ("San Jose", 95139)
      , ("Mount Hamilton", 95140)
      , ("San Jose", 95140)
      , ("San Jose", 95141)
      , ("San Jose", 95142)
      , ("San Jose", 95148)
      , ("San Jose", 95150)
      , ("San Jose", 95151)
      , ("San Jose", 95152)
      , ("San Jose", 95153)
      , ("San Jose", 95154)
      , ("San Jose", 95155)
      , ("San Jose", 95156)
      , ("San Jose", 95157)
      , ("San Jose", 95158)
      , ("San Jose", 95159)
      , ("San Jose", 95160)
      , ("San Jose", 95161)
      , ("San Jose", 95164)
      , ("San Jose", 95170)
      , ("San Jose", 95171)
      , ("San Jose", 95172)
      , ("San Jose", 95173)
      ]

type Column = ByteString

bazdColonnade :: Colonnade Headed BazdRow Column
bazdColonnade = mconcat
  [ headed "state" (BC8.pack . show . state)
  , headed "county" (Text.encodeUtf8 . county)
  , headed "city" (Text.encodeUtf8 . city)
  , headed "zip_code" (BC8.pack . show . zip_code)
  , headed "plus_four" (maybe "" Text.encodeUtf8 . plus_four)
  ]

