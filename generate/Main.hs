{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Main (main) where

import Data.Maybe (fromMaybe)
import Data.Word (Word32)
import Streaming (Stream, Of(..))
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Streaming as BSM
import qualified Data.List as List
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified System.IO as IO

main :: IO ()
main = do
  encodeRows "../data/bazd.csv" transduceBazd
  $ S.each bazdRows

concatMapM :: (Monad m)
  => (a -> Stream (Of b) m x)
  -> Stream (Of a) m r
  -> Stream (Of b) m r
concatMapM f = id
  . S.concats
  . S.mapsM (\(a :> s) -> pure (f a *> pure s))

transduceBazd :: ()
  => Stream (Of BazdRow) IO r
  -> Stream (Of String) IO r
transduceBazd s = do
  S.yield "state,county,city,zip_code,plus_four"
  S.yield "\n"
  flip concatMapM s $ \row -> do
    S.yield (rowToString row)
    S.yield "\n"

encodeRows :: ()
  => FilePath
  -> (forall r. Stream (Of a) IO r -> Stream (Of String) IO r)
  -> Stream (Of a) IO ()
  -> IO ()
encodeRows path g s=
  IO.withFile path IO.WriteMode $ \output -> do
    id
    $ BSM.hPut output
    $ BSM.fromChunks
    $ S.map BC8.pack
    $ g
    $ s

type County = String
type City = String
type ZipCode = Word32
type PlusFour = String

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
  deriving stock (Show)

rowToString :: BazdRow -> String
rowToString BazdRow{..} = List.intercalate ","
  [ show state
  , county
  , city
  , show zip_code
  , fromMaybe "" plus_four
  ]

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
  allCounties
  where
    allCounties = concat
      [ alameda
      , santaClara
      ]
    alameda = map
      (\(c, z) -> ("Alameda", c, z))
      [ ("Alameda", 94501)
      , ("Alameda", 94502)
      , ("Fremont", 94536)
      , ("Fremont", 94537)
      , ("Fremont", 94538)
      , ("Fremont", 94539)
      , ("Hayward", 94540)
      , ("Hayward", 94541)
      , ("Hayward", 94542)
      , ("Hayward", 94543)
      , ("Hayward", 94544)
      , ("Hayward", 94545)
      , ("Castro Valley", 94546)
      , ("Hayward", 94546)
      , ("Livermore", 94550)
      , ("Livermore", 94551)
      , ("Castro Valley", 94552)
      , ("Hayward", 94552)
      , ("Fremont", 94555)
      , ("Mount Eden", 94557)
      , ("Hayward", 94557)
      , ("Newark", 94560)
      , ("Pleasanton", 94566)
      , ("Dublin", 94568)
      , ("Pleasanton", 94568)
      , ("San Leandro", 94577)
      , ("San Leandro", 94578)
      , ("San Leandro", 94579)
      , ("San Leandro", 94580)
      , ("Sunol", 94586)
      , ("Union City", 94587)
      , ("Pleasanton", 94588)
      , ("Oakland", 94601)
      , ("Oakland", 94602)
      , ("Piedmont", 94602)
      , ("Oakland", 94603)
      , ("Oakland", 94604)
      , ("Oakland", 94605)
      , ("Oakland", 94606)
      , ("Oakland", 94607)
      , ("Emeryville", 94608)
      , ("Oakland", 94608)
      , ("Oakland", 94609)
      , ("Oakland", 94610)
      , ("Piedmont", 94611)
      , ("Oakland", 94612)
      , ("Oakland", 94613)
      , ("Oakland", 94614)
      , ("Oakland", 94615)
      , ("Oakland", 94617)
      , ("Oakland", 94618)
      , ("Piedmont", 94618)
      , ("Oakland", 94619)
      , ("Oakland", 94620)
      , ("Piedmont", 94620)
      , ("Oakland", 94621)
      , ("Oakland", 94623)
      , ("Oakland", 94624)
      , ("Oakland", 94661)
      , ("Emeryville", 94662)
      , ("Oakland", 94662)
      , ("Berkeley", 94701)
      , ("Berkeley", 94702)
      , ("Berkeley", 94703)
      , ("Berkeley", 94704)
      , ("Berkeley", 94705)
      , ("Albany", 94706)
      , ("Kensington", 94706)
      , ("Berkeley", 94706)
      , ("Kensington", 94707)
      , ("Berkeley", 94707)
      , ("Kensington", 94708)
      , ("Berkeley", 94708)
      , ("Berkeley", 94709)
      , ("Albany", 94710)
      , ("Berkeley", 94710)
      , ("Berkeley", 94712)
      , ("Berkeley", 94720)
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
