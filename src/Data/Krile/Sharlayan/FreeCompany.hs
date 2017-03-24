-- |Free Company details and rankings
module Data.Krile.Sharlayan.FreeCompany (FreeCompany(..), fetchFC, parseFCPage)
  where
 
import Data.List.Split
import Data.Time.Clock
import Network.Protocol.HTTP.Parser
import Text.HTML.TagSoup

-- |Data type for storing FC details
data FreeCompany = FreeCompany {
    fid       :: Integer    -- ^ The FC's Lodestone identifier
  , fcName    :: String     -- ^ The Free Company's name
  , world     :: String     -- ^ The world upon which the FC resides
  , fcURL     :: String     -- ^ The URL of the FC's profile on Lodestone
  , tUpdated  :: UTCTime    -- ^ The time the FC's details were last updated
  , cBoard    :: String     -- ^ The company board message
  , members   :: Int        -- ^ Number of active members
  , ranks     :: (Int, Int) -- ^ The monthly and weekly rankings of the FC
} deriving Show


-- |The root of the URL to be used for player searches
lsFcRoot :: String
lsFcRoot = "http://eu.finalfantasyxiv.com/lodestone/freecompany/"

-- |Creates a new FC object from an FC ID
fetchFC :: Integer -> IO FreeCompany
fetchFC = parseFCPage . ((++) lsFcRoot) . show


-- |Creates a new FC object from a Lodestone URL
parseFCPage :: String -> IO FreeCompany
parseFCPage url
  = do
    tags <- parseTags <$> mobOpenURL url
    getFC tags url
    where
      getFC :: [Tag String] -> String -> IO FreeCompany
      getFC ts u = do
        updateTime <- getCurrentTime
        return FreeCompany {
            fid = calcId u
          , fcName = getName ts
          , world = getWorld ts
          , fcURL = u
          , tUpdated = updateTime
          , cBoard = getBoard ts
          , members = getMembers ts
          , ranks = getRanks ts
        }
      calcId :: String -> Integer
      calcId = read . head . (splitOn "/") . (!! 1) . (splitOn "freecompany/")
      getWeekly :: [Tag String] -> Int
      getWeekly = read . head . words . innerText . (take 2) . dropWhile (~/= TagOpen "td" []) . dropWhile (~/= TagText "Weekly Rank:")
      getMonthly :: [Tag String] -> Int
      getMonthly = read . head . words . innerText . (take 2) . dropWhile (~/= TagOpen "td" []) . dropWhile (~/= TagText "Monthly Rank:")
      getRanks :: [Tag String] -> (Int, Int)
      getRanks ts = (getMonthly ts, getWeekly ts)
      getMembers :: [Tag String] -> Int
      getMembers = read . innerText . (take 2) . dropWhile (~/= TagOpen "p" []) . dropWhile (~/= TagText "Active Members")
      getWorld :: [Tag String] -> String
      getWorld = unwords . words . innerText . (take 2) . head . tail . sections (~== TagOpen "p" [("class", "entry__world")])
      getBoard :: [Tag String] -> String
      getBoard = innerText . (take 2) . head . sections (~== TagOpen "div" [("class", "freecompany__text freecompany__text__slogan")])
      getGc :: [Tag String] -> String
      getGc = innerText . (take 2) . head . sections (~== TagOpen "p" [("class", "entry__world")])
      getName :: [Tag String] -> String
      getName = innerText . (take 2) . head . sections (~== TagOpen "h2" [("class", "entry__name")])
