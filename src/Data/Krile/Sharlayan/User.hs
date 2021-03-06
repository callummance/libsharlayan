-- |Functions for fetching player/character data from the lodestone
module Data.Krile.Sharlayan.User (Character(..), findChar, expandCharacter)
  where

import Text.HTML.TagSoup
import Network.Protocol.HTTP.Parser
import Control.Monad.TakeWhileM
import Data.Time.Clock
import Data.List.Split
import Data.Maybe           ( fromMaybe )


-- |Data type for stroing details on a character
data Character = Character {
   uid     :: Int            -- ^ The Lodestone character id
  ,name    :: String         -- ^ The character name
  ,world   :: String         -- ^ The world the character is on
  ,face    :: String         -- ^ The URL of the character's mugshot
  ,profile :: String         -- ^ Character profile URL
  ,time    :: UTCTime        -- ^ The time this character's data was last updated
  ,fc      :: Maybe Integer  -- ^ Free Company's lodestone id (if available)
} deriving (Show)

-- |The root of the URL to be used for player searches
lsSearchRoot :: String
lsSearchRoot = "http://eu.finalfantasyxiv.com/lodestone/character/more/"

-- |The lodestone hostname
lsHost :: String
lsHost = "http://eu.finalfantasyxiv.com"

-- |Searches for a character on Lodestone given a full name and World
findChar :: String -> String -> IO [Character]
findChar n w = fmap concat $ takeWhileM (not . null) $ map parseLodestoneResults $ genQueryURL n w




-- |Takes a player name and world, and returns a list of search URLs
genQueryURL :: String -> String -> [String]
genQueryURL n w = map (((++) lsSearchRoot) . genQueryString n w) [1..]

-- |Takes a player name and a world, and returns the query string to search the first page only of lodestone results
genQueryString :: String -> String -> Int -> String
genQueryString name world page
  = "?order=&q=" ++ escapeName name 
    ++ "&worldname=" ++ world 
    ++ "&classjob=&race_tribe=&page="
    ++ (show page)
    where
      escapeName name = map escapeChar name
      escapeChar ' ' = '+'
      escapeChar x   = id x

-- |Searches on the lodestone at a given URL and returns a list of results
parseLodestoneResults :: String -> IO [Character]
parseLodestoneResults url
  = do
    tags <- parseTags <$> mobOpenURL url
    let entries = partitions startOfEntry tags
    mapM toCharRecord entries
      where 
        startOfEntry e = (||) (e ~== TagOpen "div" [("class", "entry")]) 
                              (e ~== TagOpen "div" [("class", "entry last_message")])
        toCharRecord :: [Tag String] -> IO Character
        toCharRecord t = do
          curTime <- getCurrentTime
          return Character { uid  = getUid t
                           , name = getName t
                           , world = getWorld t
                           , face = getFace t
                           , profile = getCharProfile t
                           , fc = Nothing
                           , time = curTime }
        getWorld :: [Tag String] -> String
        getWorld = innerText . (take 2) . head . sections (~== TagOpen "p" [("class", "entry__world")])
        getName :: [Tag String] -> String
        getName = innerText . (take 2) . head . sections (~== TagOpen "p" [("class", "entry__name")])
        getFace :: [Tag String] -> String
        getFace = (fromAttrib "src") . head
                . (filter (~== TagOpen "img" []))
                . (dropWhile (~/= TagOpen "div" [("class", "entry__chara__face")]))
        getCharProfile :: [Tag String] -> String
        getCharProfile = head 
                       . (map (((++) lsHost) . fromAttrib "href")) 
                       . filter (~== TagOpen "a" [("class", "entry__chara__link")])
        getUid :: [Tag String] -> Int
        getUid = read . head . (splitOn "/") . (!! 1) . (splitOn "character/") . getCharProfile

-- |Updates a character data object with information parsed from their profile page
expandCharacter :: Character -> IO (Character)
expandCharacter char
  = do
    let profileURL = profile char
    (pic, fcid) <- parseProfilePage profileURL
    time <- getCurrentTime
    return Character { uid = uid char
                     , name = name char
                     , world = world char
                     , face = face char
                     , profile = profile char
                     , fc = Just fcid
                     , time = time }

-- |Parses a lodestone character profile page at a given URL and returns an image link and FC identifier
parseProfilePage :: String -> IO (String, Integer)
parseProfilePage url
  = do
    tags <- parseTags <$> mobOpenURL url
    return (pictureLoc tags, fcId tags)
      where
        pictureLoc :: [Tag String] -> String
        pictureLoc = (fromAttrib "href") . head . tail . head . sections (~== TagOpen "div" [("class", "character__detail__image")])
        fcPage :: [Tag String] -> String
        fcPage = (fromAttrib "href") . head . filter (~== TagOpen "a" [("class", "entry__freecompany")])
        fcId :: [Tag String] -> Integer
        fcId = read . head . (splitOn "/") . (!! 1) . (splitOn "freecompany/") . fcPage
