-- |Functions for fetching player/character data from the lodestone
module Data.Krile.Sharlayan.User
  where

import Network.HTTP
import Network.HTTP.Headers
import Network.HTTP.Base
import Text.HTML.TagSoup
import Data.Time.Clock
import Data.List.Split
import Network.URI          ( parseURI )
import Data.Maybe           ( fromMaybe )
import Network.BufferType   ( BufferOp(..), BufferType(..) )


-- |Data type for stroing details on a character
data Character = Character {
   uid     :: Int            -- ^ The Lodestone character id
  ,name    :: String         -- ^ The character name
  ,world   :: String         -- ^ The world the character is on
  ,face    :: String         -- ^ The URL of the character's mugshot
  ,profile :: String         -- ^ Character profile URL
  ,fc      :: Maybe Integer  -- ^ Free Company's lodestone id (if available)
  ,time    :: Maybe UTCTime  -- ^ The time this character's data was last updated
} deriving (Show)

-- |The User Agent to be used for fetching mobile pages
userAgent :: String
userAgent = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Mobile Safari/537.36"

-- |The root of the URL to be used for player searches
lsSearchRoot :: String
lsSearchRoot = "http://eu.finalfantasyxiv.com/lodestone/character/more/"

-- |The lodestone hostname
lsHost :: String
lsHost = "http://eu.finalfantasyxiv.com"

-- |Searches for a character on Lodestone given a full name and World
findChar :: String -> String -> IO [Character]
findChar n w = parseLodestoneResults $ genQueryURL n w



-- |Helper function to fetch the contents of a URL
openURL :: String -> IO String
openURL url
  = getResponseBody =<< simpleHTTP (mobRequest url)

-- |Helper function to create a request with modified UserAgent
mobRequest :: String -> Request_String
mobRequest urlString
  = case parseURI urlString of
      Nothing   -> error ("mobRequest: Not a valid URL - " ++ urlString)
      Just uri  -> req GET uri
    where
      empty = buf_empty (bufferOps)
      req m uri = Request { rqURI      = uri
                          , rqBody     = empty
                          , rqHeaders  = [ Header HdrContentLength "0"
                                         , Header HdrUserAgent     userAgent
                                         ]
                          , rqMethod   = m
                          }

-- |Takes a player name and world, and returns the full search URL
genQueryURL :: String -> String -> String
genQueryURL n w = lsSearchRoot ++ genQueryString n w

-- |Takes a player name and a world, and returns the query string to search the first page only of lodestone results
genQueryString :: String -> String -> String
genQueryString name world
  = "?order=&q=" ++ escapeName name 
    ++ "&worldname=" ++ world 
    ++ "&classjob=&race_tribe=&page=1"
    where
      escapeName name = map escapeChar name
      escapeChar ' ' = '+'
      escapeChar x   = id x

-- |Searches on the lodestone at a given URL and returns a list of results
parseLodestoneResults :: String -> IO [Character]
parseLodestoneResults url
  = do
    tags <- parseTags <$> openURL url
    let entries = partitions startOfEntry tags
    return $ map toCharRecord entries
      where 
        startOfEntry e = (||) (e ~== TagOpen "div" [("class", "entry")]) 
                              (e ~== TagOpen "div" [("class", "entry last_message")])
        toCharRecord :: [Tag String] -> Character
        toCharRecord t = Character { uid  = getUid t
                                   , name = getName t
                                   , world = getWorld t
                                   , face = getFace t
                                   , profile = getCharProfile t
                                   , fc = Nothing
                                   , time = Nothing}
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
                       . filter (~== TagOpen "a" [("class", "entry__link")])
        getUid :: [Tag String] -> Int
        getUid = read . head . (splitOn "/") . (!! 1) . (splitOn "character/") . getCharProfile

-- |Parses a lodestone character profile page at a given URL and returns an image link and FC identifier
parseProfilePage :: String -> IO (String, Integer)
parseProfilePage url
  = do
    tags <- parseTags <$> openURL url
    return (pictureLoc tags, fcId tags)
      where
        pictureLoc :: [Tag String] -> String
        pictureLoc = (fromAttrib "href") . head . tail . head . sections (~== TagOpen "div" [("class", "character__detail__image")])
        fcPage :: [Tag String] -> String
        fcPage = (fromAttrib "href") . head . filter (~== TagOpen "a" [("class", "entry__freecompany")])
        fcId :: [Tag String] -> Integer
        fcId = read . head . (splitOn "/") . (!! 1) . (splitOn "freecompany/") . fcPage
