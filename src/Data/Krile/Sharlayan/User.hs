-- |Functions for fetching player/character data from the lodestone
module Data.Krile.Sharlayan.User
  where

import Network.HTTP
import Text.HTML.TagSoup

-- |The root of the URL to be used for player searches
lsSearchRoot :: String
lsSearchRoot = "http://eu.finalfantasyxiv.com/lodestone/character/"






-- |Helper function to fetch the contents of a URL
openURL :: String -> IO String
openURL url
  = getResponseBody =<< simpleHTTP (getRequest url)

-- |Takes a player name and a world, and returns the query string to search the lodestone
genQueryString :: String -> String -> String
genQueryString name world
  = "?q=" ++ escapeName name ++ "&worldname=" ++ world ++ "&classjob=&race_tribe=&order="
    where
      escapeName name = map escapeChar name
      escapeChar ' ' = '+'
      escapeChar x   = id x

-- |Searches on the lodestone at a given URL and returns a list of results
parseLodestoneResults :: String -> IO ()
parseLodestoneResults url
  = undefined
