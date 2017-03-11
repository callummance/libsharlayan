-- |Functions for scraping news data from the lodestone
module Data.Krile.Sharlayan.News (fetchNews)
  where

import Network.HTTP
import Text.HTML.TagSoup

-- |The page that needed to be fetched for news scraping
lsNewsPage :: String
lsNewsPage = "http://eu.finalfantasyxiv.com/lodestone/news"

-- |Fetch a list of titles and links from the default news URL
fetchNews :: IO [(String, String)]
fetchNews = parseLodestoneNews lsNewsPage






-- |Helper function to fetch the contents of a URL
openURL :: String -> IO String
openURL url
  = getResponseBody =<< simpleHTTP (getRequest url)

-- |Parse the lodestone news page to generate a list of title-url tuples
parseLodestoneNews :: String -> IO [(String, String)]
parseLodestoneNews url
  = do
    tags <- parseTags <$> openURL url
    let entries = concat $ map (take 4) $ partitions findNewsEntries tags
    let titles = map (fromTagText . head . (drop 1)) $ partitions extractTitles entries
    let links = map (fromAttrib "href") $ filter extractTitles entries
    return $ zip titles links
      where 
        findNewsEntries = (~== TagOpen "li" [("class","news__content__list--header clearfix")])
        extractTitles = (~== TagOpen "a" [("class","link")])
