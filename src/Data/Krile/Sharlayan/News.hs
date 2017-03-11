module Data.Krile.Sharlayan.News (fetchNews)
  where

import Network.HTTP
import Text.HTML.TagSoup


lsNewsPage :: String
lsNewsPage = "http://eu.finalfantasyxiv.com/lodestone/news"

fetchNews :: IO [(String, String)]
fetchNews = parseLodestone lsNewsPage

openURL :: String -> IO String
openURL url
  = getResponseBody =<< simpleHTTP (getRequest url)

parseLodestone :: String -> IO [(String, String)]
parseLodestone url
  = do
    tags <- parseTags <$> openURL url
    let entries = concat $ map (take 4) $ partitions findNewsEntries tags
    let titles = map (fromTagText . head . (drop 1)) $ partitions extractTitles entries
    let links = map (fromAttrib "href") $ filter extractTitles entries
    return $ zip titles links
      where 
        findNewsEntries = (~== TagOpen "li" [("class","news__content__list--header clearfix")])
        extractTitles = (~== TagOpen "a" [("class","link")])
