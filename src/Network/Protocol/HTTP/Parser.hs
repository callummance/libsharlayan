-- |Helper functions for fetching pages with custom options
module Network.Protocol.HTTP.Parser
  where

import Network.URI          ( parseURI )
import Network.BufferType   ( BufferOp(..), BufferType(..) )
import Network.HTTP
import Network.HTTP.Headers
import Network.HTTP.Base

-- General fetching of webpages
-------------------------------

-- |Helper function to fetch the contents of a URL
openURL :: String -> IO String
openURL url
  = getResponseBody =<< simpleHTTP (getRequest url)


-- Functions for handling requests using a mobile user agent
------------------------------------------------------------

-- |The user agent to be used for mobile sites
mobUserAgent :: String
mobUserAgent = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/56.0.2924.87 Mobile Safari/537.36"

-- |Helper function to fetch the contents of a URL
mobOpenURL :: String -> IO String
mobOpenURL url
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
                                         , Header HdrUserAgent     mobUserAgent
                                         ]
                          , rqMethod   = m
                          }
