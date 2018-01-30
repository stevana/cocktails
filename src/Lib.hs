{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib
    ( startDb
    , waitForDbToBeReady
    , setupDb
    , generateContent
    ) where

import           Control.Concurrent         (threadDelay)
import           Control.Exception          (SomeException, catch)
import           Control.Lens               (at, folded, ifolded, re, review,
                                             to, traversed, withIndex, (%~),
                                             (&), (.~), (?~), (^.), (^..), (^?),
                                             (^?!), (^@..), _Just)
import           Control.Monad
import           Data.Aeson                 as Json
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isAlphaNum, isAscii)
import qualified Data.HashMap.Lazy          as HM
import           Data.List                  (zip4)
import           Data.Scientific
import           Data.Semigroup             ((<>))
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T
import qualified Data.Text.Lazy             as LT
import           Data.Text.Lazy.Encoding
import qualified Data.Text.Lazy.IO          as LT
import           Data.Vector                (Vector)
import qualified Data.Vector                as V
import           Data.Yaml
import           Network.Wreq
import           System.Directory           (copyFile, createDirectoryIfMissing)
import           System.Exit                (ExitCode (..))
import           System.FilePath            ((<.>), (</>))
import           System.Process             (rawSystem)
import           Text.Microstache

import           Paths_cocktails

------------------------------------------------------------------------

neo4jUrl, url, cypherFile :: String

neo4jUrl   = "http://localhost:7474"
url        = "http://localhost:7474/db/data/cypher"
cypherFile = "cocktails.cypher"

distDir :: FilePath
distDir = "dist"

------------------------------------------------------------------------

startDb :: IO ()
startDb = do
  exitCode <- rawSystem "docker"
    [ "run", "-d"
    , "--publish=7474:7474"
    , "--publish=7687:7687"
    , "--env=NEO4J_AUTH=none"
    , "neo4j:3.3.1"
    ]
  case exitCode of
    ExitSuccess      -> return ()
    ExitFailure code ->
      error ("startDb: failed to start database, exit code: " ++ show code)

waitForDbToBeReady :: IO ()
waitForDbToBeReady = do
  retry 30 (isDbAvailable `catch` (\(_ :: SomeException) -> return False))
  where
  isDbAvailable :: IO Bool
  isDbAvailable = do
    r <- get neo4jUrl
    return (r ^. responseStatus . statusCode . to (== 200))

  retry :: Int -> IO Bool -> IO ()
  retry 0 _ = error "waitForDbToBeReady: database is not available"
  retry n io = do
    b <- io
    if b
      then return ()
      else do
        threadDelay 1000000
        retry (n - 1) io

------------------------------------------------------------------------

parseYaml :: FilePath -> IO Value
parseYaml fp = do
  eyaml <- decodeFileEither =<< getDataFileName fp
  case eyaml of
    Left  err  -> error (show err)
    Right yaml -> return yaml

mustache :: Value -> LT.Text -> LT.Text
mustache v t = case compileMustacheText "pname" t of
  Left  err      -> error (show err)
  Right template -> case renderMustacheW template v of
    ([],       t') -> t'
    (warnings, _)  -> error (unlines (map displayMustacheWarning warnings))

setupDb :: IO ()
setupDb = do

  createDirectoryIfMissing True distDir

  -- Clear database.
  post url (object [ "query" .= String "MATCH(n) DETACH DELETE n;" ])

  post url (object [ "query" .= String
      "CREATE CONSTRAINT ON (i:Ingredient) ASSERT i.ingredient IS UNIQUE;" ])

  post url (object [ "query" .= String
      "CREATE CONSTRAINT ON (c:Cocktail) ASSERT c.name IS UNIQUE;" ])

  LT.writeFile (distDir </> cypherFile) . ingredients =<<
    parseYaml "data/ingredients.yaml"

  yaml <- parseYaml "data/cocktails.yaml"

  LT.appendFile (distDir </> cypherFile) (cocktails yaml)
  LT.appendFile (distDir </> cypherFile) (recipes yaml)

  cypher <- T.readFile (distDir </> cypherFile)
  void (post url (object [ "query" .= cypher ]))

recipes :: Value -> LT.Text
recipes v = mustache (addAlphaNumField "name" v')
  "{{#.}}\
  \{{#ingredients}}\
  \CREATE ({{name-alpha-num}})-[:CONTAINS\n\
  \  { amount: {{amount}}\n\
  \  , unit:   \"{{unit}}\"\n\
  \  , index:  {{index}}\n\
  \  }]->\
  \({{ingredient-alpha-num}})\n\
  \{{/ingredients}}\
  \{{/.}}"
  where
  v' = v & values . key "ingredients" . values . _Object %~
            (\o -> o & at "ingredient-alpha-num" .~
              (o ^. at "ingredient" & _Just . _String %~ T.filter isAsciiAlphaNum))
         & values . key "ingredients" . _Array %~ addIndexField

addIndexField :: Vector Value -> Vector Value
addIndexField vec = V.zipWith
  (\ix o -> o & _Object . at "index" ?~ ix^.re _Integer)
  (V.fromList [0 .. toInteger (V.length vec - 1)])
  vec

cocktails :: Value -> LT.Text
cocktails v = mustache (addAlphaNumField "name" v)
  "{{#.}}\
  \CREATE ({{name-alpha-num}}:Cocktail\n\
  \  { name:        \"{{name}}\"\n\
  \  , timing:      \"{{timing}}\"\n\
  \  , preparation: \"{{preparation}}\"\n\
  \  , taste:       \"{{taste}}\"\n\
  \  })\n\
  \{{/.}}"

addAlphaNumField :: Text -> Value -> Value
addAlphaNumField field v = v & values . _Object %~
  (\o -> o & at (field <> "-alpha-num") .~
    (o ^. at field & _Just . _String %~ T.filter isAsciiAlphaNum))

ingredients :: Value -> LT.Text
ingredients v = mustache (addAlphaNumField "ingredient" v)
  "{{#.}}\
  \CREATE ({{ingredient-alpha-num}}:Ingredient\
  \  { ingredient: \"{{ingredient}}\" })\n\
  \{{/.}}"

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

generateContent :: IO ()
generateContent = process =<< parseYaml "data/queries.yaml"

makeMenu :: Value -> Value
makeMenu v =
  v & _Object . at "menu" . _Just . values %~
        (\o -> o & _Object . at "items" . _Just . values %~ (\o' ->
                     addFieldIfItDoesntExist "link" (makeLink o o') o'))
  where
  makeLink o o' =
    o^._Object . at "category" . _Just . _String
    <> "-" <>
    o'^._Object. at "text" . _Just . _String . to (T.filter isAsciiAlphaNum)
    <> ".html"

addFieldIfItDoesntExist :: Text -> Text -> Value -> Value
addFieldIfItDoesntExist field value object =
  object & _Object . at field %~ maybe (Just (String value)) Just

process :: Value -> IO ()
process v = do
  menuJson <- makeMenu <$> parseYaml "data/menu.yaml"

  css <- getDataFileName "data/style.css"
  copyFile css (distDir </> "style.css")

  forM_ (zip4 names queries paramss templates) $ \(name, query, params, template) -> do
    template' <- compileMustacheFile =<< getDataFileName "data/templates/site.mustache"

    if V.null params
    then do
      r <- post url (object [ "query"  .= query
                            , "params" .= object []
                            ])
      let json = r ^?! responseBody . key "data" . values . values
          bs   = Json.encode json
          fp   = distDir </> T.unpack name
      -- BS.putStrLn bs
      BS.writeFile (fp <.> ".json") bs
      LT.writeFile (fp <.> "html") (renderMustache template' (json `mergeValue` menuJson))
    else do
      forM_ params $ \param -> do
        r <- post url (object
               [ "query"  .= query
               , "params" .= param
               ])
        let json    = r ^?! responseBody . key "data" . values . values
            bs      = Json.encode json
            [value] = param ^?! _Object . to HM.elems
            vstr    = value ^?! _String
                    . to (T.unpack . T.filter isAsciiAlphaNum)
            fp      = distDir </> T.unpack name <> "-" <> vstr
        -- BS.putStrLn bs
        BS.writeFile (fp <.> "json") bs
        let json' = json `mergeValue` menuJson
        LT.writeFile (fp <.> "html") (renderMustache template' json')

  where
  names     = v ^.. values . key "name"     . _String
  queries   = v ^.. values . key "query"    . _String
  paramss   = v ^.. values . key "params"   . _Array
  templates = v ^.. values . key "template" . _String

mergeValue :: Value -> Value -> Value
mergeValue (Object hm1) (Object hm2) = Object (hm1 <> hm2)
