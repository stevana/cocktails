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
                                             (^?!), _Just)
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
import           System.Directory           (createDirectoryIfMissing)
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

  post url (object [ "query" .= String "MATCH(n) DETACH DELETE n;" ])

  LT.writeFile (distDir </> cypherFile) . ingredients =<<
    parseYaml "data/ingredients.yaml"

  yaml <- parseYaml "data/cocktails.yaml"

  LT.appendFile (distDir </> cypherFile) (cocktails yaml)
  T.appendFile (distDir </> cypherFile) (recipes yaml)

  cypher <- T.readFile (distDir </> cypherFile)
  void (post url (object [ "query" .= cypher ]))

recipes' :: Value -> LT.Text
recipes' v = mustache (addAlphaNumField "name" v')
  "{{#.}} \
  \{{#ingredients}} \
  \  CREATE ({{name-alpha-num}})-[:CONTAINS \
  \    { amount: {{amount}} \
  \    , unit:   {{unit}}   \
  \    , index:  {{index}}  \
  \    }) \
  \  ->({{name}})\n \
  \{{/ingredients}} \
  \{{/.}}"
  where
  vs :: [(Int, Value)]
  vs = v ^.. values . withIndex
  v' :: Value
  v' = Array $ V.fromList $ map (\(ix, o) -> o & _Object %~ (\u -> u & at "index" ?~ review _Integer (toInteger ix))) vs

recipes :: Value -> Text
recipes (Array vec) = T.unlines $ concatMap go (V.toList vec)
  where
  go :: Value -> [Text]
  go v = flip map (zip ingredients [0..]) $ \(ingredient, ix) ->
           create (v^?! key "name" . _String)
                  (ingredient ^?! key "name"   . _String)
                  (ingredient ^?! key "amount" . _Number)
                  (ingredient ^?! key "unit"   . _String)
                  ix
    where
    ingredients = v ^?! key "ingredients" . _Array . to V.toList

    create :: Text -> Text -> Scientific -> Text -> Int -> Text
    create cname iname amount unit ix = mconcat
      [ "CREATE (", T.filter isAsciiAlphaNum cname, ")-[:CONTAINS "
      , "{amount: ", T.pack $ case floatingOrInteger amount of
                               Left  r -> show r
                               Right i -> show i
      , ", unit: \"", unit, "\""
      , ", index: ", T.pack (show ix)
      , "}]->(", T.filter isAsciiAlphaNum iname, ")"
      ]

cocktails :: Value -> LT.Text
cocktails v = mustache (addAlphaNumField "name" v)
  "{{#.}} \
  \  CREATE ({{name-alpha-num}}:Cocktail \
  \    { name:        \"{{name}}\" \
  \    , timing:      \"{{timing}}\" \
  \    , preparation: \"{{preparation}}\" \
  \    })\n \
  \{{/.}}"

addAlphaNumField :: Text -> Value -> Value
addAlphaNumField field v = v & values . _Object %~
  (\o -> o & at (field <> "-alpha-num") .~
    (o ^. at field & _Just . _String %~ T.filter isAsciiAlphaNum))

ingredients :: Value -> LT.Text
ingredients v = mustache (addAlphaNumField "name" v)
  "{{#.}} \
  \  CREATE ({{name-alpha-num}}:Ingredient \
  \    { name: \"{{name}}\" \
  \    }) \
  \{{/.}}"

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

generateContent :: IO ()
generateContent = process =<< parseYaml "data/queries.yaml"

process :: Value -> IO ()
process v = do
  let menuPairs = [ ("index.html", "all")
                  , ("ingredient-Gin.html", "gin")
                  , ("ingredient-Whiskey.html", "whiskey")
                  , ("ingredient-Rum.html", "rum")
                  , ("ingredient-Vodka.html", "vodka")
                  , ("ingredient-Champagne.html", "champagne")
                  ]

  let menuJson = object
                   ["menu" .= map (\(link, text) ->
                         object [ "link" .= String link
                                , "text" .= String text]) menuPairs]

  forM_ (zip4 names queries paramss templates) $ \(name, query, params, template) -> do
    templateDir <- getDataFileName "data/templates"
    template' <- compileMustacheDir (PName template) templateDir

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
