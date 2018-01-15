{-# LANGUAGE LambdaCase          #-}
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
import           Control.Lens               (to, traversed, (^.), (^..), (^?),
                                             (^?!))
import           Control.Monad
import           Data.Aeson                 as Json
import           Data.Aeson.Lens
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Char                  (isAlphaNum, isAscii)
import qualified Data.HashMap.Lazy          as HM
import           Data.List                  (zip4)
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

setupDb :: IO ()
setupDb = do
  let query :: Text
      query = "MATCH(n) DETACH DELETE n;"
  r <- post url (object [ "query"  .= query ])

  createDirectoryIfMissing False distDir

  ingredientsFile <- getDataFileName "data/ingredients.yaml"
  decodeFileEither ingredientsFile >>= \case
    Left  err  -> error (show err)
    Right yaml -> do
      let ingredientsCypher = ingredients yaml
      -- T.putStrLn (T.unlines ingredientsCypher)
      T.writeFile (distDir </> cypherFile) (T.unlines ingredientsCypher)

  cocktailsFile <- getDataFileName "data/cocktails.yaml"
  decodeFileEither cocktailsFile >>= \case
    Left  err  -> error (show err)
    Right yaml -> do
      let cocktailsCypher = cocktails yaml
      -- T.putStrLn (T.unlines cocktailsCypher)
      T.appendFile (distDir </> cypherFile) (T.unlines cocktailsCypher)
      let recipesCypher = recipes yaml
      -- T.putStrLn (T.unlines recipesCypher)
      T.appendFile (distDir </> cypherFile) (T.unlines recipesCypher)

  cypher <- T.readFile (distDir </> cypherFile)

  r <- post url (object [ "query" .= cypher ])
  return ()

recipes :: Value -> [Text]
recipes (Array vec) = concatMap go (V.toList vec)
  where
  go :: Value -> [Text]
  go v = flip map (zip ingredients [0..]) $ \(ingredient, ix) ->
           create (v^?! key "name" . _String)
                  (ingredient ^?! key "name"   . _String)
                  (ingredient ^?! key "amount" . _Integer)
                  (ingredient ^?! key "unit"   . _String)
                  ix
    where
    ingredients = v ^?! key "ingredients" . _Array . to V.toList

    create :: Text -> Text -> Integer -> Text -> Int -> Text
    create cname iname amount unit ix = mconcat
      [ "CREATE (", T.filter isAsciiAlphaNum cname, ")-[:CONTAINS "
      , "{amount: ", T.pack (show amount)
      , ", unit: \"", unit, "\""
      , ", index: ", T.pack (show ix)
      , "}]->(", T.filter isAsciiAlphaNum iname, ")"
      ]

cocktails :: Value -> [Text]
cocktails v = flip map (zip3 names timings preparations) $ \(name, timing, preparation) ->
  mconcat
    [ "CREATE (", T.filter isAsciiAlphaNum name, ":Cocktail "
    , "{ name: \"", name
    , "\", timing: \"", timing
    , "\", preparation: \"", preparation, "\"})"
    ]
  where
  names        = v ^.. values . key "name"        . _String
  timings      = v ^.. values . key "timing"      . _String
  preparations = v ^.. values . key "preparation" . _String

ingredients :: Value -> [Text]
ingredients v = v ^.. values . key "name" . _String . to create
  where
  create :: Text -> Text
  create name = mconcat
    [ "CREATE (", T.filter isAsciiAlphaNum name
    , ":Ingredient {name: \"", name, "\"})"
    ]

isAsciiAlphaNum :: Char -> Bool
isAsciiAlphaNum c = isAscii c && isAlphaNum c

generateContent :: IO ()
generateContent = do
  queriesFile <- getDataFileName "data/queries.yaml"
  decodeFileEither queriesFile >>= \case
    Left  err  -> error (show err)
    Right yaml -> process yaml

makeMenu :: Value -> Template
makeMenu
  = either (error . show) id
  . compileMustacheText "menu"
  . go
  where
  go _ =
    "<a href=index.html>all</a> | <a href=ingredient-Gin.html>gin</a> | <a href=ingredient-Whiskey.html>whiskey</a> | <a href=ingredient-Rum.html>rum</a> | <a href=ingredient-Champagne.html>champagne</a><br />"

process :: Value -> IO ()
process v = do
  let menuTemplate = makeMenu v

  forM_ (zip4 names queries paramss templates) $ \(name, query, params, template) -> do
    templateDir <- getDataFileName "data/templates"
    template' <- (<> menuTemplate) <$> compileMustacheDir (PName template) templateDir

    if V.null params
    then do
      r <- post url (object [ "query" .= query ])
      let json = r ^?! responseBody . key "data" . values . values
          bs   = Json.encode json
          fp   = distDir </> T.unpack name
      -- BS.putStrLn bs
      BS.writeFile (fp <.> ".json") bs
      LT.writeFile (fp <.> "html") (renderMustache template' json)
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
        LT.writeFile (fp <.> "html") (renderMustache template' json)

  where
  names     = v ^.. values . key "name"     . _String
  queries   = v ^.. values . key "query"    . _String
  paramss   = v ^.. values . key "params"   . _Array
  templates = v ^.. values . key "template" . _String
