module Pursuit where

import Prelude

import Affjax (Error)
import Affjax.Node as Affjax
import Affjax.RequestHeader (RequestHeader(..))
import Affjax.ResponseFormat (json)
import Data.Argonaut (class DecodeJson, Json, decodeJson, (.:), (.:?))
import Data.Array (filter)
import Data.Either (Either(..), either)
import Data.Maybe (Maybe)
import Data.MediaType.Common (applicationJSON)
import Effect.Aff (Aff)

newtype PursuitSearchInfo = PursuitSearchInfo
  { typeOrValue :: Maybe String
  , mod :: Maybe String
  , typeText :: Maybe String
  , title :: Maybe String
  , typ :: String
  }

instance decodeJsonPursuitSearchInfo :: DecodeJson PursuitSearchInfo where
  decodeJson json =
    do
      obj <- decodeJson json
      typeOrValue <- obj .:? "typeOrValue"
      mod <- obj .:? "module"
      typeText <- obj .:? "typeText"
      title <- obj .:? "title"
      typ <- obj .: "type"
      pure $ PursuitSearchInfo { typeOrValue, mod, typeText, title, typ }

newtype PursuitSearchResult = PursuitSearchResult
  { text :: String
  , markup :: String
  , url :: String
  , version :: String
  , package :: String
  , info :: PursuitSearchInfo
  }

instance decodeJsonPursuitSearchResult :: DecodeJson PursuitSearchResult where
  decodeJson json =
    do
      obj <- decodeJson json
      text <- obj .: "text"
      markup <- obj .: "markup"
      url <- obj .: "url"
      version <- obj .: "version"
      package <- obj .: "package"
      info <- obj .: "info"
      pure $ PursuitSearchResult { text, markup, url, version, package, info }

pursuitRequest :: String -> Aff (Either Error (Affjax.Response Json))
pursuitRequest text = Affjax.request $ Affjax.defaultRequest
  { url = "https://pursuit.purescript.org/search?q=" <> text
  , headers = [ Accept applicationJSON ]
  , responseFormat = json
  }

pursuitSearchRequest :: String -> Aff (Array PursuitSearchResult)
pursuitSearchRequest text = do
  res <- pursuitRequest text
  case res of
    Left _ -> pure []
    Right { body } -> pure $ either (pure []) identity $ decodeJson body

pursuitModuleSearchRequest :: String -> Aff (Array PursuitSearchResult)
pursuitModuleSearchRequest text = do
  res <- pursuitRequest text
  case res of
    Left _ -> pure []
    Right { body } -> do
      let decoded = decodeJson body
          results = either (pure []) identity $ decoded
      pure $ filter isModule results

  where
    isModule (PursuitSearchResult { info: PursuitSearchInfo { typ: "module" } }) = true
    isModule _ = false
