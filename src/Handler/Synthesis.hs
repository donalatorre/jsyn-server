{-# LANGUAGE OverloadedStrings #-}
module Handler.Synthesis where

import Import
import qualified Data.Aeson as Aeson
import qualified Jsyn

-- instance ToJSON Comment where
--     toJSON (Comment t) = object ["message" .= t]
-- instance FromJSON Comment where
--     parseJSON = withObject "Comment" $ \o -> Comment <$> o .: "message"

newtype SynthResult = SynthResult { unSynthRes :: Jsyn.SynthRes }
  deriving (Show)

instance Aeson.ToJSON SynthResult where
  toJSON synthResult =
    let sr = unSynthRes synthResult
    in
      case sr of
        (Jsyn.SynthRes p) -> object ["program" .= (Jsyn.toJS p)]
        Jsyn.ProgramNotFound -> object ["error" .= ("program not found" :: Text)]
        Jsyn.SynthTimeout -> object ["error" .= ("synthesis ran out of time" :: Text)]
  

postSynthesisR :: Handler Aeson.Value
postSynthesisR = do
    -- requireCheckJsonBody will parse the request body into the appropriate type, or return a 400 status code if the request JSON is invalid.
    examples <- requireCheckJsonBody :: Handler [Jsyn.JsonExample]

    res <- liftIO $ Jsyn.runSynth (2 * (10^6 :: Int)) examples

    return $ toJSON $ SynthResult res
