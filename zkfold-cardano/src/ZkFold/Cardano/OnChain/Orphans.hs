{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OnChain.Orphans where

import           Control.Arrow          ((>>>))
import           Data.Aeson             (FromJSON, ToJSON, Value (..), withText)
import qualified Data.Aeson             as Aeson
import qualified Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding     as TE
import           PlutusTx.Builtins      (BuiltinByteString)
import qualified PlutusTx.Builtins      as PlutusTx
import           Prelude                (Either (..), MonadFail (fail), pure, ($), (++))

instance ToJSON BuiltinByteString where
    toJSON = PlutusTx.fromBuiltin >>> B64.encode >>> TE.decodeUtf8 >>> String

instance FromJSON BuiltinByteString where
    parseJSON = withText "BuiltinByteString" $ \t ->
        case B64.decode (TE.encodeUtf8 t) of
            Left err -> fail $ "Invalid base64: " ++ err
            Right bs -> pure $ PlutusTx.toBuiltin bs
