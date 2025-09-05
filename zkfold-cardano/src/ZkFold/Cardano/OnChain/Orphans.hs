{-# OPTIONS_GHC -Wno-orphans #-}

module ZkFold.Cardano.OnChain.Orphans where

import Control.Arrow ((>>>))
import Data.Aeson (FromJSON, ToJSON, Value (..), withText)
import Data.Aeson qualified as Aeson
import Data.ByteString.Base64 qualified as B64
import Data.Text.Encoding qualified as TE
import PlutusTx.Builtins (BuiltinByteString)
import PlutusTx.Builtins qualified as PlutusTx
import Prelude (Either (..), MonadFail (fail), pure, ($), (++))

instance ToJSON BuiltinByteString where
    toJSON = PlutusTx.fromBuiltin >>> B64.encode >>> TE.decodeUtf8 >>> String

instance FromJSON BuiltinByteString where
    parseJSON = withText "BuiltinByteString" $ \t ->
        case B64.decode (TE.encodeUtf8 t) of
            Left err -> fail $ "Invalid base64: " ++ err
            Right bs -> pure $ PlutusTx.toBuiltin bs
