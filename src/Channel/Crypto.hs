{-# LANGUAGE OverloadedStrings #-}

module Channel.Crypto
  ( -- * Key generation
    generateKeyPair
  , generateKeyPairFromSeed
    -- * Signing
  , signState
  , signStateBytes
    -- * Verification
  , verifySignature
  , verifySignedState
    -- * Serialization
  , encodeState
  ) where

import           Channel.Types

import           Crypto.Error          (CryptoFailable(..))
import           Crypto.PubKey.Ed25519 (PublicKey, SecretKey, Signature)
import qualified Crypto.PubKey.Ed25519 as Ed25519
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC

-- | Generate a fresh Ed25519 key pair from OS entropy.
generateKeyPair :: IO PartyKeys
generateKeyPair = do
  secret <- Ed25519.generateSecretKey
  let public = Ed25519.toPublic secret
  pure PartyKeys { pkSecret = secret, pkPublic = public }

-- | Generate a deterministic key pair from a 32-byte seed.
-- Useful for testing with reproducible keys.
generateKeyPairFromSeed :: ByteString -> Either String PartyKeys
generateKeyPairFromSeed seed
  | BS.length seed /= 32 = Left "Seed must be exactly 32 bytes"
  | otherwise =
      case Ed25519.secretKey seed of
        CryptoPassed secret ->
          let public = Ed25519.toPublic secret
          in  Right PartyKeys { pkSecret = secret, pkPublic = public }
        CryptoFailed err ->
          Left $ "Failed to create secret key: " ++ show err

-- | Encode a channel state to bytes for signing.
-- Format: "nonce:<n>|balA:<a>|balB:<b>"
encodeState :: ChannelState -> ByteString
encodeState cs = BS.concat
  [ "nonce:", BC.pack (show (csNonce cs))
  , "|balA:", BC.pack (show (balanceA (csBalance cs)))
  , "|balB:", BC.pack (show (balanceB (csBalance cs)))
  ]

-- | Sign the raw bytes with a secret key.
signStateBytes :: SecretKey -> PublicKey -> ByteString -> Signature
signStateBytes = Ed25519.sign

-- | Sign a channel state with a party's keys.
signState :: PartyKeys -> ChannelState -> Signature
signState keys cs =
  let msg = encodeState cs
  in  Ed25519.sign (pkSecret keys) (pkPublic keys) msg

-- | Verify a signature against a public key and channel state.
verifySignature :: PublicKey -> ChannelState -> Signature -> Bool
verifySignature pubKey cs sig =
  let msg = encodeState cs
  in  Ed25519.verify pubKey msg sig

-- | Verify that a SignedState has valid signatures from both parties.
verifySignedState :: PublicKey -> PublicKey -> SignedState -> Bool
verifySignedState pubA pubB ss =
  let cs = ssState ss
  in  verifySignature pubA cs (ssSigA ss)
      && verifySignature pubB cs (ssSigB ss)
