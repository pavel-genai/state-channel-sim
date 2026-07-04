{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Channel.Types
  ( -- * Channel states
    ChannelStatus(..)
    -- * Parties
  , Party(..)
  , PartyKeys(..)
    -- * Balances and state
  , Balance(..)
  , ChannelState(..)
  , SignedState(..)
    -- * Channel
  , Channel(..)
    -- * Errors
  , ChannelError(..)
    -- * Configuration
  , ChannelConfig(..)
  , defaultConfig
  ) where

import           Crypto.PubKey.Ed25519 (PublicKey, SecretKey, Signature)
import           Data.ByteString       (ByteString)
import           Data.Time.Clock       (NominalDiffTime, UTCTime)

-- | The on-chain status of a payment channel.
data ChannelStatus
  = Open       -- ^ Channel created, awaiting second deposit
  | Active     -- ^ Both parties deposited, channel is operational
  | Disputed   -- ^ A dispute has been raised; challenge period active
  | Closed     -- ^ Channel is settled and closed
  deriving (Show, Eq, Ord)

-- | Identifies which party in the channel.
data Party = PartyA | PartyB
  deriving (Show, Eq, Ord)

-- | A party's cryptographic key pair.
data PartyKeys = PartyKeys
  { pkSecret :: !SecretKey
  , pkPublic :: !PublicKey
  } deriving (Show)

-- | Balance allocation between the two parties.
data Balance = Balance
  { balanceA :: !Integer  -- ^ Amount allocated to Party A
  , balanceB :: !Integer  -- ^ Amount allocated to Party B
  } deriving (Show, Eq)

-- | Off-chain channel state with a monotonically increasing nonce.
data ChannelState = ChannelState
  { csNonce   :: !Word     -- ^ Sequence number; higher = more recent
  , csBalance :: !Balance  -- ^ Current balance allocation
  } deriving (Show, Eq)

-- | A channel state signed by both parties.
data SignedState = SignedState
  { ssState :: !ChannelState
  , ssSigA  :: !Signature    -- ^ Party A's signature
  , ssSigB  :: !Signature    -- ^ Party B's signature
  } deriving (Show, Eq)

-- | The full payment channel.
data Channel = Channel
  { chId            :: !ByteString     -- ^ Unique channel identifier
  , chStatus        :: !ChannelStatus  -- ^ Current on-chain status
  , chPartyA        :: !PublicKey       -- ^ Party A's public key
  , chPartyB        :: !PublicKey       -- ^ Party B's public key
  , chDeposits      :: !Balance        -- ^ Initial deposits
  , chLatestState   :: !(Maybe SignedState)  -- ^ Latest agreed state
  , chDisputeState  :: !(Maybe SignedState)  -- ^ State submitted in dispute
  , chDisputeDeadline :: !(Maybe UTCTime)    -- ^ Challenge period end
  , chConfig        :: !ChannelConfig  -- ^ Channel configuration
  } deriving (Show, Eq)

-- | Errors that can occur during channel operations.
data ChannelError
  = InvalidSignature Party
  | InsufficientBalance Party Integer
  | InvalidNonce Word Word        -- ^ (expected, got)
  | InvalidChannelStatus ChannelStatus ChannelStatus  -- ^ (expected, got)
  | NegativeTransfer
  | ChannelNotFound
  | DisputePeriodActive
  | DisputePeriodExpired
  | OutdatedState Word Word       -- ^ (current nonce, submitted nonce)
  | SamePartySigning
  deriving (Show, Eq)

-- | Configuration for a channel.
data ChannelConfig = ChannelConfig
  { ccChallengePeriod :: !NominalDiffTime  -- ^ Duration of challenge window
  } deriving (Show, Eq)

-- | Default configuration with a 24-hour challenge period.
defaultConfig :: ChannelConfig
defaultConfig = ChannelConfig
  { ccChallengePeriod = 86400  -- 24 hours in seconds
  }
