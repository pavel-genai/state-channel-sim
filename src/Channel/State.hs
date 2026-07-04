{-# LANGUAGE OverloadedStrings #-}

module Channel.State
  ( -- * Channel lifecycle
    openChannel
  , activateChannel
    -- * Off-chain operations
  , createPayment
    -- * Closing
  , cooperativeClose
  , unilateralClose
    -- * Queries
  , channelBalance
  , totalDeposits
  ) where

import           Channel.Crypto
import           Channel.Types

import           Crypto.PubKey.Ed25519 (PublicKey)
import           Data.ByteString       (ByteString)
import           Data.Time.Clock       (UTCTime, addUTCTime)

-- | Open a new channel with Party A's deposit. Channel starts in Open status.
openChannel
  :: ByteString    -- ^ Channel ID
  -> PartyKeys     -- ^ Party A's keys
  -> PublicKey     -- ^ Party B's public key
  -> Integer       -- ^ Party A's deposit
  -> ChannelConfig -- ^ Channel config
  -> Either ChannelError Channel
openChannel cid keysA pubB depositA config
  | depositA <= 0 = Left (InsufficientBalance PartyA depositA)
  | otherwise = Right Channel
      { chId            = cid
      , chStatus        = Open
      , chPartyA        = pkPublic keysA
      , chPartyB        = pubB
      , chDeposits      = Balance depositA 0
      , chLatestState   = Nothing
      , chDisputeState  = Nothing
      , chDisputeDeadline = Nothing
      , chConfig        = config
      }

-- | Activate a channel by adding Party B's deposit. Creates the initial
-- signed state (nonce 0) with both deposits.
activateChannel
  :: Channel
  -> PartyKeys  -- ^ Party A's keys (for co-signing initial state)
  -> PartyKeys  -- ^ Party B's keys
  -> Integer    -- ^ Party B's deposit
  -> Either ChannelError Channel
activateChannel ch keysA keysB depositB
  | chStatus ch /= Open = Left (InvalidChannelStatus Open (chStatus ch))
  | depositB <= 0       = Left (InsufficientBalance PartyB depositB)
  | otherwise =
      let deposits = Balance (balanceA (chDeposits ch)) depositB
          initialState = ChannelState
            { csNonce   = 0
            , csBalance = deposits
            }
          sigA = signState keysA initialState
          sigB = signState keysB initialState
          signedSt = SignedState
            { ssState = initialState
            , ssSigA  = sigA
            , ssSigB  = sigB
            }
      in  Right ch
            { chStatus      = Active
            , chDeposits    = deposits
            , chLatestState = Just signedSt
            }

-- | Create an off-chain payment, producing a new signed state.
-- The sender's balance decreases and the receiver's increases.
createPayment
  :: Channel
  -> PartyKeys  -- ^ Party A's keys
  -> PartyKeys  -- ^ Party B's keys
  -> Party      -- ^ Sender
  -> Integer    -- ^ Amount to transfer
  -> Either ChannelError (Channel, SignedState)
createPayment ch keysA keysB sender amount
  | chStatus ch /= Active = Left (InvalidChannelStatus Active (chStatus ch))
  | amount <= 0           = Left NegativeTransfer
  | otherwise = do
      let mLatest = chLatestState ch
      case mLatest of
        Nothing -> Left ChannelNotFound
        Just latest ->
          let curBal  = csBalance (ssState latest)
              curNonce = csNonce (ssState latest)
              newNonce = curNonce + 1
          in case sender of
            PartyA
              | balanceA curBal < amount ->
                  Left (InsufficientBalance PartyA amount)
              | otherwise ->
                  let newBal = Balance
                        (balanceA curBal - amount)
                        (balanceB curBal + amount)
                  in  buildSignedState ch keysA keysB newNonce newBal
            PartyB
              | balanceB curBal < amount ->
                  Left (InsufficientBalance PartyB amount)
              | otherwise ->
                  let newBal = Balance
                        (balanceA curBal + amount)
                        (balanceB curBal - amount)
                  in  buildSignedState ch keysA keysB newNonce newBal

-- | Internal helper to build a new signed state and update the channel.
buildSignedState
  :: Channel
  -> PartyKeys
  -> PartyKeys
  -> Word
  -> Balance
  -> Either ChannelError (Channel, SignedState)
buildSignedState ch keysA keysB nonce bal =
  let newState = ChannelState { csNonce = nonce, csBalance = bal }
      sigA = signState keysA newState
      sigB = signState keysB newState
      signed = SignedState
        { ssState = newState
        , ssSigA  = sigA
        , ssSigB  = sigB
        }
      ch' = ch { chLatestState = Just signed }
  in  Right (ch', signed)

-- | Cooperative close: both parties agree to close the channel using the
-- latest signed state.
cooperativeClose
  :: Channel
  -> PartyKeys  -- ^ Party A's keys
  -> PartyKeys  -- ^ Party B's keys
  -> Either ChannelError Channel
cooperativeClose ch keysA keysB
  | chStatus ch /= Active = Left (InvalidChannelStatus Active (chStatus ch))
  | otherwise =
      case chLatestState ch of
        Nothing -> Left ChannelNotFound
        Just latest ->
          if verifySignedState (pkPublic keysA) (pkPublic keysB) latest
            then Right ch { chStatus = Closed }
            else Left (InvalidSignature PartyA)

-- | Unilateral close: one party submits their latest signed state and
-- starts the challenge period.
unilateralClose
  :: Channel
  -> SignedState  -- ^ The state being submitted
  -> UTCTime      -- ^ Current time
  -> Either ChannelError Channel
unilateralClose ch signedSt now
  | chStatus ch /= Active = Left (InvalidChannelStatus Active (chStatus ch))
  | not (verifySignedState (chPartyA ch) (chPartyB ch) signedSt) =
      Left (InvalidSignature PartyA)
  | otherwise =
      let deadline = addUTCTime (ccChallengePeriod (chConfig ch)) now
      in  Right ch
            { chStatus          = Disputed
            , chDisputeState    = Just signedSt
            , chDisputeDeadline = Just deadline
            }

-- | Get the current balance from the latest signed state.
channelBalance :: Channel -> Maybe Balance
channelBalance ch = csBalance . ssState <$> chLatestState ch

-- | Get total deposits in the channel.
totalDeposits :: Channel -> Integer
totalDeposits ch =
  let d = chDeposits ch
  in  balanceA d + balanceB d
