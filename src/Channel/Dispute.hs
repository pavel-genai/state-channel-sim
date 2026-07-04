{-# LANGUAGE OverloadedStrings #-}

module Channel.Dispute
  ( -- * Dispute operations
    raiseDispute
  , counterDispute
  , resolveDispute
  , isChallengePeriodExpired
  ) where

import           Channel.Crypto
import           Channel.Types

import           Data.Time.Clock (UTCTime, addUTCTime)

-- | Raise a dispute by submitting a signed state. Starts the challenge period.
-- Can only be called on an Active channel.
raiseDispute
  :: Channel
  -> SignedState  -- ^ State to submit as dispute evidence
  -> UTCTime      -- ^ Current time
  -> Either ChannelError Channel
raiseDispute ch signedSt now
  | chStatus ch /= Active =
      Left (InvalidChannelStatus Active (chStatus ch))
  | not (verifySignedState (chPartyA ch) (chPartyB ch) signedSt) =
      Left (InvalidSignature PartyA)
  | otherwise =
      let deadline = addUTCTime (ccChallengePeriod (chConfig ch)) now
      in  Right ch
            { chStatus          = Disputed
            , chDisputeState    = Just signedSt
            , chDisputeDeadline = Just deadline
            }

-- | Counter a dispute by submitting a state with a higher nonce.
-- This replaces the dispute state and resets the challenge period.
counterDispute
  :: Channel
  -> SignedState  -- ^ Counter-evidence with higher nonce
  -> UTCTime      -- ^ Current time
  -> Either ChannelError Channel
counterDispute ch signedSt now
  | chStatus ch /= Disputed =
      Left (InvalidChannelStatus Disputed (chStatus ch))
  | not (verifySignedState (chPartyA ch) (chPartyB ch) signedSt) =
      Left (InvalidSignature PartyA)
  | otherwise =
      case chDisputeState ch of
        Nothing -> Left ChannelNotFound
        Just currentDispute ->
          let currentNonce = csNonce (ssState currentDispute)
              newNonce     = csNonce (ssState signedSt)
          in  if newNonce <= currentNonce
                then Left (OutdatedState currentNonce newNonce)
                else
                  let deadline = addUTCTime (ccChallengePeriod (chConfig ch)) now
                  in  Right ch
                        { chDisputeState    = Just signedSt
                        , chDisputeDeadline = Just deadline
                        , chLatestState     = Just signedSt
                        }

-- | Resolve a dispute after the challenge period has expired.
-- Closes the channel with the final dispute state.
resolveDispute
  :: Channel
  -> UTCTime   -- ^ Current time
  -> Either ChannelError Channel
resolveDispute ch now
  | chStatus ch /= Disputed =
      Left (InvalidChannelStatus Disputed (chStatus ch))
  | otherwise =
      case chDisputeDeadline ch of
        Nothing -> Left ChannelNotFound
        Just deadline
          | now < deadline -> Left DisputePeriodActive
          | otherwise ->
              Right ch
                { chStatus      = Closed
                , chLatestState = chDisputeState ch
                }

-- | Check if the challenge period has expired.
isChallengePeriodExpired :: Channel -> UTCTime -> Bool
isChallengePeriodExpired ch now =
  case chDisputeDeadline ch of
    Nothing       -> False
    Just deadline -> now >= deadline
