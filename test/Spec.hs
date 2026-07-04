{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Test.Hspec

import           Channel.Crypto
import           Channel.Dispute
import           Channel.State
import           Channel.Types

import qualified Data.ByteString       as BS
import           Data.Time.Calendar    (fromGregorian)
import           Data.Time.Clock       (UTCTime(..), addUTCTime, secondsToDiffTime)

-- | Test configuration with a short (10 second) challenge period.
testConfig :: ChannelConfig
testConfig = ChannelConfig { ccChallengePeriod = 10 }

-- | Base time for tests.
t0 :: UTCTime
t0 = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

-- | Helper to create deterministic key pairs for testing.
makeTestKeys :: BS.ByteString -> PartyKeys
makeTestKeys seed =
  case generateKeyPairFromSeed (BS.take 32 (seed <> BS.replicate 32 0)) of
    Right keys -> keys
    Left err   -> error $ "Failed to create test keys: " ++ err

-- | Set up a full active channel for testing.
setupActiveChannel :: (Channel, PartyKeys, PartyKeys)
setupActiveChannel =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      Right ch1 = openChannel "test-chan" keysA (pkPublic keysB) 1000 testConfig
      Right ch2 = activateChannel ch1 keysA keysB 500
  in  (ch2, keysA, keysB)

main :: IO ()
main = hspec $ do
  describe "Channel.Crypto" $ do
    it "generates deterministic keys from seed" $ do
      let keys1 = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keys2 = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      pkPublic keys1 `shouldBe` pkPublic keys2

    it "signs and verifies channel state" $ do
      let keys = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sig = signState keys st
      verifySignature (pkPublic keys) st sig `shouldBe` True

    it "rejects forged signatures" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigB = signState keysB st
      -- Verify A's key against B's signature should fail
      verifySignature (pkPublic keysA) st sigB `shouldBe` False

  describe "Channel.State - Happy Path" $ do
    it "opens a channel in Open status" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      case openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig of
        Left err -> expectationFailure $ "Expected Right, got: " ++ show err
        Right ch -> do
          chStatus ch `shouldBe` Open
          balanceA (chDeposits ch) `shouldBe` 1000
          balanceB (chDeposits ch) `shouldBe` 0

    it "activates a channel with both deposits" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      chStatus ch `shouldBe` Active
      totalDeposits ch `shouldBe` 1500

    it "processes off-chain payments correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyA 200 of
        Left err -> expectationFailure $ show err
        Right (ch', _) -> do
          channelBalance ch' `shouldBe` Just (Balance 800 700)
          case chLatestState ch' of
            Nothing -> expectationFailure "Expected state"
            Just ss -> csNonce (ssState ss) `shouldBe` 1

    it "processes multiple payments and tracks nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, _) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, _) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, _) = createPayment ch2 keysA keysB PartyA 50
      channelBalance ch3 `shouldBe` Just (Balance 850 650)
      case chLatestState ch3 of
        Nothing -> expectationFailure "Expected state"
        Just ss -> csNonce (ssState ss) `shouldBe` 3

    it "rejects payment exceeding balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyA 1500
        `shouldBe` Left (InsufficientBalance PartyA 1500)

    it "rejects negative/zero payment" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyA 0
        `shouldBe` Left NegativeTransfer

    it "performs cooperative close" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch', _) = createPayment ch keysA keysB PartyA 200
      case cooperativeClose ch' keysA keysB of
        Left err -> expectationFailure $ show err
        Right closed -> do
          chStatus closed `shouldBe` Closed
          channelBalance closed `shouldBe` Just (Balance 800 700)

  describe "Channel.Dispute - Dispute with outdated state" $ do
    it "allows unilateral close and starts challenge period" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
      case unilateralClose ch1 ss1 t0 of
        Left err -> expectationFailure $ show err
        Right disputed -> do
          chStatus disputed `shouldBe` Disputed
          chDisputeDeadline disputed `shouldSatisfy` (/= Nothing)

    it "allows counter-dispute with higher nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          -- Unilateral close with old state (nonce 1)
          Right disputed = unilateralClose ch2 ss1 t0
      case counterDispute disputed ss2 (addUTCTime 1 t0) of
        Left err -> expectationFailure $ show err
        Right countered -> do
          case chDisputeState countered of
            Nothing -> expectationFailure "Expected dispute state"
            Just ss -> csNonce (ssState ss) `shouldBe` 2

    it "rejects counter-dispute with same or lower nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          -- Unilateral close with newer state (nonce 2)
          Right disputed = unilateralClose ch2 ss2 t0
      -- Try to counter with old state (nonce 1)
      counterDispute disputed ss1 (addUTCTime 1 t0)
        `shouldBe` Left (OutdatedState 2 1)

    it "resolves dispute with correct final state after timeout" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, ss3) = createPayment ch2 keysA keysB PartyA 50
          -- B tries with old state
          Right disputed = unilateralClose ch3 ss1 t0
          -- A counters with latest
          Right countered = counterDispute disputed ss3 (addUTCTime 1 t0)
          -- Wait for challenge period to expire
          afterChallenge = addUTCTime 20 t0
      case resolveDispute countered afterChallenge of
        Left err -> expectationFailure $ show err
        Right resolved -> do
          chStatus resolved `shouldBe` Closed
          -- Final state should reflect the counter-dispute state (nonce 3)
          case chLatestState resolved of
            Nothing -> expectationFailure "Expected final state"
            Just ss -> do
              csNonce (ssState ss) `shouldBe` 3
              csBalance (ssState ss) `shouldBe` Balance 850 650

  describe "Channel.Dispute - Timeout scenarios" $ do
    it "rejects dispute resolution before challenge period expires" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
          Right disputed = unilateralClose ch1 ss1 t0
          duringChallenge = addUTCTime 5 t0  -- only 5 seconds, need 10
      resolveDispute disputed duringChallenge
        `shouldBe` Left DisputePeriodActive

    it "allows resolution exactly at deadline" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
          Right disputed = unilateralClose ch1 ss1 t0
          atDeadline = addUTCTime 10 t0  -- exactly at deadline
      case resolveDispute disputed atDeadline of
        Left err -> expectationFailure $ show err
        Right resolved -> chStatus resolved `shouldBe` Closed

    it "detects challenge period expiration correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
          Right disputed = unilateralClose ch1 ss1 t0
      isChallengePeriodExpired disputed (addUTCTime 5 t0) `shouldBe` False
      isChallengePeriodExpired disputed (addUTCTime 10 t0) `shouldBe` True
      isChallengePeriodExpired disputed (addUTCTime 15 t0) `shouldBe` True

    it "counter-dispute resets challenge period" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          Right disputed = unilateralClose ch2 ss1 t0
          -- Counter at t+5
          t5 = addUTCTime 5 t0
          Right countered = counterDispute disputed ss2 t5
      -- Old deadline (t+10) should not be enough for new challenge
      isChallengePeriodExpired countered (addUTCTime 10 t0) `shouldBe` False
      -- New deadline is t5 + 10 = t+15
      isChallengePeriodExpired countered (addUTCTime 15 t0) `shouldBe` True

  describe "Channel.State - Edge cases" $ do
    it "rejects opening channel with zero deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      openChannel "chan-bad" keysA (pkPublic keysB) 0 testConfig
        `shouldBe` Left (InsufficientBalance PartyA 0)

    it "rejects activating already active channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      activateChannel ch keysA keysB 500
        `shouldBe` Left (InvalidChannelStatus Open Active)

    it "rejects payment on non-active channel" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-x" keysA (pkPublic keysB) 1000 testConfig
      createPayment ch keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Open)
