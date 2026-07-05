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

-- | Set up an active channel and make a payment, returning the channel,
-- keys, and the signed state produced by the payment.
setupWithPayment :: (Channel, PartyKeys, PartyKeys, SignedState)
setupWithPayment =
  let (ch, keysA, keysB) = setupActiveChannel
      Right (ch', ss) = createPayment ch keysA keysB PartyA 200
  in  (ch', keysA, keysB, ss)

-- | Set up a disputed channel for testing dispute-related functions.
setupDisputedChannel :: (Channel, PartyKeys, PartyKeys, SignedState)
setupDisputedChannel =
  let (ch', keysA, keysB, ss) = setupWithPayment
      Right disputed = unilateralClose ch' ss t0
  in  (disputed, keysA, keysB, ss)

main :: IO ()
main = hspec $ do

  ---------------------------------------------------------------------------
  -- Channel.Types
  ---------------------------------------------------------------------------
  describe "Channel.Types" $ do
    it "defaultConfig has 24-hour challenge period" $ do
      ccChallengePeriod defaultConfig `shouldBe` 86400

    it "ChannelStatus has correct Eq instances" $ do
      Open `shouldBe` Open
      Active `shouldBe` Active
      Disputed `shouldBe` Disputed
      Closed `shouldBe` Closed
      Open `shouldNotBe` Active
      Active `shouldNotBe` Disputed
      Disputed `shouldNotBe` Closed

    it "ChannelStatus has correct Ord ordering" $ do
      compare Open Active `shouldBe` LT
      compare Active Disputed `shouldBe` LT
      compare Disputed Closed `shouldBe` LT
      compare Closed Open `shouldBe` GT
      compare Open Open `shouldBe` EQ

    it "ChannelStatus Show instances are correct" $ do
      show Open `shouldBe` "Open"
      show Active `shouldBe` "Active"
      show Disputed `shouldBe` "Disputed"
      show Closed `shouldBe` "Closed"

    it "Party has correct Show/Eq/Ord instances" $ do
      show PartyA `shouldBe` "PartyA"
      show PartyB `shouldBe` "PartyB"
      PartyA `shouldNotBe` PartyB
      compare PartyA PartyB `shouldBe` LT

    it "Balance has correct Eq instance" $ do
      Balance 100 200 `shouldBe` Balance 100 200
      Balance 100 200 `shouldNotBe` Balance 200 100

    it "Balance Show instance works" $ do
      show (Balance 100 200) `shouldSatisfy` (not . null)

    it "ChannelState has correct Eq instance" $ do
      let st1 = ChannelState 0 (Balance 100 200)
          st2 = ChannelState 0 (Balance 100 200)
          st3 = ChannelState 1 (Balance 100 200)
          st4 = ChannelState 0 (Balance 200 100)
      st1 `shouldBe` st2
      st1 `shouldNotBe` st3
      st1 `shouldNotBe` st4

    it "ChannelError constructors are distinguishable" $ do
      InvalidSignature PartyA `shouldNotBe` InvalidSignature PartyB
      InsufficientBalance PartyA 10 `shouldNotBe` InsufficientBalance PartyB 10
      InsufficientBalance PartyA 10 `shouldNotBe` InsufficientBalance PartyA 20
      InvalidNonce 1 2 `shouldNotBe` InvalidNonce 2 1
      InvalidChannelStatus Open Active `shouldNotBe` InvalidChannelStatus Active Open
      NegativeTransfer `shouldNotBe` ChannelNotFound
      DisputePeriodActive `shouldNotBe` DisputePeriodExpired
      OutdatedState 1 2 `shouldNotBe` OutdatedState 2 1
      SamePartySigning `shouldNotBe` NegativeTransfer

    it "ChannelError Show instances work for all constructors" $ do
      show (InvalidSignature PartyA) `shouldSatisfy` (not . null)
      show (InvalidSignature PartyB) `shouldSatisfy` (not . null)
      show (InsufficientBalance PartyA 10) `shouldSatisfy` (not . null)
      show (InsufficientBalance PartyB 20) `shouldSatisfy` (not . null)
      show (InvalidNonce 1 2) `shouldSatisfy` (not . null)
      show (InvalidChannelStatus Open Active) `shouldSatisfy` (not . null)
      show NegativeTransfer `shouldSatisfy` (not . null)
      show ChannelNotFound `shouldSatisfy` (not . null)
      show DisputePeriodActive `shouldSatisfy` (not . null)
      show DisputePeriodExpired `shouldSatisfy` (not . null)
      show (OutdatedState 1 2) `shouldSatisfy` (not . null)
      show SamePartySigning `shouldSatisfy` (not . null)

    it "ChannelConfig Eq instance works" $ do
      testConfig `shouldBe` testConfig
      testConfig `shouldNotBe` defaultConfig

  ---------------------------------------------------------------------------
  -- Channel.Crypto
  ---------------------------------------------------------------------------
  describe "Channel.Crypto" $ do
    it "generates deterministic keys from seed" $ do
      let keys1 = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keys2 = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      pkPublic keys1 `shouldBe` pkPublic keys2

    it "different seeds produce different keys" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      pkPublic keysA `shouldNotBe` pkPublic keysB

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

    it "rejects signature for different state" $ do
      let keys = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          st1 = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          st2 = ChannelState { csNonce = 2, csBalance = Balance 500 500 }
          sig1 = signState keys st1
      -- Signature for st1 should not verify against st2
      verifySignature (pkPublic keys) st2 sig1 `shouldBe` False

    it "rejects signature for state with different balances" $ do
      let keys = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          st1 = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          st2 = ChannelState { csNonce = 1, csBalance = Balance 600 400 }
          sig1 = signState keys st1
      verifySignature (pkPublic keys) st2 sig1 `shouldBe` False

    it "generateKeyPairFromSeed rejects short seed" $ do
      case generateKeyPairFromSeed "tooshort" of
        Left msg -> msg `shouldBe` "Seed must be exactly 32 bytes"
        Right _  -> expectationFailure "Expected Left for short seed"

    it "generateKeyPairFromSeed rejects empty seed" $ do
      case generateKeyPairFromSeed "" of
        Left msg -> msg `shouldBe` "Seed must be exactly 32 bytes"
        Right _  -> expectationFailure "Expected Left for empty seed"

    it "generateKeyPairFromSeed rejects long seed" $ do
      case generateKeyPairFromSeed (BS.replicate 33 0) of
        Left msg -> msg `shouldBe` "Seed must be exactly 32 bytes"
        Right _  -> expectationFailure "Expected Left for long seed"

    it "generateKeyPairFromSeed accepts exactly 32-byte seed" $ do
      case generateKeyPairFromSeed (BS.replicate 32 42) of
        Left err -> expectationFailure $ "Expected Right, got Left: " ++ err
        Right keys -> do
          let sig = signState keys (ChannelState 0 (Balance 100 100))
          verifySignature (pkPublic keys) (ChannelState 0 (Balance 100 100)) sig
            `shouldBe` True

    it "generateKeyPair produces valid keys" $ do
      keys <- generateKeyPair
      let st = ChannelState { csNonce = 0, csBalance = Balance 100 200 }
          sig = signState keys st
      verifySignature (pkPublic keys) st sig `shouldBe` True

    it "generateKeyPair produces different keys each time" $ do
      keys1 <- generateKeyPair
      keys2 <- generateKeyPair
      pkPublic keys1 `shouldNotBe` pkPublic keys2

    it "encodeState produces consistent encoding" $ do
      let st = ChannelState { csNonce = 5, csBalance = Balance 300 700 }
      encodeState st `shouldBe` "nonce:5|balA:300|balB:700"

    it "encodeState handles zero balances" $ do
      let st = ChannelState { csNonce = 0, csBalance = Balance 0 0 }
      encodeState st `shouldBe` "nonce:0|balA:0|balB:0"

    it "encodeState handles large values" $ do
      let st = ChannelState { csNonce = 999999, csBalance = Balance 1000000 2000000 }
      encodeState st `shouldBe` "nonce:999999|balA:1000000|balB:2000000"

    it "signStateBytes signs raw bytes correctly" $ do
      let keys = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          msg = "test message"
          sig = signStateBytes (pkSecret keys) (pkPublic keys) msg
      -- Verify using Ed25519 directly
      let st = ChannelState { csNonce = 0, csBalance = Balance 0 0 }
          encoded = encodeState st
          sigFromState = signStateBytes (pkSecret keys) (pkPublic keys) encoded
          sigFromSign = signState keys st
      -- Both methods should produce the same signature for the same data
      sigFromState `shouldBe` sigFromSign

    it "verifySignedState accepts correctly double-signed state" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigA = signState keysA st
          sigB = signState keysB st
          signed = SignedState st sigA sigB
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` True

    it "verifySignedState rejects when Party A signature is invalid" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigC = signState keysC st  -- wrong signer
          sigB = signState keysB st
          signed = SignedState st sigC sigB
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

    it "verifySignedState rejects when Party B signature is invalid" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigA = signState keysA st
          sigC = signState keysC st  -- wrong signer
          signed = SignedState st sigA sigC
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

    it "verifySignedState rejects when both signatures are invalid" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          keysD = makeTestKeys "DDDDDDDDDDDDDDDDDDDDDDDDDDDDDD"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigC = signState keysC st
          sigD = signState keysD st
          signed = SignedState st sigC sigD
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

    it "verifySignedState rejects when signatures are swapped" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigA = signState keysA st
          sigB = signState keysB st
          -- Swap: B's sig in A's slot, A's sig in B's slot
          signed = SignedState st sigB sigA
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

  ---------------------------------------------------------------------------
  -- Channel.State - Happy Path
  ---------------------------------------------------------------------------
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

    it "opens a channel with correct ID and keys" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      case openChannel "my-chan" keysA (pkPublic keysB) 500 testConfig of
        Left err -> expectationFailure $ show err
        Right ch -> do
          chId ch `shouldBe` "my-chan"
          chPartyA ch `shouldBe` pkPublic keysA
          chPartyB ch `shouldBe` pkPublic keysB
          chLatestState ch `shouldBe` Nothing
          chDisputeState ch `shouldBe` Nothing
          chDisputeDeadline ch `shouldBe` Nothing
          chConfig ch `shouldBe` testConfig

    it "activates a channel with both deposits" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      chStatus ch `shouldBe` Active
      totalDeposits ch `shouldBe` 1500

    it "activates channel with correct initial state" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      case chLatestState ch of
        Nothing -> expectationFailure "Expected initial signed state"
        Just ss -> do
          csNonce (ssState ss) `shouldBe` 0
          csBalance (ssState ss) `shouldBe` Balance 1000 500

    it "processes off-chain payments correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyA 200 of
        Left err -> expectationFailure $ show err
        Right (ch', _) -> do
          channelBalance ch' `shouldBe` Just (Balance 800 700)
          case chLatestState ch' of
            Nothing -> expectationFailure "Expected state"
            Just ss -> csNonce (ssState ss) `shouldBe` 1

    it "processes PartyB payment correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyB 100 of
        Left err -> expectationFailure $ show err
        Right (ch', _) -> do
          channelBalance ch' `shouldBe` Just (Balance 1100 400)
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

    it "creates payment that returns valid signed state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyA 200 of
        Left err -> expectationFailure $ show err
        Right (_, ss) -> do
          csNonce (ssState ss) `shouldBe` 1
          csBalance (ssState ss) `shouldBe` Balance 800 700
          -- The signed state should be verifiable
          verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "performs cooperative close" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch', _) = createPayment ch keysA keysB PartyA 200
      case cooperativeClose ch' keysA keysB of
        Left err -> expectationFailure $ show err
        Right closed -> do
          chStatus closed `shouldBe` Closed
          channelBalance closed `shouldBe` Just (Balance 800 700)

    it "cooperative close preserves latest state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch', ss) = createPayment ch keysA keysB PartyA 300
      case cooperativeClose ch' keysA keysB of
        Left err -> expectationFailure $ show err
        Right closed -> do
          chLatestState closed `shouldBe` Just ss

  ---------------------------------------------------------------------------
  -- Channel.State - Error cases
  ---------------------------------------------------------------------------
  describe "Channel.State - Error cases" $ do
    it "rejects opening channel with zero deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      openChannel "chan-bad" keysA (pkPublic keysB) 0 testConfig
        `shouldBe` Left (InsufficientBalance PartyA 0)

    it "rejects opening channel with negative deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      openChannel "chan-bad" keysA (pkPublic keysB) (-100) testConfig
        `shouldBe` Left (InsufficientBalance PartyA (-100))

    it "rejects activating already active channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      activateChannel ch keysA keysB 500
        `shouldBe` Left (InvalidChannelStatus Open Active)

    it "rejects activating a closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right closed = cooperativeClose ch keysA keysB
      activateChannel closed keysA keysB 500
        `shouldBe` Left (InvalidChannelStatus Open Closed)

    it "rejects activating with zero deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      activateChannel ch keysA keysB 0
        `shouldBe` Left (InsufficientBalance PartyB 0)

    it "rejects activating with negative deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      activateChannel ch keysA keysB (-50)
        `shouldBe` Left (InsufficientBalance PartyB (-50))

    it "rejects payment on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-x" keysA (pkPublic keysB) 1000 testConfig
      createPayment ch keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects payment on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right closed = cooperativeClose ch keysA keysB
      createPayment closed keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Closed)

    it "rejects payment on disputed channel" $ do
      let (ch', keysA, keysB, ss) = setupWithPayment
          Right disputed = unilateralClose ch' ss t0
      createPayment disputed keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Disputed)

    it "rejects payment exceeding PartyA balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyA 1500
        `shouldBe` Left (InsufficientBalance PartyA 1500)

    it "rejects payment exceeding PartyB balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyB 600
        `shouldBe` Left (InsufficientBalance PartyB 600)

    it "rejects payment exactly at PartyB balance boundary" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      -- PartyB has 500, paying 501 should fail
      createPayment ch keysA keysB PartyB 501
        `shouldBe` Left (InsufficientBalance PartyB 501)

    it "allows payment exactly equal to PartyA balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyA 1000 of
        Left err -> expectationFailure $ show err
        Right (ch', _) ->
          channelBalance ch' `shouldBe` Just (Balance 0 1500)

    it "allows payment exactly equal to PartyB balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyB 500 of
        Left err -> expectationFailure $ show err
        Right (ch', _) ->
          channelBalance ch' `shouldBe` Just (Balance 1500 0)

    it "rejects negative/zero payment (0)" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyA 0
        `shouldBe` Left NegativeTransfer

    it "rejects negative payment amount" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyA (-10)
        `shouldBe` Left NegativeTransfer

    it "rejects zero payment from PartyB" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      createPayment ch keysA keysB PartyB 0
        `shouldBe` Left NegativeTransfer

    it "rejects cooperative close on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      cooperativeClose ch keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects cooperative close on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right closed = cooperativeClose ch keysA keysB
      cooperativeClose closed keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Closed)

    it "rejects cooperative close on disputed channel" $ do
      let (ch', keysA, keysB, ss) = setupWithPayment
          Right disputed = unilateralClose ch' ss t0
      cooperativeClose disputed keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Disputed)

    it "rejects unilateral close on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
          -- Create a fake signed state (won't matter since status check comes first)
          st = ChannelState 0 (Balance 1000 0)
          sig = signState keysA st
          ss = SignedState st sig sig
      unilateralClose ch ss t0
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects unilateral close with invalid signatures" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState 1 (Balance 800 700)
          sigC = signState keysC st
          sigB = signState keysB st
          badSigned = SignedState st sigC sigB
      unilateralClose ch badSigned t0
        `shouldBe` Left (InvalidSignature PartyA)

    it "unilateral close sets correct deadline" $ do
      let (ch', _keysA, _keysB, ss) = setupWithPayment
      case unilateralClose ch' ss t0 of
        Left err -> expectationFailure $ show err
        Right disputed -> do
          chDisputeDeadline disputed `shouldBe` Just (addUTCTime 10 t0)
          chDisputeState disputed `shouldBe` Just ss

  ---------------------------------------------------------------------------
  -- Channel.State - Queries
  ---------------------------------------------------------------------------
  describe "Channel.State - Queries" $ do
    it "channelBalance returns Nothing for channel with no state" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      channelBalance ch `shouldBe` Nothing

    it "channelBalance returns balance from latest state" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      channelBalance ch `shouldBe` Just (Balance 1000 500)

    it "channelBalance updates after payment" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch', _) = createPayment ch keysA keysB PartyA 200
      channelBalance ch' `shouldBe` Just (Balance 800 700)

    it "totalDeposits is correct for open channel" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      totalDeposits ch `shouldBe` 1000

    it "totalDeposits is correct for active channel" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      totalDeposits ch `shouldBe` 1500

    it "totalDeposits does not change after payments" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch', _) = createPayment ch keysA keysB PartyA 200
      totalDeposits ch' `shouldBe` 1500

  ---------------------------------------------------------------------------
  -- Channel.Dispute - raiseDispute
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - raiseDispute" $ do
    it "raises dispute on active channel with valid signed state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
      case raiseDispute ch1 ss1 t0 of
        Left err -> expectationFailure $ show err
        Right disputed -> do
          chStatus disputed `shouldBe` Disputed
          chDisputeState disputed `shouldBe` Just ss1
          chDisputeDeadline disputed `shouldBe` Just (addUTCTime 10 t0)

    it "rejects raiseDispute on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
          st = ChannelState 0 (Balance 1000 0)
          sig = signState keysA st
          ss = SignedState st sig sig
      raiseDispute ch ss t0
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects raiseDispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right closed = cooperativeClose ch keysA keysB
          st = ChannelState 0 (Balance 1000 500)
          sigA = signState keysA st
          sigB = signState keysB st
          ss = SignedState st sigA sigB
      raiseDispute closed ss t0
        `shouldBe` Left (InvalidChannelStatus Active Closed)

    it "rejects raiseDispute on already disputed channel" $ do
      let (disputed, keysA, keysB, _ss) = setupDisputedChannel
          st = ChannelState 1 (Balance 800 700)
          sigA = signState keysA st
          sigB = signState keysB st
          ss2 = SignedState st sigA sigB
      raiseDispute disputed ss2 t0
        `shouldBe` Left (InvalidChannelStatus Active Disputed)

    it "rejects raiseDispute with invalid signatures" $ do
      let (ch, _keysA, keysB) = setupActiveChannel
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState 1 (Balance 800 700)
          sigC = signState keysC st
          sigB = signState keysB st
          badSigned = SignedState st sigC sigB
      raiseDispute ch badSigned t0
        `shouldBe` Left (InvalidSignature PartyA)

  ---------------------------------------------------------------------------
  -- Channel.Dispute - counterDispute
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - counterDispute" $ do
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

    it "rejects counter-dispute with same nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right disputed = unilateralClose ch1 ss1 t0
      -- Try to counter with same state (nonce 1)
      counterDispute disputed ss1 (addUTCTime 1 t0)
        `shouldBe` Left (OutdatedState 1 1)

    it "rejects counter-dispute on non-disputed channel (Active)" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (_ch1, ss1) = createPayment ch keysA keysB PartyA 200
      counterDispute ch ss1 t0
        `shouldBe` Left (InvalidChannelStatus Disputed Active)

    it "rejects counter-dispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
          Right closed = cooperativeClose ch1 keysA keysB
      counterDispute closed ss1 t0
        `shouldBe` Left (InvalidChannelStatus Disputed Closed)

    it "rejects counter-dispute with invalid signatures" $ do
      let (disputed, _keysA, keysB, _ss) = setupDisputedChannel
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState 2 (Balance 700 800)
          sigC = signState keysC st
          sigB = signState keysB st
          badSigned = SignedState st sigC sigB
      counterDispute disputed badSigned (addUTCTime 1 t0)
        `shouldBe` Left (InvalidSignature PartyA)

    it "counter-dispute updates latest state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          Right disputed = unilateralClose ch2 ss1 t0
          Right countered = counterDispute disputed ss2 (addUTCTime 1 t0)
      chLatestState countered `shouldBe` Just ss2

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

  ---------------------------------------------------------------------------
  -- Channel.Dispute - resolveDispute
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - resolveDispute" $ do
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

    it "resolves dispute without counter (original dispute state wins)" $ do
      let (ch', _keysA, _keysB, ss) = setupWithPayment
          Right disputed = unilateralClose ch' ss t0
          afterChallenge = addUTCTime 20 t0
      case resolveDispute disputed afterChallenge of
        Left err -> expectationFailure $ show err
        Right resolved -> do
          chStatus resolved `shouldBe` Closed
          -- The dispute state becomes the latest state
          chLatestState resolved `shouldBe` chDisputeState disputed

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

    it "rejects resolveDispute on non-disputed channel (Active)" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      resolveDispute ch t0
        `shouldBe` Left (InvalidChannelStatus Disputed Active)

    it "rejects resolveDispute on open channel" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      resolveDispute ch t0
        `shouldBe` Left (InvalidChannelStatus Disputed Open)

    it "rejects resolveDispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right closed = cooperativeClose ch keysA keysB
      resolveDispute closed t0
        `shouldBe` Left (InvalidChannelStatus Disputed Closed)

    it "resolveDispute just before deadline fails" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
          justBefore = addUTCTime 9.999 t0
      resolveDispute disputed justBefore
        `shouldBe` Left DisputePeriodActive

  ---------------------------------------------------------------------------
  -- Channel.Dispute - isChallengePeriodExpired
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - isChallengePeriodExpired" $ do
    it "returns False when no deadline is set" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
      isChallengePeriodExpired ch t0 `shouldBe` False

    it "returns False for active channel without dispute" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      isChallengePeriodExpired ch t0 `shouldBe` False

    it "returns False before deadline" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
      isChallengePeriodExpired disputed (addUTCTime 5 t0) `shouldBe` False

    it "returns True at deadline" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
      isChallengePeriodExpired disputed (addUTCTime 10 t0) `shouldBe` True

    it "returns True after deadline" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
      isChallengePeriodExpired disputed (addUTCTime 15 t0) `shouldBe` True

    it "returns True well after deadline" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
      isChallengePeriodExpired disputed (addUTCTime 1000000 t0) `shouldBe` True

  ---------------------------------------------------------------------------
  -- Channel.Dispute - Full dispute lifecycle
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - Full lifecycle" $ do
    it "allows unilateral close and starts challenge period" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
      case unilateralClose ch1 ss1 t0 of
        Left err -> expectationFailure $ show err
        Right disputed -> do
          chStatus disputed `shouldBe` Disputed
          chDisputeDeadline disputed `shouldSatisfy` (/= Nothing)

    it "full dispute-counter-resolve lifecycle works" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, ss3) = createPayment ch2 keysA keysB PartyA 50
      -- Step 1: raiseDispute with old state
      case raiseDispute ch3 ss1 t0 of
        Left err -> expectationFailure $ show err
        Right disputed -> do
          chStatus disputed `shouldBe` Disputed
          -- Step 2: counter with newer state
          case counterDispute disputed ss3 (addUTCTime 2 t0) of
            Left err -> expectationFailure $ show err
            Right countered -> do
              -- Step 3: resolve after challenge period
              case resolveDispute countered (addUTCTime 20 t0) of
                Left err -> expectationFailure $ show err
                Right resolved -> do
                  chStatus resolved `shouldBe` Closed
                  case chLatestState resolved of
                    Nothing -> expectationFailure "Expected final state"
                    Just ss -> do
                      csNonce (ssState ss) `shouldBe` 3
                      csBalance (ssState ss) `shouldBe` Balance 850 650

    it "dispute without counter resolves to original dispute state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch keysA keysB PartyA 200
      case raiseDispute ch1 ss1 t0 of
        Left err -> expectationFailure $ show err
        Right disputed ->
          case resolveDispute disputed (addUTCTime 20 t0) of
            Left err -> expectationFailure $ show err
            Right resolved -> do
              chStatus resolved `shouldBe` Closed
              case chLatestState resolved of
                Nothing -> expectationFailure "Expected final state"
                Just ss -> do
                  csNonce (ssState ss) `shouldBe` 1
                  csBalance (ssState ss) `shouldBe` Balance 800 700

    it "multiple counter-disputes work correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, ss1) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, ss2) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, ss3) = createPayment ch2 keysA keysB PartyA 50
          -- Dispute with nonce 1
          Right disputed = unilateralClose ch3 ss1 t0
          -- First counter with nonce 2
          Right countered1 = counterDispute disputed ss2 (addUTCTime 1 t0)
          -- Second counter with nonce 3
          Right countered2 = counterDispute countered1 ss3 (addUTCTime 2 t0)
      case chDisputeState countered2 of
        Nothing -> expectationFailure "Expected dispute state"
        Just ss -> do
          csNonce (ssState ss) `shouldBe` 3
          csBalance (ssState ss) `shouldBe` Balance 850 650

  ---------------------------------------------------------------------------
  -- Channel.State - defaultConfig usage
  ---------------------------------------------------------------------------
  describe "Channel with defaultConfig" $ do
    it "uses 24-hour challenge period from defaultConfig" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch = openChannel "chan-def" keysA (pkPublic keysB) 1000 defaultConfig
          Right active = activateChannel ch keysA keysB 500
          Right (ch1, ss1) = createPayment active keysA keysB PartyA 200
          Right disputed = unilateralClose ch1 ss1 t0
      -- Should NOT be expired after 12 hours (43200 seconds)
      isChallengePeriodExpired disputed (addUTCTime 43200 t0) `shouldBe` False
      -- Should be expired after 24 hours (86400 seconds)
      isChallengePeriodExpired disputed (addUTCTime 86400 t0) `shouldBe` True

  ---------------------------------------------------------------------------
  -- Channel.State - Boundary and special cases
  ---------------------------------------------------------------------------
  describe "Channel.State - Boundary cases" $ do
    it "handles deposit of 1 (minimum valid)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      case openChannel "chan-min" keysA (pkPublic keysB) 1 testConfig of
        Left err -> expectationFailure $ show err
        Right ch -> do
          chStatus ch `shouldBe` Open
          balanceA (chDeposits ch) `shouldBe` 1

    it "handles very large deposits" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      case openChannel "chan-big" keysA (pkPublic keysB) 999999999999 testConfig of
        Left err -> expectationFailure $ show err
        Right ch -> do
          balanceA (chDeposits ch) `shouldBe` 999999999999

    it "payment of exactly 1 unit works" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case createPayment ch keysA keysB PartyA 1 of
        Left err -> expectationFailure $ show err
        Right (ch', _) ->
          channelBalance ch' `shouldBe` Just (Balance 999 501)

    it "multiple payments that drain one party entirely" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          -- PartyA has 1000, send it all to B
          Right (ch1, _) = createPayment ch keysA keysB PartyA 1000
      channelBalance ch1 `shouldBe` Just (Balance 0 1500)
      -- Now A can't send any more
      createPayment ch1 keysA keysB PartyA 1
        `shouldBe` Left (InsufficientBalance PartyA 1)
      -- But B can send back
      case createPayment ch1 keysA keysB PartyB 500 of
        Left err -> expectationFailure $ show err
        Right (ch2, _) ->
          channelBalance ch2 `shouldBe` Just (Balance 500 1000)

    it "balance sum is preserved through payments" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          initialTotal = totalDeposits ch
          Right (ch1, _) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, _) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, _) = createPayment ch2 keysA keysB PartyA 50
      case channelBalance ch3 of
        Nothing -> expectationFailure "Expected balance"
        Just b -> (balanceA b + balanceB b) `shouldBe` initialTotal

    it "channel ID is preserved through lifecycle" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch1 = openChannel "my-unique-id" keysA (pkPublic keysB) 1000 testConfig
          Right ch2 = activateChannel ch1 keysA keysB 500
          Right (ch3, _) = createPayment ch2 keysA keysB PartyA 200
          Right ch4 = cooperativeClose ch3 keysA keysB
      chId ch1 `shouldBe` "my-unique-id"
      chId ch2 `shouldBe` "my-unique-id"
      chId ch3 `shouldBe` "my-unique-id"
      chId ch4 `shouldBe` "my-unique-id"

    it "party keys are preserved through lifecycle" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          Right ch1 = openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig
          Right ch2 = activateChannel ch1 keysA keysB 500
          Right (ch3, _) = createPayment ch2 keysA keysB PartyA 200
      chPartyA ch1 `shouldBe` pkPublic keysA
      chPartyB ch1 `shouldBe` pkPublic keysB
      chPartyA ch2 `shouldBe` pkPublic keysA
      chPartyB ch2 `shouldBe` pkPublic keysB
      chPartyA ch3 `shouldBe` pkPublic keysA
      chPartyB ch3 `shouldBe` pkPublic keysB

  ---------------------------------------------------------------------------
  -- Signature integrity across state transitions
  ---------------------------------------------------------------------------
  describe "Signature integrity" $ do
    it "latest signed state is always verifiable after payment" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          Right (ch1, _) = createPayment ch  keysA keysB PartyA 200
          Right (ch2, _) = createPayment ch1 keysA keysB PartyB 100
          Right (ch3, _) = createPayment ch2 keysA keysB PartyA 50
      case chLatestState ch3 of
        Nothing -> expectationFailure "Expected state"
        Just ss ->
          verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "initial state after activation is verifiable" $ do
      let (ch, keysA, keysB) = setupActiveChannel
      case chLatestState ch of
        Nothing -> expectationFailure "Expected initial state"
        Just ss ->
          verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "dispute state is verifiable" $ do
      let (disputed, keysA, keysB, _ss) = setupDisputedChannel
      case chDisputeState disputed of
        Nothing -> expectationFailure "Expected dispute state"
        Just ss ->
          verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True
