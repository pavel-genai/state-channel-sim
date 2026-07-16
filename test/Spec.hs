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

-- | Extract the 'Right' value from an 'Either', failing the test loudly if it
-- is a 'Left'. Using this instead of @case ... of Left err -> expectationFailure@
-- keeps the happy-path branches fully covered by HPC.
fromRight' :: Show e => Either e a -> a
fromRight' (Right a) = a
fromRight' (Left e)  = error ("expected Right, got Left: " ++ show e)

-- | Extract the 'Just' value from a 'Maybe', failing the test loudly if it is
-- 'Nothing'.
fromJust' :: Maybe a -> a
fromJust' (Just a)  = a
fromJust' Nothing   = error "expected Just, got Nothing"

-- | Set up a full active channel for testing.
setupActiveChannel :: (Channel, PartyKeys, PartyKeys)
setupActiveChannel =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      ch1 = fromRight' (openChannel "test-chan" keysA (pkPublic keysB) 1000 testConfig)
      ch2 = fromRight' (activateChannel ch1 keysA keysB 500)
  in  (ch2, keysA, keysB)

-- | Set up an active channel and make a payment, returning the channel,
-- keys, and the signed state produced by the payment.
setupWithPayment :: (Channel, PartyKeys, PartyKeys, SignedState)
setupWithPayment =
  let (ch, keysA, keysB) = setupActiveChannel
      (ch', ss) = fromRight' (createPayment ch keysA keysB PartyA 200)
  in  (ch', keysA, keysB, ss)

-- | Set up a disputed channel for testing dispute-related functions.
setupDisputedChannel :: (Channel, PartyKeys, PartyKeys, SignedState)
setupDisputedChannel =
  let (ch', keysA, keysB, ss) = setupWithPayment
      disputed = fromRight' (unilateralClose ch' ss t0)
  in  (disputed, keysA, keysB, ss)

-- | Construct an Active channel whose latest signed state is missing. This is
-- not reachable through the public API (activation always sets a state) but is
-- used to exercise the defensive @Left ChannelNotFound@ branches.
mkActiveChannelNoState :: Channel
mkActiveChannelNoState =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
  in Channel
       { chId              = "no-state"
       , chStatus          = Active
       , chPartyA          = pkPublic keysA
       , chPartyB          = pkPublic keysB
       , chDeposits        = Balance 1000 500
       , chLatestState     = Nothing
       , chDisputeState    = Nothing
       , chDisputeDeadline = Nothing
       , chConfig          = testConfig
       }

-- | Construct a Disputed channel whose dispute state is missing, to exercise
-- the defensive @Left ChannelNotFound@ branch in 'counterDispute'.
mkDisputedChannelNoDisputeState :: Channel
mkDisputedChannelNoDisputeState =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
  in Channel
       { chId              = "no-dispute-state"
       , chStatus          = Disputed
       , chPartyA          = pkPublic keysA
       , chPartyB          = pkPublic keysB
       , chDeposits        = Balance 1000 500
       , chLatestState     = Nothing
       , chDisputeState    = Nothing
       , chDisputeDeadline = Just (addUTCTime 10 t0)
       , chConfig          = testConfig
       }

-- | Construct a Disputed channel whose dispute deadline is missing, to
-- exercise the defensive @Left ChannelNotFound@ branch in 'resolveDispute'.
mkDisputedChannelNoDeadline :: Channel
mkDisputedChannelNoDeadline =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
  in Channel
       { chId              = "no-deadline"
       , chStatus          = Disputed
       , chPartyA          = pkPublic keysA
       , chPartyB          = pkPublic keysB
       , chDeposits        = Balance 1000 500
       , chLatestState     = Nothing
       , chDisputeState    = Nothing
       , chDisputeDeadline = Nothing
       , chConfig          = testConfig
       }

-- | Construct an Active channel whose latest signed state carries invalid
-- signatures, to exercise the @Left (InvalidSignature PartyA)@ branch of
-- 'cooperativeClose'.
mkActiveChannelBadSigs :: (Channel, PartyKeys, PartyKeys)
mkActiveChannelBadSigs =
  let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
      keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
      keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
      st    = ChannelState 0 (Balance 1000 500)
      ss    = SignedState st (signState keysC st) (signState keysB st)
      ch    = Channel
        { chId              = "bad-sigs"
        , chStatus          = Active
        , chPartyA          = pkPublic keysA
        , chPartyB          = pkPublic keysB
        , chDeposits        = Balance 1000 500
        , chLatestState     = Just ss
        , chDisputeState    = Nothing
        , chDisputeDeadline = Nothing
        , chConfig          = testConfig
        }
  in (ch, keysA, keysB)

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
      verifySignature (pkPublic keysA) st sigB `shouldBe` False

    it "rejects signature for different state" $ do
      let keys = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          st1 = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          st2 = ChannelState { csNonce = 2, csBalance = Balance 500 500 }
          sig1 = signState keys st1
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
          st = ChannelState { csNonce = 0, csBalance = Balance 0 0 }
          encoded = encodeState st
          sigFromState = signStateBytes (pkSecret keys) (pkPublic keys) encoded
          sigFromSign = signState keys st
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
          sigC = signState keysC st
          sigB = signState keysB st
          signed = SignedState st sigC sigB
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

    it "verifySignedState rejects when Party B signature is invalid" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState { csNonce = 1, csBalance = Balance 500 500 }
          sigA = signState keysA st
          sigC = signState keysC st
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
          signed = SignedState st sigB sigA
      verifySignedState (pkPublic keysA) (pkPublic keysB) signed `shouldBe` False

  ---------------------------------------------------------------------------
  -- Channel.State - Happy Path
  ---------------------------------------------------------------------------
  describe "Channel.State - Happy Path" $ do
    it "opens a channel in Open status" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      chStatus ch `shouldBe` Open
      balanceA (chDeposits ch) `shouldBe` 1000
      balanceB (chDeposits ch) `shouldBe` 0

    it "opens a channel with correct ID and keys" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "my-chan" keysA (pkPublic keysB) 500 testConfig)
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
          ss = fromJust' (chLatestState ch)
      csNonce (ssState ss) `shouldBe` 0
      csBalance (ssState ss) `shouldBe` Balance 1000 500

    it "processes off-chain payments correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 200)
      channelBalance ch' `shouldBe` Just (Balance 800 700)
      let ss = fromJust' (chLatestState ch')
      csNonce (ssState ss) `shouldBe` 1

    it "processes PartyB payment correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyB 100)
      channelBalance ch' `shouldBe` Just (Balance 1100 400)
      let ss = fromJust' (chLatestState ch')
      csNonce (ssState ss) `shouldBe` 1

    it "processes multiple payments and tracks nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, _) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, _) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
      channelBalance ch3 `shouldBe` Just (Balance 850 650)
      let ss = fromJust' (chLatestState ch3)
      csNonce (ssState ss) `shouldBe` 3

    it "creates payment that returns valid signed state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (_, ss) = fromRight' (createPayment ch keysA keysB PartyA 200)
      csNonce (ssState ss) `shouldBe` 1
      csBalance (ssState ss) `shouldBe` Balance 800 700
      verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "performs cooperative close" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 200)
          closed = fromRight' (cooperativeClose ch' keysA keysB)
      chStatus closed `shouldBe` Closed
      channelBalance closed `shouldBe` Just (Balance 800 700)

    it "cooperative close preserves latest state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', ss) = fromRight' (createPayment ch keysA keysB PartyA 300)
          closed = fromRight' (cooperativeClose ch' keysA keysB)
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
          closed = fromRight' (cooperativeClose ch keysA keysB)
      activateChannel closed keysA keysB 500
        `shouldBe` Left (InvalidChannelStatus Open Closed)

    it "rejects activating with zero deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      activateChannel ch keysA keysB 0
        `shouldBe` Left (InsufficientBalance PartyB 0)

    it "rejects activating with negative deposit" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      activateChannel ch keysA keysB (-50)
        `shouldBe` Left (InsufficientBalance PartyB (-50))

    it "rejects payment on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-x" keysA (pkPublic keysB) 1000 testConfig)
      createPayment ch keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects payment on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          closed = fromRight' (cooperativeClose ch keysA keysB)
      createPayment closed keysA keysB PartyA 100
        `shouldBe` Left (InvalidChannelStatus Active Closed)

    it "rejects payment on disputed channel" $ do
      let (ch', keysA, keysB, ss) = setupWithPayment
          disputed = fromRight' (unilateralClose ch' ss t0)
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
      createPayment ch keysA keysB PartyB 501
        `shouldBe` Left (InsufficientBalance PartyB 501)

    it "allows payment exactly equal to PartyA balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 1000)
      channelBalance ch' `shouldBe` Just (Balance 0 1500)

    it "allows payment exactly equal to PartyB balance" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyB 500)
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
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      cooperativeClose ch keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects cooperative close on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          closed = fromRight' (cooperativeClose ch keysA keysB)
      cooperativeClose closed keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Closed)

    it "rejects cooperative close on disputed channel" $ do
      let (ch', keysA, keysB, ss) = setupWithPayment
          disputed = fromRight' (unilateralClose ch' ss t0)
      cooperativeClose disputed keysA keysB
        `shouldBe` Left (InvalidChannelStatus Active Disputed)

    it "rejects unilateral close on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
          st = ChannelState 0 (Balance 1000 0)
          sig = signState keysA st
          ss = SignedState st sig sig
      unilateralClose ch ss t0
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects unilateral close with invalid signatures" $ do
      let (ch, _keysA, keysB) = setupActiveChannel
          keysC = makeTestKeys "CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC"
          st = ChannelState 1 (Balance 800 700)
          sigC = signState keysC st
          sigB = signState keysB st
          badSigned = SignedState st sigC sigB
      unilateralClose ch badSigned t0
        `shouldBe` Left (InvalidSignature PartyA)

    it "unilateral close sets correct deadline" $ do
      let (ch', _keysA, _keysB, ss) = setupWithPayment
          disputed = fromRight' (unilateralClose ch' ss t0)
      chDisputeDeadline disputed `shouldBe` Just (addUTCTime 10 t0)
      chDisputeState disputed `shouldBe` Just ss

  ---------------------------------------------------------------------------
  -- Channel.State - Defensive / inconsistent-state branches
  ---------------------------------------------------------------------------
  describe "Channel.State - Defensive branches" $ do
    it "createPayment returns ChannelNotFound when Active channel has no latest state" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          noStateCh = mkActiveChannelNoState
      createPayment noStateCh keysA keysB PartyA 100
        `shouldBe` Left ChannelNotFound

    it "cooperativeClose returns ChannelNotFound when Active channel has no latest state" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          noStateCh = mkActiveChannelNoState
      cooperativeClose noStateCh keysA keysB
        `shouldBe` Left ChannelNotFound

    it "cooperativeClose returns InvalidSignature when latest state has bad signatures" $ do
      let (badCh, keysA, keysB) = mkActiveChannelBadSigs
      cooperativeClose badCh keysA keysB
        `shouldBe` Left (InvalidSignature PartyA)

  ---------------------------------------------------------------------------
  -- Channel.State - Queries
  ---------------------------------------------------------------------------
  describe "Channel.State - Queries" $ do
    it "channelBalance returns Nothing for channel with no state" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      channelBalance ch `shouldBe` Nothing

    it "channelBalance returns balance from latest state" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      channelBalance ch `shouldBe` Just (Balance 1000 500)

    it "channelBalance updates after payment" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 200)
      channelBalance ch' `shouldBe` Just (Balance 800 700)

    it "totalDeposits is correct for open channel" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      totalDeposits ch `shouldBe` 1000

    it "totalDeposits is correct for active channel" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      totalDeposits ch `shouldBe` 1500

    it "totalDeposits does not change after payments" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 200)
      totalDeposits ch' `shouldBe` 1500

  ---------------------------------------------------------------------------
  -- Channel.Dispute - raiseDispute
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - raiseDispute" $ do
    it "raises dispute on active channel with valid signed state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          disputed = fromRight' (raiseDispute ch1 ss1 t0)
      chStatus disputed `shouldBe` Disputed
      chDisputeState disputed `shouldBe` Just ss1
      chDisputeDeadline disputed `shouldBe` Just (addUTCTime 10 t0)

    it "rejects raiseDispute on non-active channel (Open)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
          st = ChannelState 0 (Balance 1000 0)
          sig = signState keysA st
          ss = SignedState st sig sig
      raiseDispute ch ss t0
        `shouldBe` Left (InvalidChannelStatus Active Open)

    it "rejects raiseDispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          closed = fromRight' (cooperativeClose ch keysA keysB)
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
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, ss2) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          disputed = fromRight' (unilateralClose ch2 ss1 t0)
          countered = fromRight' (counterDispute disputed ss2 (addUTCTime 1 t0))
      let ss = fromJust' (chDisputeState countered)
      csNonce (ssState ss) `shouldBe` 2

    it "rejects counter-dispute with same or lower nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, ss2) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          disputed = fromRight' (unilateralClose ch2 ss2 t0)
      counterDispute disputed ss1 (addUTCTime 1 t0)
        `shouldBe` Left (OutdatedState 2 1)

    it "rejects counter-dispute with same nonce" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          disputed = fromRight' (unilateralClose ch1 ss1 t0)
      counterDispute disputed ss1 (addUTCTime 1 t0)
        `shouldBe` Left (OutdatedState 1 1)

    it "rejects counter-dispute on non-disputed channel (Active)" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (_ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
      counterDispute ch ss1 t0
        `shouldBe` Left (InvalidChannelStatus Disputed Active)

    it "rejects counter-dispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          closed = fromRight' (cooperativeClose ch1 keysA keysB)
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
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, ss2) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          disputed = fromRight' (unilateralClose ch2 ss1 t0)
          countered = fromRight' (counterDispute disputed ss2 (addUTCTime 1 t0))
      chLatestState countered `shouldBe` Just ss2

    it "counter-dispute resets challenge period" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, ss2) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          disputed = fromRight' (unilateralClose ch2 ss1 t0)
          t5 = addUTCTime 5 t0
          countered = fromRight' (counterDispute disputed ss2 t5)
      isChallengePeriodExpired countered (addUTCTime 10 t0) `shouldBe` False
      isChallengePeriodExpired countered (addUTCTime 15 t0) `shouldBe` True

  ---------------------------------------------------------------------------
  -- Channel.Dispute - resolveDispute
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - resolveDispute" $ do
    it "resolves dispute with correct final state after timeout" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, ss3) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
          disputed = fromRight' (unilateralClose ch3 ss1 t0)
          countered = fromRight' (counterDispute disputed ss3 (addUTCTime 1 t0))
          afterChallenge = addUTCTime 20 t0
          resolved = fromRight' (resolveDispute countered afterChallenge)
      chStatus resolved `shouldBe` Closed
      let ss = fromJust' (chLatestState resolved)
      csNonce (ssState ss) `shouldBe` 3
      csBalance (ssState ss) `shouldBe` Balance 850 650

    it "resolves dispute without counter (original dispute state wins)" $ do
      let (ch', _keysA, _keysB, ss) = setupWithPayment
          disputed = fromRight' (unilateralClose ch' ss t0)
          afterChallenge = addUTCTime 20 t0
          resolved = fromRight' (resolveDispute disputed afterChallenge)
      chStatus resolved `shouldBe` Closed
      chLatestState resolved `shouldBe` chDisputeState disputed

    it "rejects dispute resolution before challenge period expires" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          disputed = fromRight' (unilateralClose ch1 ss1 t0)
          duringChallenge = addUTCTime 5 t0
      resolveDispute disputed duringChallenge
        `shouldBe` Left DisputePeriodActive

    it "allows resolution exactly at deadline" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          disputed = fromRight' (unilateralClose ch1 ss1 t0)
          atDeadline = addUTCTime 10 t0
          resolved = fromRight' (resolveDispute disputed atDeadline)
      chStatus resolved `shouldBe` Closed

    it "rejects resolveDispute on non-disputed channel (Active)" $ do
      let (ch, _keysA, _keysB) = setupActiveChannel
      resolveDispute ch t0
        `shouldBe` Left (InvalidChannelStatus Disputed Active)

    it "rejects resolveDispute on open channel" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
      resolveDispute ch t0
        `shouldBe` Left (InvalidChannelStatus Disputed Open)

    it "rejects resolveDispute on closed channel" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          closed = fromRight' (cooperativeClose ch keysA keysB)
      resolveDispute closed t0
        `shouldBe` Left (InvalidChannelStatus Disputed Closed)

    it "resolveDispute just before deadline fails" $ do
      let (disputed, _keysA, _keysB, _ss) = setupDisputedChannel
          justBefore = addUTCTime 9.999 t0
      resolveDispute disputed justBefore
        `shouldBe` Left DisputePeriodActive

  ---------------------------------------------------------------------------
  -- Channel.Dispute - Defensive / inconsistent-state branches
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - Defensive branches" $ do
    it "counterDispute returns ChannelNotFound when dispute state is missing" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (_, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          noDisputeStateCh = mkDisputedChannelNoDisputeState
      counterDispute noDisputeStateCh ss1 (addUTCTime 1 t0)
        `shouldBe` Left ChannelNotFound

    it "resolveDispute returns ChannelNotFound when deadline is missing" $ do
      let noDeadlineCh = mkDisputedChannelNoDeadline
      resolveDispute noDeadlineCh t0
        `shouldBe` Left ChannelNotFound

  ---------------------------------------------------------------------------
  -- Channel.Dispute - isChallengePeriodExpired
  ---------------------------------------------------------------------------
  describe "Channel.Dispute - isChallengePeriodExpired" $ do
    it "returns False when no deadline is set" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
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
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          disputed = fromRight' (unilateralClose ch1 ss1 t0)
      chStatus disputed `shouldBe` Disputed
      chDisputeDeadline disputed `shouldSatisfy` (/= Nothing)

    it "full dispute-counter-resolve lifecycle works" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, ss3) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
          disputed = fromRight' (raiseDispute ch3 ss1 t0)
          countered = fromRight' (counterDispute disputed ss3 (addUTCTime 2 t0))
          resolved = fromRight' (resolveDispute countered (addUTCTime 20 t0))
      chStatus resolved `shouldBe` Closed
      let ss = fromJust' (chLatestState resolved)
      csNonce (ssState ss) `shouldBe` 3
      csBalance (ssState ss) `shouldBe` Balance 850 650

    it "dispute without counter resolves to original dispute state" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch keysA keysB PartyA 200)
          disputed = fromRight' (raiseDispute ch1 ss1 t0)
          resolved = fromRight' (resolveDispute disputed (addUTCTime 20 t0))
      chStatus resolved `shouldBe` Closed
      let ss = fromJust' (chLatestState resolved)
      csNonce (ssState ss) `shouldBe` 1
      csBalance (ssState ss) `shouldBe` Balance 800 700

    it "multiple counter-disputes work correctly" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, ss1) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, ss2) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, ss3) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
          disputed = fromRight' (unilateralClose ch3 ss1 t0)
          countered1 = fromRight' (counterDispute disputed ss2 (addUTCTime 1 t0))
          countered2 = fromRight' (counterDispute countered1 ss3 (addUTCTime 2 t0))
      let ss = fromJust' (chDisputeState countered2)
      csNonce (ssState ss) `shouldBe` 3
      csBalance (ssState ss) `shouldBe` Balance 850 650

  ---------------------------------------------------------------------------
  -- Channel.State - defaultConfig usage
  ---------------------------------------------------------------------------
  describe "Channel with defaultConfig" $ do
    it "uses 24-hour challenge period from defaultConfig" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-def" keysA (pkPublic keysB) 1000 defaultConfig)
          active = fromRight' (activateChannel ch keysA keysB 500)
          (ch1, ss1) = fromRight' (createPayment active keysA keysB PartyA 200)
          disputed = fromRight' (unilateralClose ch1 ss1 t0)
      isChallengePeriodExpired disputed (addUTCTime 43200 t0) `shouldBe` False
      isChallengePeriodExpired disputed (addUTCTime 86400 t0) `shouldBe` True

  ---------------------------------------------------------------------------
  -- Channel.State - Boundary and special cases
  ---------------------------------------------------------------------------
  describe "Channel.State - Boundary cases" $ do
    it "handles deposit of 1 (minimum valid)" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-min" keysA (pkPublic keysB) 1 testConfig)
      chStatus ch `shouldBe` Open
      balanceA (chDeposits ch) `shouldBe` 1

    it "handles very large deposits" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch = fromRight' (openChannel "chan-big" keysA (pkPublic keysB) 999999999999 testConfig)
      balanceA (chDeposits ch) `shouldBe` 999999999999

    it "payment of exactly 1 unit works" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch', _) = fromRight' (createPayment ch keysA keysB PartyA 1)
      channelBalance ch' `shouldBe` Just (Balance 999 501)

    it "multiple payments that drain one party entirely" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          (ch1, _) = fromRight' (createPayment ch keysA keysB PartyA 1000)
      channelBalance ch1 `shouldBe` Just (Balance 0 1500)
      createPayment ch1 keysA keysB PartyA 1
        `shouldBe` Left (InsufficientBalance PartyA 1)
      let (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 500)
      channelBalance ch2 `shouldBe` Just (Balance 500 1000)

    it "balance sum is preserved through payments" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          initialTotal = totalDeposits ch
          (ch1, _) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, _) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
          b = fromJust' (channelBalance ch3)
      (balanceA b + balanceB b) `shouldBe` initialTotal

    it "channel ID is preserved through lifecycle" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch1 = fromRight' (openChannel "my-unique-id" keysA (pkPublic keysB) 1000 testConfig)
          ch2 = fromRight' (activateChannel ch1 keysA keysB 500)
          (ch3, _) = fromRight' (createPayment ch2 keysA keysB PartyA 200)
          ch4 = fromRight' (cooperativeClose ch3 keysA keysB)
      chId ch1 `shouldBe` "my-unique-id"
      chId ch2 `shouldBe` "my-unique-id"
      chId ch3 `shouldBe` "my-unique-id"
      chId ch4 `shouldBe` "my-unique-id"

    it "party keys are preserved through lifecycle" $ do
      let keysA = makeTestKeys "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"
          keysB = makeTestKeys "BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB"
          ch1 = fromRight' (openChannel "chan-1" keysA (pkPublic keysB) 1000 testConfig)
          ch2 = fromRight' (activateChannel ch1 keysA keysB 500)
          (ch3, _) = fromRight' (createPayment ch2 keysA keysB PartyA 200)
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
          (ch1, _) = fromRight' (createPayment ch  keysA keysB PartyA 200)
          (ch2, _) = fromRight' (createPayment ch1 keysA keysB PartyB 100)
          (ch3, _) = fromRight' (createPayment ch2 keysA keysB PartyA 50)
          ss = fromJust' (chLatestState ch3)
      verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "initial state after activation is verifiable" $ do
      let (ch, keysA, keysB) = setupActiveChannel
          ss = fromJust' (chLatestState ch)
      verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True

    it "dispute state is verifiable" $ do
      let (disputed, keysA, keysB, _ss) = setupDisputedChannel
          ss = fromJust' (chDisputeState disputed)
      verifySignedState (pkPublic keysA) (pkPublic keysB) ss `shouldBe` True