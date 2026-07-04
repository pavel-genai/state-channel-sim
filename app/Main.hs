{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Channel.Crypto
import           Channel.Dispute
import           Channel.State
import           Channel.Types

import           Data.Time.Clock       (NominalDiffTime, UTCTime(..), addUTCTime,
                                        secondsToDiffTime)
import           Data.Time.Calendar    (fromGregorian)

main :: IO ()
main = do
  putStrLn "=== State Channel Simulator ==="
  putStrLn ""

  -- Generate key pairs for both parties
  putStrLn "[1] Generating Ed25519 key pairs..."
  keysA <- generateKeyPair
  keysB <- generateKeyPair
  putStrLn $ "    Party A public key: " ++ show (pkPublic keysA)
  putStrLn $ "    Party B public key: " ++ show (pkPublic keysB)
  putStrLn ""

  -- Open a channel with Party A's deposit
  putStrLn "[2] Opening channel with Party A deposit of 1000..."
  let config = ChannelConfig { ccChallengePeriod = 10 }  -- 10 seconds for demo
  case openChannel "chan-001" keysA (pkPublic keysB) 1000 config of
    Left err -> putStrLn $ "    ERROR: " ++ show err
    Right ch1 -> do
      putStrLn $ "    Channel status: " ++ show (chStatus ch1)
      putStrLn $ "    Deposits: " ++ show (chDeposits ch1)
      putStrLn ""

      -- Activate the channel with Party B's deposit
      putStrLn "[3] Activating channel with Party B deposit of 500..."
      case activateChannel ch1 keysA keysB 500 of
        Left err -> putStrLn $ "    ERROR: " ++ show err
        Right ch2 -> do
          putStrLn $ "    Channel status: " ++ show (chStatus ch2)
          putStrLn $ "    Initial balance: " ++ showBalance ch2
          putStrLn ""

          -- Off-chain payment: A sends 200 to B
          putStrLn "[4] Off-chain payment: A sends 200 to B..."
          case createPayment ch2 keysA keysB PartyA 200 of
            Left err -> putStrLn $ "    ERROR: " ++ show err
            Right (ch3, _ss1) -> do
              putStrLn $ "    Balance after payment: " ++ showBalance ch3
              putStrLn ""

              -- Off-chain payment: B sends 50 to A
              putStrLn "[5] Off-chain payment: B sends 50 to A..."
              case createPayment ch3 keysA keysB PartyB 50 of
                Left err -> putStrLn $ "    ERROR: " ++ show err
                Right (ch4, ss2) -> do
                  putStrLn $ "    Balance after payment: " ++ showBalance ch4
                  putStrLn $ "    Current nonce: " ++ showNonce ch4
                  putStrLn ""

                  -- Another payment: A sends 100 to B
                  putStrLn "[6] Off-chain payment: A sends 100 to B..."
                  case createPayment ch4 keysA keysB PartyA 100 of
                    Left err -> putStrLn $ "    ERROR: " ++ show err
                    Right (ch5, ss3) -> do
                      putStrLn $ "    Balance after payment: " ++ showBalance ch5
                      putStrLn $ "    Current nonce: " ++ showNonce ch5
                      putStrLn ""

                      -- Demonstrate cooperative close
                      putStrLn "[7] Cooperative close..."
                      case cooperativeClose ch5 keysA keysB of
                        Left err -> putStrLn $ "    ERROR: " ++ show err
                        Right ch6 -> do
                          putStrLn $ "    Channel status: " ++ show (chStatus ch6)
                          putStrLn $ "    Final balance: " ++ showBalance ch6
                          putStrLn ""

                      -- Now demonstrate dispute scenario (using ch5 which is still Active)
                      putStrLn "--- Dispute Scenario ---"
                      putStrLn ""

                      -- Party B tries unilateral close with an old state (ss2, nonce 2)
                      putStrLn "[8] Party B submits outdated state (nonce 2) for unilateral close..."
                      let t0 = baseTime
                      case unilateralClose ch5 ss2 t0 of
                        Left err -> putStrLn $ "    ERROR: " ++ show err
                        Right ch7 -> do
                          putStrLn $ "    Channel status: " ++ show (chStatus ch7)
                          putStrLn $ "    Dispute state nonce: " ++ showDisputeNonce ch7
                          putStrLn ""

                          -- Party A counters with latest state (ss3, nonce 3)
                          putStrLn "[9] Party A counters with latest state (nonce 3)..."
                          let t1 = addUTCTime 5 t0  -- 5 seconds later
                          case counterDispute ch7 ss3 t1 of
                            Left err -> putStrLn $ "    ERROR: " ++ show err
                            Right ch8 -> do
                              putStrLn $ "    Dispute state updated to nonce: " ++ showDisputeNonce ch8
                              putStrLn ""

                              -- Try to resolve before challenge period expires
                              putStrLn "[10] Attempting to resolve before challenge period expires..."
                              let t2 = addUTCTime 3 t1  -- only 3 seconds later
                              case resolveDispute ch8 t2 of
                                Left DisputePeriodActive -> do
                                  putStrLn "    Correctly rejected: challenge period still active"
                                  putStrLn ""
                                Left err -> putStrLn $ "    ERROR: " ++ show err
                                Right _ -> putStrLn "    Unexpected success"

                              -- Resolve after challenge period expires
                              putStrLn "[11] Resolving after challenge period expires..."
                              let t3 = addUTCTime 20 t1  -- 20 seconds later (> 10s challenge)
                              case resolveDispute ch8 t3 of
                                Left err -> putStrLn $ "    ERROR: " ++ show err
                                Right ch9 -> do
                                  putStrLn $ "    Channel status: " ++ show (chStatus ch9)
                                  putStrLn $ "    Final balance: " ++ showBalance ch9
                                  putStrLn ""

                      putStrLn "=== Simulation Complete ==="

-- | Base time for the dispute simulation.
baseTime :: UTCTime
baseTime = UTCTime (fromGregorian 2026 1 1) (secondsToDiffTime 0)

-- | Show the current balance from a channel.
showBalance :: Channel -> String
showBalance ch = case channelBalance ch of
  Nothing -> "no state"
  Just b  -> "A=" ++ show (balanceA b) ++ " B=" ++ show (balanceB b)

-- | Show the current nonce from the latest state.
showNonce :: Channel -> String
showNonce ch = case chLatestState ch of
  Nothing -> "none"
  Just ss -> show (csNonce (ssState ss))

-- | Show the dispute state nonce.
showDisputeNonce :: Channel -> String
showDisputeNonce ch = case chDisputeState ch of
  Nothing -> "none"
  Just ss -> show (csNonce (ssState ss))
