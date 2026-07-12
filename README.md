# State Channel Simulator

[![CI](https://github.com/ai-pavel/pact/actions/workflows/ci.yml/badge.svg)](https://github.com/ai-pavel/pact/actions/workflows/ci.yml)
[![codecov](https://codecov.io/gh/ai-pavel/pact/branch/main/graph/badge.svg)](https://codecov.io/gh/ai-pavel/pact)

A Haskell simulation of a blockchain payment channel (state channel) with
off-chain state transitions, Ed25519 cryptographic signatures, dispute
resolution, and challenge periods.

## Overview

This project models a two-party payment channel where:

- An **on-chain contract** tracks channel state as an algebraic data type with
  states: `Open`, `Active`, `Disputed`, `Closed`.
- **Off-chain state transitions** allow two parties to sign incremental balance
  updates using Ed25519 (via the `crypton` library).
- The channel supports: opening with deposits, sending off-chain payments,
  cooperative close, unilateral close with a challenge period, and dispute
  resolution using the latest signed state.

## Project Structure

```
src/
  Channel/
    Types.hs    -- Core algebraic data types for channel states and balances
    Crypto.hs   -- Ed25519 key generation, signing, and verification
    State.hs    -- Off-chain state transitions and channel operations
    Dispute.hs  -- Dispute resolution and challenge period logic
app/
  Main.hs       -- CLI that walks through a sample channel lifecycle
test/
  Spec.hs       -- Hspec tests for happy path, disputes, and timeouts
```

## Building

```bash
stack build
```

## Running

```bash
stack exec state-channel-sim
```

## Testing

```bash
stack test
```

## License

MIT
