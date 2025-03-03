{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# OPTIONS_GHC -fno-ignore-interface-pragmas #-}
module Starter where
-- TRIM TO HERE
-- This is a starter contract, based on the Game contract,
-- containing the bare minimum required scaffolding.
--
-- What you should change to something more suitable for
-- your use case:
--   * The MyDatum type
--   * The MyRedeemer type
--
-- And add function implementations (and rename them to
-- something suitable) for the endpoints:
--   * publish
--   * redeem

import Control.Monad (void)
import Ledger (Address)
import Ledger.Constraints qualified as Constraints
import Ledger.Typed.Scripts qualified as Scripts
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import Plutus.V1.Ledger.Contexts (ScriptContext)
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..))

-- | These are the data script and redeemer types. We are using an integer
--   value for both, but you should define your own types.
newtype MyDatum = MyDatum Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyDatum

newtype MyRedeemer = MyRedeemer Integer deriving newtype (PlutusTx.ToData, PlutusTx.FromData, PlutusTx.UnsafeFromData)
PlutusTx.makeLift ''MyRedeemer

-- | This method is the spending validator (which gets lifted to
--   its on-chain representation).
validateSpend :: MyDatum -> MyRedeemer -> ScriptContext -> Bool
validateSpend _myDataValue _myRedeemerValue _ = error () -- Please provide an implementation.

-- | The address of the contract (the hash of its validator script).
contractAddress :: Address
contractAddress = Scripts.validatorAddress starterInstance

data Starter
instance Scripts.ValidatorTypes Starter where
    type instance RedeemerType Starter = MyRedeemer
    type instance DatumType Starter = MyDatum

-- | The script instance is the compiled validator (ready to go onto the chain)
starterInstance :: Scripts.TypedValidator Starter
starterInstance = Scripts.mkTypedValidator @Starter
    $$(PlutusTx.compile [|| validateSpend ||])
    $$(PlutusTx.compile [|| wrap ||]) where
        wrap = Scripts.mkUntypedValidator @MyDatum @MyRedeemer

-- | The schema of the contract, with two endpoints.
type Schema =
        Endpoint "publish" (Integer, Value)
        .\/ Endpoint "redeem" Integer

contract :: AsContractError e => Contract () Schema e ()
contract = selectList [publish, redeem]

-- | The "publish" contract endpoint.
publish :: AsContractError e => Promise () Schema e ()
publish = endpoint @"publish" $ \(i, lockedFunds) -> do
    let tx = Constraints.mustPayToTheScript (MyDatum i) lockedFunds
    void $ submitTxConstraints starterInstance tx

-- | The "redeem" contract endpoint.
redeem :: AsContractError e => Promise () Schema e ()
redeem = endpoint @"redeem" $ \myRedeemerValue -> do
    unspentOutputs <- utxosAt contractAddress
    let redeemer = MyRedeemer myRedeemerValue
        tx       = Constraints.collectFromTheScript unspentOutputs redeemer
    void $ submitTxConstraintsSpending starterInstance unspentOutputs tx

endpoints :: AsContractError e => Contract () Schema e ()
endpoints = contract

mkSchemaDefinitions ''Schema

$(mkKnownCurrencies [])
