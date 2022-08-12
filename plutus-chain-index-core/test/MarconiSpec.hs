module MarconiSpec where

-- Regarding property testing, here's a starting point.

-- You can test the txScripts function as follows. Generate a random
-- number of plutus scripts (see `genPlutusScript` from
-- Gen.Cardano.Api.Typed), then generate an TxIns which spends funds
-- from these scripts, then generate a transaction with this TxIns
-- (based on `genTxBodyContent`), run your txScripts on this generated
-- tx, and verify that the initial generated plutus scripts are part
-- of the function's output.

import Hedgehog
import Hedgehog.Gen qualified as Gen
import Hedgehog.Range qualified as Range

import Test.Tasty (TestTree, testGroup)
-- import Test.Tasty.Hedgehog (testPropertyNamed)
import Cardano.Api
import Control.Monad (replicateM)
import Gen.Cardano.Api.Typed
import Test.Tasty.Hedgehog (testProperty)

import Marconi.Index.ScriptTx ()

tests :: TestTree
tests = testGroup "marconi"
  [ testProperty "prop_script_hashes_in_tx_match" testTxScripts]

testTxScripts :: Property
testTxScripts = property $ do
  -- Generate a random number of plutus scripts (see `genPlutusScript` from Gen.Cardano.Api.Typed)
  nScripts <- forAll $ Gen.integral (Range.linear 0 500)
  scripts :: [PlutusScript lang] <- replicateM nScripts $ forAll $ genPlutusScript undefined
  -- data PlutusScript lang where
  -- ^ /cardano-node/cardano-api/src/Cardano/Api/Script.hs::1061

  -- then generate an TxIns which spends funds from these scripts
  txIns <- undefined
  -- type TxIns build era = [(TxIn, BuildTxWith build (Witness WitCtxTxIn era))]
  -- ^ /cardano-node/cardano-api/src/Cardano/Api/TxBody.hs::1157

  -- then generate a transaction with this TxIns (based on `genTxBodyContent`)
  -- genTxBodyContent
  -- data TxBodyContent build era =
  -- ^ /cardano-node/cardano-api/src/Cardano/Api/TxBody.hs::1518

  -- run your txScripts on this generated tx, and verify that the
  -- initial generated plutus scripts are part of the function's
  -- output

  undefined
