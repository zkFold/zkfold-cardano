#! /bin/bash

set -e
# Unofficial bash strict mode.
# See: http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -u
set -o pipefail

keypath=./keys
assets=../assets

echo ""
echo "owner pool delegate ada."
echo ""

in=$(cardano-cli query utxo --address $(cat $keypath/owner.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[0]')
collateral=$(cardano-cli query utxo --address $(cat $keypath/owner.addr) --testnet-magic 4 --out-file  /dev/stdout | jq -r 'keys[1]')

echo "owner address:"
echo "$(cardano-cli query utxo --address $(cat $keypath/owner.addr) --testnet-magic 4)"
echo ""

#------------------ :script staking registration certificate: ------------------

# We have a fully functioning stake pool at this point. We now want to test Plutus staking script withdrawals.

# We now create the Plutus script staking registration certificate

cardano-cli conway stake-address registration-certificate \
  --testnet-magic 4 \
  --stake-script-file "$assets/plonkupVerifierTx.plutus" \
  --out-file "$keypath/plonkupVerifierTx.regcert"

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --certificate-file "$keypath/plonkupVerifierTx.regcert" \
    --out-file "$keypath/register-plutus-staking-script.txbody"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/register-plutus-staking-script.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/register-plutus-staking-script.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/register-plutus-staking-script.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "register transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/delegate-staking-script.tx")"
echo ""

# H.note_ "Check if Plutus staking script address was registered"
# 
# void $ H.execCli' execConfig
#     [ "query", "stake-address-info"
#     , "--address", plutusStakingAddr
#     , "--testnet-magic", show @Int testnetMagic
#     , "--out-file", work </> "pledge-stake-address-info.json"
#     ]
# 
# plutusStakeAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "pledge-stake-address-info.json"
# delegsAndRewardsMapPlutus <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards plutusStakeAddrInfoJSON
# let delegsAndRewardsPlutus = mergeDelegsAndRewards delegsAndRewardsMapPlutus
#     plutusStakeAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsPlutus
#     (plutusSAddr, _rewards, _poolId) = head plutusStakeAddrInfo
# 
# H.note_ "Check if Plutus staking script has been registered"
# T.unpack (serialiseAddress plutusSAddr) === plutusStakingAddr

#------------------- :script staking delegation certificate: -------------------

#Create delegation certificate for Plutus staking script to stake pool

cardano-cli conway stake-address registration-certificate \
  --testnet-magic 4 \
  --stake-script-file "$assets/plonkupVerifierTx.plutus" \
  --cold-verification-key-file "$keypath/cold.vkey" \
  --out-file "$keypath/plutus-script.delegcert"


#Delegate Plutus staking script to stake pool

cardano-cli conway query protocol-parameters \
   --testnet-magic 4 \
   --out-file "$keypath/pparams.json"

plutusStakingScriptRedeemer=$assets/unit.json

cardano-cli conway transaction build \
    --testnet-magic 4 \
    --change-address "$(cat $keypath/someone.addr)" \
    --tx-in $in \
    --tx-in-collateral $collateral \
    --tx-out "$(cat $keypath/plonkupVerifierTx.addr) + 10000000 lovelace" \
    --certificate-file "$keypath/plutus-script.delegcert" \
    --certificate-script-file "$assets/plonkupVerifierTx.plutus" \
    --certificate-redeemer-file $plutusStakingScriptRedeemer \
    --protocol-params-file "$keypath/pparams.json" \
    --out-file "$keypath/delegate-staking-script.txbody"

cardano-cli conway transaction sign \
    --testnet-magic 4 \
    --tx-body-file "$keypath/delegate-staking-script.txbody" \
    --signing-key-file "$keypath/bob.skey" \
    --out-file "$keypath/delegate-staking-script.tx"

cardano-cli conway transaction submit \
    --testnet-magic 4 \
    --tx-file "$keypath/delegate-staking-script.tx"

#-------------------------------------------------------------------------------

echo ""
echo "Pausing for 60 seconds..."
echo ""
sleep 60

echo ""
echo "delegate transaction id: $(cardano-cli conway transaction txid --tx-file "$keypath/delegate-staking-script.tx")"
echo ""

# H.note_ "Check to see if staking script was delegated"
# 
# void $ H.execCli' execConfig
#   [ "query",  "stake-address-info"
#   , "--address", plutusStakingAddr
#   , "--testnet-magic", show @Int testnetMagic
#   , "--out-file", work </> "plutus-staking-script-delegation.json"
#   ]
# 
# stakingScriptAddrInfoJSON <- H.leftFailM . H.readJsonFile $ work </> "plutus-staking-script-delegation.json"
# delegsAndRewardsMapStakingScript <- H.noteShowM $ H.jsonErrorFail $ J.fromJSON @DelegationsAndRewards stakingScriptAddrInfoJSON
# let delegsAndRewardsStakingScript = mergeDelegsAndRewards delegsAndRewardsMapStakingScript
#     stakingScriptAddrInfo = filter (\(sAddr,_,_) -> plutusStakingAddr == T.unpack (serialiseAddress sAddr)) delegsAndRewardsStakingScript
#     (_stakingSAddr, _rewards, poolIdPlutusDeleg) = head stakingScriptAddrInfo
# 
# H.note_ $ "Check plutus staking script: " <> (work </> "plutus-staking-script-delegation.json") <> " was delegated"
# case poolIdPlutusDeleg of
#   Nothing -> H.failMessage callStack "Plutus script was not delegated to stake pool"
#   Just plutusDelegPoolId ->
#     T.unpack (serialiseToBech32 plutusDelegPoolId) === stakePoolId
