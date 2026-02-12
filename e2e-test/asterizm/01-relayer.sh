#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./assets/config.json
keypath=./keys

# Build an Asterizm message with 112-byte header + payload
# Header structure (112 bytes total):
#   srcChainId  (8 bytes):  0x01
#   srcAddress  (32 bytes): 0x39d2ba91296029aFBE725436B4824cA803e27391
#   dstChainId  (8 bytes):  0x38
#   dstAddress  (32 bytes): 0x39d2ba91296029aFBE725436B4824cA803e27391
#   txId        (32 bytes): 0x01
# Payload: "Hello, Asterizm!" in hex

srcChainId="0000000000000001"
srcAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
dstChainId="0000000000000038"
dstAddress="00000000000000000000000039d2ba91296029afbe725436b4824ca803e27391"
txId="0000000000000000000000000000000000000000000000000000000000000001"
payload=$(echo -n "Hello, Asterizm!" | xxd -p | tr -d '\n')

message="${srcChainId}${srcAddress}${dstChainId}${dstAddress}${txId}${payload}"

# Compute hash for relayer
messageHash=$(cabal run zkfold-cli:asterizm -- hash --message "$message" | tr -d '"')

echo "Message: $message"
echo "Message hash: $messageHash"

# Save message for use by client-incoming script
echo "$message" > ./assets/message-incoming.hex

cabal run zkfold-cli:asterizm -- relayer \
  --core-config-file $configpath \
  --signing-key-file $keypath/relayer.skey \
  --relayer-vkey-file $keypath/relayer.vkey \
  --beneficiary-address $(cat $keypath/relayer.addr) \
  --message-hash "$messageHash"
