#! /bin/bash

set -e
set -u
set -o pipefail

configpath=./config.json
keypath=./keys
messageFile=./assets/message-user-outgoing.hex

cabal_run() {
  cabal ${CABAL_FLAGS:-} -v0 run "$@"
}

if [ ! -f "$messageFile" ]; then
  echo "Missing $messageFile. Run ./03-user-outgoing.sh first to post the user message."
  exit 1
fi

message=$(cat "$messageFile")
trustedAddress="${message:80:80}"

echo "Message: $message"
echo "Submitting approved outgoing message..."

cabal_run zkfold-cli:asterizm -- client send \
  --core-config-file $configpath \
  --signing-key-file $keypath/client.skey \
  --client-vkey-file $keypath/client.vkey \
  --trusted-address "$trustedAddress" \
  --beneficiary-address $(cat $keypath/client.addr) \
  --message "$message"
