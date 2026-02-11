#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

# Check if a name argument is provided
if [ $# -eq 0 ]; then
  echo "Usage: $0 <key-name>"
  echo "Example: $0 alice"
  exit 1
fi

name=$1

# Create keys directory if it doesn't exist
mkdir -p "$keypath"

# Generate the key pair
cardano-cli address key-gen \
  --signing-key-file "$keypath/$name.skey" \
  --verification-key-file "$keypath/$name.vkey"

echo "Generated key pair for '$name':"
echo "  Signing key:      $keypath/$name.skey"
echo "  Verification key: $keypath/$name.vkey"
