#! /bin/bash

set -e
set -u
set -o pipefail

keypath=./keys

cabal run zkfold-cli:asterizm -- retrieve-messages
