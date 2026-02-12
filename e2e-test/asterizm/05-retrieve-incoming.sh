#! /bin/bash

set -e
set -u
set -o pipefail

export CORE_CONFIG_PATH=./assets/config.json

cabal run zkfold-cli:asterizm -- retrieve-messages --incoming
