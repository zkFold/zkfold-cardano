# Plonk off-chain

We need a backend to execute plonk transactions off-chain.

This is still a toy backend for transactions, from which we will soon turn into a zk infrastructure.
Separate server parts will be written, for example cardano-submit-api.
There is now a repository for the prover server [zkfold-prover](https://github.com/zkFold/zkfold-prover).

1) We need to create a setup, a script, put the setup in the script parameters and send it to the blockchain to an address from which it will not be taken.
2) We need to create a hash of the script (fix it as cs for the mint script) with the setup and put it in the datum of the transfer script with the reward and send it to the blockchain to the address as a blocked reward.
3) We need to create a proof in order to create a token from the script with the setup and the proof, then send it to the account of the person who will receive the reward from the translation script.
4) We need to take the translation script, burn the token and receive the reward.