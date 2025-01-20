#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the PlonkVerifierTx*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC scenario:* Charles sets up a reward in ada that can be claimed by proving (using the Plonk `verify` algorithm) the completion of a KYC process.

First, we perform a setup transaction that posts the PlonkVerifierTx script on-chain.

#set text(size: small-size)
#v(3em)
#transaction(
  [*Script setup transaction*],
  inputs: (
    (
      name: "Someone",
      address: "Public key",
      value: (
        ada: 1000
      )
    ),
  ),
  outputs: (
    (
      name: "Someone",
      address: "Public key",
      value: (
        ada: 999
      ),
    ),
    (
      name: "Always fails",
      address: "Plutus script",
      value: (
        ada: 1
      ),
      datum: (
        "script": "<PlonkVerifierTx>"
      )
    )
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the PlonkVerifierTx script on-chain.]
)
#v(5em)

#set text(size: 12pt)
In the second transaction, Charles sets up a reward for doing the ZK-KYC process. He sends 100 ada to a Plutus script address. The script unlocks the funds if and only if the respective PlonkVerifierTx is executed successfully.

#set text(size: small-size)
#transaction(
  [*Reward setup transaction*],
  inputs: (
    (
      name: "Charles",
      address: "Public key",
      value: (
        ada: 567
      )
    ),
  ),
  outputs: (
    (
      name: "Forward to PlonkVerifierTx",
      address: "Plutus script",
      value: (
        ada: 100
      )
    ),
    (
      name: "Charles",
      address: "Public key",
      value: (
        ada: 467
      ),
    ),
  ),
  signatures: (
    "Charles",
  ),
  notes: [Charles sets up a reward for proving completion of a ZK-KYC process.]
)
#v(5em)

#pagebreak()
#set text(size: 12pt)
Bob proves KYC process completion and claims the reward.

#set text(size: small-size)
#transaction(
  [*Withdraw transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 123
      )
    ),
    (
      name: "Forward to PlonkVerifierTx",
      address: "Plutus script",
      value: (
        ada: 100
      )
    ),
    (
      name: "Always fails",
      reference: true,
      address: "Plutus script",
      value: (
        ada: 1
      ),
      datum: (
        "script": "<PlonkVerifierTx>"
      )
    )
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 223
      ),
    ),
  ),
  staking: (
    "PlonkVerifierTx for ZK-KYC",
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob proves completion of the ZK-KYC process and claims the reward.]
)