#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the PlonkVerifierToken*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC scenario:* Alice issues a token that represents a cryptographic proof of some statement (KYC info) about Bob and sends the token to him. The minting policy of the token is the Plonk `verify` algorithm for that statement. Bob can then burn the token in exchange for a reward in ada.

First, we perform a setup transaction that posts the PlonkVerifierToken script on-chain.

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
        "script": "<PlonkVerifierToken>"
      )
    )
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the PlonkVerifierToken script on-chain.]
)
#v(5em)

#set text(size: 12pt)
In the second transaction, Charles sets up a reward for doing the ZK-KYC process. He sends 100 ada to a Plutus script address. The script unlocks the funds if and only if the KYC token is burned.

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
      name: "Burns a ZK-KYC token?",
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
  notes: [Charles sets up a reward for burning a ZK-KYC token.]
)
#v(5em)

#pagebreak()
#set text(size: 12pt)
Alice can now mint a token that represents a cryptographic proof of some statement about Bob. The minting policy of the token is the Plonk `verify` algorithm for that statement. In the same transaction, she sends the token to Bob.

#set text(size: small-size)
#transaction(
  [*ZK-KYC transaction*],
  inputs: (
    (
      name: "Alice",
      address: "Public key",
      value: (
        ada: 567
      )
    ),
    (
      name: "Always fails",
      address: "Plutus script",
      reference: true,
      value: (
        ada: 1
      ),
      datum: (
        "script": "<PlonkVerifierToken>"
      )
    )
  ),
  outputs: (
    (
      name: "Alice",
      address: "Public key",
      value: (
        ada: 566
      ),
    ),
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 1,
        "Bob's ZK-KYC": 1,
      )
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice mints a ZK-KYC token and sends it to Bob.]
)
#v(5em)

#set text(size: 12pt)
Bob can now burn the token and claim the reward.

#set text(size: small-size)
#transaction(
  [*Burning transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 100
      )
    ),
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 1,
        "Bob's ZK-KYC": 1,
      )
    ),
    (
      name: "Burns a ZK-KYC token?",
      address: "Plutus script",
      value: (
        ada: 100
      )
    ),
    (
      name: "Always fails",
      address: "Plutus script",
      reference: true,
      value: (
        ada: 1
      ),
      datum: (
        "script": "<PlonkVerifierToken>"
      )
    )
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key",
      value: (
        ada: 201,
      ),
    ),
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob burns a ZK-KYC token and claims the reward.]
)