#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end zkFold Symbolic test*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC example:* Alice wants to send ada to Bob, which he can then use on the condition that he is above 18 years old.

- The first transaction is a setup that needs to be performed only once for this application;
- In the second transaction, Alice sends ada to a smart contract;
- In the third transaction, Bob proves that he is above 18 year old and withdraws the funds to his wallet address.

#set text(size: small-size)
#v(3em)
#transaction(
  [*Init transaction*],
  inputs: (
    (
      name: "Someone",
      address: "Public key hash",
      value: (
        ada: 1000
      )
    ),
  ),
  outputs: (
    (
      name: "Someone",
      address: "Public key hash",
      value: (
        ada: 998
      ),
    ),
    (
      name: "\"Above 18?\" setup datum",
      address: "zkfold-setup",
      value: (
        ada: 1
      ),
      datum: (
        "setup": "Above 18?"
      )
    ),
    (
      name: "Symbolic verifier script reference",
      address: "symbolicVerifier.plutus",
      value: (
        ada: 1
      ),
      datum: (
        "input": "zero hash"
      )
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Symbolic verifier script and the "Above 18?" forwarding script on-chain.]
)
#v(10em)

#transaction(
  [*Transfer transaction*],
  inputs: (
    (
      name: "Alice",
      address: "Public key hash",
      value: (
        ada: 567
      )
    ),
  ),
  outputs: (
    (
      name: "\"Above 18?\" Input",
      address: "symbolicVerifier.plutus",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address> + <Bob address>"
      )
    ),
    (
      name: "Alice",
      address: "Public key hash",
      value: (
        ada: 467
      ),
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice sents ada to a smart contract address.]
)
#v(4em)

#pagebreak()
#transaction(
  [*Smart contract transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 123
      )
    ),
    (
      name: "\"Above 18?\" Input",
      address: "Symbolic forwarding script",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address> + <Bob address>"
      )
    ),
    (
      name: "\"Above 18?\" setup datum",
      reference: true,
      address: "zkfold-setup",
      value: (
        ada: 1
      ),
      datum: (
        "setup": "Above 18?"
      )
    ),
    (
      name: "Symbolic verifier script reference",
      reference: true,
      address: "symbolicVerifier.plutus",
      value: (
        ada: 1
      )
    )
  ),
  outputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 223
      ),
    ),
  ),
  staking: (
    "zkFold Symbolic verifier",
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob must prove that he is above 18 to withdraw ada sent by Alice.]
)