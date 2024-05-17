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
        ada: 950
      ),
    ),
    (
      name: "\"Above 18?\" forwarding script reference",
      address: "\"Always False\" script",
      value: (
        ada: 25
      ),
    ),
    (
      name: "Symbolic verifier script reference",
      address: "\"Always False\" script",
      value: (
        ada: 25
      ),
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
      name: "\"Above 18?\"",
      address: "\"Above 18?\" forwarding script",
      value: (
        ada: 100
      ),
      datum: (
        "address": "<Bob's wallet address>"
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
      name: "\"Above 18?\"",
      address: "Symbolic forwarding script",
      value: (
        ada: 100
      ),
      datum: (
        "address": "<Bob's wallet address>"
      )
    ),
    (
      name: "Symbolic forwarding script reference",
      reference: true,
      address: "\"Always False\" script",
      value: (
        ada: 25
      )
    ),
    (
      name: "Symbolic verifier script reference",
      reference: true,
      address: "\"Always False\" script",
      value: (
        ada: 25
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