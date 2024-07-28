#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the Symbolic verifier*

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
      name: "Symbolic: setup Above 18 + <Bob address>",
      address: "stake pool",
      value: (
        ada: 1
      )
    ),
    (
      name: "Forwarding reward",
      address: "zkfold-main",
      value: (
        ada: 1
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
      name: "Scripthash to Symbolic \"Above 18?\"",
      address: "Forwarding reward script",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address>"
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
  [*Withdraw transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 123
      )
    ),
    (
      name: "Scripthash to Symbolic \"Above 18?\"",
      address: "Forwarding reward script",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address>"
      )
    ),
    (
      name: "Symbolic: setup Above 18 + <Bob address>",
      reference: true,
      address: "stake pool",
      value: (
        ada: 1
      )
    ),
    (
      name: "Forwarding reward",
      reference: true,
      address: "zkfold-main",
      value: (
        ada: 1
      )
    ),
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