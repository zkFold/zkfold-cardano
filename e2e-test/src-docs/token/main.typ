#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end zkFold Token test*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC example:* Alice wants to send ada to Bob, and some tokens which confirm that a proof of the scheme exists.

- The first transaction is a setup that needs to be performed only once for this application;
- In the second transaction, Alice minting tokens and sends ada + tokens to Bob;
- In the third transaction, Bob burning tokens.

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
        ada: 999
      ),
    ),
    (
      name: "Token verifier script reference",
      address: "Script hash",
      value: (
        ada: 1
      )
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Token verifier script on-chain.]
)
#v(10em)

#transaction(
  [*Minting transaction*],
  inputs: (
    (
      name: "Alice",
      address: "Public key hash",
      value: (
        ada: 567
      )
    ),
    (
      name: "Token verifier script reference",
      reference: true,
      address: "Script hash",
      value: (
        ada: 1
      )
    )
  ),
  outputs: (
    (
      name: "Alice",
      address: "Public key hash",
      value: (
        ada: 467
      ),
    ),
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 100,
        tokenName: 5,
      )
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice sents ada and plonk tokens to Bob.]
)
#v(4em)

#pagebreak()
#transaction(
  [*Burning transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 100,
        tokenName: 5,
      )
    ),
    (
      name: "Token verifier script reference",
      reference: true,
      address: "Script hash",
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
        ada: 100
      ),
    ),
  ),
  staking: (
    "zkFold Token verifier",
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob burn plonk tokens.]
)