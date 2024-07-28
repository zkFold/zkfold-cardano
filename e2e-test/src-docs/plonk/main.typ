#import "@preview/unequivocal-ams:0.1.0": ams-article, theorem, proof, script-size, small-size, normal-size

#import "transaction.typ": *

#set align(center)
#set text(size: 18pt)
*End-to-end test for the Plonk verifier*

#set align(start)
#set text(size: 12pt)

*A ZK-KYC example:* Alice wants to mint and send Bob a token that represents a proof of some statement about Bob.

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
        ada: 998
      ),
    ),
    (
      name: "Plonk: setup Above 18",
      address: "zkfold-main",
      value: (
        ada: 1
      )
    ),
    (
      name: "Forwarding minting",
      address: "zkfold-main",
      value: (
        ada: 1
      )
    ),
  ),
  signatures: (
    "Someone",
  ),
  notes: [Someone posts the Plonk verifier script on-chain.]
)
#v(10em)

#transaction(
  [*Transfer transaction*],
  inputs: (
    (
      name: "Charles",
      address: "Public key hash",
      value: (
        ada: 567
      )
    ),
  ),
  outputs: (
    (
      name: "Symbolhash to Plonk \"Above 18?\"",
      address: "Forwarding mint script",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address>"
      )
    ),
    (
      name: "Charles",
      address: "Public key hash",
      value: (
        ada: 467
      ),
    ),
  ),
  signatures: (
    "Charles",
  ),
  notes: [Charles sents ada to a smart contract address.]
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
      name: "Plonk: setup Above 18",
      reference: true,
      address: "zkfold-main",
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
        ada: 566
      ),
    ),
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 1,
        tokenName: 1,
      )
    ),
  ),
  signatures: (
    "Alice",
  ),
  notes: [Alice sents ada and plonk tokens to Bob.]
)

#pagebreak()
#transaction(
  [*Burning transaction*],
  inputs: (
    (
      name: "Bob",
      address: "Public key hash",
      value: (
        ada: 100,
        tokenName: 1,
      )
    ),
    (
      name: "Symbolhash to Plonk \"Above 18?\"",
      address: "Forwarding mint script",
      value: (
        ada: 100
      ),
      datum: (
        "input": "<setup address>"
      )
    ),
    (
      name: "Plonk: setup Above 18",
      address: "zkfold-main",
      reference: true,
      value: (
        ada: 1
      )
    ),
    (
      name: "Forwarding minting",
      address: "zkfold-main",
      reference: true,
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
        ada: 200,
      ),
    ),
  ),
  signatures: (
    "Bob",
  ),
  notes: [Bob burn plonk tokens.]
)