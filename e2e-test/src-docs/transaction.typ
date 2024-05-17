#let tx_out_height_estimate(input) = {
  let address = if "address" in input { 1 } else { 0 }
  let value = if "value" in input { input.value.len() } else { 0 }
  let datum = if "datum" in input { input.datum.len() } else { 0 }
  return (address + value + datum) * 8pt
}

#let datum_field(indent, k, val) = [
  #if val == "" [
    #h(indent)\+ #raw(k)
  ] else [
    #h(indent)\+ #raw(k):
    #if type(val) == content { val }
    #if type(val) == str and val != "" {repr(val)}
    #if type(val) == int {repr(val)}
    #if type(val) == array [
      #stack(dir: ttb, spacing: 0.4em,
        for item in val [
          #datum_field(indent + 1.2em, "", item) \
        ]
      )
    ]
    #if type(val) == dictionary [
      #v(-0.7em)
      #stack(dir: ttb, spacing: 0em,
        for (k, v) in val.pairs() [
          #datum_field(indent + 1.2em, k, v) \
        ]
      )
    ]
  ]
]

#let tx_out(input, position, inputHeight, styles) = {
  let address = if "address" in input [
    *Address: #h(0.5em) #input.address*
  ] else []
  let value = if "value" in input [
    *Value:* #if ("ada" in input.value) [ *#input.value.ada* ADA ] \
    #v(-1.0em)
    #stack(dir: ttb, spacing: 0.4em,
      ..input.value.pairs().map(((k, v)) => [
        #if k != "ada" {
          [#h(2.3em) \+ *#v* #raw(k)]
        }
      ])
    )
  ] else []
  let datum = if "datum" in input [
    *Datum:* \ 
    #v(-0.8em)
    #stack(dir: ttb, spacing: 0.4em,
      ..input.datum.pairs().map(((k,val)) => datum_field(1.2em, k, val))
    )
  ] else []
  let addressHeight = measure(address, styles).height + if "address" in input { 6pt } else { 0pt }
  let valueHeight = measure(value, styles).height + if "value" in input { 6pt } else { 0pt }
  let datumHeight = measure(datum, styles).height + if "datum" in input { 6pt } else { 0pt }
  let thisHeight = 32pt + addressHeight + valueHeight + datumHeight
  return (
    content: place(dx: position.x, dy: position.y, [
      #v(-0.4em)
      *#input.name*
      #v(0.4em)
      #line(start: (-4em, -1em), end: (10em, -1em))
      #place(dx: 10em, dy: -1.5em)[#circle(radius: 0.5em, fill: white, stroke: black)]
      #if "address" in input { place(dx: 0em, dy: -3pt)[#address] }
      #place(dx: 0em, dy: addressHeight)[#value]
      #if "datum" in input { place(dx: 0em, dy: addressHeight + valueHeight)[#datum] }
    ]),
    height: thisHeight,
  )
}

#let collapse_values(existing, v, one) = {
  if type(v) == int {
    existing.qty += one * v
  } else {
    let parts = v.matches(regex("([ ]*([+-]?)[ ]*([0-9]*)[ ]*([a-zA-Z]*)[ ]*)"))
    for part in parts {
      let sign = part.captures.at(1)
      let qty = int(if part.captures.at(2) == "" { 1 } else { part.captures.at(2) })
      let var = part.captures.at(3)
      let existing_var = existing.variables.at(var, default: 0)
      if var == "" {
        existing.qty += one * qty
      } else {
        if sign == "-" {
          existing.variables.insert(var, existing_var - one * qty)
        } else {
          existing.variables.insert(var, existing_var + one * qty)
        }
      }
    }
  }
  existing
}

#let transaction(name, inputs: (), outputs: (), staking: (), certificates: (), validRange: none, signatures: (), notes: none) = style(styles => {
  let inputHeightEstimate = inputs.fold(0pt, (sum, input) => sum + tx_out_height_estimate(input))
  let inputHeight = 0em
  let mint = (:)
  let inputs = [
      #let start = (x: -18em, y: 1em)
      #for input in inputs {
        // Track how much is on the inputs
        if not input.at("reference", default: false) {
          if "value" in input {
            for (k, v) in input.value {
              let existing = mint.at(k, default: (qty: 0, variables: (:)))
              let updated = collapse_values(existing, v, -1)
              mint.insert(k, updated)
            }
          }
        }

        let tx_out = tx_out(input, start, inputHeight, styles)

        tx_out.content

        // Now connect this output to the transaction
        place(dx: start.x + 10.5em, dy: start.y + 0.84em)[
          #path(
            stroke: if input.at("reference", default: false) { aqua } else { black },
            ((0em, 0em), (0em, 0em), (8em, 0em)),
            ((7.44em, (inputHeightEstimate / 1.25) - (inputHeight / 1.25)), (-4em, 0em))
          )
        ]
        place(dx: start.x + 10.26em, dy: start.y + 0.59em)[#circle(radius: 0.25em, fill: black)]
        if input.at("redeemer", default: none) != none {
          place(dx: start.x + 12.26em, dy: start.y - 0.2em)[#input.at("redeemer")]
        }

        start = (x: start.x, y: start.y + tx_out.height)
        inputHeight += tx_out.height
      }
    ]
  
  let outputHeightEstimate = outputs.fold(0pt, (sum, output) => sum + tx_out_height_estimate(output))
  let outputHeight = 0em
  let outputs = [
      #let start = (x: 4em, y: 1em)
      #for output in outputs {
        // Anything that leaves on the outputs isn't minted/burned!
        if "value" in output {
          for (k, v) in output.value {
            let existing = mint.at(k, default: (qty: 0, variables: (:)))
            let updated = collapse_values(existing, v, 1)
            mint.insert(k, updated)
          }
        }

        let tx_out = tx_out(output, start, outputHeight, styles)
        tx_out.content
        start = (x: start.x, y: start.y + tx_out.height)
        outputHeight += tx_out.height
      }
    ]

  // Collapse down the `mint` array
  let display_mint = (:)
  for (k, v) in mint {
    let has_variables = v.variables.len() > 0 and v.variables.values().any(v => v != 0)
    if v.qty == 0 and not has_variables {
      continue
    }
    let display = []
    if v.qty != 0 {
      display = if v.qty > 0 { [\+] } + [#v.qty]
    }
    let vs = v.variables.pairs().sorted(key: ((k,v)) => -v)
    if vs.len() > 0 {
      for (k, v) in vs {
        if v == 0 {
          continue
        } else if v > 0 {
          display += [ \+ ]
        } else if v < 0 {
          display += [ \- ]
        }
        if v > 1 or v < -1 {
          display += [#calc.abs(v)]
        }
        display += [*#k*]
      }
    }
    display += [ *#raw(k)*]
    display_mint.insert(k, display)
  }

  let mints = if display_mint.len() > 0 [
    *Mint:* \
    #for (k, v) in display_mint [
      #v \
    ]
  ] else []
  let sscripts = if staking.len() > 0 [
    *Staking scripts:*
    #for sscript in staking [
      - #sscript
    ]
  ] else []
  let certs = if certificates.len() > 0 [
    *Certificates:*
    #for certificate in certificates [
      - #certificate
    ]
  ] else []
  let valid_range = if validRange != none [
    *Valid Range:* \
    #if "lower" in validRange [#validRange.lower $<=$ ]
    `slot`
    #if "upper" in validRange [$<=$ #validRange.upper]
  ] else []
  let sigs = if signatures.len() > 0 [
    *Signatures:* \
    #for signature in signatures [
      - #signature
    ]
  ] else []

  let boxHeight = 100pt + if certificates.len() > 0 { 32pt * certificates.len() } else { 0pt } + if signatures.len() > 0 { 32pt * signatures.len() } else { 0pt } 

  let transaction = [
      #set align(center)
      #rect(
        radius: 4pt,
        width: 30%,
        height: calc.max(boxHeight, inputHeight + 16pt, outputHeight + 16pt),
        [
          #pad(top: 1em, name)
          #v(1em)
          #set align(left)
          #stack(dir: ttb, spacing: 1em,
            mints,
            sscripts,
            certs,
            valid_range,
            sigs
          )
        ]
      )
    ]

  let diagram = stack(dir: ltr,
    inputs,
    transaction,    
    outputs
  )
  let size = measure(diagram, styles)
  block(width: 100%, height: size.height)[
    #set align(center)
    #diagram
    #if notes != none [ *Note*: #notes ]
  ]
})