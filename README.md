# zkFold: Cardano integration library
Integration of zkFold Symbolic smart contracts with the Cardano blockchain. The repository contains
- current zk protocols in `zkfold-cardano`;
- the Plutus scripts in `zkfold-cardano-scripts`;
- off-chain code for constructing and sending transactions in `zkfold-cardano-backends`;
- examples of zk circuits in `zkfold-example`;
- end-to-end tests for smart contracts in `e2e-test`;
- current smart contract documentation in `docs`.

## Building the project

This project can be built with Cabal 3.10.3.0 and GHC 9.6.6.

Crypotgraphic depencencies needed for building Haskell packages:

* [`libsodium`](https://github.com/jedisct1/libsodium)
* [`libsecp256k1`](https://github.com/bitcoin-core/secp256k1)
* [`libblst`](https://github.com/supranational/blst)

Additional steps are required for libblt to register it in pkg-config database. After building the library, execute the following in the same directory:

```bash
cat > libblst.pc << EOF
prefix=/usr/local
exec_prefix=\${prefix}
libdir=\${exec_prefix}/lib
includedir=\${prefix}/include

Name: libblst
Description: Multilingual BLS12-381 signature library
URL: https://github.com/supranational/blst
Version: ${BLST_VERSION#v}
Cflags: -I\${includedir}
Libs: -L\${libdir} -lblst
EOF
sudo cp libblst.pc /usr/local/lib/pkgconfig/
sudo cp bindings/blst_aux.h bindings/blst.h bindings/blst.hpp  /usr/local/include/
sudo cp libblst.a /usr/local/lib
sudo chmod u=rw,go=r /usr/local/{lib/{libblst.a,pkgconfig/libblst.pc},include/{blst.{h,hpp},blst_aux.h}}
```

You might also need Libtool if you are getting errors associated with it:

```
sudo apt-get install libtool
```

Then run

```
$ cabal update
$ cabal build
```

