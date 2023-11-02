# zkFold Symbolic demo project

ZKFold Symbolic is a high-level programming language for zero-knowledge smart contracts, a subset of Haskell. It targets arithmetic circuits, the language used in various zero-knowledge protocols.

The language is a part of the `zkfold-base` package. You can compile functions and expressions using the `compile` function from `ZkFold.Symbolic.Compiler` module.

## Building the project

This project can be built with Cabal 3.6.2.0 and GHC 8.10.7.

## Examples

Check out code examples in the `test/Examples` folder of this repository. You can run the examples using the following command:

```bash
cabal run -- zkfold-symbolic-demo-examples
```

The resulting compiled scripts in the JSON format can be found in the `compiled_scripts` folder.

## Creating your own script

You can edit the `ScriptTemplate.hs` file to create your own script. Then, use the following command to compile the script:
```bash
cabal run -- zkfold-symbolic-demo
```

The compiler can work with any function of the general form
```Haskell
f :: Symbolic a => T1 a -> T2 a
```
where `T1 a` and `T2 a` have instances of the `Arithmetizable a` typeclass. In practice, developers will work with a set of types and type operations for which those instances are provided.

Currently, the only basic fully supported type for input argument is a finite field element `a`. For the function output, we support finite field elements and boolean values `Bool a`. You can construct more complex types using tuples and finite lists.

Let us consider an example:
```Haskell
leq :: Symbolic a => a -> a -> Bool a
```
This function expects two arguments that implement finite field operations (e.g., integers modulo `p`) and returns a boolean type.

Standard types like `Int32 a`, `Int64 a`, `ByteString32 a`, and `ByteString64` are coming soon. The support for arbitrary-length types is also in the works.
