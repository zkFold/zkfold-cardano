# zkFold Symbolic demo project

ZKFold Symbolic is a high-level programming language for zero-knowledge smart contracts, a subset of Haskell. It targets arithmetic circuits, the language used in various zero-knowledge protocols.

The language is a part of `zkfold-base` package. You can compile functions and expressions using the `compile` function from `ZkFold.Symbolic.Compiler` module.

## Examples

You can run the examples using the following command:

```bash
cabal run -- zkfold-symbolic-demo-examples
```

The resulting compiled scripts in the JSON format can be found in the `compiled_scripts` folder.

## Creating your own script

You can edit the `ScriptTemplate.hs` file to create your own script. Then use the following command to compile the script:
```bash
cabal run -- zkfold-symbolic-demo
```