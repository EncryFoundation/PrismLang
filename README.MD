# PrismLang
[![Build Status](https://api.travis-ci.org/EncryFoundation/PrismLang.svg?branch=master)](https://travis-ci.org/EncryFoundation/PrismLang)

PrismLang is a Contract Oriented programming language used in [EncryCore](https://github.com/EncryFoundation/EncryCore) which is:
* Compiled
* Strong static typed
* Non turing-complete

### [Language Specification](https://github.com/EncryFoundation/PrismLang/blob/release/docs/LangSpec.md)
### [Contract Examples](https://github.com/EncryFoundation/PrismLang/tree/release/docs/examples)

## About PrismLang

PrismLang was developed in such way for a reason. For example in Ethereum, which is one of the biggest block chain platforms nowadays, miners use gas as execution fee for every operation made on Ethereum to get payed exactly for the computational resources they spend on this operation. This sometimes leads to OutOfGasError: when user didn't attach enough gas to execute his transaction, gas is payed to miner, because he was spending computational power, but transaction will not be send to block chain.

PrismLang Structure is able to overcome this issue by statically analyzing source code structure we can compute computational cost of the operations. PrismLang does not support looping or recursion primitives for end user, that leeds to the fact, than in the compile time we already know exactly how many operations we need to perform. Taking previous statement in consideration, we can conclude that PrismLang can provide both end user and miner with precise Cost of the smart contract.



