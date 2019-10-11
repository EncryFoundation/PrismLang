# PrismLang
[![Build Status](https://api.travis-ci.org/EncryFoundation/PrismLang.svg?branch=master)](https://travis-ci.org/EncryFoundation/PrismLang)

PrismLang is a Contract Oriented statically typed programming language developed for the [EncryCore](https://github.com/EncryFoundation/EncryCore) blockchain protocol.

## Language Specification

### Supported Data Types

Primitive Data Types
- `Any` - a supertype of any other type
- `Unit` - a type with a single value `()`
- `Byte` - 8 bit signed integer
- `Int` - 64 bit signed integer
- `Bool` - a type with two logical values `true` and `false`
- `String` - arbitrary sequence of chars

Containers
- `Array[T]` - arrays of arbitrary length with all values of type `T`
- `Tuple` - tuple of possibly different data types supported in PrismLang

Built-in Complex Types
- `Signature25519` - is Edwards-curve Digital Signature Algorithm (EdDSA) over Curve25519
- `Transaction` - EncryTransaction
- `State` - Information about current height of the blockchain, timestamp of the last published block
- `Box` - EncryBox
- `AssetBox` - EncryAssetBox
- `DataBox` - EncryDataBox

Build-in functions
- `unixTime(time: String)` - convert String to Timestamp using yyyy-MM-dd hh:mm:ss format
- `checkSig(signature: Signature25519, message: Array[Byte], key: Array[Byte])` - check message was signed with Key

Build-in base functions
- `encode16(input: Array[Byte]): String` - represents bytes using a base of 16 symbols
- `encode58(input: Array[Byte]): String` - represents bytes using a base of 58 symbols
- `base16'String': Array[Byte]` - decode string to bytes using a base of 16 symbols
- `base58'String': Array[Byte]` - decode string to bytes using a base of 58 symbols

Build-in cryptographic hash functions
- `blake2b256(input: Array[Byte]): Array[Byte]`
- `blake2b512(input: Array[Byte]): Array[Byte]`
- `keccak256(input: Array[Byte]): Array[Byte]`
- `keccak512(input: Array[Byte]): Array[Byte]`
- `sha256(input: Array[Byte]): Array[Byte]`

### Basic Syntax

    // Constant definition
    let a: Int = 10                       // Explicit type annotation
    let b = 100                           // Type will be inferred automatically
    let c = if (a > b) true else false    // Conditional assignment

    // Function definition
    def sum(a: Int, b: Int): Int = {
        a + b
    }

    // Lambda definition
    lamb (a: Int, b: Int) = a + b

    // If statement
    let flag: Bool = if (10 < 100) {
        true
    } else {
        false
    }

    // Type matching
    let validProof: Bool = if (let sig: Signature25519 = poof) checkSig(sig, msg, pk) else false

    // Base58 string
    let pubKeyBytes: Array[Byte] = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'

    // Byte
    let byte: Byte = (127).toByte

    // Collections
    let ageList: Array[Int] = List(1, 2, 3, 4)
    let ageDict: Dict[String, Int] = Dict('Alice' -> 4, 'Bob' -> 9, 'Tom' -> 17) [Scheduled for the next release]

    // Collection subscription
    let someonesAge = ageList[0]            // Will result in `1`

    // Lambda application
    let doesExist: Bool = ageList.exists(lamb (i: Int) = i > 3)             // true
    let ageListDoubled: Array[Int] = ageList.map(lamb (i: Int) = i * 2)     // Array(2, 4, 6, 8)

### Contract Example

    contract (signature: Signature25519) {
        let ownerPubKey = base58"GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew"
        checkSig(ctx.transaction.msg, ownerPubKey, signature)
    }
#### Even more [Contract Examples](https://github.com/EncryFoundation/PrismLang/tree/release/docs/examples)

## About PrismLang

PrismLang has the following design:
* Compiled
* Strong typed
* Static type-checking

PrismLang was developed in such way for a reason. For example in Ethereum, which is one of the biggest blockchain platforms nowadays, miners use gas as execution fee for every operation made on Ethereum to get paid exactly for the computational resources they spend on this operation. This sometimes leads to OutOfGasError: when the user didn't attach enough gas to execute his transaction, gas is paid to the miner, because he was spending computational power, but the transaction will not be sent to the blockchain.

PrismLang Structure is able to overcome this issue by statically analyzing the source code structure we can precisely estimate the computational cost of the operations. PrismLang does not support looping or recursion primitives for the end user, that leads to the fact, then in the compile time, we already know exactly how many operations we need to perform. Taking the previous statement into consideration, we can conclude that PrismLang can provide both end user and miner with precise Cost of the smart contract.

Furthermore, EncryCore protocol uses an Unspent Transaction Output(UTXO) model for the record keeping and smart contracts are used to lock the transaction output. Therefore, the main features kept in mind for PrismLang development were :
1) The simplicity of usage.
2) Clearness of the abstractions.
3) Acceptable entry threshold.
4) Ability to precisely estimate Cost of the operations in a convenient way for both end user and miner.

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).

