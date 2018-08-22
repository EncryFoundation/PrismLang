# PrismLang
[![Build Status](https://api.travis-ci.org/EncryFoundation/PrismLang.svg?branch=master)](https://travis-ci.org/EncryFoundation/PrismLang)

PrismLang is a Contract Oriented strong typed programming language with static type-checking developed for the [EncryCore](https://github.com/EncryFoundation/EncryCore).

## Language Specification

### Supported Data Types
// Primitive Data Types
- `Any` - a supertype of any other type
- `Unit` - a type with a single value `()`
- `Byte` - 8 bit signed integer
- `Int` - 64 bit signed integer
- `Bool` - a type with two logical values `true` and `false`
- `String` - arbitrary sequence of chars

// Collections
- `Array[T]` - arrays of arbitrary length with all values of type `T`
- `Tuple` - tuple of possibly different data types supported in PrismLang

//Built-in Complex Types
- `Signature25519` - is Edwards-curve Digital Signature Algorithm (EdDSA) over Curve25519
- `Transaction` - EncryTransaction
- `Box` - EncryBox
- `AssetBox` - EncryAssetBox
- `DataBox` - EncryDataBox

//Build-in functions
- `Str2Time(time: String)` - Convert String to Timestamp
- `checkSig(signature: Signature25519, message: Array[Byte], key: Array[Byte])` - Check message was signed with Key

// Build-in  cryptographic hash functions
- `Blake2b256Hash`
- `Blake2b512Hash`
- `Keccak256Hash`
- `Keccak512Hash`
- `Sha256Hash`



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
    let ageDict: Dict[String, Int] = Dict('Alice' -> 4, 'Bob' -> 9, 'Tom' -> 17) [Under discussion]

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

PrismLang have following design:
* Compiled
* Strong typed
* with static type-checking
* Non turing-complete

PrismLang was developed in such way for a reason. For example in Ethereum, which is one of the biggest block chain platforms nowadays, miners use gas as execution fee for every operation made on Ethereum to get payed exactly for the computational resources they spend on this operation. This sometimes leads to OutOfGasError: when user didn't attach enough gas to execute his transaction, gas is payed to miner, because he was spending computational power, but transaction will not be send to block chain.

PrismLang Structure is able to overcome this issue by statically analyzing source code structure we can compute computational cost of the operations. PrismLang does not support looping or recursion primitives for end user, that leeds to the fact, than in the compile time we already know exactly how many operations we need to perform. Taking previous statement in consideration, we can conclude that PrismLang can provide both end user and miner with precise Cost of the smart contract.

## License

All contributions are made under the [GNU General Public License v3](https://www.gnu.org/licenses/gpl-3.0.en.html). See [LICENSE](LICENSE).

