
## Data Types

- `Any` - a supertype of any other type
- `Unit` - a type with a single value `()`
- `Int` - 64 bit signed integer
- `Bool` - a type with two logical values `true` and `false`
- `ByteVector` - arbitrary sequence of bytes
- `Option[T]` - a container which either have some value of type `T` or none.
- `Array[T]` - arrays of arbitrary length with all values of type `T` 

## Syntax

    // Constant definition
    let a: Int = 10                       // Explicit type declaration
    let b = 100                           // Type will be inferred automatically
    let c = if (a > b) true else false    // Conditional assignment

    // Function definition
    def sum(a: Int, b: Int): Int = {
        a + b
    }
    
    // Lambda definition
    lamb (a: Int, b: Int) = a + b

    // If statement
    val flag: Bool = if (10 < 100) {
        true
    } elif (10 == 100) {
        true
    } else {
        false
    }
    
    // Type matching
    let validProof: Bool = if (let sig: Signature25519 = poof) checkSig(sig, msg, pk) else false

    // Base58 string
    let pubKeyBytes: ByteVector = base58'75Gs7HHUNnoEzsPgRRVABzQaC3UZVcayw9NY457Kx5p'

    // Collections
    let ageList: Array[Int] = List(1, 2, 3, 4)
    let ageDict: Dict[String, Int] = Dict('Alice' -> 4, 'Bob' -> 9, 'Tom' -> 17)

    let someonesAge = ageList[0].get        // Will result in `1`
    let aliceAge = ageDict['Alice'].get     // Will result in `4`

    // Lambdas application
    let doesExist: Bool = ageList.exists(lamb (i: Int) = i > 3)     // Will result in `true`
