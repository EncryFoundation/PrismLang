package org.encryfoundation.prismlang.compiler.scope

import org.encryfoundation.prismlang.core.Types


object PredefinedScope {

  import org.encryfoundation.prismlang.lib.predefined._

  val all: List[BuiltInFunctionHolder] = List(
    signature.CheckSig,
    hash.Blake2b256Hash,
    hash.Blake2b512Hash,
    hash.Keccak256Hash,
    hash.Keccak512Hash,
    hash.Sha256Hash,
    time.UnixTime,
    decode.Base58decode,
    math.Min,
    math.Max,
    collection.AnyOf,
    collection.AllOf
  )

  val members: List[(String, Types.PType)] = List(
    (signature.CheckSig.name, Types.PFunc(signature.CheckSig.args.toList, Types.PBoolean)),
    (hash.Blake2b256Hash.name, Types.PFunc(hash.Blake2b256Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Blake2b512Hash.name, Types.PFunc(hash.Blake2b512Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Keccak256Hash.name, Types.PFunc(hash.Keccak256Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Keccak512Hash.name, Types.PFunc(hash.Keccak512Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Sha256Hash.name, Types.PFunc(hash.Sha256Hash.args.toList, Types.PCollection.ofByte)),
    (time.UnixTime.name, Types.PFunc(time.UnixTime.args.toList, Types.PInt)),
    (decode.Base58decode.name, Types.PFunc(decode.Base58decode.args.toList, Types.PCollection.ofByte)),
    (math.Min.name, Types.PFunc(math.Min.args.toList, Types.PInt)),
    (math.Max.name, Types.PFunc(math.Max.args.toList, Types.PInt)),
    (collection.AnyOf.name, Types.PFunc(collection.AnyOf.args.toList, Types.PBoolean)),
    (collection.AllOf.name, Types.PFunc(collection.AllOf.args.toList, Types.PBoolean))
  )
}
