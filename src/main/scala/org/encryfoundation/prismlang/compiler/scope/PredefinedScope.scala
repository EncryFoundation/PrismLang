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
    base.Base16encode,
    base.Base58encode,
    math.Min,
    math.Max,
    collection.AnyOf,
    collection.AllOf
  )

  val members: List[(String, Types.PFunc)] = List(
    (signature.CheckSig.name, Types.PFunc(signature.CheckSig.args.toList, Types.PBoolean)),
    (hash.Blake2b256Hash.name, Types.PFunc(hash.Blake2b256Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Blake2b512Hash.name, Types.PFunc(hash.Blake2b512Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Keccak256Hash.name, Types.PFunc(hash.Keccak256Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Keccak512Hash.name, Types.PFunc(hash.Keccak512Hash.args.toList, Types.PCollection.ofByte)),
    (hash.Sha256Hash.name, Types.PFunc(hash.Sha256Hash.args.toList, Types.PCollection.ofByte)),
    (time.UnixTime.name, Types.PFunc(time.UnixTime.args.toList, Types.PInt)),
    (base.Base16encode.name, Types.PFunc(base.Base16encode.args.toList, Types.PString)),
    (base.Base58encode.name, Types.PFunc(base.Base58encode.args.toList, Types.PString)),
    (math.Min.name, Types.PFunc(math.Min.args.toList, Types.PInt)),
    (math.Max.name, Types.PFunc(math.Max.args.toList, Types.PInt)),
    (collection.AnyOf.name, Types.PFunc(collection.AnyOf.args.toList, Types.PBoolean)),
    (collection.AllOf.name, Types.PFunc(collection.AllOf.args.toList, Types.PBoolean))
  )
}
