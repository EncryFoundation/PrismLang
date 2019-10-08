package org.encryfoundation.prismlang.lib.predefined

import org.encryfoundation.prismlang.core.wrapped.PWrappedMember

object PredefFunctions {

  /** All predef functions by their categories. */
  val timeF: Seq[BuiltInFunctionHolder] = Seq(time.UnixTime)
  val cryptoF: Seq[BuiltInFunctionHolder] = Seq(signature.CheckSig)
  val hashF: Seq[BuiltInFunctionHolder] = Seq(
    hash.Blake2b256Hash,
    hash.Blake2b512Hash,
    hash.Keccak256Hash,
    hash.Keccak512Hash,
    hash.Sha256Hash
  )
  val baseF: Seq[BuiltInFunctionHolder] = Seq(
    base.Base16encode, base.Base58encode
  )
  val mathF: Seq[BuiltInFunctionHolder] = Seq(
    math.Max,
    math.Min
  )
  val collF: Seq[BuiltInFunctionHolder] = Seq(
    collection.AllOf,
    collection.AnyOf
  )

  val all: Map[String, PWrappedMember] =
    (timeF ++ cryptoF ++ hashF ++ baseF ++ mathF ++ collF).map(f => f.name -> f.asFunc).toMap

  /** The most computationally expensive functions */
  val heavyFunctions: Seq[BuiltInFunctionHolder] = cryptoF ++ hashF

  /** The most computationally expensive functions */
  val veryFunctions: Seq[BuiltInFunctionHolder] = Seq(
    signature.CheckSig
  )
}
