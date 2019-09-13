package org.encryfoundation.prismlang.utils

import ove.crypto.digest.Blake2b
import scorex.crypto.hash.{CryptographicHash, CryptographicHash32, Digest32}

import scala.util.Try

trait Hasher extends CryptographicHash[Digest32] {

  def blake2bHash(message: Array[Byte]): Digest32 = Digest32 @@ blake2b.digest(message)
  val blake2b: ove.crypto.digest.Blake2b = Blake2b.Digest.newInstance
  override val DigestSize: Int = 32

  override def hash(input: Message): Digest32 = Digest32 @@ blake2b.digest(input)

  override def byteArrayToDigest(bytes: Array[Byte]): Try[Digest32] = Try {
    require(bytes.lengthCompare(DigestSize) == 0, "Incorrect digest size")
    Digest32 @@ bytes
  }
}

trait BouncyCastleHasher extends scorex.crypto.hash.Blake2b[Digest32] with CryptographicHash32 {
  override def hash(input: Message): Digest32 = Digest32 @@ internalHash(input)

  override def prefixedHash(prefix: Byte, inputs: Array[Byte]*): Digest32 =
    Digest32 @@ internalPrefixedHash(prefix, inputs: _*)
}
