package org.encryfoundation.prismlang.compiler.scope

import org.encryfoundation.prismlang.core.Types
import org.encryfoundation.prismlang.lib.predefined.decode.Base58decode
import org.encryfoundation.prismlang.lib.predefined.hash._
import org.encryfoundation.prismlang.lib.predefined.signature.CheckSig
import org.encryfoundation.prismlang.lib.predefined.time.Str2Time

object PredefinedScope {

  val members: Seq[Symbol] = Seq(
    Symbol(CheckSig.name, Types.PFunc(CheckSig.args.toList, Types.PBoolean)),
    Symbol(Blake2b256Hash.name, Types.PFunc(Blake2b256Hash.args.toList, Types.PCollection.ofByte)),
    Symbol(Blake2b512Hash.name, Types.PFunc(Blake2b512Hash.args.toList, Types.PCollection.ofByte)),
    Symbol(Keccak256Hash.name, Types.PFunc(Keccak256Hash.args.toList, Types.PCollection.ofByte)),
    Symbol(Keccak512Hash.name, Types.PFunc(Keccak512Hash.args.toList, Types.PCollection.ofByte)),
    Symbol(Sha256Hash.name, Types.PFunc(Sha256Hash.args.toList, Types.PCollection.ofByte)),
    Symbol(Str2Time.name, Types.PFunc(Str2Time.args.toList, Types.PInt)),
    Symbol(Base58decode.name, Types.PFunc(Base58decode.args.toList, Types.POption(Types.PCollection.ofByte)))
  )
}
