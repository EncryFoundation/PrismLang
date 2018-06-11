package org.encryfoundation.prismlang.codec

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{Expr, Ident}
import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult}

class PNodeCodecSpec extends PropSpec with Matchers {

  property("Encode/decode") {

    val expr: Expr = Block(
        List(
          Let(
            Ident("ownerPubKey"),
            Base58Str("GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew"),
            None
          ),
          Call(
            Name(Ident("checkSig"), Types.Nit),
            List(
              Attribute(
                Attribute(
                  Name(Ident("ctx"), Types.Nit),
                  Ident("transaction"), Types.Nit
                ),
                Ident("msg"), Types.Nit
              ),
              Name(Ident("ownerPubKey"), Types.Nit),
              Name(Ident("signature"), Types.Nit)), Types.Nit
          )
        ),
        Types.Nit
      )

    val exprEncoded: BitVector = PNodeCodec.exprCodec.encode(expr).require

    val exprDecoded: Attempt[DecodeResult[Expr]] = PNodeCodec.exprCodec.decode(exprEncoded)

    exprDecoded.isSuccessful shouldBe true

    exprDecoded.require.value.toString shouldEqual expr.toString
  }
}
