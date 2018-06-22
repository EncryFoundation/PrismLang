package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.Expr._
import org.encryfoundation.prismlang.core.Ast.{Expr, Ident, Operator, TypeIdent}
import org.encryfoundation.prismlang.core.{CostTable, Types}
import org.scalatest.{Matchers, PropSpec}

class CostEstimatorSpec extends PropSpec with Matchers {

  property("Cost analysis") {
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
              Ident("msg"), Types.Nit),
            Name(Ident("ownerPubKey"), Types.Nit),
            Name(Ident("signature"), Types.Nit)), Types.Nit
        )
      ),
      Types.Nit
    )

    val calculator: CostEstimator = CostEstimator.default

    val cost: Int = calculator.costOf(expr)

    cost shouldBe 173
  }

  property("Function call cost estimation") {

    val func: Expr = Def(
      Ident("sum"),
      List(
        (Ident("a"), TypeIdent("Int", List())),
        (Ident("b"), TypeIdent("Int", List()))
      ),
      Block(
        List(
          Bin(
            Name(Ident("a")),
            Operator.Add,
            Name(Ident("b"))
          )
        )
      ),
      TypeIdent("Int",List())
    )

    val funcCalls: Expr = Block(
      List(
        Call(Name(Ident("sum"), Types.Nit), List(IntConst(1), IntConst(2))),
        Call(Name(Ident("sum"), Types.Nit), List(IntConst(1), IntConst(2))),
        Call(Name(Ident("sum"), Types.Nit), List(IntConst(1), IntConst(2))),
        Call(Name(Ident("sum"), Types.Nit), List(IntConst(1), IntConst(2)))
      )
    )

    val calculator: CostEstimator = CostEstimator.default

    val _: Int = calculator.costOf(func)

    val callsCost: Int = calculator.costOf(funcCalls) - CostTable.BlockDeclarationC

    callsCost / 4 shouldEqual 25
  }
}
