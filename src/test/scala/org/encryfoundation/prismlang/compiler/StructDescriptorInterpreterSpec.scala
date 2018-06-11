package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.core.Ast.{Ident, Struct}
import org.encryfoundation.prismlang.core.Ast.TypeDescriptor.{ProductType, SimpleType}
import org.encryfoundation.prismlang.core.Types._
import org.scalatest.{Matchers, PropSpec}

import scala.util.Try

class StructDescriptorInterpreterSpec extends PropSpec with Matchers {

  property("Structure description interpretation") {
    val struct: Struct = Struct(
      Ident("CustomerBox"),
      ProductType(
        List(
          (
            Ident("person"),
            ProductType(
              List(
                (Ident("name"), SimpleType(Ident("String"),List())),
                (Ident("age"),SimpleType(Ident("Int"), List()))))
          ),
          (
            Ident("orders"),
            SimpleType(
              Ident("Array"),
              List(
                ProductType(
                  List(
                    (Ident("product_id"), SimpleType(Ident("Int"), List())),
                    (Ident("amount"), SimpleType(Ident("Int"), List()))
                  )
                )
              )
            )
          ),
          (Ident("id"), SimpleType(Ident("Int"),List()))
        )
      )
    )

    val expectedType: StructTag = StructTag(
      "CustomerBox",
      ArbitraryProduct(
        "$anon_obj",
        List(
          (
            "person",
            ArbitraryProduct(
              "$anon_obj",
              List(
                (
                  "name",
                  PString
                ),
                (
                  "age",
                  PInt
                )
              )
            )
          ),
          (
            "orders",
            PCollection(
              ArbitraryProduct(
                "$anon_obj",
                List(
                  (
                    "product_id",
                    PInt
                  ),
                  (
                    "amount",
                    PInt
                  )
                )
              )
            )
          ),
          (
            "id",
            PInt
          )
        )
      )
    )

    val resultTry = Try(StructDescriptorInterpreter.interpretStruct(struct))

    resultTry.isSuccess shouldBe true

    resultTry.get.toString shouldEqual expectedType.toString
  }
}
