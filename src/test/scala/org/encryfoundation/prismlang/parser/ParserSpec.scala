package org.encryfoundation.prismlang.parser

import org.encryfoundation.prismlang.core.Ast.TypeDescriptor.{ProductType, SimpleType}
import org.encryfoundation.prismlang.core.Types
import org.scalatest.{Matchers, PropSpec}

import scala.collection.mutable.ArrayBuffer

class ParserSpec extends PropSpec with Matchers with Parser {

  import org.encryfoundation.prismlang.core.Ast.Expr._
  import org.encryfoundation.prismlang.core.Ast._

  property("BoolOp parsing (&&)") {

    val source = "true && true"

    val expected: Seq[Expr] = ArrayBuffer(
      Bool(
        BooleanOp.And,
        List(True, True)
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("BoolOp parsing (||)") {

    val source = "true || true"

    val expected: Seq[Expr] = ArrayBuffer(
      Bool(
        BooleanOp.Or,
        List(True, True)
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Lambda parsing") {

    val source = "lamb (a: Int, b: Int) = a + b"

    val expected: Seq[Expr] = ArrayBuffer(
      Lambda(
        List(
          (Ident("a"), TypeIdent("Int", List())),
          (Ident("b"), TypeIdent("Int", List()))
        ),
        Bin(
          Name(Ident("a")),
          Operator.Add,
          Name(Ident("b"))
        )
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Lambda parsing (with block)") {

    val source = "lamb (a: Int, b: Int) = { a + b }"

    val expected: Seq[Expr] = ArrayBuffer(
      Lambda(
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
        )
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Lambda parsing (with if)") {

    val source = "lamb (a: Int, b: Int) = if (a > b) b else a"

    val expected: Seq[Expr] = ArrayBuffer(
      Lambda(
        List(
          (Ident("a"), TypeIdent("Int", List())),
          (Ident("b"), TypeIdent("Int", List()))
        ),
        If(
          Compare(
            Name(Ident("a")),
            List(CompOp.Gt),
            List(Name(Ident("b")))
          ),
          Name(Ident("b")),
          Name(Ident("a"))
        )
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (explicit type annotation)") {

    val source = "let age: Int = 28"

    val expected: Seq[Expr] = ArrayBuffer(Let(Ident("age"), IntConst(28), Some(TypeIdent("Int", List()))))

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let") {

    val source = "let age = 28"

    val expected: Seq[Expr] = ArrayBuffer(Let(Ident("age"), IntConst(28), None))

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (block assignment)") {

    val source = "let age = { 100 / 4 }"

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("age"),
        Block(
          List(
            Bin(
              IntConst(100),
              Operator.Div,
              IntConst(4)
            )
          )
        ),
        None
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Let (complex block assignment)") {

    val source =
      """
        |let age: Int = {
        |  let sum = 30 + 40
        |  sum - 10
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("age"),
        Block(
          List(
            Let(
              Ident("sum"),
              Bin(
                IntConst(30),
                Operator.Add,
                IntConst(40)
              ),
              None
            ),
            Bin(
              Name(Ident("sum")),
              Operator.Sub,
              IntConst(10)
            )
          )
        ),
        Some(TypeIdent("Int", List()))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Def") {

    val source =
      """
        |def sum(a: Int, b: Int): Int = {
        |  a + b
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Def(
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
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("If (inline)") {

    val source =
      """
        |if (3 > 0) true
        |else false
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      If(
        Compare(
          IntConst(3),
          List(CompOp.Gt),
          List(IntConst(0))
        ),
        True,
        False
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("If") {

    val source =
      """
        |if (3 > 0) {
        |  true
        |} else {
        |  false
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      If(
        Compare(
          IntConst(3),
          List(CompOp.Gt),
          List(IntConst(0))
        ),
        Block(List(True)),
        Block(List(False))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("IfLet") {

    val source =
      """
        |if (let a: String = b) {
        |  true
        |} else {
        |  false
        |}
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      IfLet(
        Ident("a"),
        TypeIdent("String",List()),
        Name(Ident("b")),
        Block(List(True)),
        Block(List(False))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Dot-notation") {

    val source =
      """
        |object.method()
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Call(
        Attribute(
          Name(Ident("object")),
          Ident("method"),
          Types.Nit
        ),
        List()
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Attribute on constant") {

    val source =
      """
        |(1).toByte
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Attribute(
        IntConst(1),
        Ident("toByte"),
        Types.Nit
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Nested parametrized type annotation") {

    val source =
      """
        |let coll: Array[Array[Int]] = Array(Array(1), Array(1))
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("coll"),
        Collection(
          List(
            Collection(
              List(
                IntConst(1)
              ),
              Types.Nit
            ),
            Collection(
              List(
                IntConst(1)
              ),
              Types.Nit
            )
          ),
          Types.Nit
        ),
        Some(TypeIdent("Array", List(TypeIdent("Array", List(TypeIdent("Int"))))))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Collection constant") {

    val source =
      """
        |let coll: Array[Int] = Array(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024)
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("coll"),
        Collection(
          List(
            IntConst(1),
            IntConst(2),
            IntConst(4),
            IntConst(8),
            IntConst(16),
            IntConst(32),
            IntConst(64),
            IntConst(128),
            IntConst(256),
            IntConst(512),
            IntConst(1024)
          ),
          Types.Nit
        ),
        Some(TypeIdent("Array", List(TypeIdent("Int"))))
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Nested collection constant") {

    val source =
      """
        |let coll = Array(Array(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024), Array(1, 2, 4, 8, 16, 32, 64, 128, 256, 512, 1024))
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Let(
        Ident("coll"),
        Collection(
          List(
            Collection(
              List(
                IntConst(1),
                IntConst(2),
                IntConst(4),
                IntConst(8),
                IntConst(16),
                IntConst(32),
                IntConst(64),
                IntConst(128),
                IntConst(256),
                IntConst(512),
                IntConst(1024)
              ),
              Types.Nit
            ),
            Collection(
              List(
                IntConst(1),
                IntConst(2),
                IntConst(4),
                IntConst(8),
                IntConst(16),
                IntConst(32),
                IntConst(64),
                IntConst(128),
                IntConst(256),
                IntConst(512),
                IntConst(1024)
              ),
              Types.Nit
            )
          ),
          Types.Nit
        ),
        None
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Direct function call on collection") {

    val source =
      """
        |allOf(Array(2 > 1, 1 == 1, 3 != 4, 10 < 100, true))
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Call(
        Name(Ident("allOf"), Types.Nit),
        List(
          Collection(
            List(
              Compare(
                IntConst(2),
                List(CompOp.Gt),
                List(IntConst(1))
              ),
              Compare(
                IntConst(1),
                List(CompOp.Eq),
                List(IntConst(1))
              ),
              Compare(
                IntConst(3),
                List(CompOp.NotEq),
                List(IntConst(4))
              ),
              Compare(
                IntConst(10),
                List(CompOp.Lt),
                List(IntConst(100))
              ),
              True
            ), Types.Nit
          )
        ), Types.Nit
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("CompOp.In") {

    val source =
      """
        |2 in Array(1, 2, 3, 4)
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Compare(
        IntConst(2),
        List(CompOp.In),
        List(
          Collection(
            List(
              IntConst(1),
              IntConst(2),
              IntConst(3),
              IntConst(4)
            ),
            Types.Nit
          )
        )
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("Array (multiline)") {

    val source =
      """
        |Array(
        |  1,
        |  2,
        |  3,
        |  4
        |)
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Collection(
        List(
          IntConst(1),
          IntConst(2),
          IntConst(3),
          IntConst(4)
        ),
        Types.Nit
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("BooleanOps associativity") {

    val source =
      """
        |true && false || false && false
      """.stripMargin

    val expected: Seq[Expr] = ArrayBuffer(
      Bool(
        BooleanOp.Or,
        List(
          Bool(
            BooleanOp.And,
            List(True, False)
          ),
          Bool(
            BooleanOp.And,
            List(False, False)
          )
        )
      )
    )

    val parsedTry = parse(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }

  property("module") {

    val source =
      """
        |struct CustomerBox:Object(
        |  person:Object(name:String; age:Int);
        |  orders:Array[Object(product_id:Long; amount:Long;)];
        |  id:Long;
        |)
        |
        |contract (signature: Signature25519) = {
        |  let ownerPubKey = base58"GtBn7qJwK1v1EbB6CZdgmkcvt849VKVfWoJBMEWsvTew"
        |  checkSig(ctx.transaction.msg, ownerPubKey, signature)
        |}
      """.stripMargin

    val expected: Module = Module(
      Contract(
        Block(
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
        ),
        List(
          (Ident("signature"), TypeIdent("Signature25519", List()))
        )
      ),
      List(
        Struct(
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
                        (Ident("product_id"), SimpleType(Ident("Long"), List())),
                        (Ident("amount"), SimpleType(Ident("Long"), List()))
                      )
                    )
                  )
                )
              ),
              (Ident("id"), SimpleType(Ident("Long"),List()))
            )
          )
        )
      )
    )

    val parsedTry = parseModule(source)

    parsedTry.isSuccess shouldBe true

    parsedTry.get.toString shouldEqual expected.toString
  }
}
