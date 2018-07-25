package org.encryfoundation.prismlang.core

object CostTable {

  val ConstDeclarationC: Int = 10
  val FuncDeclarationC: Int  = 20
  val LambDeclarationC: Int  = 5
  val BlockDeclarationC: Int = 5
  val NestedBlockPenalty: Double = 1.2

  val NameRefC: Int          = 2
  val CallC: Int             = 4
  val BoolOpC: Int           = 5
  val BinOpC: Int            = 8
  val CompOpC: Int           = 4
  val IfC: Int               = 5
  val IfLetC: Int            = 10
  val UnaryOpC: Int          = 1
  val SubscriptC: Int        = 8

  // Constants
  val IntConstC: Int         = 1
  val ByteConstC: Int        = 1
  val CharC: Int             = 2
  val CollEltC: Int          = 1
  val TupleEltC: Int         = 1
  val BoolLeafC: Int         = 1

  val DecodingC: Int         = 4

  val MapC: Int              = 4
  val SizeOfC: Int           = 2
  val SumC: Int              = 2
  val ExistsC: Int           = MapC
}
