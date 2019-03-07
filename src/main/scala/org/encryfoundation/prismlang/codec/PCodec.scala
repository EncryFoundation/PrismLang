package org.encryfoundation.prismlang.codec

import org.encryfoundation.prismlang.core.Types._
import org.encryfoundation.prismlang.core.Ast._
import org.encryfoundation.prismlang.core.wrapped.BoxedValue
import scodec.Codec
import scodec.codecs.{Discriminated, uint2, uint4, uint8}

object PCodec {

  import scodec.codecs.implicits._

  implicit def dT = Discriminated[PType, Int](uint8)
  implicit def dAny = dT.bind[PAny.type](0)
  implicit def dUnit = dT.bind[PUnit.type](1)
  implicit def dInt = dT.bind[PInt.type](2)
  implicit def dBool = dT.bind[PBoolean.type](3)
  implicit def dString = dT.bind[PString.type](4)
  implicit def dByte = dT.bind[PByte.type](5)
  implicit def dColl = dT.bind[PCollection](6)
  implicit def dFunc = dT.bind[PFunc](7)
  implicit def dTuple = dT.bind[PTuple](8)
  implicit def dObj = dT.bind[ArbitraryProduct](9)
  implicit def dStructTag = dT.bind[StructTag](10)
  implicit def dSig = dT.bind[Signature25519.type](11)
  implicit def dMulSig = dT.bind[MultiSig.type](12)
  implicit def dEncryBox = dT.bind[EncryBox.type](13)
  implicit def dAssetBox = dT.bind[AssetBox.type](14)
  implicit def dAiBox = dT.bind[AssetIssuingBox.type](15)
  implicit def dDBox = dT.bind[DataBox.type](16)
  implicit def dTransact = dT.bind[EncryTransaction.type](17)
  implicit def dState = dT.bind[EncryState.type](18)
  implicit def dNit = dT.bind[Nit.type](19)

  implicit def dExp = Discriminated[Expr, Int](uint8)
  implicit def dBlc = dExp.bind[Expr.Block](0)
  implicit def dLet = dExp.bind[Expr.Let](1)
  implicit def dDef = dExp.bind[Expr.Def](2)
  implicit def dLamb = dExp.bind[Expr.Lambda](3)
  implicit def dIf = dExp.bind[Expr.If](4)
  implicit def dIfLet = dExp.bind[Expr.IfLet](5)
  implicit def dIfLetR = dExp.bind[Expr.IfLetR](6)
  implicit def dBoolOp = dExp.bind[Expr.Bool](7)
  implicit def dBin = dExp.bind[Expr.Bin](8)
  implicit def dUnary = dExp.bind[Expr.Unary](9)
  implicit def dCompOp = dExp.bind[Expr.Compare](10)
  implicit def dName = dExp.bind[Expr.Name](11)
  implicit def dCall = dExp.bind[Expr.Call](12)
  implicit def dAttr = dExp.bind[Expr.Attribute](13)
  implicit def dSubscr = dExp.bind[Expr.Subscript](14)
  implicit def dIntConst = dExp.bind[Expr.IntConst](15)
  implicit def dByteConst = dExp.bind[Expr.ByteConst](16)
  implicit def dStr = dExp.bind[Expr.Str](17)
  implicit def dCollConst = dExp.bind[Expr.Collection](18)
  implicit def dTupleConst = dExp.bind[Expr.Tuple](19)
  implicit def dBase58Str = dExp.bind[Expr.Base58Str](20)
  implicit def dBase16Str = dExp.bind[Expr.Base16Str](21)
  implicit def dTrue = dExp.bind[Expr.True.type](22)
  implicit def dFalse = dExp.bind[Expr.False.type](23)
  implicit def dSizeOf = dExp.bind[Expr.SizeOf](24)
  implicit def dExists = dExp.bind[Expr.Exists](25)
  implicit def dSum = dExp.bind[Expr.Sum](26)
  implicit def dMap = dExp.bind[Expr.Map](27)
  implicit def dFilt = dExp.bind[Expr.Filter](28)

  implicit def dNode = Discriminated[Node, Int](uint2)
  implicit def dModule = dNode.bind[Module](0)
  implicit def dSchema = dNode.bind[Struct](1)
  implicit def dContract = dNode.bind[Contract](2)

  implicit def dSliceOp = Discriminated[SliceOp, Int](uint2)
  implicit def dSlice = dSliceOp.bind[SliceOp.Slice](0)
  implicit def dIdx = dSliceOp.bind[SliceOp.Index](1)

  implicit def dBooleanOp = Discriminated[BooleanOp, Int](uint2)
  implicit def dAnd = dBooleanOp.bind[BooleanOp.And.type](0)
  implicit def dOr = dBooleanOp.bind[BooleanOp.Or.type](1)

  implicit def dOperator = Discriminated[Operator, Int](uint4)
  implicit def dAdd = dOperator.bind[Operator.Add.type](0)
  implicit def dSub = dOperator.bind[Operator.Sub.type](1)
  implicit def dMult = dOperator.bind[Operator.Mult.type](2)
  implicit def dDiv = dOperator.bind[Operator.Div.type](3)
  implicit def dMod = dOperator.bind[Operator.Mod.type](4)
  implicit def dPow = dOperator.bind[Operator.Pow.type](5)

  implicit def dUnaryOp = Discriminated[UnaryOp, Int](uint2)
  implicit def dInv = dUnaryOp.bind[UnaryOp.Invert.type](0)
  implicit def dNot = dUnaryOp.bind[UnaryOp.Not.type](1)

  implicit def dCompare = Discriminated[CompOp, Int](uint4)
  implicit def dEq = dCompare.bind[CompOp.Eq.type](0)
  implicit def dNotEq = dCompare.bind[CompOp.NotEq.type](1)
  implicit def dLt = dCompare.bind[CompOp.Lt.type](2)
  implicit def dLtE = dCompare.bind[CompOp.LtE.type](3)
  implicit def dGt = dCompare.bind[CompOp.Gt.type](4)
  implicit def dGtE = dCompare.bind[CompOp.GtE.type](5)
  implicit def dIn = dCompare.bind[CompOp.In.type](6)
  implicit def dNotIn = dCompare.bind[CompOp.NotIn.type](7)

  implicit def dTypeDesc = Discriminated[TypeDescriptor, Int](uint2)
  implicit def dSimpleT = dTypeDesc.bind[TypeDescriptor.SimpleType](0)
  implicit def dProdT = dTypeDesc.bind[TypeDescriptor.ProductType](1)

  implicit def dBoxedVal = Discriminated[BoxedValue, Int](uint4)
  implicit def dIntVal = dBoxedVal.bind[BoxedValue.IntValue](0)
  implicit def dBoolVal = dBoxedVal.bind[BoxedValue.BoolValue](1)
  implicit def dStringVal = dBoxedVal.bind[BoxedValue.StringValue](2)
  implicit def dByteVal = dBoxedVal.bind[BoxedValue.ByteValue](3)
  implicit def dByteCollVal = dBoxedVal.bind[BoxedValue.ByteCollectionValue](4)
  implicit def dSigVal = dBoxedVal.bind[BoxedValue.Signature25519Value](5)
  implicit def dMultiSigVal = dBoxedVal.bind[BoxedValue.MultiSignatureValue](6)

  val exprCodec: Codec[Expr] = Codec[Expr]
  val nodeCodec: Codec[Node] = Codec[Node]
  val typeCodec: Codec[PType] = Codec[PType]
  val boxedValCodec: Codec[BoxedValue] = Codec[BoxedValue]
}
