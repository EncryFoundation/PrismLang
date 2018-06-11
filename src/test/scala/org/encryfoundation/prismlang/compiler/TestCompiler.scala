package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.PredefinedScope
import org.encryfoundation.prismlang.core.Ast.{Expr, Module}
import org.encryfoundation.prismlang.core.{TypeSystem, Types}

import scala.util.Try

trait TestCompiler {

  def compileExpr(expr: Expr): Try[Expr] = Try(StaticAnalyser.default.scan(Transformer.transform(expr)))

  def compileModule(module: Module): Try[CompiledContract] = Try {
    val schemas: List[Types.StructTag] = module.schemas.map(StructDescriptorInterpreter.interpretStruct)
    val contractArgs: List[(String, Types.PType)] = TypeSystem.default.resolveArgs(module.contract.args)
    val analyser: StaticAnalyser = StaticAnalyser(contractArgs ++ PredefinedScope.members, schemas)
    val compiledScript = analyser.scan(Transformer.transform(module.contract.body))
    CompiledContract(contractArgs, TypeBinder.bind(compiledScript, schemas))
  }
}
