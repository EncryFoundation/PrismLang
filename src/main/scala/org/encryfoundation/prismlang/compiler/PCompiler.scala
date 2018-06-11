package org.encryfoundation.prismlang.compiler

import org.encryfoundation.prismlang.compiler.scope.PredefinedScope
import org.encryfoundation.prismlang.core.{Ast, TypeSystem, Types}
import org.encryfoundation.prismlang.parser.Parser

import scala.util.Try

object PCompiler {

  /** Parser -> Transformer -> StaticAnalyser -> TypeBinder */
  def compile(source: String): Try[CompiledContract] = Parser.parseModule(source).map { module =>
    val schemas: List[Types.StructTag] = module.schemas.map(StructDescriptorInterpreter.interpretStruct)
    val contractArgs: List[(String, Types.PType)] = TypeSystem.default.resolveArgs(module.contract.args)
    val analyser: StaticAnalyser = StaticAnalyser(contractArgs ++ PredefinedScope.members, schemas)
    val compiledScript: Ast.Expr = analyser.scan(Transformer.transform(module.contract.body))
    CompiledContract(contractArgs, TypeBinder.bind(compiledScript, schemas))
  }
}
