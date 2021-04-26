package meta.generator

import fastparse.{Parsed, parse}
import meta.parser._

object DPIAPrimitives {
  def main(args: Array[String]): Unit = {
    val sourceDir = args.head
    val shinePath = os.Path(sourceDir) / "shine"
    os.walk.stream(shinePath).filter(_.ext == "dpia").foreach(path => {

      import DPIA.Phrase.AST._

      val definition = os.read(path)
      parse(definition, DPIA.Phrase.PrimitiveDeclarations(_)) match {
        case failure: Parsed.Failure =>
          println(s"Failed to parse `${failure.extra.input}'")
          println(s"  $failure")
        case Parsed.Success(seq, _) =>
          seq.foreach {
            case PrimitiveDeclaration(Identifier(originalName), scalaParams, params, returnType)
              if DPIA.isWellKindedDefinition(params, returnType) =>
              val name = originalName.capitalize

              val outputPath = (path / os.up) / s"$name.scala"
              println(s"Generate $outputPath")

              import scala.meta._
              val packageName = path.relativeTo(shinePath).segments.dropRight(1).foldLeft[Term.Ref](Term.Name("shine")) {
                case (t, name) => Term.Select(t, Term.Name(name))
              }
              val scalaParamsString = scalaParams match {
                case Some((start, end)) => definition.substring(start, end)
                case None => ""
              }
              val code = s"""// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
                            |// This file is automatically generated and should not be changed manually //
                            |// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
                            |${q"""
package $packageName {

import arithexpr.arithmetic._
import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._

${generateCaseClass(Type.Name(name), scalaParamsString, params, returnType)}

}""".toString()}
                            |""".stripMargin

              os.write.over(outputPath, code)
            case PrimitiveDeclaration(Identifier(name), _, params, returnType) =>
              println(s"Could not generate code for `$name' as parameters `$params' and/or `$returnType' are not well kinded.")
          }
      }
    })
  }

  def generateCaseClass(name: scala.meta.Type.Name,
                        scalaParamsString: String,
                        params: Seq[DPIA.Phrase.AST.Param],
                        returnType: DPIA.Type.AST): scala.meta.Defn.Class = {
    import scala.meta._
    import meta.parser.DPIA.Type.AST
    val (scalaReturnType, superClass) = returnType match {
      case AST.ExpType(_, _) => (t"ExpType", init"ExpPrimitive")
      case AST.AccType(_) => (t"AccType", init"AccPrimitive")
      case AST.CommType => (t"CommType", init"CommandPrimitive")
      case _ => throw new Exception(s"Expected `exp', `acc' or `comm' as return type for ${name.value}")
    }
    val generatedParams = generateParams(scalaParamsString, params)
    q"""
      final case class $name(...$generatedParams) extends $superClass {
        {
          ..${generateTypeChecks(params).stats}
        }

        ..${if (scalaReturnType != t"CommType") {
            List(q"override val t: $scalaReturnType = ${generateTerm(returnType)}")
          } else List() }

        ${generateVisitAndRebuild(name, generatedParams)}

        ..${if (scalaParamsString.nonEmpty && generatedParams.last.size > 1) {
              List(generateUnwrap(generatedParams.last))
            } else List() }
      }
    """
  }

  def generateParams(scalaParamsString: String,
                     params: Seq[DPIA.Phrase.AST.Param]): List[List[scala.meta.Term.Param]] = {
    import scala.meta._

    val scalaParams = if (scalaParamsString.nonEmpty) {
      List(scalaParamsString.split(",").map(param => {
        val parts = param.split(":").map(_.trim)
        val ty = parts(1).parse[Type].get
        param"${Term.Name(parts(0))}: $ty"
      }).toList)
    } else {
      List()
    }

    scalaParams ++ List(params.map(generateParam).toList)
  }

  def generateParam(param: DPIA.Phrase.AST.Param): scala.meta.Term.Param = {
    import scala.meta._
    import _root_.meta.parser.DPIA.Kind
    param"val ${Term.Name(param.id.name)}: ${
      param.ty match {
        case Left(kindAST) => generateType(kindAST)
        case Right(typeAST) => t"Phrase[${generatePhraseType(typeAST)}]"
      }
    }"
  }

  def generateTypeChecks(params: Seq[DPIA.Phrase.AST.Param]): scala.meta.Term.Block = {
    import scala.meta._
    q"""{
      ..${params.
            filter(param => param.ty.isRight). // only check types for parameters with phrase types
            map(param =>
              q"${Term.Name(param.id.name)} :: ${
                param.ty match {
                  case Right(typeAST@DPIA.Type.AST.DepFunType(id, kind, t)) =>
                    q"""{
                       ${Defn.Val(
                          mods = Nil,
                          pats = List(Pat.Var(name = Term.Name(id.name))),
                          decltpe = None,
                          rhs = q"${Term.Name(param.id.name)}.t.x"
                        )}
                       ${generateTerm(typeAST)}
                     }"""
                  case Right(typeAST) => generateTerm(typeAST)
                  case Left(kindAST) => throw new Exception("This should not happen")
                }}"
            ).toList}
    }"""
  }

  def generatePhraseType(typeAST: DPIA.Type.AST): scala.meta.Type = {
    import scala.meta._
    import meta.parser.DPIA.Type.AST
    typeAST match {
      case AST.ExpType(_, _) => t"ExpType"
      case AST.AccType(_) => t"AccType"
      case AST.CommType => t"CommType"
      case AST.PairType(lhs, rhs) => t"PhrasePairType[${generatePhraseType(lhs)}, ${generatePhraseType(rhs)}]"
      case AST.FunType(inT, outT) => t"FunType[${generatePhraseType(inT)}, ${generatePhraseType(outT)}]"
      case AST.DepFunType(id, kind, t) => t"DepFunType[${generateKindType(kind)}, ${generatePhraseType(t)}]"
      case AST.Identifier(name) => Type.Name(name)
    }
  }

  def generateType(kindAST: DPIA.Kind.AST): scala.meta.Type = {
    import scala.meta._
    import meta.parser.DPIA.Kind.AST
    kindAST match {
      case AST.RiseKind(riseKind) =>
        import meta.parser.rise.Kind.AST
        riseKind match {
          case AST.Data => Type.Name("DataType")
          case AST.Address => Type.Name("AddressSpace")
          case AST.Nat2Nat => Type.Name("NatToNat")
          case AST.Nat2Data => Type.Name("NatToData")
          case AST.Nat => Type.Name("Nat")
          case AST.Fragment => ???
          case AST.MatrixLayout => ???
          case AST.Function => ???
        }
      case AST.Access => Type.Name("AccessType")
    }
  }

  def generateKindType(kindAST: DPIA.Kind.AST): scala.meta.Type = {
    import scala.meta._
    import meta.parser.DPIA.Kind.AST
    kindAST match {
      case AST.RiseKind(riseKind) =>
        import meta.parser.rise.Kind.AST
        riseKind match {
          case AST.Data => Type.Name("DataKind")
          case AST.Address => Type.Name("AddressSpaceKind")
          case AST.Nat2Nat => Type.Name("NatToNatKind")
          case AST.Nat2Data => Type.Name("NatToDataKind")
          case AST.Nat => Type.Name("NatKind")
          case AST.Fragment => ???
          case AST.MatrixLayout => ???
          case AST.Function => ???
        }
      case AST.Access => Type.Name("AccessKind")
    }
  }

  def generateTerm(typeAST: DPIA.Type.AST): scala.meta.Term = {
    import scala.meta._
    import meta.parser.DPIA.Type.AST
    typeAST match {
      case AST.ExpType(dataType, access) =>
        q"expT(${RisePrimitives.generateDataType(dataType)}, ${generateTerm(access)})"
      case AST.AccType(dataType) =>
        q"accT(${RisePrimitives.generateDataType(dataType)})"
      case AST.CommType =>
        q"comm"
      case AST.PairType(lhs, rhs) =>
        q"PhrasePairType(${generateTerm(lhs)}, ${generateTerm(rhs)})"
      case AST.FunType(inT, outT) =>
        q"FunType(${generateTerm(inT)}, ${generateTerm(outT)})"
      case AST.DepFunType(id, kind, t) =>
        q"DepFunType[${generateKindType(kind)}, PhraseType](${Term.Name(id.name)}, ${generateTerm(t)})"
      case AST.Identifier(name) => Term.Name(name)
    }
  }

  def generateTerm(accessAST: DPIA.Type.Access.AST): scala.meta.Term = {
    import scala.meta._
    import meta.parser.DPIA.Type.Access.AST
    accessAST match {
      case AST.Identifier(name) => Term.Name(name)
      case AST.Read => Term.Name("read")
      case AST.Write =>Term.Name("write")
    }
  }

  def generateVisitAndRebuild(name: scala.meta.Type.Name,
                              paramLists: List[List[scala.meta.Term.Param]]): scala.meta.Defn.Def = {
    import scala.meta._

    object TypeIs {
      def unapply(ty: Type): Option[String] = ty match {
        case Type.Name(name) => Some(name)
        case Type.Select(_, Type.Name(name)) => Some(name)
        case _ => None
      }
    }

    def injectVisitCall(param: Term.Param): Term = {
      param.decltpe match {
        case Some(ty) => ty match {
          case TypeIs("Nat") | TypeIs("NatIdentifier") =>
            q"v.nat(${Term.Name(param.name.value)})"
          case TypeIs("DataType") | TypeIs("ScalarType") | TypeIs("BasicType") =>
            q"v.data(${Term.Name(param.name.value)})"
          case TypeIs("NatToNat") =>
            q"v.natToNat(${Term.Name(param.name.value)})"
          case TypeIs("NatToData") =>
            q"v.natToData(${Term.Name(param.name.value)})"
          case TypeIs("AccessType") =>
            q"v.access(${Term.Name(param.name.value)})"
          case TypeIs("AddressSpace") =>
            q"v.addressSpace(${Term.Name(param.name.value)})"
          case TypeIs("LocalSize") | TypeIs("GlobalSize") =>
            q"${Term.Name(param.name.value)}.visitAndRebuild(v)"
          case Type.Apply(Type.Name("Phrase"), _) => // Phrase[_]
            q"VisitAndRebuild(${Term.Name(param.name.value)}, v)"
          case Type.Apply(Type.Name("Vector"), List(Type.Apply(Type.Name("Phrase"), _))) // Vector[Phrase[_]]
            |  Type.Apply(Type.Name("Seq"), List(Type.Apply(Type.Name("Phrase"), _))) => // Seq[Phrase[_]]
            q"${Term.Name(param.name.value)}.map(VisitAndRebuild(_, v))"
          case _ =>
            Term.Name(param.name.value)
        }
        case None => throw new Exception(s"Expected type declaration")
      }
    }

    q"""override def visitAndRebuild(v: VisitAndRebuild.Visitor): $name =
       new $name(...${paramLists.map(_.map(injectVisitCall))})
     """
  }

  def generateUnwrap(paramList: List[scala.meta.Term.Param]): scala.meta.Defn.Def = {
    import scala.meta._
    val (types, names) = paramList.map({
      case Term.Param(_, name, Some(typ), _) => (typ, Term.Name(name.value))
    }).unzip
    q"""
      def unwrap: (..$types) = (..$names)
     """
  }
}
