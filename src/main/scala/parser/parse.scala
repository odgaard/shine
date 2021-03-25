package parser //old branch 17. Dezember 2020

import rise.core.{Lambda, primitives => rp, semantics => rS, types => rt}
import rise.{core => r, openCL => o}
import r.{DSL => rd}
import o.{primitives => op}

import scala.collection.mutable

object parse {

  abstract sealed class ParseErrorOrState()

  abstract sealed class ParseEnd() extends ParseErrorOrState()

  final case class ParseError(mes: String) extends ParseErrorOrState()

  /*
   * Precondition: Only valid Tokens in tokenList.
   */
  def apply(tokenList: List[Token]): MapFkt = {
    val parseState: ParseState = ParseState(tokenList, Nil, new MapFkt, new MapDepL, None)
    val shineLambda: MapFkt = parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState) match {
      case Left(map) => map
      case Right(errorOrState) => {
        println(errorOrState)
        throw new RuntimeException("failed parsing : " + errorOrState)
      }
    }
    println("parse: " + shineLambda)
    shineLambda
    //r.Identifier("placeholder")()
  }

  sealed trait NatElement

  final case class NNumber(nat: rt.Nat) extends NatElement

  final case class NIdentifier(nat: rt.NatIdentifier) extends NatElement

  sealed trait DataElement

  final case class DIdentifier(data: rt.DataTypeIdentifier) extends DataElement

  final case class DType(data: rt.DataType) extends DataElement

  sealed trait SIntToExprAlternatives

  final case class AltMapGlobal() extends SIntToExprAlternatives

  final case class AltMapLocal() extends SIntToExprAlternatives

  final case class AltMapWorkGroup() extends SIntToExprAlternatives

  final case class AltMakeArray() extends SIntToExprAlternatives

  sealed trait SyntaxElement

  final case class SExprClutched(expr: r.Expr, spanClutch: Span) extends SyntaxElement

  final case class SExpr(expr: r.Expr) extends SyntaxElement

  final case class SType(t: rt.Type, span: Span) extends SyntaxElement

  final case class SIntToExpr(name: SIntToExprAlternatives, span: Span) extends SyntaxElement

  final case class SNat(nat: NatElement, span: Span) extends SyntaxElement

  final case class SData(data: DataElement, span: Span) extends SyntaxElement

  final case class SAddrSpace(addrSpace: rt.AddressSpace, span: Span) extends SyntaxElement

  sealed trait RiseKind //Todo: Scoping einbauen, also Kind nennen und Token explizit immer hinzufügen
  final case class RData() extends RiseKind

  final case class RNat() extends RiseKind

  final case class RAddrSpace() extends RiseKind

  //Todo: if I have Identifier, I have to get the right Span and the Span is differntly each time
  type MapFkt = mutable.HashMap[String, Either[rd.ToBeTyped[r.Expr], r.types.Type]]
  type MapDepL = mutable.HashMap[String, RiseKind]
  type BracesSpan = Option[List[Span]]

  final case class ParseState(tokenStream: List[Token], parsedSynElems: List[SyntaxElement], mapFkt: MapFkt,
                              mapDepL: MapDepL, spanList: BracesSpan)

  implicit class ParseStatePipe(val ps: Either[ParseState, ParseErrorOrState]) extends AnyVal {
    def |>(f: ParseState => Either[ParseState, ParseErrorOrState]): Either[ParseState, ParseErrorOrState] = {
      println("|> : " + ps)
      ps match {
        case Left(p) => f(p)
        case Right(e) => Right(e)
      }
    }
  }

  implicit class ParseStateElse(val leftF: ParseState => Either[ParseState, ParseErrorOrState]) extends AnyVal {
    def ||(
            rightF: ParseState => Either[ParseState, ParseErrorOrState]
          ): ParseState => Either[ParseState, ParseErrorOrState] = {
      ps =>
        leftF(ps) match {
          case Right(_) => {
            println("|| : " + ps)
            rightF(ps)
          }
          case Left(resPs) => Left(resPs)
        }
    }
  }


  //_________________________________________________________Lambda


  def parseBackslash(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, mapFkt, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Backlash: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    val spanL = nextToken match {
      case Backslash(span) => spanList match {
        case None => span :: Nil
        case Some(l) => span :: l
      }
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, mapFkt, mapDepL, Some(spanL)))
  }


  def matchPrimitiveOrIdentifier(name: String, span: Span): SyntaxElement = {
    require(name.matches("[a-z][a-zA-Z0-9_]*"), "'" + name + "' has not the preffered structure")
    name match {
      //openCL/primitives
      case "mapGlobal" => SIntToExpr(AltMapGlobal(), span)
      case "mapLocal" => SIntToExpr(AltMapLocal(), span)
      case "mapWorkGroup" => SIntToExpr(AltMapWorkGroup(), span)
      case "oclReduceSeq" => SExpr(op.oclReduceSeq(Some(span)))
      case "oclReduceSeqUnroll" => SExpr(op.oclReduceSeqUnroll(Some(span)))
      case "oclIterate" => SExpr(op.oclIterate(Some(span)))
      case "oclCircularBuffer" => SExpr(op.oclCircularBuffer(Some(span)))
      case "oclRotateValues" => SExpr(op.oclRotateValues(Some(span)))

      //openCL/TypedDSL //Todo: not sure if this with .toExpr is working. Test with nBody
      case "toGlobal" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Global
      )(rt.TypePlaceholder, Some(span)))
      case "toLocal" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Local
      )(rt.TypePlaceholder, Some(span)))
      case "toPrivate" => SExpr(r.DepApp[rt.AddressSpaceKind](op.oclToMem(Some(span)),
        rt.AddressSpace.Private
      )(rt.TypePlaceholder, Some(span)))

      //core/primitives
      case "makeArray" => SIntToExpr(AltMakeArray(), span)
      case "cast" => SExpr(rp.cast(Some(span)))
      case "depJoin" => SExpr(rp.depJoin(Some(span)))
      case "depMapSeq" => SExpr(rp.depMapSeq(Some(span)))
      case "depZip" => SExpr(rp.depZip(Some(span)))
      case "drop" => SExpr(rp.drop(Some(span)))
      case "fst" => SExpr(rp.fst(Some(span)))
      case "gather" => SExpr(rp.gather(Some(span)))
      case "generate" => SExpr(rp.generate(Some(span)))
      case "idx" => SExpr(rp.idx(Some(span)))
      case "id" => SExpr(rp.id(Some(span)))
      case "indexAsNat" => SExpr(rp.indexAsNat(Some(span)))
      case "iterate" => SExpr(rp.iterate(Some(span)))
      case "join" => SExpr(rp.join(Some(span)))
      case "concat" => SExpr(rp.concat(Some(span)))
      case "let" => SExpr(rp.let(Some(span)))
      case "map" => SExpr(rp.map(Some(span)))
      case "mapFst" => SExpr(rp.mapFst(Some(span)))
      case "mapSnd" => SExpr(rp.mapSnd(Some(span)))
      case "mapSeq" => SExpr(rp.mapSeq(Some(span)))
      case "mapStream" => SExpr(rp.mapStream(Some(span)))
      case "iterateStream" => SExpr(rp.iterateStream(Some(span)))
      case "mapSeqUnroll" => SExpr(rp.mapSeqUnroll(Some(span)))
      case "toMem" => SExpr(rp.toMem(Some(span)))
      case "natAsIndex" => SExpr(rp.natAsIndex(Some(span)))
      case "padEmpty" => SExpr(rp.padEmpty(Some(span)))
      case "padClamp" => SExpr(rp.padClamp(Some(span)))
      case "partition" => SExpr(rp.partition(Some(span)))
      case "makePair" => SExpr(rp.makePair(Some(span)))
      case "reduce" => SExpr(rp.reduce(Some(span)))
      case "reduceSeq" => SExpr(rp.reduceSeq(Some(span)))
      case "reduceSeqUnroll" => SExpr(rp.reduceSeqUnroll(Some(span)))
      case "reorder" => SExpr(rp.reorder(Some(span)))
      case "scanSeq" => SExpr(rp.scanSeq(Some(span)))
      case "slide" => SExpr(rp.slide(Some(span)))
      case "circularBuffer" => SExpr(rp.circularBuffer(Some(span)))
      case "rotateValues" => SExpr(rp.rotateValues(Some(span)))
      case "snd" => SExpr(rp.snd(Some(span)))
      case "split" => SExpr(rp.split(Some(span)))
      case "take" => SExpr(rp.take(Some(span)))
      case "transpose" => SExpr(rp.transpose(Some(span)))
      case "select" => SExpr(rp.select(Some(span)))
      case "zip" => SExpr(rp.zip(Some(span)))
      case "neg" => SExpr(rp.neg(Some(span)))
      case "not" => SExpr(rp.not(Some(span)))
      case "add" => SExpr(rp.add(Some(span)))
      case "sub" => SExpr(rp.sub(Some(span)))
      case "mul" => SExpr(rp.mul(Some(span)))
      case "div" => SExpr(rp.div(Some(span)))
      case "mod" => SExpr(rp.mod(Some(span)))
      case "gt" => SExpr(rp.gt(Some(span)))
      case "lt" => SExpr(rp.lt(Some(span)))
      case "equal" => SExpr(rp.equal(Some(span)))
      case "asVectorAligned" => SExpr(rp.asVectorAligned(Some(span)))
      case "asVector" => SExpr(rp.asVector(Some(span)))
      case "asScalar" => SExpr(rp.asScalar(Some(span)))
      case "vectorFromScalar" => SExpr(rp.vectorFromScalar(Some(span)))
      case "printType" => SExpr(rp.printType("", Some(span)).primitive) //Todo: I was forced to delete span in printType and typeHole because of the error with wrong number of arguments
      case "typeHole" => SExpr(rp.typeHole("", Some(span)).primitive)
      case _ => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
    }
  }

  private def getIdInIdentNoDec(map:MapFkt, name:String, span: Span):SyntaxElement={
    val id = map.get(name) match {
      case None => throw new IllegalArgumentException("Variable is not declared: " + name)
      case Some(Right(_)) => throw new IllegalArgumentException("Variable is only declared but has no definition: " + name) //Todo: Proper Error for Function has declaration but not definition yet
      case Some(Left(rd.ToBeTyped(e))) => e match {
        case r.Identifier(_) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
        case _ => SExpr(e)
      }
    }
    id
  }

  private def getIdInIdentWithDec(map:MapFkt, name:String, span: Span):SyntaxElement={
    val id = map.get(name) match {
      case None => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
      case Some(Right(_)) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span))) //throw new IllegalArgumentException("Variable is only declared but has no definition: "+ name)//Todo: Proper Error for Function has declaration but not definition yet
      case Some(Left(rd.ToBeTyped(e))) => e match {
        case r.Identifier(_) => SExpr(r.Identifier(name)(rt.TypePlaceholder, Some(span)))
        case _ => throw new IllegalArgumentException("The name already exists with an definition: " + name)
      }
    }
    id
  }

  def parseIdent(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIdent: " + parseState)
    parseIdentPos(parseState, true)
  }

  def parseIdentNoDec(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIdentNoDec: " + parseState)
    parseIdentPos(parseState, false)
  }

  def parseIdentPos(parseState: ParseState, withDec: Boolean): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Identifier(name, span) => {
        matchPrimitiveOrIdentifier(name, span) match {
          case SIntToExpr(name, span) => Left(ParseState(remainderTokens, SIntToExpr(name, span) :: parsedSynElems, map,
            mapDepL, spanList))
          case SExpr(r.Identifier(_))=>{
            val id = if(withDec) getIdInIdentWithDec(map, name, span) else getIdInIdentNoDec(map, name, span)
            Left(ParseState(remainderTokens, id :: parsedSynElems, map, mapDepL, spanList))
          }
          case SExpr(prim) => {
            if(prim.span.isEmpty){
              throw new IllegalStateException("The span of '" + prim +"' is empty")
            }
            Left(ParseState(remainderTokens, SExpr(prim) :: parsedSynElems, map, mapDepL, spanList))
          }
          case otherSyntaxElement => throw new IllegalStateException("The Syntax Element '" + otherSyntaxElement +
            "' was not expected from matchPrimitiveOrIdentifer")
        }
      }
      case tok => {
        println("Abbruch parseIdent: " + tok + " : " + parseState)
        Right(ParseError("failed to parse Ident: " + tok + " is not an Identifier"))
      }
    }
  }

  def parseTypeIdentToCorrectForm(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeIdentToCorrectForm: "+ parseState)
    parseState.tokenStream.head match {
      case TypeIdentifier(name, span) => parseState.mapDepL.get(name) match {
        case None =>  Right(ParseError("It exists no DepLambda with this Name: "+ name))
        case Some(RNat()) =>
          Left(ParseState(parseState.tokenStream.tail, SNat(NIdentifier(
            rt.NatIdentifier(name)), span) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL, parseState.spanList))
        case Some(RData()) =>
          Left(ParseState(parseState.tokenStream.tail, SData(DIdentifier(
            rt.DataTypeIdentifier(name)), span) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL, parseState.spanList))
        case Some(RAddrSpace()) =>throw new IllegalStateException("DepAddrSpace is not implemented yet")
      }
      case t => Right(ParseError("Not an TypeIdentifier: "+ t))
    }
  }

  def parseTypeIdent(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeIdent: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse Ident: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case TypeIdentifier(name, spanId) => Left(ParseState(remainderTokens, SType(rt.TypeIdentifier(name), spanId) :: parsedSynElems,
        map, mapDepL, spanList))
      case tok => {
        println("Abbruch parseTypeIdent: " + tok + " : " + parseState)
        Right(ParseError("failed to parse TypeIdent: " + tok + " is not an TypeIdentifier"))
      }
    }
  }

  private def getScalarType(typ: ConcreteType): Option[rt.DataType] = typ match {
    case ShortTyp() => Some(rt.i8)
    case IntTyp() => Some(rt.i32)
    case FloatTyp() => Some(rt.f32)
    case DoubleType() => Some(rt.f64)
    case BoolType() => Some(rt.bool)
    case notAtype => {
      println("                        This is not an accepted Type: "+ notAtype)
      None
    }
  }

  def parseMaybeTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList) = parseState
    val colonToken :: typeTokenWithremainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        parseType(ParseState(typeTokenWithremainderTokens, Nil, map, mapDepL, spanList)) match {
          case Right(e) => Right(e)
          case Left(newPS) => if (newPS.parsedSynElems.length == 1) {
            return Left(ParseState(newPS.tokenStream, newPS.parsedSynElems.head :: parsedSynElems, newPS.mapFkt,
              newPS.mapDepL, spanList))
          } else {
            throw new IllegalStateException(
              "It should only be one argument in the end of the computing of the Type exist")
          }
        }
      }
      case _ => Left(parseState)
    }
  }

  def parseTypeAnnotation(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList) = parseState
    val colonToken :: typeToken :: remainderTokens = tokens

    colonToken match {
      case Colon(_) => {
        //if a type Annotation exist, we set the type new of the Identifier
        typeToken match {
          case ScalarType(typ, sp) =>  {
            //Todo: Complex Alg for Types Parsing
            val t = getScalarType(typ)
            t match {
              case None => Right(ParseError("failed to parse ScalarType: " + typ + " is not an accpeted ScalarType"))
              case Some(parsedType) => Left(ParseState(remainderTokens, SType(parsedType, sp)::parseState.parsedSynElems,
                parseState.mapFkt, mapDepL, spanList))
            }
          }
          case notAtype => Right(ParseError("failed to parse Type: " + notAtype + " is not an ScalarType"))
        }
      }
      case notAColon => Right(ParseError("failed to parse Type: A TypeAnnotation is expected, but " + notAColon +
        " is not an Colon"))
    }
  }

  def parseVecType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList) = parseState
    val inputType :: remainderTokens = tokens

    println("parseVecType: " + parseState)
    inputType match {
      case VectorType(len, concreteType, sp) => {
        println("ConcreteType was in parseVecType: " + concreteType + " , with length: " + len)
        //Todo: Complex Alg for Type Parsing
        val parsedInType = getScalarType(concreteType)
        val inT = parsedInType.getOrElse(return Right(ParseError("IllegalInputScalaType in VecType")))
        Left(ParseState(remainderTokens, SType(rt.VectorType(len, inT), sp):: parseState.parsedSynElems, parseState.mapFkt,
          mapDepL, spanList) )
      }
      case notAtype => Right(ParseError("failed to parse VecType: " + notAtype + " is not correct Type"))
    }
  }

  def parseScalarType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
      val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
      val inputType :: remainderTokens = tokens

      println("parseTypeWithoutArrow: " + parseState)
          inputType match {
            case ScalarType(typ, spanTyp) => {
              println("Type was in parseTypeWithoutArrow parsed: " + typ +" ; " + spanTyp.end)
              //Todo: Complex Alg for Type Parsing
              val parsedInType = getScalarType(typ)
              val inT = parsedInType.getOrElse(return Right(ParseError("IllegalInputScalaType")))
              Left(ParseState(remainderTokens, SType(inT, spanTyp):: parseState.parsedSynElems, parseState.mapFkt, mapDepL, spanList) )
            }
            case notAtype => Right(ParseError("failed to parse ScalarType (in parseScalarType): " + notAtype +
              " is not an Type"))
          }
  }

  def parseKindWithDepArrowAfterForDepLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseKind: " + parseState)
    val (nameOfIdentifier, spanType, spanList) = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name), sp) => parseState.spanList match {
        case Some(Nil) => throw new IllegalArgumentException("this should not happen to be Nil")
          case None => throw new IllegalArgumentException("this should not happen to be None")
          case Some(span::Nil) => (name, sp+span, None)
          case Some(span::l)=> (name, sp+span, Some(l))
        }
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'")
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
    tokens.head match {
      case Kind(concreteKind, _) => tokens.tail.head match{
        case DepArrow(_) =>       {
          println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseMaybeAppExpr(ParseState(tokens.tail.tail, Nil, parseState.mapFkt, parseState.mapDepL, spanList) ) match {
            case Right(e) => Right(e)
            case Left(pS) => {
              println("In the middle of parseDepFunctionType: " + pS)
              if (pS.parsedSynElems.tail.nonEmpty) return Right(ParseError("ParsedSynElems.tail has to be empty!"))
              val depLam:SExpr = pS.parsedSynElems.head match {
                case SExpr(outT) => {
                  val span = outT.span match {
                    case None => throw new IllegalStateException("Span should not be None in DepLambdafkt")
                    case Some(sp) => {
                      val spanZW = spanType + sp
                      println("Span of spanType123: ("+ spanZW.begin + ","+spanZW.end+ ") = (" + sp.begin + ","+sp.end+ ") + ("+ spanType.begin + ")"+ spanType.end+ ") ; " + nameOfIdentifier)
                      spanZW
                    }
                  }
                  concreteKind match { //Todo: einfach Span direkt reingeben!!! Auch bei Lambda DepLambda etc.
                    case Data() => SExpr(r.DepLambda[rt.DataKind](rt.DataTypeIdentifier(nameOfIdentifier),
                      outT)(rt.TypePlaceholder, Some(span)))
                    case Nat() => SExpr(r.DepLambda[rt.NatKind](rt.NatIdentifier(nameOfIdentifier),
                      outT)(rt.TypePlaceholder, Some(span)))
                    case AddrSpace() => SExpr(r.DepLambda[rt.AddressSpaceKind](
                      rt.AddressSpaceIdentifier(nameOfIdentifier), outT)(rt.TypePlaceholder, Some(span)))
                    case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
                  }
                }
                case _ => return Right(ParseError("Not a Type"))
              }
              Left(ParseState(pS.tokenStream, depLam:: newPS, pS.mapFkt, pS.mapDepL, spanList) )
            }
          }
        }
        case notAnDepArrow => Right(ParseError("failed to parse DepArrow: " + notAnDepArrow + " is not an DepArrow"))
      }
      case Arrow(_)=> Right(ParseError("DepArrow and not Arrow was expected"))
      case t => Right(ParseError("DepArrow and not "+ t + " was expected"))
    }
  }




  def parseKindWithDepArrowAfterForDepFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseKind: " + parseState)
    val nameOfIdentifier:String = parseState.parsedSynElems.head match {
      case SType(rt.TypeIdentifier(name), _) => name
      case t => throw new IllegalStateException("Here should be an TypeIdentifier and not '" + t + "'")
    }
    val newPS = parseState.parsedSynElems.tail

    val tokens = parseState.tokenStream
     tokens.head match {
      case Kind(concreteKind, span) => tokens.tail.head match{
        case DepArrow(_) =>       {
          if(parseState.mapDepL.contains(nameOfIdentifier)){
            throw new IllegalArgumentException("It exists already an DepLambda with this Name: "+ nameOfIdentifier)
          }else if(parseState.mapFkt.contains(nameOfIdentifier)){
            throw new IllegalArgumentException("It exists already an Fkt with this Name: "+ nameOfIdentifier)
          }
          concreteKind match {
            case Data() => parseState.mapDepL.update(nameOfIdentifier, RData())
            case Nat() => parseState.mapDepL.update(nameOfIdentifier, RNat())
            case AddrSpace() => parseState.mapDepL.update(nameOfIdentifier, RAddrSpace())
            case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
          }
          println("Kind was in parseDepFunctionType parsed: " + concreteKind)
          parseType(ParseState(tokens.tail.tail, Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) ) match {
            case Right(e) => Right(e)
            case Left(pS) => {
              if (pS.parsedSynElems.tail.nonEmpty) return Right(ParseError("ParsedSynElems.tail has to be empty!"))
              val depFun:SType = pS.parsedSynElems.head match {
                case SType(outT, sp) => {
                  concreteKind match {
                    case Data() => SType(rt.DepFunType[rt.DataKind, rt.Type](
                      rt.DataTypeIdentifier(nameOfIdentifier), outT), sp)
                    case Nat() => SType(rt.DepFunType[rt.NatKind, rt.Type](
                      rt.NatIdentifier(nameOfIdentifier), outT), sp)
                    case AddrSpace() => SType(rt.DepFunType[rt.AddressSpaceKind, rt.Type](
                      rt.AddressSpaceIdentifier(nameOfIdentifier), outT), sp)
                    case ki => return Right(ParseError("Not an accepted Kind: "+ ki))
                  }
                }
                case _ => return Right(ParseError("Not a Type"))
              }
              Left(ParseState(pS.tokenStream, depFun:: newPS, pS.mapFkt, pS.mapDepL, pS.spanList) )
            }
          }
        }
        case notAtype => Right(ParseError("failed to parse Type (in DepFunction): " + notAtype + " is not an Type"))
      }
      case Arrow(_)=> Right(ParseError("DepArrow and not Arrow was expected"))
      case t => Right(ParseError("DepArrow and not "+ t + " was expected"))
      }
  }

  def parseDepFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    if( tokens.length < 3){
      return Right(ParseError("only "+ tokens.length + " arguments are in the TokenList, but we need minimum 3!"))
    }

    val psOld =
      Left(parseState) |>
        parseTypeIdent     |>
        parseColon |>
        parseKindWithDepArrowAfterForDepFunctionType
    psOld
  }

  def parseDepLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseDepFunctionType: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    if( tokens.length < 3){
      return Right(ParseError("only "+ tokens.length + " arguments are in the TokenList, but we need minimum 3!"))
    }

    val psOld =
      Left(parseState) |>
        parseBackslash |>
        parseTypeIdent     |>
        parseColon |>
        parseKindWithDepArrowAfterForDepLambda
    psOld
  }

  def parseArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Arrow(_) =>
      case tok => Right(ParseError("failed to parse Arrow: " + tok + " is not an Arrow"))
    }

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL, spanList) )
  }

  private def combineTypes(typesList: List[r.types.Type]) : r.types.Type = {
    if(typesList.isEmpty){
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    if(typesList.tail.isEmpty){
      typesList.head
    }else{
      rt.FunType(typesList.head, combineTypes(typesList.tail))
    }
  }

  private def combineSynElemList(leftSynElemList: List[SyntaxElement], rightSynElemList: List[SyntaxElement]) :
  List[SyntaxElement] = {
    var lS = leftSynElemList
    var rS = rightSynElemList
    var l:List[SyntaxElement] = Nil
    while(rS.nonEmpty){
      l = rS.head::l
      rS = rS.tail
    }
    while(lS.nonEmpty){
      l = lS.head::l
      lS = lS.tail
    }
    l
  }

  private def createSIntToExpr(name:SIntToExprAlternatives, n:Int, span:Span) : r.Expr = {
    val expr = name match {
      case AltMapGlobal() => op.mapGlobal(n, Some(span)).toExpr
      case AltMapLocal() => op.mapLocal(n,Some(span)).toExpr
      case AltMapWorkGroup() => op.mapWorkGroup(n, Some(span)).toExpr
      case AltMakeArray() => rp.makeArray(n,Some(span)).toExpr
    }
    expr
  }

  private def combineExpressionDependentFirsExpr(synElemList: List[SyntaxElement], mapDepL: MapDepL) : (r.Expr, List[SyntaxElement]) = {
    if(synElemList.isEmpty){
      throw new IllegalArgumentException("the ElemList is empty!")
    }
    var synE = synElemList.reverse
    var e:r.Expr = synE.head match {
      case SExprClutched(expr, spanClutch) => {
        synE = synE.tail
        if(synE.isEmpty){
          expr
        }else{
          val (exprComb, l) = combineExpressionsDependentOneStep(expr, synE,mapDepL,Some(spanClutch))
          synE = l
          exprComb
        }
      }
      case SExpr(expr) => {
        if(expr.span.isEmpty){
          throw new IllegalStateException("Span of the first Expr '"+ expr+ "' is None in combineExpressionsDependent")
        }
        synE = synE.tail
        expr
      }
      case SIntToExpr(name, span) => {
        if(synE.tail.isEmpty){
          throw new IllegalStateException("For this Primitive '" + name +"' we expect to see an lenght in Int")
        }
        val (n, spanOfN) = synE.tail.head match{
          case SExpr(r.Literal(rS.IntData(len), spanOfLen)) => (len, spanOfLen)
          case _ => throw new IllegalStateException("For this Primitive '" + name +"' we expect to see an lenght in Int")
        }
        synE = synE.tail.tail
        spanOfN match {
          case None => throw new IllegalStateException("Span of N should not be None")
          case Some(spa2) => createSIntToExpr(name, n, span+spa2)
        }
      }
      case SAddrSpace(addrSpace,_) => throw new RuntimeException(
        "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
      case SType(t, _) => throw new RuntimeException("List should't have Types at this beginning position! " + t)
      case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
      case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
    }
    (e,synE)
  }

  private def combineExpressionsDependentOneStep(exp: r.Expr, synElemList: List[SyntaxElement],
                                  mapDepL: MapDepL, spanOfClutchedExprBefore: Option[Span]): (r.Expr, List[SyntaxElement]) = {
    var synE = synElemList
    var e = exp

    val span_e = if(spanOfClutchedExprBefore==None) {
      e.span match {
        case Some(span) => span
        case None => throw new IllegalStateException("Span of combined Expr is None in combineExpressionsDependent: "+ e)
      }
    }else{
      spanOfClutchedExprBefore.get
    }

    synE.head match {
      case SExprClutched(expr1, spanClutch) => {
        val span = span_e + spanClutch
        println("\n\nspan in SExprClutched: "+ span)
        e = r.App(e, expr1)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SExpr(expr1) => {
        val span_expr1 = expr1.span match {
          case Some(span) => span
          case None => throw new IllegalStateException("Span of the next Expr '"+ expr1+ "'is None in combineExpressionsDependent")
        }
        val span = span_e + span_expr1
        e = r.App(e, expr1)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SData(DIdentifier(data @ rt.DataTypeIdentifier(name, _)), dataSpan) => {
        val span = dataSpan+span_e

        mapDepL.get(name) match {
          case None => {
            //Todo: Bessere Fehlermeldung!!!
            throw new IllegalArgumentException("The DataTypeIdentifier '"+name+"' is unknown!")
          }
          case Some(k)=> k match {
            case RData() => e = r.DepApp[rt.DataKind](e, rt.DataTypeIdentifier(name,true))(rt.TypePlaceholder, Some(span))
            case RNat() => e = r.DepApp[rt.NatKind](e, rt.NatIdentifier(name,true))(rt.TypePlaceholder, Some(span))
            case RAddrSpace() => e = r.DepApp[rt.AddressSpaceKind](e,
              rt.AddressSpaceIdentifier(name,true))(rt.TypePlaceholder, Some(span))
          }
        }
        synE = synE.tail
      }
      case SIntToExpr(name, span1) => {
        if(synE.tail.isEmpty){
          throw new IllegalStateException("For this Primitive '" + name +"' we expect to see an lenght in Int")
        }
        val (n, spanOfN) = synE.tail.head match{
          case SExpr(r.Literal(rS.IntData(len), spanInt)) => spanInt match {
            case None => throw new IllegalStateException("Span should not be None")
            case Some(spanI) => (len, spanI)
          }
          case _ => throw new IllegalStateException("For this Primitive '" + name +"' we expect to see an lenght in Int")
        }
        val span = span1 + spanOfN + span_e
        synE = synE.tail.tail
        createSIntToExpr(name, n, span)
      }
      case SAddrSpace(addrSpace, spanAddr) => {
        val span = span_e
        e= r.DepApp[rt.AddressSpaceKind](e,addrSpace)(rt.TypePlaceholder, Some(span))
        synE = synE.tail
      }
      case SType(t, spanType) => {
        val span = span_e + spanType
//        println("spanType: "+ spanType.end + " of Type: " + t)
        if(t.isInstanceOf[rt.DataTypeIdentifier]){
          t match {
            case rt.DataTypeIdentifier(name,_) => mapDepL.get(name) match {
              case None => {
                //Todo: Bessere Fehlermeldung!!!
                throw new IllegalArgumentException("The DataTypeIdentifier '"+name+"' is unknown!")
              }
              case Some(k)=> {
//                print("Type: "+ t + "; "+ span.end)
                k match {
                  case RData() => e = r.DepApp[rt.DataKind](e, rt.DataTypeIdentifier(name,true))(rt.TypePlaceholder, Some(span))
                  case RNat() => e = r.DepApp[rt.NatKind](e, rt.NatIdentifier(name,true))(rt.TypePlaceholder, Some(span))
                  case RAddrSpace() => e = r.DepApp[rt.AddressSpaceKind](e,
                    rt.AddressSpaceIdentifier(name,true))(rt.TypePlaceholder, Some(span))
                }
              }
            }
            case _ => throw new IllegalStateException(
              "This should not be happening in the combining of the Dependent Expressions")
          }
        }else{
          e = r.DepApp[rt.TypeKind](e, t)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
      case SData(dataElem, dataSpan) => {
        val span = span_e
        dataElem match {
          case DIdentifier(data) => e= r.DepApp[rt.DataKind](e,data)(rt.TypePlaceholder, Some(span))
          case DType(data) => e= r.DepApp[rt.DataKind](e,data)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
      case SNat(natElem, spanNat) => {
        val span = span_e
        natElem match {
          case NIdentifier(nat) => e= r.DepApp[rt.NatKind](e,nat)(rt.TypePlaceholder, Some(span))
          case NNumber(nat) => e= r.DepApp[rt.NatKind](e,nat)(rt.TypePlaceholder, Some(span))
        }
        synE = synE.tail
      }
    }

    (e,synE)
  }

  private def combineExpressionsDependent(synElemList: List[SyntaxElement], mapDepL: MapDepL) : r.Expr = {
    var (e, synE) = combineExpressionDependentFirsExpr(synElemList, mapDepL)
    println("I will combine Expressions in Lambda: "+ synE + " <::> " + e)
    while(!synE.isEmpty){
      val r = combineExpressionsDependentOneStep(e, synE, mapDepL, None)
      e = r._1
      synE = r._2
    }
    println("I have combined the Expressions in Lambda: "+ e + " ; " + e.span.get.end)
    e
  }

  def parseTypAnnotatedIdentAndThenNamedExprAndOtherTypAnnotatedIdens(parseState:ParseState):
  Either[MapFkt, ParseErrorOrState] = {
    if(parseState.tokenStream.isEmpty){
      throw new IllegalArgumentException("TokenStream is empty")
    }
    if(!parseState.parsedSynElems.isEmpty){
      throw new IllegalArgumentException("parsedSynElemnts has to be empty: " + parseState.parsedSynElems)
    }
    if(!parseState.mapFkt.isEmpty){
      throw new IllegalArgumentException("map has to be empty: " + parseState.mapFkt)
    }

    var (t, map): (List[Token], MapFkt) = parseState.tokenStream match {
      case BeginTypAnnotatedIdent(_) :: remainderTokens => {
        val ps:ParseState = ParseState(remainderTokens, Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList)
        val psNew = parseTypAnnotatedIdent(ps)
        psNew match {
          case Left((tokens, m)) => if(!tokens.isEmpty){
            (tokens, m)
          }else{
            throw new IllegalStateException("We need an NamedExpr too, because of that it " +
              "should not be possible only to have an TypAnnotatedIdent")
          }
          case Right(e) => return Right(e)
        }
      }
      case BeginNamedExpr(_):: remainderTokens => {
        throw new IllegalArgumentException("You aren't allowed to start with an NamedExpr")
      }
      case a => throw new IllegalArgumentException("You have started with something different: " + a)
    }

    while(!t.isEmpty) {
      println("tokens: " + t + " ,MapFkt: " + map)
       t match {
        case BeginTypAnnotatedIdent(_) :: remainderTokens => {
          val p:ParseState = ParseState(remainderTokens, Nil, map, parseState.mapDepL, parseState.spanList)
          val psNew = parseTypAnnotatedIdent(p)
          psNew match {
            case Left((tokens, m)) => {
              t = tokens
              map = m
            }
            case Right(e) => return Right(e)
          }
        }
        case BeginNamedExpr(_):: remainderTokens => {
          val p:ParseState = ParseState(remainderTokens, Nil, map, parseState.mapDepL, parseState.spanList)
          println("p: " + p)
          val psNew = parseNamedExpr(p)
          println("psNew: " + psNew)
          psNew match {
            case Left((tokens, m)) => {
              t = tokens
              map = m
            }
            case Right(e) => return Right(e)
          }
        }
        case a => throw new IllegalArgumentException("You have started with something different: " + a)
      }
    }

    Left(map)
  }

  /*
  top level Lambda expects that the type of the Identifier is defined!

  only use as top level!
   */
  def parseNamedExpr(parseState: ParseState): Either[(List[Token], MapFkt) , ParseErrorOrState] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambdaOld =
      Left(parseState)      |>
        parseIdent

    val (ps, identifierFkt, typeOfFkt) : (ParseState, r.Identifier, r.types.Type) = psLambdaOld match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(id @ r.Identifier(n))=>
            p.mapFkt.get(n) match {
              case None => {
                println("Identifier doesn't exist: " + n + " , " + psLambdaOld)
                throw new IllegalStateException("We want to parse an NamedExpr for " + n +
                  " but this Identifier is not declared yet!")
              }
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(typeFkt)) =>
                (ParseState(p.tokenStream, parseState.parsedSynElems, p.mapFkt, p.mapDepL, p.spanList) ,
                  id.setType(typeFkt), typeFkt)
            }
          case SExprClutched(expr, spanClutch) =>
            throw new IllegalStateException("it is an Identifier expected not expr with spanClutch: "+ expr +" ; " + spanClutch)
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t, _) => throw new IllegalStateException(
            "it is an Identifier expected but an Type is completely false: "+ t)
          case SIntToExpr(prim, _) => throw new IllegalStateException("it is an Identifier expected: "+ prim)
          case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t, _) => throw new RuntimeException("List should't have any Nats at this position! " + t)
          case SAddrSpace(addrSpace,_) => throw new RuntimeException(
            "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
        }
      }
    }

    val psNamedExprBefore = {
        Left(ps)        |>
        parseEqualsSign |>
        parseMaybeAppExpr
    }



    val psNamedExpr = psNamedExprBefore match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.tokenStream match {

          case EndNamedExpr(_) :: remainderTokens => {
            val m = p.mapFkt
            m.get(identifierFkt.name) match {
              case None => throw new IllegalStateException("Identifier seems not to be in the Map: " +
                identifierFkt.name + " , " + m)
              case Some(Left(e)) => throw new IllegalStateException("The Lambda-Fkt should't be initiated yet!: "+ e)
              case Some(Right(l)) =>
                //Todo: I have to add Types in the Identifiers in Lambda and delete it after it, after this this if-clause makes sense
//                if(!l.isEmpty){
//                throw new IllegalStateException("The List should be empty! But it isn't. " +
//                  "Probably we have one or more Types in the " +
//                  "TypAnnotationIdent declared than the NamedExpr really has. Types left: " + l + "\nTypes defined: " + typesDefined + "\nNamedExpr: " + p.parsedSynElems)
//              }else{
                //Todo: We have to give the Identifier (identifierFkt/p.map.get(n)) now a Type

                (remainderTokens, p.parsedSynElems, m, p.mapDepL)
//              }
            }
          }
          case _ => {
            throw new IllegalStateException("NewExpr ends with an EndNamedExpr, but we have no EndNamedExpr at the end")
          }
        }
      }
    }

        val synElemList = psNamedExpr._2
        val mapDepL = psNamedExpr._4
        println("\n\n\n Before combining the Expr in parseNamedExpr \n\n\n")
        var expr = combineExpressionsDependent(synElemList, mapDepL)
        expr = expr.setType(typeOfFkt)

        println("expr finished: " + expr + " with type: " + expr.t + "   (should have Type: " + typeOfFkt + " ) ")
        val m = psNamedExpr._3

        require(expr.span!= None, "expr is None!")
        rd.ToBeTyped(expr) match{
          case rd.ToBeTyped(e) => require(e.span!= None, "expr is now in ToBeTyped without infer None")
          case _ => throw new IllegalStateException("this should not be happening")
        }
        require(rd.ToBeTyped(expr).toExpr.span!= None, "expr is now with ToBeType.toExpr None!")

        m.update(identifierFkt.name, Left(rd.ToBeTyped(expr)))
        println("map updated: " + m + "\nRemainderTokens: " + psNamedExpr._1)
        Left((psNamedExpr._1, m))
      }

  def parseTypAnnotatedIdent(parseState: ParseState): Either[(List[Token], MapFkt) , ParseErrorOrState] = {
    require(parseState.parsedSynElems.isEmpty, "parseState is not empty, but nothing should be in it yet")
    val psLambdaOld =
      Left(parseState)      |>
        parseIdent

    val (ps, identifierFkt) : (ParseState, r.Identifier) = psLambdaOld match {
      case Right(e) => return Right(e)
      case Left(p) => {
        p.parsedSynElems.head match {
          case SExpr(id @ r.Identifier(n))=> if(p.mapFkt.contains(n)){
            println("Identifier does already exist: " + n + " , " + psLambdaOld)
            throw new IllegalStateException("We want to parse an TypAnnotatedIdent for " + n
              + " but this Identifier is already declared!")
          }else if(p.mapDepL.contains(n)){
            throw new IllegalArgumentException("We want to parse an TypAnnotatedIdent for " + n
              + " but it exists already an DepLambda with this Name")
          }else{
            (p, id)
          }
          case SExprClutched(expr, spanClutch) =>
            throw new IllegalStateException("it is an Identifier expected not expr with spanClutch: "+ expr +" ; " + spanClutch)
          case SExpr(expr) => throw new IllegalStateException("it is an Identifier expected: "+ expr)
          case SType(t, _) => throw new IllegalStateException(
            "it is an Identifier expected but an Type is completely false: "+ t)
          case SIntToExpr(prim, _) => throw new IllegalStateException("it is an Identifier expected: "+ prim)
          case SData(t, _) => throw new RuntimeException("List should't have any Data at this position! " + t)
          case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
          case SAddrSpace(addrSpace,_) => throw new RuntimeException(
            "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
        }
      }
    }

    val psNamedExprBefore = {
      Left(ParseState(ps.tokenStream, Nil, ps.mapFkt, ps.mapDepL, ps.spanList) )        |>
        parseDoubleColons |>
        parseType
    }


    val psNew = psNamedExprBefore match {
      case Right(e) => return Right(e)
      case Left(p) => p
    }

    println("in the middle of TypAnnotatedIden: "+ psNew)
    psNew.tokenStream match {
      case EndTypAnnotatedIdent(_) :: remainderTokens => {
        val m = psNew.mapFkt
        val (typesList, _) = getTypesInList(psNew.parsedSynElems, None)
        if(typesList.length!=1){
          throw new IllegalStateException("The TypesList should have lenght 1: "+ typesList)
        }
        m.put(identifierFkt.name, Right(typesList.head))
        println("return TypAnnotatedIdent: "+ remainderTokens+ " <<<<>>>> " + m )
        Left((remainderTokens, m))
      }
      case EndNamedExpr(_) :: remainderTokens => {
        throw new IllegalStateException("TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but end with EndNamedExpr")
      }
      case a => {
        throw new IllegalStateException(
          "TypAnnotatedIdent ends with an EndTypAnnotatedIdent, but we have no EndTypAnnotatedIdent at the end: " + a)
      }
    }
  }

  def getTypesInList(synElems: List[SyntaxElement], spanOp:Option[Span]): (List[r.types.Type], Span)= {
    if( !synElems.isEmpty){
      synElems.head match {
        case SType(typ, sp) =>{
          val span = spanOp match{
            case None => sp
            case Some(s) => s + sp
          }
          val (l,newSpan) = getTypesInList(synElems.tail, Some(span))
          (typ :: l, newSpan)
        }
        case SExprClutched(expr, spanClutch) =>
          throw new IllegalStateException("in getTypesInList we have as head a not Type:"+ expr +" ; " + spanClutch)
        case SExpr(e) => throw new IllegalArgumentException("in getTypesInList we have as head a not Type: "+ e)
        case SIntToExpr(e, _) => throw new IllegalArgumentException(
          "in getTypesInList we have as head a not Type: "+ e)
        case SData(t,sp) =>{
          val span = spanOp match {
            case None => sp
            case Some(s) => s + sp
          }
          t match {
            case DIdentifier(data) =>{
              val (l,newSpan) = getTypesInList(synElems.tail, Some(span))
              (data :: l, newSpan)
            }
            case DType(data) => {
              val (l,newSpan) = getTypesInList(synElems.tail, Some(span))
              (data :: l, newSpan)
            }
          }
        }
        case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
        case SAddrSpace(addrSpace,_) => throw new RuntimeException(
          "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
      }
    }else{
      spanOp match {
        case None => throw new IllegalStateException("spanOp should not be None")
        case Some(sp) => (Nil,sp)
      }
    }
  }

  def parseType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseType: " + parseState)
    val ps: Either[ParseState, ParseErrorOrState] =
      Left(parseState) |>
        (parseDepOrNormalFunctionType _ || parseSimpleType )
    ps
  }

  def parseDepOrNormalFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseType: " + parseState)
    val ps: Either[ParseState, ParseErrorOrState] =
      Left(parseState) |>
        (parseDepFunctionType _ || parseFunctionType)
    ps
  }

  def parseSimpleType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseType: " + parseState)
    val ps: Either[ParseState, ParseErrorOrState] =
      Left(parseState) |>
        (parseBracesExprType _ ||
          parseTupleType || parseIndexType || parseArrayType || parseVecType ||
          parseScalarType || parseData ) //Todo: The Function parseTypeIdentToCorrectForm is not good, because it is not clear what we should parse. I have an Function for parseData, but I don't need a function for parseNat, because Nat should not be returned. For ArrayType i am also unsure.
    ps
  }

  def parseFunctionType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ps = parseSimpleType(parseState)
    ps match {
      case Right(e)=> Right(e)
      case Left(p)=> if(p.tokenStream.isEmpty){
        throw new IllegalStateException("Tokens are empty: "+ p)
      }else {
        p.tokenStream.head match {
          case Arrow(_) => {
            val newParse = parseFunctionType(ParseState(p.tokenStream.tail, Nil, p.mapFkt, p.mapDepL, p.spanList) ) match{
              case Left(pars) => pars
              case Right(e) => return Right(e)
            }
            val (list1, spanList1) = getTypesInList(p.parsedSynElems, None)
            val (list2, spanList2) = getTypesInList(newParse.parsedSynElems,None)
            val typesList:List[r.types.Type] = combineTypes(list1)::
              combineTypes(list2)::Nil
            val newType: r.types.Type = combineTypes(typesList)
            Left(ParseState(newParse.tokenStream, SType(newType, spanList1+spanList2)::Nil, newParse.mapFkt, p.mapDepL, p.spanList) )
          }
          //Todo: Maybe remove already here the EndTypAnnotatedIdent or RBrace from the TokenList
          case EndTypAnnotatedIdent(_) => Left(p)
          case RParentheses(_) => Left(p)
          case a => Right(ParseError("the Token '"+ a + "' is not here expected!!!"))
        }
      }
    }
  }

  /*
is the whole Syntax-Tree.
the syntax-Tree has on top an Lambda-Expression
 */
  def parseLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseLambda: "+ parseState)
      return Left(parseState)
    }
    println("parseLambda: " +parseState)
    val psOld =
      Left(ParseState(parseState.tokenStream, Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList)) |>
        parseBackslash |>
        parseIdent     |>
        parseMaybeTypeAnnotation |>
        parseArrow

    val ((psOrErr, spanBackslash),idName) = psOld match {
      case Left(p) => {
        val synElemListExpr = p.parsedSynElems
        val (maybeTypedIdent, synElemListMaybeTIdent):(r.Expr, List[SyntaxElement]) = {
          synElemListExpr.head match {
            case SType(t, _) =>
              synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
            case SExprClutched(_,_) => throw new IllegalStateException("SExprClutched is not expected here")
            case SExpr(i) => (i, synElemListExpr.tail)
            case SIntToExpr(prim, _) => throw new RuntimeException("Here is an Expression expected, but " + prim +
              " is not an Expression!")
            case SData(t,_) => t match {
              case DIdentifier(data) => synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
              case DType(data) => synElemListExpr.tail.head match {
                case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
                case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
              }
            }
            case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
            case SAddrSpace(addrSpace,_) => throw new RuntimeException(
              "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
          }
        }
        require(synElemListMaybeTIdent.isEmpty, "the List should be empty")
        val identifierName = maybeTypedIdent.asInstanceOf[r.Identifier]
        var map = p.mapFkt
        //local variables are in the list, so that not two same localVariables are declared
        if (map.contains(identifierName.name)) {//Todo: Better error
          throw new IllegalArgumentException("A variable or function with the exact same name '"+ identifierName.name +
            "' is already declared! <- " + map.get(identifierName.name))
        }
        map.update(identifierName.name, Left(rd.ToBeTyped(identifierName)))
        val ret = p.spanList match {
          case Some(Nil) => throw new IllegalArgumentException("this should not happen to be Nil")
          case None => throw new IllegalArgumentException("this should not happen to be None")
          case Some(span::Nil) => (parseMaybeAppExpr(ParseState(p.tokenStream,Nil, map, p.mapDepL, None)) , span )
          case Some(span::l)=> ( parseMaybeAppExpr(ParseState(p.tokenStream,Nil, map, p.mapDepL, Some(l)) ), span)
        }
        (ret, identifierName)
      }
      case Right(e) => {
        println("endLambda: "+ e)
        return Right(e)
      }
    }

    val (toks, expr, map, mapDepL, spanList) = psOrErr match {
      case Left(psNew) => {
        val expr = combineExpressionsDependent(psNew.parsedSynElems, psNew.mapDepL)
        //println("\n\n\nSpanIsHere"+ expr.expr +" : "+ expr.expr.span+ "\n\n\n")
        (psNew.tokenStream,expr, psNew.mapFkt, psNew.mapDepL, psNew.spanList)
      }
      case Right(e) => return Right(e)
    }
    val spanOfBackslash = parseState.tokenStream.head.s
    val span = Span(spanOfBackslash.file,spanOfBackslash.begin, expr.span.head.end)
    val lambda = Lambda(idName, expr)(rt.TypePlaceholder, Some(span))

    val myNewParseState = ParseState(toks, SExpr(lambda) :: parseState.parsedSynElems, map,mapDepL, spanList)
    println("myNewParseState: "+ myNewParseState)
    Left(myNewParseState)
  }
//  /*
//  old parseLambda Todo: delete it
//   */
//  def parseLambda(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
//    if(parseState.tokenStream.isEmpty){
//      println("Abbruch; parseLambda: "+ parseState)
//      return Left(parseState)
//    }
//    println("parseLambda: " +parseState)
//    val psOld =
//      Left(parseState) |>
//        parseBackslash |>
//        parseIdent     |>
//        parseMaybeTypeAnnotation |>
//        parseArrow
//
//        val (psOrErr, spanBackslash) = psOld match {
//          case Left(p) => p.spanList match {
//            case Some(Nil) => throw new IllegalArgumentException("this should not happen to be Nil")
//            case None => throw new IllegalArgumentException("this should not happen to be None")
//            case Some(span::Nil) => (parseMaybeAppExpr(ParseState(p.tokenStream,Nil, p.mapFkt, p.mapDepL, None)) , span )
//            case Some(span::l)=> ( parseMaybeAppExpr(ParseState(p.tokenStream,Nil, p.mapFkt, p.mapDepL, Some(l)) ), span)
//          }
//          case Right(e) => {
//            println("endLambda: "+ e)
//            return Right(e)
//          }
//        }
//
//    val (toks, synElemList, map, mapDepL, spanList) = psOrErr match {
//      case Left(psNew) => {
//        val expr = SExpr(combineExpressionsDependent(psNew.parsedSynElems, psNew.mapDepL))
//        //println("\n\n\nSpanIsHere"+ expr.expr +" : "+ expr.expr.span+ "\n\n\n")
//        val newL = expr :: Nil
//        val li:List[SyntaxElement] = psOld match {
//          case Left(pa) => pa.parsedSynElems.reverse ++ newL
//          case Right(_) => throw new RuntimeException(
//            "this should not be able to happen in parseLambdda, because I already have controlled this!")
//        }
//        val l = li.reverse
//        (psNew.tokenStream,l, psNew.mapFkt, psNew.mapDepL, psNew.spanList)
//      }
//      case Right(e) => return Right(e)
//    }
//
//    val (expr, synElemListExpr) = (synElemList.head match {
//      case SExpr(e) => e
//      case a => throw new RuntimeException("Here is an Expression expected, but " + a +" ist not an Expression!")
//    }, synElemList.tail)
//    println("now in Lambda we want to combine our results: "+ expr +" # " + synElemListExpr)
//
//    val (maybeTypedIdent, synElemListMaybeTIdent):(r.Expr, List[SyntaxElement]) =
//      synElemListExpr.head match {
//        case SType(t, _) =>
//          synElemListExpr.tail.head match {
//            case SExpr(i) => (i.setType(t), synElemListExpr.tail.tail)
//            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
//          }
//        case SExprClutched(_,_) => throw new IllegalStateException("SExprClutched is not expected here")
//        case SExpr(i) => (i, synElemListExpr.tail)
//        case SIntToExpr(prim, _) => throw new RuntimeException("Here is an Expression expected, but " + prim +
//          " is not an Expression!")
//        case SData(t,_) => t match {
//          case DIdentifier(data) => synElemListExpr.tail.head match {
//            case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
//            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
//          }
//          case DType(data) => synElemListExpr.tail.head match {
//            case SExpr(i) => (i.setType(data), synElemListExpr.tail.tail)
//            case a => throw new RuntimeException("Here is an Expression expected, but " + a +" is not an Expression!")
//          }
//        }
//        case SNat(t,_) => throw new RuntimeException("List should't have any Nats at this position! " + t)
//        case SAddrSpace(addrSpace,_) => throw new RuntimeException(
//          "List should't have AddrSpaceTypes at this beginning position! " + addrSpace)
//      }
//    val identifierName = maybeTypedIdent.asInstanceOf[r.Identifier]
//    val spanOfBackslash = parseState.tokenStream.head.s
//    val span = Span(spanOfBackslash.file,spanOfBackslash.begin, expr.span.head.end)
//    val lambda = Lambda(identifierName, expr)(rt.TypePlaceholder, Some(span))
//    println("synElemListMaybeTIdent: " + synElemListMaybeTIdent +" ______ " + synElemListExpr)
//
//    //local variables are in the list, so that not two same localVariables are declared
//    if (map.contains(identifierName.name)) {
//      throw new IllegalArgumentException("A variable or function with the exact same name '"+ identifierName.name +
//        "' is already declared! <- " + map.get(identifierName.name))
//    }
//    map.update(identifierName.name, Left(rd.ToBeTyped(identifierName)))
//
//    val myNewParseState = ParseState(toks, SExpr(lambda) :: synElemListMaybeTIdent, map,mapDepL, spanList)
//    println("myNewParseState: "+ myNewParseState)
//    Left(myNewParseState)
//  }

  //_________________________________________________________Lambda
  //_________________________________________________________Expres

  def parseNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseLowExpression: " + parseState)
    Left(parseState) |>
      (parseLambda _ || parseDepLambda || parseBracesExpr ||
        parseUnOperator || parseBinOperator || parseIdentNoDec ||
        parseNumber || parseAddrSpaceType || parseTypeinNoAppExpr|| parseNat //|| parseDependencies
        )

  }

  //Todo:Maybe not needed any more
//  def ParseTypesUntilRBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
//    if(parseState.tokenStream.isEmpty||parseState.tokenStream.head.isInstanceOf[RBracket]){
//      println("Abbruch; endlessPossibleParseType: "+ parseState)
//      return Left(parseState)
//    }
//    val p =
//      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, spanList) )  |>
//        parseType |> //Todo: I can't express FunctionTypes yet
//        ParseTypesUntilRBracket
//
//    p match {
//      case Left(newPS) => {
//        val synList = combineSynElemList(parseState.parsedSynElems, newPS.parsedSynElems).reverse
////        println("SynList in parseTypesUntilRBracket: " + synList + " , newPS: " + newPS.parsedSynElems + " , parseStateOld: " + parseState.parsedSynElems)
//        Left(ParseState(newPS.tokenStream, synList, newPS.mapFkt, newPS.mapDepL, spanList) )
//      }
//      case Right(e) => Right(e)
//    }
//  }

  def parseDepOrNormalFunctionTypeInNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val p = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      Left(parseState) |>
        parseLeftParentheses |>
        parseDepOrNormalFunctionType |>
        parseRightParentheses
    p match {
      case Right(e) => Right(e)
      case Left(pa) => Left(ParseState(pa.tokenStream, pa.parsedSynElems, pa.mapFkt, pa.mapDepL, parseState.spanList))
    }
  }

  def parseTypeinNoAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTypeinNoAppExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseTypeinNoAppExpr: "+ parseState)
      return Left(parseState)
    }
    val p = //Todo: change so that no Brackets are needed, only parentheses for Dep- and FunctionType
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        (parseDepOrNormalFunctionTypeInNoAppExpr _ || parseSimpleType)

    println("after parseTypeinNoAppExpr: "+ p)
    p match {
      case Left(newPS) => {
        val synList = combineSynElemList(parseState.parsedSynElems, newPS.parsedSynElems).reverse
//        println("SynList2 in parseTypeinNoAppExpr: " + synList + " , newPS: " + newPS.parsedSynElems + " , parseStateOld: " + parseState.parsedSynElems)
        Left(ParseState(newPS.tokenStream, synList, newPS.mapFkt, newPS.mapDepL, newPS.spanList) )
      }
      case Right(e) => Right(e)
    }
  }

  def parseBracesExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExpr: "+ parseState)
    if(parseState.tokenStream.isEmpty){
      println("Abbruch; parseBracesExpr: "+ parseState)
      return Left(parseState)
    }
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        parseLeftParentheses  |>
        parseMaybeAppExpr |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val spanClutch = pState.spanList match {
          case None => throw new IllegalStateException("Here should SpanList not be NONE")
          case Some(sp1::sp2::Nil) => sp1+sp2
          case Some(l) => throw new IllegalStateException("The List should have two Spans: "+ l)
        }
        val synElem = if(pState.parsedSynElems.length==1){
          pState.parsedSynElems.head match {
            case SExpr(expr) => SExprClutched(expr, spanClutch)
            case SExprClutched(expr, _) => SExprClutched(expr, spanClutch)
            case SAddrSpace(addrSpace, _) => SAddrSpace(addrSpace, spanClutch)
            case SData(data, _) => SData(data, spanClutch)
            case SIntToExpr(name, _) => SIntToExpr(name, spanClutch)
            case SNat(nat, _) => SNat(nat, spanClutch)
            case SType(t, _) => SType(t, spanClutch)
          }
        }else {
          val newExpr = combineExpressionsDependent(pState.parsedSynElems, pState.mapDepL)
          SExprClutched(newExpr, spanClutch)
        }
        println("synElem: " + synElem)
        val newL = synElem :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL, parseState.spanList)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseArrayType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIndexType: " + parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        parseNat |>
        parseDot |>
        parseSimpleType

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2, "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SNat(nat,spanNat)::SData(data,spanData) :: Nil => {
            val span = spanNat+spanData
            nat match {
              case NNumber(n) => data match {
                case DIdentifier(d) => SType(rt.ArrayType(n, d), span)
                case DType(d) => SType(rt.ArrayType(n,d), span)
              }
              case NIdentifier(n) => data match {
                case DIdentifier(d) => SType(rt.ArrayType(n, d), span)
                case DType(d) => SType(rt.ArrayType(n,d), span)
              }
            }
          }
          case SNat(nat,spanNat)::SType(t, spanType) :: Nil => {
            val span = spanNat+spanType
            nat match {
              case NNumber(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]), span)
              case NIdentifier(n) => SType(rt.ArrayType(n, t.asInstanceOf[rt.DataType]), span)
            }
          }
          case a => throw new RuntimeException("List should't have only Nat at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL, pState.spanList)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseIndexType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseIndexType: " + parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        parseTypeIdent |>
        parseLeftBracket |>
        parseNat |>
        parseRightBracket

    p match {
      case Left(pState) => {
        require(pState.parsedSynElems.length==2,
          "It should exactly be 1 Nat and one Identifer for IndexType in the list!")
        val ty = pState.parsedSynElems.reverse match {
          case SType(rt.TypeIdentifier("Idx"), spanType) :: SNat(nat,spanNat)::Nil => {
            val span = spanType+spanNat
            nat match {
              case NNumber(nat) => SType(rt.IndexType(nat), span)
              case NIdentifier(nat) => SType(rt.IndexType(nat), span)
            }
          }
          case a => throw new RuntimeException("List should't have only Identifier at this position! " + a)
        }
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL, pState.spanList)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }
  def parseTupleType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseTupleType: "+ parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        parseLeftParentheses  |>
        parseType |>
        parseComma |>
        parseType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val (tL, tLSpan) = getTypesInList(pState.parsedSynElems, None)
        val typesList = tL.reverse
        require(typesList.length==2, "It should exactly be two Types for PairType in the list!")
        val ty = SType(rt.PairType(typesList.head.asInstanceOf[rt.DataType],
          typesList.tail.head.asInstanceOf[rt.DataType]), tLSpan)
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL, parseState.spanList)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseBracesExprType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBracesExprType: "+ parseState)
    val p =
      Left(ParseState(parseState.tokenStream,Nil, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )  |>
        parseLeftParentheses  |>
        parseType |>
        parseRightParentheses

    p match {
      case Left(pState) => {
        if(pState.parsedSynElems.isEmpty){
          val rBraceIndex = parseState.tokenStream.indexWhere(p=> p.isInstanceOf[RParentheses])
          throw new RuntimeException("There was no Expression in Braces at posstion (" + 0 + " , " + rBraceIndex +
            " : "+ parseState.tokenStream.toString())
        }
        val (tL, spanTL) = getTypesInList(pState.parsedSynElems, None)
        val ty = SType(combineTypes(tL), spanTL)
        val newL = ty :: Nil
        val li:List[SyntaxElement] = parseState.parsedSynElems.reverse ++ newL
        val l = li.reverse
        val newParse:ParseState = ParseState(pState.tokenStream, l, pState.mapFkt, pState.mapDepL, parseState.spanList)
        Left(newParse)
      }
      case Right(e) => Right(e)
    }
  }

  def parseMaybeAppExpr(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseMaybeAppExpr: " + parseState)
    if(parseState.tokenStream.head.isInstanceOf[RParentheses]){
      println("L" +
        "RBrace is at the beginning of parseApp: " + parseState)
      return Left(parseState)
    }else if(parseState.tokenStream.head.isInstanceOf[EndNamedExpr]){
      println("EndNamedExpr sighted in ParseMaybeAppExpr: "+ parseState)
      return Left(parseState)
    }
    val parseStateOrError =
      Left(parseState)  |>
        parseNoAppExpr
    println("parseApp after parseLowExpression: "+ parseStateOrError)
    parseStateOrError match {
      case Right(e) => Right(e)
      case Left(ps)=> if(ps.tokenStream.isEmpty){
                              println("parseApp End, because TokenList is empty: "+ ps)
                              val expr = combineExpressionsDependent(ps.parsedSynElems, ps.mapDepL)
                              Left(ParseState(ps.tokenStream, SExpr(expr)::Nil, ps.mapFkt, ps.mapDepL, ps.spanList) )
                            }else{
                              val p = parseMaybeAppExpr(ps)
                              p match {
                                case Right(e) => Right(e)
                                case Left(newPS) => {
                                  val synElem = if(newPS.parsedSynElems.length==1){
                                    newPS.parsedSynElems.head
                                  }else {
                                    val expr = combineExpressionsDependent(newPS.parsedSynElems, newPS.mapDepL)
                                    println("\n\n\nSpanIsHere"+ expr +" : "+ expr.span+ "\n\n\n")
                                    SExpr(expr)
                                  }
                                  Left(ParseState(newPS.tokenStream, synElem::Nil, newPS.mapFkt, newPS.mapDepL, newPS.spanList) )
                                }
                              }
                            }
    }
  }

  def parseEqualsSign(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse EqualsSign: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case EqualsSign(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL, spanList) )
  }

  def parseDoubleColons(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedExprs, map, mapDepL, spanList) = parseState
    if (tokens.isEmpty) {
      return Right(ParseError("failed to parse DoubleColons: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DoubleColons(_) =>
      case tok => {
        println("failed parseBacklash: " + parseState)
        return Right(ParseError("failed to parse Backslash: " + tok + " is not an Backslash"))
      }
    }

    Left(ParseState(remainderTokens, parsedExprs, map, mapDepL, spanList) )
  }


  def parseAddrSpaceType(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val nextToken :: remainderTokens = parseState.tokenStream

    nextToken match {
      case AddrSpaceType(addrSpace, spanAddr) => {
        val p:rt.AddressSpace = addrSpace match {
          case "Local" => rt.AddressSpace.Local
          case "Global" => rt.AddressSpace.Global
          case "Private" => rt.AddressSpace.Private
          case "Constant" => rt.AddressSpace.Constant
        }
        Left(ParseState(remainderTokens,
          SAddrSpace(p,spanAddr) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )
      }
      case tok => {
        println("AddrSpaceTypeWasExpected: "+ tok + ": " + remainderTokens)
        Right(ParseError("failed to parse parseAddrSpaceType: " + tok + " is not an AddrSpaceType"))
      }
    }
  }

  def parseUnOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val nextToken :: remainderTokens = parseState.tokenStream

    val p = nextToken match {
      case UnOp(un, span) => un match {
        case OpType.UnaryOpType.NEG => Left(ParseState(remainderTokens,
          SExpr(r.primitives.neg(Some(span))) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )
        case OpType.UnaryOpType.NOT => Left(ParseState(remainderTokens,
          SExpr(r.primitives.not(Some(span))) :: parseState.parsedSynElems, parseState.mapFkt, parseState.mapDepL, parseState.spanList) )
      }
      case tok => {
        println("UnaryOperatorWasExpected: "+ tok + ": " + remainderTokens)
        Right(ParseError("failed to parse parseUnOperator: " + tok + " is not an UnaryOperator"))
      }
    }
    p
  }

  def parseNumber(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("ParseNumber: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    if(tokens.isEmpty){
      return Right(ParseError("failed to parse Number: " + " List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case I8(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number), Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
      case I32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.IntData(number),Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
      case F32(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.FloatData(number),Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
      case F64(number, span) =>
        Left(ParseState(remainderTokens, SExpr(r.Literal(rS.DoubleData(number),Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse Number: " + tok + " is not an accepted Integer of Float"))
    }
  }


  def parseBinOperator(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseBinOperator: "+ parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case BinOp(op, span) => op match {
        case OpType.BinOpType.ADD =>{
          println("\n\n Span of add: "+ span + " , add: "+ r.primitives.add(Some(span)) + "  ,SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr ) //es wird vergessen von Scala, dass es auch vom Type ToBeTyped ist
          println("span of add: " + r.primitives.add(Some(span)).span + "span of add: " + r.primitives.add(Some(span)).toUntypedExpr.span+ " , span of SExpr(add): "+ SExpr(r.primitives.add(Some(span))).expr.span)
          println("span of makeArray: "+ r.primitives.makeArray(5, Some(span)).span)
          Left(ParseState(remainderTokens, SExpr(r.primitives.add(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        }

        case OpType.BinOpType.DIV =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.div(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.EQ =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.equal(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.GT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.gt(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.LT =>
          Left(ParseState(remainderTokens, SExpr(r.primitives.lt(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.MOD => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mod(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.MUL => Left(ParseState(remainderTokens, SExpr(
          r.primitives.mul(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case OpType.BinOpType.SUB => Left(ParseState(remainderTokens, SExpr(
          r.primitives.sub(Some(span))) :: parsedSynElems, map, mapDepL, spanList) )
        case tok => {
          println("Das hier kann nicht sein, weil alle Operatoren müsste ich abgedeckt haben. BinOp: '" + tok +
            "' is no BinOperator!")
          Right(ParseError("failed to parse BinOperator: " + tok + " is not an accepted BinOp"))
        }
      }
      case tok => {
        println("BinOp: '" + tok + "' is no BinOperator!")
        Right(ParseError("failed to parse BinOperator: " + tok + " is not an BinOp"))
      }
    }
  }


  def parseComma(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    println("parseComma: " + parseState)
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Comma(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse Comma: " + tok + " is not an Comma"))
    }
  }



  def parseDepArrow(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case DepArrow(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse DepArrow: " + tok + " is not an DepArrow"))
    }
  }

  def parseColon(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Colon(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse Colon: " + tok + " is not an Colon"))
    }
  }

  def parseDot(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    if(tokens.isEmpty||tokens.length<=1){
      return Right(ParseError("failed to parse Dot, because List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case Dot(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse Dot: " + tok + " is not an Dot"))
    }
  }

  def parseNat(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case NatNumber(number, spanNat) => Left(ParseState(remainderTokens, SNat(NNumber(number:rt.Nat),spanNat)::parsedSynElems,
        map, mapDepL, spanList) )
      case TypeIdentifier(name, spanTypeIdentifier) =>Left(ParseState(remainderTokens,
        SNat(NIdentifier(rt.NatIdentifier(name)), spanTypeIdentifier)::parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse Nat: " + tok + " is not an Nat"))
    }
  }

  def parseData(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case TypeIdentifier(name, sp) =>Left(ParseState(remainderTokens,
        SData(DIdentifier(rt.DataTypeIdentifier(name)), sp)::parsedSynElems, map, mapDepL, spanList) )
      case a => Right(ParseError("It ist DatatypeIdentifier expected but '"+ a.toString +"' is not an DataTypeIdentifier: " + a.s.toString + a.s.file.toString))
    }
  }

  def parseLeftBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    if(tokens.isEmpty||tokens.length<=1){
      return Right(ParseError("failed to parse LeftBracket, because List is empty"))
    }
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case LBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse LeftBracket: " + tok + " is not an LeftBracket"))
    }
  }

  def parseRightBracket(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case RBracket(_) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, spanList) )
      case tok => Right(ParseError("failed to parse RightBracket: " + tok + " is not an RightBracket"))
    }
  }

  def parseLeftParentheses(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, _)  = parseState
    val nextToken :: remainderTokens = tokens

    nextToken match {
      case LParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, Some(sp::Nil)))
      case tok => Right(ParseError("failed to parse LeftParentheses: " + tok + " is not an LeftParentheses"))
    }
  }

  def parseRightParentheses(parseState: ParseState): Either[ParseState, ParseErrorOrState] = {
    val ParseState(tokens, parsedSynElems, map, mapDepL, spanList)  = parseState
    val nextToken :: remainderTokens = tokens

    val spanL = spanList match{
      case None => throw new IllegalStateException("The spanList should not be None")
      case Some(l) => l match {
        case spLeftPar :: Nil => spLeftPar
        case _ => throw new IllegalStateException("The list in parseRightParentheses: '" + l + "' has not the correct form")
      }
    }

    nextToken match {
      case RParentheses(sp) => Left(ParseState(remainderTokens, parsedSynElems, map, mapDepL, Some(sp::spanL ::Nil)) )
      case tok => Right(ParseError("failed to parse RightParentheses: " + tok + " is not an RightParentheses"))
    }
  }


  //_________________________________________________________Expres
}