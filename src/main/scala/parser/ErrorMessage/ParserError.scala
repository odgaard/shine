package parser.ErrorMessage

import parser.{ConcreteKind, ConcreteType, Identifier, RecognizeLexeme, Span, Token, TypeIdentifier}

//__________________________________________ParserError

/*
https://stackoverflow.com/questions/38243530/custom-exception-in-scala is reference
 */
trait ParserError { self: Throwable =>
  val span: Span
  val description_error:String
  val what_exp:String
  val help:Option[String]
  val name_of_error:String
  var viewOriginal = false

  val begin = span.range.begin
  val end = span.range.end

  def get_sourceLines():Array[String] = if(viewOriginal){
    span.file.sourceLines_withoutPreLexer
  }else{
    span.file.sourceLines
  }
  /*
  problem: we have multiple lines where we look on, so we have to define for each Exeption individually
  what is important
   */
  def getImportantPos():(Int,Int,Int) = throw new IllegalStateException("this has to be overwritten")
  /*
  if -1 returned then failure
   */
  def getPos():(Int,Int,Int,Int,Int)={
    var (start_column, end_column, important_column,
    important_row_Begin, important_row_End):(Option[Int],
      Option[Int], Option[Int],Option[Int],Option[Int])=(None,None,None,None,None)

    start_column = Some(begin.column)
    end_column = Some(end.column)

    if(begin.column==end.column){
      important_column = Some(begin.column)
      important_row_Begin = Some(begin.row)
      important_row_End = Some(end.row)
    }else{
      val (iC, iRB, iRE) =getImportantPos()
      important_column=Some(iC)
      important_row_Begin=Some(iRB)
      important_row_End=Some(iRE)
    }


    (start_column.get,end_column.get,important_column.get,important_row_Begin.get,important_row_End.get)
  }

  override def toString: String = {
    val (start_column, end_column, important_column,
    important_row_Begin, important_row_End)= getPos()

    val underl = ErrorMessage.Underline_With_Char('^', RED())

    val error = ErrorMessage.give_error(span.file.fileName, description_error,what_exp,help,name_of_error,
      start_column,end_column,get_sourceLines(), underl, important_column, important_row_Begin,
      important_row_End)
    error
  }
}


//abstract sealed class ErrorMessage(span:Option[Span]) {
//  val sp = span
//  val s = span.getOrElse(SpanPlaceholder)
//  //def getErrorStackTraceElem():StackTraceElement=Thread.currentThread.getStackTrace().tail.tail.tail.tail.head
//  //val errorStackTraceElem = getErrorStackTraceElem()
//  val fileReader = s.file
//  val begin = s.range.begin
//  val end = s.range.end
//
//
//}

//______________________________________________________________________________________________________
//Lexer

object PreAndErrorToken{
  val underscoreNotAllowed = "Underscore is not allowed at the beginning"
  val unknownSymbol = "unknown symbol"
  val identifierExpectForm = Some("Identifier expect form '"+Identifier.regex+"'")
  val numberExpectForm = Some("Number expect form '"+Identifier.regex+"'")
}

abstract sealed class PreAndErrorToken(override val span: Span) extends Error with ParserError{
  override val name_of_error: String = "LexerError"
}

final case class ExpectedArrowButGotTwoDash(override val span:Span) extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error = "expected '->' but '--' is a comment and that destroyed it"
  override val what_exp = "this is the start of a comment"
  override val help = Some("delete one '-'")
  viewOriginal = true
}

final case class EndOfLine(override val span:Span) extends PreAndErrorToken(span){
  require(begin == end, "begin is unequal to end")
  override val description_error = "End of Line"
  override val what_exp = "no end of Line here expected"
  override val help = Some("this error is not your fault")
}
final case class EndOfFile(override val span:Span) extends PreAndErrorToken(span){
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "End of File"
  override val what_exp: String = "no end of File here expected"
  override val help: Option[String] = Some("you haven't end the expr")
}
final case class UnknownSymbol(c:Char, override val span:Span) extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The character '" + c + "' is unknown"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  val knownChars=RecognizeLexeme.knownCharacters.toString()
  override val help: Option[String] = Some("allowed characters '"+knownChars+"'")
}
final case class OnlyOneEqualSign(override val span:Span) extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "Only one EqualSign"
  override val what_exp: String = "two equal Signs are expected"
  override val help: Option[String] = Some("use '==' for the Operator Equal")
}
final case class IdentifierWithNotAllowedSymbol(unknownSymbol:Char, str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class IdentifierBeginsWithUnderscore(override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "Identifier with an Underscore"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class IdentifierBeginsWithDigits(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '"+ str+ "' begins with an Digits/a Number"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class NumberWithUnknownSymbol(unknownSymbol: Char, str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Number '" + str + "' has an unknown Symbol '" + unknownSymbol + "'"
  override val what_exp: String = PreAndErrorToken.unknownSymbol
  override val help: Option[String] = PreAndErrorToken.numberExpectForm
}
final case class F32DeclaredAsI8(number:Float, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The number '" + number + "' is an F32 Number but it is declared as I8"
  override val what_exp: String = "ends with I8"
  override val help: Option[String] = Some("delete I8 at the end")
}
final case class F32DeclaredAsI32(number:Float, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The number '" + number + "' is an F32 Number but it is declared as I32"
  override val what_exp: String = "ends with I32"
  override val help: Option[String] = Some("delete I32 at the end")
}
final case class IdentifierBeginsWithAF32Number(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Identifier '"+ str+ "' begins with a F32 Number"
  override val what_exp: String = "begins with Number"
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}
final case class NumberWithUnderscore(str:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String =  "The Number '"+ str+ "' has an underscore in it"
  override val what_exp: String = PreAndErrorToken.underscoreNotAllowed
  override val help: Option[String] = PreAndErrorToken.numberExpectForm
}
final case class NotExpectedToken(expectedToken:String, givenToken:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an '"+ expectedToken +"' expected. The Lexeme '" + givenToken +
    "' is not an '"+ expectedToken+ "'!"
  override val what_exp: String = "exptected was '"+expectedToken+"'"
  override val help: Option[String] = Some("Use the Token '"+expectedToken+"'")
}

final case class NotExpectedTwoBackslash(expectedToken:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an '"+ expectedToken +"' expected. But we have here two '\\'!"
  override val what_exp: String = "exptected was '"+expectedToken+"'"
  override val help: Option[String] = Some("Use the Token '"+expectedToken+"'")
}

final case class ToShortToBeThisToken(expectedLength:Int, token:String, override val span:Span)
  extends PreAndErrorToken(span){
  require(expectedLength >0, "expectedLength is less or equal to zero")
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "the given length is less than "+ expectedLength +" for "+ token +"!"
  override val what_exp: String = "under " +expectedLength + " length"
  override val help: Option[String] = None
}
final case class NOTanBinOperator(symbol: String, override val span:Span)
  extends PreAndErrorToken(span) {
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "The Symbol '" + symbol + "' is not an Binary Operator"
  override val what_exp: String = "no Binary Operator"
  override val help: Option[String] = Some("allowed binaryOperators '"+RecognizeLexeme.binarySymbol.toString()+"'")
}
final case class NOTanUnOperator(symbol: String, override val span:Span)
  extends PreAndErrorToken(span) {
  require(begin == end, "begin is unequal to end")
  override val description_error: String = "The Symbol '" + symbol + "' is not an Unary Operator"
  override val what_exp: String = "no Unary Operator"
  override val help: Option[String] = Some("allowed unary Operators '"+RecognizeLexeme.unarySymbol.toString()+"'")
}

final case class UnknownType(str: String, override val span:Span) extends PreAndErrorToken(span) {
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The Type '" + str + "' is not an accepted Type in RISE"
  override val what_exp: String = "not accepted type"
  override val help: Option[String] = Some("allowed scalarTypes are '"+RecognizeLexeme.scalarTypes.toString()+"'")
}

final case class UnknownKind(str: String, override val span:Span) extends PreAndErrorToken(span) {
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "The Kind '" + str + "' is not an accepted Kind in RISE"
  override val what_exp: String = "not accepted Kind"
  override val help: Option[String] = Some("allowed Kinds are '"+RecognizeLexeme.kinds.toString()+"'")
}

abstract sealed class ThisTokenShouldntBeHere(token: Token, override val span:Span)
  extends PreAndErrorToken(span) {
  val begin_token = token.s.range.begin
  val end_token = token.s.range.end
  require(begin.column == end.column, "not in one column")
  require(begin_token.column == end_token.column, "not in one column")
  override val description_error: String = "The Token '" + token.toString + "' was not here expected"
  override val what_exp: String = "wrong place"
  override val help: Option[String] = None
}

final case class ThisTokenShouldntBeHereExpectedArrowOrDots(token: Token, override val span:Span)
  extends ThisTokenShouldntBeHere(token, span){
  require(begin.column == end.column, "not in one column")
  override val what_exp: String = "expect '->' or ':'"
  override val help: Option[String] = Some("Lambda has form '\\Ident:Type->...' or '\\Ident->...'")
}

final case class TypeIdentifierExpectedNotIdentifier(name: String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an TypeIdentifier expected. '" + name + "' is an Identifier"
  override val what_exp: String = "expected TypeIdent"
  override val help: Option[String] = Some("TypeIdentifier expects form '"+TypeIdentifier.regex+"'")
}
final case class IdentifierExpectedNotTypeIdentifier(name: String, override val span:Span)
  extends PreAndErrorToken(span){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "It is an Identifier expected. '" + name + "' is an TypeIdentifier"
  override val what_exp: String = "expected Identifier"
  override val help: Option[String] = PreAndErrorToken.identifierExpectForm
}


//__________________________________________________________________________________________________________
//Parser


abstract sealed class PreAndErrorSynElems(override val span: Span, whatToParse: String)extends Error with ParserError{
  override val name_of_error: String = "ParserError"
}

final case class SynListIsEmpty(override val span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "SyntayElementslist is empty"
  override val what_exp: String = "List is empty"
  override val help: Option[String] = None
}

final case class TokListIsEmpty(override val span: Span, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "Tokenlist is empty"
  override val what_exp: String = "List is empty"
  override val help: Option[String] = None
}

/*
has to have at least one elem else ListIsEmpty Exeption should be used
 */
final case class TokListTooSmall(list: List[Token], minimumLen: Int, whatToParse: String)
  extends PreAndErrorSynElems(list.head.s, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "Tokenlist size is "+ list.size+ " but min. Lenght is "+ minimumLen
  override val what_exp: String = "List is too small"
  override val help: Option[String] = None
}

final case class NotCorrectToken(token: Token, expectedToken:String, whatToParse: String)
  extends PreAndErrorSynElems(token.s, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+token+"' is not '"+ expectedToken+"'"
  override val what_exp: String = "expected is '"+expectedToken+"'"
  override val help: Option[String] = None
}

final case class NoKindWithThisName(kind: TypeIdentifier, whatToParse: String)
  extends PreAndErrorSynElems(kind.span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "TypeIdent '"+kind.name+"' is not declared"
  override val what_exp: String = "undeclared KindName"
  override val help: Option[String] = None
}

final case class NotAcceptedScalarType(override val span:Span, notAcceptedType: ConcreteType, whatToParse: String)
  extends  PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+notAcceptedType+"' is not an ScalarType"
  override val what_exp: String = "not accepted ScalarType"
  override val help: Option[String] = Some("accepted ScalarTypes are '"+RecognizeLexeme.scalarTypes+"'")
}

final case class NotCorrectKind(override val span:Span, kind: ConcreteKind, whatToParse: String)
  extends PreAndErrorSynElems(span, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+kind+"' is not the expected Kind"
  override val what_exp: String = "not correct Kind"
  override val help: Option[String] = None
}

final case class NotCorrectSynElem(wrongSynElem: parser.parse.SyntaxElement,expectedSynElem: String, whatToParse: String)
  extends PreAndErrorSynElems(Span.getSpanSynElem(wrongSynElem).get, whatToParse){
  require(begin.column == end.column, "not in one column")
  override val description_error: String = "'"+wrongSynElem+"' is not the expected SynElem '" + expectedSynElem +"'"
  override val what_exp: String = "not accepted SynElem"
  override val help: Option[String] = None
}

//__________________________________________________________________________________________________________
abstract sealed class UseOrFailState()
final case class isParsing() extends UseOrFailState
final case class isFailed() extends UseOrFailState
final case class isMatched() extends UseOrFailState

final case class UsedOrFailedRule(state: UseOrFailState, whatToParse: String){
  override def toString():String =  {
    state match {
      case isParsing() => "parsing Rule "+ whatToParse
      case isFailed() =>  "failed Rule  "+ whatToParse
      case isMatched() => "matched Rule "+ whatToParse
    }
  }
}

//__________________________________________________________________________________________________________
//ErrorList
/*Gib aus welche Regeln angewendet werden und ob sie fehlschlagen.
  parsing NoAppExpr
  parsing Identifier
  failed to apply Rule: Identifier
  parsing Lambda
  ....
  matched Lambda
  matched NoAppExpr
 */

final case class ErrorList(){
  private var errorList: List[Either[UsedOrFailedRule, PreAndErrorSynElems]] = Nil
  private var deepestError: Int = -1

  def add(e:PreAndErrorSynElems):ErrorList={
    if(errorList.isEmpty || deepestError.equals(-1)){
      deepestError = 0
    }else{
      errorList(deepestError) match {
        case Right(newE)=> if(e.span.isAfter(newE.span)){
          deepestError = 0
        } else {
          deepestError += 1
        }
        case Left(usedOrFailedRule) => throw new IllegalStateException("Rules dont save spans and because of that can not be the deepest elem: "+ usedOrFailedRule)
      }
    }
    errorList = Right(e)::errorList
    //println("ErrorList: " +this.toString)
    this
  }
  def add(r:UsedOrFailedRule):ErrorList={
    if(!deepestError.equals(-1)){
      deepestError += 1
    }
    errorList = Left(r)::errorList
    this
  }

  /*
  so that is looks better with spaces, but if it takes too much time, it is probably better to solve it in an different way
   */
  def retSpace(space:Int): String ={
    val str = ""
    String.format("%-"+space+"s", str)
  }
  def returnDeepestElem():String = {
    //in returnList I reverse the List, so I have to reverse the number too!
    val dErrorInReverseList = errorList.length-this.deepestError-1
    "deepest Error at "+ dErrorInReverseList + ":\n"+
      this.errorList(this.deepestError).
        getOrElse(throw new IllegalStateException("Rules should not be the deepest elem")).toString
  }
  def returnList():String = {
    var s = "\nfull ErrorList:\n"
    val l = this.errorList.reverse
    var space = 1

    for(i <- 0 until l.size){
      if(space<0){
        println(s)
        throw new IllegalStateException("Oh no space is negative")
      }
      l(i) match {
        case Left(r)=> {
          s = s+retSpace(space)+space+";"+i+".th: "+r.toString()+ "\n"
          r.state match{
            case isParsing()=> space += 1
            case isMatched()=> space -= 1
            case isFailed() => space -= 1
          }
        }
        case Right(e)=> {
          s = s +retSpace(space)+space+";"+i+".th:\n"+ e.toString + "\n"
          space -= 1
        }
      }
    }
    s
  }
  override def toString: String =  returnDeepestElem()+returnList()
  def getList():List[Either[UsedOrFailedRule, PreAndErrorSynElems]]=this.errorList
  def getDeepestElemPos():Int =this.deepestError
  def getDeepestElem():PreAndErrorSynElems =this.errorList(this.deepestError).
    getOrElse(throw new IllegalStateException("Rules should not be the deepest elem"))
}
