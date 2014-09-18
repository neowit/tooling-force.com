package com.neowit.apex.completion

import java.io.{FilenameFilter, FileInputStream, File}
import java.util.regex.Pattern

import com.neowit.apex.parser.TreeListener.ApexTree
import com.neowit.apex.parser.antlr.ApexcodeParser._
import com.neowit.apex.parser.{Member, TreeListener}
import com.neowit.apex.parser.antlr.{ApexcodeParser, ApexcodeLexer}
import org.antlr.v4.runtime._
import org.antlr.v4.runtime.tree.{TerminalNode, ParseTree, ParseTreeWalker}

import scala.util.matching.Regex


class AutoComplete(file: File, line: Int, column: Int, cachedTree: ApexTree = Map[String, Member]()) {

    def getBaseToken: Caret = {
        val str = scala.io.Source.fromFile(file).getLines().toList(line-1)
        val startIndex = column - 1
        val leftStr = str.substring(0, startIndex)
        val WORD_CHARS = "\\w|_".r
        val EXPRESSION_CHARS = "\\w|_|\\[|\\]".r

        val symbolBoundaries = getExpressionBoundaries(leftStr, startIndex, WORD_CHARS)
        val expressionBoundaries = getExpressionBoundaries(leftStr, startIndex, EXPRESSION_CHARS)
        //cls. will return: symbol = "cls"
        val symbol = leftStr.substring(symbolBoundaries._1, symbolBoundaries._2 +1 )
        val expression = leftStr.substring(expressionBoundaries._1, expressionBoundaries._2 + 1)
        new Caret(line, symbolBoundaries._1, symbol, expression, file)
    }

    /**
     * scan str backwards starting startIndex while regex matches
     * @param str
     * @param r
     * @return
     */
    def getExpressionBoundaries(str: String, startIndex: Integer, r: Regex): (Int, Int) = {
        var keepGoing = true
        var symbol = ""
        var index = startIndex
        var end = -1
        //find first word (not dot or brackets)
        while (keepGoing && index >= 0) {
            index -= 1
            val ch = str.charAt(index).toString
            r.findFirstIn(ch) match {
                case Some(x) =>
                    symbol = x + symbol
                    if (end < 0) {
                        end = index
                    }
                case None => if (symbol.nonEmpty) keepGoing = false
            }
        }
        val start = index+1
        (start, end)
    }

    def listOptions:List[Member] = {
        //find base position
        //val symbol = "cls"
        //val caret = new Caret(line, column, symbol, file)
        val baseToken = getBaseToken

        //val tokenSource = new CodeCompletionTokenSource(getLexer(file), caret)
        //val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file)) //DEBUG
        val parser = new ApexcodeParser(tokens)

        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())

        listOptions(baseToken)
        /*
        try {
            val tree = parser.compilationUnit()
        } catch {
            case e:CaretReachedException =>
                return listOptions(caret)
            case e:Throwable =>
        }

        List()
        */
    }

    private def getLexer(file: File): ApexcodeLexer = {
        val input = new ANTLRInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }

    private def listOptions(caret: Caret): List[Member] = {
        //extractor.dump()
        //check if we have enough in the current file to list completion options
        println("Potential LOCAL signatures:")
        listOptions(caret, file, Some(new CompletionIgnoreErrorStrategy())) match {
          case Some(members) =>
              return members.toList
          case None =>
              //scan other available files
              println("\ntry to find definitions in other project files")
              val classFiles = file.getParentFile.listFiles(new FilenameFilter(){
                  override def accept(parentDir: File, name: String): Boolean = name.endsWith(".cls")
              })
              for (currentFile <- classFiles ) {
                  listOptions(caret, currentFile) match {
                    case Some(members) =>
                        return members.toList
                    case None =>
                  }
              }
        }

        List()
    }

    def listOptions(caret: Caret, file: File, errorStrategy: Option[ANTLRErrorStrategy] = None): Option[List[Member]] = {
        val lexer = getLexer(file)
        val tokens = new CommonTokenStream(lexer)
        val parser = new ApexcodeParser(tokens)
        errorStrategy match {
          case Some(completionErrorStrategy) =>
              //parser.setErrorHandler(new BailErrorStrategy())
              //parser.setErrorHandler(new CompletionErrorStrategy())
              parser.setErrorHandler(completionErrorStrategy)
          case None =>
        }
        val tree = parser.compilationUnit()
        val walker = new ParseTreeWalker()
        val extractor = new TreeListener(parser)
        walker.walk(extractor, tree)
        //has definition
        val definition = findSymbolType(caret, extractor)
        definition match {
          case Some((parseTree, typeContext)) =>
              println("caret type =" + typeContext.getText)
              val fullApexTree = cachedTree ++ extractor.tree
              fullApexTree.get(typeContext.getText) match {
                  case Some(member) =>
                      val members = member.getChildren
                      println("Potential signatures:")
                      println("-" + members.map(m => m.getIdentity + "=> " + m.getSignature).mkString("\n-"))
                      println("\n\n" + members.map(m => m.toJson).mkString("\n "))
                      return Some(members.toList)
                  case None => //check if this is one of standard Apex types
                      val typeName = typeContext.getText
                      val staticMembers = ApexModel.getMembers(typeName)
                      val instanceMethods = ApexModel.getInstanceMembers(typeName)
                      if (staticMembers.nonEmpty || instanceMethods.nonEmpty) {
                          return Some(staticMembers ++ instanceMethods)
                      } else {
                          return None
                      }
              }
          case None => //check if this is one of apex internal types
              val typeName = caret.symbol
              ApexModel.getMembers(typeName).find(_.getSignature.toLowerCase == typeName.toLowerCase) match {
                  case Some(typeMember) => return Some(typeMember.getChildren)
          case None =>
        }
              val staticMembers = ApexModel.getMembers(typeName)
              if (staticMembers.nonEmpty) {
                  return Some(staticMembers)
              }
        }
        return None

    }


    // try to find definition using parse tree
    def findSymbolType(caret: Caret, extractor: TreeListener): Option[(ParseTree, TypeContext)] = {
        extractor.getIdentifiers(caret.symbol) match {
          case Some(nodes) =>
              //remove node which matches the caret
              nodes.toList.find(n => caret.equals(n.getSymbol))  match {
                case Some(caretNode) =>
                    val potentialDefinitionNodes = nodes - caretNode
                    /* sort by proximity to caret*/
                    //sortWith((x, y) => x.getSymbol.getLine  > y.getSymbol.getLine)
                    //now find one which is closest to the caret and most likely definition
                    val distances = collection.mutable.ArrayBuffer[(Int, ParseTree, TerminalNode)]()
                    for (node <- potentialDefinitionNodes ) {
                        distanceToCommonParent(caretNode, node) match {
                          case Some((steps, commonParent)) =>
                              distances += ((steps, commonParent, node))
                          case None =>
                        }
                        println(node)
                    }
                    //find one with shortest distance
                    distances.toList.sortWith((x: (Int, ParseTree, TerminalNode), y: (Int, ParseTree, TerminalNode)) => x._1 < y._1).headOption  match {
                      case Some((steps, commonParent, n)) =>
                          println(steps + "=" + n)
                          //node which we found is most likely a definition of one under caret
                          return getTypeParent(n) match {
                            case Some((pt, typeContext)) => Some((pt, typeContext))
                            case None => None
                          }

                      case None =>
                    }
                case None =>
              }
              println(nodes)
          case None =>
        }
        None
    }


    /**
     *
     * @param n - node which may contain type definition
     *       1:   e.g. String str;
     *       2:   str.
     *          in this case n will be node containing str in line 1:
     * @return
     */
    private def getTypeParent(n: ParseTree): Option[(ParseTree, TypeContext)] = {
        if (null == n) {
            None
        } else {
            n match {
                case x: ClassDeclarationContext => Some((x, x.`type`()))
                //case x: TypeBoundContext => Some((x, x.`type`()))
                case x: MethodDeclarationContext => Some((x, x.`type`()))
                case x: FieldDeclarationContext => Some((x, x.`type`()))
                case x: InterfaceMethodDeclarationContext => Some((x, x.`type`()))
                case x: TypeArgumentContext => Some((x, x.`type`()))
                case x: FormalParameterContext => Some((x, x.`type`()))
                case x: LastFormalParameterContext => Some((x, x.`type`()))
                case x: AnnotationTypeElementRestContext => Some((x, x.`type`()))
                case x: LocalVariableDeclarationContext => Some((x, x.`type`()))
                case x: EnhancedForControlContext => Some((x, x.`type`()))
                case x: ExpressionContext => Some((x, x.`type`()))
                case x: PrimaryContext => Some((x, x.`type`()))
                case _ => getTypeParent(n.getParent)
            }
        }

    }

    private def distanceToCommonParent(caret: TerminalNode, node: TerminalNode): Option[(Int, ParseTree)] = {
        def getParents(parent: ParseTree, parents: List[ParseTree]): List[ParseTree] = {
            if (null != parent) {
                getParents(parent.getParent, parent :: parents)
            } else {
                parents
            }
        }
        val caretParents = getParents(caret.getParent, List()).reverse //from nearest parent to farthest
        val nodeParents = getParents(node.getParent, List()).reverse
        //in theory the nearest parent should be most likely definition
        //count number of steps to nearest parent
        var i = 0
        for (p <- caretParents) {
            nodeParents.find(_ == p) match {
                case Some(commonParent) =>
                    //found common parent
                    return Some(i, commonParent)
                case None =>
            }
            i += 1
        }
        None
    }
}

object CompletionUtils {

    private val WORD_PATTERN_STR = "^[\\$A-Za-z_][A-Za-z0-9_]*$"
    private val WORD_PATTERN: Pattern = Pattern.compile(WORD_PATTERN_STR)

    def isWordToken(token: Token): Boolean = {
        WORD_PATTERN.matcher(token.getText).matches
    }

    def getOffset(file: File, line: Int, startIndex: Int): Int = {
        val text = scala.io.Source.fromFile(file).mkString
        //val bytes = text.take
        var lineNum: Int = 0
        var pos = 0
        while ( lineNum < line && pos < text.length ) {
            val ch = text(pos)
            if ('\n' == ch) {
                lineNum += 1
            }
            pos = pos + 1
        }
        val offset = pos + startIndex
        offset
    }
}

object CaretToken {
    final val CARET_TOKEN_TYPE: Int = -2
    def apply(source: org.antlr.v4.runtime.misc.Pair[TokenSource, CharStream], channel: Int, start: Int, stop: Int) =
        new org.antlr.v4.runtime.CommonToken(source, CaretToken.CARET_TOKEN_TYPE, channel, start, stop) with CaretTokenTrait

    def apply(oldToken: Token) = {
        val token = new org.antlr.v4.runtime.CommonToken(oldToken) with CaretTokenTrait
        token.setType(CaretToken.CARET_TOKEN_TYPE)
        token.setOriginalToken(oldToken)
        token
    }
}

trait CaretTokenTrait extends org.antlr.v4.runtime.CommonToken {

    private var originalToken: Token = null

    def setOriginalToken(token: Token) {
        originalToken = token
    }
    def getOriginalToken: Token = {
        originalToken
    }

    def getOriginalType: Int = {
        originalToken.getType
    }

}


class Caret(val line:  Int, val startIndex: Int, val symbol:String, expression: String, file: File) {
    private var tokenType: String = ""

    def getOffset: Int = {
        CompletionUtils.getOffset(file, line, startIndex)
    }

    def setType(path: String): Unit = {
        tokenType = path
    }

    def getType: String = {
        tokenType
    }

    def equals(node: Token): Boolean = {
        line == node.getLine && startIndex == node.getCharPositionInLine
    }
}


/**
 *
 * used to stop parser at current cursor position
 */
class CodeCompletionTokenSource( source: TokenSource, caret: Caret) extends TokenSource {

    private var tokenFactory: TokenFactory[_] = CommonTokenFactory.DEFAULT
    private var caretToken: Token = null
    private val tokenFactorySourcePair = new misc.Pair(source, source.getInputStream)

    private val caretOffset = caret.getOffset

    private val stack= scala.collection.mutable.Stack[Token]()
    var lastPotentialDefinitionPosition = -1

    def getTokens: Array[Token] = {
        stack.toArray.reverse
    }

    /**
     * @return original token which was is replaced by CaretToken
     */
    def getCursorToken:Option[Token] = {
        if (null != caretToken && null != caretToken.asInstanceOf[CaretTokenTrait].getOriginalToken) {
            Some(caretToken.asInstanceOf[CaretTokenTrait].getOriginalToken)
        } else {
            None
        }
    }

    def nextToken: Token = {
        if (caretToken == null) {
            var token: Token = source.nextToken

            println("token=" + token.toString)
            if (token.getStopIndex + 1 < caretOffset) {
                stack.push(token)
                if (caret.symbol == token.getText) {
                    lastPotentialDefinitionPosition = stack.length - 1
                }
            }
            else if (token.getStartIndex > caretOffset) {
                token = CaretToken(tokenFactorySourcePair, Token.DEFAULT_CHANNEL, caretOffset, caretOffset)
                caretToken = token
            }
            else {
                if (token.getStopIndex + 1 == caretOffset && token.getStopIndex >= token.getStartIndex) {
                    if (!isWordToken(token)) {
                        return token
                    }
                }
                token = CaretToken(token)
                caretToken = token
            }
            return token
        }
        throw new UnsupportedOperationException("Attempted to look past the caret.")
    }

    def getLine: Int = {
        source.getLine
    }

    def getCharPositionInLine: Int = {
        source.getCharPositionInLine
    }

    def getInputStream: CharStream = {
        source.getInputStream
    }

    def getSourceName: String = {
        source.getSourceName
    }

    def getTokenFactory: TokenFactory[_] = {
        tokenFactory
    }

    def setTokenFactory(tokenFactory: TokenFactory[_]) {
        source.setTokenFactory(tokenFactory)
        this.tokenFactory = if (tokenFactory != null) tokenFactory else CommonTokenFactory.DEFAULT
    }

    protected def isWordToken(token: Token): Boolean = {
        CompletionUtils.isWordToken(token)
    }
}

/**
 * CompletionTokenStream allows to remove incomplete token under cursor to limit the possibility of erroneous input
 * @param tokenSource
 */
class CompletionTokenStream(tokenSource: TokenSource, offendingToken: Token) extends CommonTokenStream(tokenSource) {
    override def nextTokenOnChannel(i: Int, channel: Int): Int = {
        var index = i
        val token = get(super.nextTokenOnChannel(index, channel))
        if (token.getStartIndex == offendingToken.getStartIndex) {
            index = index + 1
            if (get(super.nextTokenOnChannel(index, channel)).getText == ".") {
                index = index + 1
                if (CompletionUtils.isWordToken(get(super.nextTokenOnChannel(index, channel)))) {
                    index = index + 1
                }
            }
        }
        super.nextTokenOnChannel(index, channel)
    }
}