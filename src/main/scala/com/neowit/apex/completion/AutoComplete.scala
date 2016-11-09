package com.neowit.apex.completion

import java.io.{FileInputStream, File}

import com.neowit.apex.Session
import com.neowit.apex.completion.models._
import com.neowit.apex.parser._
import com.neowit.apex.parser.antlr.{ApexcodeLexer, ApexcodeParser}
import com.neowit.apex.parser.antlr.ApexcodeParser._

import org.antlr.v4.runtime.tree.{ParseTree, ParseTreeWalker}
import org.antlr.v4.runtime._


class AutoComplete(file: File, line: Int, column: Int, cachedTree: ApexTree, session: Session) {
    /**
     * public void method() {
     *     String str;
     *     str.<caret>
     * }
     *
     * @param definitionMember - in the above example: definitionMember will be LocalVariableMember - "str"
     * @param typeMember - in the above example: typeMember will be ApexModel.ApexType member  - "String"
     */
    private class DefinitionWithType(val definitionMember: Member, val typeMember: Member)

    def listOptions:List[Member] = {
        val (initialExpressionTokens, caretReachedException) = getCaretStatement
        if (initialExpressionTokens.isEmpty) {
            //looks like the file is too broken to get to the point where caret resides
            return List()
        }
        val lexer = getLexer(file)
        val tokens = new CommonTokenStream(lexer)
        val parser = new ApexcodeParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        val tree = parser.compilationUnit()
        val walker = new ParseTreeWalker()
        val extractor = caretReachedException match {
          case Some(_caretReachedException) =>
              new ApexTreeListener(parser, file.toPath, line, column, _caretReachedException.caretToken.getCaret)
          case None =>
              new ApexTreeListener(parser, file.toPath, line, column)
        }
        walker.walk(extractor, tree)

        //now for each caretAToken find its type and use it to resolve subsequent caretAToken
        //List( someClassInstance, method(), goes, .)
        val fullApexTree: ApexTree = cachedTree.clone()
        fullApexTree.extend(extractor.tree)

        //check if caret is inside SOQL expression [select ...]
        var realExpressionTokens = initialExpressionTokens
        if (1 == initialExpressionTokens.size && initialExpressionTokens.head.isSoql) {
            val soqlComplete = new SoqlAutoComplete(initialExpressionTokens.head.token.get, line, column, fullApexTree, session)
            val result = soqlComplete.listOptions
            if (result.isSoqlStatement) {
                return result.options
            } else {
                realExpressionTokens = result.expressionTokens
            }
        }
        val expressionTokens = realExpressionTokens


        val definition = findSymbolType(expressionTokens.head, extractor, fullApexTree)
        definition match {
            case Some(definitionWithType) =>
                val members = if (expressionTokens.size > 1) {
                    resolveExpression(definitionWithType.typeMember, expressionTokens.tail, fullApexTree,
                        Some(definitionWithType.definitionMember), extractor.getCaretScopeMember)
                } else {
                    resolveExpression(definitionWithType.typeMember, expressionTokens, fullApexTree,
                        Some(definitionWithType.definitionMember), extractor.getCaretScopeMember)
                }
                return members
            case _ =>
                //check if this is something like MyClass.MySubclass
                val startType = expressionTokens.head.symbol
                fullApexTree.getClassMemberByType(startType) match {
                    case Some(_member) => //e.g. someClassInstance
                        val members = resolveExpression(_member, expressionTokens.tail, fullApexTree, None, extractor.getCaretScopeMember)
                        return members

                    case None =>
                        //final attempt - check if current symbol is a namespace or one of System types
                        ApexModel.getNamespace(startType) match {
                            case Some(_member) => //caret is a namespace
                                val members = resolveExpression(_member, expressionTokens.tail, fullApexTree,  None, extractor.getCaretScopeMember)
                                return members
                            case None => //check if caret is part of System
                                ApexModel.getSystemTypeMember(startType) match {
                                  case Some(_member) =>
                                      val members = resolveExpression(_member, expressionTokens.tail, fullApexTree,  None, extractor.getCaretScopeMember)
                                      return members
                                  case None => return List()
                                }
                        }
                }
        }
        List()
    }

    def isArrayDefinition(typeName: String): Boolean = {
        clearWhiteSpace(typeName).endsWith("[]")
    }
    def isListDefinition(typeName: String): Boolean = {
        clearWhiteSpace(typeName).toLowerCase.startsWith("list<")
    }
    def isMapDefinition(typeName: String): Boolean = {
        clearWhiteSpace(typeName).toLowerCase.startsWith("map<")
    }
    def isSetDefinition(typeName: String): Boolean = {
        clearWhiteSpace(typeName).toLowerCase.startsWith("set<")
    }
    private def clearWhiteSpace(str: String): String = {
        if (null == str) {
            ""
        } else {
            str.replaceAllLiterally(" ", "")
        }
    }
    /**
     * get member which defines the type of current member
     * e.g. SomeClass var;
     * if this member is 'var' then we will return member of SomeClass
     * @param memberWithTypeToResolve member which type needs to be resolved
     * @return
     */
    private def findTypeMember(memberWithTypeToResolve: Member, fullTree: ApexTree): Option[Member] = {
        if (memberWithTypeToResolve.isInstanceOf[SObjectRelationshipFieldMember]) {
            //for relationship fields there is no need to resolve further, actual field is the type member
            return Some(memberWithTypeToResolve)
        }
        if (memberWithTypeToResolve.isInstanceOf[ApexEnumConstantMember]) {
            //for apex model enums members there is no need to resolve further, actual field is the type member
            return Some(memberWithTypeToResolve)

        }
        val initialTypeName = memberWithTypeToResolve.getFullType
        val typeName = if (isArrayDefinition(initialTypeName) || isListDefinition(initialTypeName)) {
            //this is Array
            "System.List" //Array in Apex is just one a shortcut for list definition
        } else if (isMapDefinition(initialTypeName)){
            "System.Map"
        } else if (isSetDefinition(initialTypeName)){
            "System.Set"
        } else {
            initialTypeName
        }

        if (ApexVoid.getIdentity == typeName) {
            return Some(ApexVoid)
        }

        //first check if this is one of parsed classes
        fullTree.getClassMemberByType(typeName) match {
          case Some(typeMember) => return Some(typeMember)
          case None =>
                //now check if memberType is the type of inner class in the current Main/Outer class
                val innerMember = memberWithTypeToResolve.getTopMostClassMember match {
                  case Some(outerClassMember) =>
                      outerClassMember.getInnerClassByType(typeName) match {
                        case Some(x) => Some(x)
                        case None => outerClassMember.getEnumByName(typeName)
                      }
                  case None => None
                }

                innerMember match {
                  case Some(member) =>
                      //type of memberToResolve is inner class or Enum defined by current member
                      return Some(member)
                  case None =>
                }
        }

        //search in main tree and current file failed, let's see if memberType is one of apex types
        //check if this is one of standard Apex types
        getApexModelMember(typeName, memberWithTypeToResolve) match {
            case Some(member) => Some(member)
            case None =>
                //if all of the above has been unsuccessful then check if current startType is SObject type
                DatabaseModel.getModelBySession(session) match {
                    case Some(model) => model.getSObjectMember(typeName)
                    case None => None
                }
        }

    }

    //check if this is one of standard Apex types
    private def getApexModelMember(symbol: String, memberWithTypeToResolve: Member): Option[Member] = {
        //check if caret is fully qualified type with namespace
        ApexModel.getNamespace(symbol) match {
            case Some(namespaceMember) =>
                Some(namespaceMember)
            case None =>
                //check if caret is fully qualified type with namespace
                ApexModel.getTypeMember(symbol) match {
                  case Some(_member) => Some(_member)
                  case None => //check if we can find type by using identity of current member and its parent: parent-type.child-type
                        memberWithTypeToResolve.getParent match {
                          case Some(_parent: Member) => ApexModel.getTypeMember(_parent.getIdentity + "." + symbol)
                          case _ => None
                        }
                }
        }
    }
    private def findMember(typeName: String, apexTree: ApexTree, ctx: Option[ParseTree] = None ): Option[Member] = {
        //check if this is a top level type
        apexTree.getClassMemberByType(typeName) match {
          case Some(classMember) =>
              //this will cover OuterClass and OuterClass.InnerClass cases
              return Some(classMember)
          case None => // check if current type name is short (not FQN) class name of inner class in the current file
              if (ctx.isDefined) {
                  ClassBodyMember.getTopMostClassContext(ctx.get) match {
                      case Some(classContext) if null != classContext.asInstanceOf[ClassDeclarationContext].Identifier() =>
                          val topClassTypeName = classContext.asInstanceOf[ClassDeclarationContext].Identifier()
                          apexTree.getClassMemberByType(topClassTypeName + "."  + typeName) match {
                            case Some(_classMember) =>
                                return Some(_classMember)
                            case None =>
                          }

                      case None =>
                  }
              }

        }
        //check if this is one of standard Apex types
        ApexModel.getNamespace(typeName) match {
            case Some(namespaceMember) =>
                Some(namespaceMember)
            case None =>
                //check if caret is fully qualified type with namespace
                ApexModel.getTypeMember(typeName) match {
                    case Some(member) => Some(member)
                    case None => None
                }
        }


    }

    //TODO add support for collections str[1] or mylist.get()
    private def resolveExpression(parentType: Member, expressionTokens: List[AToken], apexTree: ApexTree,
                                  definitionMember: Option[Member] = None, caretScopeOpt: Option[AnonymousMember] = None ): List[Member] = {
        if (Nil == expressionTokens) {
            return removeInvisibleMembers(parentType.getChildrenWithInheritance(apexTree), definitionMember, caretScopeOpt)
        }
        val token: AToken = expressionTokens.head
        if (token.symbol.isEmpty) {
            return removeInvisibleMembers(parentType.getChildrenWithInheritance(apexTree), definitionMember, caretScopeOpt)
        }
        val tokensToGo = expressionTokens.tail

        //see if we can find the exact match
        parentType.getChild(token.symbol) match {
            case Some(_childMember) =>
                findTypeMember(_childMember, apexTree) match {
                    case Some(_typeMember) =>
                        return resolveExpression(_typeMember, tokensToGo, apexTree, Some(_childMember), caretScopeOpt)
                    case None =>
                }
            case None if tokensToGo.isEmpty => //parent does not have a child with this identity, return partial match
                val partialMatchChildren = filterByPrefix(parentType.getChildrenWithInheritance(apexTree), token.symbol)
                if (partialMatchChildren.isEmpty) {
                    //token.symbol may be apex type
                    return getApexTypeMembers(token.symbol)
                } else {
                    return partialMatchChildren
                }
            case _ => //check if parentType has child which has displayable identity == token.symbol
                //this is usually when token.symbol is a method name (method Id includes its params, so have to use getIdentityToDisplay)
                parentType.getChildren.find(_.getIdentityToDisplay.toLowerCase == token.symbol.toLowerCase) match {
                case Some(sobjectFieldMember: SObjectFieldMember) =>
                    return resolveExpression(sobjectFieldMember, tokensToGo, apexTree, Some(sobjectFieldMember), caretScopeOpt)
                case Some(_childMember) =>
                    findTypeMember(_childMember, apexTree) match {
                      case Some(_typeMember) =>
                          return resolveExpression(_typeMember, tokensToGo, apexTree, Some(_childMember), caretScopeOpt)
                      case None =>
                    }
                case None =>
            }
        }
        List()
    }

    /**
     * using context of current caret definition leave only those members which match context (static/private, etc)
     * @param members - full list of completion candidates
     * @param definitionMember - member which defines starting caret expression
     * @return
     */
    private def removeInvisibleMembers(members: List[Member], definitionMember: Option[Member], caretScopeOpt: Option[AnonymousMember]): List[Member] = {
        val members1 = definitionMember match {
            case Some(m:EnumMember) => members //do not filter anything for Enum
            case Some(m:EnumConstantMember) => members //do not filter anything for Enum
            case Some(m:ApexEnumMember) => members //do not filter anything for Enum
            case Some(m:ApexEnumConstantMember) => members //do not filter anything for Enum
            case Some(m:FieldMember) => members.filter(instanceOnlyFilter)
            case Some(m:PropertyMember) => members.filter(instanceOnlyFilter)
            case Some(m:MethodMember) => members.filter(instanceOnlyFilter)
            case Some(m:ApexMethod) => members.filter(instanceOnlyFilter)
            case _ => definitionMember match {
                case Some(_defMember) if !_defMember.isStatic => //remove all static members
                    members.filter(instanceOnlyFilter)
                case Some(_defMember) if _defMember.isStatic => //remove all instance members
                    members.filter(staticOnlyFilter)
                case _ => //if there is no definition then current context is most likely static
                         //remove all instance members
                    members.filter(staticOnlyFilter)
            }
        }
        //remove members not visible from current context

        definitionMember match {
        /* TODO implement scope resolution for cases like SomeClass.SomeInnerClass.<caret>
          case Some(member) if member.isInstanceOf[ClassMember]=>
              //current expression is of type class itself, i.e. something like MyClass.MyInnerClass
              //if current file is the same as the one where otherMember is defined then we can show private/protected members
              //otherwise no reason to show them
              caretScopeOpt match {
                  case Some(scopeMember) =>
                      scopeMember.getTopMostClassMember match {
                          case Some(classOfCaretMember) =>
                              val visibleMembers = members1.filter(otherMember => {
                                  val otherMemberVisibility = otherMember.getVisibility.toLowerCase
                                  val otherMemberIsNotPrivate = !Set("private", "protected").contains(otherMemberVisibility)
                                  otherMember.getTopMostClassMember match {
                                      case Some(otherMemberClass) =>
                                          otherMemberClass == classOfCaretMember || otherMemberIsNotPrivate
                                      case None => otherMemberIsNotPrivate
                                  }
                              })
                              visibleMembers
                          case None => members1
                  }
                  case None => members1
              }
          */
          case Some(member) => //remove members not visible from current context
              member.getClassMember match {
                case Some(classOfCaretMember) => members1.filter(otherMember => visibilityFilter(member, classOfCaretMember, otherMember))
                case None => members1
              }
          case None => members1
        }
    }

    private def staticOnlyFilter(m: Member): Boolean = {
        m.isStatic || m.isInstanceOf[ClassLikeMember] || m.isInstanceOf[EnumMember] || m.isInstanceOf[ApexType]
    }
    private def instanceOnlyFilter(m: Member): Boolean = {
        !staticOnlyFilter(m)
    }

    private def visibilityFilter(caretMember: Member, classMember: ClassLikeMember, m: Member): Boolean = {
        m.getClassMember match {
            case Some(otherClassMember) =>
                m.getVisibility match {
                    case "private" =>
                        classMember == otherClassMember
                    case "protected" => //check that other member is same or super class of caretMember
                        classMember == otherClassMember || classMember.isInheritFrom(otherClassMember)
                    case _ => true
                }
            case None => true
        }
    }

    private def filterByPrefix(members: List[Member], prefix: String): List[Member] = {
        members.filter(_.getIdentity.toLowerCase.startsWith(prefix.toLowerCase))
    }

    private def getApexTypeMembers(typeName: String): List[Member] = {
        ApexModel.getMembers(typeName.toLowerCase) match {
          case x :: xs => x :: xs
          case Nil => //check if caret is part of System
              ApexModel.getMembers("system." + typeName)
        }
    }

    /**
     * find blockStatement where caret reside
     *
     * vari| - list of all elements that can start with “vari” in current scope
     * variable.| - list of all methods of “variable” type
     * variable.andSome| - list of all methods of “variable” type that start with “andSome”
     * variable.andSome.| - list of all methods of property “andSome” of “variable”
     *
     * collection.| - list of all methods of collection (need to know collection type)
     * collection[0].| - list of all methods of element in collection (need to know element type)
     * @return
     */
    private def getCaretStatement: (List[AToken], Option[CaretReachedException]) = {
        val caret = new CaretInFile(line, column, file)
        val tokenSource = new CodeCompletionTokenSource(getLexer(file), caret)
        val tokens: CommonTokenStream = new CommonTokenStream(tokenSource)  //Actual
        //val tokens: CommonTokenStream = new CommonTokenStream(getLexer(file))
        val parser = new ApexcodeParser(tokens)
        ApexParserUtils.removeConsoleErrorListener(parser)
        parser.setBuildParseTree(true)
        parser.setErrorHandler(new CompletionErrorStrategy())

        //parse tree until we reach caret caretAToken
        try {
            parser.compilationUnit()
        } catch {
            case ex: CaretReachedException =>
                //println("found caret?")
                //println(ex.getToken.getText)
                //listOptions(ex)
                return (CompletionUtils.breakExpressionToATokens(ex), Some(ex))
            case e:Throwable =>
                println(e.getMessage)
        }

        (List(), None)
    }

    /**
     *
     * @param caretAToken AToken representing symbol which type we need to resolve
     * @param extractor - TreeListener with information about current file
     * @return
     *         - ParseTree - declarationContext
     *         - ParseTree = identifier or type context
     */
    private def findSymbolType(caretAToken: AToken, extractor: ApexTreeListener, fullCachedTree: ApexTree): Option[DefinitionWithType] = {
        val symbol = caretAToken.symbol.toLowerCase
        if ("this" == symbol || "super" == symbol) {
            //process special cases: this & super
            ApexParserUtils.getParent(caretAToken.finalContext, classOf[ClassDeclarationContext]) match {
                case Some(classDeclarationContext) =>
                    return findMember(classDeclarationContext.Identifier().getText, fullCachedTree, Some(caretAToken.finalContext)) match {
                        case Some(thisClassMember: ClassLikeMember) if "this" == symbol => Some(new DefinitionWithType(thisClassMember, thisClassMember))
                        case Some(thisClassMember: ClassLikeMember) if "super" == symbol => thisClassMember.getSuperClassMember match {
                            case Some(typeMember) => Some(new DefinitionWithType(thisClassMember, typeMember))
                            case None => None
                        }
                        case _ => None
                    }
                case None => None
            }
        } else if ("@" == symbol) {
            ApexModel.getTypeMember("Annotations") match {
              case Some(annotationsMember) => Some(new DefinitionWithType(annotationsMember, annotationsMember))
              case None => None
            }
        } else {
            extractor.getCaretScopeMember match {
                case Some(parentScopeMemberOfCaret) =>
                    return findSymbolInMemberHierarchy(parentScopeMemberOfCaret, symbol, fullCachedTree) match {
                      case Some(definitionWithType) => Some(definitionWithType)
                      case None if parentScopeMemberOfCaret.isInstanceOf[CreatorMember] && !isToTheRightOfEqualsSign(caretAToken, extractor) =>
                          //this is probably SObject creator, e.g. new Account (<caret>, Name = 'some')
                          val creatorMember = parentScopeMemberOfCaret.asInstanceOf[CreatorMember]
                          DatabaseModel.getModelBySession(session) match {
                              case Some(model) => model.getSObjectMember(creatorMember.createdName) match {
                                case Some(databaseModelMember) =>
                                    val sobjectDefWithType = Some(new DefinitionWithType(databaseModelMember, databaseModelMember))
                                    if (symbol.isEmpty) {
                                        sobjectDefWithType
                                    } else {
                                        //symbol is clarified by field name
                                        databaseModelMember.getChild(symbol) match {
                                          case Some(_typeMember) =>
                                              //return field member
                                              Some(new DefinitionWithType(_typeMember, _typeMember))
                                          case None => //fall back to SObject
                                              sobjectDefWithType
                                        }
                                    }
                                case None => None
                              }
                              case None => None
                          }
                      case _ => None
                    }
                case None => //current symbol is not defined in the current class
            }
            None
        }
    }

    //check if this item is on the right side of '=', in which case it can not be a field of SObject
    private def isToTheRightOfEqualsSign(caretAToken: AToken, extractor: ApexTreeListener): Boolean = {
        var i = caretAToken.index
        val inputStream = extractor.parser.getInputStream

        while (i > 0) {
            if (!ApexParserUtils.isWordTokenOrDot(inputStream.get(i))) {
                if (i > 0 && "=" == inputStream.get(i).getText) {
                    return true
                } else {
                    return false
                }
            }
            i -= 1
        }
        false
    }

    private def findSymbolInMemberHierarchy(parentMember: AnonymousMember, identity: String, fullApexTree: ApexTree): Option[DefinitionWithType] = {
        parentMember.getChild(identity, withHierarchy = true) match {
          case Some(definitionMember) =>
              //definitionMember //member that defines type of token under cursor
              //now find the type of this member
              findTypeMember(definitionMember, fullApexTree) match {
                case Some(typeMember) => Some(new DefinitionWithType(definitionMember, typeMember))
                case None => None
              }
          case None =>
                parentMember.getParent match {
                  case Some(x) =>
                      findSymbolInMemberHierarchy(x, identity, fullApexTree)
                  case None => None
                }
        }
    }


    private def getLexer(file: File): ApexcodeLexer = {
        //val input = new ANTLRInputStream(new FileInputStream(file))
        val input = new CaseInsensitiveInputStream(new FileInputStream(file))
        val lexer = new ApexcodeLexer(input)
        lexer
    }


}











