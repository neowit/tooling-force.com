package com.neowit.apex.completion

import com.neowit.apex.parser.{ApexTree, ApexTreeListener, Member}
import com.neowit.apex.parser.antlr.SoqlParser
import org.antlr.v4.runtime.tree.ParseTree

/**
  * Author: Andrey Gavrikov (westbrook)
  * Date: 15/11/2016
  */
sealed trait TokenDefinition

case class SoqlTokenDefinition(soqlTypeMember: Member,
                               finalContext: ParseTree,
                               expressionTokens: List[AToken],
                               soqlExpressionTree: SoqlParser.SoqlCodeUnitContext) extends TokenDefinition

case class ApexTokenDefinition(definitionMember: Member,
                               typeMember: Member) extends TokenDefinition

case class ApexTokenDefinitionWithContext(definitionMember: Member,
                                          typeMember: Member,
                                          extractor: ApexTreeListener,
                                          fullApexTree: ApexTree,
                                          expressionTokens: List[AToken]) extends TokenDefinition

case class UnresolvedApexTokenDefinition(
                               extractor: ApexTreeListener,
                               fullApexTree: ApexTree,
                               expressionTokens: List[AToken]) extends TokenDefinition

case class ApexExpression(apexExpressionTokens: List[AToken])
