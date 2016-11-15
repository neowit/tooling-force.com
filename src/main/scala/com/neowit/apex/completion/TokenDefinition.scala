/*
 * Copyright (c) 2016 Andrey Gavrikov.
 * this file is part of tooling-force.com application
 * https://github.com/neowit/tooling-force.com
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package com.neowit.apex.completion

import com.neowit.apex.parser.{ApexTree, ApexTreeListener, Member}
import com.neowit.apex.parser.antlr.SoqlParser
import org.antlr.v4.runtime.tree.ParseTree

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
