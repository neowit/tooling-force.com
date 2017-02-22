/**
 *  Copyright (c) 2015-2017, Andrey Gavrikov. All rights reserved.
 *
 *  License: LGPL v3 <http://www.gnu.org/licenses/>
 *
 *  SOQL grammar for ANTLR v4.
 *  
 *  You can test with
 *
 *  $ antlr4 Soql.g4
 *  $ javac *.java
 *  $ grun Apexcode soqlStatement *.sql
 */
grammar Soql;

@header {
package com.neowit.apex.parser.antlr; 
}

// starting point for parsing a SOQL expression
soqlCodeUnit
	:	 ('[' soqlStatement ']' | soqlStatement) EOF
	;

soqlStatement
    :   selectStatement 
		fromStatement 
		whereStatement? 
		withStatement? 
		groupByStatement? 
		orderByStatement? 
		limitStatement? 
		offsetStatement? 
		(forViewStatement | forReferenceStatement)? updateStatement?
    ;



selectStatement
	:	SELECT selectItem (',' selectItem)*
	|	SELECT typeOfExpression
	;

selectItem
	:	fieldItem
	|	subquery
	;


fromStatement
	:	FROM objectType (',' objectType)* usingStatement?
	;

withStatement
	:	WITH DATA CATEGORY expression
	;

groupByStatement
	:	GROUP BY fieldList 
		( HAVING havingConditionExpression
		| ROLLUP fieldList
		| CUBE fieldList
		)?
	;

orderByStatement
	:	ORDER BY fieldList (ASC | DESC)? (NULLS (FIRST | LAST))?
	;

limitStatement
	:	LIMIT IntegerLiteral
	;

offsetStatement
	:	OFFSET IntegerLiteral
	;

forViewStatement
	:	FOR VIEW
	;

forReferenceStatement
	:	FOR REFERENCE
	;

updateStatement
	:	UPDATE (TRACKING | VIEWSTAT)
	;

fieldList
	:	fieldItem (',' fieldItem)*
	;

// (SELECT Contact.LastName FROM Account.Contacts)
subquery
	:	'(' SELECT fieldList fromSubqueryStatement whereStatement? ')'
	;
	
fromSubqueryStatement
	:	FROM relationshipItem
	;

typeOfExpression
	:	TYPEOF fieldItem WHEN whenExpression THEN fieldList (WHEN whenExpression THEN fieldList)* (ELSE fieldList)? END
	;

objectType
	:	Identifier alias?
	;

usingStatement
	:	USING SCOPE filterScope
	;

filterScope
	:	expression
	;

whereStatement
	:	WHERE whereConditionExpressions
	;

whereConditionExpressions
	:	whereConditionExpression ((AND | OR) whereConditionExpression)*
	;

whereConditionExpressionsSimple
	:	whereConditionExpressionSimple ((AND | OR) whereConditionExpressionSimple)*
	;

whereConditionExpression
	:	whereConditionExpressionSimple
	|	whereConditionExpressionWithSubquery
	;

whereConditionExpressionSimple
	:	conditionExpression
	;

whereConditionExpressionWithSubquery
	:	fieldItem IN whereSubquery
	;

whereSubquery
	:	'(' SELECT fieldName FROM objectType WHERE whereConditionExpressionsSimple ')'
	;

conditionExpression
	:	fieldItem (simpleOperator | likeOperator) expression
    |   fieldItem NOT? IN ':' expression
	|	NOT fieldItem simpleOperator expression
	|	fieldItem specialOperator '(' primary (',' primary)* ')'
	|	fieldItem bindOperator expression
	|	distanceFunction simpleOperator expression
    |   '(' whereConditionExpressions ')'
	;
	
whenExpression
	:	objectType
	;
	
havingConditionExpression
	:	aggregateFunction simpleOperator primary
	;

simpleOperator
	: ('=' 
		| '!=' 
		| '<>' 
		| '<'
		| '<='
		| '>'
		| '>='
		)
	;

likeOperator : LIKE;

specialOperator
	: ( IN
		| NOT IN
		| INCLUDES
		| EXCLUDES
		)
	;

bindOperator
	: simpleOperator ':'
	| likeOperator ':'
	;

fieldItem
	:	aggregateFunction alias?
	|	displayFunction alias?
	|	fieldName
	;

relationshipItem
	:	relationshipPath
	;

aggregateFunction
	:	AVG '(' fieldName ')'
	|	COUNT '(' ')'
	|	COUNT '(' fieldName ')'
	|	COUNT_DISTINCT '(' fieldName ')'
	|	MIN '(' fieldName ')'
	|	MAX '(' fieldName ')'
	|	SUM '(' fieldName ')'
	;

convertCurrencyFunction
    :  CONVERT_CURRENCY '(' fieldName ')'
    ;

convertTimezoneFunction
    :  CONVERT_TIMEZONE '(' fieldName ')'
    ;

displayFunction
	:	TO_LABEL '(' fieldName ')'
	|	FORMAT '(' ( aggregateFunction | convertCurrencyFunction | fieldName ) ')'
	|	convertCurrencyFunction
	|	convertTimezoneFunction
	|	dateFunction
    |   distanceFunction
	;

distanceFunction
	: DISTANCE '(' (fieldName | geolocationFunction) ',' (fieldName | geolocationFunction) ',' geoUnit ')'
	;

geoUnit 
	: '\'mi\'' | '\'km\''
	;
	
geolocationFunction
	: GEOLOCATION '(' 
			(IntegerLiteral | FloatingPointLiteral)
			',' 
			(IntegerLiteral | FloatingPointLiteral)  
		')'
	;

alias
    :   Identifier
    ;

fieldName
	:	Identifier ('.' Identifier)*
	;

relationshipPath
	:	Identifier ('.' Identifier)*
	;

expression
    :   primary
    |   primary '[' expression ']'
	|	expression '.' expression
    |   innerCreator
	;

primary
	:	parExpression
	|	literal
	|	dateLiteral
	|	Identifier
	;

parExpression
	:	'(' expression ')'
	;

literal
    :	(IntegerLiteral | FloatingPointLiteral)
    |   CharacterLiteral
    |   DateLiteral
    |   StringLiteral
    |   BooleanLiteral
    |   NULL
    ;

dateFunction
	: dateFunctionLiteral '(' fieldName ')'
	| dateFunctionLiteral '(' CONVERT_TIMEZONE '(' fieldName ')' ')'
	;
	
dateFunctionLiteral
	: CALENDAR_MONTH | CALENDAR_QUARTER | CALENDAR_YEAR
	| DAY_IN_MONTH | DAY_IN_WEEK | DAY_IN_YEAR | DAY_ONLY		
	| FISCAL_MONTH | FISCAL_QUARTER | FISCAL_YEAR		
	| HOUR_IN_DAY | WEEK_IN_MONTH | WEEK_IN_YEAR	
	;

dateLiteral
	: YESTERDAY | TODAY | TOMORROW | LAST_WEEK | THIS_WEEK | NEXT_WEEK | LAST_MONTH | THIS_MONTH | NEXT_MONTH | LAST_90_DAYS | NEXT_90_DAYS | LAST_N_DAYS_N
	| NEXT_N_DAYS_N | NEXT_N_WEEKS_N | LAST_N_WEEKS_N | NEXT_N_MONTHS_N | LAST_N_MONTHS_N 
	| THIS_QUARTER | LAST_QUARTER | NEXT_QUARTER | NEXT_N_QUARTERS_N | LAST_N_QUARTERS_N 
	| THIS_YEAR | LAST_YEAR | NEXT_YEAR | NEXT_N_YEARS_N | LAST_N_YEARS_N | THIS_FISCAL_QUARTER 
	| LAST_FISCAL_QUARTER | NEXT_FISCAL_QUARTER | NEXT_N_FISCAL_QUARTERS_N | LAST_N_FISCAL_QUARTERS_N 
	| THIS_FISCAL_YEAR | LAST_FISCAL_YEAR | NEXT_FISCAL_YEAR | NEXT_N_FISCAL_YEARS_N | LAST_N_FISCAL_YEARS_N 
	;

innerCreator
    : NEW type typeArgumentsOrDiamond? '{' expression (',' expression)* '}'
    ;

type
    :   Identifier ('[' ']')*
    ;

typeArguments
    :   '<' typeArgument (',' typeArgument)* '>'
    ;

typeArgument
    :   type
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;

// LEXER

// Keywords

ASC			: A S C;
AVG			: A V G;
BY			: B Y;
CATEGORY	: C A T E G O R Y;
CONVERT_CURRENCY : C O N V E R T C U R R E N C Y;
COUNT		: C O U N T;
COUNT_DISTINCT	: C O U N T '_' D I S T I N C T;
CUBE		: C U B E;
DATA		: D A T A;
DESC		: D E S C;
ELSE		: E L S E;
END			: E N D;
EXCLUDES	: E X C L U D E S;
FIRST		: F I R S T;
FOR			: F O R;
FORMAT      : F O R M A T;
FROM        : F R O M;
GROUP		: G R O U P;
HAVING		: H A V I N G;
IN			: I N;
INCLUDES	: I N C L U D E S;
LAST		: L A S T;
LIKE		: L I K E;
LIMIT		: L I M I T;
MAX			: M A X;
MIN			: M I N;
NOT			: N O T;
NULL		: N U L L;
NULLS		: N U L L S;
OFFSET		: O F F S E T;
ORDER		: O R D E R;
REFERENCE	: R E F E R E N C E;
ROLLUP		: R O L L U P;
SCOPE		: S C O P E;
SELECT      : S E L E C T;
SUM			: S U M;
THEN		: T H E N;
TO_LABEL	: T O SPACE L A B E L;
TRACKING	: T R A C K I N G;
TYPEOF		: T Y P E O F;
UPDATE      : U P D A T E;
USING		: U S I N G;
VIEW		: V I E W;
VIEWSTAT	: V I E W S T A T;
WHEN		: W H E N;
WHERE       : W H E R E;
WITH		: W I T H;

// Apex Keywords
NEW         : N E W;   

// Date Function Literals
CALENDAR_MONTH		: C A L E N D A R '_' M O N T H;
CALENDAR_QUARTER	: C A L E N D A R '_' Q U A R T E R;
CALENDAR_YEAR		: C A L E N D A R '_' Y E A R;
DAY_IN_MONTH		: D A Y '_' I N '_' M O N T H;
DAY_IN_WEEK			: D A Y '_' I N '_' W E E K;
DAY_IN_YEAR			: D A Y '_' I N '_' Y E A R;
DAY_ONLY			: D A Y '_' O N L Y ; 
FISCAL_MONTH		: F I S C A L '_' M O N T H;
FISCAL_QUARTER		: F I S C A L '_' Q U A R T E R;
FISCAL_YEAR			: F I S C A L '_' Y E A R;
HOUR_IN_DAY			: H O U R '_' I N '_' D A Y;
WEEK_IN_MONTH		: W E E K '_' I N '_' M O N T H;
WEEK_IN_YEAR		: W E E K '_' I N '_' Y E A R;


// Date Literals
YESTERDAY			: Y E S T E R D A Y;
TODAY				: T O D A Y;
TOMORROW			: T O M O R R O W;
LAST_WEEK			: L A S T '_' W E E K;
THIS_WEEK			: T H I S '_' W E E K;
NEXT_WEEK			: N E X T '_' W E E K;
LAST_MONTH			: L A S T '_' M O N T H;
THIS_MONTH			: T H I S '_' M O N T H;
NEXT_MONTH			: N E X T '_' M O N T H;
LAST_90_DAYS		: L A S T '_' '90_' D A Y S;
NEXT_90_DAYS		: N E X T '_' '90_' D A Y S;
LAST_N_DAYS_N		: L A S T '_' N '_' D A Y S ':' DecimalIntegerLiteral;
NEXT_N_DAYS_N		: N E X T '_' N '_' D A Y S ':' DecimalIntegerLiteral;
NEXT_N_WEEKS_N		: N E X T '_' N '_' W E E K S':' DecimalIntegerLiteral;
LAST_N_WEEKS_N		: L A S T '_' N '_' W E E K S':' DecimalIntegerLiteral;
NEXT_N_MONTHS_N		: N E X T '_' N '_' M O N T H S':' DecimalIntegerLiteral;
LAST_N_MONTHS_N		: L A S T '_' N '_' M O N T H S':' DecimalIntegerLiteral;
THIS_QUARTER		: T H I S '_' Q U A R T E R;
LAST_QUARTER		: L A S T '_' Q U A R T E R;
NEXT_QUARTER		: N E X T '_' Q U A R T E R;
NEXT_N_QUARTERS_N	: N E X T '_' N '_' Q U A R T E R S':' DecimalIntegerLiteral;
LAST_N_QUARTERS_N	: L A S T '_' N '_' Q U A R T E R S':' DecimalIntegerLiteral;
THIS_YEAR			: T H I S '_' Y E A R;
LAST_YEAR			: L A S T '_' Y E A R;
NEXT_YEAR			: N E X T '_' Y E A R;
NEXT_N_YEARS_N		: N E X T '_' N '_' Y E A R S':' DecimalIntegerLiteral;
LAST_N_YEARS_N		: L A S T '_' N '_' Y E A R S':' DecimalIntegerLiteral;
THIS_FISCAL_QUARTER	: T H I S '_' F I S C A L '_' Q U A R T E R ;
LAST_FISCAL_QUARTER	: L A S T '_' F I S C A L '_' Q U A R T E R ;
NEXT_FISCAL_QUARTER	: N E X T '_' F I S C A L '_' Q U A R T E R ;
NEXT_N_FISCAL_QUARTERS_N :	N E X T '_' N '_' F I S C A L '_' Q U A R T E R S ':' DecimalIntegerLiteral;
LAST_N_FISCAL_QUARTERS_N :	L A S T '_' N '_' F I S C A L '_' Q U A R T E R S ':' DecimalIntegerLiteral;
THIS_FISCAL_YEAR	: T H I S '_' F I S C A L '_' Y E A R ; 
LAST_FISCAL_YEAR	: L A S T '_' F I S C A L '_' Y E A R ; 
NEXT_FISCAL_YEAR	: N E X T '_' F I S C A L '_' Y E A R ; 
NEXT_N_FISCAL_YEARS_N :	N E X T '_' N '_' F I S C A L '_' Y E A R S ':' DecimalIntegerLiteral;
LAST_N_FISCAL_YEARS_N :	L A S T '_' N '_' F I S C A L '_' Y E A R S ':' DecimalIntegerLiteral;

// Timezone Conversion Literal (only used inside Date Function Literals)
CONVERT_TIMEZONE : C O N V E R T T I M E Z O N E;

// Geolocation Literals
DISTANCE : D I S T A N C E;
GEOLOCATION : G E O L O C A T I O N; //can only be used inside DISTANCE()

// Integer Literals

IntegerLiteral
    :   Sign? DecimalIntegerLiteral
    ;

fragment
DecimalIntegerLiteral
    :   DecimalNumeral
    ;

fragment
DecimalNumeral
    :   '0'
    |   NonZeroDigit (Digits? | Underscores Digits)
    ;

fragment
Digits
    :   Digit (DigitOrUnderscore* Digit)?
    ;

fragment
Digit
    :   '0'
    |   NonZeroDigit
    ;

fragment
NonZeroDigit
    :   [1-9]
    ;

fragment
DigitOrUnderscore
    :   Digit
    |   '_'
    ;

fragment
Underscores
    :   '_'+
    ;


// Floating-Point Literals

FloatingPointLiteral
    :  Sign? DecimalFloatingPointLiteral
    ;

fragment
DecimalFloatingPointLiteral
    :   Digits '.' Digits? ExponentPart?
    |   '.' Digits ExponentPart?
    |   Digits ExponentPart
    |   Digits
    ;

fragment
ExponentPart
    :   ExponentIndicator SignedInteger
    ;

fragment
ExponentIndicator
    :   [eE]
    ;

fragment
SignedInteger
    :   Sign? Digits
    ;

fragment
Sign
    :   [+-]
    ;

// Boolean Literals

BooleanLiteral
    :   T R U E
    |   F A L S E
    ;

// Character Literals

CharacterLiteral
    :   '\'' SingleCharacter '\''
    ;

fragment
SingleCharacter
    :   ~['\\]
    ;

// String Literals

StringLiteral
    :   '\'' StringCharacters? '\''
    ;

fragment
StringCharacters
    :   StringCharacter+
    ;

fragment
StringCharacter
    :   ~['\\]
    ;


// The Null Literal

NullLiteral
    :   N U L L
    ;

// SOQL Date Literal, e.g. 2010-09-20T22:16:30.000Z
DateLiteral
	: NonZeroDigit Digit Digit Digit '-'  Digit Digit '-'  Digit Digit 'T'  Digit Digit ':'  Digit Digit ':'  Digit Digit ('.'  Digit Digit?  Digit?)? Letter
	;

// Separators

LPAREN          : '(';
RPAREN          : ')';
LBRACE          : '{';
RBRACE          : '}';
LBRACK          : '[';
RBRACK          : ']';
SEMI            : ';';
COMMA           : ',';
DOT             : '.';

// Operators

ASSIGN          : '=';
GT              : '>';
LT              : '<';
BANG            : '!';
TILDE           : '~';
QUESTION        : '?';
COLON           : ':';
EQUAL           : '==';
LE              : '<=';
GE              : '>=';
NOTEQUAL        : '!=';
AND             : A N D;
OR              : O R;
INC             : '++';
DEC             : '--';
ADD             : '+';
SUB             : '-';
MUL             : '*';
DIV             : '/';
BITAND          : '&';
BITOR           : '|';
CARET           : '^';
MOD             : '%';


// Identifiers (must appear after all keywords in the grammar)

Identifier
    :   Letter LetterOrDigit*
    ;

fragment
Letter : [a-zA-Z$_]; 

fragment
LetterOrDigit : [a-zA-Z0-9$_];

// Whitespace

WS  :  [ \t\r\n\u000C]+ -> skip
    ;

COMMENT
    :   '/*' .*? '*/' -> skip
    ;


// characters

fragment A : [aA];
fragment B : [bB];
fragment C : [cC];
fragment D : [dD];
fragment E : [eE];
fragment F : [fF];
fragment G : [gG];
fragment H : [hH];
fragment I : [iI];
fragment J : [jJ];
fragment K : [kK];
fragment L : [lL];
fragment M : [mM];
fragment N : [nN];
fragment O : [oO];
fragment P : [pP];
fragment Q : [qQ];
fragment R : [rR];
fragment S : [sS];
fragment T : [tT];
fragment U : [uU];
fragment V : [vV];
fragment W : [wW];
fragment X : [xX];
fragment Y : [yY];
fragment Z : [zZ];
fragment SPACE : ' ';

