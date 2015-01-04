/**
 * description of test attributes
 * line - search for line which ends with specified substring
 * column - exact column where caret is located
 * itemsCountMin - expect at least this many completion candidates
 * items - expect each of the listed identities in the completion candidate list
 *
 */

public with sharing class ApexClassCompletions {
	private static Integer STATIC_INT = 0;
	public enum Season {WINTER, SPRING, SUMMER, FALL}

	public String propShort { public get; set; }
	
	public Double propLong {get; 
						private set { 
							propLong = value;
						} 
	}

    void test1() {}
    //void test24() {}

	/**
	 * #START
		{
		"lineMarker": "testMultipleLocalVariableDeclaration",
		"column": 6,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
		}
	 * #END
	 */
	private void testMultipleLocalVariableDeclaration() {
		String a1='a', b1='b';
		b1.;//testMultipleLocalVariableDeclaration
	}
    
	/**
	 * #START
		{
		"lineMarker": "testStaticString",
		"column": 10,
		"itemsCountMin": 17,
		"signatureMustNotContain": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureContains" : ["escapeSingleQuotes", "format", "valueOf", "join", "isEmpty"]
		}
	 * #END
	 */
	@isTest public void testStaticString () {
		String.;//testStaticString
		
	}

	/**
	 * #START
		{
		"lineMarker": "testStaticStringWithPrefix",
		"column": 12,
		"itemsCountMin": 1,
		"signatureMustNotContain": ["abbreviate(Integer maxWidth", "isEmpty", "equals"],
		"signatureContains" : ["join"]
		}
	 * #END
	 */
	@isTest public void testStaticStringWithPrefix () {
		String.jo;//testStaticStringWithPrefix
		
	}

	/**
	 * #START
		{
		"lineMarker": "testProperty",
		"column": 12,
		"itemsCountMin": 4,
		"signatureContains": ["format", "intValue", "longValue", "round"],
		"signatureMustNotContain" : ["valueOf"]
		}
	 * #END
	 */
	@isTest public void testProperty () {
		propLong.;//testProperty
	}

	/**
	 * #START
		{
		"lineMarker": "testEnum",
		"column": 10,
		"itemsCountMin": 4,
		"signatureContains": ["WINTER", "SPRING", "SUMMER", "FALL", "values()"],
		"signatureMustNotContain" : ["name()"]
		}
	 * #END
	 */
	@isTest public void testEnum () {
		Season.;//testEnum
	}

	/**
	 * #START
		{
		"lineMarker": "testEnumConstant",
		"column": 17,
		"itemsCountMin": 2,
		"signatureContains": ["name", "ordinal"],
		"signatureMustNotContain" : ["values"]
		}
	 * #END
	 */
	@isTest public void testEnum () {
		Season.WINTER.;//testEnumConstant
	}

	/**
	 * #START
		{
		"lineMarker": "completeFor",
		"column": 9,
		"itemsCountMin": 1,
		"signatureContains": ["format"],
		"signatureMustNotContain" : ["valueOf"]
		}
	 * #END
	 */
	private void completeFor() {
		for(Integer i =0; i < 10; i++) {
		    i.;//completeFor
		}
	}

	/**
	 * #START
		{
		"lineMarker": "completeForEnhanced",
		"column": 11,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
		}
	 * #END
	 */
	private void completeForEnhanced() {
		for(String str : strings) {
		    str.;//completeForEnhanced
		}
	}

	/**
	 * #START
		{
		"lineMarker": "completeList",
		"column": 7,
		"itemsCountMin": 17,
		"signatureContains": ["addAll(List fromList)", "addAll(Set fromSet)", "add(", "clear", "clone", "get", "sort"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf", "abbreviate"]
		}
	 * #END
	 */
	public void completeList() {
		final List<String> lst = new List<String>();
		lst.;//completeList
	}


	/**
	 * #START
		{
		"lineMarker": "completeArray",
		"column": 11,
		"itemsCountMin": 17,
		"signatureContains": ["addAll", "add(", "clear", "clone", "get", "sort"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf", "abbreviate"]
		}
	 * #END
	 */
	public void completeArray() {
		ApexClassCompletionsester[] testers;
		testers.; //completeArray
	}

	/**
	 * #START
		{
		"lineMarker": "completeSet",
		"column": 8,
		"itemsCountMin": 17,
		"signatureContains": ["addAll(List<Object> fromList)", "addAll(Set<Object> fromSet)", "add(", "clear", "Set<Object> clone", "removeAll"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf", "abbreviate", "get", "sort"]
		}
	 * #END
	 */
	public void completeSet() {
		final Set<String> sset = new Set<String>();
		sset.;//completeSet
	}

	/**
	 * #START
		{
		"lineMarker": "completeSetLowerCase",
		"column": 8,
		"itemsCountMin": 17,
		"signatureContains": ["addAll(List<Object> fromList)", "addAll(Set<Object> fromSet)", "add(", "clear", "Set<Object> clone", "removeAll"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf", "abbreviate", "get", "sort"]
		}
	 * #END
	 */
	public void completeSetLowerCase() {
		//check clash of set collection name with 'set' of property setter
		final set<String> sset = new sEt<String>();
		sset.;//completeSetLowerCase
	}

	/**
	 * #START
		{
		"lineMarker": "completeMap",
		"column": 8,
		"itemsCountMin": 16,
		"signatureContains": ["get", "put(", "values", "clear", "clone", "putAll", "deepClone"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf", "abbreviate", "sort", "add"]
		}
	 * #END
	 */
	public void completeMap() {
		final Map<String, Integer> mmap = new Map<String, Integer>();
		mmap.;//completeMap

	}

	/**
	 * #START
		{
		"lineMarker": "completeSystemDate",
		"column": 8,
		"itemsCountMin": 7,
		"signatureMustNotContain": ["addDays", "addMonths", "addYears", "format"],
		"signatureContains" : ["newInstance", "valueOf"]
		}
	 * #END
	 */
	public void completeSystemDate() {
		Date.;//completeSystemDate
	}
	
	/**
	 * #START
		{
		"lineMarker": "completeSystemDateInstance",
		"column": 5,
		"itemsCountMin": 10,
		"signatureContains": ["addDays", "addMonths", "addYears", "format"],
		"signatureMustNotContain" : ["newInstance", "getTime", "valueOf"]
		}
	 * #END
	 */
	public void completeSystemDateInstance() {
		Date d;
		d.;//completeSystemDateInstance
	}

	/**
	 * #START
		{
		"lineMarker": "completeSystemDateTimeInstance",
		"column": 5,
		"itemsCountMin": 10,
		"signatureContains": ["addDays", "addMonths", "addYears", "format", "getTime"],
		"signatureMustNotContain" : ["newInstance", "valueOf"]
		}
	 * #END
	 */
	public void completeSystemDateTimeInstance() {
		Datetime d;
		d.;//completeSystemDateTimeInstance
	}

	/**
	 * #START
		{
		"lineMarker": "completeSystemNow",
		"column": 16,
		"itemsCountMin": 10,
		"signatureContains": ["addDays", "addMonths", "addYears", "format", "getTime"],
		"signatureMustNotContain" : ["newInstance", "valueOf"]
		}
	 * #END
	 */
	public void completeSystemNow() {
		System.now().; //completeSystemNow
	}

	/**
	 * #START
		{
		"lineMarker": "completeException",
		"column": 6,
		"itemsCountMin": 7,
		"signatureContains": ["getCause", "getLineNumber", "getMessage"],
		"signatureMustNotContain" : ["getDmlMessage"]
		}
	 * #END
	 */

	/**
	 * #START
		{
		"lineMarker": "completeDmlException",
		"column": 6,
		"itemsCountMin": 14,
		"signatureContains": ["getCause", "getLineNumber", "getMessage", "getDmlMessage"]
		}
	 * #END
	 */

	/**
	 * #START
		{
		"lineMarker": "completeCalloutException",
		"column": 6,
		"itemsCountMin": 7,
		"signatureContains": ["getCause", "getLineNumber", "getMessage"],
		"signatureMustNotContain" : ["getDmlMessage"]
		}
	 * #END
	 */
	public void completeException() {
		try {
		} catch (Exception e) {
			e.; //completeException
		} catch (System.DmlException e) {
			e.; //completeDmlException
		} catch (System.CalloutException e) {
			e.; //completeCalloutException
		}
		
	}
	
	/**
	 * #START
		{
		"lineMarker": "completeStaticApexTypeReference",
		"column": 13,
		"itemsCountMin": 8,
		"signatureContains": ["Action", "IdeaStandardController", "Message", "StandardController", "currentPage", "addMessage"]
		}
	 * #END
	 */
	public void completeStaticApexTypeReference() {
		ApexPages.; //completeStaticApexTypeReference
	}

	/**
	 * #START
		{
		"lineMarker": "completeApexModelEnum",
		"column": 22,
		"itemsCountMin": 6,
		"signatureContains": ["CONFIRM", "ERROR", "FATAL", "WARNING", "INFO", "values"]
		}
	 * #END
	 */
	public void completeApexModelEnum() {
		ApexPages.Severity.; //completeApexModelEnum
	}
}

