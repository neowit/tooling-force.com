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

	public void completeList() {
		final List<String> lst = new List<String>();
		lst.;
	}

	public void completeSet() {
		final Set<String> sset = new Set<String>();
		sset.;
	}

	public void completeMap() {
		final Map<String, Integer> mmap = new Map<String, Integer>();
		mmap.;

	}

	public void completeArray() {
		ApexClassCompletionsester[] testers;
		testers.;
	}

	public void completeSystemString() {
		String str= 'abcd';
		str.;
	}

	public void completeSystemDate() {
		Date d;
		d.;
	}

	public void completeSystemDateTime() {
		Datetime d;
		d.;
	}

	public void completeException() {
		try {
		} catch (Exception e) {
			e.;
		} catch (System.DmlException e) {
			e.;
		} catch (System.CalloutException e) {
			e.;
		}
		
	}
	
}

