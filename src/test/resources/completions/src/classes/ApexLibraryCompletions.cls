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
		"signatureContains": ["abbreviate(Integer maxWidth", "abbreviate(Integer maxWidth, Integer offset", "capitalize", "center", "charAt", "compareTo", "equals"]
		"signatureDoesNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
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
		"column": 6,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth", "abbreviate(Integer maxWidth, Integer offset", "capitalize", "center", "charAt", "compareTo", "equals"]
		}
	 * #END
	 */
	@isTest public void testStaticString () {
		String.;
		
	}

	@isTest public void testProperty () {
		propLong.;
	}

	@isTest public void testEnum () {
		Season.;
	}

	private void completeFor() {
		for(Integer i =0; i < 10; i++) {
		    i.;
		}
	}

	private void completeForEnhanced() {
		for(String str : strings) {
		    str.;
		}
	}

	@future(callout=true)
	static void futureTest() {
		final AccountController acc = new AccountController();
		acc.;

	}

	public void completeMultiLevel() {
		SFDCMyJLCustomerTypes.InteractionContent code;
		//code.ContactMethodTypeCode.
		code.ContactMethodTypeCode.;
	}
	
	public Boolean soqlCheck(final String param1, Map<Id, String> param2) {
		final List<Account> accounts = [select Id, Name from Account limit 1];

		for(Account acc : [select Id, (select Name from Contacts) from Account]) {
			//do something
			acc.;
		    
		}
		return false;
	}

	public final Map<String, String> sfdcAddressTypeByWSAddressType = new Map<String, String>{'Billing' => 'Mailing'};
	final Map<String, String> maptTest2 = new Map<String, String>{'Billing1' => new Map<String, String>{'inner1' => 12.34}, 'Shipping1' => 'Mailing1'};
	final Set<String> setTest2 = new Set<String>{'Billing2', 'Shipping2', 'Mailing2'};

	public void completeInner() {
		ApexClassCompletions.InnerClass1 cls = new ApexClassCompletions.InnerClass1();
		System.debug('agX ');
		cls.;
	}

	public void completeInnerWithInheritance() {
		InnerClass2 cls = new ApexClassCompletions.InnerClass2();
		System.debug('agX ');
		cls.;
	}
	
	public void complete2Local() {
		ApexClassCompletions cls = new ApexClassCompletions();
		System.debug('agX ');
		cls.;
	}

	public void complete2LocalStatic() {
		ApexClassCompletions.;
	}
	
	public void completeGlobalInner() {
		ApexClassCompletionsester.InnerClass cls = new ApexClassCompletionsester.InnerClass();
		System.debug('agX ');
		cls.;
	}
	
	public void completeGlobal() {
		ApexClassCompletionsester cls = new ApexClassCompletionsester();
		System.debug('agX ');
		cls.;
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


	public void afterComplete1() {
		System.debug('agX some text');
	
	}

	public virtual class InnerClass1 {
		public final String inner1Str1;
		private final String privateStr;
		InnerClass3 innerClass3;

		public virtual void method1(String str) {return;}
		public virtual String method1(String str, Boolean b) {return '1, 1';}
	}
	public class InnerClass2 extends InnerClass1 {
		public final String inner2Str1;
		
		override public void method1(String str) { 
			super.;
			return;
		}
		public void method2Super(String str) { 
			this.;
			return;
		}
	}
	public virtual class InnerClass3 extends ApexClassCompletions.InnerClass2 {
		public final String inner3Str1;
		public final String inner3Str2;

	}
	
}

