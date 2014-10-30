/**
 * description of test attributes
 * line - search for line which ends with specified substring
 * column - exact column where caret is located
 * itemsCountMin - expect at least this many completion candidates
 * items - expect each of the listed identities in the completion candidate list
 *
 */

public with sharing class ApexClassCompletions {
	//static field
	private static Integer STATIC_INT = 0;
	public enum Season {WINTER, SPRING, SUMMER, FALL}
	
	//static property
	public static Integer propStatic { public get; set; }

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
		"lineMarker": "testStaticField",
		"column": 14,
		"itemsCountMin": 1,
		"signatureContains": ["format"],
		"signatureMustNotContain" : ["valueOf"]
		}
	 * #END
	 */
	public void testStaticField () {
		STATIC_INT.; //testStaticField
	}
    
	/**
	 * #START
		{
		"lineMarker": "testStaticProperty",
		"column": 14,
		"itemsCountMin": 1,
		"signatureContains": ["format"],
		"signatureMustNotContain" : ["valueOf"]
		}
	 * #END
	 */
	public void testStaticProperty () {
		propStatic.; //testStaticProperty
	}
	/**
	 * #START
		{
		"lineMarker": "testStaticClassRef",
		"column": 24,
		"itemsCountMin": 6,
		"signatureContains": ["STATIC_INT", "futureTest", "Season", "InnerClass1", "InnerClass2", "InnerClass3", "MyInnerInterface"]
		}
	 * #END
	 */
	public void testStaticClassRef () {
		ApexClassCompletions.; //testStaticClassRef
		
	}

	/**
	 * #START
		{
		"lineMarker": "testClassInstanceRef",
		"column": 7,
		"itemsCountMin": 5,
		"signatureContains": ["propShort", "propLong", "test1", "testStaticClassRef", "completeTwoLevels", "publicField"],
		"signatureMustNotContain" : ["testClassInstanceRef", "futureTest", "STATIC_INT"]
		}
	 * #END
	 */
	static void testClassInstanceRef() {
		final ApexClassCompletions cls;
		cls.; //testClassInstanceRef

	}

	/**
	 * #START
		{
		"lineMarker": "testOtherClassInstanceRef",
		"column": 7,
		"itemsCountMin": 2,
		"signatureContains": ["getPublicString", "publicProperty"],
		"signatureMustNotContain" : ["getPrivateString", "privateProperty", "getPublicStringStatically"]
		}
	 * #END
	 */
	static void testOtherClassInstanceRef() {
		final OtherClass1 cls;
		cls.; //testOtherClassInstanceRef

	}

	/**
	 * #START
		{
		"lineMarker": "testOtherInterfaceInstanceRef",
		"column": 7,
		"itemsCountMin": 2,
		"signatureContains": ["interfaceMethod1", "interfaceMethod2"]
		}
	 * #END
	 */
	static void testOtherInterfaceInstanceRef() {
		final MyInterface cls;
		cls.; //testOtherInterfaceInstanceRef

	}

	@future(callout=true)
	static void futureTest() {

	}

	/**
	 * #START
		{
		"lineMarker": "completeOtherInnerInstance",
		"column": 8,
		"itemsCountMin": 2,
		"signatureContains": ["inner1_public_Integer", "inner1Method1"],
		"signatureMustNotContain" : ["inner1_private_Boolean"]
		}
	 * #END
	 */
	public void completeOtherInnerInstance() {
		OtherClass1.Inner1 inn1;
		inn1.;//completeOtherInnerInstance
	}

	/**
	 * #START
		{
		"lineMarker": "completeTwoLevels",
		"column": 8,
		"itemsCountMin": 2,
		"signatureContains": ["inner1_public_Integer", "inner1Method1"],
		"signatureMustNotContain" : ["inner1_private_Boolean"]
		}
	 * #END
	 */
	public void completeTwoLevels() {
		OtherClass1.Inner2 inn2;
		inn2.inner2_ref_to_Inner1.;//completeTwoLevels
	}

	/**
	 * #START
		{
		"lineMarker": "completeMultiLevel",
		"column": 71,
		"itemsCountMin": 2,
		"signatureContains": ["inner3_public_Integer", "inner3_ref_to_Inner2"],
		"signatureMustNotContain" : ["inner2_public_Boolean", "inner1_public_Integer"]
		}
	 * #END
	 */
	public void completeMultiLevel() {
		OtherClass1.Inner2 inn2;
		inn2.inner2_ref_to_Inner3.inner3_ref_to_Inner2.inner2_ref_to_Inner3.;//completeMultiLevel
	}
	
	public final Map<String, String> publicField = new Map<String, String>{'Something' => 'Other'};

	/**
	 * #START
		{
		"lineMarker": "completeInner",
		"column": 7,
		"itemsCountMin": 3,
		"signatureContains": ["inner1Str1", "String method1", "void method1"],
		"signatureMustNotContain" : ["privateStr", "innerClass3"]
		}
	 * #END
	 */
	public void completeInner() {
		ApexClassCompletions.InnerClass1 cls = new ApexClassCompletions.InnerClass1();
		System.debug('agX ');
		cls.; //completeInner
	}

	/**
	 * #START
		{
		"lineMarker": "completeInnerInterface",
		"column": 7,
		"itemsCountMin": 2,
		"signatureContains": ["innerInterfaceMethod1_String", "innerInterfaceMethod2_Int"]
		}
	 * #END
	 */
	public void completeInnerInterface() {
		ApexClassCompletions.MyInnerInterface itf;
		System.debug('agX ');
		itf.; //completeInnerInterface
	}
	/**
	 * #START
		{
		"lineMarker": "completeInnerWithInheritance",
		"column": 7,
		"itemsCountMin": 4,
		"signatureContains": ["inner1Str1", "String method1", "override public void method1", "method2Super"],
		"signatureMustNotContain" : ["public virtual void method1", "innerClass3"]
		}
	 * #END
	 */
	public void completeInnerWithInheritance() {
		InnerClass2 cls = new ApexClassCompletions.InnerClass2();
		System.debug('agX ');
		cls.;//completeInnerWithInheritance
	}
	
	public void afterComplete1() {
		System.debug('agX some text');
	
	}
	
	public interface MyInnerInterface {
		String innerInterfaceMethod1_String(final Boolean param1, String param2);
		Integer innerInterfaceMethod2_Int();
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

