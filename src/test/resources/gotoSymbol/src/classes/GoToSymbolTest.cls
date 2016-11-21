public virtual class GoToDefinitionTest extends BaseClass1 {
    public String methodSingle() {
        return methodMultiple('');
    }

    public override String methodMultiple(final Integer valGoToDefinitionTest) {
        this.methodSingle();
    }

    virtual class InnerClass1 {
        public void method_one() {}; 
        public void method_many() {}; 
        public void method_many(String s) {}; 
        public void method_many(Integer i) {}; 
    }

    public class InnerClass2 extends InnerClass1 {
        public void method_many(String sInnerClass2) {}; 
    }

    ///////////////////////////////////// TESTS /////////////////////////////////
    /**
	 * #START
		{
		"lineMarker": "test_methodSingle",
		"column": 23,
		"itemsCountMin": 1,
		"identityContains": ["methodSingle"],
		"identityMustNotContain" : ["methodMultiple"]
		}
	 * #END
     */
    public String test_methodSingle(final Integer val) {
        this.methodSingle(); //test_methodSingle
    }

    /**
	 * #START
		{
		"lineMarker": "test_methodMultiple",
		"column": 23,
		"itemsCountMin": 3,
		"identityContains": ["methodMultiple(final String val)", "methodMultiple(final Integer valGoToDefinitionTest)", "methodMultiple(final String val1, final Integer val2"],
		"identityMustNotContain" : ["methodSingle"]
		}
	 * #END
     */
    public String test_methodMultiple(final Integer val) {
        this.methodMultiple(); //test_methodMultiple
    }

    /**
	 * #START
		{
		"lineMarker": "test_methodMultiple_InnerClass2",
		"column": 23,
		"itemsCountMin": 3,
		"identityContains": ["method_many(String sInnerClass2)", "method_many(Integer i)", "method_many()"],
		"identityMustNotContain" : ["methodSingle"]
		}
	 * #END
     */
    public String test_methodMultiple_InnerClass2(final Integer val) {
        InnerClass2 cls = new InnerClass2();
        cls.method_many(); //test_methodMultiple_InnerClass2
    }
}
