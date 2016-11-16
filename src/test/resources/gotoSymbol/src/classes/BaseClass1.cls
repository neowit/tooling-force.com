public virtual class BaseClass1 implements BaseInterface1 {
    public String methodSingle() {
        return methodMultiple('');
    }
    
    public String methodMultiple(final String val) {
        return methodMultiple(1);
    }

    public String methodMultiple(final Integer val) {
        this.methodSingle();
    }


    public String methodMultiple(final String val1, final Integer val2) {
        return '';
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
		"identityContains": ["methodMultiple(final String val)", "methodMultiple(final Integer val)", "methodMultiple(final String val1, final Integer val2"],
		"identityMustNotContain" : ["methodSingle"]
		}
	 * #END
     */
    public String test_methodMultiple(final Integer val) {
        this.methodMultiple(); //test_methodMultiple
    }

}
