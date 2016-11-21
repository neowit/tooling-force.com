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


}
