/**
 * this class definition is used in ApexClassCompletions.cls tests
 */
public with sharing class OtherClass1 {

    private Long lng;
	private String privateProperty {get; set;}
	public String publicProperty {get; set;}

	private static String getPrivateStringStatically()	{
		return '';
	}

	public static String getPublicStringStatically()	{
		return '';
	}

	private String getPrivateString()	{
		return '';
	}

	public String getPublicString()	{
		return '';
	}

	public class Inner1 {
		private Boolean inner1_private_Boolean;
		public Integer inner1_public_Integer;
		public void inner1Method1() {}
	}

	public class Inner2 {
		public Boolean inner2_public_Boolean;
		public Inner1 inner2_ref_to_Inner1;

		public Inner3 inner2_ref_to_Inner3;//circular link
	}

	public class Inner3 {
		public Integer inner3_public_Integer;

		public Inner2 inner3_ref_to_Inner2;//circular link
	}
}

