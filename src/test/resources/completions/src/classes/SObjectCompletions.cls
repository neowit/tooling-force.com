/**
 * description of test attributes
 * line - search for line which ends with specified substring
 * column - exact column where caret is located
 * itemsCountMin - expect at least this many completion candidates
 * items - expect each of the listed identities in the completion candidate list
 *
 */
public with sharing class SObjectCompletions {

	/**
	 * #START
		{
		"lineMarker": "test_My_Object__r",
		"column": 63,
		"itemsCountMin": 17,
		"identities": ["CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Email__c", "Name", "Owner", "OwnerId"]
		}
	 * #END
	 */
	public void testSObjectNewWithRelationship() {
		Account a  = new Account(AccountNumber = '10', My_Object__r.);// test_My_Object__r
	}

	/**
	 * #START
		{
		"lineMarker": "testSObjectNewWithMultiLevelRelationship",
		"column": 74,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
		}
	 * #END
	 */
	public void testSObjectNewWithMultiLevelRelationship() {
		Account a  = new Account(AccountNumber = '10', CreatedBy.Contact.Email.);// testSObjectNewWithMultiLevelRelationship
	}

	/**
	 * #START
		{
		"lineMarker": "testDirectSObjecFieldReference",
		"column": 19,
		"itemsCountMin": 4,
		"signatureContains": ["format", "intValue", "longValue", "round"],
		"signatureMustNotContain" : ["valueOf"]
		}
	 * #END
	 */
	public void testDirectSObjecFieldReference() {
		Account a = new Account();
		a.AnnualRevenue.;// testDirectSObjecFieldReference
	}
	
	/**
	 * #START
		{
		"lineMarker": "testSObjecReferenceWithMultiLevelRelationship",
		"column": 29,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
		}
	 * #END
	 */
	public void testSObjecReferenceWithMultiLevelRelationship() {
		Account a = new Account();
		a.CreatedBy.Contact.Email.;// testSObjecReferenceWithMultiLevelRelationship
	}

	/**
	 * #START
		{
		"lineMarker": "test_NewEmpty",
		"column": 28,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	public void testSObjectNewEmpty() {
		Account a  = new Account();// test_NewEmpty
	}

	/**
	 * #START
		{
		"lineMarker": "test_NewEnd",
		"column": 39,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	public void testSObjectNewEnd() {
		Account a  = new Account(Name = '', ); //test_NewEnd
	}

	/**
	 * #START
		{
		"lineMarker": "testSobjectNewComplex",
		"column": 32,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	void testSobjectCreatorComplex() {
		final List<Account> accs = new List<Account>{
			new Account( Name = 'Test1', ), //testSobjectNewComplex
			new Account( Name = 'Test2')
		};
	}

	/**
	 * #START
		{
		"lineMarker": "test_NewBetween",
		"column": 39,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	public void testSObjectNewBetween() {
		Account a  = new Account(Name = '',  , Active = true);//test_NewBetween
	}

	/**
	 * #START
		{
		"lineMarker": "test_NewBetweenStartsWith",
		"column": 46,
		"itemsCountMin": 2,
		"identities": ["AccountNumber", "AccountSource"]
		}
	 * #END
	 */
	public void testSObjectNewBetweenStartsWith() {
		Account a  = new Account(Name = '', Account  , Active = true);//test_NewBetweenStartsWith
	}

	/**
	 * #START
		{
		"lineMarker": "testSObjecCreatorInsideList",
		"column": 34,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	public void testSObjecCreatorInsideList() {
		List<Account> manyAccounts = new List<Account>{
					new Account (Name = 'Some', ) /*testSObjecCreatorInsideList*/
				};
	}

	/**
	 * #START
		{
		"lineMarker": "testSObjecCreatorInsideMap",
		"column": 44,
		"itemsCountMin": 40,
		"identities": ["AccountNumber", "Active__c", "CreatedBy", "CreatedById", "CreatedDate", "IsDeleted", "Name", "Owner", "OwnerId", "My_Object__r", "My_Object__c"]
		}
	 * #END
	 */
	public void testSObjecCreatorInsideList() {
		Map<String, Account> manyAccounts = new Map<String, Account>{
					'Acc1' => new Account (Name = 'Some', ) /*testSObjecCreatorInsideMap*/
				};
	}

	/**
	 * #START
		{
		"lineMarker": "testLocalVariableInsideCreator",
		"column": 38,
		"itemsCountMin": 40,
		"signatureContains": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureMustNotContain" : ["escapeSingleQuotes", "format", "valueOf"]
		}
	 * #END
	 */
	private void testLocalVariableInsideCreator() {
		String a1='a', b1='b';
		Account a  = new Account(Name = b1. , Active = true);//testLocalVariableInsideCreator
	}
    
	/**
	 * #START
		{
		"lineMarker": "testStaticStringInsideCreator",
		"column": 42,
		"itemsCountMin": 17,
		"signatureMustNotContain": ["abbreviate(Integer maxWidth)", "abbreviate(Integer maxWidth, Integer offset)", "capitalize", "center", "charAt", "compareTo", "equals"],
		"signatureContains" : ["escapeSingleQuotes", "format", "valueOf", "join", "isEmpty"]
		}
	 * #END
	 */
	@isTest public void testStaticStringInsideCreator () {
		Account a  = new Account(Name = String. );//testStaticStringInsideCreator
		
	}

	/**
	 * #START
		{
		"lineMarker": "testObjectsNotReturnedinListMetadata",
		"column": 7,
		"itemsCountMin": 15,
		"identities": ["EndTime", "NextFireTime", "CronExpression"]
		}
	 * #END
	 */
	@isTest public void testObjectsNotReturnedinListMetadata () {
		CronTrigger job;
		job.;//testObjectsNotReturnedinListMetadata

		
	}
}