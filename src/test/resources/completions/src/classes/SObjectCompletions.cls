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
}