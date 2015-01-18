/**
 * description of test attributes
 * line - search for line which ends with specified substring
 * column - exact column where caret is located
 * itemsCountMin - expect at least this many completion candidates
 * identities - expect each of the listed identities in the completion candidate list
 * identityMustNotContain - do NOT expect any of the listed identities in the completion candidate list
 *
 */
public with sharing class SOQLCompletions {

	/**
	 * #START
		{
		"lineMarker": "testFirstFieldInSelect",
		"column": 24,
		"itemsCountMin": 30,
		"identities": ["Id", "Name", "Owner", "OwnerId", "AccountNumber"]
		}
	 * #END
	 */
	public void testFirstFieldInSelect() {
		Account a  = [select  from Account];// testFirstFieldInSelect
	}

	/**
	 * #START
		{
		"lineMarker": "testSecondFieldInSelect",
		"column": 27,
		"itemsCountMin": 30,
		"identities": ["Name", "Owner", "OwnerId", "AccountNumber"],
		"identityMustNotContain": ["Id"]
		}
	 * #END
	 */
	public void tesSecondFieldInSelect() {
		Account a  = [select Id, from Account];// testSecondFieldInSelect
	}

	/**
	 * #START
		{
		"lineMarker": "testFromObjectList",
		"column": 32,
		"itemsCountMin": 30,
		"identities": ["Account", "Contact"],
		"identityMustNotContain": ["Id"]
		}
	 * #END
	 */
	public void testFromObjectList() {
		Account a  = [select Id from ];// testFromObjectList
	}

	/**
	 * #START
		{
		"lineMarker": "testFromObjectListMultiLine",
		"column": 9,
		"itemsCountMin": 30,
		"identities": ["Account", "Contact"],
		"identityMustNotContain": ["Id"]
		}
	 * #END
	 */
	public void testFromObjectListMultiLine() {
		Account a  = [select 
			Id 
			from ];// testFromObjectListMultiLine
	}

	/**
	 * #START
		{
		"lineMarker": "testPartialFromObjectList",
		"column": 35,
		"itemsCountMin": 3,
		"identities": ["Account", "AccountContactRole"],
		"identityMustNotContain": ["Id", "Contact"]
		}
	 * #END
	 */
	public void testPartialFromObjectList() {
		Account a  = [select Id from Acc];// testPartialFromObjectList
	}

	/**
	 * #START
		{
		"lineMarker": "testPartialFromObjectListMultiLine",
		"column": 12,
		"itemsCountMin": 3,
		"identities": ["Account", "AccountContactRole"],
		"identityMustNotContain": ["Id", "Contact"]
		}
	 * #END
	 */
	public void testPartialFromObjectListMultiLine() {
		Account a  = [select Id 
			from Acc];// testPartialFromObjectListMultiLine
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQueryFrom",
		"column": 12,
		"itemsCountMin": 3,
		"identities": ["Attachments", "Contacts", "Notes"],
		"identityMustNotContain": ["Id", "Contact", "Account"]
		}
	 * #END
	 */
	public void testInnerQueryFrom() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, AssistantName
			   from ), AccountSource, testInnerQueryFrom
				from Account
				where CreatedDate = TODAY ];
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQueryFromPartial",
		"column": 15,
		"itemsCountMin": 2,
		"identities": ["Contacts", "Contracts"],
		"identityMustNotContain": ["Attachments", "Notes", "Id"]
		}
	 * #END
	 */
	public void testInnerQueryFromPartial() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, AssistantName
			   from Con), AccountSource, testInnerQueryFromPartial
				from Account
				where CreatedDate = TODAY ];
	}
	
	/**
	 * #START
		{
		"lineMarker": "testInnerQuerySelectLastField",
		"column": 56,
		"itemsCountMin": 3,
		"identities": ["Birthdate", "AssistantName", "Id", "Email", "Account"],
		"identityMustNotContain": ["AccountId"]
		}
	 * #END
	 */
	public void testInnerQuerySelectLastField() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, testInnerQuerySelectLastField, 
			   from Contacts), AccountSource 
				from Account
				where CreatedDate = TODAY ];
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQuerySelectMidleField",
		"column": 25,
		"itemsCountMin": 3,
		"identities": ["Birthdate", "AssistantName", "Id", "Email", "Account"],
		"identityMustNotContain": ["AccountId"]
		}
	 * #END
	 */
	public void testInnerQuerySelectMidleField() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, ,testInnerQuerySelectMidleField
			   from Contacts), AccountSource 
				from Account
				where CreatedDate = TODAY ];
	}
}