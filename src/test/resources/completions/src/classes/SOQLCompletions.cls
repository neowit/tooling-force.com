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
		"identities": ["Id", "Name", "Owner", "OwnerId", "AccountNumber", "count", "avg", "sum", "count_distinct", "convertCurrency"]
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
		"identities": ["Name", "Owner", "OwnerId", "AccountNumber", "count", "avg", "sum", "count_distinct", "convertCurrency"],
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
		"lineMarker": "testEmptyWhere",
		"column": 46,
		"itemsCountMin": 3,
		"identities": ["AccountNumber", "AccountSource"],
		"identityMustNotContain": ["Contact"]
		}
	 * #END
	 */
	public void testEmptyWhere() {
		Account a  = [select Id from Account where ];// testEmptyWhere
	}

	/**
	 * #START
		{
		"lineMarker": "testEmptyWhereMultiLine",
		"column": 10,
		"itemsCountMin": 3,
		"identities": ["AccountNumber", "AccountSource"],
		"identityMustNotContain": ["Contact"]
		}
	 * #END
	 */
	public void testEmptyWhereMultiLine() {
		Account a  = [select Id from Account 
			where ];// testEmptyWhereMultiLine
	}
	
	/**
	 * #START
		{
		"lineMarker": "testWhere",
		"column": 69,
		"itemsCountMin": 3,
		"identities": ["AccountNumber", "AccountSource"],
		"identityMustNotContain": ["Contact"]
		}
	 * #END
	 */
	public void testWhere() {
		Account a  = [select Id from Account where CreatedDate < TODAY or ];// testWhere
	}

	/**
	 * #START
		{
		"lineMarker": "testWhereMultiLine",
		"column": 34,
		"itemsCountMin": 3,
		"identities": ["AccountNumber", "AccountSource"],
		"identityMustNotContain": ["Contact"]
		}
	 * #END
	 */
	public void testWhereMultiLine() {
		Account a  = [select Id from Account 
			where CreatedDate < TODAY and ];// testWhereMultiLine
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
		"lineMarker": "testInnerQueryFromWithoutFields",
		"column": 19,
		"itemsCountMin": 3,
		"identities": ["Attachments", "Contacts", "Notes"],
		"identityMustNotContain": ["Id", "Contact", "Account"]
		}
	 * #END
	 */
	public void testInnerQueryFromWithoutFields() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select from ), AccountSource, testInnerQueryFromWithoutFields
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

	/**
	 * #START
		{
		"lineMarker": "testInnerQuerySelectRelationship",
		"column": 22,
		"itemsCountMin": 10,
		"identities": ["AccountNumber", "AccountSource", "Id", "BillingCity", "CreatedBy"],
		"identityMustNotContain": ["Birthdate"]
		}
	 * #END
	 */
	public void testInnerQuerySelectRelationship() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select Account., ,testInnerQuerySelectRelationship
			   from Contacts), AccountSource 
				from Account
				where CreatedDate = TODAY ];
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQueryWhere",
		"column": 27,
		"itemsCountMin": 3,
		"identities": ["Birthdate", "AssistantName", "Id", "Email", "Account"]
		}
	 * #END
	 */
	public void testInnerQueryWhere() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, AssistantName
			   from Contacts where ), AccountSource, testInnerQueryFrom
				from Account
				where CreatedDate = TODAY ];
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQueryWherePartial",
		"column": 30,
		"itemsCountMin": 3,
		"identities": ["CreatedDate", "CreatedBy"],
		"identityMustNotContain": ["Birthdate", "AssistantName", "Id"]
		}
	 * #END
	 */
	public void testInnerQueryWherePartial() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId, AssistantName
			   from Contacts where Cre), AccountSource, testInnerQueryWherePartial
				from Account
				where CreatedDate = TODAY ];
	}
	
	/**
	 * #START
		{
		"lineMarker": "testInnerQueryWhereSingleline",
		"column": 128,
		"itemsCountMin": 3,
		"identities": ["Birthdate", "AssistantName", "Id", "Email", "Account"]
		}
	 * #END
	 */
	public void testInnerQueryWhereSingleline() {
		final List<My_Object__c> accs = [ select AccountNumber, (select AccountId, testInnerQueryWhereSingleline from Contacts where ), AccountSource from Account];
	}

	/**
	 * #START
		{
		"lineMarker": "testInnerQueryWhereMiddleField",
		"column": 64,
		"itemsCountMin": 3,
		"identities": ["Birthdate", "AssistantName", "Id", "Email", "Account"]
		}
	 * #END
	 */
	public void testInnerQueryWhereMiddleField() {
		final List<My_Object__c> accs = [ select AccountNumber,
			  (select AccountId from Contacts where AccountId = '1' and  and CreatedBy.AboutMe = ''), testInnerQueryWhereMiddleField 
				from Account
				where CreatedDate = TODAY ];
	}

	/**
	 * #START
		{
		"lineMarker": "testBoundVariableSingleLine",
		"column": 87,
		"itemsCountMin": 2,
		"identities": ["publicProperty", "getPublicString"],
		"identityMustNotContain": ["getPrivateStringStatically", "privateProperty"]
		}
	 * #END
	 */
	public void testBoundVariableSingleLine() {
		OtherClass1 some;

		final List<My_Object__c> accs = [select Id from Account where FieldName like : some.]; // testBoundVariableSingleLine
	}

	/**
	 * #START
		{
		"lineMarker": "testBoundVariableMultiLine",
		"column": 32,
		"itemsCountMin": 2,
		"identities": ["publicProperty", "getPublicString"],
		"identityMustNotContain": ["getPrivateStringStatically", "privateProperty"]
		}
	 * #END
	 */
	public void testBoundVariableMultiLine() {
		OtherClass1 some;

		final List<My_Object__c> accs = [select Id from Account 
			where FieldName like : some.]; // testBoundVariableMultiLine
	}

	/**
	 * #START
		{
		"lineMarker": "testBoundVariableInnerSoql",
		"column": 25,
		"itemsCountMin": 2,
		"identities": ["publicProperty", "getPublicString"],
		"identityMustNotContain": ["getPrivateStringStatically", "privateProperty"]
		}
	 * #END
	 */
	public void testBoundVariableInnerSoql() {
		OtherClass1 some;

		final List<My_Object__c> accs = [ select AccountNumber, (select Id from Contacts
					where Name = :some.), testBoundVariableInnerSoql
				from Account]; 
	}

}