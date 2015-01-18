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
}