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
		"lineMarker": "tesSecondFieldInSelect",
		"column": 27,
		"itemsCountMin": 30,
		"identities": ["Name", "Owner", "OwnerId", "AccountNumber"],
		"identityMustNotContain": ["Id"]
		}
	 * #END
	 */
	public void tesSecondFieldInSelect() {
		Account a  = [select Id, from Account];// tesSecondFieldInSelect
	}

}