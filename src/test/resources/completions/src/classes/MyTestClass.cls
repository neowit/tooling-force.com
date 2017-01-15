/**
 * used to check syntax/grammar related to Apex specific Test statements
 */
@isTest
public class MyTestClass  {
    /**
     * 
     */
    static testMethod void test1 () {
        final User activeUser = new User();
        System.runAs(activeUser){
            System.debug(' some code ');
        }

    }
}
