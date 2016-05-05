--#START: Simple query
SELECT Id, Name, BillingCity FROM Account 
--#END

--#START: WHERE
SELECT Id FROM Contact WHERE Name LIKE 'A%' AND MailingCity = 'California'
--#END

--#START: ORDER BY	
SELECT Name FROM Account ORDER BY Name DESC NULLS LAST
--#END

--#START: LIMIT
SELECT Name FROM Account WHERE Industry = 'media' LIMIT 125
--#END

--#START: ORDER BY with LIMIT
SELECT Name FROM Account WHERE Industry = 'media' ORDER BY BillingPostalCode ASC NULLS LAST LIMIT 125
--#END

--#START: count()
SELECT COUNT() FROM Contact
--#END

--#START: GROUP BY
SELECT LeadSource, COUNT(Name) FROM Lead GROUP BY LeadSource
--#END

--#START: HAVING
SELECT Name, COUNT(Id) FROM Account GROUP BY Name HAVING COUNT(Id) > 1
--#END

--#START: OFFSET with ORDER BY
SELECT Name, Id FROM Merchandise__c ORDER BY Name OFFSET 100
--#END

--#START: OFFSET with ORDER BY and LIMIT
SELECT Name, Id FROM Merchandise__c ORDER BY Name LIMIT 20 OFFSET 100
--#END

--#START: Relationship queries: child-to-parent	1
SELECT Contact.FirstName, Contact.Account.Name FROM Contact
--#END
--#START: Relationship queries: child-to-parent	2
SELECT Id, Name, Account.Name FROM Contact WHERE Account.Industry = 'media'
--#END

--#START: Relationship queries: parent-to-child	1
SELECT Name, (SELECT LastName FROM Contacts) FROM Account
--#END
--#START: Relationship queries: parent-to-child	2
SELECT Account.Name, (SELECT Contact.LastName FROM Account.Contacts) FROM Account
--#END

--#START: Relationship query with WHERE
SELECT Name, (SELECT LastName FROM Contacts WHERE CreatedBy.Alias = 'x') FROM Account WHERE Industry = 'media'
--#END

--#START: Relationship query: child-to parent with custom objects
SELECT Id, FirstName__c, Mother_of_Child__r.FirstName__c FROM Daughter__c WHERE Mother_of_Child__r.LastName__c LIKE 'C%'
--#END

--#START: Relationship query: parent to child with custom objects
SELECT Name, (SELECT Name FROM Line_Items__r) FROM Merchandise__c WHERE Name LIKE 'Acme%'
--#END

--#START: Relationship queries with polymorphic key 1
SELECT Id, Owner.Name FROM Task WHERE Owner.FirstName like 'B%'
--#END
--#START: Relationship queries with polymorphic key 2
SELECT Id, Who.FirstName, Who.LastName FROM Task WHERE Owner.FirstName LIKE 'B%'
--#END

--#START: Relationship queries with polymorphic key 3
SELECT Id, What.Name FROM Event
--#END

--#START: Polymorphic relationship queries using TYPEOF
SELECT TYPEOF What WHEN Account THEN Phone, NumberOfEmployees WHEN Opportunity THEN Amount, CloseDate ELSE Name, Email END FROM Event
--#END

--#START: subquery in WHERE part
SELECT 
    TYPEOF What
        WHEN Account THEN Phone
        ELSE Name
    END
FROM Event
WHERE CreatedById IN
    (
    SELECT CreatedById
    FROM Case
	where CreatedDate < TODAY
    )
--#END


--#START: Date Literal 1
SELECT Id FROM Account WHERE CreatedDate = YESTERDAY
--#END

--#START: Date Literal 2
SELECT Id FROM Account WHERE CreatedDate < LAST_N_DAYS:15
--#END

--#START: Relationship queries with aggregate 1
SELECT Name, (SELECT CreatedBy.Name FROM Notes) FROM Account
--#END
--#START: Relationship queries with aggregate 2
SELECT Amount, Id, Name, (SELECT Quantity, ListPrice, PricebookEntry.UnitPrice, PricebookEntry.Name FROM OpportunityLineItems) FROM Opportunity
--#END

--#START: Simple query: the UserId and LoginTime for each user
SELECT UserId, LoginTime from LoginHistory
--#END

--#START:  date format 1
SELECT UserId, COUNT(Id) from LoginHistory WHERE LoginTime > 2010-09-20T22:16:30.000Z AND LoginTime < 2010-09-21T22:16:30Z GROUP BY UserId
--#END

--#START:  NOT Operator
SELECT Id from User WHERE NOT IsActive = true 
--#END
--
--#START: NOT, <>, =, != Operators
SELECT Id from User WHERE IsActive <> true and IsActive != true and not IsActive = true
--#END

--#START: Date Functions 1
SELECT CALENDAR_YEAR(CreatedDate), SUM(Amount)
FROM Opportunity
GROUP BY CALENDAR_YEAR(CreatedDate)
--#END
--
--#START: Date Functions 2
SELECT CreatedDate, Amount
FROM Opportunity
WHERE CALENDAR_YEAR(CreatedDate) = 2009
--#END

--#START: GEO Location 1
SELECT Name, Location__c 
FROM Warehouse__c 
WHERE DISTANCE(Location__c, GEOLOCATION(37.775,-122.418), 'mi') < 20 
--#END

--#START: collection reference
select Id from Contact
where Id = :contacts[2].Id and Status__c = 'Inactive' 
--#END

--#START: where in parens
select Id from Contact
                    where 
                    (Id = :contacts[1].Id and Status__c = 'Inactive') 
                    or (Id = :contacts[2].Id and (Status__c = 'Active' or Status__c = 'Other') ) 
                    or Parent_Contact__c = :contactId 
                    or ( Id <> :contactId and Status__c = 'Active2' )
                    order by Parent_Contact__r.LastName
--#END

--#START: empty brackets inside query
select Id, Account.Name from Opportunity where Id in: new Id[] {opp1.Id, opp2.Id}
--#END

--#START: inner creator inside query
select Id, Account.Name from Opportunity where Id in: new Set<Id> {opp1.Id, opp2.Id}
--#END


