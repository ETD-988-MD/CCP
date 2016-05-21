--Examining Corporations: general queries

--- Corporate Roles (how corporations delegate...built in feature of the code)
SELECT TOP 100* FROM ebs_RESEARCH.corporation.roles
SELECT COUNT(*) FROM ebs_RESEARCH.corporation.roles
SELECT TOP 100* FROM  ebs_RESEARCH.dbo.crpRoleGroups

--- tracking applications into corporations
SELECT TOP 100*
FROM ebs_RESEARCH.corporation.applications 
ORDER BY applicationDateTime DESC -- to grab the bottom portion of the table

SELECT TOP 100* FROM ebs_FACTORY.eve.activeCorporations

-- are there membership rosters for all corporations?
SELECT TOP 100* FROM ebs_RESEARCH.corporation.corporationsLookup --corporation names
SELECT COUNT(*) FROM ebs_RESEARCH.corporation.corporationsLookup --339,213 total recorded names
SELECT * FROM ebs_FACTORY.eve.activeCorporations --active corporations and the # of members they have
SELECT COUNT(*), MAX(members), MIN(members) FROM ebs_FACTORY.eve.activeCorporations 

-- piecing together corporation rosters: Of Corp#: 98188326; Name: "Space Mechanics"
SELECT top 10* FROM ebs_RESEARCH.zuser.users
SELECT * FROM ebs_FACTORY.eve.activeCharacters WHERE corporationID = 98188326 -- active roster
SELECT * FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE corporationID = 98188326 -- time series
SELECT COUNT(*) FROM ebs_RESEARCH.dbo.crpEmploymentRecords
-- let's now identify the roles of particular players
SELECT * FROM ebs_RESEARCH.dbo.crpRoleHistory WHERE corporationID = 98188326 ORDER BY changeTime ASC-- roles as delegation
SELECT * FROM ebs_RESEARCH.dbo.crpMembers WHERE corporationID = 98188326 -- more comprehensive picture of what indiv are doing in corps
SELECT * FROM ebs_RESEARCH.dbo.crpTitles WHERE corporationID = 98188326


-- Voting in Space Mechanics
SELECT TOP 1000* FROM ebs_RESEARCH.dbo.crpVoteCases WHERE corporationID = 98188326


-- Messaging
SELECT TOP 10* FROM ebs_RESEARCH.mail.messages -- in general...but we can identify the users of a particular corp. 

-- Other important corporation queries
SELECT TOP 100* FROM  ebs_RESEARCH.corporation.memberAutoKicks -- kicking players out of a corporation
SELECT * FROM  ebs_RESEARCH.corporation.memberAutoKicks WHERE corporationID = 98188326
SELECT TOP 100* FROM  ebs_RESEARCH.corporation.recruitmentAdRecruiters -- recruiters

-- were the space mechanics ever in an alliance?
SELECT * FROM  ebs_RESEARCH.alliance.employmentRecords WHERE corporationID = 98188326
--- yes, three: 99003006, 99003500, and 99001783
SELECT * FROM  ebs_RESEARCH.alliance.employmentRecords WHERE allianceID = 99003006 ORDER BY startDate ASC
---- alliance build quick, persist over time, involve multiple corps, and seem to recruit in bursts.
SELECT * FROM  ebs_RESEARCH.alliance.employmentRecords WHERE allianceID = 990035006 ORDER BY startDate ASC

-- locating corporation resources
--- Corporation STATE statistics 
	SELECT TOP 100* FROM  ebs_RESEARCH.dbo.crpCorporations WHERE corporationID = 98188326
---- tax rate, "walletDivisions", recruitment size, date created, etc. 
----- (The Space mechanics have a tax rate of .1)
----- who's managing the books? Where are the "books"?
SELECT COUNT(*) FROM  ebs_RESEARCH.dbo.crpCorporations

-- NPC Corporations 
	SELECT * FROM ebs_RESEARCH.corporation.npcCorporations
	SELECT COUNT(*) FROM ebs_RESEARCH.corporation.npcCorporations -- 239

-- Day-By-Day States of corp activity
SELECT TOP 100* FROM ebs_FACTORY.eve.corporationHistory  WHERE corporationID = 98188326 ORDER BY historyDate Desc

-- Corp Activity (finer Grain)
SELECT TOP 100 * FROM ebs_RESEARCH.dbo.logCorporationEvents ORDER BY eventDateTime
--- fine-grained log of activity tied to an "eventID" - so users tied to a corp tied to a specific event
--- Such as 
	SELECT TOP 100 * FROM ebs_RESEARCH.dbo.logKillEvents -- kill events
	
--- Fluctuations in alliances/taxes/etc
	SELECT TOP 100* FROM datawarehouse.dbo.dimCharactersCorp

WITH corps AS
(
  SELECT DISTINCT corporationID
        ,corpCount = COUNT(*)
    FROM ebs_WAREHOUSE.owner.dimCorporationSCD
    GROUP BY corporationID
    HAVING COUNT(*) > 10
)
 
SELECT TOP 1000 *
  FROM ebs_WAREHOUSE.owner.dimCorporationSCD DC
    INNER JOIN corps C ON (C.corporationID = DC.corporationID)
  ORDER BY DC.corporationID




-- tying multiple users to the same event ID?
SELECT TOP 100 * FROM ebs_RESEARCH.dbo.logCorporationEvents WHERE eventID = 25318701318
	SELECT COUNT(*) FROM ebs_RESEARCH.dbo.logCorporationEvents -- this number is too low
	SELECT TOP 100 * FROM ebs_RESEARCH.dbo.logCorporationEvents WHERE corporationID = 98188326 ORDER BY eventID ASC
	SELECT TOP 1000 * FROM ebs_RESEARCH.dbo.logKillEvents WHERE corporationID = 98188326 ORDER BY eventID ASC
---- not really: the event ID is not a unique identifier.


	SELECT 
	cID = corporationID,
	member = memberCount,
	taxRate,
	logonCount,
	logonMinutes
	FROM  ebs_RESEARCH.dbo.crpCorporations

------------------------------------------------------------------------------------------------
-- Corporate Life Cycles: How long does the average corp last? ---------------------------------
------------------------------------------------------------------------------------------------
SELECT TOP 100* FROM ebs_FACTORY.eve.corporationHistory
SELECT COUNT(*) FROM ebs_FACTORY.eve.corporationHistory

--- corporate histories only offer from the beginning of 2015 onward
select corporationID, startdate = min(historyDate),enddate= max(historyDate) from ebs_FACTORY.eve.corporationHistory group by corporationID 

SELECT TOP 1000* FROM ebs_RESEARCH.dbo.crpCorporations ORDER by createDate DESC
SELECT min(createDate),max(createDate) FROM ebs_RESEARCH.dbo.crpCorporations -- here we have relevant start dates for corps...this gets us part of the way there.

SELECT corporationID,createDate, memberCount, creatorID,stationID,taxRate 
FROM ebs_RESEARCH.dbo.crpCorporations WHERE createDate >= '1/1/2015' -- list of all corps from 2015 onward

SELECT TOP 100* FROM ebs_RESEARCH.dbo.crpEmploymentRecordsEx


-- Corporate Transactions
SELECT TOP 10000
       T.transactionDate
      ,corporationID = T.ownerID1
      ,corporationName = N1.itemName
      ,counterPart = T.ownerID2
      ,counterPartName = N2.itemName
      ,E.entryTypeID
      ,ET.entryTypeName
      ,T.ownerID1
      ,T.ownerID2
      ,E.accountID
      ,E.accountID2
      ,E.amount
  FROM ebs_RESEARCH.zaccounting.transactions T
    INNER JOIN ebs_RESEARCH.zaccounting.entries E ON (E.transactionID = T.transactionID)
      INNER JOIN ebs_RESEARCH.zaccounting.entryTypes ET ON (ET.entryTypeID = E.entryTypeID)
    INNER JOIN ebs_RESEARCH.dbo.crpCorporations CP ON (CP.corporationID = T.ownerID1 AND creatorID != 1)  -- OwnerID1 is a corporation
    INNER JOIN ebs_RESEARCH.zinventory.uniqueNames N1 ON (N1.itemID = T.ownerID1)
    INNER JOIN ebs_RESEARCH.zinventory.names N2 ON (N2.itemID = T.ownerID2)
  WHERE E.entryIndex = 1
 

 

--- Changes in corporate tax rate
WITH corps AS
(
  SELECT DISTINCT corporationID
        ,corpCount = COUNT(*)
    FROM ebs_WAREHOUSE.owner.dimCorporationSCD
    GROUP BY corporationID
    HAVING COUNT(*) > 10
)
 
SELECT TOP 1000 *
  FROM ebs_WAREHOUSE.owner.dimCorporationSCD DC
    INNER JOIN corps C ON (C.corporationID = DC.corporationID)
  ORDER BY DC.corporationID
 

 
  SELECT 
  corporationID,
  taxRate,
  dateFrom,
  dateTo
  FROM ebs_WAREHOUSE.owner.dimCorporationSCD
  WHERE corporationID = 98188326 AND dateFrom > '2014-01-01' AND dateTo < '2015-01-01'

SELECT TOP 1000 *
  FROM ebs_WAREHOUSE.owner.dimCorporationSCD DC
    INNER JOIN corps C ON (C.corporationID = DC.corporationID)
  ORDER BY DC.corporationID
 