-- WAR/Alliances
SELECT TOP 100* FROM ebs_RESEARCH.war.allies -- Records of Alliances
SELECT COUNT(*) FROM ebs_RESEARCH.war.allies -- total # of alliances = 29,836
SELECT TOP 1000* FROM ebs_RESEARCH.war.wars ORDER BY timeFinished DESC -- record of wars 
SELECT COUNT(*) FROM ebs_RESEARCH.war.wars -- total # of wars = 486,320

-- alliance applications
SELECT TOP 100* FROM  ebs_RESEARCH.alliance.applications

-- alliance "employment records"
SELECT TOP 100* FROM  ebs_RESEARCH.alliance.employmentRecords

-- alliance membership
SELECT TOP 100* FROM  ebs_RESEARCH.alliance.members
SELECT COUNT(*) FROM  ebs_RESEARCH.alliance.members

-- alliance history (day-by-day)
SELECT TOP 100* FROM  ebs_FACTORY.eve.allianceHistory
SELECT COUNT(*) FROM  ebs_FACTORY.eve.allianceHistory

-- alliance relationships
SELECT TOP 100* FROM  ebs_RESEARCH.alliance.relationships
SELECT COUNT(*) FROM  ebs_RESEARCH.alliance.relationships -- 130,862
--- Not clear what is being captured here.
SELECT min(relationship), MAX(relationship) FROM  ebs_RESEARCH.alliance.relationships -- the coding is most likely meaningful

-- alliance "revival"
SELECT TOP 100* FROM  ebs_RESEARCH.alliance.revival -- "revival" is rare.

SELECT TOP 100* FROM  ebs_RESEARCH.alliance.primeTimes
SELECT COUNT(*) FROM  ebs_RESEARCH.alliance.primeTimes

-- Alliance Coalitions
SELECT TOP 100* FROM Kristofer_research.dbo._ebs_METRICS_allianceCoalitions
-- * kristofer folder holds useful tables for alliance playing out

-- battle datasets 
SELECT TOP 100* FROM Kristofer_research.dbo.dataBattleStats_BattleData -- not useful

-- coalitions
SELECT TOP 100* FROM ebs_FACTORY.eve.coalitionMembers

-- Specifics on Battles:
--- battle types
	SELECT TOP 100* FROM ebs_FACTORY.battle.types
--- faction types
	SELECT TOP 100* FROM ebs_FACTORY.battle.factions
--- Battle "transaction" logs
	SELECT TOP 100 * FROM ebs_RESEARCH.battle.battles ORDER BY startDate DESC



-- holding soverienty
SELECT TOP 100 * FROM datawarehouse.dbo.factSovereignty
----- here we can track soverenteiy in a solar system


--locations ... solar System Name, constellation, security, etc. 
SELECT TOP 100 * FROM datawarehouse.dbo.dimSolarSystems WHERE solarSystemID = 30002829


---- jumps "into" a solar system on a particular day. 
SELECT TOP 100 * FROM datawarehouse.dbo.factJumps



--- extracting specific location information (ownerEventsEx is huge! 984,267,680)
SELECT min(eventDate) FROM ebs_RESEARCH.zevent.ownerEventsEx -- only have events occurring in 2016

SELECT TOP 100* FROM ebs_RESEARCH.zevent.ownerEventsEx
WHERE eventTypeID = 6 

--- Faster way to query..... doesn't work for me...don't have access to this function
DECLARE @fromDate DATE = '2016-05-15'
DECLARE @toDate DATE = '2016-05-16'
 
DECLARE @fromID1 BIGINT, @fromID2 BIGINT, @toID1 BIGINT, @toID2 BIGINT
EXEC metric.GetIdentities 'zevent.ownerEvents', @fromDate, @fromID1 OUTPUT, @fromID2 OUTPUT
EXEC metric.GetIdentities 'zevent.ownerEvents', @toDate, @toID1 OUTPUT, @toID2 OUTPUT

SELECT TOP 100 *
  FROM ebs_RESEARCH.zevent.ownerEvents E
  WHERE E.eventID BETWEEN @fromID1 AND @toID2 AND E.eventTypeID = 6


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
 