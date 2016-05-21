
-- Piecing together corporate activity for April 2016
SELECT TOP 100 * FROM ebs_RESEARCH.zevent.ownerEventsEx
-- event type: logon 30005 and logoff 30006

SELECT TOP 100* FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE eventTypeID = 30005 or  eventTypeID = 30006 AND 
eventDate BETWEEN '2016-04-01' AND '2016-04-30' -- 9mil logon/offs but useful

-- Let's go back, how much jump/wormhole activity is there? And is it useful to grab this all in one go for a single month?
SELECT TOP 1000* FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE eventGroupName = 'Movement'

SELECT COUNT(*) FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE (eventTypeID = 6 OR eventTypeID = 130) AND 
eventDate BETWEEN '2016-04-01' AND '2016-04-30' -- this is big! ~73mil need to subset by chars we care about from corporations we care about



-- Here we have a corporate history log
SELECT TOP 10* FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID = 1388417094

SELECT TOP 10* FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE corporationID IN (2081484134,2082062386)

SELECT  
corporationID,
characterID,
FORMAT(startDate,'yyyy-MM-dd') AS startDate
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE corporationID IN (2081484134,2082062386)




SELECT  
corporationID,
characterID,
FORMAT(startDate,'yyyy-MM-dd') AS startDate
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID IN (2109629728)

-- Need a query to find a specific user ID that falls within a given date
SELECT
corporationID,
MIN(startDate) AS minDate, 
MAX(startDate) AS maxDate,
datediff(day,MIN(startDate),MAX(startDate)) AS diff
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID IN (2109629728)
GROUP BY corporationID

SELECT
characterID,
corporationID, 
MAX(startDate) AS maxDate
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID IN (2109629728)
GROUP BY corporationID,characterID

--DROP TABLE #employment_timeTable
CREATE table #employment_timeTable (dt date, ownerID int, locationID INT, corpID int, taxRate float)
INSERT into #employment_timeTable(dt, ownerID, locationID, corpID, taxRate)
EXEC hadoop.hive.query 'SELECT dt, ownerID, locationID, corpID, taxRate
                          FROM eventLogs_corporation__UpdateCorporation
                          WHERE dt BETWEEN "2016.05.02" AND "2016.05.03" AND corpID = 98188326'
SELECT * FROM #corp_space_mech



SELECT  
corporationID,
characterID,
FORMAT(startDate,'yyyy-MM-dd') AS startDate
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID IN (2109629728,1388417094)


--- SUCCESS!!!
SELECT  
corporationID,
characterID,
LAG(FORMAT(startDate,'yyyy-MM-dd')) over (partition by characterID order by characterID) AS startDate,
FORMAT(startDate,'yyyy-MM-dd') AS endDate,
datediff(day,LAG(FORMAT(startDate,'yyyy-MM-dd')) over (partition by characterID order by characterID),FORMAT(startDate,'yyyy-MM-dd')) AS DiffDate
FROM ebs_RESEARCH.dbo.crpEmploymentRecords WHERE characterID IN (2109629728,1388417094)


-- Final 
SELECT *
FROM ebs_RESEARCH.dbo.crpEmploymentRecords 
WHERE characterID IN (2109629728,1388417094) AND startDate < '2014-05-01' AND corporationID 


