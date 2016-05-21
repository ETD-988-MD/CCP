-- hadoop samples :: all samples are of length 100. This is just a taste of what is available on the hadoop server. 
--- cash transfers
	SELECT * FROM hadoop.samples.eventLogs_account__GiveCash 
	SELECT * FROM hadoop.samples.eventLogs_account__MoveCash
	
-- ship insurance
	SELECT * FROM hadoop.samples.eventLogs_account__InsureShip 
-- corp accounting 
	SELECT * FROM hadoop.samples.eventLogs_account__SetCorpAccountKey
-- ship
	SELECT * FROM hadoop.samples.eventLogs_ActivateShip -- ship (past to current)/type id
-- travel
	SELECT * FROM hadoop.samples.eventLogs_ActivateAccelerationGate -- gate/ship/ownerID/locationID

-- Alliances	
	SELECT * FROM hadoop.samples.eventLogs_alliance__CreateAlliance -- create alliances

-- contacts
	SELECT * FROM hadoop.samples.eventLogs_contacts__AddCorporateContact

-- corporations
	SELECT * FROM hadoop.samples.eventLogs_account__GiveCashFromCorpAccount -- cash transters from corporate account
	SELECT * FROM hadoop.samples.eventLogs_corporation__CorporationCreated -- dates created!
	SELECT * FROM hadoop.samples.eventLogs_corporation__UpdateCorporation -- updates (tax rate)
	SELECT * FROM hadoop.samples.eventLogs_account__SetCorpAccountKey -- accounts
	SELECT * FROM hadoop.samples.eventLogs_corporation__AddBulletin -- bullitins

-- Character- grain details
	SELECT * FROM hadoop.samples.eventLogs_crimewatch__PlayerKill -- kill interactions (isk lost, ships/weapons used, attacker-victim, the works)
	SELECT * FROM hadoop.samples.eventLogs_Death_Ship -- ship kill details
	SELECT * FROM hadoop.samples.eventLogs_dogma__AddTargets -- targeting other players
	SELECT * FROM hadoop.samples.eventLogs_park__AlignTo -- aligning
	SELECT * FROM hadoop.samples.eventLogs_park__Warp_Launch -- warping
	SELECT * FROM hadoop.samples.eventLogs_park__Warp_Char
	SELECT * FROM hadoop.samples.eventLogs_park__Orbit -- orbiting
	SELECT * FROM hadoop.samples.eventLogs_PlayerStatus_Active -- when a player is active, location, and ship
	SELECT * FROM hadoop.samples.eventLogs_pos__AttackPlayer_Standing

-- Fleets
	SELECT * FROM hadoop.samples.eventLogs_fleet__Broadcast -- fleet level broadcasts and interactions
	SELECT * FROM hadoop.samples.eventLogs_fleet__CreateWing -- can track creating fleets/squads etc
	SELECT * FROM hadoop.samples.eventLogs_fleet__KickMember -- kicking members out of a fleet

	SELECT * FROM hadoop.samples.eventLogs_trial__GiveCorporationCash


---- Extracting from Hadoop (slow way) ----------------------
-------- Pull specific information about Space Mechanics
SELECT TOP 100* FROM ebs_FACTORY.eve.corporationHistory  WHERE corporationID = 98188326 ORDER BY historyDate Desc
DROP TABLE #corp_space_mech
CREATE table #corp_space_mech (dt date, ownerID int, locationID INT, corpID int, taxRate float)
 
INSERT into #corp_space_mech(dt, ownerID, locationID, corpID, taxRate)
EXEC hadoop.hive.query 'SELECT dt, ownerID, locationID, corpID, taxRate
                          FROM eventLogs_corporation__UpdateCorporation
                          WHERE dt BETWEEN "2016.05.02" AND "2016.05.03" AND corpID = 98188326'
SELECT * FROM #corp_space_mech


DROP TABLE #corps_creates
CREATE table #corps_creates (dt date, ownerID int, corporationID int)
INSERT into #corps_creates(dt, ownerID, corporationID)
EXEC hadoop.hive.query 'SELECT dt, ownerID, corporationID
                        FROM eventLogs_corporation__CorporationCreated
						WHERE dt >= "2012.01.01"'
SELECT top 100* FROM #corps_creates


CREATE table #reprocessed (dt date, stationID int, stationOwnerID INT, typeID int, amount float)
 
INSERT into #reprocessed(dt, stationID, stationOwnerID, typeID, amount)
EXEC hadoop.hive.query 'SELECT dt, locationID, stationOwnerID, typeID, SUM(amountRefined)
                          FROM eventLogs_reprocess__ReprocessItem
                          WHERE dt BETWEEN "2014.05.29" AND "2014.05.30"
                          GROUP by dt, locationID, stationOwnerID, typeID'

Select TOP 100* FROM #reprocessed
 


