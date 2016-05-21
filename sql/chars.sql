-- locating myself
SELECT TOP 100* FROM ebs_RESEARCH.zuser.users WHERE userID = 12748017
SELECT TOP 100* FROM ebs_FACTORY.eve.characterHistory WHERE userID = 12748017
SELECT TOP 100* FROM ebs_FACTORY.eve.activeUsers WHERE userID = 12748017


SELECT TOP 100* FROM ebs_RESEARCH.zuser.usersEx

SELECT COUNT(*) FROM ebs_FACTORY.eve.characterHistory

-- Isolate just STEAM accounds
SELECT * FROM ebs_RESEARCH.zuser.users WHERE (userName LIKE 'STEAM::%')
-- 237,321 total steam users; all might not be active

SELECT COUNT(*) FROM ebs_RESEARCH.zuser.users -- ~19.5 mil
SELECT TOP 10* FROM ebs_RESEARCH.zuser.users 

-- STATE OF THE WHOLE GAME
SELECT TOP 100* FROM ebs_FACTORY.eve.activityIndexData ORDER BY activityDate DESC -- state of the game


-- Isolating specifics on character STATS
--- kills
SELECT * FROM ebs_FACTORY.character.killBlobAttackers WHERE characterID = 708131619
----The following seems to offer you a time series of characters death events.

-- Loss
--- based off specific kills, you can get a rough estimate of loss
SELECT TOP 100 * FROM ebs_FACTORY.character.killBlobItems WHERE killID = 148
---- need an index of typeID and to evaluate how much something costs. 

-- Yearly STATs for a specific character.
SELECT TOP 100* FROM ebs_FACTORY.character.yearlyStats  WHERE characterID = 708131619
SELECT COUNT(*) FROM ebs_FACTORY.character.yearlyStats -- ~6 mil
---- not clear when these state stats are taken. But they cover a lot of bases


-- Character details
SELECT TOP 1000* FROM ebs_FACTORY.eve.characters ORDER BY createDate DESC -- state of the characters
SELECT * FROM ebs_FACTORY.eve.characters WHERE characterID = 1990366912 
--- provides a players current status, but not a time series. 

-- Time series of character activity
SELECT TOP 1000* FROM ebs_FACTORY.eve.characterHistory WHERE characterID = 1990366912 ORDER BY historyDate Desc

-- Message activity
SELECT TOP 100 * FROM ebs_RESEARCH.dbo.lscChannels WHERE ownerID = 1990366912

-- locate a cluster of characters in the same space at the same time?


SELECT TOP 100 * FROM ebs_FACTORY.eve.characterHistory
SELECT TOP 1000 * FROM datawarehouse.dbo.dimCorporations

-- Character Skin Color
SELECT TOP 100 * FROM ebs_RESEARCH.zcharacter.paperdollModifierLocationsTx
SELECT TOP 100 * FROM ebs_FACTORY.eve.allianceHistory



SELECT TOP 100 * FROM ebs_RESEARCH.zevent.ownerEventsEx
-- event type: logon 30005 and logoff 30006

SELECT TOP 100* FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE eventTypeID = 30005 or  eventTypeID = 30006 AND 
eventDate BETWEEN '2016-04-01' AND '2016-04-30' -- 9mil logon/offs but useful

-- Let's go back, how much jump/wormhole activity is there? And is it useful to grab this all in one go for a single month?
SELECT TOP 1000* FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE eventGroupName = 'Movement'

SELECT COUNT(*) FROM ebs_RESEARCH.zevent.ownerEventsEx 
WHERE (eventTypeID = 6 OR eventTypeID = 5 OR eventTypeID = 4 OR eventTypeID = 130) AND 
eventDate BETWEEN '2016-04-01' AND '2016-04-30'
