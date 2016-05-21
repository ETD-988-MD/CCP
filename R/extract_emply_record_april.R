# Extracting Employment records using the "active corps with more than 6 
# members" threshold. Then use this list of characters to subset a larger 
# movement table. This might be a more direct way to place players in space and
# time.

require(dplyr)
require(lubridate)
require(data.table)
require(RODBC)

conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')

# List of relevant corporations
load("~/ETD/R/Data/cleaned_corp_list.Rdata")
active <- data.table(sqlQuery(conn,"SELECT * FROM ebs_FACTORY.eve.activeCorporations"))
active = active[active$corporationID %in% cleaned_corp_list$corporationID]
active = active[members>=6] # Take only corporations with more than 6 members
active.corps = active$corporationID
active.corps = paste0(active.corps,collapse=",")


# (1) rough count of the size
Tcount = data.table(sqlQuery(conn,
                             paste0("SELECT count(*) FROM ebs_RESEARCH.dbo.crpEmploymentRecords 
                                    WHERE corporationID IN (",active.corps,")"),stringsAsFactors =F))
# Only 2,043,301 -- that's manageable

# (2) Grab the records for all relevant corporations
employ_records = data.table(sqlQuery(conn,
                                     paste0("SELECT  
                                            corporationID,
                                            characterID,
                                            FORMAT(startDate,'yyyy-MM-dd') AS startDate
                                            FROM ebs_RESEARCH.dbo.crpEmploymentRecords
                                            WHERE corporationID IN (",active.corps,")")))

# (3) Subset out any employees that came on/after May 2016
employ_records2 = employ_records[as.Date(startDate) < as.Date("2016-05-01")]

# (4) extract relvant characters and repeat (2)
employ_records2[corporationID==98064900]
# Now we have a subset list of employees that were in the relevant corporations 
# for the period that we care about... but we don't know if those individuals 
# left...are still currently in the corporation in April. What we need to do is 
# draw out the relevant characters and get their individual employment records 
# and then piece back if they were in the corporations of interest for the
# relevant period.
rel.chars = unique(employ_records2$characterID)
rel.chars = paste0(rel.chars,collapse=",")
# rough count of size 
Tcount2 = data.table(sqlQuery(conn,
                              paste0("SELECT count(*) FROM ebs_RESEARCH.dbo.crpEmploymentRecords 
                                     WHERE characterID IN (",rel.chars,")"),stringsAsFactors =F))



# save(employ_records,file="~/ETD/R/Data/employ_records_april")
