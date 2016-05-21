# April log on/off record

conn <- odbcDriverConnect('DRIVER={SQL Server};SERVER=researchdb;DATABASE=ebs_FACTORY;Trusted Connection=true;Integrated Security=true;')
# april_log_onOff = data.table(sqlQuery(conn,
#                                       "SELECT * FROM ebs_RESEARCH.zevent.ownerEventsEx 
#                                       WHERE (eventTypeID = 30005 OR eventTypeID = 30006) AND 
#                                       eventDate BETWEEN '2016-04-01' AND '2016-04-30'",stringsAsFactors =F))
# save(april_log_onOff,file="~/ETD/R/Data/april_log_onOff.Rdata")

# # Range is correct
# unique(april_log_onOff$eventTypeID)
# unique(april_log_onOff$eventDate)


# Query run - load in object when need a subset
load("~/ETD/R/Data/april_log_onOff.Rdata")


# Subset to only include the relevant characters that are in the active
# corporations that the sample was drawn from

rel.chars = unique(snapTable$characterID)
sub_april_log_onOff = april_log_onOff[ownerID%in% rel.chars]

# Reduce to relevant components
sub_april_log_onOff = sub_april_log_onOff[,list(eventDate,ownerID,referenceID,eventTypeName)]

# Is referenceID refering to location.
ss = interTable$locationID %>% unique(.)
sub_april_log_onOff[referenceID%in%ss] # Yep!

colnames(sub_april_log_onOff) = c("eventDate","characterID","locationID","eventType")

# This will need to happen with each the main frame is updated. 
# save(sub_april_log_onOff,file="~/ETD/R/Data/SUBSET_april_log_onOff.Rdata")
