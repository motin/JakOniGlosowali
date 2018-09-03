library(RMySQL)

# default settings when using https://github.com/motin/riksdagsdata to import the data
db_user <- 'root'
db_password <- 'local-mysql-pass'
db_name <- 'riksdagsdata'
db_host <- '127.0.0.1'
db_port <- 53306

# connect to db
con <-  dbConnect(MySQL(), user = db_user, password = db_password,
                  dbname = db_name, host = db_host, port = db_port)

# read data from db
s <- "SELECT 
v.intressent_id AS voter_id,
v.parti AS party,
v.rost AS vote,
REPLACE(v.votering_id, '{', '}') AS id_voting,
CONCAT(duf.rubrik, ' (', d.titel, ')') AS topic_voting,
CONCAT(fornamn, ' ', efternamn) AS voter_name,
DATE_FORMAT(d.datum, '%Y-%m-%d') AS date_meeting,
v.valkrets AS voter_district,
v.rm AS period,
v.id AS voting_db_record_id,
duf.votering_id AS duf_db_record_id,
d.hangar_id AS document_db_record_id,
GROUP_CONCAT(d.dok_id) AS voting_related_document_ids,
GROUP_CONCAT(d.dokument_url_html) AS more_info_url_voting,
CONCAT(duf.forslag, ' - ', duf.forslag_del2) AS description_voting
FROM
votering v
INNER JOIN
dokutskottsforslag duf ON duf.votering_id = REPLACE(v.votering_id, '{', '}')
AND v.votering_id <> '' 
AND v.punkt = 1 
AND v.avser = 'sakfrågan'
AND duf.beslutstyp = 'röstning'
INNER JOIN
dokument d ON duf.hangar_id = d.hangar_id
GROUP BY v.id
"

encodingRs <- dbSendQuery(con, 'set character set "utf8mb4"')

rs <- dbSendQuery(con, s)
all_votes <-  fetch(rs, n = -1)
on.exit(dbDisconnect(con))

save(all_votes, file = "./se/all_votes.rda")
