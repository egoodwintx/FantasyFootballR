## FantasyFootball Snake Draft Picker
##
## author : Ed Goodwin

## QB = 1; RB = 2; RB/WR = 1; WR = 2; TE = 1;
## D/ST = 1; PK = 1; BE = 7
library(RMySQL)
library(Rglpk)

## load initialization variables
source("/Users/egoodwin/Documents/active/code/R/fantasyFootball/draftinit.r")

## are we drafting starters or bench players?
#globalDRAFT = "STARTERS"
globalDRAFT = "BENCH"

#globalCONNECT = "CSV"
globalCONNECT = "DB"

dbPullData = function() {
  ## pull data from Database connection
  con = dbConnect(MySQL(),
                 user = db.user,
                 password = db.password,
                 host = db.host,
                 dbname=db.name)

  ## filter out all team IDs
  draftpool = suppressWarnings(dbGetQuery(conn = con, 
                                        statement = stmt))
  ## retrieve all undrafted players
  stmt = "SELECT * FROM projections WHERE draftstatus=\"0\""
  draftpool = suppressWarnings(dbGetQuery(conn = con, 
                                        statement = stmt))

  stmt = "SELECT * FROM projections WHERE draftstatus=\"E\""
  ## retrieve all positions I've filled so far
  teampool = suppressWarnings(dbGetQuery(conn = con, 
                                       statement = stmt))
  results = list(draftpool, teampool)
  dbDisconnect(con)
  results
}

csvPullData = function() {
  ## pull data from CSV file with projections data
  dirname = "/Users/egoodwin/Documents/active/code/R/fantasyFootball/"
  fname = "data/FFA-Projections.csv"
  uri = paste0(dirname, fname)

  ## some players are duplicated since they play multiple positions...create separate key ID on file
  pool = read.csv(file = uri, stringsAsFactors=FALSE)


  ## players that I have drafted
  teampool = subset(pool, draft=='E')

  ## players that are still available
  draftpool = subset(pool, draft==0)
  results = list(draftpool, teampool)
  results
}

## MAIN PROGRAM
if(globalCONNECT == "DB") {
  print("Pulling data from DB...")
  results = dbPullData()  
} else {
  print("Pulling data from CSV file...")
  results = csvPullData()
}

draftpool = results[[1]]
teampool = results[[2]]
currteam =  data.frame(qb = nrow(subset(teampool, position == "QB")),
                       rb = nrow(subset(teampool, position == "RB")),
                       wr = nrow(subset(teampool, position == "WR")),
                       rec = nrow(subset(teampool, position == "WR" |
                                           position == "RB")),
                       te = nrow(subset(teampool, position == "TE")),
                       d = nrow(subset(teampool, position == "DST")),
                       pk = nrow(subset(teampool, position == "K"))
)

## set team constraints
## QB = 1; RB = 2; RB/WR = 1; WR = 2; TE = 1;
## D/ST = 1; PK = 1; BE = 7
# qb.max = 4, rb.max = 8,
# wr.max = 8, te.max = 3,
# d.max = 3, pk.max = 3

## set constraints based on where you're at in the draft cycle (bench or starter)
## to draft starters
if(globalDRAFT == "STARTERS") {
  constraints = data.frame(qb = 1, rb = 3, wr = 3, rec = 5,
                                   te = 1, d = 1, pk = 1, starter = 9,
                                   risk = 50)
  const.dir = c(rep("<=",6), "==", "<=", "<=")
  } else {
  ## to draft bench, increase risk tolerance to maximize points
  constraints = data.frame(qb = 3, rb = 6, wr = 6, rec = 12, te = 2, 
                           d = 2, pk = 2, starter = 16, risk=1000)
  const.dir = c(rep("<=",9))
}
currteam

## set up position vectors; these will be used to source positions
draftpool$qb = ifelse(draftpool$position == "QB", 1, 0)
draftpool$rb = ifelse(draftpool$position == "RB", 1, 0)
draftpool$wr = ifelse(draftpool$position == "WR", 1, 0)
draftpool$rec = ifelse(draftpool$position == "WR" |
                         draftpool$position == "RB", 1, 0)
draftpool$te = ifelse(draftpool$position == "TE", 1, 0)
draftpool$d = ifelse(draftpool$position == "DST", 1, 0)
draftpool$pk = ifelse(draftpool$position == "K", 1, 0)
draftpool$starter = 1

## set nulls equal to default risk
draftpool$risk[draftpool$risk == "null"] = 5
draftpool$risk = as.numeric(draftpool$risk)

const.matrix = rbind(as.numeric(draftpool$qb), 
                     as.numeric(draftpool$rb), 
                     as.numeric(draftpool$wr),
                     as.numeric(draftpool$rec),
                     as.numeric(draftpool$te), 
                     as.numeric(draftpool$d), 
                     as.numeric(draftpool$pk),
                     as.numeric(draftpool$starter),
                     as.numeric(draftpool$risk)
)


## starter team remaining (less already drafted)
rhs = c(constraints$qb - currteam$qb,
        constraints$rb - currteam$rb,
        constraints$wr - currteam$wr,
        constraints$rec - currteam$rb - currteam$wr,
        constraints$te - currteam$te,
        constraints$d - currteam$d,
        constraints$pk - currteam$pk,
        constraints$starter - sum(currteam) + currteam$rec,
        constraints$risk - sum(teampool$risk)
)

## objective to maximize
objective = as.numeric(draftpool$points)

var.types = rep("B", length(draftpool$points))

sol <- Rglpk_solve_LP(obj = objective, mat = const.matrix, dir = const.dir,
                      rhs = rhs, types = var.types, max = TRUE)
sol
draftpool[sol$solution>=1,c(2,3,5,6,7,10,11,15,23)]

sum(draftpool[sol$solution>=1,c(2,3,5,6,7,10,11,15,23)]$risk) + sum(teampool$risk)
sum(draftpool[sol$solution>=1,c(2,3,5,6,7,10,11,15,23)]$points) + sum(teampool$points)
