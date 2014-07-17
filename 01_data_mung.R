
# 01. Get data
#-------------------------------------------------------------
setwd("C:/Users/crossfit_al1985/Documents/UCLA/_2014 summer projects/2014_CFGames_pred/R")
source("./00_functions.R")
# grab data
library(XML)
url1 <- "http://www.crossfitregionalshowdown.com/leaderboard/men"
url2 <- "http://www.crossfitregionalshowdown.com/leaderboard/women"
cf_data <- list()
cf_data[[1]] <- readHTMLTable(url1, header=T, skip.rows=1, stringsAsFactors=F)
cf_data[[2]] <- readHTMLTable(url2, header=T, skip.rows=1, stringsAsFactors=F)

# rename
cf_men <- cf_data[[1]][[1]]
cf_fem <- cf_data[[2]][[1]]
var_names <- c("rank", "athlete", "reg_rank", "reg_name", "ev1_rank", "ev1_raw",
               "ev2_rank", "ev2_raw","ev3_rank", "ev3_raw","ev4_rank", "ev4_raw"
               ,"ev5_rank", "ev5_raw","ev6_rank", "ev6_raw","ev7_rank", "ev7_raw")
names(cf_fem) <- names(cf_men) <- var_names

rm(cf_data, url1, url2, var_names) # clean up

# 02. Basic data munging
#-------------------------------------------------------------
library(lubridate)

# recode
cf_men <- to_int(cf_men, seq(3,17,2))
cf_fem <- to_int(cf_fem, seq(3,17,2))
cf_men <- to_time(cf_men, seq(10,18,2))
cf_fem <- to_time(cf_fem, seq(10,18,2))
cf_men$rank <- apply(cf_men[, c(seq(5,17,2))],1 , sum)
cf_fem$rank <- apply(cf_fem[, c(seq(5,17,2))],1 , sum)
cf_men$ev1_raw <- as.numeric(cf_men$ev1_raw)
cf_fem$ev1_raw <- as.numeric(cf_fem$ev1_raw)
cf_men$ev2_raw <- as.numeric(cf_men$ev2_raw)
cf_fem$ev2_raw <- as.numeric(cf_fem$ev2_raw)

# 03. subset top 225 and standardization
#-------------------------------------------------------------
rm(cf_men, cf_fem)
men <- cf_men[1:225,]
fem <- cf_fem[1:225,]

# first, clean event 6--- add 2.5 sec (not 1 sec) for each non-rep
# NOTE: Using final heats for M/W of central east, approx  2.8-3 sec / rep
men$ev6_raw2 <- adj_event(men$ev6_raw, 1260, 2.8)
fem$ev6_raw2 <- adj_event(fem$ev6_raw, 1260, 2.8)

# 3b - standardize scores
a <- scale(men$ev1_raw[men$ev1_raw > 0]) # deal with athletes w/ scores of 0
men$ev1_std <- c(a[1:177], NA, a[178:197], NA, a[198:223])
men$ev2_std <- scale(men$ev2_raw)
men$ev3_std <- scale_ms(men$ev3_raw)
men$ev4_std <- scale_ms(men$ev4_raw)
men$ev5_std <- scale_ms(men$ev5_raw)
men$ev6_std <- scale_ms(men$ev6_raw2)
men$ev7_std <- scale_ms(men$ev7_raw)

a <- scale(fem$ev1_raw[fem$ev1_raw > 0]) # deal with athletes w/ scores of 0
fem$ev1_std <- c(a[1:122], NA, a[123:138], NA, a[139:223])
fem$ev2_std <- scale(fem$ev2_raw)
fem$ev3_std <- scale_ms(fem$ev3_raw)
fem$ev4_std <- scale_ms(fem$ev4_raw)
fem$ev5_std <- scale_ms(fem$ev5_raw)
fem$ev6_std <- scale_ms(fem$ev6_raw2)
fem$ev7_std <- scale_ms(fem$ev7_raw)

rm(a); # save.image("./data01.Rdata")

# 03. subset Games athletes, summarize standardized scores
#-------------------------------------------------------------
library(xtable)
# ID games athletes. Not perfect, but works for now
men$games <- ifelse(men$reg_rank <= 3, T,F) 
fem$games <- ifelse(fem$reg_rank <= 3, T,F)
# subset games ATH, fix errors
men_g <- men[men$games == T, c(1:4, seq(6,18,2) ,21:27)] # raw and std_scores
men_g <- men_g[!(men_g$rank %in% c(917,1227, 1413, 1741, 1880)), ]
fem_g <- fem[fem$games == T, c(1:4, seq(6,18,2) ,21:27)] # raw and std_scores
fem_g <- fem_g[!(fem_g$rank %in% c(248, 1287, 1572, 1736)), ]

# tabulate
  # standardized
tab_games(men_g[, c(12:18)], std=T)
tab_games(fem_g[, c(12:18)], std=T)
  # raw scores
rbind(tab_games(men_g[, c(5:6)], std=T), tab_games(men_g[, c(7:11)], std=F)) #?

# 04. Total standardized scores:
#-------------------------------------------------------------
m <- data.frame(names= men_g[,2], 
                tot_std= apply(cbind(-men_g[,12:13], men_g[, 14:18]),1, sum))
f <- data.frame(names= fem_g[,2], 
                tot_std= apply(cbind(-fem_g[,12:13], fem_g[, 14:18]),1, sum))
# order results
m[order(m[,2]),]
f[order(f[,2]),]

# top competitors
men_g[c(1:3,8,11), c(2,12:18)]
fem_g[c(1:5,8), c(2,12:18)]