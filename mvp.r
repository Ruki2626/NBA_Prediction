
setwd("D:/Python Work/football prediction/NBA-MVP-Prediction-master")

player_data <- read.csv("data/player_regular_season.csv", stringsAsFactors=FALSE)
team_data <- read.csv("data/team_season.csv")
mvp_data <- read.csv("data/mvp.csv")
#names(team_data) <- c("ï..team", "team")
colnames(team_data)[colnames(team_data)=="ï..team"] <- "team"

char2num <- function(x)as.numeric(as.character(x))
findNAs <- function(x)which(is.na(x))
findBlanks <- function(x)which(x=="")
normalize <- function(x)(x-mean(x, na.rm=TRUE))/sd(x, na.rm=TRUE)

firstname <- function(x){
  names = strsplit(x, ' ')
  firstname = names[[1]][1]
}
lastname <- function(x){
  names = strsplit(x, ' ')
  lastname = names[[1]][2]
}
#clean up
team_data <- subset(team_data, leag=="N")
team_data <- team_data[, c("team", "year", "won", "lost")]
team_data$won <- as.numeric(as.character(team_data$won))
team_data$lost <- as.numeric(as.character(team_data$lost))
team_data$pct <- round(team_data$won / (team_data$won + team_data$lost), 3)
team_data$year <- team_data$year + 1    # Convert year to end year

player_data <- subset(player_data, leag=="N")   

sapply(player_data, findNAs)          # No NAs
sapply(player_data, findBlanks)       # No Blanks
length(which(player_data$gp == 0))    # No observations with zero games played

numeric_vars <- c("gp", "pts", "reb", "asts", "stl", "blk", "fga", "fgm", "fta", "ftm")
player_data[,numeric_vars] <- sapply(player_data[,numeric_vars], char2num)

# Create desired per game averages
player_data$ppg <- round(player_data$pts / player_data$gp, 2)
player_data$rpg <- round(player_data$reb / player_data$gp, 2)
player_data$apg <- round(player_data$asts / player_data$gp, 2)
player_data$bpg <- round(player_data$blk / player_data$gp, 2)
player_data$spg <- round(player_data$stl / player_data$gp, 2)
player_data$fg_pct <- round(player_data$fgm / player_data$fga, 3)
player_data$ft_pct <- round(player_data$ftm / player_data$fta, 3)


# Convert all years to the year the season ended, not the year it started
player_data$year <- player_data$year + 1

#merge
all_data <- merge(player_data, team_data) #by=c("year", "won"))

# MVP seasons that won't be found in mvp data
all_data[which(all_data$firstname=="Shaquille"), "lastname"] <- "O'Neal"
all_data[which(all_data$lastname=="Abdul-jabbar"), "lastname"] <- "Abdul-Jabbar"
all_data[which(all_data$lastname=="Mcadoo"), "lastname"] <- "McAdoo"

all_data$mvp_id <- paste(all_data$firstname, "-", all_data$lastname, "-", all_data$year, sep="")

#mvp data
mvp_data$Player <- as.character(mvp_data$Player)
mvp_data$firstname <- sapply(mvp_data$Player, firstname)
mvp_data$lastname <- sapply(mvp_data$Player, lastname)
mvp_data$id <- 1:nrow(mvp_data)
mvp_data$year <- 2015 - mvp_data$id



mvp_data$mvp_id <- paste(mvp_data$firstname, "-", mvp_data$lastname, "-", mvp_data$year, sep="")

#all data
all_data$mvp <- rep(NA, nrow(all_data))
for (i in 1:nrow(all_data)){
  if (all_data$mvp_id[i] %in% mvp_data$mvp_id){
    all_data$mvp[i] = 1
  } else {
    all_data$mvp[i] = 0
  }
}

all_data <- subset(all_data, year >= 1974)
all_data <- subset(all_data, gp >= 49)
all_data <- subset(all_data, ftm >= 125)
all_data <- subset(all_data, fgm >= 300)

all_data <- all_data[, c("year", "firstname", "lastname", 
                         "ppg", "rpg", "apg", "bpg", "spg", "fg_pct",
                         "ft_pct", "pct", "mvp")]




all_data_new <- data.frame()
for (i in unique(all_data$year)){
  
  data <- subset(all_data, year == i)
  
  data_2 <- sapply(data[,c("ppg", "rpg", "apg", "bpg", "spg", "fg_pct", "ft_pct", "pct")], normalize)
  
  data_3 <- cbind(data[,c("year", "firstname", "lastname", "mvp")], data_2)
  
  all_data_new <- rbind(all_data_new, data_3)
}

#logisitic regression

library(caret)
inTrain <- createDataPartition(y=all_data_new$mvp, p=0.7, list=FALSE)

train <- all_data_new[inTrain, ]
test <- all_data_new[-inTrain, ]
dim(train)
dim(test)
table(train$mvp)
table(test$mvp)
mvp_model <- glm(mvp ~ ., family=binomial(link="logit"), data=train[,4:ncol(train)])


mvp_probs_train <- predict(mvp_model, newdata=train, type="response")
mvp_probs_test <- predict(mvp_model, newdata=test, type="response")


### Testing stuff
all_data_test <- all_data
all_data_test$mvp_prob <- rep(NA, nrow(all_data_test))
all_data_test$mvp_prob[inTrain] <- mvp_probs_train
all_data_test$mvp_prob[-inTrain] <- mvp_probs_test

head(all_data_test[order(all_data_test$mvp_prob, decreasing=TRUE), ], 20)

years <- unique(all_data_new$year)
set.seed(1)
train.pct <- 0.6
N <- length(years)
train.years <- sample(years, train.pct*N)
train.index <- which(all_data_new$year %in% train.years)

train <- all_data_new[train.index, ]
test <- all_data_new[-train.index, ]


## Apply the same model as above
mvp.log.model <- glm(mvp ~ ., family=binomial(link="logit"), data=train[,4:ncol(train)])
mvp.log.model.2 <- glm(mvp ~ ., family=binomial(link="logit"), data=all_data_new[,4:ncol(all_data_new)])
summary(mvp.log.model)

mvp.probs.train <- predict(mvp.log.model, newdata=train, type="response")
mvp.probs.test <- predict(mvp.log.model, newdata=test, type="response")
mvp.probs.all <- predict(mvp.log.model.2, newdata=all_data_new, type="response")

train.probs.df <- cbind(all_data[train.index, ], mvp.probs.train)
test.probs.df <- cbind(all_data[-train.index, ], mvp.probs.test)
all.probs.df <- cbind(all_data, mvp.probs.all)

# Let's see how well the model performs on the training data
best.odds.train <- data.frame()
normalized.probs <- data.frame()
for (i in train.years){
  data <- subset(train.probs.df, year==i)
  data$mvp.probs.train <- data$mvp.probs.train/sum(data$mvp.probs.train)
  normalized.probs <- rbind(normalized.probs, data)
  best <- data[which(data$mvp.probs.train == max(data$mvp.probs.train)), ]
  best.odds.train <- rbind(best.odds.train, best)
}

best.odds.train <- best.odds.train[order(best.odds.train$year), ]
best.odds.train

table(best.odds.train$mvp)

test.years <- setdiff(years, train.years)
best.odds.test <- data.frame()
normalized.probs.2 <- data.frame()
for (i in test.years){
  data <- subset(test.probs.df, year==i)
  data$mvp.probs.test <- data$mvp.probs.test/sum(data$mvp.probs.test)
  normalized.probs.2 <- rbind(normalized.probs.2, data)
  best <- data[which(data$mvp.probs.test == max(data$mvp.probs.test)), ]
  best.odds.test <- rbind(best.odds.test, best)
}
best.odds.test <- best.odds.test[order(best.odds.test$year), ]
best.odds.test
table(best.odds.test$mvp)

best.odds.all <- data.frame()
normalized.probs.3 <- data.frame()
for (i in unique(all_data$year)){
  data = subset(all.probs.df, year==i)
  data$mvp.probs.all <- data$mvp.probs.all/sum(data$mvp.probs.all)
  normalized.probs.3 <- rbind(normalized.probs.3, data)
  best <- data[which(data$mvp.probs.all == max(data$mvp.probs.all)), ]
  best.odds.all <- rbind(best.odds.all, best)
}

best.odds.all <- best.odds.all[order(best.odds.all$year, decreasing=FALSE), ]
best.odds.all
table(best.odds.all$mvp)

odds.2006 <- subset(normalized.probs.2, year==2006)
odds.2006 <- odds.2006[order(odds.2006$mvp.probs.test, decreasing=TRUE),]
head(odds.2006, 10)

names(normalized.probs)[ncol(normalized.probs)] <- "mvp.prob"
names(normalized.probs.2)[ncol(normalized.probs.2)] <- "mvp.prob"
all.probs.data <- rbind(normalized.probs, normalized.probs.2)
all.probs.data <- all.probs.data[order(all.probs.data$mvp.prob, decreasing=TRUE), ]

favorites <- data.frame()
for (i in years){
  data <- subset(all.probs.data, year==i)
  fav <- data[which(data$mvp.prob==max(data$mvp.prob)), ]
  favorites <- rbind(favorites, fav)
}
write.csv(favorites, "mvp_favorites.csv", row.names=FALSE)
table(favorites$mvp)
26/37

# About 70% accurate
mean(favorites$mvp.prob)

favorites$fullname <- paste(favorites$firstname, favorites$lastname, sep= " ")
mvp.tbl <- table(favorites$fullname)
mvp.fav.df <- data.frame(
  player = names(mvp.tbl),
  count = as.numeric(mvp.tbl)
)
mvp.fav.df <- mvp.fav.df[order(mvp.fav.df$count, decreasing=TRUE), ]
mvp.fav.df

# 2015 Player Data and Predictions 
data.2015 <- read.csv("data/player_data_2015.csv")
data.2015 <- data.2015[-which(data.2015$Rk == "Rk"), ]
data.2015 <- data.2015[, c("Player", "Tm", "PTS", "TRB", "AST", "BLK", "STL", "FG.", "FT.")]


teams.2015 <- read.csv("data/team_data_2015.csv")
names(teams.2015)
teams.2015 <- teams.2015[-1,]
teams.2015 <- teams.2015[,c("X.1", "X.2")]
names(teams.2015) <- c("Team", "Record")
teams.2015$Record <- as.character(teams.2015$Record)
pcts <- c()
for (i in 1:30){
  wins <- strsplit(teams.2015$Record, "-")[[i]][1]
  pct <- as.numeric(wins)/82
  pcts <- c(pcts, pct)
}
teams.2015$pct <- pcts
head(teams.2015)
teams.2015$Tm <- c("GSW", "ATL", "HOU", "LAC", "MEM", "SAS", "CLE", "POR", "CHI",
                   "DAL", "TOR", "WAS", "NOP", "OKC", "MIL", "BOS", "PHO", "BRK",
                   "IND", "UTA", "MIA", "CHO", "DET", "DEN", "SAC", "ORL", "LAL",
                   "PHI", "NYK", "MIN")
names(data.2015)
all.data.2015 <- merge(data.2015, teams.2015, by="Tm")
head(all.data.2015)
all.data.2015 <- all.data.2015[,c("Player", "PTS", "TRB", "AST", "BLK", "STL", "FG.", "FT.", "pct")]
names(all.data.2015) <- c("Player", "ppg", "rpg", "apg", "bpg", "spg", "fg_pct", "ft_pct", "pct")

## Normalize the data
data.2015.b <- sapply(all.data.2015[,c("ppg", "rpg", "apg", "bpg", "spg", "fg_pct", "ft_pct", "pct")], char2num)
data.2015.b$ppg
data.2015.b <- data.frame(data.2015.b)
head(data.2015.b)
names(data.2015.b)
data.2015.c <- sapply(data.2015.b, normalize)
data.2015.c <- data.frame(data.2015.c)


### Predict for 2015
probs.2015 <- predict(mvp.log.model, newdata=data.2015.c, type="response")
all.data.2015$mvp.prob <- probs.2015
all.data.2015 <- all.data.2015[order(all.data.2015$mvp.prob, decreasing=TRUE), ]
all.data.2015 <- all.data.2015[-which(is.na(all.data.2015$mvp.prob)), ]
all.data.2015$mvp.prob.norm <- all.data.2015$mvp.prob/sum(all.data.2015$mvp.prob)
output <- head(all.data.2015, 20)
write.csv(output, "mvp_2015_probs.csv", row.names=FALSE)

