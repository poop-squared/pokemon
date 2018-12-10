### This section should be run after pokemon_description is run because it builds on it
pokedex$Sum.total = 0
pokedex[,15] = pokedex[5]+pokedex[6]+pokedex[7]+pokedex[8]+pokedex[9]+pokedex[10] #check out how to do this efficiently

combats = read.csv ("combats.csv")
# pokedex = read.csv ("pokemon.csv")
# 
# names(pokedex)
# names(combats)
# names(pokedex)[c(1,8,9)] = c("PokeID","Sp.Atk","Sp.Def")

#Add Battle ID
combats$BattleID <- seq.int(nrow(combats))
combats= combats[,c(4,1,2,3)]

combats$Loser = 0

str(combats) # data is numeric

#To identify the loser, since the ID's are numeric and the winner is known, we can subtract its id from the sum of both

combats$Loser = combats$First_pokemon + combats$Second_pokemon - combats$Winner

combats= combats[,-c(2,3)]
#now we have dataframe with winner and loser and combat ID

#melt them into a tidy dataframe
library(reshape)
library(reshape2)
melted.combats = melt(combats, id = "BattleID")
head(melted.combats)
#add attributes

melted.combats.merged= merge(melted.combats, pokedex, by.x = "value", by.y = "PokeID", all.x = TRUE)

melted.combats = merge(melted.combats, pokedex, by.x = "value", by.y = "X.", all.x = TRUE)

#reorder them by battle ID and outcome
names(melted.combats.merged)[c(1,3)] = c("PokeID","Outcome")
melted.combats.merged = melted.combats.merged[order(melted.combats.merged$BattleID, melted.combats.merged$Outcome),]
rownames(melted.combats.merged) = 1:nrow(melted.combats.merged)

### Subsetting ####
combats0 = melted.combats.merged

combats0[,13] = as.factor(combats0[,13]) ###Check why this is not factorizing
str(combats0)


all.winners = subset(combats0, Outcome == "Winner")
all.losers = subset(combats0, Outcome != "Winner")

lengendary.winners = subset(all.winners, Legendary == "True")
lengendary.losers = subset(all.losers, Legendary == "True")

#create a new dataframe for attribute differences
combats.diff0 = data.frame(matrix(nrow =nrow(combats0)/2 , ncol = 10))
names(combats.diff0)[1] = "BattleID"
names(combats.diff0)[c(2:7)] = names(combats0)[c(7:12)]
names(combats.diff0)[8] = "SumTotal"
names(combats.diff0)[c(9:10)] = c("legendary.winner" , "legendary.loser")
names(combats.diff0)
#start filling it with data
combats.diff0[1] = rownames(combats.diff0) #battle ID
combats.diff0[c(2:7)] = all.winners[c(7:12)] - all.losers[c(7:12)] #difference in stats
combats.diff0[8] = all.winners[17] - all.losers[17]
combats.diff0[9] = ifelse(all.winners[14] == "True" , 1, 0)
combats.diff0[10] = ifelse(all.losers[14] == "True" , 1, 0)

View(combats.diff0)
#### Move to analysis portions


