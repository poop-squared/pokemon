combats = read.csv ("combats.csv")
pokedex = read.csv ("pokemon.csv")

names(pokedex)
names(combats)
names(pokedex)[c(1,8,9)] = c("PokeID","Sp.Atk","Sp.Def")

#Add Battle ID
combats$BattleID <- seq.int(nrow(combats))
combats= combats[,c(4,1,2,3)]

combats$Loser = 0

str(combats) # data is numeric

#To identify the loser, since the ID's are numeric and the winner is known, we can subtract its id from the sum of both
for (i in 1:nrow(combats)){
  combats$Loser[i] = combats$First_pokemon[i] + combats$Second_pokemon[i] - combats$Winner[i] 
}
names(combats)
combats= combats[,-c(2,3)]
#now we have dataframe with winner and loser and combat ID

#melt them into a tidy dataframe
library(reshape)
library(reshape2)
melted.combats = melt(combats, id = "BattleID")
#add attributes
melted.combats = merge(melted.combats, pokedex, by.x = "value", by.y = "PokeID", all.x = TRUE)
#reorder them by battle ID and
melted.combats = melted.combats[order(melted.combats$BattleID, melted.combats$Outcome),]
names(melted.combats)[c(1,3)] = c("PokeID","Outcome")
rownames(melted.combats) = 1:nrow(melted.combats)

### Subsetting ####
combats0 = melted.combats

combats0[13] = factor(combats0[13]) ###Check why this is not factorizing


all.winners = subset(combats0, Outcome == "Winner")
all.losers = subset(combats0, Outcome != "Winner")

lengendary.winners = subset(all.winners, Legendary == "True")
lengendary.losers = subset(all.losers, Legendary == "True")
names(combats0)


#create a new dataframe for attribute differences
combats.diff0 = data.frame(matrix(nrow =nrow(combats0)/2 , ncol = 9))
names(combats.diff0)[1] = "BattleID"
names(combats.diff0)[c(2:7)] = names(combats0)[c(7:12)]
names(combats.diff0)[c(8:9)] = c("legendary.winner" , "legendary.loser")
names(combats.diff0)
#start filling it with data
combats.diff0[1] = rownames(combats.diff0) #battle ID
combats.diff0[c(2:7)] = all.winners[c(7:12)] - all.losers[c(7:12)] #difference in stats
names(all.winners)
combats.diff0[8] = ifelse(all.winners[14] == "True" , 1, 0)
combats.diff0[9] = ifelse(all.losers[14] == "True" , 1, 0)

