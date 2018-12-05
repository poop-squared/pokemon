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
#library(reshape) , library(reshape2)
melted.combats = melt(combats, id = "BattleID")
#add attributes
melted.combats = merge(melted.combats, pokedex, by.x = "value", by.y = "PokeID", all.x = TRUE)
#reorder them by battle ID
melted.combats = melted.combats[order(melted.combats$BattleID),]
names(melted.combats)[c(1,3)] = c("PokeID","Outcome")



