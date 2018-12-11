############ Pokedex ############

pokedex = read.csv("pokemon.csv")

names(pokedex)[c(1,8,9)] = c("PokeID", "Sp.Atk","Sp.Def")
names(pokedex)

####training data = pokedex ######

pokedex.original = pokedex
pokedex.test = pokedex[pokedex$Generation==6,]
pokedex = subset(pokedex, !(PokeID %in% pokedex.test$PokeID))
pokedex$Sum.total =pokedex[5]+pokedex[6]+pokedex[7]+pokedex[8]+pokedex[9]+pokedex[10] #check out how to do this efficiently
names(pokedex$Sum.total) = 'Sum.total'
###>>>> add the cluster later




############# Combat ##############

combats = read.csv("combats.csv")

#Add Battle ID
combats$BattleID <- seq.int(nrow(combats))
combats= combats[,c(4,1,2,3)]

combats.a = combats
combats$Loser = 0

#To identify the loser, since the ID's are numeric and the winner is known, we can subtract its id from the sum of both

combats$Loser = combats$First_pokemon + combats$Second_pokemon - combats$Winner

combats= combats[,-c(2,3)]

combats.original = combats
combats.test = subset(combats, Winner %in% pokedex.test$PokeID |  Loser %in% pokedex.test$PokeID )
combats = subset(combats, !(BattleID %in% combats.test$BattleID))
combats.a.train = subset(combats.a, !(BattleID %in% combats.test$BattleID))




View(combats.diff0)
#### Move to analysis portions