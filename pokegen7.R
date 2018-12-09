## New Gen Dataset

pokedex2018 = read.csv("pokemon2018.csv")
pokedex2 = read.csv("pokemon.csv")


str(pokedex2018)

pokedex2[722,]

poke.gen7= pokedex2018[722:802,c(3,5,6,10:16)]

poke.gen7$Generation = '7'
poke.gen7$Legendary = 'False'
poke.gen7$count = 1
poke.gen7$cluster = 0
poke.gen7$Sum.total = poke.gen7$total
poke.gen7$X. = as.numeric(row.names(poke.gen7)) + 79
row.names(poke.gen7) = poke.gen7$X.
poke.gen7 = poke.gen7[,c(16,1:9,11:15)]

#Identifying Legendary pokemons
poke.gen7$Legendary[c(51:52,64:71,)]

tail(poke.gen7,30)


names(pokedex2018)
names(pokedex)

