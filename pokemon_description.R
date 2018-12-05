#### Import Datasets ####
combats = read.csv("combats.csv")
pokedex = read.csv("pokemon.csv")

### Libraries ###
library("ggplot2")

### Pokedex Data Description ###
attach(pokedex)

str(pokedex)
names(pokedex)
summary(pokedex)  
summary(pokedex$Type.1)  
summary(pokedex$Generation)
summary(pokedex$Legendary)

table(pokedex$Legendary)

# Bar graphs for Type and Generation #

plot1 = ggplot(pokedex, aes(Type.1))
plot1 + geom_bar(aes(fill = Legendary)) + xlab("Principal Pokemon Type") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type") + theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5)) +
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5)

pokedex$count = 1
Generation.Legendary = aggregate(pokedex$count, by = list(Generation = pokedex$Generation, islegendary = pokedex$Legendary), FUN = sum)

'NEEDS work'
plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + geom_bar(aes(fill = islegendary), stat = "identity", position = "dodge") + xlab("Generation") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + theme(legend.position = "bottom") + facet_grid(~Generation) +
  geom_text(stat = 'count', aes(label = ..identity..), vjust = -0.5)




