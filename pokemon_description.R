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

plot1 = ggplot(pokedex, aes(Type.1))
plot1 + geom_bar(aes(fill = Legendary)) + xlab("Principal Pokemon Type") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type") + theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

plot2 = ggplot(pokedex, aes(Generation))
plot2 + geom_bar(aes(fill = Legendary)) + xlab("Generation") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + theme(legend.position = "bottom")


