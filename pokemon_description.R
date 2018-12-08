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
plot1 + geom_bar(aes(fill = Legendary)) + 
  xlab("Principal Pokemon Type") + 
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))+
  scale_fill_brewer(palette="Set1")

pokedex$count = 1
Generation.Legendary = aggregate(pokedex$count, by = list(Generation = pokedex$Generation, Legendary_Pokemon = pokedex$Legendary)
                                 , FUN = sum)
# Bar graph 
'NEEDS work on the labels positioning'
plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + 
  geom_bar(aes(fill = Legendary_Pokemon), stat = "identity", position = "dodge") + 
  xlab("Generation") + 
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + 
  theme(legend.position = "bottom") +
  geom_text(aes(label = Generation.Legendary$x), vjust=1.5, color="white", position = position_dodge(width = 0.8))+
  scale_fill_brewer(palette="Set1")

t
'facet_grid(~Generation)'

# New graph combining pokemon main types and evolution by generation (stacked) #
attach(pokedex)

summary(pokedex)
pokedex$count2 = 1
Generation.Type1 = aggregate(pokedex$count2, by = list(Generation = pokedex$Generation, Type.3 = pokedex$Type.1), 
                             FUN = sum)

Generation.Type1$x = as.factor(Generation.Type1$x)
Generation.Type1$Generation = as.factor(Generation.Type1$Generation)

# For each type of pokemon, how are the generations distributed
ggplot(data = Generation.Type1, aes(x = Type.3, y = x)) +
  geom_col(aes(x = Type.3, fill = Generation, y = x)) +
  scale_fill_brewer(palette="Set1") +
  xlab("Principal Pokemon Type") + ylab("Number of Pokemon") + 
  ggtitle("Generational Distribution by Type")+
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))
