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
  ggtitle("Pokemon Distribution by Type") + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

pokedex$count = 1
Generation.Legendary = aggregate(pokedex$count, by = list(Generation = pokedex$Generation, Legendary_Pokemon = pokedex$Legendary)
                                 , FUN = sum)
# Bar graph 
'NEEDS work on the labels positioning'
plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + geom_bar(aes(fill = Legendary_Pokemon), stat = "identity", position = "dodge") + xlab("Generation") + 
  ylab("Number of Pokemon") + ggtitle("Pokemon Distribution by Generation") + theme(legend.position = "bottom") +
  geom_text(aes(label = Generation.Legendary$x), vjust=1.5, color="white", position = position_dodge(width = 0.8))

'Not Working yet'
library("grid")
pushViewport(viewport(layout = grid.layout(1,2)))
print(plot1, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
print(plot2, vp = viewport(layout.pos.row = 1, layout.pos.col = 2))

'facet_grid(~Generation)'

# New graph combining pokemon main types and evolution by generation (stacked) #
attach(pokedex)

summary(pokedex)
pokedex$count2 = 1
Generation.Type1 = aggregate(pokedex$count2, by = list(Generation = pokedex$Generation, Type.3 = pokedex$Type.1), 
                             FUN = sum)

length(unique(Generation.Type1$Type.3))
length(Generation.Legendary$x)

plot3 = ggplot(Generation.Type1, aes(x = Type.3, y = Generation.Type1$x))
plot3 + geom_bar(fill = Generation.Type1$x, stat = 'identity') + xlab("Principal Pokemon Type") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type") #geom_text(aes(label = ..count..), vjust = -0.5) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

