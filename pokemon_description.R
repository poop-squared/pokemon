#### Import Datasets ####
combats = read.csv("combats.csv")
pokedex = read.csv("pokemon.csv")

### Libraries ###
library("ggplot2")

##### Pokedex Data Description #####
attach(pokedex)

## Bar graph

# Pokemon Type Distribution (includes legendary breakdown)
plot1 = ggplot(pokedex, aes(Type.1))
plot1 + geom_bar(aes(fill = Legendary)) + xlab("Principal Pokemon Type") + ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type I") + geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

# Pokemon Generation Distribution (includes legendary breakdown) 
pokedex$count = 1
Generation.Legendary = aggregate(pokedex$count, by = list(Generation = pokedex$Generation, Legendary_Pokemon = pokedex$Legendary)
                                 , FUN = sum)

'NEEDS work on the labels positioning'
plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + geom_bar(aes(fill = Legendary_Pokemon), stat = "identity", position = "dodge") + xlab("Generation") + 
  ylab("Number of Pokemon") + ggtitle("Pokemon Distribution by Generation") + theme(legend.position = "bottom") +
  geom_text(aes(label = Generation.Legendary$x), vjust=1.5, hjust=1.7,color="white", position = position_dodge(width = 0.8))

#### Hierarchical clustering: divides data using a classification tree ####
attach(pokedex)

## Dendogram for Generation 1 ##
pokedex.gen1 = pokedex[c(1:166), -c(1,2)]
pokedex.gen1.names = pokedex[c(1:166),]
View(pokedex.gen1)
rownames(pokedex.gen1) = pokedex.gen1.names$Name

# Create a dendogram using hierarchical clustering
install.packages('ape')
library("ape")
hc = hclust(dist(pokedex.gen1))

# Plor a regular dendogram to make a "good" cut - decided to go for 11
plot(hc, hang = -1, cex = 0.6)
abline(h=160, col = "green", lty = 2)
abline(h=150, col = "red", lty = 2)
abline(h=140, col = "blue", lty = 2)

# Real Dendogram that we will use for the paper (only generation 1 for viewing purposes)
plot(as.phylo(hc), type = "fan")
# cutting dendrogram in 11 clusters
cluster.18 = cutree(hc, 18)

palette.col = c("blueviolet","blue","beige","aquamarine","cadetblue1","skyblue","orange","darkolivegreen","red","green",
                "yellow","plum","cyan4","wheat","magenta","dimgray","coral","goldenrod")
#spectralcol = brewer.pal(11, "Spectral")
plot(as.phylo(hc), type = "fan", tip.color = palette.col[cluster.18], label.offset = 1)

# Cut the tree at 11 branches and create a vector using that clustering
cluster.cut = cutree(hc, 18)

# Include the new vector in the original gen1 pokedex DF
pokedex.gen1$cluster = cluster.cut

## Clustering (dendogram) for All Generations
pokedex.allgen = pokedex[, -c(1,2)]
rownames(pokedex.allgen) = pokedex$Name
hc1 = hclust(dist(pokedex.allgen))
plot(as.phylo(hc1), type = "fan")
cluster.all.18 = cutree(hc1, 18)
palette.col = c("blueviolet","blue","beige","aquamarine","cadetblue1","skyblue","orange","darkolivegreen","red","green",
                "yellow","plum","cyan4","wheat","magenta","dimgray","coral","goldenrod")
plot(as.phylo(hc1), type = "fan", tip.color = palette.col[cluster.all.18], label.offset = 1)

# Include the new vector in the original pokedex DF
cluster.cut.all = cutree(hc1, 18)
pokedex$cluster = cluster.cut.all

##

