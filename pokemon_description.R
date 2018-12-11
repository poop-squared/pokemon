#### Import Datasets ####
combats = read.csv("combats.csv")
pokedex = read.csv("pokemon.csv")



### Libraries ###
library("ggplot2")

##### Pokedex Data Description #####
attach(pokedex)

names(pokedex)[c(1,8,9)] = c("PokeID", "Sp.Atk","Sp.Def")
names(pokedex)

####training data = pokedex ######

pokedex.original = pokedex
pokedex.test = pokedex[pokedex$Generation==6,]
pokedex = subset(pokedex, !(PokeID %in% pokedex.test$PokeID))

combats.original = combats
combats.test = subset(combats, Winner %in% pokedex.test$PokeID |  Loser %in% pokedex.test$PokeID )
combats = subset(combats, !(BattleID %in% combats.test$BattleID))


# Palette Color
palette.col.long = c("#FF4933","#FF8633","#FFBB33","#FFE633","#D7FF33","#99FF33","#52FF33","#33FF86","#33FFD1",
                "#33D4FF","#339FFF","#336BFF","#3358FF","#5233FF","#8033FF","#B233FF","#FF33C4",
                "#FF3396")
palette.col.short = palette.col.long[c(1,4,7,10,12,17)]
## Bar graph

# Pokemon Type Distribution (includes legendary breakdown)
plot1 = ggplot(pokedex.original, aes(Type.1))
plot1 + geom_bar(aes(fill = Legendary)) + 
  xlab("Principal Pokemon Type") +
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type I") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  guides(fill=guide_legend("Legendary", nrow = 1)) +
  scale_fill_manual(values = palette.col.short) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

# Pokemon Type II Distribution (includes legendary breakdown)
plot1.1 = ggplot(pokedex.original, aes(Type.2))
plot1.1 + geom_bar(aes(fill = Legendary)) + 
  xlab("Secondary Pokemon Type") +
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Type II") + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5) + 
  guides(fill=guide_legend("Legendary", nrow = 1)) +
  scale_fill_manual(values = palette.col.short) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

# Pokemon Generation Distribution (includes legendary breakdown) 
pokedex.original$count = 1
Generation.Legendary = 
  aggregate(pokedex.original$count, by = list(Generation = pokedex.original$Generation, Legendary_Pokemon = pokedex.original$Legendary)
                                 , FUN = sum)

'NEEDS work on the labels positioning'
plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + 
  geom_bar(aes(fill = Legendary_Pokemon), stat = "identity", position = "dodge") +
  xlab("Generation") + 
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Legendary", nrow = 1)) +
  scale_fill_manual(values = palette.col.short) +
  geom_text(aes(label = Generation.Legendary$x), 
            vjust=1.5, hjust=1.7,color="white", position = position_dodge(width = 0.8))

Generation.Type1 = aggregate(pokedex.original$count, by = list(Generation = pokedex.original$Generation, Type = pokedex.original$Type.1), 
                             FUN = sum)

length(unique(Generation.Type1$Type))
length(Generation.Legendary$x)

plot3 = ggplot(Generation.Type1, aes(x = reorder(Type, x), y = Generation.Type1$x))
plot3 + geom_col(aes(x = Generation.Type1$Type, fill = factor(Generation.Type1$Generation), y = x)) +
  xlab("Type") + 
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + 
  guides(fill=guide_legend("Generation", nrow = 1)) +
  scale_fill_manual(values = palette.col.short) +
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))



#### Hierarchical clustering: divides data using a classification tree ####
attach(pokedex)

## Dendogram for Generation 1 ##
pokedex.gen1 = pokedex[c(1:166),]
pokedex.gen1.names = pokedex[c(1:166),]


# Create a dendogram using hierarchical clustering
install.packages('ape')
library("ape")

pokedex.gen1.stats = pokedex.gen1[,c(5:10)]
rownames(pokedex.gen1.stats) = pokedex.gen1.names$Name
hc = hclust(dist(pokedex.gen1.stats))

# # Plor a regular dendogram to make a "good" cut - decided to go for 11
# plot(hc, hang = -1, cex = 0.6)
# abline(h=160, col = "green", lty = 2)
# abline(h=150, col = "red", lty = 2)
# abline(h=140, col = "blue", lty = 2)

# Real Dendogram that we will use for the paper (only generation 1 for viewing purposes)
plot(as.phylo(hc), type = "fan")
# cutting dendrogram in 18 clusters
cluster.18 = cutree(hc, 18)

plot(as.phylo(hc), type = "fan", tip.color = palette.col.long[cluster.18], label.offset = 1)

# Cut the tree at 18 branches and create a vector using that clustering
cluster.cut = cutree(hc, 18)

# Include the new vector in the original gen1 pokedex DF
pokedex.gen1$cluster = cluster.cut



par(mfrow=c(1,1))
## Clustering (dendogram) for All Generations (5 gens)
pokedex.allgen.stats = pokedex[, c(5:10)]
rownames(pokedex.allgen.stats) = pokedex$Name
hc1 = hclust(dist(pokedex.allgen.stats))
plot(as.phylo(hc1), type = "fan")
cluster.all.18 = cutree(hc1, 18)
plot(as.phylo(hc1), type = "fan", tip.color = palette.col.long[cluster.all.18], label.offset = 1)

# Include the new vector in the original pokedex DF
cluster.cut.all = cutree(hc1, 18)
pokedex$cluster = cluster.cut.all

table(pokedex$cluster)

####Start Merging
