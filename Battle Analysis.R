
hp.pos = 100*nrow(combats.diff0[combats.diff0[2] > 0,])/ nrow(combats.diff0)
atk.pos = 100*nrow(combats.diff0[combats.diff0[3] > 0,])/ nrow(combats.diff0)
def.pos = 100*nrow(combats.diff0[combats.diff0[4] > 0,])/ nrow(combats.diff0)
sp.atk.pos = 100*nrow(combats.diff0[combats.diff0[5] > 0,])/ nrow(combats.diff0)
sp.def.pos = 100*nrow(combats.diff0[combats.diff0[6] > 0,])/ nrow(combats.diff0)
speed.pos = 100*nrow(combats.diff0[combats.diff0[7] > 0,])/ nrow(combats.diff0)
sumtotal.pos = 100*nrow(combats.diff0[combats.diff0$SumTotal > 0,])/ nrow(combats.diff0)

positive.difs = c(hp.pos,atk.pos,def.pos,sp.atk.pos,sp.def.pos,speed.pos,sumtotal.pos)
positive.difs.df = data.frame(matrix(nrow = length (positive.difs), ncol = 2))
positive.difs.df[,2] = positive.difs
positive.difs.df[,1] = names(combats.diff0[2:8])

### PCA analysis ####

#What are the attributes of winners
str(all.winners)
all.winners.vars = all.winners[c(7:12)]
pca.winners = prcomp(all.winners.vars, scale = TRUE)
pca.winners

install.packages('factoextra')
library(factoextra)
#scree chart
fviz_eig(pca.winners) 
#variable analysis
fviz_pca_var(pca.winners,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = palette.col.long,
             repel = TRUE,     # Avoid text overlapping
             title = "Variables PCA - Attributes of Winners")

#What are the attributes of losers
all.losers.vars = all.losers[c(7:12)]
pca.losers = prcomp(all.losers.vars, scale = TRUE)
pca.losers

#scree chart
fviz_eig(pca.losers) 
#variable analysis
fviz_pca_var(pca.losers,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = palette.col.long,
             repel = TRUE,     # Avoid text overlapping
             title = "Variables PCA - Attributes of Losers")



# library(ggplot2)
# library(ggfortify)
# autoplot(pca.winners, data = all.winners.vars, loadings = TRUE, loadings.label = TRUE)
# 
# 
# pve1=(pca.winners$sdev^2)/sum(pca.winners$sdev^2)
# par(mfrow=c(1,2))
# plot(pve1, ylim=c(0,1))
# plot(cumsum(pve1), ylim=c(0,1))

#### combat diff analysis
combats.diff0.vars = combats.diff0[c(2:7)]
pca.combat.diff = prcomp(combats.diff0.vars, scale = TRUE)

#scree chart
fviz_eig(pca.combat.diff,
         main = "Variable PCA - Attribute Difference") 
#variable analysis
fviz_pca_var(pca.combat.diff,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = palette.col.long,
             repel = TRUE,     # Avoid text overlapping
             title = "Variables PCA - Attribute Difference"
)


#### Predictive Model ###
#separate pokemon 1 and pokemon 2
combats1 = read.csv("combats.csv")
combats1$BattleID <- seq.int(nrow(combats1))
combats1= combats1[,c(4,1,2,3)]
names(combats1)
combats.poke1 = combats1[c(1,2)] 
combats.poke2 = combats1[c(1,3)]
#add stats
  
combats.poke1 = merge(combats.poke1, pokedex, by.x = "First_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke2 = merge(combats.poke2, pokedex, by.x = "Second_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke = merge(combats.poke1, combats.poke2, by.x = "BattleID", by.y = "BattleID", all.x = TRUE)
#add difference lines for all 6 sats and Sumtotal
names(combats.poke)
combats.poke[c(32:38)] = 0
names(combats.poke)[c(32:38)] = c("diff.HP", "dif.Attack","dif.Def","dif.Sp.Atk","dif.Sp.Def","dif.Speed","dif.Sumtotal")
combats.poke[c(32:38)] = combats.poke[c(6:11)] - combats.poke[c(21:26)]

