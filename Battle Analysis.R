
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
### Tree analysis ####
#### Predictive Model ###
#separate pokemon 1 and pokemon 2
combats1 = combats.a.train
combats1$BattleID <- seq.int(nrow(combats1))

names(combats1)
combats.poke1 = combats1[c(1,2)] 
combats.poke2 = combats1[c(1,3)]
#add stats
names(pokedex)
names(combats.poke1)
combats.poke1 = merge(combats.poke1, pokedex, by.x = "First_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke2 = merge(combats.poke2, pokedex, by.x = "Second_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke = merge(combats.poke1, combats.poke2, by.x = "BattleID", by.y = "BattleID", all.x = TRUE)
#clean up names
names(combats.poke)
names(combats.poke)[c(3:16)] = c("Name.1","Type.1.1","Type.2.1","HP.1", "Attack.1" ,"Defense.1","Sp.Atk.1","Sp.Def.1",
                                 "Speed.1"   ,"Generation.1" ,"Legendary.1", "count.1","cluster.1",     
                                 "Sum.total.1")

names(combats.poke)[c(18:31)] = c("Name.2","Type.1.2","Type.2.2","HP.2", "Attack.2" ,"Defense.2","Sp.Atk.2","Sp.Def.2",
                                  "Speed.2"   ,"Generation.2" ,"Legendary.2", "count.2","cluster.2",     
                                  "Sum.total.2")
#add difference lines for all 6 sats and Sumtotal
combats.poke[c(32:38)] = 0
names(combats.poke)[c(32:38)] = c("diff.HP", "dif.Attack","dif.Def","dif.Sp.Atk","dif.Sp.Def","dif.Speed","dif.Sumtotal")
combats.poke[c(32:38)] = combats.poke[c(6:11)] - combats.poke[c(21:26)]
names(combats.poke)
#merge winner id
combats.poke = merge(combats.poke, combats1, by.x = "BattleID" , by.y = "BattleID")
names(combats.poke)
combats.poke = combats.poke[-c(39,40)] #remove redundant clumns

names(combats.poke)[c(2,26)] = c("First_pokemon","Second_pokemon")
# check if first pokemon was winner
for (i in 1:nrow(combats.poke)){
  combats.poke$outcome.for.1[i] = ifelse(combats.poke$Winner[i] == combats.poke$First_pokemon[i] , "Win", "Lose")
}

combats.poke$outcome.for.1=as.factor(combats.poke$outcome.for.1)
str(combats.poke$outcome.for.1)
# #perform QDA
# attach(combats.poke)
# combats.qda = lda(outcome.for.1~diff.HP+ dif.Attack+dif.Def+dif.Sp.Atk+dif.Sp.Def+dif.Speed)
# partimat(outcome.for.1~diff.HP+ dif.Attack + dif.Def, method = "qda") #terrible graphs. abort mission


#Random forest
attach(combats.poke)
battle.forest = rpart(outcome.for.1~diff.HP+ dif.Attack+dif.Def+dif.Sp.Atk+dif.Sp.Def+dif.Speed,
                      control=rpart.control(cp=0.01))
summary(battle.forest)
rpart.plot(battle.forest) #find optimal cp

battle.forest1 = rpart(outcome.for.1~diff.HP+ dif.Attack+dif.Def+dif.Sp.Atk+dif.Sp.Def+dif.Speed,
                       control=rpart.control(cp=0.0000001))
printcp(battle.forest1)
plotcp(battle.forest1)
cp.opt= battle.forest1$cptable[which.min(battle.forest1$cptable[,"xerror"]),"CP"]

battle.forest.opt = rpart(outcome.for.1~diff.HP+ dif.Attack+dif.Def+dif.Sp.Atk+dif.Sp.Def+dif.Speed,
                          control=rpart.control(cp=cp.opt))
rpart.plot(battle.forest.opt)

#Random forest
battle.rand.forest = randomForest(outcome.for.1~diff.HP+ dif.Attack+dif.Def+dif.Sp.Atk+dif.Sp.Def+dif.Speed,
                                  ntree = 500,
                                  importance = TRUE,
                                  do.trace = 10,
                                  data = combats.poke)
battle.rand.forest #OOB Estimate of error = 5%
importance(battle.rand.forest)
varImpPlot(battle.rand.forest)

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
             title = "Variables PCA - Battle Attribute Difference"
)

