### Analytics

################################################ Clustering ###################################################################

#### Hierarchical clustering: divides data using a classification tree ####

## Dendogram for Generation 1 ##
pokedex.gen1 = pokedex[c(1:166),]
pokedex.gen1.names = pokedex[c(1:166),]


# Create a dendogram using hierarchical clustering

pokedex.gen1.stats = pokedex.gen1[,c(5:10)]
rownames(pokedex.gen1.stats) = pokedex.gen1.names$Name
hc = hclust(dist(pokedex.gen1.stats))


# Real Dendogram that we will use for the paper (only generation 1 for viewing purposes)
plot(as.phylo(hc), type = "fan")
# cutting dendrogram in 18 clusters
cluster.18 = cutree(hc, 18)

plot(as.phylo(hc), type = "fan", tip.color = palette.col.long[cluster.18], label.offset = 1)

# Cut the tree at 18 branches and create a vector using that clustering
cluster.cut = cutree(hc, 18)

# Include the new vector in the original gen1 pokedex DF
pokedex.gen1$cluster = cluster.cut



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

####### Winning % by cluster #############


  #### Creating new Combat dataframes ############
  
  melted.combats = melt(combats, id = "BattleID")
  
  #add attributes
  
  
  melted.combats.merged= merge(melted.combats, pokedex, by.x = "value", by.y = "PokeID", all.x = TRUE)
  
  melted.combats = merge(melted.combats, pokedex, by.x = "value", by.y = "PokeID", all.x = TRUE)
  
  
  #reorder them by battle ID and outcome
  names(melted.combats.merged)[c(1,3)] = c("PokeID","Outcome")
  melted.combats.merged = melted.combats.merged[order(melted.combats.merged$BattleID, melted.combats.merged$Outcome),]
  rownames(melted.combats.merged) = 1:nrow(melted.combats.merged)
  
  ### Subsetting ####
  combats0 = melted.combats.merged
  
  combats0[,13] = as.factor(combats0[,13]) ###Check why this is not factorizing
  str(combats0)
  
  
  all.winners = subset(combats0, Outcome == "Winner")
  all.losers = subset(combats0, Outcome != "Winner")
  
  
  #create a new dataframe for attribute differences
  combats.diff0 = data.frame(matrix(nrow =nrow(combats0)/2 , ncol = 10))
  names(combats.diff0)[1] = "BattleID"
  names(combats.diff0)[c(2:7)] = names(combats0)[c(7:12)]
  names(combats.diff0)[8] = "SumTotal"
  names(combats.diff0)[c(9:10)] = c("legendary.winner" , "legendary.loser")
  names(combats.diff0)
  #start filling it with data
  combats.diff0[1] = rownames(combats.diff0) #battle ID
  combats.diff0[c(2:7)] = all.winners[c(7:12)] - all.losers[c(7:12)] #difference in stats
  combats.diff0[8] = all.winners$Sum.total - all.losers$Sum.total
  combats.diff0[9] = ifelse(all.winners[14] == "True" , 1, 0)
  combats.diff0[10] = ifelse(all.losers[14] == "True" , 1, 0)

#### Winning ratios by attributes
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

plot4 = ggplot(data = positive.difs.df, aes(x = X1, y = X2))
plot4 + geom_bar(stat = "identity", fill = "#FF4933") +
  geom_text(aes(label = sprintf("%.01f %%", X2), vjust = 1.6)) +
  xlab("Attributes") +
  ylab("Percentage of Wins") + 
  ggtitle("Percentage of Wins given Attribute Advantage") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

######## Cluster web chart ######

color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797",
         "#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")

res<-data.frame(pokedex %>% dplyr::select(cluster,HP, Attack, Defense, Sp.Atk, Sp.Def, Speed) %>% dplyr::group_by(cluster) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp.Atk + Sp.Def + Speed) %>% arrange(-sumChars))
res$color<-color
max<- ceiling(apply(res[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)
par(mfrow=c(3,6))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res)){
  curCol<-(col2rgb(as.character(res$color[i]))%>% as.integer())/255
  radarchart(rbind(max,min,res[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res$cluster[i]))
}

#### 

###################################################################################################################

###############################################  Predictive Analysis ##############################################


#separate pokemon 1 and pokemon 2
combats1 = combats.a.train
combats1$BattleID <- seq.int(nrow(combats1))

names(combats1)
combats.poke1 = combats1[c(1,2)] 
combats.poke2 = combats1[c(1,3)]
#add stats

combats.poke1 = merge(combats.poke1, pokedex, by.x = "First_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke2 = merge(combats.poke2, pokedex, by.x = "Second_pokemon", by.y = "PokeID", all.x = TRUE)
combats.poke = merge(combats.poke1, combats.poke2, by.x = "BattleID", by.y = "BattleID", all.x = TRUE)
#clean up names
names(combats.poke)
names(combats.poke)[c(3:15)] = c("Name.1","Type.1.1","Type.2.1","HP.1", "Attack.1" ,"Defense.1","Sp.Atk.1","Sp.Def.1",
                                 "Speed.1"   ,"Generation.1" ,"Legendary.1","Sum.total.1","cluster.1")

names(combats.poke)[c(17:29)] = c("Name.2","Type.1.2","Type.2.2","HP.2", "Attack.2" ,"Defense.2","Sp.Atk.2","Sp.Def.2",
                                  "Speed.2"   ,"Generation.2" ,"Legendary.2", "Sum.total.2","cluster.2")
#add difference lines for all 6 sats and Sumtotal
combats.poke[c(30:36)] = 0
names(combats.poke)[c(30:36)] = c("diff.HP", "dif.Attack","dif.Def","dif.Sp.Atk","dif.Sp.Def","dif.Speed","dif.Sumtotal")
combats.poke[c(30:36)] = combats.poke[c(6:11)] - combats.poke[c(20:25)]
names(combats.poke)
#merge winner id
combats.poke = merge(combats.poke, combats1, by.x = "BattleID" , by.y = "BattleID")
names(combats.poke)
combats.poke = combats.poke[-c(37,38)] #remove redundant clumns

names(combats.poke)[c(2,16)] = c("First_pokemon","Second_pokemon")
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


##############################   Random forest #####################################
par(mfrow=c(1,1))
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
detach(combats.poke)
#############################################################################################################

#################################################### PCA analysis ###########################################

#What are the attributes of winners
str(all.winners)
all.winners.vars = all.winners[c(7:12)]
pca.winners = prcomp(all.winners.vars, scale = TRUE)
pca.winners

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


#############################################################################################################































############### Cluster graphs ###########################

##By Cluster

# #HP
# hcboxplot(x=pokedex.original$HP,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")
# 
# #Attack
# hcboxplot(x=pokedex.original$Attack,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")
# 
# #Defense
# hcboxplot(x=pokedex.original$Defense,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")
# 
# 
# #SP. Attack
# hcboxplot(x=pokedex.original$Sp.Atk,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")
# 
# 
# #SP. Def
# hcboxplot(x=pokedex.original$Sp.Def,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")
# 
# #Speed
# hcboxplot(x=pokedex.original$Speed,var=pokedex.original$cluster,color="green") %>%
#   hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")