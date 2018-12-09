
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
  geom_text(aes(label = X2, vjust = 1.6)) +
  xlab("Attributes") +
  ylab("Percentage of Wins") + 
  ggtitle("Percentage of Wins given positive attribute advantage") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=0, vjust=0.5))

### PCA analysis ####

#What are the attributes of winners
str(all.winners)
all.winners.vars = all.winners[c(7:12)]
pca.winners = prcomp(all.winners.vars, scale = TRUE)
autoplot(pca.winners, data = all.winners.vars, loadings = TRUE, loadings.label = TRUE)


combats.diff0.vars = combats.diff0[c(2:7)]
pca.combat.diff = prcomp(combats.diff0.vars, scale = TRUE)
autoplot(pca.combat.diff, data = combats.diff0.vars, loadings = TRUE, loadings.label = TRUE)


combats0[combats0$BattleID=='1',]
## good up to this point

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
