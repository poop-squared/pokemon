
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

plot3d(pca.combat.diff$scores[,1:3])
text3d(pca.combat.diff$loadings[,1:3], texts = rownames(pc$loadings), col="red"
        coords=NULL
        for (i in 1:nrow(pc$loadings)) { coords= rbind(coords, rbind(c(0,0,0), pc$loadings[i,1:3])) }
        lines3d(coords, col="red", lwd=4) text3d(pc$scores[,1:3])
        text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")