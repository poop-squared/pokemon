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