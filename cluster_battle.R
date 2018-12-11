
#### Figuring out the backup straigh line for clusters ####

# Create a new DF that has the necessary information to do further calculations on clusters
cluster.battle = merge(all.winners, combats.diff0, by = "BattleID")
cluster.battle = cluster.battle[, c(1,2,16,18:24)]
cluster.battle = cluster.battle[order(cluster.battle$cluster), ]

names(cluster.battle)

# Create subsets for every cluster

cluster.battle.1 = subset(cluster.battle, cluster.battle$cluster == 1)
clus1.speed.pos = 100*nrow(cluster.battle.1[cluster.battle.1[8] > 0,])/ nrow(cluster.battle.1)

cluster.battle.2 = subset(cluster.battle, cluster.battle$cluster == 2)
clus2.speed.pos = 100*nrow(cluster.battle.2[cluster.battle.2[8] > 0,])/ nrow(cluster.battle.2)

cluster.battle.3 = subset(cluster.battle, cluster.battle$cluster == 3)
clus3.speed.pos = 100*nrow(cluster.battle.3[cluster.battle.3[8] > 0,])/ nrow(cluster.battle.3)

cluster.battle.4 = subset(cluster.battle, cluster.battle$cluster == 4)
clus4.speed.pos = 100*nrow(cluster.battle.4[cluster.battle.4[8] > 0,])/ nrow(cluster.battle.4)

cluster.battle.5 = subset(cluster.battle, cluster.battle$cluster == 5)
clus5.speed.pos = 100*nrow(cluster.battle.5[cluster.battle.5[8] > 0,])/ nrow(cluster.battle.5)

cluster.battle.6 = subset(cluster.battle, cluster.battle$cluster == 6)
clus6.speed.pos = 100*nrow(cluster.battle.6[cluster.battle.6[8] > 0,])/ nrow(cluster.battle.6)

cluster.battle.7 = subset(cluster.battle, cluster.battle$cluster == 7)
clus7.speed.pos = 100*nrow(cluster.battle.7[cluster.battle.7[8] > 0,])/ nrow(cluster.battle.7)

cluster.battle.8 = subset(cluster.battle, cluster.battle$cluster == 8)
clus8.speed.pos = 100*nrow(cluster.battle.8[cluster.battle.8[8] > 0,])/ nrow(cluster.battle.8)

cluster.battle.9 = subset(cluster.battle, cluster.battle$cluster == 9)
clus9.speed.pos = 100*nrow(cluster.battle.9[cluster.battle.9[8] > 0,])/ nrow(cluster.battle.9)

cluster.battle.10 = subset(cluster.battle, cluster.battle$cluster == 10)
clus10.speed.pos = 100*nrow(cluster.battle.10[cluster.battle.10[8] > 0,])/ nrow(cluster.battle.10)

cluster.battle.11 = subset(cluster.battle, cluster.battle$cluster == 11)
clus11.speed.pos = 100*nrow(cluster.battle.11[cluster.battle.11[8] > 0,])/ nrow(cluster.battle.11)

cluster.battle.12 = subset(cluster.battle, cluster.battle$cluster == 12)
clus12.speed.pos = 100*nrow(cluster.battle.12[cluster.battle.12[8] > 0,])/ nrow(cluster.battle.12)

cluster.battle.13 = subset(cluster.battle, cluster.battle$cluster == 13)
clus13.speed.pos = 100*nrow(cluster.battle.13[cluster.battle.13[8] > 0,])/ nrow(cluster.battle.13)

cluster.battle.14 = subset(cluster.battle, cluster.battle$cluster == 14)
clus14.speed.pos = 100*nrow(cluster.battle.14[cluster.battle.14[8] > 0,])/ nrow(cluster.battle.14)

cluster.battle.15 = subset(cluster.battle, cluster.battle$cluster == 15)
clus15.speed.pos = 100*nrow(cluster.battle.15[cluster.battle.15[8] > 0,])/ nrow(cluster.battle.15)

cluster.battle.16 = subset(cluster.battle, cluster.battle$cluster == 16)
clus16.speed.pos = 100*nrow(cluster.battle.16[cluster.battle.16[8] > 0,])/ nrow(cluster.battle.16)

# cluster.battle.17 = subset(cluster.battle, cluster.battle$cluster == 17)
# clus17.speed.pos = 100*nrow(cluster.battle.17[cluster.battle.17[8] > 0,])/ nrow(cluster.battle.17)

cluster.battle.18 = subset(cluster.battle, cluster.battle$cluster == 18)
clus18.speed.pos = 100*nrow(cluster.battle.18[cluster.battle.18[8] > 0,])/ nrow(cluster.battle.18)

# Populating the dataframe
clus.speed.positive.difs.df = data.frame(matrix(nrow = 18, ncol = 2))

clus.speed.positive.difs.df[1,2] = clus1.speed.pos
clus.speed.positive.difs.df[2,2] = clus2.speed.pos
clus.speed.positive.difs.df[3,2] = clus3.speed.pos
clus.speed.positive.difs.df[4,2] = clus4.speed.pos
clus.speed.positive.difs.df[5,2] = clus5.speed.pos
clus.speed.positive.difs.df[6,2] = clus6.speed.pos
clus.speed.positive.difs.df[7,2] = clus7.speed.pos
clus.speed.positive.difs.df[8,2] = clus8.speed.pos
clus.speed.positive.difs.df[9,2] = clus9.speed.pos
clus.speed.positive.difs.df[10,2] = clus10.speed.pos
clus.speed.positive.difs.df[11,2] = clus11.speed.pos
clus.speed.positive.difs.df[12,2] = clus12.speed.pos
clus.speed.positive.difs.df[13,2] = clus13.speed.pos
clus.speed.positive.difs.df[14,2] = clus14.speed.pos
clus.speed.positive.difs.df[15,2] = clus15.speed.pos
clus.speed.positive.difs.df[16,2] = clus16.speed.pos
clus.speed.positive.difs.df[17,2] = 0
clus.speed.positive.difs.df[18,2] = clus18.speed.pos

clus.speed.positive.difs.df[1,1] = "Cluster_1"
clus.speed.positive.difs.df[2,1] = "Cluster_2"
clus.speed.positive.difs.df[3,1] = "Cluster_3"
clus.speed.positive.difs.df[4,1] = "Cluster_4"
clus.speed.positive.difs.df[5,1] = "Cluster_5"
clus.speed.positive.difs.df[6,1] = "Cluster_6"
clus.speed.positive.difs.df[7,1] = "Cluster_7"
clus.speed.positive.difs.df[8,1] = "Cluster_8"
clus.speed.positive.difs.df[9,1] = "Cluster_9"
clus.speed.positive.difs.df[10,1] = "Cluster_10"
clus.speed.positive.difs.df[11,1] = "Cluster_11"
clus.speed.positive.difs.df[12,1] = "Cluster_12"
clus.speed.positive.difs.df[13,1] = "Cluster_13"
clus.speed.positive.difs.df[14,1] = "Cluster_14"
clus.speed.positive.difs.df[15,1] = "Cluster_15"
clus.speed.positive.difs.df[16,1] = "Cluster_16"
clus.speed.positive.difs.df[17,1] = "Cluster_17"
clus.speed.positive.difs.df[18,1] = "Cluster_18"

clus.speed.positive.difs.df$X3 = reorder(clus.speed.positive.difs.df$X1, clus.speed.positive.difs.df$X2)

plot5 = ggplot(data = clus.speed.positive.difs.df, aes(x = X3, y = X2))
plot5 + geom_bar(stat = "identity", fill = "#FF4933") +
  geom_text(aes(label = sprintf("%.01f %%", X2), vjust = 1.6)) +
  xlab("Cluster Classification") +
  ylab("Percentage of Wins") + 
  ggtitle("Percentage of Wins given Speed Advantage per Cluster") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))

# Re-order the DF by ranking
clus.speed.positive.difs.df = clus.speed.positive.difs.df[order(clus.speed.positive.difs.df$X2), ]

# Renaming Cluster to reflect their real battle power (Tiers) based on win percentage
renamed.clusters.df = data.frame(matrix(nrow = 18, ncol = 3))
renamed.clusters.df[,1] = clus.speed.positive.difs.df$X1
renamed.clusters.df[,2] = clus.speed.positive.difs.df$X2
renamed.clusters.df[,3] = 1:18

plot6 = ggplot(data = renamed.clusters.df, aes(x = X3, y = X2))
plot6 + geom_bar(stat = "identity", fill = "#FF4933") +
  geom_text(aes(label = sprintf("%.01f %%", X2), vjust = 1.6)) +
  xlab("Clusters") +
  ylab("Percentage of Wins") + 
  ggtitle("Percentage of Wins given Speed Advantage per Cluster") + 
  theme(legend.position = "bottom", axis.text.x = element_text(angle=90, vjust=0.5))
