### perform ###

##preparing data por perform analysis
combats.cluster = merge(combats,pokedex[,c(1,14)], by.x = 'Winner', by.y = 'PokeID')
colnames(combats.cluster)[4] = 'cluster.win'   
                   
combats.cluster = merge(combats.cluster,pokedex[,c(1,14)], by.x = 'Loser', by.y = 'PokeID')
colnames(combats.cluster)[5] = 'cluster.lose'

combats.cluster = combats.cluster[order(combats.cluster$BattleID),]


### Printing popularity of pokemon by cluster

for( i in 1:18){
print(nrow(subset(combats.cluster , cluster.win == i | cluster.lose == i))/nrow(combats))

}

### Creating win ratio matrix by cluster  rows are winners, columns are losers

total.same = rep(0,18)
win.ratio = data.frame(matrix(nrow = 18, ncol = 18))

for( i in 1:18){
 
  for ( j in 1:18){
    
    x = subset(combats.cluster , cluster.win == i & cluster.lose == j)  
    y = subset(combats.cluster , cluster.win == j & cluster.lose == i) 
    
    if(i==j){
      total.same[i] = nrow(x)
      win.ratio[i,j] = 1
    }else{
      
      win.ratio[i,j] = nrow(x)/(nrow(x)+nrow(y))
      
    }
    
  } 

}

write.csv(win.ratio,'winratio.csv')

