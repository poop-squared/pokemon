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

color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797",
         "#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")

color2<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130")

res2<-data.frame(pokedex %>% dplyr::select(Generation,HP, Attack, Defense, Sp..Atk, Sp..Def, Speed) %>% dplyr::group_by(Generation) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp..Atk + Sp..Def + Speed) %>% arrange(-sumChars))
res2$color<-color2
max<- ceiling(apply(res2[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)
par(mfrow=c(2,3))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res2)){
  curCol<-(col2rgb(as.character(res2$color[i]))%>% as.integer())/255
  radarchart(rbind(max,min,res2[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res2$Generation[i]))
}

color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797",
         "#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A","#A6B91A")
