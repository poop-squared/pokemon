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


plot2 = ggplot(Generation.Legendary, aes(x = Generation, y = Generation.Legendary$x))
plot2 + 
  geom_bar(aes(fill = Legendary_Pokemon), stat = "identity", position = "dodge") +
  xlab("Generation") + 
  ylab("Number of Pokemon") + 
  ggtitle("Pokemon Distribution by Generation") + 
  theme(legend.position = "bottom") +
  guides(fill=guide_legend("Legendary", nrow = 1))




geom_text(aes(label=Number), position=position_dodge(width=0.9), vjust=-0.25)

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


pokedex.nonleg = pokedex[pokedex$Legendary=='False',]

## Non legendary Stats
#Top 10 HP Pokemon
pokedex.original%>% group_by(Name,HP)%>% arrange(desc(HP)) %>% ungroup()  %>%
  select("Name","Type.1","HP")%>%head(10)%>% data.frame()

#Top 10 Attack Pokemon
pokedex.original %>% group_by(Name,Attack)%>% arrange(desc(Attack)) %>% ungroup()  %>%
  select("Name","Type.1","Attack")%>%head(10)%>% data.frame()

#Top 10 Defense Pokemon
pokedex.original %>% group_by(Name,Defense)%>% arrange(desc(Defense)) %>% ungroup()  %>%
  select("Name","Type.1","Defense")%>%head(10)%>% data.frame()

#Top 10 Sp..Atk Pokemon
pokedex.original %>% group_by(Name,Sp.Atk)%>% arrange(desc(Sp.Atk)) %>% ungroup()  %>%
  select("Name","Type.1","Sp.Atk")%>%head(10)%>% data.frame()

#Top 10 SP..Def Pokemon
pokedex.original %>% group_by(Name,Sp.Def)%>% arrange(desc(Sp.Def)) %>% ungroup()  %>%
  select("Name","Type.1","Sp.Def")%>%head(10)%>% data.frame()

#Top 10 Speed Pokemon
pokedex.original%>% group_by(Name,Speed)%>% arrange(desc(Speed)) %>% ungroup()  %>%
  select("Name","Type.1","Speed")%>%head(10)%>% data.frame()

## Type 1 2 graph

hchart(pokedex.original$Type.1,type="column")
hchart(pokedex.original$Type.2,type="column")


## Stats distribution by type

#HP
hcboxplot(x=pokedex.original$HP,var=pokedex.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column") %>% hc_title(text='HP Distribution by Type')

ggplot(pokedex.original,aes(HP))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+ggtitle('HP Density by type')

#Attack
hcboxplot(x=pokedex.original$Attack,var=pokedex.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column") %>% hc_title(text='Attack Distribution by Type')

ggplot(pokedex.original,aes(Attack))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+ggtitle('Attack Density by type')

#Defense
hcboxplot(x=pokedex.original$Defense,var=pokedex.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")%>% hc_title(text='Defense Distribution by Type')


ggplot(pokedex.original,aes(Defense))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+facet_wrap(~Type.1)+
  ggtitle('Defense Density by type')

#SP. Attack
hcboxplot(x=pokedex.original$Sp.Atk,var=pokedex.original.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")%>% hc_title(text='SP. Attack Distribution by Type')

ggplot(pokedex.original,aes(Sp.Atk))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+ggtitle('Sp. Attack Density by type')

#SP. Def
hcboxplot(x=pokedex.original$Sp.Def,var=pokedex.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")%>% hc_title(text='SP. Defense Distribution by Type')

ggplot(pokedex.original,aes(Sp.Def))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+ggtitle('SP. Defense Density by type')

#Speed
hcboxplot(x=pokedex.original$Speed,var=pokedex.original$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column") %>% hc_title(text='Speed Distribution by Type')

ggplot(pokedex.original,aes(Speed))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)+ggtitle('Speed Density by type')


## By Generation
#HP
hcboxplot(x=pokedex.original$HP,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Attack
hcboxplot(x=pokedex.original$Attack,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.original,aes(Attack))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#Defense
hcboxplot(x=pokedex.original$Defense,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.original,aes(Defense))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#SP. Attack
hcboxplot(x=pokedex.original$Sp.Atk,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.original,aes(Sp.Atk))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#SP. Def
hcboxplot(x=pokedex.original$Sp.Def,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.original,aes(Sp.Def))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#Speed
hcboxplot(x=pokedex.original$Speed,var=pokedex.original$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.original,aes(Speed))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)


###################Distribution by Gen############################

color2<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136")

res2<-data.frame(pokedex %>% dplyr::select(Generation,HP, Attack, Defense, Sp.Atk, Sp.Def, Speed) %>% dplyr::group_by(Generation) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp.Atk + Sp.Def + Speed) %>% arrange(-sumChars))
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

###################  distribution by type ###########
color<-c("#6F35FC","#B7B7CE","#A98FF3","#F95587","#B6A136","#EE8130","#F7D02C","#705746","#735797",
         "#E2BF65","#96D9D6","#6390F0","#7AC74C","#C22E28","#D685AD","#A33EA1","#A8A77A")


res3<-data.frame(pokedex[pokedex$Legendary=='False',] %>% dplyr::select(Type.1,HP, Attack, Defense, Sp.Atk, Sp.Def, Speed) %>% dplyr::group_by(Type.1) %>% dplyr::summarise_all(funs(mean)) %>% mutate(sumChars = HP + Attack + Defense + Sp.Atk + Sp.Def + Speed) %>% arrange(-sumChars))
res3$color<-color
max<- ceiling(apply(res3[,2:7], 2, function(x) max(x, na.rm = TRUE)) %>% sapply(as.double)) %>% as.vector
min<-rep.int(0,6)
par(mfrow=c(3,6))
par(mar=c(1,1,1,1))
for(i in 1:nrow(res3)){
  curCol<-(col2rgb(as.character(res3$color[i]))%>% as.integer())/255
  radarchart(rbind(max,min,res3[i,2:7]),
             axistype=2 , 
             pcol=rgb(curCol[1],curCol[2],curCol[3], alpha = 1) ,
             pfcol=rgb(curCol[1],curCol[2],curCol[3],.5) ,
             plwd=2 , cglcol="grey", cglty=1, 
             axislabcol="black", caxislabels=seq(0,2000,5), cglwd=0.8, vlcex=0.8,
             title=as.character(res3$Type.1[i]))
}


### would be intersting to now add a total stats column and see that distribution between clusters

## Stats Corr and Distribution

ggpairs(pokedex.original[5:10])

## Finding best combination of attack + speed / sp.attack + speed / HP + Def  / Def + Sp.Def visual 
plot_ly(pokedex.original,x=~Attack,y=~Speed,type="scatter",mode="markers",text=~Name, color = ~as.factor(Generation)) 
plot_ly(pokedex.original,x=~Sp.Atk,y=~Speed,type="scatter",mode="markers",text=~Name,color = ~as.factor(Generation)) 
plot_ly(pokedex.original,x=~HP,y=~Defense,type="scatter",mode="markers",text=~Name,color = ~as.factor(Generation)) 
plot_ly(pokedex.original,x=~Defense,y=~Sp.Def,type="scatter",mode="markers",text=~Name,color = ~as.factor(Generation)) 
plot_ly(pokedex.original,x=~Defense,y=~Speed,type="scatter",mode="markers",text=~Name,color = ~as.factor(Generation)) 

unique(pokedex.original$cluster)
