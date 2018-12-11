

cluster.stats = pokedex[,c(14,5:10)]

aggregate(cluster.stats[,-1], by=list(cluster = cluster.stats$cluster), FUN = mean)
cluster.stats.agg = aggregate(cluster.stats[,-1], by=list(cluster = cluster.stats$cluster), FUN = mean)

pokedex.nonleg = pokedex[pokedex$Legendary=='False',]

## Non legendary Stats
#Top 10 HP Pokemon
pokedex.original%>% group_by(Name,HP)%>% arrange(desc(HP)) %>% ungroup()  %>%
  select("Name","Type.1","HP","cluster")%>%head(10)%>% data.frame()

#Top 10 Attack Pokemon
pokedex.original %>% group_by(Name,Attack)%>% arrange(desc(Attack)) %>% ungroup()  %>%
  select("Name","Type.1","Attack","cluster")%>%head(10)%>% data.frame()

#Top 10 Defense Pokemon
pokedex.original %>% group_by(Name,Defense)%>% arrange(desc(Defense)) %>% ungroup()  %>%
  select("Name","Type.1","Defense","cluster")%>%head(10)%>% data.frame()

#Top 10 Sp..Atk Pokemon
pokedex.original %>% group_by(Name,Sp..Atk)%>% arrange(desc(Sp..Atk)) %>% ungroup()  %>%
  select("Name","Type.1","Sp..Atk","cluster")%>%head(10)%>% data.frame()

#Top 10 SP..Def Pokemon
pokedex.original %>% group_by(Name,Sp..Def)%>% arrange(desc(Sp..Def)) %>% ungroup()  %>%
  select("Name","Type.1","Sp..Def","cluster")%>%head(10)%>% data.frame()

#Top 10 Speed Pokemon
pokedex.original%>% group_by(Name,Speed)%>% arrange(desc(Speed)) %>% ungroup()  %>%
  select("Name","Type.1","Speed","cluster")%>%head(10)%>% data.frame()

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


##By Cluster

#HP
hcboxplot(x=pokedex.original$HP,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Attack
hcboxplot(x=pokedex.original$Attack,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Defense
hcboxplot(x=pokedex.original$Defense,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


#SP. Attack
hcboxplot(x=pokedex.original$Sp.Atk,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


#SP. Def
hcboxplot(x=pokedex.original$Sp.Def,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Speed
hcboxplot(x=pokedex.original$Speed,var=pokedex.original$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


### wpoould be intersting to now add a total stats column and see that distribution between clusters

## Stats Corr and Distribution

ggpairs(pokedex.original[5:10])

## Finding best combination of attack + speed / sp.attack + speed / HP + Def  / Def + Sp.Def visual 
plot_ly(pokedex.original,x=~Attack,y=~Speed,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.original,x=~Sp.Atk,y=~Speed,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.original,x=~HP,y=~Defense,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.original,x=~Defense,y=~Sp.Def,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.original,x=~Defense,y=~Speed,type="scatter",mode="markers",text=~Name) 

unique(pokedex.original$cluster)


