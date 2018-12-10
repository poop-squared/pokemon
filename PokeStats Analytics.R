library(gplots)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(fmsb)
library(corrplot)
library(corrgram)
library(caTools)
library(gplots)
library(RColorBrewer)
library(ggradar)
library(ggplot2)
library(tidytext)
library(tidyverse)
library(highcharter)
library(plotly)
library(janitor)
library(GGally)

cluster.stats = pokedex[,c(14,5:10)]

aggregate(cluster.stats[,-1], by=list(cluster = cluster.stats$cluster), FUN = mean)
cluster.stats.agg = aggregate(cluster.stats[,-1], by=list(cluster = cluster.stats$cluster), FUN = mean)

pokedex.nonleg = pokedex[pokedex$Legendary=='False',]

## Non legendary Stats
#Top 10 HP Pokemon
pokedex.nonleg%>% group_by(Name,HP)%>% arrange(desc(HP)) %>% ungroup()  %>%
  select("Name","Type.1","HP","cluster")%>%head(10)%>% data.frame()

#Top 10 Attack Pokemon
pokedex.nonleg %>% group_by(Name,Attack)%>% arrange(desc(Attack)) %>% ungroup()  %>%
  select("Name","Type.1","Attack","cluster")%>%head(10)%>% data.frame()

#Top 10 Defense Pokemon
pokedex.nonleg %>% group_by(Name,Defense)%>% arrange(desc(Defense)) %>% ungroup()  %>%
  select("Name","Type.1","Defense","cluster")%>%head(10)%>% data.frame()

#Top 10 Sp..Atk Pokemon
pokedex.nonleg %>% group_by(Name,Sp..Atk)%>% arrange(desc(Sp..Atk)) %>% ungroup()  %>%
  select("Name","Type.1","Sp..Atk","cluster")%>%head(10)%>% data.frame()

#Top 10 SP..Def Pokemon
pokedex.nonleg %>% group_by(Name,Sp..Def)%>% arrange(desc(Sp..Def)) %>% ungroup()  %>%
  select("Name","Type.1","Sp..Def","cluster")%>%head(10)%>% data.frame()

#Top 10 Speed Pokemon
pokedex.nonleg %>% group_by(Name,Speed)%>% arrange(desc(Speed)) %>% ungroup()  %>%
  select("Name","Type.1","Speed","cluster")%>%head(10)%>% data.frame()

## Type 1 2 graph

hchart(pokedex$Type.1,type="column")
hchart(pokedex$Type.2,type="column")


## Stats distribution by type

#HP
hcboxplot(x=pokedex.nonleg$HP,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(HP))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)

#Attack
hcboxplot(x=pokedex.nonleg$Attack,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Attack))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)

#Defense
hcboxplot(x=pokedex.nonleg$Defense,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Defense))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)

#SP. Attack
hcboxplot(x=pokedex.nonleg$Sp..Atk,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Sp..Atk))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)

#SP. Def
hcboxplot(x=pokedex.nonleg$Sp..Def,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Sp..Def))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)

#Speed
hcboxplot(x=pokedex.nonleg$Speed,var=pokedex.nonleg$Type.1,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Speed))+geom_density(color="black", fill="lightblue")+facet_wrap(~Type.1)


## By Generation
#HP
hcboxplot(x=pokedex.nonleg$HP,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Attack
hcboxplot(x=pokedex.nonleg$Attack,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Attack))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#Defense
hcboxplot(x=pokedex.nonleg$Defense,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Defense))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#SP. Attack
hcboxplot(x=pokedex.nonleg$Sp..Atk,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Sp..Atk))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#SP. Def
hcboxplot(x=pokedex.nonleg$Sp..Def,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Sp..Def))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)

#Speed
hcboxplot(x=pokedex.nonleg$Speed,var=pokedex.nonleg$Generation,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

ggplot(pokedex.nonleg,aes(Speed))+geom_density(color="black", fill="lightblue")+facet_wrap(~Generation)


##By Cluster

#HP
hcboxplot(x=pokedex.nonleg$HP,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Attack
hcboxplot(x=pokedex.nonleg$Attack,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Defense
hcboxplot(x=pokedex.nonleg$Defense,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


#SP. Attack
hcboxplot(x=pokedex.nonleg$Sp..Atk,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


#SP. Def
hcboxplot(x=pokedex.nonleg$Sp..Def,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")

#Speed
hcboxplot(x=pokedex.nonleg$Speed,var=pokedex.nonleg$cluster,color="green") %>%
  hc_add_theme(hc_theme_economist())%>%hc_chart(type = "column")


### wpoould be intersting to now add a total stats column and see that distribution between clusters

## Stats Corr and Distribution

ggpairs(pokedex.nonleg[5:10])

## Finding best combination of attack + speed / sp.attack + speed / HP + Def  / Def + Sp.Def
plot_ly(pokedex.nonleg,x=~Attack,y=~Speed,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.nonleg,x=~Sp..Atk,y=~Speed,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.nonleg,x=~HP,y=~Defense,type="scatter",mode="markers",text=~Name) 
plot_ly(pokedex.nonleg,x=~Defense,y=~Sp..Def,type="scatter",mode="markers",text=~Name) 

