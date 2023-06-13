library(gpinter)
data<-read.csv("Povcalnet 2017.csv")
View(data)

#CONSIGNES
# Pour tous pays récupérer la dernière année des donnée sdispo 
# estimer taux crissance  entre cette année kla et 2030 (cf tx croissance observée sur BM en $ 2017 PPP)
# imputer une croissance entre maintennat et 2030 (soit trouver une institiution tq BM ou GMS qui le fait soit prendre la croissance observée dans les 5 dernières années observ&es hors covid pour avoir un tx de recalibrage avec lequel on va multiplier le revenu total du pays)
# reproduire le travail du papier
# regarder le poverty gap à 2,15$ lvl indiv puis pays l'exprimer en $/personne (poverty  gap moyen) 
# faire une Hi=g entre 2030 va être la même pour chaque personne dans chaque pays
# calculer: trouver valeur au delà de laquelle il faut tout expropier pour combler pgap avec transferts internes
# autre indicateurs: au delà de 2,15$ impôt linéaire; quel tx appliquer pour combler pvgap et regarder tx linéaire de 2,15`$, 6,95$, 13$`


library(tidyverse)
data <- read.csv("Povcalnet 2017.csv")
temp <- data %>% group_by(country_code) %>% summarize(year_max= max(year))
year_max <- setNames(temp$year_max, temp$country_code)
data$year_max <- year_max[data$country_code]
data <- data[data$year == data$year_max,]
View(data)

data_pivot <- data %>%
  pivot_wider(names_from = percentile, values_from = avg_welfare)

str(data_pivot)

data_pivot <- data %>%
  filter(!is.na(avg_welfare)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_avg_welfare = mean(avg_welfare))
str(data_pivot)
data_wide <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_avg_welfare)

str(data_wide)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_avg_welfare, values_fill = 0) %>%
  head()
view(data_wide)

chiffresmoyenne <- names(data_wide)[-1]
moyenne <- rowMeans(data_wide[,chiffresmoyenne])

print(Croissance_pays[-c(1), ])

colnames(Croissance_pays)[1:2:3:4:5:6:7:8:9] <- c("country_code","Growth rate 2014", "Growth rate 2015","Growth rate 2016", "Growth rate 2017", "Growth rate 2018","Growth rate 2019","Growth rate 2020","Growth rate 2021")
print("Renamed Croissance_pays : ")
print(Croissance_pays)
View(Croissance_pays)

data_pivot <- data %>%
  filter(!is.na(avg_welfare)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_avg_welfare = mean(avg_welfare))
str(data_pivot)
data_avgwelf <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_avg_welfare)

str(data_avgwelf)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_avg_welfare, values_fill = 0) %>%
  head()
View(data_avgwelf)

merge(data_avgwelf,Croissance_pays)
View(merge(data_avgwelf,Croissance_pays))