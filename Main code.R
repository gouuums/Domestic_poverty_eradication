library(gpinter)
data<-read.csv("Povcalnet 2017.csv")
View(data)

#CONSIGNES
# Pour tous pays récupérer la dernière année des donnéeSdispo 
# estimer taux crissance  entre cette année kla et 2030 (cf tx croissance observée sur BM en $ 2017 PPP)
# imputer un croissance entre maintennat et 2030 (soit trouver une institiution tq BM ou GMS qui le fait soit prendre la croissance observée dans les 5 dernières années observ&es hors covid pour avoir un tx de recalibrage avec lequel on va multiplier le revenu total du pays)
# reproduire le travail du papier
# regarder le poverty gap à 2,15$ lvl indiv puis pays l'exprimer en $/personne (poverty  gap moyen) 
# faire une Hi=g entre 2030 va être la même pour chaque personne daNs chaque pays
# calculer: trouver valeur au delà de laquelle il faut tout expropier pour combler pgap avec transferts internes
# autre indicateurs: au delà de 2,15$ impôt linéaire; quel tx appliquer pour combler pvgap et regarder tx linéaire de 2,15`$, 6,95$, 13$`

#Récupération des données
library(tidyverse)
data <- read.csv("Povcalnet 2017.csv")
temp <- data %>% group_by(country_code) %>% summarize(year_max= max(year))
year_max <- setNames(temp$year_max, temp$country_code)
data$year_max <- year_max[data$country_code]
data <- data[data$year == data$year_max,]

#Tableau de l'average welfare par percentile
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

#Tableau du welfare share par percentile
data_pivot <- data %>%
  filter(!is.na(welfare_share)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_welfare_share = mean(welfare_share))
str(data_pivot)
data_welfshare <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_welfare_share)
str(data_welfshare)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_welfare_share, values_fill = 0) %>%
  head()

#Tableau de la pop share par percentile
data_pivot <- data %>%
  filter(!is.na(pop_share)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_pop_share = mean(pop_share))
str(data_pivot)
data_pop_share <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_pop_share)
str(data_pop_share)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_pop_share, values_fill = 0) %>%
  head()

#Tableau du quantile par percentile
data_pivot <- data %>%
  filter(!is.na(quantile)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_quantile = mean(quantile))
str(data_pivot)
data_quantile <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_quantile)
str(data_quantile)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_quantile, values_fill = 0) %>%
  head()

#Tableau des types de welfare
data_pivot <- data %>%
  filter(!is.na(welfare_type)) %>%
  group_by(country_code, percentile) %>%
  summarize(mean_welfare_type = welfare_type)
str(data_pivot)
data_welfare_type <- data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_welfare_type)
str(data_welfare_type)
data_pivot %>%
  pivot_wider(names_from = percentile, values_from = mean_welfare_type, values_fill = 0) %>%
  head()

#Croissance de 2014 à 2021
colnames(Croissance_pays)[1:2:3:4:5:6:7:8:9] <- c("country_code","Growth rate 2014", "Growth rate 2015","Growth rate 2016", "Growth rate 2017", "Growth rate 2018","Growth rate 2019","Growth rate 2020","Growth rate 2021")
print("Renamed Croissance_pays : ")
print(Croissance_pays)

#Reglages sur les noms des colonnes des percentiles
country_code <- colnames(data_welfare_type)[1]
welftype <- paste0("welftype", 1:(ncol(data_welfare_type)-1))
colnames(data_welfare_type)[-1] <- welftype
colnames(data_welfare_type)[1] <- country_code

country_code <- colnames(data_avgwelf)[1]
avgwelf <- paste0("avgwelf", 1:(ncol(data_avgwelf)-1))
colnames(data_avgwelf)[-1] <- avgwelf
colnames(data_avgwelf)[1] <- country_code

country_code <- colnames(data_pop_share)[1]
popshare <- paste0("popshare", 1:(ncol(data_pop_share)-1))
colnames(data_pop_share)[-1] <- popshare
colnames(data_pop_share)[1] <- country_code

country_code <- colnames(data_quantile)[1]
quant <- paste0("quant", 1:(ncol(data_quantile)-1))
colnames(data_quantile)[-1] <- quant
colnames(data_quantile)[1] <- country_code

country_code <- colnames(data_welfshare)[1]
welfshare <- paste0("welfshare", 1:(ncol(data_welfshare)-1))
colnames(data_welfshare)[-1] <- welfshare
colnames(data_welfshare)[1] <- country_code

#Grand tableau
Merge_1 <- merge(data_welfare_type, data_avgwelf)
Merge_2 <- merge(Merge_1,data_pop_share)
Merge_3 <- merge(Merge_2, data_welfshare)
Merge_4 <- merge(Merge_3,data_quantile)
Merge_5 <- merge(Merge_4, Croissance_pays)
View(Merge_5)

#Calcul du Poverty Gap individuel et national
Seuil <- 2.15

Pov_gap <- data_avgwelf
Pov_gap[, -1] <- apply(data_avgwelf[, -1], 2, function(x) ifelse(Seuil-x<0, 0, Seuil-x))
country_code <- colnames(data_avgwelf)[1]
Pov_gap_of_p <- paste0("Pov_gap_of_p", 1:(ncol(data_avgwelf)-1))
colnames(Pov_gap)[-1] <- Pov_gap_of_p
colnames(Pov_gap)[1] <- country_code
chiffresmoyenne <- names(Pov_gap)[-1]
Pov_gap$moyenne_nat <- rowMeans(Pov_gap[,chiffresmoyenne])
View(Pov_gap)

#En cours de travail

# Croissance

colnames(Croissance_pays)[8] <- c("z")
colnames(Croissance_pays)[7] <- c("Growth rate 2019 ")
subset(Croissance_pays, select=-c(z))
View(subset(Croissance_pays, select=-c(z)))
Croissance_Pays2 <- subset(Croissance_pays, select=-c(z))
View(Croissance_Pays2)
subset(Croissance_Pays2, select=-c(country_code))
View(subset(Croissance_Pays2, select=-c(country_code)))
moyenne <-rowMeans(subset(Croissance_Pays2, select=-c(country_code)))
Moyenne_croissance <- data.frame(RowMean = moyenne)
View(Moyenne_croissance)
Croissance_pays <- cbind(Croissance_pays, Moyenne_croissance$RowMean)
View(Croissance_pays)



                         
                                   
                                    
                                    
                       
         
                



