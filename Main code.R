install.packages("devtools")
devtools::install_github("thomasblanchet/gpinter")
library(gpinter)
data<-read.csv("Povcalnet 2017.csv")

#CONSIGNES
# Pour tous pays récupérer la dernière année des donnéeSdispo 
# estimer taux crissance  entre cette année kla et 2030 (cf tx croissance observée sur BM en $ 2017 PPP)
# imputer un croissance entre maintennat et 2030 (soit trouver une institiution tq BM ou GMS qui le fait soit prendre la croissance observée dans les 5 dernières années observ&es hors covid pour avoir un tx de recalibrage avec lequel on va multiplier le revenu total du pays)
# reproduire le travail du papier
# regarder le poverty gap à 2,15$ lvl indiv puis pays l'exprimer en $/personne (poverty  gap moyen) 
# faire une Hi=g entre 2030 va être la même pour chaque personne daNs chaque pays
# calculer: trouver valeur au delà de laquelle il faut tout expropier pour combler pgap avec transferts internes
# autre indicateurs: au delà de 2,15$ impôt linéaire; quel tx appliquer pour combler pvgap et regarder tx linéaire de 2,15`$, 6,95$, 13$`

#Récup données perso (Elise)
library(readr)
data <- read.csv("/Users/goumont/Desktop/Stage/Domestic_poverty_eradication/Povcalnet 2017.csv")
library(readxl)
Croissance_pays <- read_excel("/Users/goumont/Desktop/Stage/Domestic_poverty_eradication/Croissance pays.xls")
library(readxl)
PIB_capita <- read_excel("/Users/goumont/Desktop/Stage/Domestic_poverty_eradication/PIB_capita.xls")



#Récupération des données
library(tidyverse)
data <- read.csv("Povcalnet 2017.csv")
temp <- data %>% group_by(country_code) %>% summarize(year_max= max(year))
year_max <- setNames(temp$year_max, temp$country_code)
data$year_max <- year_max[data$country_code]
data <- data[data$year == data$year_max,]


library(readxl)
Croissance_pays <- read_excel("Croissance pays.xls")
library(readxl)
PIB_capita <- read_excel("/Users/goumont/Desktop/Stage/PIB_capita.xls")

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

# Croissance Moyenne
colnames(Croissance_pays)[8] <- c("z")
colnames(Croissance_pays)[7] <- c("Growth rate 2019 ")
Croissance_Pays2 <- subset(Croissance_pays, select=-c(z))
moyenne <- rowMeans(Croissance_Pays2[, -1], na.rm = TRUE)
Moyenne_croissance <- data.frame(RowMean = moyenne)
Croissance_Pays3 <- cbind(Croissance_Pays2, Moyenne_croissance$RowMean)

colnames(Croissance_Pays3)[1] <- country_code
Merge_6 <- merge(Croissance_Pays3, data_quantile, by="country_code")
View(Merge_6)
colnames(Merge_6)[9] <- c("Moyenne croissance")
library(dplyr)
Croissance_pays_final <- Merge_6[,c("country_code","Growth rate 2021","Moyenne croissance")]

# Calcul projections de Croissance

colnames(Moyenne_croissance)[1] <- c("x")
Croissance_pays_final$Growth_projections_2022 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2022[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^9*(Croissance_pays_final$`Growth rate 2021`[i])

Croissance_pays_final$Growth_projections_2023 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2023[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^8*(Croissance_pays_final$`Growth_projections_2022`[i])

Croissance_pays_final$Growth_projections_2024 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2024[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^7*(Croissance_pays_final$`Growth_projections_2023`[i])

Croissance_pays_final$Growth_projections_2025 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2025[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^6*(Croissance_pays_final$`Growth_projections_2024`[i])

Croissance_pays_final$Growth_projections_2026 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2026[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^5*(Croissance_pays_final$`Growth_projections_2025`[i])

Croissance_pays_final$Growth_projections_2027 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2027[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^4*(Croissance_pays_final$`Growth_projections_2026`[i])

Croissance_pays_final$Growth_projections_2028 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2028[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^3*(Croissance_pays_final$`Growth_projections_2027`[i])

Croissance_pays_final$Growth_projections_2029 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2029[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^2*(Croissance_pays_final$`Growth_projections_2028`[i])

Croissance_pays_final$Growth_projections_2030 <- NA
for(i in 1:length(Croissance_pays_final$`Moyenne croissance`)) Croissance_pays_final$Growth_projections_2030[i] <- ((Croissance_pays_final$`Moyenne croissance`[i]/100)+1)^1*(Croissance_pays_final$`Growth_projections_2029`[i])

#Merge croissance et pov gap dans notre grand tableau
colnames(Croissance_pays_final)[2] <- c("SUPP")
Croissance_pays_final <- subset(Croissance_pays_final, select=-c(SUPP))
Merge_7 <- merge(Merge_5, Croissance_pays_final)
Merge_8 <- merge(Merge_7, Pov_gap)

#Tri données PIB/capita
PIB_capita<-read_xls("PIB_capita.xls")
Merge_9 <- merge(PIB_capita, data_quantile , by="country_code")
PIB_capita_non_tri <- Merge_9[, 1:33]
colonne_year_max <- data$country_code[!duplicated(data$country_code)]
data_year_max <- data[!duplicated(data$country_code), ]
data_year_max <- data_year_max[,c("country_code","year")]
PIB_capita_non_tri <- merge(data_year_max,PIB_capita_non_tri)
PIB_capita_non_tri$year[PIB_capita_non_tri$year == 2020] <- 2019

PIB_capita_tri <- data.frame(
  country_code = PIB_capita_non_tri$country_code,
  focus_year = PIB_capita_non_tri$year,
  PIB = numeric(length(PIB_capita_non_tri$country_code))
)

for (i in 1:nrow(PIB_capita_non_tri)) {
  focus_year <- PIB_capita_non_tri$year[i]
  if (focus_year < 2022) {
    year_column <- colnames(PIB_capita_non_tri)[which(colnames(PIB_capita_non_tri) == paste0(focus_year))]
    PIB_capita_tri$PIB[i] <- PIB_capita_non_tri[i, year_column]
  }
  else PIB_capita_tri$PIB[i] = NA
}

#Calcul de ratio de PIB
PIB_pourcalcul <- PIB_capita_non_tri[ , - c(2:33)]
Merge_10 <- merge(PIB_capita_tri, PIB_pourcalcul, by="country_code")
View(Merge_10)
colnames(Merge_10)[4] <- c("X2021")
Merge_10$Ratio_PIB <- NA
for(i in 1:length(Merge_10$`PIB`)) for(j in 1:length(Merge_10$`X2021`)) Merge_10$Ratio_PIB[i] <- (Merge_10$`X2021`[i]/Merge_10$`PIB`[i])
colnames(Croissance_Pays3)[1] <- c("country_code")
Merge_11 <- merge(Merge_10, Croissance_Pays3)

#En cours de travail

Merge_11$Tx_Calage <- NA
for(i in 1:length(Merge_11$`Ratio_PIB`)) for(j in 1:length(Merge_11$`Moyenne_croissance$RowMean`)) Merge_11$Tx_Calage[i] <- (Merge_11$`Ratio_PIB`[i]*(1+Merge_11$`Moyenne_croissance$RowMean`[j]/100)^9)

data_calage <- merge( Merge_11, data_avgwelf)
data_calage <- data_calage[-c(2:13)]
calcul_calage <- data_calage
calcul_calage[,3:101] <- calcul_calage[,3:101]*calcul_calage[,2]
avgwelf_calage_of_p <- paste0("avgwelf_calage_of_p", 1:(ncol(calcul_calage)-2))
colnames(calcul_calage)[3:102] <- avgwelf_calage_of_p
View(calcul_calage)


#calculer pour chaque pays le "anti-poverty maximum"=le seuil à partir duquel il faut exproprier tous les revenus pour éradiquer la pauvreté au seuil de 2.15$ 
#calculer le "anti-poverty tax", i.e. taux de taxation (au-delà de, disons, 6.85$) nécessaire pour combler le poverty gap

# Calcul projections de Croissance bis


# Ratio des PIB en 2017 PPP pour trouver les taux de croissance en 2017 PPP

new_PIB2017 <- read_xlsx("/Users/goumont/Desktop/Stage/Mardi 20:06.xlsx")
new_PIB2017 <- new_PIB2017[-c(1), ]
colnames(new_PIB2017)[1] <- country_code
colnames(new_PIB2017)[2] <- c("X2015")
colnames(new_PIB2017)[3] <- c("X2016")
colnames(new_PIB2017)[4] <- c("X2017")
colnames(new_PIB2017)[5] <- c("X2018")
colnames(new_PIB2017)[6] <- c("X2019")
colnames(new_PIB2017)[7] <- c("X2021")
view(new_PIB2017)

G_PIB2017 <- merge(new_PIB2017, PIB_capita_tri)

G_PIB2017$X2015 <- as.numeric(G_PIB2017$X2015)
G_PIB2017$X2016 <- as.numeric(G_PIB2017$X2016)
G_PIB2017$X2017 <- as.numeric(G_PIB2017$X2017)
G_PIB2017$X2018 <- as.numeric(G_PIB2017$X2018)
G_PIB2017$X2019 <- as.numeric(G_PIB2017$X2019)
G_PIB2017$X2021 <- as.numeric(G_PIB2017$X2021)
G_PIB2017 <- G_PIB2017[, -10]
G_PIB2017$Growth_rate1 <- NA
for(i in 1:length(G_PIB2017$`X2015`)) for(i in 1:length(G_PIB2017$`X2016`)) G_PIB2017$Growth_rate1[i] <- (G_PIB2017$`X2016`[i]/G_PIB2017$`X2015`[i])
G_PIB2017$Growth_rate2 <- NA
for(i in 1:length(G_PIB2017$`X2016`)) for(i in 1:length(G_PIB2017$`X2017`)) G_PIB2017$Growth_rate2[i] <- (G_PIB2017$`X2017`[i]/G_PIB2017$`X2016`[i])
G_PIB2017$Growth_rate3 <- NA
for(i in 1:length(G_PIB2017$`X2017`)) for(i in 1:length(G_PIB2017$`X2018`)) G_PIB2017$Growth_rate3[i] <- (G_PIB2017$`X2018`[i]/G_PIB2017$`X2017`[i])
G_PIB2017$Growth_rate4 <- NA
for(i in 1:length(G_PIB2017$`X2018`)) for(i in 1:length(G_PIB2017$`X2019`)) G_PIB2017$Growth_rate4[i] <- (G_PIB2017$`X2019`[i]/G_PIB2017$`X2018`[i])
G_PIB2017$Growth_rate5 <- NA
for(i in 1:length(G_PIB2017$`X2019`)) for(i in 1:length(G_PIB2017$`X2021`)) G_PIB2017$Growth_rate5[i] <- (G_PIB2017$`X2021`[i]/G_PIB2017$`X2019`[i])
G_PIB2017 <- G_PIB2017[, -2]
G_PIB2017 <- G_PIB2017[, -3]
G_PIB2017 <- G_PIB2017[, -7]
G_PIB2017 <- G_PIB2017[, -4]
G_PIB2017 <- G_PIB2017[, -2]
G_PIB2017 <- G_PIB2017[, -2]
G_PIB2017 <- G_PIB2017[, -2]
G_PIB2017 <- G_PIB2017[, -1]

# Moyenne sur les 5 dernières années avec ces tx pour trouver le g en 2017 PPP

G_PIB2017$g_moyen <-rowMeans(G_PIB2017)
View(G_PIB2017)

# calcul des projections de PIB de 2022 à 2030

new_PIB2017bis <- merge(new_PIB2017,data_avgwelf, by=country_code)
View(new_PIB2017bis)
G_PIB2017_final <- cbind(new_PIB2017bis$country_code, new_PIB2017bis$X2021, G_PIB2017)
colnames(G_PIB2017_final)[1:2] <- c("country_code","PIB_2021")
View(G_PIB2017_final)

G_PIB2017_final$PIB_2021 <- as.numeric(G_PIB2017_final$PIB_2021)
G_PIB2017_final$PIBproj_2022 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2022[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^9*(G_PIB2017_final$`PIB_2021`[i])
G_PIB2017_final$PIBproj_2023 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2023[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^8*(G_PIB2017_final$`PIBproj_2022`[i])
G_PIB2017_final$PIBproj_2024 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2024[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^7*(G_PIB2017_final$`PIBproj_2023`[i])
G_PIB2017_final$PIBproj_2025 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2025[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^6*(G_PIB2017_final$`PIBproj_2024`[i])
G_PIB2017_final$PIBproj_2026 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2026[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^5*(G_PIB2017_final$`PIBproj_2025`[i])
G_PIB2017_final$PIBproj_2027 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2027[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^4*(G_PIB2017_final$`PIBproj_2026`[i])
G_PIB2017_final$PIBproj_2028 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2028[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^3*(G_PIB2017_final$`PIBproj_2027`[i])
G_PIB2017_final$PIBproj_2029 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2029[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^2*(G_PIB2017_final$`PIBproj_2028`[i])
G_PIB2017_final$PIBproj_2030 <- NA
for(i in 1:length(G_PIB2017_final$`g_moyen`)) G_PIB2017_final$PIBproj_2030[i] <- ((G_PIB2017_final$`g_moyen`[i]/100)+1)^1*(G_PIB2017_final$`PIBproj_2029`[i])
View(G_PIB2017_final)





