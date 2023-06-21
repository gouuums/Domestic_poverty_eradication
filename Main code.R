# AF: tout ce qui est avant consigne peut être mis dans un fichier nommé ".Rprofile" pour que les paquets se chargent à l'ouverture du projet.
# AF: je ne vois aucune fonction dans votre code. Une bonne façon de coder c'est d'utiliser des fonctions (ma_fonction <- function(argument1) { ... } )
library(dplyr)
library(readr)
library(tidyverse)

# install.packages("devtools")
# devtools::install_github("thomasblanchet/gpinter")
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
data <- read.csv("Povcalnet 2017.csv") # AF: faut toujours mettre des chemins de fichiers relatifs, pas absolu
# AF: il vaut mieux écrire le code en anglais pour qu'il puisse être compris par le monde entier
Croissance_pays <- read_excel("Croissance pays.xls") # AF: c'est pas dans le répertoire ! Faut mettre ces trucs dans un dossier /Data dans le répertoire github
PIB_capita <- read_excel("C:/Users/elise/Documents/stage Cired/PIB_capita.xls") # AF: same here


#Récupération des données
temp <- data %>% group_by(country_code) %>% summarize(year_max= max(year))
year_max <- setNames(temp$year_max, temp$country_code)
data$year_max <- year_max[data$country_code]
data <- data[data$year == data$year_max,]
Croissance_pays <- read_excel("Croissance pays.xls")
PIB_capita <- read_excel("/Users/goumont/Desktop/Stage/PIB_capita.xls")

#Reglages tableau pour les pays qui n'ont pas un reporting level national
data$country_code <- ifelse(data$reporting_level %in% c("rural", "urban"), paste(data$country_code, data$reporting_level, sep = "_"), data$country_code)

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
Merge_5 <- merge(Merge_4, Croissance_pays, by = "country_code", all = TRUE)
Merge_5 <- Merge_5[!is.null(Merge_5$welftype1) & Merge_5$welftype1 != "NULL", ]

#Calcul du Poverty Gap individuel et national
Seuil <- 2.15
Pov_gap <- data_avgwelf
Pov_gap[, -1] <- apply(data_avgwelf[, -1], 2, function(x) ifelse(Seuil-x<0, 0, Seuil-x))
country_code <- colnames(data_avgwelf)[1]
Pov_gap_of_p <- paste0("Pov_gap_of_p", 1:(ncol(data_avgwelf)-1))
colnames(Pov_gap)[-1] <- Pov_gap_of_p
colnames(Pov_gap)[1] <- country_code
Pov_gap$Somme_pov_gap <- rowSums(Pov_gap[, -1], na.rm = TRUE)

# Croissance Moyenne
colnames(Croissance_pays)[8] <- c("z")
colnames(Croissance_pays)[7] <- c("Growth rate 2019 ")
Croissance_Pays2 <- subset(Croissance_pays, select=-c(z))
moyenne <- rowMeans(Croissance_Pays2[, -1], na.rm = TRUE)
Moyenne_croissance <- data.frame(RowMean = moyenne)
Croissance_Pays3 <- cbind(Croissance_Pays2, Moyenne_croissance$RowMean)
colnames(Croissance_Pays3)[1] <- country_code
Merge_6 <- merge(Croissance_Pays3, data_quantile, by="country_code",all = TRUE)
Merge_6 <- Merge_6[!is.na(Merge_6$quant1), ]
colnames(Merge_6)[9] <- c("Moyenne croissance")
Croissance_pays_final <- Merge_6[,c("country_code","Growth rate 2021","Moyenne croissance")]
missing_country_codes <- subset(Croissance_pays_final, !complete.cases(Croissance_pays_final$`Moyenne croissance`))$country_code
Croissance_pays_final[Croissance_pays_final$country_code == "ARG_urban", c("Growth rate 2021", "Moyenne croissance")] <- c(10.3982495, 0.9623874)
Croissance_pays_final[Croissance_pays_final$country_code == "CHN_rural", c("Growth rate 2021", "Moyenne croissance")] <- c(8.1097926, 7.0104461)
Croissance_pays_final[Croissance_pays_final$country_code == "CHN_urban", c("Growth rate 2021", "Moyenne croissance")] <- c(8.1097926, 7.0104461)
Croissance_pays_final[Croissance_pays_final$country_code == "IDN_rural", c("Growth rate 2021", "Moyenne croissance")] <- c(3.6912401, 4.8386664)
Croissance_pays_final[Croissance_pays_final$country_code == "IDN_urban", c("Growth rate 2021", "Moyenne croissance")] <- c(3.6912401, 4.8386664)
Croissance_pays_final[Croissance_pays_final$country_code == "IND_rural", c("Growth rate 2021", "Moyenne croissance")] <- c(8.6812287, 7.0473098)
Croissance_pays_final[Croissance_pays_final$country_code == "IND_urban", c("Growth rate 2021", "Moyenne croissance")] <- c(8.6812287, 7.0473098)
Croissance_pays_final[Croissance_pays_final$country_code == "SUR_urban", c("Growth rate 2021", "Moyenne croissance")] <- c(-2.7296188, -0.4452484)

#A REVOIR TOTALEMENT Calcul projections de Croissance
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
colnames(Merge_10)[4] <- c("X2021")
Merge_10$Ratio_PIB <- NA
for(i in 1:length(Merge_10$`PIB`)) for(j in 1:length(Merge_10$`X2021`)) Merge_10$Ratio_PIB[i] <- (Merge_10$`X2021`[i]/Merge_10$`PIB`[i])
colnames(Croissance_Pays3)[1] <- c("country_code")
Merge_11 <- merge(Merge_10, Croissance_Pays3)

Merge_11$Tx_Calage <- NA
for(i in 1:length(Merge_11$`Ratio_PIB`)) for(j in 1:length(Merge_11$`Moyenne_croissance$RowMean`)) Merge_11$Tx_Calage[i] <- (Merge_11$`Ratio_PIB`[i]*(1+Merge_11$`Moyenne_croissance$RowMean`[j]/100)^9)

data_calage <- merge( Merge_11, data_avgwelf)
data_calage <- data_calage[-c(2:13)]
calcul_calage <- data_calage
calcul_calage[,3:101] <- calcul_calage[,3:101]*calcul_calage[,2]
avgwelf_calage_of_p <- paste0("avgwelf_calage_of_p", 1:(ncol(calcul_calage)-2))
colnames(calcul_calage)[3:102] <- avgwelf_calage_of_p

#En cours de travail

#Peut on combler le poverty gap avec seulement des échanges internes?
Anti_pov_gap_nv_seuil <- merge(Pov_gap, data_quantile)
Anti_pov_gap_nv_seuil <- Anti_pov_gap_nv_seuil[-c(2:101)]
Anti_pov_gap_avg_welf <- merge(Pov_gap, data_avgwelf)
Anti_pov_gap_avg_welf <- Anti_pov_gap_avg_welf[-c(2:101)]
Anti_pov_gap <- data.frame(country_code = Anti_pov_gap_nv_seuil$country_code, Pov_gap_nat = Anti_pov_gap_nv_seuil$Somme_pov_gap )
Anti_pov_gap$fund <- 0
Anti_pov_gap$percentile_expropriated <- NA
for (row in 1:nrow(Anti_pov_gap)) {
  fund <- Anti_pov_gap[row, "fund"]
  i <- 102
  while (fund < Anti_pov_gap$Pov_gap_nat[row] && i >= 3) {
    percentile_expropriated_seuil <- colnames(Anti_pov_gap_nv_seuil)[i-1]
    percentile_expropriated_welf <- colnames(Anti_pov_gap_avg_welf)[i]
    fund <- fund + Anti_pov_gap_avg_welf[percentile_expropriated_welf] - Anti_pov_gap_nv_seuil[percentile_expropriated_seuil]
    i <- i - 1
  }
  if (i < 3 || fund >= Anti_pov_gap$Pov_gap_nat[row]) {
    Anti_pov_gap$percentile_expropriated[row] <- as.numeric(gsub("avgwelf", "", percentile_expropriated_welf))
    Anti_pov_gap[row, "fund"] <- fund
  }
}

write.csv2(Anti_pov_gap_avg_welf, file = "C:/Users/elise/Documents/stage Cired/Anti_pov_gap_avg_welf.csv", row.names = FALSE)
write.csv2(Anti_pov_gap_nv_seuil, file = "C:/Users/elise/Documents/stage Cired/Anti_pov_gap_nv_seuil.csv", row.names = FALSE)
write.csv2(Anti_pov_gap, file = "C:/Users/elise/Documents/stage Cired/Anti_pov_gap.csv", row.names = FALSE)

