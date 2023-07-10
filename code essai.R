library(dplyr)

consumption_countries <- p %>%
  filter(welfare_type == "consumption") %>%
  pull(country_code)

income_countries <- p %>%
  filter(welfare_type == "income") %>%
  pull(country_code)

install.packages("mapdata")
install.packages("maps")
install.packages("ggplot2")
library(mapdata)
library(maps)
library(ggplot2)
print(income_countries)
print(consumption_countries)

install.packages("countrycode")
library(countrycode)
world_map <- map_data("world")
country_names <- countrycode(consumption_countries, "iso3c", "country.name")
country_names2 <- countrycode(income_countries, "iso3c", "country.name")
country_names2 <- append(country_names2, "USA")
world_map$couleur <- "blanc"
world_map$couleur[world_map$region %in% country_names] <- "rouge"
world_map$couleur[world_map$region %in% country_names2] <- "bleu"
carte <- ggplot() +
  geom_map(data = world_map, map = world_map,
           aes(x = long, y = lat, map_id = region, fill = couleur),
           color = "black") +
  scale_fill_manual(values = c("rouge" = "red", "bleu" = "blue", "blanc" = "white"),
                    guide = FALSE) +
  theme_void()

print(carte)
ggsave("consumption_income_map.pdf", plot = carte)


plot_world_map("antipoverty_2_tax_2", breaks = c(0, .1, 1, 5, 10, 25, 50, 100, Inf),
               legend = "Linear tax rate\nabove $2.15/day\nrequired to lift all\nabove $2.15/day\n(in 2017 PPP)", #fill_na = T, 
               save = T, rev_color = T, format = c('png', 'pdf'), legend_x = .07, trim = T)





lignes_na2 <- gdp_pc[!complete.cases(gdp_pc[, 27:33]), ]

