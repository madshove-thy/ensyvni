#--------------
# VALGKAMPEN OVERALL

# Load libraries
library(tidyverse)
library(openxlsx)
library(plotly)
library(reactable)
library(crosstalk)
library(tidytext)
library(shiny)

# Til leaflet
library(leaflet)
library(leaflet.extras)
library(htmlwidgets) # Til at gemme maps
library(leaflegend)

#---------------------
# META

# # Load data
# A <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Parti_ads.rds") # Ads
# R <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Parti_region.rds") # Region
# D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Parti_demo.rds") # Demographics

# Load data (new)
A <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Parti_ads.rds") %>% # Ads
  filter(ad_delivery_start_time >= "2022-08-01")
R <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Parti_region.rds") %>% # Region
  filter(ad_delivery_start_time >= "2022-08-01")
D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Parti_demo.rds") %>% # Demographics
  filter(ad_delivery_start_time >= "2022-08-01")

# Load geodata
GD <- readRDS("/Users/Madshove/OneDrive/Mads/golem/Spatial-data/G-regioner.rds")
# Convert geodata so we can use it in ggplot
GD = sf::st_as_sf(GD)
# Make simpler version om dataframe
GD <- sf::st_simplify(GD, preserveTopology = TRUE, dTolerance = 1000)
# Let’s go back to the WGS84 CRS (this is the CRS used by leaflet):
GD <- GD %>% sf::st_transform(4326)
# Check object size
object.size(GD)

# Colors
party.col <- c("Socialdemokratiet" = "#BF0418", "Radikale Venstre" = "#E82583", "Konservative" = "#00571F", "Nye Borgerlige" = "#004450", "SF" = "#F04D46",
               "Liberal Alliance" = "#12213f", "Dansk Folkeparti" = "#E7D01E", "Venstre" = "#005392", "Enhedslisten" = "#C21B3E", "Frie Grønne" = "#DAA520",
               "Alternativet" = "#00ff00", "Danmarksdemokraterne" = "#33F3FF",
               "Kristendemokraterne" = "#334CFF", 
               # "Veganerpartiet" = "#52BE80",
               "Moderaterne" = "#6C3483")

# Change demographic names to "Mand" "Kvinde"
D$gender[D$gender == "male"] <- "Mand"
D$gender[D$gender == "female"] <- "Kvinde"

#---------------------
# REGIONAL FOCUS
# Now group by region and party
DR <- R %>%
  filter(page_name != "Pernille Vermund") %>% # She skews the picture
  group_by(Parti) %>%
  mutate(percentagesum = sum(percentage)) %>%
  group_by(Parti, region) %>%
  summarise(value = sum(percentage)/percentagesum) %>%
  filter(region != "Unknown")

# Remove duplicates
DR <- DR[!duplicated(DR), ]

# Get some of the important variables back on
DR <- merge(x = DR, y = A[ , c("Parti", "Partibogstav", "Partikort")], by = "Parti")
DR <- DR[!duplicated(DR), ]

# Keep only regions we want
regions <- c("Capital Region of Denmark", "Central Denmark Region",
             "North Denmark Region", "Region of Southern Denmark",
             "Zealand Region")

DR <- DR %>%
  filter(region %in% regions)

DR <- DR %>%
  mutate(navn = recode_factor(region, "Capital Region of Denmark" = "Region Hovedstaden",
                       "Central Denmark Region" = "Region Midtjylland",
                       "North Denmark Region" = "Region Nordjylland",
                       "Region of Southern Denmark" = "Region Syddanmark",
                       "Zealand Region" = "Region Sjælland"
  ))

# Combine D with some coordinates
G <- merge(x = GD, y = DR, by = "navn")

# LEAFLET
L <- list()

for(i in unique(G$Parti)) {
  L[[i]] <- G %>% filter(Parti == i)
}

pal <- colorNumeric(
  palette = "RdYlBu",
  domain = 0:1,
  reverse = TRUE#,
  #bins = c(0, 0.10, 0.15, 0.2, 0.25, 0.30, 1)
)

mapDK <- leaflet(options = leafletOptions(dragging = FALSE, zoomControl = FALSE,
                                          minZoom = 06, maxZoom = 06)) %>%
  #setView(lng = 9.105516, lat = 55.39548, zoom = 09) %>%
  addProviderTiles("CartoDB") %>%
  addPolygons(data = L[["Socialdemokratiet"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Socialdemokratiet"]][["navn"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Socialdemokratiet"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Socialdemokratiet") %>%
  addPolygons(data = L[["Venstre"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Venstre"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Venstre"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Venstre") %>%
  addPolygons(data = L[["Det Konservative Folkeparti"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Det Konservative Folkeparti"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Det Konservative Folkeparti"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Konservative") %>%
  addPolygons(data = L[["Radikale Venstre"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Radikale Venstre"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Radikale Venstre"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Radikale") %>%
  addPolygons(data = L[["Nye Borgerlige"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Nye Borgerlige"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Nye Borgerlige"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Nye Borgerlige") %>%
  addPolygons(data = L[["SF"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["SF"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["SF"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "SF") %>%
  addPolygons(data = L[["Liberal Alliance"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Liberal Alliance"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Liberal Alliance"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Liberal Alliance") %>%
  addPolygons(data = L[["Dansk Folkeparti"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Dansk Folkeparti"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Dansk Folkeparti"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Dansk Folkeparti") %>%
  addPolygons(data = L[["Enhedslisten"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Enhedslisten"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Enhedslisten"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Enhedslisten") %>%
  addPolygons(data = L[["Danmarksdemokraterne"]], weight = 1, color = "black",
            smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
            popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
              "<b>Region: </b>", L[["Danmarksdemokraterne"]][["region"]], "<br/>",
              "<b>Procent visninger: </b>", round(L[["Danmarksdemokraterne"]][["value"]]*100, 1), "%", "<br/>"
            ),
            fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
            group = "Danmarksdemokraterne") %>%
  addPolygons(data = L[["Frie Grønne"]], weight = 1, color = "black", 
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Frie Grønne"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Frie Grønne"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Frie Groenne") %>%
  addPolygons(data = L[["Moderaterne"]], weight = 1, color = "black",
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Moderaterne"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Moderaterne"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Moderaterne") %>%
  addPolygons(data = L[["Kristendemokraterne"]], weight = 1, color = "black",
              smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
              popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
                "<b>Region: </b>", L[["Kristendemokraterne"]][["region"]], "<br/>",
                "<b>Procent visninger: </b>", round(L[["Kristendemokraterne"]][["value"]]*100, 1), "%", "<br/>"
              ),
              fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
              group = "Kristendemokraterne") %>%
  addPolygons(data = L[["Alternativet"]], weight = 1, color = "black",
            smoothFactor = 0.5, stroke = TRUE, # fikser linjer ml. enheder
            popup = paste( # Noedt til at vaere popup (ikke label) for at kunne skille linjerne ad
              "<b>Region: </b>", L[["Alternativet"]][["region"]], "<br/>",
              "<b>Procent visninger: </b>", round(L[["Alternativet"]][["value"]]*100, 1), "%", "<br/>"
            ),
            fillColor = ~pal(value), fillOpacity = 1, # udfylder farver
            group = "Alternativet") %>%
  addLayersControl(
    baseGroups = c("Socialdemokratiet", "Venstre", "Konservative", "Radikale",
                      "Nye Borgerlige", "SF", "Liberal Alliance", "Dansk Folkeparti",
                      "Enhedslisten", "Frie Groenne", "Alternativet", "Danmarksdemokraterne"
                      , "Moderaterne", "Kristendemokraterne"
    ),
    options = layersControlOptions(collapsed = FALSE)) %>%
  hideGroup(c("Venstre", "Konservative", "Radikale", "Enhedslisten", "SF",
              "Dansk Folkeparti", "Nye Borgerlige", "Liberal Alliance",
              "Alternativet", "Frie Groenne", "Danmarksdemokraterne"
              , "Moderaterne", "Kristendemokraterne"
              )) %>%
  addLegend("bottomleft",
            pal = pal,
            values = 0:1,
            title= "Annoncevisninger procent",
            opacity = 1)

#---------------------
# AGE AND GENDER FOCUS
D <- D %>%
  filter(D$gender!="unknown")

# Change party names
D$Parti[D$Parti == "Det Konservative Folkeparti"] <- "Konservative"

## Which gender does politicians focus on?
# # Old way of doing it
# D1 <- D %>%
#   group_by(Parti) %>%
#   mutate(perc = percentage/sum(percentage)) %>%
#   group_by(gender, Parti) %>%
#   summarise(value = sum(perc))

# New way of doing it (inspiration from DutchElectionObservatory)
D1 <- D %>%
  filter(age != "13-17") %>%
  filter(gender != "unknown") %>%
  group_by(page_name) %>%
  complete(id, age, gender, Parti, fill = list(percentage = 0)) %>%
  ungroup() %>%
  group_by(id, gender, Parti) %>%
  summarise(perc = sum(percentage)) %>%
  ungroup() %>%
  group_by(Parti, gender) %>%
  summarise(value = mean(perc)) %>%
  ungroup()

# Plot
gender <- D1 %>%
  #filter(page_name == "Radikale Venstre") %>%
  ggplot(aes(x = gender, y = value*100, fill = Parti,
             text = paste(
               "Køn: ", gender, "\n",
               "Parti: ", Parti, "\n",
               "Eksponering: ", round(value*100,2), "%"
             ))) +
  geom_bar(stat = "Identity") +
  geom_hline(yintercept = 50, linetype = "dotted") +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 10, color = "black"),
        axis.title = element_text(size = 10, color = "black"),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold"),
        title = element_text(size = 12),
        panel.grid.major = element_blank(),
        text = element_text(family = "Helvetica")) +
  scale_fill_manual(values = party.col) +
  ylim(0, 100) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Demografisk målretning (køn) pr. parti",
       subtitle = "Pr. parti",
       caption = "Baseret på Meta annoncer siden 1. august 2022") +
  facet_wrap(~ Parti, ncol = 5)

## Which age does politicians focus on?
# # Old way of doing it
# D2 <- D %>%
#   group_by(Parti) %>%
#   mutate(perc = percentage/sum(percentage)) %>%
#   group_by(age, Parti) %>%
#   summarise(value = sum(perc)) %>%
#   ungroup()

# New way of doing it
D2 <- D %>%
  filter(age != "13-17") %>%
  filter(gender != "unknown") %>%
  group_by(page_name) %>%
  complete(id, age, gender, Parti, fill = list(percentage = 0)) %>%
  ungroup() %>%
  group_by(id, age, Parti) %>%
  summarise(perc = sum(percentage)) %>%
  ungroup() %>%
  group_by(Parti, age) %>%
  summarise(value = mean(perc)) %>%
  ungroup()

# Bar plot
age <- D2 %>%
  filter(age != "13-17") %>%
  ggplot(aes(x = age, y = value*100, fill = Parti,
             text = paste(
               "Alder: ", age, "\n",
               "Parti: ", Parti, "\n",
               "Eksponering: ", round(value*100,2), "%"
             ))) +
  geom_bar(stat = "Identity") +
  #geom_bar(stat = "Identity", data = D2[5, ], aes(x=age, y=value*100), colour="red", size=2) +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 8, color = "black"),
        legend.position="none",
        strip.text = element_text(size = 10, face = "bold"),
        title = element_text(size = 12),
        panel.grid.major = element_blank(),
        text = element_text(family = "Helvetica")) +
  scale_fill_manual(values = party.col) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Demografisk målretning (alder) pr. parti",
       subtitle = "Pr. parti",
       caption = "Baseret på Meta annoncer siden 1. august 2022") +
  facet_wrap(~ Parti, ncol = 5)

# GENDER AND AGE TOGETHER
# # Old way of doing it
# D3 <- D %>%
#   group_by(Parti) %>%
#   mutate(perc = percentage/sum(percentage)) %>%
#   group_by(age, gender, Parti) %>%
#   summarise(value = sum(perc)) %>%
#   ungroup()

# New way of doing it
D3 <- D %>%
  filter(age != "13-17") %>%
  filter(gender != "unknown") %>%
  group_by(page_name) %>%
  complete(id, age, gender, Parti, fill = list(percentage = 0)) %>%
  ungroup() %>%
  group_by(id, age, gender, Parti) %>%
  summarise(perc = sum(percentage)) %>%
  ungroup() %>%
  group_by(Parti, age, gender) %>%
  summarise(value = mean(perc)) %>%
  ungroup()

gender_age <- D3 %>%
  # filter(age != "13-17") %>%
  ggplot(aes(x = age, y = value*100, fill = gender,
             text = paste(
               "Alder: ", age, "\n",
               "Køn: ", gender, "\n",
               "Parti: ", Parti, "\n",
               "Eksponering: ", round(value*100,2), "%"
             ))) +
  geom_bar(stat = "Identity", position = "stack") +
  #geom_bar(stat = "Identity", data = D2[5, ], aes(x=age, y=value*100), colour="red", size=2) +
  theme_bw() +
  theme(axis.text = element_text(size = 8, color = "black"),
        axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 0.5, hjust=0.5),
        axis.title = element_text(size = 10, color = "black"),
        legend.position="bottom",
        strip.text = element_text(size = 10, face = "bold"),
        title = element_text(size = 12),
        panel.grid.major = element_blank(),
        text = element_text(family = "Helvetica")) +
  # scale_fill_manual(values = party.col) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Demografisk målretning alder og køn",
       subtitle = "Pr. parti",
       caption = "Baseret på Meta annoncer siden 1. august 2022") +
  facet_wrap(~ Parti, ncol = 5)

#---------------------
# MONEY SPEND OVER TIME
# Make spend column with mean spend
A <- A %>%
  mutate(spend = rowMeans(A[ , c("spend_lower", "spend_upper")], na.rm=TRUE))

# Change page names
A$Parti[A$Parti == "Det Konservative Folkeparti"] <- "Konservative"

# Plot
Money_day <- A %>%
  filter(ad_delivery_start_time >= "2022-08-01") %>%
  group_by(Parti, ad_delivery_start_time) %>%
  summarise(spend = sum(spend),
            spend_lower = sum(spend_lower),
            spend_upper = sum(spend_upper)) %>%
  ungroup() %>%
  complete(ad_delivery_start_time, Parti) %>%
  mutate(spend = replace_na(spend, 0),
         spend_lower = replace_na(spend_lower, 0),
         spend_upper = replace_na(spend_upper, 0)) %>%
  group_by(Parti) %>%
  mutate(value = cumsum(spend),
         spend_lower = cumsum(spend_lower),
         spend_upper = cumsum(spend_upper)) %>%
  ggplot(aes(x = as.Date(ad_delivery_start_time), y = value/1000, color = Parti, group = Parti,
             text = paste(
               "<b>Parti: ", Parti, "\n",
               "Dato: ", ad_delivery_start_time, "\n",
               "Est. akkumuleret forbrug: ", format(value, big.mark = ".", decimal.mark = ","), "kr. </b>", "\n",
               "Min. akkumuleret forbrug: ", format(spend_lower, big.mark = ".", decimal.mark = ","), "kr.", "\n",
               "Max. akkumuleret forbrug: ", format(spend_upper, big.mark = ".", decimal.mark = ","), "kr."
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = party.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = "1.000 kr.",
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret annonceforbrug pr. dag siden 1. august",
       subtitle = "",
       caption = "")

# Money_total <- A %>%
#   filter(ad_delivery_start_time >= "2022-08-01") %>%
#   group_by(Parti) %>%
#   summarise(value = sum(spend)) %>%
#   ggplot(aes(x = value/1000, y = reorder(Parti, value), fill = Parti,
#              text = paste(
#                "Parti: ", Parti, "\n",
#                "Forbrug: ", format(value, big.mark = ".", decimal.mark = ","), "kr."
#              ))) +
#   geom_bar(stat = "Identity") +
#   scale_fill_manual(values = party.col) +
#   scale_x_continuous(limits = c(0, 250), breaks = seq(0, 200, by = 50)) +
#   theme_bw() +
#   theme(axis.text = element_text(size = 12, color = "black"),
#         axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5, hjust=0.5),
#         axis.title = element_text(size = 12, color = "black"),
#         legend.position="none",
#         strip.text = element_text(size = 12, face = "bold"),
#         title = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         text = element_text(family = "Helvetica")) +
#   labs(y = NULL,
#        x = NULL,
#        shape = NULL, 
#        col = NULL,
#        title = "Forbrug på annoncer i 1.000 kr. siden 1. august 2022",
#        subtitle = "i 1.000 kr. siden 1. august 2022",
#        caption = "Data er hentet fra Facebook Ad Library \n gælder perioden 1. august til seneste opdatering")

ad_day <- A %>%
  filter(ad_delivery_start_time >= "2022-08-01") %>%
  distinct(ad_creative_bodies, Parti, ad_delivery_start_time) %>%
  group_by(Parti, ad_delivery_start_time) %>%
  summarise(nads = n()) %>%
  ungroup() %>%
  complete(ad_delivery_start_time, Parti) %>%
  mutate(nads = replace_na(nads, 0)) %>%
  group_by(Parti) %>%
  mutate(value = cumsum(nads)) %>%
  ggplot(aes(x = as.Date(ad_delivery_start_time), y = value, color = Parti, group = Parti,
             text = paste(
               "Parti: ", Parti, "\n",
               "Dato: ", ad_delivery_start_time, "\n",
               "Antal annoncer: ", format(value, big.mark = ".", decimal.mark = ",")
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = party.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Antal annoncer siden 1. august",
       subtitle = "",
       caption = "")

gender_plotly <- ggplotly(gender, tooltip = "text") %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
age_plotly <- ggplotly(age, tooltip = "text") %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
# Money_total_plotly <- ggplotly(Money_total, tooltip = "text")
Money_day_plotly <- ggplotly(Money_day, tooltip = "text") %>% 
  layout(legend = list(orientation = 'h'),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
gender_age_plotly <- ggplotly(gender_age, tooltip = "text")
ad_day_plotly <- ggplotly(ad_day, tooltip = "text") %>% 
  layout(legend = list(orientation = 'h'),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))






# #---------------
# # INTEREST GROUPS
# # Load data
# A <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Interestgroup_ads.rds") # Ads
# R <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Interestgroup_region.rds") # Region
# D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Interestgroup_demo.rds") # Demographics
# 
# # Change demographic names to "Mand" "Kvinde"
# D$gender[D$gender == "male"] <- "Mand"
# D$gender[D$gender == "female"] <- "Kvinde"
# 
# #---------------------
# # REGIONAL FOCUS
# # Now group by region and party
# DR <- R %>%
#   group_by(Navn) %>%
#   mutate(percentagesum = sum(regionvalue)) %>%
#   group_by(Navn, region) %>%
#   summarise(value = sum(regionvalue)/percentagesum) %>%
#   filter(region != "Unknown")
# 
# # Remove duplicates
# DR <- DR[!duplicated(DR), ]
# 
# # Get some of the important variables back on
# DR <- merge(x = DR, y = A[ , c("Navn")], by = "Navn")
# DR <- DR[!duplicated(DR), ]
# 
# # Keep only regions we want
# regions <- c("Capital Region of Denmark", "Central Denmark Region",
#              "North Denmark Region", "Region of Southern Denmark",
#              "Zealand Region")
# 
# DR <- DR %>%
#   filter(region %in% regions)
# 
# DR <- DR %>%
#   mutate(navn = recode(region, "Capital Region of Denmark" = "Region Hovedstaden",
#                        "Central Denmark Region" = "Region Midtjylland",
#                        "North Denmark Region" = "Region Nordjylland",
#                        "Region of Southern Denmark" = "Region Syddanmark",
#                        "Zealand Region" = "Region Sjælland"
#   ))
# 
# #---------------------
# # AGE AND GENDER FOCUS
# D <- D %>%
#   filter(D$gender!="unknown")
# 
# ## Which gender does politicians focus on?
# D1 <- D %>%
#   group_by(Navn) %>%
#   mutate(perc = demovalue/sum(demovalue)) %>%
#   group_by(gender, Navn) %>%
#   summarise(value = sum(perc))
# 
# # GENDER AND AGE TOGETHER
# D3 <- D %>%
#   group_by(Navn) %>%
#   mutate(perc = demovalue/sum(demovalue)) %>%
#   group_by(age, gender, Navn) %>%
#   summarise(value = sum(perc)) %>%
#   ungroup()
# 
# I_gender_age <- D3 %>%
#   #filter(page_name == "Nye Borgerlige") %>%
#   ggplot(aes(x = age, y = value*100, fill = gender,
#              text = paste(
#                "Alder: ", age, "\n",
#                "Køn: ", gender, "\n",
#                "Organisation: ", Navn, "\n",
#                "Eksponering: ", round(value*100,2), "%"
#              ))) +
#   geom_bar(stat = "Identity", position = "stack") +
#   theme_bw() +
#   theme(axis.text = element_text(size = 8, color = "black"),
#         axis.text.x = element_text(size = 10, color = "black", angle = 45, vjust = 0.5, hjust=0.5),
#         axis.title = element_text(size = 10, color = "black"),
#         legend.position="bottom",
#         strip.text = element_text(size = 10, face = "bold"),
#         title = element_text(size = 12),
#         panel.grid.major = element_blank(),
#         text = element_text(family = "Helvetica")) +
#   labs(y = NULL,
#        x = NULL,
#        shape = NULL, 
#        col = NULL,
#        title = "Demografisk målretning alder og køn pr. parti",
#        subtitle = "",
#        caption = "Data er hentet fra Facebook Ad Library") +
#   facet_wrap(~ Navn, ncol = 5)
# 
# #---------------------
# # MONEY SPEND OVER TIME
# # Plot
# I_Money_day <- A %>%
#   filter(ad_delivery_start_time >= "2022-08-01") %>%
#   group_by(Navn, ad_delivery_start_time) %>%
#   summarise(spend = sum(spend_upper)) %>%
#   ungroup() %>%
#   complete(ad_delivery_start_time, Navn) %>%
#   mutate(spend = replace_na(spend, 0)) %>%
#   group_by(Navn) %>%
#   mutate(value = cumsum(spend)) %>%
#   ggplot(aes(x = as.Date(ad_delivery_start_time), y = value, color = Navn, group = Navn,
#              text = paste(
#                "Organisation: ", Navn, "\n",
#                "Dato: ", ad_delivery_start_time, "\n",
#                "Akkumuleret forbrug: ", format(value, big.mark = ".", decimal.mark = ","), "kr."
#              ))) +
#   geom_line() +
#   scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
#                limits = c(as.Date("2022-08-01"), as.Date("2022-10-15"))) +
#   # scale_color_manual(values = party.col) +
#   theme_bw() +
#   theme(axis.ticks = element_blank(),
#         axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
#         title = element_text(size = 12, color = "black"),
#         text = element_text(family = "Helvetica")) +
#   labs(y = NULL,
#        x = NULL,
#        shape = NULL, 
#        col = NULL,
#        title = "Akkumuleret annonceforbrug pr. dag siden 1. august",
#        subtitle = "",
#        caption = "")
# 
# 
# I_Money_day_plotly <- ggplotly(I_Money_day, tooltip = "text") %>% 
#   layout(legend = list(orientation = 'h'))
# I_gender_age_plotly <- ggplotly(I_gender_age, tooltip = "text")





#---------------
# GOOGLE / YOUTUBE
# Load data
G <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data/Parti_Google.rds")

# Rename Konservative
G$Parti[G$Parti == "Det Konservative Folkeparti"] <- "Konservative"

# Plot money (mean) per day
Money_day_google <- G %>%
  filter(date_range_start >= "2022-08-01") %>%
  mutate(spend = rowMeans(G[ , c("spend_range_min_dkk", "spend_range_max_dkk")], na.rm=TRUE)) %>%
  group_by(Parti, date_range_start) %>%
  summarise(spend = sum(spend),
            spend_lower = sum(spend_range_min_dkk),
            spend_upper = sum(spend_range_max_dkk)) %>%
  ungroup() %>%
  complete(date_range_start, Parti) %>%
  mutate(spend = replace_na(spend, 0),
         spend_lower = replace_na(spend_lower, 0),
         spend_upper = replace_na(spend_upper, 0)) %>%
  group_by(Parti) %>%
  mutate(value = cumsum(spend),
         spend_lower = cumsum(spend_lower),
         spend_upper = cumsum(spend_upper)) %>%
  ggplot(aes(x = as.Date(date_range_start), y = value/1000, color = Parti, group = Parti,
             text = paste(
               "<b>Parti: ", Parti, "\n",
               "Dato: ", date_range_start, "\n",
               "Est. akkumuleret forbrug: ", format(value, big.mark = ".", decimal.mark = ","), "kr. </b>", "\n",
               "Min. akkumuleret forbrug: ", format(spend_lower, big.mark = ".", decimal.mark = ","), "kr.", "\n",
               "Max. akkumuleret forbrug: ", format(spend_upper, big.mark = ".", decimal.mark = ","), "kr."
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = party.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = "1.000 kr.",
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret annonceforbrug pr. dag siden 1. august",
       subtitle = "",
       caption = "")

ad_day_google <- G %>%
  filter(date_range_start >= "2022-08-01") %>%
  group_by(Parti, date_range_start) %>%
  summarise(nads = n()) %>%
  ungroup() %>%
  complete(date_range_start, Parti) %>%
  mutate(nads = replace_na(nads, 0)) %>%
  group_by(Parti) %>%
  mutate(value = cumsum(nads)) %>%
  ggplot(aes(x = as.Date(date_range_start), y = value, color = Parti, group = Parti,
             text = paste(
               "Parti: ", Parti, "\n",
               "Dato: ", date_range_start, "\n",
               "Antal annoncer: ", format(value, big.mark = ".", decimal.mark = ",")
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_y_continuous(labels = function(x) format(x, big.mark = ".", decimal.mark = ",")) +
  scale_color_manual(values = party.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Antal annoncer siden 1. august",
       subtitle = "",
       caption = "")

Money_day_google_plotly <- ggplotly(Money_day_google, tooltip = "text") %>% 
  layout(legend = list(orientation = 'h'),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
ad_day_google_plotly <- ggplotly(ad_day_google, tooltip = "text") %>% 
  layout(legend = list(orientation = 'h'))



#------------
# READ SNAPCHAT DATA
S <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/Snapchat.rds")

# Fix so all are in same currency
S <- S %>%
  mutate(spend_DKK = ifelse(currency_code == "EUR", spend*7.45, 
                            ifelse(currency_code == "USD", spend*7.6, spend)))

# Rename Venstre
S$paying_advertiser_name[S$paying_advertiser_name == "Venstre, Danmarks Liberale Parti"] <- "Venstre"

# Plot
Money_snap <- S %>%
  filter(start_date >= "2022-08-01",
         paying_advertiser_name != "Fagbevægelsens Hovedorganisation",
         paying_advertiser_name != "PlanBørnefonden",
         paying_advertiser_name != "Jens Meinke Meilvang",
         paying_advertiser_name != "Steffen Helledie") %>%
  group_by(paying_advertiser_name) %>%
  summarise(value = sum(spend_DKK)) %>%
  ungroup() %>%
  ggplot(aes(x = value/1000, y = reorder(paying_advertiser_name, value), fill = paying_advertiser_name,
             text = paste(
               "Annoncør: ", paying_advertiser_name, "\n",
               "Akkumuleret forbrug: ", format(value, big.mark = ".", decimal.mark = ","), "kr."
             ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = party.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica"),
        legend.position = "none") +
  labs(y = NULL,
       x = "1.000 kr.",
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret annonceforbrug siden 1. august",
       subtitle = "",
       caption = "")

ad_total_snapchat_plotly <- ggplotly(Money_snap, tooltip = "text")






#-------
# TOTAL MONEY META
Money_total <- A %>%
  filter(ad_delivery_start_time >= "2022-09-01") %>%
  group_by(Parti) %>%
  summarise(spend_estimate = sum(spend),
            spend_lower = sum(spend_lower),
            spend_upper = sum(spend_upper))
