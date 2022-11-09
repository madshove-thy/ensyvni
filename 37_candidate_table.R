#---------------
# ALL CANDIDATES TABLE

library(tidyverse)
library(reactable)
library(sparkline)
library(htmltools)
library(openxlsx)


# # Load ads data
# D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/MF_onerow.rds")

# Load ads data (new)
D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Kandidater_ads_onerow.rds") %>%
  filter(FB_Navn != "Socialdemokraterne", FB_Navn != "Venstre", FB_Navn != "Socialistisk Folkeparti")
# Get party label on
D <- D %>%
  filter(entities.short_name != "Str", entities.short_name != "Oth") %>%
  mutate(Parti = recode(entities.short_name, "Alt" = "Alternativet", "Dem" = "Danmarksdemokraterne",
                        "Enh" = "Enhedslisten", "Kon" = "Konservative",
                        "Kri" = "Kristendemokraterne", "Lib" = "Liberal Alliance",
                        "Mod" = "Moderaterne", "Rad" = "Radikale Venstre",
                        "SF" = "SF", "Soc" = "Socialdemokratiet",
                        "Ven" = "Venstre", "Nye" = "Nye Borgerlige", "Fol" = "Dansk Folkeparti"))


#-- DEAL WITH NAs --#
# Since most cases NA means not in the data that is equal having e.g. 0 followers
# Therefore I replace NAs with 0
# There's a NA in Usikkerhed column, which I replace with 0
# Spend
D$spend <- D$spend %>% replace_na(0)
# Gender
D$female <- D$female %>% replace_na(0)
# Nads
D$nads <- D$nads %>% replace_na(0)
# Spend round
D$spend <- round(D$spend, 0)


# Select only columns needed in a dataframe to the table
D1 <- D %>%
  select(FB_Navn, Parti,
         spend, nads, 
         agevalue, female, regionvalue)



#### helper function for adding the tooltip
spk_tool <- function(labels) {
  htmlwidgets::JS(
    sprintf(
      "function(sparkline, options, field){
  return %s[field[0].offset];
}",
      jsonlite::toJSON(labels)
    )
  )
}

# Custom cell render function
# Render a bar chart with a label on the left
bar_chart <- function(label, width = "100%", height = "1rem", fill = "#005392", background = NULL) {
  bar <- div(style = list(background = fill, width = width, height = height))
  chart <- div(style = list(flexGrow = 1, marginLeft = "0.5rem", background = background), bar)
  div(style = list(display = "flex", alignItems = "center"), label, chart)
}

# Reactable ADS
Candidates <- reactable(D1, 
                    # Allow searching
                    searchable = TRUE,
                    # Sort after most money spend
                    defaultSorted = list(spend = "desc"),
                    # Group the columns
                    columnGroups = list(
                      colGroup("Info om kandidat", columns = c("FB_Navn", "Parti")),
                      colGroup("Info om annoncer", columns = c("spend", "nads")),
                      colGroup("Målretning", columns = c("agevalue", "female", "regionvalue"))
                    ),
                    # Theming
                    striped = TRUE,
                    style = list(fontFamily = 'Menlo', fontSize = '12px'),
                    # Controlling the specific columns
                    columns = list(
                      FB_Navn = colDef(
                        name = "Navn", width = 200),
                      Parti = colDef(
                        name = "Parti", width = 200
                      ),
                      agevalue = colDef(cell = function(values) {
                        sparkline(values, type = "bar", chartRangeMin = 0, tooltipFormatter=spk_tool(c("13-17", "18-24","25-34",
                                                                                                       "35-44", "45-54",
                                                                                                       "55-64", "65+")))
                      }, name = "Annoncevisninger (alder)", width = 140),
                      spend = colDef(
                        name = "Annonceforbrug (kr.)", align = "left", cell = function(value) {
                          width <- paste0(value / max(D$spend, na.rm = TRUE) * 100, "%")
                          value <- format(value, big.mark = ".")
                          bar_chart(value, width = width)
                        }, width = 120),
                      female = colDef(
                        name = "Annoncevisninger (% kvinder)", align = "left", cell = function(value) {
                          width <- paste0(value / max(D$female, na.rm = TRUE) * 100, "%")
                          value <- format(round(value*100,1), big.mark = ",")
                          bar_chart(value, width = width)
                        }, width = 140),
                      regionvalue = colDef(cell = function(values) {
                        sparkline(values, type = "bar", chartRangeMin = 0, tooltipFormatter=spk_tool(c("Region Hovedstaden", "Region Midtjylland", "Region Nordjylland",  "Region Syddanmark", "Region Sjaelland")))
                      }, name = "Annoncevisninger (region)", width = 140),
                      nads = colDef(
                        name = "Antal annoncer"
                      )
                    )
)

