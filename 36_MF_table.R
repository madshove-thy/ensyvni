#---------------------------------
# REACTABLE TABEL FOR MF'ere

library(tidyverse)
library(reactable)
library(sparkline)
library(htmltools)
library(openxlsx)


# # Load ads data
# D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/MF_onerow.rds")

# Load ads data (new)
D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/MF_ads_onerow.rds")

# Load FB post data
DF <- read_csv2("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_post/MF_1month.csv") %>%
  mutate(FB_Navn = as.character(`Page Name`),
         Overperforming.Score = `Overperforming Score`) %>%
  filter(`Post Created Date` >= Sys.Date()-8)
names(DF) <- make.names(names(DF), unique = TRUE) # remove spaces in the column names

DF <- DF %>%
  group_by(FB_Navn) %>%
  summarise(follower_count = max(Followers.at.Posting),
            interactions = mean(Total.Interactions),
            likes = mean(Likes),
            comments = mean(Comments),
            shares = mean(Shares),
            love = mean(Love),
            wow = mean(Wow),
            haha = mean(Haha),
            sad = mean(Sad),
            angry = mean(Angry),
            care = mean(Care),
            performance = median(Overperforming.Score),
            npost = n())

Names <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/ensyvni.xlsx")
DF <- merge(x = DF, y = Names[ , c("FB_Navn", "Parti")], by = "FB_Navn")

#-- DEAL WITH NAs --#
# Since most cases NA means not in the data that is equal having e.g. 0 followers
# Therefore I replace NAs with 0
# There's a NA in Usikkerhed column, which I replace with 0
D$Usikkerhed_2019 <- D$Usikkerhed_2019 %>%
  replace_na(0)
D$Usikkerhed_2019 <- round(D$Usikkerhed_2019, 2)

# Spend
D$spend <- D$spend %>% replace_na(0)
# Gender
D$female <- D$female %>% replace_na(0)
# Nads
D$nads <- D$nads %>% replace_na(0)


# Followers
DF$follower_count <- as.numeric(DF$follower_count) %>% replace_na(0)
# Interaktions
DF$interactions <- DF$interactions %>% replace_na(0)
# Overperforming
DF$performance <- DF$performance %>% replace_na(0)
# Kommentarer
DF$comments <- DF$comments %>% replace_na(0)
# Likes
DF$likes <- DF$likes %>% replace_na(0)
# Love
DF$love <- DF$love %>% replace_na(0)
# Angry
DF$angry <- DF$angry %>% replace_na(0)

# Round
DF$likes <- round(DF$likes, 0)
DF$love <- round(DF$love, 0)
DF$angry <- round(DF$angry, 0)
DF$haha <- round(DF$haha, 0)
DF$wow <- round(DF$wow, 0)
DF$sad <- round(DF$sad, 0)
DF$care <- round(DF$care, 0)
DF$interactions <- round(DF$interactions, 0)
DF$comments <- round(DF$comments, 0)
DF$shares <- round(DF$shares, 0)
D$spend <- round(D$spend, 0)


# Select only columns needed in a dataframe to the table
D1 <- D %>%
  select(Navn, Parti, storkreds,
         spend, nads, 
         agevalue, female, regionvalue)

D2 <- DF %>%
  select(FB_Navn, Parti,
         follower_count, npost, performance,
         interactions, comments, shares, 
         likes, love, angry, haha, wow, sad, care)

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
MF_ads <- reactable(D1, 
              # Allow searching
              searchable = TRUE,
              # Sort after most money spend
              defaultSorted = list(spend = "desc"),
              # Group the columns
              columnGroups = list(
                colGroup("Info om kandidat", columns = c("Navn", "Parti", "storkreds")),
                colGroup("Info om annoncer", columns = c("spend", "nads")),
                colGroup("Målretning", columns = c("agevalue", "female", "regionvalue"))
              ),
              # Theming
              striped = TRUE,
              style = list(fontFamily = 'Menlo', fontSize = '12px'),
              # Controlling the specific columns
              columns = list(
                Navn = colDef(
                  name = "Navn", width = 140),
                Parti = colDef(
                  name = "Parti", width = 140
                ),
                storkreds = colDef(
                  name = "Storkreds", width = 140
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
                  sparkline(values, type = "bar", chartRangeMin = 0, tooltipFormatter=spk_tool(c("Region Syddanmark", "Region Midtjylland", "Region Nordjylland",  "Region Hovedstaden", "Region Sjaelland")))
                }, name = "Annoncevisninger (region)", width = 140),
                nads = colDef(
                  name = "Antal annoncer"
                )
              )
    )

# REACTABLE FACEBOOK POSTS
MF_post <- reactable(D2,
          # Allow searching
          searchable = TRUE,
          # Sort after most interactions
          defaultSorted = list(interactions = "desc"),
          # Group the columns
          columnGroups = list(
            colGroup("Info om kandidat", columns = c("FB_Navn", "Parti")),
            colGroup("Overordnet Facebook performance", columns = c("follower_count", "npost", "performance", "interactions", "comments", "shares")),
            colGroup("Reaktioner", columns = c("likes", "love", "angry", "haha", "wow", "sad", "care"))
          ),
          # Theming
          striped = TRUE,
          style = list(fontFamily = 'Menlo', fontSize = '12px'),
          # Controlling the specific columns
          columns = list(
            FB_Navn = colDef(
              name = "Navn", width = 140),
            Parti = colDef(
              name = "Parti", width = 140
            ),
            follower_count = colDef(
              name = "Følgere", align = "left", cell = function(value) {
                width <- paste0(value / max(D2$follower_count, na.rm = TRUE) * 100, "%")
                value <- format(value, big.mark = ".")
                bar_chart(value, width = width)
              }
            ),
            npost = colDef(
              name = "Antal opslag", width = 80
            ),
            performance = colDef(
              name = "Performance (median)", 
              # align = "left", cell = function(value) {
              #   width <- paste0(value / max(D2$performance, na.rm = TRUE) * 100, "%")
              #   value <- format(round(value, 2), big.mark = ".")
              #   bar_chart(value, width = width)
              # }, 
              width = 150
            ),
            interactions = colDef(
              name = "Interaktion (gns.)", width = 100
            ),
            comments = colDef(
              name = "Kommentar (gns.)", width = 100
            ),
            shares = colDef(
              name = "Delinger (gns.)", width = 80 
            ),
            likes = colDef(
              name = "Like (gns.)", width = 80
            ),
            love = colDef(
              name = "Love (gns.)", width = 80
            ),
            angry = colDef(
              name = "Angry (gns.)", width = 80
            ),
            haha = colDef(
              name = "Haha (gns.)", width = 80
            ),
            wow = colDef(
              name = "Wow (gns.)", width = 80
            ),
            sad = colDef(
              name = "Sad (gns.)", width = 80
            ),
            care = colDef(
              name = "Care (gns.)", width = 80
            )
          ))
