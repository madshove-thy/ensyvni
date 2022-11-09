#-----------------------
# DETAILED TARGETING

# Load libraries
library(tidyverse)
library(reactable)

# Load data
D <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Parti_detailed_targeting.rds")

# Make values to percent
D$total_spend_pct <- as.numeric(D$total_spend_pct)*100
D$total_spend_pct <- round(D$total_spend_pct, 1)

#--------------
# Table with interest targeting
D1 <- D %>%
  filter(type == "detailed") %>%
  select(Parti, value, is_exclusion, num_ads, total_spend_pct)

# Change values of ekskluderet / inkluderet
D1 <- D1 %>%
  mutate(is_exclusion = recode(as.character(is_exclusion), "FALSE" = "Inkluderet", "TRUE" = "Ekskluderet"))

# Make table
Interest_table <- reactable(D1, 
                            # Allow searching
                            searchable = TRUE,
                            # Sort after party
                            defaultSorted = list(Parti = "asc"),
                            # Group the columns
                            columnGroups = list(
                              colGroup("Parti", columns = c("Parti")),
                              colGroup("Målretning (interesser)", columns = c("value", "is_exclusion")),
                              colGroup("Omfang", columns = c("num_ads", "total_spend_pct"))
                            ),
                            # Theming
                            striped = TRUE,
                            style = list(fontFamily = 'Menlo', fontSize = '12px'),
                            # Controlling the specific columns
                            columns = list(
                              Parti = colDef(
                                name = "Parti"),
                              value = colDef(
                                name = "Interesse"),
                              is_exclusion = colDef(
                                name = "Inkluderet / ekskluderet"
                              ),
                              num_ads = colDef(
                                name = "Antal annoncer"
                              ),
                              total_spend_pct = colDef(
                                name = "Procent af samlet forbrug"
                              )
                              ))

#--------------
# Table with location targeting
D2 <- D %>%
  filter(type == "location") %>%
  select(Parti, value, is_exclusion, num_ads, total_spend_pct)

# Change values of ekskluderet / inkluderet
D2 <- D2 %>%
  mutate(is_exclusion = recode(as.character(is_exclusion), "FALSE" = "Inkluderet", "TRUE" = "Ekskluderet"))

# Make table
Location_table <- reactable(D2, 
                            # Allow searching
                            searchable = TRUE,
                            # Sort after party
                            defaultSorted = list(Parti = "asc"),
                            # Group the columns
                            columnGroups = list(
                              colGroup("Parti", columns = c("Parti")),
                              colGroup("Målretning (lokation)", columns = c("value", "is_exclusion")),
                              colGroup("Omfang", columns = c("num_ads", "total_spend_pct"))
                            ),
                            # Theming
                            striped = TRUE,
                            style = list(fontFamily = 'Menlo', fontSize = '12px'),
                            # Controlling the specific columns
                            columns = list(
                              Parti = colDef(
                                name = "Parti"),
                              value = colDef(
                                name = "Lokation"),
                              is_exclusion = colDef(
                                name = "Inkluderet / ekskluderet"
                              ),
                              num_ads = colDef(
                                name = "Antal annoncer"
                              ),
                              total_spend_pct = colDef(
                                name = "Procent af samlet forbrug"
                              )
                            ))

#--------------
# AGE TARGETING DISTRIBUTION
D3 <- D %>%
  filter(type == "age") %>%
  filter(value >= "18") %>%
  select(value, num_ads, total_spend_pct, Parti)

age_targeting <- D3 %>%
    ggplot(aes(x = value, y = total_spend_pct, group = 1)) +
    geom_smooth(se = FALSE, color = "black") +
    scale_x_discrete(breaks = c("20", "30", "40", "50", "60")) +
    theme_bw() +
    theme(axis.text = element_text(size = 8, color = "black"),
          axis.text.x = element_text(size = 8, color = "black"),
          axis.title = element_text(size = 10, color = "black"),
          legend.position="none",
          strip.text = element_text(size = 10, face = "bold"),
          title = element_text(size = 12),
          panel.grid.major = element_blank(),
          text = element_text(family = "Helvetica")) +
    labs(y = NULL,
         x = "Alder",
         shape = NULL, 
         col = NULL,
         title = "Detaljeret målretning (alder) pr. parti",
         subtitle = "Procent af annoncer, hvor aldersgruppen må se annoncer",
         caption = "Baseret på detaljeret målretning seneste 30 dage") +
  ylim(0, 100) +
    facet_wrap(~ Parti, ncol = 4)
