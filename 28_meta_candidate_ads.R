# Everything below is also found at https://facebookresearch.github.io/Radlibrary/articles/Radlibrary.html

### INSTALL AND LOAD PACKAGES ###
# Install packages
# devtools::install_github("facebookresearch/Radlibrary")

# Load packages
library("Radlibrary")
library("tidyverse")
library("openxlsx")

### Introduction steps to get access to the Facebook Ad Library ###
# First step is that you need to confirm your identity and location with Facebook.
# I did this when we ran the election campaign in 2019 for Folketinget,
# so I don't perfectly remember how the process is. But what you do is that you go to the ID section
# of your Facebook settings, where you upload a picture of e.g. your drivers license or something else that can confirm
# your identity.
# Second step is that you afterwards
# create a Facebook Developer account which you can do at https://developers.facebook.com/

### Store my access token ###
# This might have to be changed from time to time, since they expire at some point
# can be retrevied at https://developers.facebook.com/tools/explorer/
# be careful not to send this token to anyone since they can access your personal Facebook account with this token

# Run this when token needs to be refreshed
# adlib_setup()

# If you want the token, then run token_get()

token <- token_get()
token <- token$token

### Load dataset containing ID
ID <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/WHOTARGETSME.xlsx") %>%
  mutate(page_id = as.character(page_id)) %>%
  filter(entities.is_political == "TRUE")

# Load MF list
ensyvni <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/ensyvni.xlsx")

# Remove those without page ids
ID <- ID %>%
  filter(page_id != 0)

# Keep Inger Stoejberg and Franciska Rosenkilde
keep <- ID %>%
  filter(FB_Navn == "Inger Støjberg" | FB_Navn == "Franciska Rosenkilde, Alternativet")

# Remove those who are not already included in the MF list
ID <- ID %>%
  filter(!FB_Navn %in% ensyvni$FB_Navn)

# give them their row back!
ID <- rbind(ID, keep)

#-------------------------------------------------------------------------
# DOWNLOAD AD DATA
#-------------------------------------------------------------------------

### Search queries
# Create queries for each political page as Facebook does
# not allow more than 10 downloads at a time

# Create empty list to store queries
A <- list()
B <- list()
C <- list()

# Create query for each ID in a loop
for(i in ID$page_id) {
  
  # Printing info to console
  cat("Query for ID number", i, "...\n")
  
  # Create query for given ID (AD DATA INFORMATION)
  query_ad <- adlib_build_query(ad_reached_countries = 'DK',
                                ad_active_status = 'ALL',
                                # search_terms = "NULL",
                                ad_delivery_date_min = "2022-08-01",
                                search_page_ids = i,
                                fields = c(
                                  #"ad_data",
                                  "id",
                                  "ad_creation_time",
                                  "ad_creative_bodies",
                                  "ad_delivery_start_time",
                                  "ad_delivery_stop_time",
                                  "ad_snapshot_url",
                                  "currency",
                                  "page_id",
                                  "page_name",
                                  "publisher_platforms",
                                  "impressions",
                                  "spend"
                                ))
  
  # Create query for given ID (DEMOGRAPHIC INFORMATION)
  query_demo <- adlib_build_query(ad_reached_countries = 'DK',
                                  ad_active_status = 'ALL',
                                  # search_terms = "NULL",
                                  ad_delivery_date_min = "2022-08-01",
                                  search_page_ids = i,
                                  fields = c("demographic_distribution"))
  
  # Create query for given ID (DEMOGRAPHIC INFORMATION)
  query_region <- adlib_build_query(ad_reached_countries = 'DK',
                                    ad_active_status = 'ALL',
                                    # search_terms = "NULL",
                                    ad_delivery_date_min = "2022-08-01",
                                    search_page_ids = i,
                                    fields = c("delivery_by_region"))
  
  # Store in the list
  A[[i]] <- query_ad
  B[[i]] <- query_demo
  C[[i]] <- query_region
  
}


### Download from API
# Create empty list to store data from API
D <- list()
E <- list()
F <- list()

# Download data for each query in a loop
for(i in names(A)) {
  
  tryCatch({
    # Printing info to console
    cat("Downloading data for user", i, "...\n")
    
    # Submit queries
    response_ad <- adlib_get(params = A[[i]], token = token)
    response_demo <- adlib_get(params = B[[i]], token = token)
    response_region <- adlib_get(params = C[[i]], token = token)
    
    ## Convert to dataframe
    # Ad data
    results_tibble_ad <- as_tibble(response_ad, type = "ad",
                                   censor_access_token = NULL)
    
    # Gender and age data
    results_tibble_demo <- as_tibble(response_demo, type = "demographic",
                                     censor_access_token = NULL)
    
    # Regional data
    results_tibble_region <- as_tibble(response_region, type = "region",
                                       censor_access_token = NULL)
    
    # Store in different lists
    D[[i]] <- results_tibble_ad
    E[[i]] <- results_tibble_demo
    F[[i]] <- results_tibble_region
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
}

# Rbind all elements from the list containing
# downloaded ads
# A few politicians only have 13 columns
# That is because there is not registred a ad_delivery_stop_date
# I use the rbind.fill from plyr so I still keep the columns for
# most without throwing out the politicians with only 13 columns
DF_ad <- do.call(plyr::rbind.fill, D)
DF_demo <- do.call(plyr::rbind.fill, E)
DF_demo <- unnest(DF_demo, demographic_distribution) # Because demo info are stored in list
DF_region <- do.call(plyr::rbind.fill, F)
DF_region <- unnest(DF_region, delivery_by_region) # Because regional info are stored in list

# # Old way of dealing with missing columns
# D <- purrr::keep(D, ~ ncol(.x) > 13)
# DF_ad <- do.call(rbind, D)

# # Left join all dataframes with adlib id as unique identifier
# DF <- plyr::join_all(list(DF1, DF2, DF3), by = c("adlib_id"), type = "left")

#------------- PARTY LABELS AND MORE
# Get party labels and other important information on
DF_ad <- merge(x = DF_ad, y = ID, by = "page_id")

DF_demo <- merge(x = DF_demo, y = DF_ad, by = "id")

DF_region <- merge(x = DF_region, y = DF_ad, by = "id")

#---------- SAVE NEW ADS FROM YESTERDAY
# Take only ads from yesterday and save it as a specific file
SDU_ad <- DF_ad %>% filter(ad_delivery_start_time == Sys.Date()-1)
SDU_demo <- DF_demo %>% filter(ad_delivery_start_time == Sys.Date()-1)
SDU_region <- DF_region %>% filter(ad_delivery_start_time == Sys.Date()-1)
SDU_ad3 <- DF_ad %>% filter(ad_delivery_start_time == Sys.Date()-3)
SDU_demo3 <- DF_demo %>% filter(ad_delivery_start_time == Sys.Date()-3)
SDU_region3 <- DF_region %>% filter(ad_delivery_start_time == Sys.Date()-3)

# Save as rds marked with yesterday's date
saveRDS(SDU_ad, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater/Ads_", format(Sys.Date()-1, "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(SDU_demo, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater/Demographics_", format(Sys.Date()-1, "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(SDU_region, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater/Region_", format(Sys.Date()-1, "%Y-%m-%d"), ".rds", sep = ""))

saveRDS(DF_ad, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_TOTAL/Ads_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(DF_demo, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_TOTAL/Demographics_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(DF_region, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_TOTAL/Region_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))

saveRDS(SDU_ad3, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_3days/Ads_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(SDU_demo3, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_3days/Demographics_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))
saveRDS(SDU_region3, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_fb_ads_kandidater_3days/Region_", format(Sys.Date(), "%Y-%m-%d"), ".rds", sep = ""))

# And save these
saveRDS(DF_ad, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Kandidater_ads.rds")
saveRDS(DF_demo, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Kandidater_demo.rds")
saveRDS(DF_region, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Kandidater_region.rds")

#---------- WRANGLE AND SAVE ONEROW PER CANDIDATE
# So, we'll proceed to making a dataframe with one row per ad
# First the regional data
regioner <- c("Capital Region of Denmark", "Central Denmark Region",
              "North Denmark Region", "Region of Southern Denmark",
              "Zealand Region")

R <- DF_region %>%
  filter(region %in% regioner) %>%
  group_by(page_id) %>%
  mutate(perc = percentage/sum(percentage)) %>%
  ungroup() %>%
  group_by(page_id, region) %>%
  summarise(regionvalue = sum(perc))

R <- pivot_wider(R, names_from = region, values_from = regionvalue)

R[is.na(R)] <- 0

R <- pivot_longer(R, cols = 2:6, names_to = "region", values_to = "regionvalue")

R <- R %>%
  group_by(page_id) %>%
  summarise(region = list(region),
            regionvalue = list(regionvalue))

# Then demographic
# then wrangle to one row pr ad (GENDER)
Gender <- DF_demo %>%
  filter(gender != "unknown") %>%
  group_by(page_id) %>%
  mutate(perc = percentage/sum(percentage)) %>%
  group_by(gender, page_id) %>%
  summarise(gendervalue = sum(perc))

Gender <- pivot_wider(Gender, names_from = "gender", values_from = "gendervalue")

# then wrangle to one row pr ad (AGE)
Age <- DF_demo %>%
  filter(age != "Unknown") %>%
  group_by(page_id) %>%
  mutate(perc = percentage/sum(percentage)) %>%
  group_by(age, page_id) %>%
  summarise(agevalue = sum(perc))

Age <- pivot_wider(Age, names_from = age, values_from = agevalue)

Age[is.na(Age)] <- 0

Age <- pivot_longer(Age, cols = 2:8, names_to = "age", values_to = "agevalue")

Age <- Age %>%
  group_by(page_id) %>%
  summarise(age = list(age),
            agevalue = list(agevalue))

Ads <- DF_ad %>%
  mutate(spend = rowMeans(DF_ad[ , c("spend_lower", "spend_upper")], na.rm=TRUE),
         impressions = rowMeans(DF_ad[ , c("impressions_lower", "impressions_upper")], na.rm=TRUE)) %>%
  # drop_na(ad_creative_bodies) %>%
  group_by(page_id) %>%
  summarise(spend = sum(spend),
            impressions = sum(impressions),
            nads = n())

## MERGE
P1 <- merge(x = Ads, y = ID, by = "page_id")
P2 <- merge(x = P1, y = R, by = "page_id")
P3 <- merge(x = P2, y = Age, by = "page_id")
P4 <- merge(x = P3, y = Gender, by = "page_id")

# Save as RDS
saveRDS(P4, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Kandidater_ads_onerow.rds")
