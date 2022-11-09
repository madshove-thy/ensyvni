#------------------
# SNAPCHAT

# Load libraries
library(tidyverse)
library(openxlsx)

# Set link to data
ggl_link <- "https://storage.googleapis.com/ad-manager-political-ads-dump/political/2022/PoliticalAds.zip"

# Define destination for data
ggl_file <- "/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/snapchat_raw.zip"

# Download file
download.file(ggl_link, ggl_file, mode="wb")

# Unzip file
unzip(ggl_file, exdir = "/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data")

# Remove .zip file
unlink(ggl_file)

#-------------
# PREPROCESS DATA
# Load data
S <- read_csv("/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/PoliticalAds.csv") %>%
  filter(str_detect(CountryCode, "denmark")) %>%
  janitor::clean_names()

# Load party information
P <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/Partier.xlsx")

# Save data from yesterdays ads
# We'll just keep all ads
S1 <- S %>%
  filter(start_date == Sys.Date()-1)

saveRDS(S1, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_snapchat/SNAPCHAT_ADS_", 
                  format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Save data from minus three days ads
# We'll just keep all ads
S3 <- S %>%
  filter(start_date == Sys.Date()-3)

saveRDS(S3, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_snapchat_3days/SNAPCHAT_ADS_", 
                  format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Save ALL ads 2022
saveRDS(S, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_snapchat_TOTAL/Snapchat_TOTAL.rds",
                 format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Merge (and by that also remove non-parties)
# And save for ensyvni data
saveRDS(S, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/Snapchat.rds")

# Remove from workspace
rm(P, S, S1, S3, ggl_file, ggl_link)

#-----------
# REMOVE STORAGE CONSUMING FILES
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/readme.txt")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/snapchat_data/PoliticalAds.csv")

