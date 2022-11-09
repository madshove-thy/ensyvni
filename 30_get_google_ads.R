#-------------
# GET GOOGLE DATA
library(tidyverse)
library(openxlsx)

# Set link to data
ggl_link <- "https://storage.googleapis.com/political-csv/google-political-ads-transparency-bundle.zip"

# Define destination for data
ggl_file <- "/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google_raw.zip"

# Download file
download.file(ggl_link, ggl_file, mode="wb")

# Unzip file
unzip(ggl_file, exdir = "/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data")

# Remove .zip file
unlink(ggl_file)

#-------------
# PREPROCESS DATA
# Load data
G <- read_csv("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-creative-stats.csv") %>%
  filter(str_detect(Regions, "DK")) %>%
  janitor::clean_names() %>%
  filter(date_range_start >= as.Date("2022-08-01"))

# Load party information
P <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/Partier.xlsx")

# Save data from yesterdays ads
# We'll just keep all ads
G1 <- G %>%
  filter(date_range_start == Sys.Date()-1)

saveRDS(G1, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data/GOOGLE_ADS_", 
                  format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Save data from minus three days ads
# We'll just keep all ads
G3 <- G %>%
  filter(date_range_start == Sys.Date()-3)

saveRDS(G3, paste("/Users/Madshove/OneDrive/Mads/SDU/microtargeting/data_google_3days/GOOGLE_ADS_", 
                  format(Sys.time(), "%Y-%m-%d"), ".rds", sep = ""))

# Merge (and by that also remove non-parties)
# And save for ensyvni data
G2 <- merge(x = G, y = P, by = "advertiser_name")

saveRDS(G2, "/Users/Madshove/OneDrive/Mads/golem/ensyvni/data/Parti_Google.rds")

# Remove from workspace
rm(G, G1, G2, G3, P, ggl_file, ggl_link)

#-----------
# REMOVE STORAGE CONSUMING FILES
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-advertiser-geo-spend.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/advertiser_id_mapping.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/creative_id_mapping.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-advertiser-weekly-spend.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-creative-stats.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-updated.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-top-keywords-history.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-advertiser-stats.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-advertiser-declared-stats.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-geo-spend.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/google-political-ads-campaign-targeting.csv")
unlink("/Users/Madshove/OneDrive/Mads/golem/ensyvni/google_data/README.txt")

