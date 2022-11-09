#------------------
# ISSUES

# Load libraries
library(tidyverse)
library(plotly)
library(Sentida)
library(openxlsx)
library(tidytext)

# Load data
D <- read_csv2("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_post/Parti_post.csv") %>%
  mutate(FB_Navn = as.character(`Page Name`),
         page_id = as.character(`Facebook Id`))
names(D) <- make.names(names(D), unique = TRUE) # remove spaces in the column names

# Merge party names and such on
Names <- read.xlsx("/Users/Madshove/OneDrive/Mads/golem/lister/Partier.xlsx")

D <- merge(x = D, y = Names[ , c("FB_Navn", "Parti", "Partibogstav", "Partikort")], by = "FB_Navn")

# Unnest tokens
AI <- D %>%
  unnest(Message)

# Set party color
party.col <- c("Socialdemokratiet" = "#BF0418", "Radikale Venstre" = "#E82583", "Konservative" = "#00571F", "Nye Borgerlige" = "#004450", "SF" = "#F04D46",
               "Liberal Alliance" = "#12213f", "Dansk Folkeparti" = "#E7D01E", "Venstre" = "#005392", "Enhedslisten" = "#C21B3E", "Frie Grønne" = "#DAA520",
               "Alternativet" = "#00ff00", "Danmarksdemokraterne" = "#33F3FF",
               "Kristendemokraterne" = "#334CFF", 
               # "Veganerpartiet" = "#52BE80",
               "Moderaterne" = "#6C3483")

# Set issue color
issue.col <- c("Ældre" = "#7570B3", "Børn" = "#D95F02", "Indvandring" = "#E6AB02",
               "Klima" = "#66A61E", "Økonomi" = "#A6761D", "Sundhed" = "#E7298A")

# Define dictionaries
oekonomi <- c("*økonomi*", "samfundsøkonomi*", "velstand*", "vækst*", "erhverv", 
              "erhvervsliv*", "beskæftigelse*", "arbejdsudbud*", "arbejdskraft*", 
              "arbejdsplads*", "konkurrenceevne*", "*virksomhed*", "afskrivning*", 
              "aktie*", "*skat*", "bank*", "betalingsri", "bruttonationalprodukt*", 
              "bnp*", "brugerbetaling", "*budget*", "skattely*", "børs*", "offentlig*",
              "forbrug*", "finanssektor*", "eksport", "generationsski", 
              "grænsehandel*", "gæld", "import*", "*obligation*", "*investering*", 
              "iværksæt*", "kapital*", "konkurs*", "*kredit*", "*fradr", 
              "offentlig* sektor*", "offentlig* udgift*", "offentlig* ydelse*", 
              "overførselsindkomst*", "privatiser*", "*regnskab*", "likivd*", 
              "arbejdsmarked*", "rente*", "udligningsreform*", "valuta*")

search_oek <- str_replace_all(oekonomi, "\\*", "")
search_oek <- str_c(search_oek, collapse = "|")

indvandring <- c("indvandr*", "udlænding*", "flygtning*", "asyl*", "immigra*", 
                 "migra*", "migrer*", "kvoteflygtning*", "udrejsecent*", 
                 "sjælsmark*", "lindholm", "nærområde*", "paradigme", 
                 "paradigmeskifte*", "udvis*", "islam*", "international* konvention*", 
                 "24-årsregl*", "familiesammenføring*", "apartheid", "ghetto*", 
                 "hjemsend*", "send* hjem*", "humanitær*", "menneskesmugling*", 
                 "racis*", "statsborgerskab", "tvangsægteskab*", "opholdstillad*", 
                 "uland*", "integr*", "velintegr*", "uintegr*", "parallelsamfund", 
                 "lære* sprog*", "danskundervisning*")

search_ind <- str_replace_all(indvandring, "\\*", "")
search_ind <- str_c(search_ind, collapse = "|")

sundhed <- c("sygehus*", "*hospital*", "*patient*", "*læge*", "*sygepleje*", 
             "*operation*", "*sundhed*", "syg", "syge", "*sygdom*", "helbred*", 
             "behandling*", "*psykiatri*", "*medicin*", "familielæge*", "ambulance*", 
             "akut*", "tandpleje*", "psykolog*", "*støj", "*støjen", "støjbekæmpelse*", 
             "støjsikring*", "støjmur*", "støjværn*", "abort*", "adhd", "alkohol*", 
             "cigaret*", "allergi*", "autisme*", "demens*", "depression*", "bipolar*", 
             "drikkevand*", "dødshjælp*", "epilepsi*", "fysioterapi*", "genoptræning*", 
             "gravid*", "hjerneskade*", "høreapparat*", "*kræft", "levealder*", 
             "menstruation*", "prævention", "organdon*", "*rygning", "*skadestue*", 
             "skizofreni", "stofmisbrug", "sukkersyg*", "tilsætningsstof*", 
             "*indlæggelse*", "udviklingshæmme*", "*vaccin*", "venteliste*", 
             "ventetid*", "kemikalie*")

search_sund <- str_replace_all(sundhed, "\\*", "")
search_sund <- str_c(search_sund, collapse = "|")

aeldre <- c("ældre*", "plejehjem*", "hjemmehjælp*", "hospice*", "alderdom*")

search_ael <- str_replace_all(aeldre, "\\*", "")
search_ael <- str_c(search_ael, collapse = "|")

# eu <- c("eu*", "*forbehold*", "brexit*", "schengen*", "ministerråd*", "*traktat*",
#         "unionen*", "bruxelles", "strasbourg", "forbehold\\b", "forsvarsfo", "folkeafstemning",
#         "1. juni", "forbeholdet", "forbeholdene", "fælles forsvar af vores sikkerhed")
# 
# search_eu <- str_replace_all(eu, "\\*", "")
# search_eu <- str_c(search_eu, collapse = "|")

boern <- c("børn*", "vuggestue*", "*normering*", "daginstitution*", "barn*", 
           "*barndom*", "*pædagog*", "opvækst", "dagpleje*", "sfo*", "barsel*", 
           "tvangs- fjerne*", "adopt*")

search_boern <- str_replace_all(boern, "\\*", "")
search_boern <- str_c(search_boern, collapse = "|")

# pension <- c("*pension*", "tilbagetrækning*", "nedslid*", "slid* ned*", "senior*", "værdig")
# 
# search_pension <- str_replace_all(pension, "\\*", "")
# search_pension <- str_c(search_pension, collapse = "|")

# Foreløbig ordbog https://viegandmaagoe.dk/viden/klimaordbog/
klima <- c("biodiversitet", "biomasse", "biosfære", "cirkulær økonomi", "co2", "drivhuseffekt",
           "emission", "fossile brænds", "global opvarmning", "grøn omstilling", "klimaforandringer",
           "vedvarende energi", "økosystem", "klimavalg", "grøn", "klima")

search_klima <- str_replace_all(klima, "\\*", "")
search_klima <- str_c(search_klima, collapse = "|")

# Classify ads
AI$i_oek <- str_detect(AI$Message,
                       regex(search_oek, ignore_case = TRUE))

AI$i_ael <- str_detect(AI$Message,
                       regex(search_ael, ignore_case = TRUE))

AI$i_boern <- str_detect(AI$Message,
                         regex(search_boern, ignore_case = TRUE))

# AI$i_eu <- str_detect(AI$Message,
#                       regex(search_eu, ignore_case = TRUE))

AI$i_ind <- str_detect(AI$Message,
                       regex(search_ind, ignore_case = TRUE))

# AI$i_pension <- str_detect(AI$Message,
#                            regex(search_pension, ignore_case = TRUE))

AI$i_sund <- str_detect(AI$Message,
                        regex(search_sund, ignore_case = TRUE))

AI$i_klima <- str_detect(AI$Message,
                         regex(search_klima, ignore_case = TRUE))

# Make them numeric
AI$i_oek <- as.numeric(AI$i_oek)
AI$i_ael <- as.numeric(AI$i_ael)
AI$i_boern <- as.numeric(AI$i_boern)
# AI$i_eu <- as.numeric(AI$i_eu)
AI$i_ind <- as.numeric(AI$i_ind)
# AI$i_pension <- as.numeric(AI$i_pension)
AI$i_sund <- as.numeric(AI$i_sund)
AI$i_klima <- as.numeric(AI$i_klima)

# We want to get all topics in one column, so we pivot the frame longer
AI <- pivot_longer(AI, starts_with("i_"), names_to = "issue")

# And filter out those rows that indicate that a post does not contain this specific topic
AI <- AI %>%
  filter(value == 1)
# We will of course still have a few more observations that there are posts,
# since some posts using this method includes multiple topics

# issues with right name
AI$issue[AI$issue == "i_ael"] <- "Ældre"
AI$issue[AI$issue == "i_boern"] <- "Børn"
# AI$issue[AI$issue == "i_eu"] <- "EU"
AI$issue[AI$issue == "i_ind"] <- "Indvandring"
AI$issue[AI$issue == "i_oek"] <- "Økonomi"
# AI$issue[AI$issue == "i_pension"] <- "Pension"
AI$issue[AI$issue == "i_sund"] <- "Sundhed"
AI$issue[AI$issue == "i_klima"] <- "Klima"

# Kons
AI$Parti[AI$Parti == "Det Konservative Folkeparti"] <- "Konservative"

# Now we would like to say for each party, how much they talk about the specific topics
# and how well they are performing on different interactions metrics
AI1 <- AI %>%
  group_by(Parti, issue) %>%
  summarise(npost = n(),
            performance = median(Overperforming.Score),
            views = mean(Post.Views, na.rm = TRUE),
            care = mean(Care),
            angry = mean(Angry),
            sad = mean(Sad),
            haha = mean(Haha),
            wow = mean(Wow),
            love = mean(Love),
            shares = mean(Shares),
            comments = mean(Comments),
            likes = mean(Likes),
            interactions = mean(Total.Interactions))

# We want a column saying how many percent we're talking about
AI1 <- AI1 %>%
  group_by(Parti) %>%
  mutate(percent = npost/sum(npost))

# Plot
issues <- AI1 %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  ggplot(aes(x = percent*100, y = reorder_within(Parti, percent, issue), fill = Parti,
             text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Procent: ", round(percent*100,2), "%"
             ))) +
  geom_bar(stat = "Identity") +
  facet_wrap(~ issue, scales = "free_y") +
  scale_fill_manual(values = party.col) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5, hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 12, color = "black"),
        legend.position="bottom",
        strip.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 12),
        panel.grid.major = element_blank(),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Kommunikerede emner pr. parti i procent (siden 1/8/2022)",
       subtitle = "I annoncer siden 1. august 2022",
       caption = "Da en annonce godt kan have mere end et tema \n kan summen godt overstige 100.")

issues_plotly <- ggplotly(issues, tooltip = "text")

# Which issues gets the most interactions per party?
issues_interaction <- AI %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  group_by(Parti, issue) %>%
  filter(Total.Interactions <= quantile(Total.Interactions, 0.9) &
           Total.Interactions >= quantile(Total.Interactions, 0.1)) %>%
  summarise(value = mean(Total.Interactions),
            npost = n()) %>%
  ungroup() %>%
  ggplot(aes(x = value, y = reorder_within(Parti, value, issue), 
             fill = issue, text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Gns. interaktioner: ", format(round(value,0), big.mark = ".", decimal.mark = ",")
             ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = issue.col) +
  # scale_fill_manual(values = party.col) +
  facet_wrap(~ Parti, scales = "free", ncol = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 8, color = "black"),
    legend.position="bottom",
    strip.text = element_text(size = 10, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Interaktioner pr. emne pr. parti siden 01/08/2021",
       subtitle = "",
       caption = "")

issues_interaction_plotly <- ggplotly(issues_interaction, tooltip = "text")

# Which sentiment does the parties use on different issues?
issues_sentiment <- AI %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  group_by(Parti, issue) %>%
  summarise(sentiment = sentida(Message, output = "mean")) %>%
  ggplot(aes(x = sentiment, y = reorder_within(Parti, sentiment, issue), 
         fill = issue, text = paste(
           "Parti: ", Parti, "\n",
           "Emne: ", issue, "\n",
           "Sentiment score: ", format(round(sentiment,2), big.mark = ".", decimal.mark = ",")
         ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = issue.col) +
  # scale_fill_manual(values = party.col) +
  facet_wrap(~ Parti, scales = "free", ncol = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 8, color = "black"),
    legend.position="bottom",
    strip.text = element_text(size = 10, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Sentiment pr. emne pr. parti siden 01/08/2021",
       subtitle = "",
       caption = "")

issues_sentiment_plotly <- ggplotly(issues_sentiment, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emne</b>")))

# Issues day per day
issues_day <- AI %>%
  group_by(issue, Post.Created.Date) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  complete(issue, Post.Created.Date) %>%
  mutate(value = replace_na(value, 0)) %>%
  group_by(issue) %>%
  mutate(cumvalue = cumsum(value)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(Post.Created.Date), y = cumvalue, color = issue, group = issue,
             text = paste(
               "Emne: ", issue, "\n",
               "Dato: ", Post.Created.Date, "\n",
               "Akkumuleret omtale: ", format(cumvalue, big.mark = ".", decimal.mark = ","), "opslag"
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_color_manual(values = issue.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret emneomtale i Facebook-opslag siden 1. august",
       subtitle = "",
       caption = "")

issues_day_plotly <- ggplotly(issues_day, tooltip = "text") %>%
  layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

# Fix legend by removing (x, 1)
for (i in 1:length(issues_day_plotly$x$data)){
  if (!is.null(issues_day_plotly$x$data[[i]]$name)){
    issues_day_plotly$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', issues_day_plotly$x$data[[i]]$name)
  }
}

# Issues per party
issues_party <- AI1 %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  filter(Parti != "Veganerpartiet") %>%
  ggplot(aes(x = percent, y = reorder_within(Parti, percent, issue), 
             fill = issue, text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Procent score: ", format(round(percent,2), big.mark = ".", decimal.mark = ",")
             ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = issue.col) +
  # scale_fill_manual(values = party.col) +
  facet_wrap(~ Parti, scales = "free", ncol = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 8, color = "black"),
    legend.position="bottom",
    strip.text = element_text(size = 10, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Kommunikerede emner i procent pr. parti siden 01/08/2021",
       subtitle = "",
       caption = "")

issues_party_plotly <- ggplotly(issues_party, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emne</b>")),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))





#-----------------
# ISSUES IN ADS

# # Load data
# A <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/data_clean/Parti_ads.rds") %>%
#   filter(ad_delivery_start_time >= "2022-08-01")

# Load data (new)
A <- readRDS("/Users/Madshove/OneDrive/Mads/golem/ensyvni/fb_ads/Parti_ads.rds") %>%
  filter(ad_delivery_start_time >= "2022-08-01")

# Make spend column with mean spend
A <- A %>%
  mutate(spend = rowMeans(A[ , c("spend_lower", "spend_upper")], na.rm=TRUE))

# Unnest tokens
A1 <- A %>%
  unnest(ad_creative_bodies)

# Classify ads
A1$i_oek <- str_detect(A1$ad_creative_bodies,
                       regex(search_oek, ignore_case = TRUE))

A1$i_ael <- str_detect(A1$ad_creative_bodies,
                       regex(search_ael, ignore_case = TRUE))

A1$i_boern <- str_detect(A1$ad_creative_bodies,
                         regex(search_boern, ignore_case = TRUE))

# A1$i_eu <- str_detect(AI$ad_creative_bodies,
#                       regex(search_eu, ignore_case = TRUE))

A1$i_ind <- str_detect(A1$ad_creative_bodies,
                       regex(search_ind, ignore_case = TRUE))

# A1$i_pension <- str_detect(AI$ad_creative_bodies,
#                            regex(search_pension, ignore_case = TRUE))

A1$i_sund <- str_detect(A1$ad_creative_bodies,
                        regex(search_sund, ignore_case = TRUE))

A1$i_klima <- str_detect(A1$ad_creative_bodies,
                         regex(search_klima, ignore_case = TRUE))

# Make them numeric
A1$i_oek <- as.numeric(A1$i_oek)
A1$i_ael <- as.numeric(A1$i_ael)
A1$i_boern <- as.numeric(A1$i_boern)
# A1$i_eu <- as.numeric(A1$i_eu)
A1$i_ind <- as.numeric(A1$i_ind)
# A1$i_pension <- as.numeric(A1$i_pension)
A1$i_sund <- as.numeric(A1$i_sund)
A1$i_klima <- as.numeric(A1$i_klima)

# We want to get all topics in one column, so we pivot the frame longer
A1 <- pivot_longer(A1, starts_with("i_"), names_to = "issue")

# And filter out those rows that indicate that a post does not contain this specific topic
A1 <- A1 %>%
  filter(value == 1)
# We will of course still have a few more observations that there are posts,
# since some posts using this method includes multiple topics

# issues with right name
A1$issue[A1$issue == "i_ael"] <- "Ældre"
A1$issue[A1$issue == "i_boern"] <- "Børn"
# A1$issue[A1$issue == "i_eu"] <- "EU"
A1$issue[A1$issue == "i_ind"] <- "Indvandring"
A1$issue[A1$issue == "i_oek"] <- "Økonomi"
# A1$issue[A1$issue == "i_pension"] <- "Pension"
A1$issue[A1$issue == "i_sund"] <- "Sundhed"
A1$issue[A1$issue == "i_klima"] <- "Klima"

# Kons
A1$Parti[A1$Parti == "Det Konservative Folkeparti"] <- "Konservative"

# Now we would like to say for each party, how much they talk about the specific topics
# and how well they are performing on different interactions metrics
A2 <- A1 %>%
  mutate(spend = rowMeans(A1[ , c("spend_lower", "spend_upper")], na.rm=TRUE),
         impressions = rowMeans(A1[ , c("impressions_lower", "impressions_upper")], na.rm=TRUE)) %>%
  group_by(Parti, issue) %>%
  summarise(npost = n(),
            spend = sum(spend),
            impressions = sum(impressions)
            ) %>%
  ungroup()

# We want a column saying how many percent we're talking about
A2 <- A2 %>%
  group_by(Parti) %>%
  mutate(percent = npost/sum(npost))

# Plot
issues_ads <- A2 %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  ggplot(aes(x = percent*100, y = reorder_within(Parti, percent, issue), fill = Parti,
             text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Procent: ", round(percent*100,2), "%"
             ))) +
  geom_bar(stat = "Identity") +
  facet_wrap(~ issue, scales = "free_y") +
  scale_fill_manual(values = party.col) +
  theme_bw() +
  theme(axis.text.x = element_text(size = 12, angle = 45, vjust = 0.5, hjust=0.5),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title = element_text(size = 12, color = "black"),
        legend.position="bottom",
        strip.text = element_text(size = 12, face = "bold"),
        title = element_text(size = 12),
        panel.grid.major = element_blank(),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Kommunikerede emner pr. parti i procent (siden 1/8/2022)",
       subtitle = "I annoncer siden 1. august 2022",
       caption = "Da en annonce godt kan have mere end et tema \n kan summen godt overstige 100.")

issues_plotly_ads <- ggplotly(issues_ads, tooltip = "text")

# Which sentiment does the parties use on different issues?
issues_sentiment_ads <- A1 %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  group_by(Parti, issue) %>%
  summarise(sentiment = sentida(ad_creative_bodies, output = "mean")) %>%
  ggplot(aes(x = sentiment, y = reorder_within(Parti, sentiment, issue), 
             fill = issue, text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Sentiment score: ", format(round(sentiment,2), big.mark = ".", decimal.mark = ",")
             ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = issue.col) +
  # scale_fill_manual(values = party.col) +
  facet_wrap(~ Parti, scales = "free", ncol = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 8, color = "black"),
    legend.position="bottom",
    strip.text = element_text(size = 10, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Sentiment pr. emne pr. parti siden 01/08/2021",
       subtitle = "",
       caption = "")

issues_sentiment_ads_plotly <- ggplotly(issues_sentiment_ads, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emne</b>")))

# Issues day per day (NUMBER OF ADS)
issues_day_ads <- A1 %>%
  distinct(ad_creative_bodies, Parti, ad_delivery_start_time, issue) %>%
  group_by(issue, ad_delivery_start_time) %>%
  summarise(value = n()) %>%
  ungroup() %>%
  complete(issue, ad_delivery_start_time) %>%
  mutate(value = replace_na(value, 0)) %>%
  group_by(issue) %>%
  mutate(cumvalue = cumsum(value)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(ad_delivery_start_time), y = cumvalue, color = issue, group = issue,
             text = paste(
               "Emne: ", issue, "\n",
               "Dato: ", ad_delivery_start_time, "\n",
               "Akkumuleret omtale: ", format(cumvalue, big.mark = ".", decimal.mark = ","), "annoncer"
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_color_manual(values = issue.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret emneomtale i annoncer siden 1. august",
       subtitle = "",
       caption = "")

issues_day_ads_plotly <- ggplotly(issues_day_ads, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emne</b>")),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

# Fix legend by removing (x, 1)
for (i in 1:length(issues_day_ads_plotly$x$data)){
  if (!is.null(issues_day_ads_plotly$x$data[[i]]$name)){
    issues_day_ads_plotly$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', issues_day_ads_plotly$x$data[[i]]$name)
  }
}

# Issues day (MONEY / SPEND)
issues_day_ads_spend <- A1 %>%
  group_by(issue, ad_delivery_start_time) %>%
  summarise(value = sum(spend)) %>%
  ungroup() %>%
  complete(issue, ad_delivery_start_time) %>%
  mutate(value = replace_na(value, 0)) %>%
  group_by(issue) %>%
  mutate(cumvalue = cumsum(value)) %>%
  ungroup() %>%
  ggplot(aes(x = as.Date(ad_delivery_start_time), y = cumvalue/1000, color = issue, group = issue,
             text = paste(
               "Emne: ", issue, "\n",
               "Dato: ", ad_delivery_start_time, "\n",
               "Kr. brugt på annoncer: ", format(round(cumvalue,0), big.mark = ".", small.mark = ","), "kr."
             ))) +
  geom_line() +
  scale_x_date(date_labels="%d %b",date_breaks  ="1 week",
               limits = c(as.Date("2022-08-01"), as.Date("2022-11-01"))) +
  scale_color_manual(values = issue.col) +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_text(size = 10, color = "black", angle = 25, vjust = 0.5, hjust=0.5),
        title = element_text(size = 12, color = "black"),
        text = element_text(family = "Helvetica")) +
  labs(y = "1.000 kr.",
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Akkumuleret forbrug pr. emne i annoncer siden 1. august",
       subtitle = "",
       caption = "")

issues_day_ads_spend_plotly <- ggplotly(issues_day_ads_spend, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emne</b>")),
         xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

# Fix legend by removing (x, 1)
for (i in 1:length(issues_day_ads_spend_plotly$x$data)){
  if (!is.null(issues_day_ads_spend_plotly$x$data[[i]]$name)){
    issues_day_ads_spend_plotly$x$data[[i]]$name = gsub('^\\(|,\\d+\\)$', '', issues_day_ads_spend_plotly$x$data[[i]]$name)
  }
}

# Issues per party
issues_party_ads <- A2 %>%
  # filter(Parti != "Danmarksdemokraterne") %>%
  filter(Parti != "Veganerpartiet") %>%
  ggplot(aes(x = percent, y = reorder_within(Parti, percent, issue), 
             fill = issue, text = paste(
               "Parti: ", Parti, "\n",
               "Emne: ", issue, "\n",
               "Procent: ", format(round(percent,2), big.mark = ".", decimal.mark = ",")
             ))) +
  geom_bar(stat = "Identity") +
  scale_fill_manual(values = issue.col) +
  # scale_fill_manual(values = party.col) +
  facet_wrap(~ Parti, scales = "free", ncol = 5) +
  theme_bw() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title = element_text(size = 8, color = "black"),
    legend.position="bottom",
    strip.text = element_text(size = 10, face = "bold"),
    title = element_text(size = 12, face = "bold"),
    panel.grid.major = element_blank(),
    text = element_text(family = "Helvetica")) +
  labs(y = NULL,
       x = NULL,
       shape = NULL, 
       col = NULL,
       title = "Kommunikerede emner i procent pr. parti siden 01/08/2021",
       subtitle = "",
       caption = "")

issues_party_ads_plotly <- ggplotly(issues_party_ads, tooltip = "text") %>%
  layout(legend = list(orientation = 'h', title = list(text = "<b>Emner</b>")))
