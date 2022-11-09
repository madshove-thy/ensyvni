# ensyvni
Code for ensyvni.dk during danish national election 2022


R scripts:
- 27 to 31 involves getting data. 27 scrapes detailed targeting from Meta Ad Library using Fabio Vottas functions. 28 and 31 gets ads from Meta Ad Library using the Radlibrary package by Colin Fraser, where the first downloads data of candidates while 31 only includes sitting members of parliament -- also, they do basic wrangling and in the end of both scripts creates a specific dataframe with one row per candidate (used for the reactable tables on the website). 29 gets ads from Snapchat. 30 gets ads from Google/Youtube.
- 33 performs basic tidyverse data wrangling and visualisation with ggplot2 and plotly on insights related to spending and targeting. This also includes the visualisations of spending on Snapchat and Google/Youtube
- 34 uses a dictionary approach to categorize Facebook posts (not included in repository, downloaded via Crowdtangle, let me know if you need something from there) and Meta ads. Also visualisations of the topics.
- 36 and 37 uses the one-row-per-candidate dataframe from 28 and 31 to create a (kinda beautiful) table with the help of reactable and sparkline.
- 38 uses the detailed targeting from script 27 to create a reactable 

Rmd script:
- Compiles script 33-38 and print out the visualisations.

Let me know if anything is troubling you or if you have good ideas about how to do it differently! :-)
