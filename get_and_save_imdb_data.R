# Get and download IMDB data
# https://datasets.imdbws.com/

# IMDB Data Dictionary
# https://www.imdb.com/interfaces/

# IMDB Series IDs
#  - Top Gear: tt1628033
#  - Grand Tour: tt5712554

# IMDB Downloads
#  - title.basics.tsv.gz
#  - title.episode.tsv.gz
#  - title.ratings.tsv.gz

library(dplyr)
library(readr)
library(tidyr)

#titles <- read_delim('data/title_basics.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')
# Load data 
titles <- read_delim("data/title_basics.tsv", "\t", escape_double = FALSE, na = "\\N", trim_ws = TRUE, quote='',
                     col_types = cols(
                       tconst = col_character(), 
                       titleType = col_character(),
                       primaryTitle = col_character(),
                       originalTitle = col_character(),
                       isAdult = col_logical(),
                       startYear = col_character(),
                       endYear = col_integer(),                 
                       runtimeMinutes = col_integer(), 
                       genres = col_character()))
episodes <- read_delim('data/title_episodes.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')
ratings <- read_delim('data/title_ratings.tsv', '\t', escape_double = FALSE, na = '\\N', trim_ws = TRUE, quote='')

series_titles <- filter(titles,tconst %in% c('tt1628033','tt5712554'))[,c(1,3)]
names(series_titles) <- c('tconst','series_title')

tg_gt_episodes <- filter(episodes,parentTconst %in% c('tt1628033','tt5712554'))
episode_titles <- filter(titles,tconst %in% tg_gt_episodes$tconst)[,c(1,3,6,7,8,9)]
names(episode_titles) <- c('tconst','episode_title','episode_startYear','episode_endYear','episode_runtimeMinutes','episode_genres')


tg_gt_df <- left_join(tg_gt_episodes,ratings, by = ("tconst" = "tconst"))
tg_gt_df <- left_join(tg_gt_df,episode_titles, by = ("tconst" = "tconst"))
tg_gt_df$series_title <- ifelse(tg_gt_df$parentTconst == series_titles$tconst[1],series_titles$series_title[1],series_titles$series_title[2])

tg_gt_df <- filter(tg_gt_df,!is.na(episode_startYear))
# Check if Patagonia Special: Part 1 & 2 2014 or 50 Years of Bond Cars 2012 should be filtered out
tg_gt_df <- filter(tg_gt_df,!is.na(episodeNumber))

total_episodes <- tg_gt_df %>% group_by(series_title,episode_startYear)%>% summarise(max_epi = max(episodeNumber))
tg_gt_df2 <- left_join(tg_gt_df,total_episodes, by = c("series_title" = "series_title", "episode_startYear" = "episode_startYear"))
tg_gt_df2$days <- round((365/tg_gt_df2$max_epi)*tg_gt_df2$episodeNumber,0)
write.csv(tg_gt_df,'data/tggt_imdb_ratings.csv', row.names = F)
