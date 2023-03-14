library(tidyverse)
library(rtweet)

# Recogemos Tweets
ele_tamps_tweet <- rtweet::search_tweets(q = "tamaulipas", 
                      n = 15000, 
                      include_rts = F)

# Guardamos información: 
saveRDS(ele_tamps_tweet, 
        "01_Datos/ele_tamps_tweet_1.rds")

names(ele_tamps_tweet)
ele_tamps_tweet %>% 
    select(created_at,screen_name, text ) %>% 
    openxlsx::write.xlsx("01_Datos/ele_tamps_tweet_1.xlsx")
