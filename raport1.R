library("stringr")
library("ggplot2")
library(tidyr)
library(reshape2)
library(dplyr)

df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")

rated_true <- df[df$rated == "True", ]
rated_false <- df[df$rated == "False", ]

openings <- vector()

for(i in 1:nrow(df)){
  
  opening_name <- df[i, c("opening_name")]
  
  if(str_detect(opening_name, ":") == TRUE){
    
    location <- str_locate(opening_name, ":")
    openings[i] <- str_sub(opening_name, 1, location[1,1] - 1)
  }
  
  else if(str_detect(opening_name, "#") == TRUE){
    
    location <- str_locate(opening_name, "#")
    openings[i] <- str_sub(opening_name, 1, location[1,1] - 2)
  }
    
  else if(str_detect(opening_name, "\\|") == TRUE){
    
      
    location <- str_locate(opening_name, "\\|")
    openings[i] <- str_sub(opening_name, 1, location[1,1] - 1)
    }
    
  else{
    
    openings[i] <- df[i, c("opening_name")]
    
  }
  
  
}

df["openings_general"] <- openings

openings_sicilian <- vector()

for(i in 1:nrow(df)){
  
  opening_name <- df[i, c("opening_name")]
  
  if(str_detect(opening_name, "Sicilian") == TRUE){
    
    openings_sicilian[i] <- opening_name
  }
  
  else{
    
    openings_sicilian[i] <- 0
    
  }
  
  
}



ggplot(df, aes(white_rating)) + geom_histogram()
ggplot(df, aes(black_rating)) + geom_histogram()

df_long_rating <- gather(df, player, rating, c("white_rating", "black_rating") ,factor_key=TRUE)

ggplot(df_long_rating, aes(rating, fill=player)) + geom_histogram(position="dodge") + facet_grid(~player)

mean(df["black_rating"])


games_id <- vector()

for(i in 1:nrow(df)){
  game_id <- df$id[i]
  
  if(game_id %in% games_id){
    games_id[i] <- i
  }
  else{
    games_id[i] <- game_id
  }
  
  
}

agg_tbl <- df %>% group_by(openings_general) %>% 
  summarise(total_count=n(),
            .groups = 'drop')

agg_tbl_ordered <-agg_tbl[order(agg_tbl$total_count, decreasing=TRUE),]

agg_to_analize_tbl_ordered <- agg_tbl_ordered[agg_tbl_ordered$total_count >= 500, ]

for(i in 1:length(games_id)){
  if(games_id[i] == i){
    print(i)
  }
  
}

the_same_id <- vector()

for(i in 1:nrow(df)){
  if(df)
  
}

df <- na.omit(df)
df <- distinct(df)

white_wins_count = sum(df$winner == "white")
black_wins_count = sum(df$winner == "black")
not_draw_count = sum(df$winner != "draw")

white_wins_percent = white_wins_count/not_draw_count
black_wins_percent = 1 - white_wins_percent

probab = function(dat){a
  all_count <- sum(dat$winner != "a")
  white_wins_count <- sum(dat$winner == "white")
  black_wins_count <- sum(dat$winner == "black")
  white_prob <-white_wins_count/all_count
  black_prob <-black_wins_count/all_count
  draw_prob <- 1 - white_prob - black_prob
  
  return(c(white_prob, black_prob, draw_prob))
}

df_sicilian <- df[df["openings_general"] == "Sicilian Defense",]
prob_sicilian <- probab(df_sicilian)

df_french <- df[df["openings_general"] == "French Defense",]
prob_french <- probab(df_french)

df_queen <- df[df["openings_general"] == "Queen's Pawn Game",]
prob_queen <- probab(df_queen)

df_italian <- df[df["openings_general"] == "Italian Game",]
prob_italian <- probab(df_italian)

df_king <- df[df["openings_general"] == "King's Pawn Game",]
prob_king <- probab(df_king)

df_ruy <- df[df["openings_general"] == "Ruy Lopez",]
prob_ruy <- probab(df_ruy)

df_english <- df[df["openings_general"] == "English Opening",]
prob_english <- probab(df_english)

df_scan <- df[df["openings_general"] == "Scandinavian Defense",]
prob_scan <- probab(df_scan)

df_philidor <- df[df["openings_general"] == "Philidor Defense",]
prob_philidor <- probab(df_philidor)

df_carokann <- df[df["openings_general"] == "Caro-Kann Defense",]
prob_carokann <- probab(df_carokann)


top_openings <- agg_to_analize_tbl_ordered$openings_general

probs_white <- vector()
probs_black <- vector()
probs_draw <- vector()

for(i in 1:10){
  opening_df <- df[df["openings_general"] == top_openings[i],] 
  prob <- probab(opening_df)
  probs_white[i] <- prob[1]
  probs_black[i] <- prob[2]
  probs_draw[i] <- prob[3]
}

probs_df <- data.frame(Otwarcie = top_openings, Bia³y_gracz = probs_white, Czarny_gracz = probs_black, Remis = probs_draw)

tab <- as.table(probs_df)
















