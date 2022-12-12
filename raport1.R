library("stringr")
library("ggplot2")
library(tidyr)
library(reshape2)
library(dplyr)
library("boot")

df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")


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

probab = function(dat){
  all_count <- sum(dat$winner != "a")
  white_wins_count <- sum(dat$winner == "white")
  black_wins_count <- sum(dat$winner == "black")
  white_prob <-white_wins_count/all_count
  black_prob <-black_wins_count/all_count
  draw_prob <- 1 - white_prob - black_prob
  
  return(c(white_prob, black_prob, draw_prob, all_count))
}



top_openings <- agg_to_analize_tbl_ordered$openings_general


top_openings_df <- filter(df, df$openings_general %in% top_openings)

probs_white <- vector()
probs_black <- vector()
probs_draw <- vector()
coun_vec <- vector()

for(i in 1:10){
  opening_df <- df[df["openings_general"] == top_openings[i],] 
  prob <- probab(opening_df)
  probs_white[i] <- prob[1]
  probs_black[i] <- prob[2]
  probs_draw[i] <- prob[3]
  coun_vec[i] <- prob[4]
}

probs_df <- data.frame(Otwarcie = top_openings, Bia³y_gracz = probs_white, Czarny_gracz = probs_black, Remis = probs_draw)

samplemean <- function(x, d) {
  mean(x[d])
}
generate_data_openings <- function(df, column){ 
  white_win <- numeric()
  white_win_bottom <- numeric()
  white_win_top <- numeric()
  black_win <- numeric()
  black_win_bottom <- numeric()
  black_win_top <- numeric()
  draw <- numeric()
  draw_bottom <- numeric()
  draw_top <- numeric()
  values <- character()
  
  for (value in sort(unique(column))) {
    df2 <- df[which(column == value), ]
    
    if (count(df2) > 10  ) {
      
      values <- append(values,value)
      white_ciboot <- boot.ci(boot(df2$white_win, samplemean, R = 1000), conf = 0.95, c("perc"))
      white_win <- append(white_win, white_ciboot$t0)
      white_win_bottom <- append(white_win_bottom, white_ciboot$perc[4])
      white_win_top <- append(white_win_top, white_ciboot$perc[5])
      
      black_ciboot <- boot.ci(boot(df2$black_win, samplemean, R = 1000), conf = 0.95, c("perc"))
      black_win <- append(black_win, black_ciboot$t0)
      black_win_bottom <- append(black_win_bottom, black_ciboot$perc[4])
      black_win_top <- append(black_win_top, black_ciboot$perc[5])
      
      draw_ciboot <- boot.ci(boot(df2$draw, samplemean, R = 1000), conf = 0.95, c("perc"))
      draw <- append(draw, draw_ciboot$t0)
      draw_bottom <- append(draw_bottom, draw_ciboot$perc[4])
      draw_top <- append(draw_top, draw_ciboot$perc[5])
      # print(draw_bottom)      
    }
  }
  new_df <- data.frame(values, white_win,white_win_top,white_win_bottom, black_win,black_win_top,black_win_bottom,draw,draw_bottom,draw_top)
  return(new_df)
}




top_df <- generate_data_openings(top_openings_df, top_openings_df$openings_general)

top_df_long <- gather(top_df, win, value, c(white_win, black_win, draw), factor_key=TRUE)



win_bottom <- vector()
win_top <- vector()

for(i in 1:30){
  if(top_df_long[i, "win"] == "white_win"){
    win_bottom[i] <- top_df_long[i, "white_win_bottom"]
    win_top[i] <- top_df_long[i, "white_win_top"]
  }
  else if(top_df_long[i, "win"] == "black_win"){
    win_bottom[i] <- top_df_long[i, "black_win_bottom"]
    win_top[i] <- top_df_long[i, "black_win_top"]
  }
  else{
    win_bottom[i] <- top_df_long[i, "draw_bottom"]
    win_top[i] <- top_df_long[i, "draw_top"]
  }
  
}

top_df_long["bottom"] <- win_bottom
top_df_long["top"] <- win_top
  

ggplot(data=top_df_long, aes(x=values, y=value, fill=win)) + geom_bar(stat="identity", position="dodge") +
  geom_errorbar(aes(ymin=bottom,
                    ymax=top), position = position_dodge(0.9)) +
  theme(axis.text.x = element_text(angle=90), legend.title = element_blank()) +
  scale_x_discrete(name ="") + 
  scale_y_continuous(name ="Estymowane prawdopodobieñstwo") + 
  scale_fill_discrete(labels=c('Wygrana bia³ego', 'Wygrana czarnego', 'Remis'))






