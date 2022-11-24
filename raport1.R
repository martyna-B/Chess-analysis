library("stringr")
library("ggplot2")
library(tidyr)
library(reshape2)

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

ggplot(df, aes(white_rating)) + geom_histogram()
ggplot(df, aes(black_rating)) + geom_histogram()

df_long_rating <- gather(df, player, rating, c("white_rating", "black_rating") ,factor_key=TRUE)

ggplot(df_long_rating, aes(rating, fill=player)) + geom_histogram(position="dodge") + facet_grid(~player)

mean(df["black_rating"])