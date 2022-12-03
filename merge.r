library('tidyverse')
library(reshape2)
library('GGally')

df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")
#df <- read.csv(file = "C:/Users/mkarc/Gauus/Chess-analysis/chess_games.csv")

#Usuwanie danych ----------------------------------------
df <- na.omit(df)
df <- distinct(df)

#Dodawanie nowych kolumn ------------------------------
df <- cbind(df,wygrana_bialego=1*(df$winner == "white"))
df <- cbind(df,przedzial_rankingu_bialego=df$white_rating %/% 100 * 100)
df <- cbind(df,ranting_difrents=df$white_rating-df$black_rating)
df <- cbind(df,przedzial_roznicy_rankingu=df$ranting_difrents %/% 100 * 100)
df <- cbind(df,przedzial_czasu=df$turns %/% 10 * 10)

#openingi
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