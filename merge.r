library('tidyverse')
library(reshape2)
library('boot')
library('gridExtra')


# df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")
df <- read.csv(file = "C:/Users/mkarc/Gauus/Chess-analysis/chess_games.csv")

#Usuwanie danych ----------------------------------------
df <- na.omit(df)
df <- distinct(df)

#Dodawanie nowych kolumn ------------------------------
df <-  df %>% mutate(
    white_win = ifelse(winner =='white',1,0),
    black_win = ifelse(winner =='black',1,0),
    draw = ifelse(winner =='draw',1,0)
)
df <- cbind(df,white_ranking_cat=(df$white_rating %/% 100 + 1) * 100)
df <- cbind(df,ranting_difrents=df$white_rating-df$black_rating)
df <- cbind(df,rating_diffrents_cat=(df$ranting_difrents %/% 100 + 1)* 100)
df <- cbind(df,turns_cat=(df$turns %/% 10 + 1)* 10)


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

#Inne data framy do generowania wykres�w

wygrana_czas <- aggregate(df$wygrana_bialego,list(df$turns_cat),mean)
wygrana_rating<- aggregate(df$wygrana_bialego,list(df$white_ranking_cat),mean)
wygrana_roznica<- aggregate(df$wygrana_bialego,list(df$rating_diffrents_cat),mean)

# Funkcja do tworzenia dataframe z przedziałami ufności 
#-------------------------------------------------------
samplemean <- function(x, d) {
    mean(x[d])
}
generate_data <- function(df, column, place){ 
    white_win <- numeric()
    white_win_bottom <- numeric()
    white_win_top <- numeric()
    black_win <- numeric()
    black_win_bottom <- numeric()
    black_win_top <- numeric()
    draw <- numeric()
    draw_bottom <- numeric()
    draw_top <- numeric()
    values <- numeric()

    for (value in sort(unique(column))) {
        # print(value)
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
    write_csv(new_df, place)
}
#-------------------------------------------------------------

#stowrzenie nowych dataframe 
generate_data(df,df$white_ranking_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_ranting.csv")
generate_data(df,df$rating_diffrents_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_diffrents_ranting.csv")
generate_data(df,df$turns_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_turns.csv")

# Na podstawie rankingu białego -----------------------------------------------------------------

plot_white_ratig <- ggplot(data=df, mapping=aes(x=white_rating)) +  geom_histogram(bins=50)+ 
    labs(x = 'Ranking białego') # rozkład  rankingu białego
plot_black_rating <- ggplot(data=df, mapping=aes(x=black_rating)) +  geom_histogram(bins=50)+ 
    labs(x = 'Ranking czarnego') # rozkład  rankingu czarnego
grid.arrange(plot_white_ratig,plot_black_rating)

new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_ranting.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'ranting białego', y = 'Prawdopodobieństwo wygranej' , title="Wygrana białego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'ranting białego', y = 'Prawdopodobieństwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'ranting białego', y = 'Prawdopodobieństwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)

# Na podstawie r�nic rankingu 

ggplot(data=df, mapping=aes(x=ranting_difrents)) +  geom_histogram(bins=50)+ 
    labs(x = 'R�nice w rankingu')

new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_diffrents_ranting.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'r�nica w rankingu', y = 'Prawdopodobie�stwo wygranej' , title="Wygrana bia�ego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'r�nica w rankigu', y = 'Prawdopodobie�stwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'r�nica w rankingu', y = 'Prawdopodobie�stwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)

# Na podstawie czasu 
ggplot(data=df, mapping=aes(x=turns)) +  geom_histogram(bins=50)+ 
    labs(x = 'Ilo�� ruch�w')
new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_turns.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'Ilo�� ruch�w', y = 'Prawdopodobie�stwo wygranej' , title="Wygrana bia�ego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'Ilo�� ruch�w', y = 'Prawdopodobie�stwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'Ilo�� ruch�w', y = 'Prawdopodobie�stwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)