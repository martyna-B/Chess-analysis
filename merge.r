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


#openingi -----
#tworzenie kolumny "openings_general"
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

#przedzia³y ufnoœci dla otwaræ

agg_tbl <- df %>% group_by(openings_general) %>% 
  summarise(total_count=n())

agg_tbl_ordered <-agg_tbl[order(agg_tbl$total_count, decreasing=TRUE),]

agg_to_analize_tbl_ordered <- agg_tbl_ordered[agg_tbl_ordered$total_count >= 500, ]

top_openings <- agg_to_analize_tbl_ordered$openings_general

top_openings_df <- filter(df, df$openings_general %in% top_openings)


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
    }
  }
  new_df <- data.frame(values, white_win,white_win_top,white_win_bottom, black_win,black_win_top,black_win_bottom,draw,draw_bottom,draw_top)
  return(new_df)
}


#wykres dla otwaræ bez przedzia³ów ufnoœci
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
  scale_y_continuous(name ="Czêstoœæ ró¿nych wyników") + 
  scale_fill_discrete(labels=c('Wygrana bia³ego', 'Wygrana czarnego', 'Remis'))

#Inne data framy do generowania wykresï¿½w

wygrana_czas <- aggregate(df$wygrana_bialego,list(df$turns_cat),mean)
wygrana_rating<- aggregate(df$wygrana_bialego,list(df$white_ranking_cat),mean)
wygrana_roznica<- aggregate(df$wygrana_bialego,list(df$rating_diffrents_cat),mean)

# Funkcja do tworzenia dataframe z przedziaÅ‚ami ufnoÅ›ci 
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
    return(new_df)
}
#-------------------------------------------------------------

#stowrzenie nowych dataframe 
generate_data(df,df$white_ranking_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_ranting.csv")
generate_data(df,df$rating_diffrents_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_diffrents_ranting.csv")
generate_data(df,df$turns_cat, "C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_turns.csv")

# Na podstawie rankingu biaÅ‚ego -----------------------------------------------------------------

plot_white_ratig <- ggplot(data=df, mapping=aes(x=white_rating)) +  geom_histogram(bins=50)+ 
    labs(x = 'Ranking biaÅ‚ego') # rozkÅ‚ad  rankingu biaÅ‚ego
plot_black_rating <- ggplot(data=df, mapping=aes(x=black_rating)) +  geom_histogram(bins=50)+ 
    labs(x = 'Ranking czarnego') # rozkÅ‚ad  rankingu czarnego
grid.arrange(plot_white_ratig,plot_black_rating)

new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_ranting.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'ranting biaÅ‚ego', y = 'PrawdopodobieÅ„stwo wygranej' , title="Wygrana biaÅ‚ego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'ranting biaÅ‚ego', y = 'PrawdopodobieÅ„stwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'ranting biaÅ‚ego', y = 'PrawdopodobieÅ„stwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)

# Na podstawie rï¿½nic rankingu 

ggplot(data=df, mapping=aes(x=ranting_difrents)) +  geom_histogram(bins=50)+ 
    labs(x = 'Rï¿½nice w rankingu')

new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_diffrents_ranting.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'rï¿½nica w rankingu', y = 'Prawdopodobieï¿½stwo wygranej' , title="Wygrana biaï¿½ego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'rï¿½nica w rankigu', y = 'Prawdopodobieï¿½stwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'rï¿½nica w rankingu', y = 'Prawdopodobieï¿½stwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)

# Na podstawie czasu 
ggplot(data=df, mapping=aes(x=turns)) +  geom_histogram(bins=50)+ 
    labs(x = 'Iloï¿½ï¿½ ruchï¿½w')
new_df = read.csv("C:/Users/mkarc/Gauus/Chess-analysis/winner_depend_turns.csv")
plot_white <- ggplot(data=new_df,aes(x=values,y=white_win))  + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=white_win_bottom,ymax=white_win_top),color = "red",width=.05)+
    labs(x = 'Iloï¿½ï¿½ ruchï¿½w', y = 'Prawdopodobieï¿½stwo wygranej' , title="Wygrana biaï¿½ego") 
plot_black <- ggplot(data=new_df,aes(x=values,y=black_win)) + geom_bar(stat='identity') + geom_errorbar(aes(ymin=black_win_bottom,ymax=black_win_top),color = "red",width=.05)+
    labs(x = 'Iloï¿½ï¿½ ruchï¿½w', y = 'Prawdopodobieï¿½stwo wygranej' , title="Wygrana czarnego") 
plot_draw <- ggplot(data=new_df,aes(x=values,y=draw)) + geom_bar(stat='identity')+ geom_errorbar(aes(ymin=draw_bottom,ymax=draw_top),color = "red",width=.05)+
    labs(x = 'Iloï¿½ï¿½ ruchï¿½w', y = 'Prawdopodobieï¿½stwo wygranej' , title="Remis") 
grid.arrange(plot_white,plot_black,plot_draw)

#Analizowanie pod k¹tem ró¿nych otwaræ----------------------------------------------------------
probab = function(dat){a
  all_count <- sum(dat$winner != "a")
  white_wins_count <- sum(dat$winner == "white")
  black_wins_count <- sum(dat$winner == "black")
  white_prob <-white_wins_count/all_count
  black_prob <-black_wins_count/all_count
  draw_prob <- 1 - white_prob - black_prob
  
  return(c(white_prob, black_prob, draw_prob))
}

top_openings <- agg_to_analize_tbl_ordered$openings_general
top_openings_df <- df[df["openings_general"] == top_openings,]

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