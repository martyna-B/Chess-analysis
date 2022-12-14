library('tidyverse')
library('GGally')
# df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")
df <- read.csv(file = "C:/Users/mkarc/Gauus/Chess-analysis/chess_games.csv")
df <- na.omit(df)
df <- distinct(df)

rated_true <- df[df$rated == "True", ]
reted_false <- df[df$rated == "False", ]
df <- cbind(df,wygrana_bialego=1*(df$winner == "white"))
mean(df$wygrana_bialego)

df <- cbind(df,white_ranking_cat=(df$white_rating %/% 100 + 1) * 100)
df <- cbind(df,ranting_difrents=df$white_rating-df$black_rating)
wygrana_rating<- aggregate(df$wygrana_bialego,list(df$white_ranking_cat),mean)
# ggplot(data=df,aes(x=white_ranking_cat,y=wygrana_bialego)) + geom_bar()
# Prawdopodobieństwo wygranej w zależności od rating
ggplot(data=wygrana_rating) + geom_bar(aes(x=Group.1,y=x),stat='identity') + 
    labs(x = 'ranting białego', y = 'Prawdopodobieństwo wygranej' , title="Prawdopodobieństwo wygranej od rantingu białego") 

#Histopgram różnic w rankingu
ggplot(data=df, mapping=aes(x=ranting_difrents)) +  geom_histogram(bins=50)+ 
    labs(x = 'różnica rantingu' , title="Rozkład  różnic rantingu") 
mean(df$ranting_difrents) # <- statystyczna przewaga
sqrt(var(df$ranting_difrents))
df <- cbind(df,rating_diffrents_cat=(df$ranting_difrents %/% 100 + 1)* 100)
wygrana_roznica<- aggregate(df$wygrana_bialego,list(df$rating_diffrents_cat),mean)
# ggplot(data=df,aes(x=white_ranking_cat,y=wygrana_bialego)) + geom_bar()
# Prawdopodobieństwo wygranej w zależności od roznicy rating
ggplot(data=wygrana_roznica) + geom_bar(aes(x=Group.1,y=x),stat='identity')+ 
    labs(x = 'różnica w rantingu', y = 'Prawdopodobieństwo wygranej' , title="Prawdopodobieństwo wygranej od różnicy rantingu") 

# Prawdopodobieństwo wygrania białych w zaleznośli od czasu gry
df <- cbind(df,turns_cat=(df$turns %/% 10 + 1)* 10)
wygrana_czas <- aggregate(df$wygrana_bialego,list(df$turns_cat),mean)

ggplot(data=wygrana_czas) + geom_bar(aes(x=Group.1,y=x),stat='identity')+ 
    labs(x = 'czas gry', y = 'Prawdopodobieństwo wygranej' , title="Prawdopodobieństwo wygranej od czasu gry") 


ggpairs(select(df,white_rating,ranting_difrents,
               black_rating))
corre



df$white_ranking_cat

