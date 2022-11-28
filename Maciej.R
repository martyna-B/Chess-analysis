library('tidyverse')
library("glue")
# df <- read.csv(file = "C:/Users/Martyna/Studia/Pakiety statystyczne/raport1/chess_games.csv")
df <- read.csv(file = "C:/Users/mkarc/Gauus/Chess-analysis/chess_games.csv")
df <- na.omit(df)
df <- distinct(df)

rated_true <- df[df$rated == "True", ]
reted_false <- df[df$rated == "False", ]
df <- cbind(df,wygrana_bialego=1*(df$winner == "white"))
mean(df$wygrana_białego)

df <- cbind(df,przedzial_rankingu_bialego=df$white_rating %/% 100 * 100)
df <- cbind(df,ranting_difrents=df$white_rating-df$black_rating)
wygrana_rating<- aggregate(df$wygrana_bialego,list(df$przedzial_rankingu_bialego),mean)
# ggplot(data=df,aes(x=przedzial_rankingu_bialego,y=wygrana_bialego)) + geom_bar()
# Prawdopodobieństwo wygranej w zależności od rating
ggplot(data=wygrana_rating) + geom_bar(aes(x=Group.1,y=x),stat='identity')


#Histopgram różnic w rankingu
ggplot(data=df, mapping=aes(x=ranting_difrents)) + 
    geom_histogram(bins=50)

df <- cbind(df,przedzial_roznicy_rankingu=df$ranting_difrents %/% 100 * 100)
wygrana_roznica<- aggregate(df$wygrana_bialego,list(df$przedzial_roznicy_rankingu),mean)
# ggplot(data=df,aes(x=przedzial_rankingu_bialego,y=wygrana_bialego)) + geom_bar()
# Prawdopodobieństwo wygranej w zależności od roznicy rating
ggplot(data=wygrana_roznica) + geom_bar(aes(x=Group.1,y=x),stat='identity')


a <- 'ala'
b <- "kot"
print(a + b)

df$przedzial_rankingu_bialego

