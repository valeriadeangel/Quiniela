#libraries
library(readxl)
library(dplyr)
library(tidyverse)
library(hrbrthemes)
library(kableExtra)
# options(knitr.table.format = "html")
# library(streamgraph)
library(viridis)
library(DT)
library(plotly)
library(tidyr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(ggrepel)


# Basic results
quiniela <- read_excel("C:/Users/valer/Desktop/Quiniela WC 2022_final.xlsx", sheet = "r")
View(quiniela)

colnames(quiniela)[36]
colnames(quiniela)[37]

team1 <- quiniela[36]
team2 <- quiniela[37]

quiniela$name[which((team1 == team2)==T)] # Who predicted a draw?
quiniela$name[which((team1 > team2)==T)] #Team 1 to win
quiniela$name[which((team1 < team2)==T)] #Team 2 to win


a<-length(quiniela$name[which((team1 == team2)==T)])
b<-length(quiniela$name[which((team1 > team2)==T)])  #Team 1 to win
c<-length(quiniela$name[which((team1 < team2)==T)]) #Team 2 to win


#pie chart
df<-data.frame(
  Prediction = c("Draw", "Portugal ", "Sam"),
  value = c(a, b, c))
df2 <- df %>% 
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))
ggplot(df, aes(x = "" , y = value, fill = fct_inorder(Prediction))) +
  geom_col(width = 1, color = 1) +
  coord_polar(theta = "y") +
  scale_fill_brewer(palette = "Pastel1") +
  geom_label_repel(data = df2,
                   aes(y = pos, label = paste0(value)),
                   size = 4.5, nudge_x = 1, show.legend = FALSE) +
  guides(fill = guide_legend(title = "Predictions")) +
  theme_void()


#bubble chart -----

test <- quiniela |> 
  select(34:35) |>
  rename(Team1 = 1, Team2 = 2)
test$c <- paste(test$Team1, test$Team2)
freqt <- as.data.frame(table(test$c))
freqt <- freqt |>
  rename(c = Var1) |>
  merge(test)
freqt %>%
  ggplot(aes(x=Team2, y=Team1, size = Freq, color = Freq)) +
  geom_point(alpha=0.5) +
  scale_color_gradient(trans = "reverse") +
  scale_size(range = c(min(freqt$Freq), max(freqt$Freq)), name="N people with this prediction") +
  ylab("Portugal ") +
  # ylim(0,4) +
  xlab("Ghana") + 
  xlim(-0.1, 3) +
  geom_abline(slope=1, intercept=0)

## exact score
quiniela$name[which(team1==0
                    & team2==3)]  
quiniela$`Czech Republic - England [Country 2]`["name" == "Val"]



##TRAJECTORY PLOT## ------
library("RColorBrewer")
install.packages("paletteer")
library("paletteer")

trajectories <- read_excel("C:/Users/valer/Desktop/Quiniela WC 2022_final.xlsx", sheet = "t")
View(trajectories)

t <- trajectories |>
  select(1, 8:12) |>
    #make long DF
  gather(key = day, value = Points, d1:d5)

#make a factor
t$day <- as.factor(t$day)
levels(t$day ) <- c("1", "2", "3", "4", "5")

minbar = min(trajectories$sum)
maxbar = max(trajectories$sum)

#stacked bar chart
p <- ggplot(t, aes(x = Names, y = Points, fill = day)) + 
  geom_col() + 
  theme(axis.text.x = element_text(angle = 90, hjust=1, vjust = -0.0)) + 
  scale_y_continuous(breaks = seq(3, maxbar+1, by = 3),
                     minor_breaks = seq(1, maxbar+1, by = 1),
                     expand = expansion(add = c(0,2))) + 
  geom_abline(slope = 0, intercept = maxbar, col = "chartreuse4", lwd=1.3 ) + 
  geom_abline(slope = 0, intercept = minbar, col = "brown", lwd=1.3) 
p 
# + scale_fill_brewer(palette = "Pastel1")



#change df into tall format !! CHANGE NUMBER OF GAMES!! 
DFtall <- point_trajectories %>% gather(key = Games, value = Points, zero:eighteen)
View(DFtall)

# make Games an ordered factor
DFtall$Games <- factor(DFtall$Games, levels = DFtall$Games)


# Plot
p <- t %>%
  ggplot(aes(x=day, y=Points, group=Names, color=Names)) +
  geom_line() +
  labs(x = "Matches") +
  scale_color_viridis(discrete = TRUE) +
  theme(
    legend.position="none",
    plot.title = element_text(size=6)
  ) +
  ggtitle("Points Trajectory") +
  theme_ipsum_es()  
  # scale_x_discrete (limits = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',  'eleven', 'twelve','thirteen',	'fourteen',	'fifteen',	'sixteen',	'seventeen',	'eighteen'),
  #                   labels = 0:18)

ggplotly(p)

scale_x_discrete (limits = c('zero', 'one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten',  'eleven', 'twelve','thirteen',	'fourteen',	'fifteen',	'sixteen',	'seventeen',	'eighteen'),
                  labels = 0:18)



#Sexist boxplot
genderplot <- read_excel("C:/Users/valer/Desktop/Quiniela Eurocopa 2021 Master List_with scores.xlsx", 2)
view(genderplot)

bp <- genderplot %>%
  ggplot( aes(x=Gender, y=total, fill=Gender)) +
  geom_violin() +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Sexist Boxplot") +
  xlab("Gender")


Male_scores <- filter(genderplot, Gender == "Male")
Female_scores <- filter(genderplot, Gender == "Female")

ggplotly(bp) %>%
  layout(annotations = list(text = "p < 0.001", x = 0.7, y = 23, showarrow = FALSE))

t.test(Female_scores$total, Male_scores$total)


#Xenophobic Plot
nationalityplot <- read_excel("C:/Users/valer/Desktop/Quiniela Eurocopa 2021 Master List_with scores.xlsx", 2)
view(nationalityplot)

bp2 <- nationalityplot %>%
  ggplot( aes(x=Nationality, y=total, fill=Nationality)) +
  geom_violin() +
  geom_jitter(color="black", size=0.4, alpha=0.9) +
  theme_minimal() +
  theme(
    legend.position="none",
    plot.title = element_text(size=11)
  ) +
  ggtitle("A Xenophobic Vaginaplot") +
  xlab("Nationality")

bp2
UK_scores <- filter(nationalityplot, Nationality == "The Good Type")
nonUK_scores <- filter(nationalityplot, Nationality == "The Bad Type")

ggplotly(bp2) %>%
  layout(annotations = list(text = "p > 0.17", x = 0.7, y = 23, showarrow = FALSE))

t.test(UK_scores$total, nonUK_scores$total)

#import total scores 
###REMEMBER TO SAVE SHEET FIRST###

total_score <- read_excel("C:/Users/valer/Desktop/Quiniela Eurocopa 2021 Master List_with scores.xlsx", 2)
sort.score <- total_score[order(total_score$total, decreasing = TRUE),]
View(sort.score)

#top 10
sort.score[1:10,]
sort.score[52:62,]

filter(sort.score, total == 4)  ##show all with a particular score



#demographics
glimpse(quiniela)[2:4]
quiniela$gender <- as.factor(quiniela$gender)
quiniela$age <- as.factor(quiniela$age)
quiniela$continent <- as.factor(quiniela$continent)
quiniela$nationality <- as.factor(quiniela$nationality)

quiniela |> 

demo <- quiniela[2:5]

demogroup <- demo |>
  group_by(continent,nationality) 

america <- demo |> filter(continent == "America") |> select (nationality) |> unique()
europe <- demo |> filter(continent == "Europe") |> select (nationality) |> unique()
print(europe)

demogroup$nationality <- ordered(demogroup$nationality, levels =c("Argentina",
                                                                  "Colombia",
                                                                  "USA"    ,
                                                                  "Ecuador"   ,
                                                                  "Chile"     ,
                                                                  "Brazil",
                                                                  "Mexico" ,
                                                                  "Germany"       ,
                                                                  "Greece"        ,
                                                                  "Swedish"       ,
                                                                  "Portugal"      ,
                                                                  "Scottish"      ,
                                                                   "Turkish twist" ,
                                                                   "Swedish/Polish",
                                                                   "Ireland"     ,  
                                                                   "Netherlands",
                                                                  "Nigerian",
                                                                  "Spain"         ,
                                                                  "England"       ,
                                                                  "British"       ,
                                                                  "Earthian"
                                                                  ))



cols = c("#00B6EB",  "yellow", "#C49A00", "grey", "beige","#EB8335", "#53B400",
         "black", "#00BDD2","#F8766D", "tomato2", "#619CFF", "darkorchid" ,"#D078FF", "darkolivegreen", "orange", "#FF63B9", "red",     "blue", "blue4", "#FF6B96")

demogroup %>% 
  ggplot(aes(x = continent, group = nationality, fill = nationality)) +
  geom_bar() + 
  scale_fill_manual(values=cols)
  





