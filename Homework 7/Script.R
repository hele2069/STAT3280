### Problem 1
library(tidyverse)
library(ggplot2)
dt <- read.csv("~/Desktop/Homework 7/CountyData_2000-2020.csv")
dt$digits <- substr(as.character(dt$candidatevotes),1,1)
illinois <- dt %>% filter(year==2020 & state=="ILLINOIS")
benford <- data.frame(digits=1:9,val=1:9)
for (i in 1:9) {
  benford$val[i] <- log10((i+1)/i)
}

## overall
vote <- illinois %>% filter(candidate=="JOSEPH R BIDEN JR" | candidate=="DONALD J TRUMP")

ggplot(vote,aes(x=digits)) + 
  geom_bar(aes(y=..prop..,group=candidate,fill="black"),fill="black") + 
  geom_point(data=benford,aes(x=digits,y=val,color="tomato"),size=3) + 
  geom_line(data=benford,aes(x=digits,y=val,color="tomato"),lwd=1) + 
  scale_color_discrete(labels = c("Expected(Benford's Law)")) +
  labs(title="Density Distribution of Biden and Trump's First Digit Voting Count in Illinois 2020",
       x="First Digit of Voting Count",y="Density") + 
  theme(legend.position="top",legend.title=element_blank()) + 
  facet_grid(.~candidate)

### Problem 2 
overall <- dt %>% filter(digits!="0" & is.na(digits)==F)
ggplot(overall,aes(x=digits)) + 
  geom_bar(aes(y=..prop..,group=1,fill="black"),fill="black") + 
  geom_point(data=benford,aes(x=digits,y=val,color="tomato"),size=3) + 
  geom_line(data=benford,aes(x=digits,y=val,color="tomato"),lwd=1) + 
  scale_color_discrete(labels = c("Expected(Benford's Law)")) +
  labs(title="Density Distribution of Voting Count's First Digit in
       US Presidential Elections Between 2000 and 2020",
       x="First Digit of Voting Count",y="Density") + 
  theme(legend.position="top",legend.title=element_blank())


