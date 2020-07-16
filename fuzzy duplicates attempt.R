library(dplyr)
library(ggplot2)
library(stringdist)
library(tidytext)
library(tictoc)

### Data

#Mini Example
test <- tibble(txt=c("hello","hallo","telefon","telephone"),best_fit=NA,similarity=1)

#Blog Texts
test <- read.table("C:/Richard/R and Python/Datasets/blog.txt",sep="\n") %>%
  unnest_tokens(word,V1) %>% 
  select(txt=word) %>% 
  tibble() %>% 
  count(txt,sort=T) %>% 
  mutate(best_fit=NA,similarity=1)

#Tweets
test <- search_tweets("#TeCreidelPueblo",n=18000,include_rts=F,lang="es") %>% 
  select(text) %>% 
  unnest_tokens(word,text) %>% 
  select(txt=word) %>% 
  tibble() %>% 
  count(txt,sort=T)

#Reset the dataset
test <- test %>% mutate(best_fit=NA,similarity1=100,similarity2=1)
test$replacement <- NULL

#Stringdistance

lapply(seq_along(test$txt)[-1],function(i) {
  dist1 <- stringdist(test$txt[i],test$txt[1:i-1],method='lv')
  dist2 <- stringdist(test$txt[i],test$txt[1:i-1],method='jw',p=0.1)
  test$best_fit[i] <- which.min(dist2)
  test$similarity1[i] <- min(dist1)
  test$similarity2[i] <- min(dist2)
})

out <- sapply(seq_along(test$txt)[-1],function(i) {
  dist2 <- stringdist(test$txt[i],test$txt[1:i-1],method='jw',p=0.1)
  best_fit <- which.min(dist2)
  similarity2 <- min(dist2)
  similarity1 <- stringdist(test$txt[i],test$txt[best_fit])
  return(c(similarity1,similarity2,best_fit))
})

as.data.frame(t(out)) %>% View()

test$replacement <- ifelse(test$similarity1<3 & test$similarity2<0.06,test$txt[test$best_fit],test$txt)
