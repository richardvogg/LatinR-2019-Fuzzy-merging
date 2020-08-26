library(dplyr)
library(ggplot2)
library(stringdist)
library(tidytext)
library(tictoc)

### Data

#Mini Example
test <- c("hello","hallo","telefon","hello","telephone","telephone")

#Blog Texts
test <- read.table("C:/Richard/R and Python/Datasets/blog.txt",sep="\n") %>%
  unnest_tokens(word,V1) %>% 
  .$word

#Tweets
test <- search_tweets("#TeCreidelPueblo",n=18000,include_rts=F,lang="es") %>% 
  select(text) %>% 
  unnest_tokens(word,text) %>% 
  select(txt=word) %>% 
  tibble() %>% 
  count(txt,sort=T)



remove_duplicates <- function(vec,find_cutoff=FALSE,cutoff_distance=0.06) {
  wordcount <- vec %>%
    tibble(txt=vec) %>% 
    dplyr::count(txt,sort=T)
  
  words <- wordcount$txt
  
  out <- sapply(seq_along(words)[-1],function(i) {
    dist2 <- stringdist(words[i],words[1:i-1],method='jw',p=0.1)
    best_fit <- which.min(dist2)
    similarity2 <- min(dist2)
    return(c(similarity2,best_fit))
  }) %>% 
    t() %>%
    as.data.frame() %>% 
    add_row(V1=1,V2=1,.before = 1) %>% 
    cbind(wordcount) %>% 
    dplyr::rename(distance=V1,best_fit=V2) %>% 
    mutate(replacement=txt[best_fit])
  
  if(find_cutoff==TRUE) {return(out)}
  
  dict <- out %>% 
    mutate(replacement=ifelse(distance<cutoff_distance,replacement,txt)) %>% 
    filter(replacement!=txt)
  
  out_vec <- plyr::mapvalues(vec,from=dict$txt,to=dict$replacement)
  
  return(out_vec)
}



show <- remove_duplicates(test,find_cutoff=T)


#Stringdistance with several distance measures

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
