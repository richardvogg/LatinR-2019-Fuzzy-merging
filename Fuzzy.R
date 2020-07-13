library(dplyr)
library(stringdist)
library(parallel)
library(tictoc)
library(formattable)

names <- c("Haliburton", "ExxonMobile","ABBOTT LABORATORIES","Marrriott","Self","Activision Blizzard",
           "Quest dianotstics","Unemployed","other company","burger king",
           "MARRIOT","wall mart", "Illumin", "3M","NORTHROP TRUMMON","MCCormicks","MARSH MCLEANNON",
           "FLO SERVE", "Kansas City Southern Fed.","MCDONALD'S","F5 Networks",
           "McDonalds","MacKindsey","Oracle","Self-employed","None","Retired",
           "f5 networks","Harley Davidson","Harly Davidson","HARLEY DAVIDSEN","DRHorton","D.R. Horten",
           "cincinati fin","cincinnatti financials","cincinnati financial","CINCINATTI FINANCE",
           "Mohaws Industry","Mowahk Industries","Mohawk Ind")

formattable(data.frame(dirty_list=names))


dirty_list <- sample(names,500000,replace=T)

#Download the list, for example from here: https://datahub.io/core/s-and-p-500-companies
clean_list <- read.csv2("S&P500.csv",stringsAsFactors = F) %>% .$Security
names2 <- clean_list[1:15]
formattable(data.frame(clean_list=names2))

#Preprocessing
cleaner <- function(data) {
  wordremove <- c(" and "," comp "," company","companies"," corp ","corporation"," inc ","[.]com")
  data <- data %>% tolower() %>% 
    {gsub(paste(wordremove,collapse='|'),"",.)} %>%
    {gsub("[[:punct:]]","",.)} %>%
    {gsub("[[:blank:]]","",.)}
  return(data)
}

control <- data.frame(original=dirty_list)

clean_list_cl <- cleaner(clean_list)
dirty_list_cl <- cleaner(dirty_list)

tic("a")
distmatrix <- stringdist::stringdistmatrix(clean_list_cl,dirty_list_cl,method='jw',p=0.1)
best_fit <- apply(distmatrix,2,which.min) %>% as.integer()
similarity <- apply(distmatrix,2,min)

control$best_fit <- clean_list[best_fit]
control$similarity <- similarity 
toc()

stringdistmatrix(c("harleydavidson","cincinattifinancial","questdiagnostics"),
                 c("cincinattifinancial","harleydavison","otherexample","questdiags"),
                 method='jw',p=0.1)

#Parallelize
tic("b")
#Create Cluster
nnodes <- detectCores()-1
cl     <- makePSOCKcluster(nnodes)    


clusterEvalQ(cl, {library(stringdist)})
clusterExport(cl,"dirty_list_cl")
clusterExport(cl,"clean_list_cl")
clusterExport(cl,"nnodes")

# Set seed
clusterSetRNGStream(cl, 123) 


#ans <- parSapply(cl, 1:10, function(x) {
#  stringdistmatrix(clean_list_cl,
#                   dirty_list_cl[((x-1)*round(length(dirty_list_cl)/10)+1):(x*round(length(dirty_list_cl)/10))])
#})


ans <- parSapply(cl, dirty_list_cl, function(x) {
  a <- stringdist(clean_list_cl,x,method='jw',p=0.1)
  best_fit <- which.min(a)
  similarity <- min(a)
  
  return(c(best_fit,similarity))
})


stopCluster(cl)


control2 <- as.data.frame(t(ans))
control2$original <- rownames(control2)
control2$best_fit <- clean_list[as.integer(control2$V1)]
control2$similarity <- control2$V2
control2$V1 <- NULL
control2$V2 <- NULL
rownames(control2) <- NULL
toc()


control$result <- ifelse(control$similarity<=0.12,control$best_fit,NA)
control$distance <- round(control$similarity,3)

control <- control[sample(1:nrow(control),nrow(control)),]
formattable(distinct(control),list(area(col = 3) ~ color_tile("lightblue", "orange")))
