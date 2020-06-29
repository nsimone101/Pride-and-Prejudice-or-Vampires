#'Nick Simone
#'Math 4870
#'4-25-19

#Project 3
#setwd("Project 3")

library(caret)
library(dplyr)

###################################################################
#Read in data
###################################################################
dat <- read.csv("train_PPOV.csv")
test_final<- read.csv("test_PPOV.csv")

#Get number of sentences in Dracula
numVampSent<-max(dat %>% select(sentence,vampires) %>% filter(vampires==1))
#Get number of sentences in PP
numPPSent<-max(dat %>% select(sentence,vampires) %>% filter(vampires==0))

#Get min and max of the two
minSent<-min(numVampSent,numPPSent)
maxSent<-max(numVampSent,numPPSent)


###################################################################
#Cross validate to find best alpha
###################################################################
alphaArray<-seq(0, 1, by=0.05)
logLossArray<-c()
minAlpha<-0
bestLLMetric<-0

for(a in alphaArray){
  #Split dat into train and test data
  train_indices <- sample(1:minSent, floor(minSent*0.8))
  test_indices <- (1:minSent)[-train_indices]
  test <- dat %>% filter(sentence %in% test_indices) 
  train <- dat %>% filter(sentence %in% train_indices)
  
  #Renumber test so all sentence numbers are unique
  test<-test%>%mutate(sentence=ifelse((vampires==1),sentence,sentence+maxSent))
  
  #Record vampires column and remove from test
  actual<-(test%>%distinct(sentence,vampires))$vampires
  test<-test%>%select(-one_of("vampires"))
  
  #Build Model for Train
  wordVamp <- train %>% filter(vampires==1) %>% count(word)
  wordPP <- train %>% filter(vampires==0) %>% count(word)
  
  numWordsVamp<-sum(wordVamp%>%select(n))
  numWordsPP<-sum(wordPP%>%select(n))
  
  wordVamp<-wordVamp%>%mutate(p=(n+a)/(numWordsVamp+a))
  wordPP<-wordPP%>%mutate(p=(n+a)/(numWordsPP+a))
  
  wordVamp<-wordVamp%>%select(-one_of("n"))
  wordPP<-wordPP%>%select(-one_of("n"))
  
  #Loop through test sentences
  probVamp<-c()
  for(i in (test%>%distinct(sentence))$sentence){
    #Split sentence into word array
    s<-(test %>% filter(sentence == i) %>% select(word))$word
    
    logProbVamp<-0
    logProbPP<-0
    for(j in s){
      pV<-a/(numWordsVamp+a)
      pPP<-a/(numWordsPP+a)
      
      if(length((wordVamp%>%filter(word==j))$word)>0){
        pV<-(wordVamp%>%filter(word==j))$p
      }
      if(length((wordPP%>%filter(word==j))$word)>0){
        pPP<-(wordPP%>%filter(word==j))$p
      }
      
      logProbVamp<-logProbVamp+log(pV)
      logProbPP<-logProbPP+log(pPP)
    }
    
    if(exp(logProbPP)==0){
      sentencePred=0.999
    }else if(exp(logProbVamp)==0){
      sentencePred=0.001
    }else{
      sentencePred<-exp(logProbVamp)/(exp(logProbVamp)+exp(logProbPP))
    }
    
    probVamp<-c(probVamp,sentencePred)
  }
  
  logLossMetric<-Metrics::logLoss(actual,probVamp)
  logLossArray<-c(logLossArray,logLossMetric)
  
  if(logLossMetric == min(logLossArray)){
    minAlpha<-a
    bestLLMetric<-logLossMetric
  }
  
  print(paste(a,minAlpha,bestLLMetric))
}


###################################################################
#Use best alpha to run Test with all data
###################################################################
alpha <- minAlpha
wordVamp <- dat %>% filter(vampires==1) %>% count(word)
wordPP <- dat %>% filter(vampires==0) %>% count(word)

numWordsVamp<-sum(wordVamp%>%select(n))
numWordsPP<-sum(wordPP%>%select(n))

wordVamp<-wordVamp%>%mutate(p=(n+alpha)/(numWordsVamp+alpha))
wordPP<-wordPP%>%mutate(p=(n+alpha)/(numWordsPP+alpha))

wordVamp<-wordVamp%>%select(-one_of("n"))
wordPP<-wordPP%>%select(-one_of("n"))

probVamp<-c()

for(i in (test_final%>%distinct(sentence))$sentence){
  #Split sentence into word array
  s<-(test_final %>% filter(sentence == i) %>% select(word))$word
  
  logProbVamp<-0
  logProbPP<-0
  for(j in s){
    pV<-alpha/(numWordsVamp+alpha)
    pPP<-alpha/(numWordsPP+alpha)
    
    if(length((wordVamp%>%filter(word==j))$word)>0){
      pV<-(wordVamp%>%filter(word==j))$p
    }
    if(length((wordPP%>%filter(word==j))$word)>0){
      pPP<-(wordPP%>%filter(word==j))$p
    }
    
    logProbVamp<-logProbVamp+log(pV)
    logProbPP<-logProbPP+log(pPP)
  }
  
  if(exp(logProbPP)==0){
    sentencePred=0.999
  }else if(exp(logProbVamp)==0){
    sentencePred=0.001
  }else{
    sentencePred<-exp(logProbVamp)/(exp(logProbVamp)+exp(logProbPP))
  }
  
  probVamp<-c(probVamp,sentencePred)
}

#Predict
submit <- data.frame(Id=(test_final%>%distinct(sentence))$sentence, prob_vampires = probVamp)
write.csv(submit,file=paste("PPOV_submission_",format(Sys.time(), "%Y-%m-%d_%H-%M-%S"),".csv",sep=""),row.names = FALSE)
