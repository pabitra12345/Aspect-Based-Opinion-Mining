#####################################################################################################################################################
####################################################################################################################################################
#################################CODES FOR EXTRACTING SERVICE DIMENSIONS FROM REVIEWS OF MOUTHSHUT AND GOOGLE######################################
####################################################################################################################################################
#####################################################################################################################################################

#####Explanation of logic with input and output files-
##1. Split review to sentences and extract pos tags for each word.
##2. identify number of nouns in each sentence.
##3. For sentence with single noun, extract forward wise. If no relevant tags found, extract backward.
##4. For several nouns, extract forward till next noun. If no results, extract backward.
##5. Repeat for all reviews and extract sentiment scores for each sentence and each noun(sentiment of relevant verb, adverb and adjective combined)

##The files are split into 3 parts-
## 1. servqual_output1_manipal.csv- consisting of all information(reviews,sentence split, pos parts and rating) excluding dimension
## 2. servqual_output2_manipal.csv- output1 + dimension
## 3. servqual_output3_manipal.csv- final file for analysis consisting of reviews with each dimension score and rating to be analyzed further


#java heap memory allocation should be always done before using library rJava
options(java.parameters = "-Xmx4096m")

if (!require("rJava")) install.packages("rJava")
if (!require("NLP")) install.packages("NLP")
if (!require("openNLP")) install.packages("openNLP")
if (!require("textstem")) install.packages("textstem")
if (!require("stringr")) install.packages("stringr")
if (!require("stringi")) install.packages("stringi")
if (!require("SentimentAnalysis")) install.packages("SentimentAnalysis")
if (!require("sentimentr")) install.packages("entimentr")
if (!require("tokenizers")) install.packages("tokenizers")
if (!require("tm")) install.packages("tm")
if (!require("tibble")) install.packages("tibble")
if (!require("quanteda")) install.packages("quanteda")
if (!require("qdap")) install.packages("qdap")
if (!require("wordnet")) install.packages("wordnet")
if (!require("textstem")) install.packages("textstem")


setDict("C:/Program Files (x86)/WordNet/2.1/dict")
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")                  ##set directory for word net dictionary
library(wordnet)



#########################################################
##including aspects after matching it with noun column
#########################################################


########################################
##WITHOUT LEMMATIZATION
########################################
##extracting noun from each row and comparing its synonames with dimension list obtained above(each word/noun is converted to synonames and compared to all words of dimensions)
#importing file from previous step
servqual_withoutlemma_df=read.csv("D:/opinion_mining_test/output/servqual_step1_output_manipal.csv")
servqual_withoutlemma_df[is.na(servqual_withoutlemma_df)]=""                   ##converting all NA values to ""

#importing dimension list containing respective nouns according to dimensions
list_dimension_df=read.csv("D:/opinion_mining_test/input/dimension_list.csv")
#converting df to list 
list_dimension=list(Tangibles=as.character(list_dimension_df[,1]),Reliability=as.character(list_dimension_df[,2]),Responsiveness=as.character(list_dimension_df[,3]),Assurance=as.character(list_dimension_df[,4]),Empathy=as.character(list_dimension_df[,5]))

for(j in 1:nrow(servqual_withoutlemma_df)){
  tryCatch({
    for (i in 1:length(list_dimension$Tangibles)) {
      tryCatch({
        if(list_dimension$Tangibles[i]!=""){
          tangible_test=list_dimension$Tangibles[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(as.character(servqual_withoutlemma_df$Noun[j]),"NOUN"),tangible_test)),"\n"," "),"TRUE")){
            servqual_withoutlemma_df[j,26]=names(list_dimension[1])
            print(names(list_dimension[1]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Reliability)) {
      tryCatch({
        if(list_dimension$Reliability[i]!=""){
          reliability_test=list_dimension$Reliability[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(as.character(servqual_withoutlemma_df$Noun[j]),"NOUN"),reliability_test)),"\n"," "),"TRUE")){
            servqual_withoutlemma_df[j,26]=names(list_dimension[2])
            print(names(list_dimension[2]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Responsiveness)) {
      tryCatch({
        if(list_dimension$Responsiveness[i]!=""){
          responsiveness_test=list_dimension$Responsiveness[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(as.character(servqual_withoutlemma_df$Noun[j]),"NOUN"),responsiveness_test)),"\n"," "),"TRUE")){
            servqual_withoutlemma_df[j,26]=names(list_dimension[3])
            print(names(list_dimension[3]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Assurance)) {
      tryCatch({
        if(list_dimension$Assurance[i]!=""){
          assurance_test=list_dimension$Assurance[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(as.character(servqual_withoutlemma_df$Noun[j]),"NOUN"),assurance_test)),"\n"," "),"TRUE")){
            servqual_withoutlemma_df[j,26]=names(list_dimension[4])
            print(names(list_dimension[4]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Empathy)) {
      tryCatch({
        if(list_dimension$Empathy[i]!=""){
          empathy_test=list_dimension$Empathy[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(as.character(servqual_withoutlemma_df$Noun[j]),"NOUN"),empathy_test)),"\n"," "),"TRUE")){
            servqual_withoutlemma_df[j,26]=names(list_dimension[5])
            print(names(list_dimension[5]))
          }
        }
      },error=function(e){})
    }
  },error=function(e){})
  print(j)
}
servqual_withoutlemma_df[is.na(servqual_withoutlemma_df)]=""
write.csv(servqual_withoutlemma_df,"D:/opinion_mining_test/output/servqual_step2_output_withoutlemma_manipal.csv")


#######################################
##WITH LEMMATIZATION
#######################################
#importing file from previous step
servqual_withlemma_df=read.csv("D:/opinion_mining_test/output/servqual_step1_output_manipal.csv")
servqual_withlemma_df[is.na(servqual_withlemma_df)]=""                   ##converting all NA values to ""

#importing dimension list containing respective nouns according to dimensions
list_dimension_df=read.csv("D:/opinion_mining_test/input/dimension_list.csv")
#converting df to list 
list_dimension=list(Tangibles=as.character(list_dimension_df[,1]),Reliability=as.character(list_dimension_df[,2]),Responsiveness=as.character(list_dimension_df[,3]),Assurance=as.character(list_dimension_df[,4]),Empathy=as.character(list_dimension_df[,5]))

for(j in 1:20){
  tryCatch({
    for (i in 1:length(list_dimension$Tangibles)) {
      tryCatch({
        if(list_dimension$Tangibles[i]!=""){
          tangible_test=list_dimension$Tangibles[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(lemmatize_words(as.character(servqual_withlemma_df$Noun[j])),"NOUN"),tangible_test)),"\n"," "),"TRUE")){
            servqual_withlemma_df[j,26]=names(list_dimension[1])
            print(names(list_dimension[1]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Reliability)) {
      tryCatch({
        if(list_dimension$Reliability[i]!=""){
          reliability_test=list_dimension$Reliability[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(lemmatize_words(as.character(servqual_withlemma_df$Noun[j])),"NOUN"),reliability_test)),"\n"," "),"TRUE")){
            servqual_withlemma_df[j,26]=names(list_dimension[2])
            print(names(list_dimension[2]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Responsiveness)) {
      tryCatch({
        if(list_dimension$Responsiveness[i]!=""){
          responsiveness_test=list_dimension$Responsiveness[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(lemmatize_words(as.character(servqual_withlemma_df$Noun[j])),"NOUN"),responsiveness_test)),"\n"," "),"TRUE")){
            servqual_withlemma_df[j,26]=names(list_dimension[3])
            print(names(list_dimension[3]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Assurance)) {
      tryCatch({
        if(list_dimension$Assurance[i]!=""){
          assurance_test=list_dimension$Assurance[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(lemmatize_words(as.character(servqual_withlemma_df$Noun[j])),"NOUN"),assurance_test)),"\n"," "),"TRUE")){
            servqual_withlemma_df[j,26]=names(list_dimension[4])
            print(names(list_dimension[4]))
          }
        }
      },error=function(e){})
    }
    for (i in 1:length(list_dimension$Empathy)) {
      tryCatch({
        if(list_dimension$Empathy[i]!=""){
          empathy_test=list_dimension$Empathy[i]
          if(str_detect(str_replace_all(as.String(str_detect(synonyms(lemmatize_words(as.character(servqual_withlemma_df$Noun[j])),"NOUN"),empathy_test)),"\n"," "),"TRUE")){
            servqual_withlemma_df[j,26]=names(list_dimension[5])
            print(names(list_dimension[5]))
          }
        }
      },error=function(e){})
    }
  },error=function(e){})
  print(j)
}
df[is.na(df)]=""
write.csv(servqual_withlemma_df,"D:/opinion_mining_test/output/servqual_step2_output_withlemma_manipal.csv")






rm(list=ls())                                   ##clear environment for next operation






