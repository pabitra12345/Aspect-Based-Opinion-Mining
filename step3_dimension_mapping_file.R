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


setDict("C:/Program Files (x86)/WordNet/2.1/dict")
Sys.setenv(WNHOME = "C:/Program Files (x86)/WordNet/2.1")                  ##set directory for word net dictionary
library(wordnet)


##dataframe consisting of service dimensions,sentiment score and rating
final_dimension_df=data.frame(matrix(nrow = 1300,ncol=9))
colnames(final_dimension_df)=c("Review_no","Review","A1:Tangibles","A2:Reliability","A3:REsponsiveness","A4:Assurance","A5:Empathy","Average Sentiment Score","Rating")
head(final_dimension_df)
final_dimension_df[is.na(final_dimension_df)]=""

##importing step1 file of code
dimension_df=read.csv("D:/opinion_mining_test/output/servqual_step2_output_withoutlemma_manipal.csv")
dimension_df[is.na(dimension_df)]=""

##extracting final matrix consisting dimensions in columns with avg rating and reviews. it does not include the last review details
k=1
for(i in 1:nrow(dimension_df))
{
  tryCatch(
    { 
      if(dimension_df$Review[i]!=""){
        dummy_df=dimension_df[which(dimension_df$Review_no==paste("Review",k)):(which(dimension_df$Review_no==paste("Review",k+1))-1),]     ##dummy_df is extracted from past matrix as reference and information is extracted
        final_dimension_df$Review_no[k]=as.character(dummy_df$Review_no[1])
        final_dimension_df$Review[k]=as.character(dummy_df$Review[1])
        final_dimension_df$Rating[k]=dummy_df$Rating[1]
        final_dimension_df$`A1:Tangibles`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Tangibles",]$`Sentiment_pos`))
        final_dimension_df$`A2:Reliability`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Reliability",]$`Sentiment_pos`))
        final_dimension_df$`A3:REsponsiveness`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Responsiveness",]$`Sentiment_pos`))
        final_dimension_df$`A4:Assurance`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Assurance",]$`Sentiment_pos`))
        final_dimension_df$`A5:Empathy`[k]=mean(as.numeric(dummy_df[dummy_df$Dimension=="Empathy",]$`Sentiment_pos`))
        print(k)
        k=k+1
      }
    },error=function(e){}
  )  
}

final_dimension_df[is.na(final_dimension_df)]=""
#removing nan values generated in dataframe
final_dimension_df[final_dimension_df==NaN]=""

view(final_dimension_df)



write.csv(final_dimension_df,"D:/opinion_mining_test/output/servqual_step3_final_output_manipal.csv")
rm(list=ls())                                   ##clear environment for next operation




