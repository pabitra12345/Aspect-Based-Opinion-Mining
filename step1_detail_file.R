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


#########################################################
################LIST OF FUNCTIONS USED##################
########################################################

#function to return pos tags to all words of each sentence(uses openNLP to find out pos tags form each sentence after splitting review sentencewise)
function_postag=function(review){
  for (l in 1:length(review)) {
    tryCatch(                                                                    ##trycatch used to manipulate error messages(here error is ignored)
      {
        
        string1=as.String(review[l])
        annotate_word=annotate(string1,list(Maxent_Sent_Token_Annotator(),Maxent_Word_Token_Annotator()))
        annotate_pos=annotate(string1,Maxent_POS_Tag_Annotator(),annotate_word)                            ##annotators of openNLP package to extract pos tags 
        subset_word=subset(annotate_pos,type=="word")
        tags_word=sapply(subset_word$features,'[[',"POS")                                           ##extracting pos tags and then arranging it as string with words attached
        s_word_tag=as.String(sprintf("%s%s%s",string1[subset_word],"/",tags_word))
        review[l]=str_replace_all(s_word_tag,"\n"," ")
        
      },error=function(e){}
    )  
  }
  return(review)
}


##function for removing sentences without any noun
function_withoutnoun_senremoval=function(review_split){
  for(m in 1:length(review_split)){ 
    tryCatch(
      {
        if(!str_detect(review_split[m],"/NN")){                   ##removing unwanted sentences(without nouns) will enhance/optimize execution time
          review_split=review_split[-m]
        }
      },error=function(e){}
    )  
  }
  
  return(review_split)
}


###function for removing sentences with 0 sentiment scores
function_0sentiment_senremoval=function(review){
  
  tryCatch({
    suppressWarnings(for(j in 1:length(review_split)){                        ##removing sentences with 0 sentiment scores
      if(round(sentiment(review_split[j])$sentiment,2)==0.00){                ##warning suppression using suppresWarnings(something related to external regressors)
        ##error message-no non-missing arguments to max; returning -Inf
        review_split=review_split[-j]
      }
    })
    return(review)
  } ,error=function(e){}
  )  
}


##function for cleaning the review before pos tagging
function_clean_review=function(review){
  suppressWarnings(
    tryCatch(
      {
        
        review = str_replace_all(review," x-ray "," Xray ")
        review=tolower(review)                                               ##converting review to lower case
        review=str_replace_all(review,"[[:digit:]].[[:digit:]]","")
        review = gsub("[[:digit:]]", "", review)                             ## removing digits from review
        review = str_replace_all(review," \\w "," ")                         ## removing singular words of review
        review = str_replace_all(review,"\\b\\w \\b"," ")                         ## removing singular words of review
        review = str_replace_all(review,"\\b \\w\\b"," ")                         ## removing singular words of review
        review = str_replace_all(review," dr. "," doctor ")                  ## replacing certain words to form meaningful aspects
        review = str_replace_all(review," dr "," doctor ")
        review = str_replace_all(review," cuz "," because ")
        review = str_replace_all(review," bcoz "," because ")
        review = str_replace_all(review," mr. ","")
        review = str_replace_all(review," ms. ","")
        review = str_replace_all(review," don "," dont ")
        
        #review = rm_stopwords(review,stopwords = qdapDictionaries::Top25Words,unlist = FALSE, separate = FALSE)     ##removing stopwords
        return(review)
      },error=function(e){}
    )
  )
}



###final clean including all functions
function_finalclean=function(review){
  tryCatch(
    {
      if(!is.na(review)){
        
        review=function_clean_review(review)           ##cleaning review before pos tagging
        
        review_split=sent_detect(review,endmarks = c("?",".","!","..","...","|"),incomplete.sub = TRUE)       ##splitting review string to sentences
        
        review_split=str_replace_all(review_split,"[[:punct:]]","")             ##replacing commas to null value
        
        #review_split=function_0sentiment_senremoval(review_split)       ##removing sentences with 0 sentiment scores
        
        review_split=function_postag(review_split)      ##applying pos tag function to get word tags in sentence
        
        #review_split=str_remove_all(review_split,"[[:punct:]]/[[:punct:]]")
        review_split=str_replace_all(review_split,"  "," ")
      }
      
      return(review_split)
    },error=function(e){}
  )  
  
}


##################################################################################################################
#####################################################PART 1#######################################################
##################################################################################################################
###################################################################################################################
##############################EXTRACTING NOUN ONE BY ONE(FORWRD AND BACKWARD EXTRACTION)############################
###################################################################################################################

#importing base file(reviews)
servqual=read.csv("D:/opinion_mining_test/input/Sourcefile_manipal_google+mouthshut.csv")
servqual$Text=as.character(servqual$Text)             #converting factor to string

#dummy dataframe with respective colnames
servqual_df=data.frame(matrix(ncol=27))
colnames(servqual_df)=c("Review_no","Review","Source","Organization","Sentence_split","Noun","Noun_plural","Noun_proper","Noun_plural_proper","Verb","Verb_Past","Verb_gerund","Verb_past_participle","Verb_nonsingluar","Verb_singular","Adverb","Adverb_comparative","Adverb_superlative","Adjective","Adjective_comparative","Adjective_superlative","conjunction","Determiner","Sentiment_pos","Dimension","Rating","Sentence_sentiment")
servqual_df[is.na(servqual_df)]=""                      # converting na values to whitespace
head(servqual_df)
j=1                                               #row number incremented after every loop

#######################################################
##Run the code till double hash 
#######################################################
suppressWarnings(for(l in 1108:1108) {
  review=servqual$Text[l]
  
  review_split=function_finalclean(review)                         ##clean the reviews after pos tagging
  
  review_split=function_0sentiment_senremoval(review_split)        ##remove sentences with 0 sentiment scores
  
  review_split=function_withoutnoun_senremoval(review_split)       ##remove sentences without noun in it
  
  #print(review_split)
  
  servqual_df[j,1]=paste("Review",l)                    ##review number in dataframe
  servqual_df[j,2]=as.character(review)                 ##complete review into the dataframe
  
  for(k in 1:length(review_split))
  {
    tryCatch({
      review_split_l=review_split[k]                                               #single sentence after pos tagging to be operated
      noun_count=lengths(str_locate_all(review_split_l,"\\b\\w+/NN\\b"))/2                #number of nouns
      review_len=sapply(strsplit(review_split_l, " "), length)                     #length of review
      
      if(noun_count==1){
        forward=review_split_l
        servqual_df[j,3]=as.character(servqual$Source[l])
        servqual_df[j,4]=as.character(servqual$Organization[l])
        servqual_df[j,5]=as.character(review_split_l)
        servqual_df[j,6]=str_replace(str_extract(forward,"\\b\\w+/NN\\b"),"/NN","")
        servqual_df[j,7]=str_replace(str_extract(forward,"\\b\\w+/NNS\\b"),"/NNS","")
        servqual_df[j,8]=str_replace(str_extract(forward,"\\b\\w+/NNP\\b"),"/NNP","")
        servqual_df[j,9]=str_replace(str_extract(forward,"\\b\\w+/NNPS\\b"),"/NNPS","")
        servqual_df[j,10]=str_replace(str_extract(forward,"\\b\\w+/VB\\b"),"/VB","")
        servqual_df[j,11]=str_replace(str_extract(forward,"\\b\\w+/VBD\\b"),"/VBD","")
        servqual_df[j,12]=str_replace(str_extract(forward,"\\b\\w+/VBG\\b"),"/VBG","")
        servqual_df[j,13]=str_replace(str_extract(forward,"\\b\\w+/VBN\\b"),"/VBN","")
        servqual_df[j,14]=str_replace(str_extract(forward,"\\b\\w+/VBP\\b"),"/VBP","")
        servqual_df[j,15]=str_replace(str_extract(forward,"\\b\\w+/VBZ\\b"),"/VBZ","")
        servqual_df[j,16]=str_replace(str_extract(forward,"\\b\\w+/RB\\b"),"/RB","")
        servqual_df[j,17]=str_replace(str_extract(forward,"\\b\\w+/RBR\\b"),"/RBR","")
        servqual_df[j,18]=str_replace(str_extract(forward,"\\b\\w+/RBS\\b"),"/RBS","")
        servqual_df[j,19]=str_replace(str_extract(forward,"\\b\\w+/JJ\\b"),"/JJ","")
        servqual_df[j,20]=str_replace(str_extract(forward,"\\b\\w+/JJR\\b"),"/JJR","")
        servqual_df[j,21]=str_replace(str_extract(forward,"\\b\\w+/JJS\\b"),"/JJS","")
        servqual_df[j,22]=str_replace(str_extract(forward,"\\b\\w+/CC\\b"),"/CC","")
        servqual_df[j,23]=str_replace(str_extract(forward,"\\b\\w+/DT\\b"),"/DT","")
        servqual_df[j,24]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(servqual_df[j,6:23]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
        servqual_df[j,26]=servqual$Rating[l]
        servqual_df[j,27]=sentiment(review_split[k])$sentiment
        
        
        servqual_df[is.na(servqual_df)]=""
        
        j=j+1
      }
      
      else {
        for(t in 1:noun_count){
          tryCatch({
            
            location_start_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t]
            location_end_1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t,2]
            noun1=str_sub(review_split_l,location_start_1,location_end_1)
            noun_index_1=match(noun1,unlist(str_split(review_split_l," ")))
            
            if(noun_index_1!=review_len){
              location_start_2=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t+1]
              location_end_2=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t+1,2]
              noun2=str_sub(review_split_l,location_start_2,location_end_2)
              noun_index_2=match(noun2,unlist(str_split(review_split_l," ")))
              
              if((noun_index_1+1)!=noun_index_2){
                
                forward=word(review_split_l,noun_index_1+1,noun_index_2-1)
                servqual_df[j,3]=as.character(servqual$Source[l])
                servqual_df[j,4]=as.character(servqual$Organization[l])
                servqual_df[j,5]=as.character(review_split_l)
                servqual_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
                servqual_df[j,7]=str_replace(str_extract(forward,"\\b\\w+/NNS\\b"),"/NNS","")
                servqual_df[j,8]=str_replace(str_extract(forward,"\\b\\w+/NNP\\b"),"/NNP","")
                servqual_df[j,9]=str_replace(str_extract(forward,"\\b\\w+/NNPS\\b"),"/NNPS","")
                servqual_df[j,10]=str_replace(str_extract(forward,"\\b\\w+/VB\\b"),"/VB","")
                servqual_df[j,11]=str_replace(str_extract(forward,"\\b\\w+/VBD\\b"),"/VBD","")
                servqual_df[j,12]=str_replace(str_extract(forward,"\\b\\w+/VBG\\b"),"/VBG","")
                servqual_df[j,13]=str_replace(str_extract(forward,"\\b\\w+/VBN\\b"),"/VBN","")
                servqual_df[j,14]=str_replace(str_extract(forward,"\\b\\w+/VBP\\b"),"/VBP","")
                servqual_df[j,15]=str_replace(str_extract(forward,"\\b\\w+/VBZ\\b"),"/VBZ","")
                servqual_df[j,16]=str_replace(str_extract(forward,"\\b\\w+/RB\\b"),"/RB","")
                servqual_df[j,17]=str_replace(str_extract(forward,"\\b\\w+/RBR\\b"),"/RBR","")
                servqual_df[j,18]=str_replace(str_extract(forward,"\\b\\w+/RBS\\b"),"/RBS","")
                servqual_df[j,19]=str_replace(str_extract(forward,"\\b\\w+/JJ\\b"),"/JJ","")
                servqual_df[j,20]=str_replace(str_extract(forward,"\\b\\w+/JJR\\b"),"/JJR","")
                servqual_df[j,21]=str_replace(str_extract(forward,"\\b\\w+/JJS\\b"),"/JJS","")
                servqual_df[j,22]=str_replace(str_extract(forward,"\\b\\w+/CC\\b"),"/CC","")
                servqual_df[j,23]=str_replace(str_extract(forward,"\\b\\w+/DT\\b"),"/DT","")
                servqual_df[j,24]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(servqual_df[j,6:23]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
                servqual_df[j,26]=servqual$Rating[l]
                servqual_df[j,27]=sentiment(review_split[k])$sentiment
                
                servqual_df[is.na(servqual_df)]=""
                
                if(vapply(lapply(strsplit(str_replace_all(as.String(servqual_df[j,7:23]),"\n"," "), " "), unique), paste, character(1L), collapse = " ")==""){
                  location_minus=str_locate_all(review_split_l,"\\b\\w+/NN\\b")
                  location_start_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1]
                  location_end_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1,2]
                  noun1_back=str_sub(review_split_l,location_start_minus1,location_end_minus1)
                  noun1_index_1minus=match(noun1_back,unlist(str_split(review_split_l," ")))
                  
                  if(t==1){
                    backward=word(review_split_l,1,noun_index_1-1)
                  }else {
                    backward=word(review_split_l,noun1_index_1minus+1,noun_index_1-1)
                  }
                  
                  servqual_df[j,3]=as.character(servqual$Source[l])
                  servqual_df[j,4]=as.character(servqual$Organization[l])
                  servqual_df[j,5]=as.character(review_split_l)
                  servqual_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
                  servqual_df[j,7]=str_replace(str_extract(backward,"\\b\\w+/NNS\\b"),"/NNS","")
                  servqual_df[j,8]=str_replace(str_extract(backward,"\\b\\w+/NNP\\b"),"/NNP","")
                  servqual_df[j,9]=str_replace(str_extract(backward,"\\b\\w+/NNPS\\b"),"/NNPS","")
                  servqual_df[j,10]=str_replace(str_extract(backward,"\\b\\w+/VB\\b"),"/VB","")
                  servqual_df[j,11]=str_replace(str_extract(backward,"\\b\\w+/VBD\\b"),"/VBD","")
                  servqual_df[j,12]=str_replace(str_extract(backward,"\\b\\w+/VBG\\b"),"/VBG","")
                  servqual_df[j,13]=str_replace(str_extract(backward,"\\b\\w+/VBN\\b"),"/VBN","")
                  servqual_df[j,14]=str_replace(str_extract(backward,"\\b\\w+/VBP\\b"),"/VBP","")
                  servqual_df[j,15]=str_replace(str_extract(backward,"\\b\\w+/VBZ\\b"),"/VBZ","")
                  servqual_df[j,16]=str_replace(str_extract(backward,"\\b\\w+/RB\\b"),"/RB","")
                  servqual_df[j,17]=str_replace(str_extract(backward,"\\b\\w+/RBR\\b"),"/RBR","")
                  servqual_df[j,18]=str_replace(str_extract(backward,"\\b\\w+/RBS\\b"),"/RBS","")
                  servqual_df[j,19]=str_replace(str_extract(backward,"\\b\\w+/JJ\\b"),"/JJ","")
                  servqual_df[j,20]=str_replace(str_extract(backward,"\\b\\w+/JJR\\b"),"/JJR","")
                  servqual_df[j,21]=str_replace(str_extract(backward,"\\b\\w+/JJS\\b"),"/JJS","")
                  servqual_df[j,22]=str_replace(str_extract(backward,"\\b\\w+/CC\\b"),"/CC","")
                  servqual_df[j,23]=str_replace(str_extract(backward,"\\b\\w+/DT\\b"),"/DT","")
                  servqual_df[j,24]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(servqual_df[j,7:23]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
                  servqual_df[j,26]=servqual$Rating[l]
                  servqual_df[j,27]=sentiment(review_split[k])$sentiment
                  
                  servqual_df[is.na(servqual_df)]=""
                  
                  
                }
                j=j+1
                
              }
              
              servqual_df[is.na(servqual_df)]=""
              
            }
            else{
              location_minus=str_locate_all(review_split_l,"\\b\\w+/NN\\b")
              location_start_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1]
              location_end_minus1=str_locate_all(review_split_l,"\\b\\w+/NN\\b")[[1]][t-1,2]
              noun1_back=str_sub(review_split_l,location_start_minus1,location_end_minus1)
              noun1_index_1minus=match(noun1_back,unlist(str_split(review_split_l," ")))
              
              backward=word(review_split_l,noun1_index_1minus+1,noun_index_1-1)
              servqual_df[j,3]=as.character(servqual$Source[l])
              servqual_df[j,4]=as.character(servqual$Organization[l])
              servqual_df[j,5]=as.character(review_split_l)
              servqual_df[j,6]=str_replace(str_extract(noun1,"\\b\\w+/NN\\b"),"/NN","")
              servqual_df[j,7]=str_replace(str_extract(backward,"\\b\\w+/NNS\\b"),"/NNS","")
              servqual_df[j,8]=str_replace(str_extract(backward,"\\b\\w+/NNP\\b"),"/NNP","")
              servqual_df[j,9]=str_replace(str_extract(backward,"\\b\\w+/NNPS\\b"),"/NNPS","")
              servqual_df[j,10]=str_replace(str_extract(backward,"\\b\\w+/VB\\b"),"/VB","")
              servqual_df[j,11]=str_replace(str_extract(backward,"\\b\\w+/VBD\\b"),"/VBD","")
              servqual_df[j,12]=str_replace(str_extract(backward,"\\b\\w+/VBG\\b"),"/VBG","")
              servqual_df[j,13]=str_replace(str_extract(backward,"\\b\\w+/VBN\\b"),"/VBN","")
              servqual_df[j,14]=str_replace(str_extract(backward,"\\b\\w+/VBP\\b"),"/VBP","")
              servqual_df[j,15]=str_replace(str_extract(backward,"\\b\\w+/VBZ\\b"),"/VBZ","")
              servqual_df[j,16]=str_replace(str_extract(backward,"\\b\\w+/RB\\b"),"/RB","")
              servqual_df[j,17]=str_replace(str_extract(backward,"\\b\\w+/RBR\\b"),"/RBR","")
              servqual_df[j,18]=str_replace(str_extract(backward,"\\b\\w+/RBS\\b"),"/RBS","")
              servqual_df[j,19]=str_replace(str_extract(backward,"\\b\\w+/JJ\\b"),"/JJ","")
              servqual_df[j,20]=str_replace(str_extract(backward,"\\b\\w+/JJR\\b"),"/JJR","")
              servqual_df[j,21]=str_replace(str_extract(backward,"\\b\\w+/JJS\\b"),"/JJS","")
              servqual_df[j,22]=str_replace(str_extract(backward,"\\b\\w+/CC\\b"),"/CC","")
              servqual_df[j,23]=str_replace(str_extract(backward,"\\b\\w+/DT\\b"),"/DT","")
              servqual_df[j,24]=sentiment(vapply(lapply(strsplit(str_replace_all(as.String(servqual_df[j,6:23]),"\n"," "), " "), unique), paste, character(1L), collapse = " "))$sentiment      
              servqual_df[j,26]=servqual$Rating[l]
              servqual_df[j,27]=sentiment(review_split[k])$sentiment
              
              servqual_df[is.na(servqual_df)]=""
              
              j=j+1
              
              
            }
            
          }, error=function(e){})
        }
        
      }
    }, error=function(e){})
  }
  print(l)
})

###############################################################
###############################################################


write.csv(servqual_df,"D:/opinion_mining_test/output/servqual_step1_output_manipal.csv")
rm(list=ls()) 
