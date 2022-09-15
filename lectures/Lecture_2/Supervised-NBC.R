# Learning from Big Data 2022
# This file was developed by Gui Liberali to illustrate a way to implement sentiment and content NBC-based methods

# Installation, based on the data available on the GitHub repo https://github.com/guiliberali/Learning-from-Big-Data-Module-1 
#  - Input files read from the local clone of the GitHub repo (you need to adjust the setwd() line).
#  - Output files saved in a folder next to the local clone of the GitHub repo (you need to create it a folder with the name "output")

# Packets required for subsequent analysis. P_load ensures these will be installed and loa
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tm, openNLP, nnet, dplyr, tidyr, ggplot2, reshape2,latex2exp)

# baseline folder (adapt here)
setwd("/Users/gui/Google Drive/_Teaching/Teach_BigData 2022" )  # local folder of your clone of the Module 1 GitHub repo

#---------------------------
# NBC Functions
#---------------------------
Compute_posterior_sentiment = function(prior, corpus_in , dict_words, p_w_given_c,TOT_DIMENSIONS ){
  output <-  capture.output (word_matrix <- 
                               inspect(DocumentTermMatrix(corpus_in,
                                                          control=list(stemming=FALSE, language = "english", 
                                                                       dictionary=as.character(dict_words)))))  
  
  # Check if there are any relevant words in the review, if there are, treat them. if not, use prior
  if (sum(word_matrix) == 0) {posterior<-prior ; words_ <- c("")} else
  { 
    # Positions in word matrix that have words from this review
    word_matrix_indices <- which(word_matrix>0)
    textual_words_vec   <- colnames(word_matrix)[word_matrix_indices]
    
    # Loop around words found in review 
    WR <- length(word_matrix_indices) ;word_matrix_indices_index=1
    for (word_matrix_indices_index in 1: WR)
    {   
      word <- colnames(word_matrix)[word_matrix_indices[word_matrix_indices_index]]
      p_w_given_c_index <- which(as.character(p_w_given_c$words) == word) 
      
      # Loop around occurrences  | word
      occurrences_current_word=1
      for (occurrences_current_word in 1: word_matrix[1,word_matrix_indices[word_matrix_indices_index]] )
      {  
        # initialize variables
        posterior     <- c(rep(0, TOT_DIMENSIONS)) 
        vec_likelihood<- as.numeric(c(p_w_given_c$pos_likelihood[p_w_given_c_index], p_w_given_c$neg_likelihood[p_w_given_c_index])) 
        
        # positive - this is the first element in the vector
        numerat         <-  prior[1] *  as.numeric(p_w_given_c$pos_likelihood[p_w_given_c_index]) 
        denomin         <-  prior %*% vec_likelihood
        posterior[1]  	<- numerat /  denomin 
        
        # negative - this is the second element in the vector
        numerat         <-  prior[2] *  as.numeric(p_w_given_c$neg_likelihood[p_w_given_c_index])
        denomin         <-  prior %*% vec_likelihood
        posterior[2]  	<- numerat /  denomin 
        
        
        if (sum(posterior)>1.01) { ERROR <- TRUE }
        prior <- posterior 		  	    
      } # close loop around occurrences
    } # close loop around words in this review 
    words_ <-  colnames(word_matrix)[word_matrix_indices] 
  } # close if review has no sent words
  
  return(list(posterior_=posterior, words_=words_) )
}        

Compute_posterior_content = function(prior,  word_matrix, p_w_given_c , BIGRAM, TOT_DIMENSIONS){

  # Check if there are any relevant words in the review, if there are, treat them.If not, use prior
  if (sum(word_matrix) == 0) {posterior<-prior  } else
  { 
    # Positions in word matrix that have words from this review
    word_matrix_indices <- which(word_matrix>0)
    textual_words_vec   <- colnames(word_matrix)[word_matrix_indices]
    
    # Loop around words found in review 
    WR <- length(word_matrix_indices) ;word_matrix_indices_index=1
    for (word_matrix_indices_index in 1: WR)
    {   
      word <- colnames(word_matrix)[word_matrix_indices[word_matrix_indices_index]]
      p_w_given_c_index <- which(as.character(p_w_given_c$words) == word) 
      
      # Loop around occurrences  | word
      occurrences_current_word=1
      for (occurrences_current_word in 1:word_matrix[1,word_matrix_indices[word_matrix_indices_index]])
      {  
        # initialize variables
        posterior     <- c(rep(0, TOT_DIMENSIONS)) 
        vec_likelihood <-as.numeric(c(p_w_given_c$storyline[p_w_given_c_index],
                                      p_w_given_c$acting[p_w_given_c_index],
                                      p_w_given_c$visual[p_w_given_c_index]) ) 
        
        # storyline - this is the first element in the vector
        numerat         <-  prior[1] *  as.numeric(p_w_given_c$storyline[p_w_given_c_index]) 
        denomin         <-  prior %*% vec_likelihood
        posterior[1]  	<-  numerat /  denomin 
        
        # acting - this is the second element in the vector
        numerat         <-  prior[2] *  as.numeric(p_w_given_c$acting[p_w_given_c_index])
        denomin         <-  prior %*% vec_likelihood
        posterior[2]  	<-  numerat /  denomin 
        
        # visual - this is the third element in the vector
        numerat         <-  prior[3] *  as.numeric(p_w_given_c$visual[p_w_given_c_index])
        denomin         <-  prior %*% vec_likelihood
        posterior[3]  	<-  numerat /  denomin 
        
        if (sum(posterior)>1.01) { ERROR <- TRUE }
        prior <- posterior 		  	    
      } # close loop around occurrences
    } # close loop around words in this review 
    
  } # close if review has no sent words
  
  return (posterior_= posterior  )
}   


#---------------------------------------------------------------------------------
# START OF THE MAIN CODE


#---------------------------
# Load Data
#---------------------------
Reviews_Raw <- read.csv('./Big-Data-Module-1/data/reviews/Reviews_tiny.csv') 
Reviews_Raw <- Reviews_Raw %>% 
  select(movie_name,review_code,	reviewer,	review_date, num_eval,
         prob_sentiment,words_in_lexicon_sentiment_and_review, ratio_helpful,	raters,   
         prob_storyline,	prob_acting,	prob_sound_visual,full_text,	processed_text,
         release_date,	first_week_box_office,	MPAA,	studio,	num_theaters 	)

#---------------------------
# Set Parameters
#---------------------------
PRIOR_SENT  = 1/2
PRIOR_TOPIC = 1/3
TOT_REVIEWS = length(Reviews_Raw[,1]) 

#---------------------------
# lexicons
#---------------------------

# training data
dictionary_storyline<-read.csv2("./Big-Data-Module-1/data/lexicons/storyline_33k.txt") 
dictionary_acting <-read.csv2("./Big-Data-Module-1/data/lexicons/acting_33k.txt")
dictionary_visual <-read.csv2("./Big-Data-Module-1/data/lexicons/visual_33k.txt")



# TO DO:  Compute the word likelihoods from 3 content dictionaries (i.e., your training data).  
#         Here, I load a list of 100 words with fake content likelihoods and a list with 
#         100 fake sentiment likelihoods. These are just examples.These 100-word lists
#         are not to be used in your assignment. In your assignment, you are expected to 
#         compute the content likelihoods for all the words in the training data yourself  
likelihoods <- read.csv2("./Big-Data-Module-1/data/lexicons/example_100_fake_likelihood_topic.csv", header=TRUE,
                         sep = ",",quote="\"",dec=".",fill=FALSE) 
likelihoods <- likelihoods[,1:4]
lexicon_content     <- as.character(likelihoods[ ,1] )

# TO DO:  Locate a list of sentiment words that fits your research question. This is available from the literature. 
#         For example, you may want to look just positive and negative (hence two dimensions) or you may want to 
#         look at other sentiment dimensions, such as specific emotions (excitement, fear, etc.).  The list of 100
#         words with fake likelihoods for sentiment used below is not to be used in your assignment
likelihoods_sentim <-read.csv2("./Big-Data-Module-1/data/lexicons/example_100 fake_likelihood_sentiment.csv",header=TRUE,
                               sep=",", quote="\"",dec=".",fill=FALSE)
lexicon_sentiment <-  as.character(likelihoods_sentim$words ) 

#---------------------------
# NBC sentiment analysis loop
#---------------------------

for (review_index in 1:TOT_REVIEWS) {  
  if (round(review_index/100,0) ==review_index/100) { cat("Computing sentiment of review #", review_index, "\n", sep="") } 
  prior_sent     <- c(PRIOR_SENT,1-PRIOR_SENT)   # Reset the prior as each review is looked at separately  
  text_review    <- as.character(Reviews_Raw$processed_text[review_index])

  # 2.2.A Pre-process the review to removepunctuation marks and numbers. 
  #       Note that we are not removing stopwords here (nor elsewhere - a point for improvement)
  corpus_review  <- tm_map(tm_map(VCorpus(VectorSource(text_review)), removePunctuation), removeNumbers)    
  
  # 2.2.B Compute posterior probability the review is positive
  TOT_DIMENSIONS = 2
  sent.results <- Compute_posterior_sentiment(prior = prior_sent, 
                                              corpus_in  = corpus_review,  
                                              dict_words = lexicon_sentiment,
                                              p_w_given_c=likelihoods_sentim, 
                                              TOT_DIMENSIONS)
  words_sent  <- sent.results$words_  
  posterior_sent <- sent.results$posterior_ 
  Reviews_Raw$prob_sentiment[review_index] <- posterior_sent[1]
  Reviews_Raw$words_in_lexicon_sentiment_and_review[review_index] <-paste(words_sent,collapse =" ")
}  


#---------------------------
# NBC content analysis loop
#---------------------------

for (review_index in 1: TOT_REVIEWS) {
  if (round(review_index/100,0) ==review_index/100) { cat("Computing content of review #", review_index, "\n", sep="") } 
  if ( Reviews_Raw$full_text[review_index]!=""){
    text_review   <- as.character(Reviews_Raw$processed_text[review_index])
    
    # 3.3.A Pre-process the review to remove numbers and punctuation marks. 
    #       Note that we are not removing stopwords here (nor elsewhere - a point for improvement)
    corpus_review <- VCorpus(VectorSource(text_review))  # put in corpus format
    output <-capture.output(content_word_matrix <-  
                              inspect(DocumentTermMatrix(corpus_review, 
                                                         control = list(stemming=FALSE,
                                                                        language = "english",
                                                                        removePunctuation=TRUE,
                                                                        removeNumbers=TRUE,
                                                                        dictionary=as.character(lexicon_content)))))
    
    # 3.3.B  Compute posterior probability the review is about each topic  
    TOT_DIMENSIONS = 3
    posterior <- Compute_posterior_content(prior=matrix(PRIOR_TOPIC, ncol=TOT_DIMENSIONS), 
                                           content_word_matrix, 
                                           p_w_given_c=likelihoods,, 
                                           TOT_DIMENSIONS) 
    Reviews_Raw$prob_storyline[review_index]    <- posterior[1]
    Reviews_Raw$prob_acting[review_index]       <- posterior[2]
    Reviews_Raw$prob_sound_visual[review_index] <- posterior[3]
  }   
}  
Processed_reviews <- Reviews_Raw
View(Processed_reviews)

# Saves the updated file, now including the sentiment and content/topic posteriors.
write.csv(Processed_reviews,file="../output/TestProcessed_reviews.csv" , row.names = FALSE )

#---------------------------
# performance: Confusion matrix
#---------------------------
# 3.1 Load judges scores
ground_truth_judges <-read.csv("./Big-Data-Module-1/data/judges/judges.csv") 

# TO DO:  Compare the performance of your NBC implementation (for content) against the judges ground truth 
#         by running your algorithm on the sentences labeled by the judges and comparing your classification
#         against the ground truth. Provide the confusion matrix, the model precision and the accuracy score. 
#         Do not forget to interpret your findings.
 
#---------------------------
# VADER implementation
#---------------------------
library(vader)
for (review_index in 1: TOT_REVIEWS) {
  if (round(review_index/100,0) ==review_index/100) { cat("Computing VADER sentiment of review #", review_index, "\n", sep="") } 
  if ( Reviews_Raw$full_text[review_index]!=""){
    text_review   <- as.character(Reviews_Raw$processed_text[review_index])
    
    # 3.3.A Pre-process the review to remove numbers and punctuation marks. 
    #       Note that we are not removing stopwords here (nor elsewhere - a point for improvement)
    corpus_review <- VCorpus(VectorSource(text_review))  # put in corpus format
    output <-capture.output(content_word_matrix <-  
                              inspect(DocumentTermMatrix(corpus_review, 
                                                         control = list(stemming=FALSE,
                                                                        language = "english",
                                                                        removePunctuation=TRUE,
                                                                        removeNumbers=TRUE,
                                                                        dictionary=as.character(lexicon_content)))))
    
    # get or df cleaned or not, options and look into df
    vader<- get_vader(text_review)
    Reviews_Raw$vader_pos[review_index] <-vader[["pos"]]
  }   
}  
Processed_reviews <- Reviews_Raw
View(Processed_reviews)
write.csv(Processed_reviews,file="../output/VADER_Processed_reviews.csv" , row.names = FALSE )

#---------------------------
# VADER vs. NBC comparison
#---------------------------
# TO DO:  Compare the performance of your NBC implementation (for sentiment) assuming that the VADER
#         classification were the ground truth and then building the confusion matrix and computing 
#         precision and recall. Note that we are now interested in understanding how and how much the two
#         classifications differ (we are not implying that VADER is error-free). We are interested 
#         in uncovering sources of systematic differences that can be attributed to the algorithms or 
#         lexicons. Do interpret your findings.
   