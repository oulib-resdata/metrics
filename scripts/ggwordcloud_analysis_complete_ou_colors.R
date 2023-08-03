########## CUSTOM FUNCTION
#####################
#Create function to do consistently formatted wordclouds
library(dplyr)
library(ggplot2)
library(ggpubr)
library(ggwordcloud)
library(stopwords)
library(stringr)
library(tidytext)




ggwordcloud_analysis_complete <- function(df,
                                          column, 
                                          min_words = 1,
                                          ggtitle = "",
                                          max_size = 5,
                                          border_margins = 1){
  df[,column] <- as.character(df[,column])
  tidy_what_is_new <- df %>%
    unnest_tokens(output = word, 
                  input = !! column)
  
  cleaned_what_is_new <- tidy_what_is_new %>%
    anti_join(get_stopwords())
  
  # TODO detect two-word phrases
  # https://stackoverflow.com/questions/36479780/making-a-wordcloud-but-with-combined-words
  
  ggcloud <- cleaned_what_is_new %>%
    count(word) %>%
    filter(n>min_words) %>%
    mutate(angle = 90 * sample(c(0, 1), n(),
                               replace = TRUE, 
                               prob = c(90, 10))) %>% #create angle
    ggplot(aes(label = word,
               size = n,
               angle = angle,
               color = n)) +
    geom_text_wordcloud() +
    scale_size_area(max_size = max_size) +
    theme_minimal()+
    theme(plot.margin = unit(c(border_margins,border_margins,border_margins,border_margins), "cm")) +
    ggtitle(ggtitle)+
    scale_color_gradient(low = "bisque4", high = "darkred")
  return(ggcloud)
}