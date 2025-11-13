#Post-workshop survey analysis
#contact cmcurry@ou.edu

#load libraries
library(dplyr)
library(stringr)
library(tidytext)
library(ggplot2)
library(gridExtra)
library(ggwordcloud)


#set working directory to source file (script location)
#THEN...
#set working directory to one above scripts (where this file is located).
setwd(dir = "..")  #.. is directory above current one (scripts)

#Import workshop metadata
workshop_metadata <- read.csv(file = "workshop_metadata.csv")
#Need to clean up titles so they match Qualtrics fields

#Import the latest qualtrics exported survey results file.
postworkshop_survey_results <- read.csv(file = "raw_data/DAVIS @ Libraries Workshops_June 26, 2019_08.40.csv",
                                        header = TRUE,
                                        skip = 2) #remove first two lines so they don't interfere with actual data

#get question headers
#make sure filename matches previous in case questions have changed
postworkshop_survey_results_headers <- as.matrix(read.csv(file = "raw_data/DAVIS @ Libraries Workshops_June 26, 2019_08.40.csv",
                                        header = FALSE)[2,])

names(postworkshop_survey_results) <- postworkshop_survey_results_headers

#join results to metadata about workshop names
postworkshop_surveys_with_metadata0 <- left_join(postworkshop_survey_results,
                                               workshop_metadata,
                                               by = c(`*What workshop did you attend?` = "WorkshopNameAsInQualtrics"))

#Only use surveys where people finished it.
postworkshop_surveys_with_metadata <- postworkshop_surveys_with_metadata0 %>% filter(Finished == T)

#Create function to do consistently formatted wordclouds
ggwordcloud_analysis_complete <- function(df,
                                          column, 
                                          min_words = 1,
                                          ggtitle = "",
                                          max_size = 5){
  df[,column] <- as.character(df[,column])
  tidy_what_is_new <- df %>%
    unnest_tokens(output = word, 
                  input = !! column)
  
  cleaned_what_is_new <- tidy_what_is_new %>%
    anti_join(get_stopwords())
  
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
    theme(plot.margin = unit(c(1,1,1,1), "cm")) +
    ggtitle(ggtitle)+
    scale_color_gradient(low = "bisque4", high = "darkred")
  return(ggcloud)
}


#Now generate figures.
#Analyze frequency for classification
pdf(file = "output_and_figures/current_audience_actually_attended.pdf",
    width = 10,
    height = 8)
ggplot(data = postworkshop_surveys_with_metadata,
       aes(x = `*Classification - Selected Choice`))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x="Classification")

dev.off() #required for pdf() to work

#Analyze skill ratings
#Order the levels
postworkshop_surveys_with_metadata$`How do you rate your skills after learning about this workshop topic?` <- 
  factor(postworkshop_surveys_with_metadata$`How do you rate your skills after learning about this workshop topic?`,
       levels = c("",
                  "Need more support and assistance if implementing",
                  "Can complete a related task independently but not to desired level",
                  "Can complete a related task independently to desired level",
                  "Can complete and expand on task, possibly assist others"))
levels(postworkshop_surveys_with_metadata$`How do you rate your skills after learning about this workshop topic?`)[1] <- "Not applicable"

pdf(file = "output_and_figures/skill_ratings.pdf",
    width = 10,
    height = 5)
ggplot(data = postworkshop_surveys_with_metadata,
       aes(x = `How do you rate your skills after learning about this workshop topic?`,
           fill = `How do you rate your skills after learning about this workshop topic?`))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0,
                                   hjust = 1),
        axis.text.y = element_text(angle = 0,
                                   hjust = 1,
                                   size = 16),
        panel.background = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values=c("#cccccc",
                             "#cccccc",
                             "#cccccc",
                             "#8D1732",
                             "#8D1732"))+
  coord_flip()+
  geom_vline(xintercept = 3.5)+
  geom_vline(xintercept = 1.5)+
  # geom_text(x=2.5, y=12,
  #           label="More support required")+
  # geom_text(x=5, y=12,
  #           hjust = "center",
  #           label="Success in expanding
  #           research skills!")+
  labs(y = "Count of attendees",
       x = "")
dev.off() #required for pdf() to work


# #Analyze learning new things
# pdf(file = "output_and_figures/did_you_learn_something_new.pdf",
#     width = 20,
#     height = 8)
# ggplot(data = postworkshop_surveys_with_metadata,
#        aes(x = `Did you learn something new today? - Selected Choice`))+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
#   theme_classic()
# dev.off() #required for pdf() to work

#Analyze new knowledge and useful for research
#later if we get any no's add this in
# yesno <- ggplot(data = postworkshop_surveys_with_metadata,
#                 aes(x = `Do you anticipate that this new knowledge can be applied towards your research? - Selected Choice`))+
#   geom_bar()+
#   theme(axis.text.x = element_text(angle = 90, hjust = 1))+
# #   theme_classic()
# pdf(file = "output_and_figures/useful_for_your_research.pdf",
#     width = 10,
#     height = 8)

learn_something_new_cloud <-   ggwordcloud_analysis_complete (df = postworkshop_surveys_with_metadata,
                                                                 column = "Did you learn something new today? - Yes, and it was... - Text",
                                                              max_size = 10)

apply_towards_research_cloud <-   ggwordcloud_analysis_complete (df = postworkshop_surveys_with_metadata,
                               column = "Do you anticipate that this new knowledge can be applied towards your research? - Yes, because... - Text",
                               max_size = 10)


grid.arrange(learn_something_new_cloud,
             apply_towards_research_cloud, 
             nrow = 1,
             ncol = 2)
dev.off() #required for pdf() to work


#Analyze useful for study
pdf(file = "output_and_figures/was_it_valuable_for_program_of_study.pdf",
    width = 10,
    height = 8)
ggplot(data = postworkshop_surveys_with_metadata,
       aes(x = `*This workshop was... - ...valuable towards your program of study.`))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off() #required for pdf() to work


#Analyze useful for career
pdf(file = "output_and_figures/was_it_valuable_for_career.pdf",
    width = 10,
    height = 8)
ggplot(data = postworkshop_surveys_with_metadata,
       aes(x = `*This workshop was... - ...valuable towards your career.`))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  theme_classic()
dev.off() #required for pdf() to work


#Analyze useful for teaching
pdf(file = "output_and_figures/was_it_valuable_for_teaching.pdf",
    width = 10,
    height = 8)
ggplot(data = postworkshop_surveys_with_metadata,
       aes(x = `This workshop was... - ...valuable towards your teaching.`))+
  geom_bar()+
  ggtitle("")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
dev.off() #required for pdf() to work

#Free responses
liked <- ggwordcloud_analysis_complete (df = postworkshop_surveys_with_metadata,
                               min_words = 2,
                               ggtitle = "Liked",
                               max_size = 6,
                               column = "Name one aspect of the workshop that you liked.")
improvement <- ggwordcloud_analysis_complete (df = postworkshop_surveys_with_metadata,
                               min_words = 1,
                               max_size = 6,
                               ggtitle = "Improvements",
                               column = "Name one aspect of the workshop that could be improved.")
othercomments <- ggwordcloud_analysis_complete (df = postworkshop_surveys_with_metadata,
                               min_words = 1,
                               max_size = 6,
                               ggtitle = "Other comments",
                               column = "Please let us know any other comments you have about the workshop.")

grid.arrange(liked,
             improvement, 
             othercomments,
             nrow = 1,
             ncol = 3)


#count of responses and topics
postworkshop_surveys_with_metadata %>%
#group_by(WorkshopCode)%>%
summarize(howmany = n())
