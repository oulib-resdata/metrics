#set working directory to one above scripts (where this file is located).
setwd(dir = "..")  #.. is directory above current one (scripts)

#Import workshop metadata
workshop_metadata <- read.csv(file = "workshop_metadata.csv")

#Import all files
preworkshop_list <- list.files(path = "raw_data/preworkshop",
                               pattern = "csv$",
                               full.names = FALSE)


import_and_clean_data <- function(x) {
  file <- read.csv(paste0("raw_data/preworkshop/",
                          x),
                   header = TRUE)
  file_cleaned <- file[,c("Order.Date",
                          "Classification",
                          "Department",
                          "How.did.you.hear.about.this.workshop.",
                          "Please.describe.",
                          "What.do.you.hope.to.learn.from.this.workshop.")]
  print(file$Order..) #shows which order it may crash on
  print(x) #shows which file
  
  
  ####parse dates and workshop codes
  file_cleaned$filename <- x #put data file name into table
  file_cleaned$date <- strsplit(x,
                                split = "-",
                                fixed = TRUE)[[1]][1] #parse out date
  
  file_cleaned$name <- strsplit(x,
                                split = "-",
                                fixed = TRUE)[[1]][2] #parse out name
  file_cleaned$name <- strsplit(file_cleaned$name,
                                split = ".",
                                fixed = TRUE)[[1]][1] #remove .csv
  return(file_cleaned)
}

list_of_results <- lapply(X=preworkshop_list,
                          FUN = import_and_clean_data)

#concatenate the files
preworkshop_survey_results <- do.call("rbind", list_of_results)

#join results to metadata about workshop names
library(dplyr)

preworkshop_surveys_with_metadata <- left_join(preworkshop_survey_results,
                                               workshop_metadata[workshop_metadata$CurrentlyUsedName==TRUE,],
                                               by = c("name" = "WorkshopCode"))

#Analyze frequency for marketing ("How did you hear about workshop?")
library(ggplot2)
library(gridExtra)
library(ggpubr)
library(RColorBrewer)

#Split marketing column from preworkshop surveys.
marketing <- strsplit(x = as.character(preworkshop_surveys_with_metadata$How.did.you.hear.about.this.workshop.),
         split = "\\|")

marketing_source <- lapply(marketing, FUN = function (x) {
  if (!is.na(x[2])){
  x[2]
    }
  else
    {
      x[[1]]
      }
  }
  )

marketing_source <- trimws(marketing_source, 
                           which = "both")

#Split classfication column from preworkshop surveys
classification <- strsplit(x = as.character(preworkshop_surveys_with_metadata$Classification),
                      split = "\\|")

classification_first <- lapply(classification, FUN = function (x) {
  if (!is.na(x[2])){
    x[2]
  }
  else
  {
    x[[1]]
  }
}
)

classification_first <- trimws(classification_first, 
                           which = "both")
###Rename fields so they match
preworkshop_surveys_with_metadata$marketing_source <- marketing_source
preworkshop_surveys_with_metadata$classification_first <- classification_first

pre_reduced <- data.frame("type" = "Pre-registered",
                          "classification" = preworkshop_surveys_with_metadata$classification_first,
                          "marketing_source" = preworkshop_surveys_with_metadata$marketing_source,
                          "describe" = preworkshop_surveys_with_metadata$Please.describe.)

#correct typos
levels(pre_reduced$describe)[26] <- "previous workshop"

unreg_post_reduced <- data.frame("type" = "Walk-ins",
                           "classification" = postworkshop_surveys_with_metadata[postworkshop_surveys_with_metadata$`*Did you pre-register for this workshop?`=="No",]$`*Classification - Selected Choice`,
                          "marketing_source" = postworkshop_surveys_with_metadata[postworkshop_surveys_with_metadata$`*Did you pre-register for this workshop?`=="No",]$`How did you hear about this workshop? - Selected Choice`,
                          "describe" = postworkshop_surveys_with_metadata[postworkshop_surveys_with_metadata$`*Did you pre-register for this workshop?`=="No",]$`How did you hear about this workshop? - Other (please describe)`)

reduced_marketing_info <- rbind(pre_reduced,
                                unreg_post_reduced)

#make all marketing source and classifications match
reduced_marketing_info$classification <- as.character(reduced_marketing_info$classification)
reduced_marketing_info$marketing_source <- as.character(reduced_marketing_info$marketing_source)

reduced_marketing_info[reduced_marketing_info$classification=="Post Doc/Researcher","classification"] <- "Postdoc/researcher"
reduced_marketing_info[reduced_marketing_info$classification=="Other","classification"] <- "Other Status"

reduced_marketing_info[reduced_marketing_info$marketing_source=="Other (please describe)","marketing_source"] <- "Other"
reduced_marketing_info[reduced_marketing_info$marketing_source=="Email from University Libraries","marketing_source"] <- "Email from OU Libraries"
reduced_marketing_info[reduced_marketing_info$marketing_source=="Email from University Libraries","marketing_source"] <- "Email from OU Libraries"
reduced_marketing_info[reduced_marketing_info$marketing_source=="Department/Program Requirement","marketing_source"] <- "Other"
#provide logical ordering
reduced_marketing_info$classification <- factor(reduced_marketing_info$classification,
                                                levels = c("Other Status",
                                                           "Alumni",
                                                           "Undergraduate Student",
                                                           "Graduate Student",
                                                           "Postdoc/researcher",
                                                           "Staff",
                                                           "Faculty"
                                                           ))
reduced_marketing_info$marketing_source <- factor(reduced_marketing_info$marketing_source,
                                                  levels = c("Email from OU Libraries",
                                                             "Flyer",
                                                             "Colleagues",
                                                             "Supervisor",
                                                             "Other"))



pdf(file = "output_and_figures/marketing.pdf",
    width = 12,
    height = 5)


marketing_breakdown_initial <- ggplot(data = reduced_marketing_info,
       aes(x = classification,
           fill = marketing_source))+
  geom_bar()+
  theme(axis.text.x = element_text(angle = 0,
                                   hjust = 1,
                                   size = 16),
        axis.text.y = element_text(angle = 0,
                                   hjust = 1,
                                   size = 16),
        panel.background = element_blank(),
        legend.text=element_text(size=16),
        strip.text.y = element_text(size = 16),
        axis.title=element_text(size=16))+
  facet_grid(type~.)+
  scale_fill_manual(values = c("#8D1732",
                               "#000000",
                               "#298ac4",
                               "#94c893",
                               "#cccccc"))+
  coord_flip()+
  labs (x = "",
        fill = "")
marketing_breakdown_initial
dev.off() #required for pdf() to work

other_source_Preregistered <- ggwordcloud_analysis_complete (df = reduced_marketing_info[reduced_marketing_info$type=="Pre-registered",],
                                                             min_words = 0,
                                                             max_size = 16,
                                                             column = "describe")

other_source_Walkins <- ggwordcloud_analysis_complete (df = reduced_marketing_info[reduced_marketing_info$type=="Walk-ins",],
                                                       min_words = 0,
                                                       max_size = 16,
                                                       column = "describe")

reduced_marketing_info %>%
  group_by(type) %>%
  summarize(n())

# use ggarrange to lay out figures together
# http://www.sthda.com/english/articles/24-ggpubr-publication-ready-plots/81-ggplot2-easy-way-to-mix-multiple-graphs-on-the-same-page/
ggarrange(other_source_Preregistered,
                    other_source_Walkins, 
                    nrow = 1,
                    ncol = 2)
