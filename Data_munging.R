# Data for this report are gathered from three sources: pre-registration questionnaire when people register, attendance numbers entered by workshop instructors in LibInsights, and post-workshop surveys.  Registration numbers include learner names and emails and so all three datasets are kept on a private OSF repository and must be downloaded there.  Do not include these in github.

###########
# Load data from OSF
###########
# Authenticate
# Your OSF PAT should be in .REnviron file following these instructions:
# https://docs.ropensci.org/osfr/reference/osf_auth.html


## Retrieve project
osf_node <- osf_retrieve_node("qjkvf")

## List files in project
osf_files <- osf_ls_files(osf_node)

## Download the files
osf_download(x = osf_files,
             # CHOOSE IF RUNNING FROM RMD FILE
             #             path = "..", #default save to directory above scripts
             # CHOOSE IF RUNNING FROM R CONSOLE
             path = NULL, #default save to local working directory
             recurse = TRUE, #download all nested files
             conflicts = "overwrite", #OSF is the canonical version
             progress = TRUE #show progress bar
)



###########
# Registration data has to be imported per-room from LibCal.
###########
# Event registration 
Room339 <- read.csv("./raw_data/preworkshop/Room339_lc_events_20230105043223.csv")
RoomLL123 <- read.csv("./raw_data/preworkshop/LL123_lc_events_20230105043507.csv")
RoomGeneral <- read.csv("./raw_data/preworkshop/General_lc_events_20230105043258.csv")
RoomLearningLabClassroom <- read.csv("./raw_data/preworkshop/LearningLabClassroom_lc_events_20230105043409.csv")

# Merge
Registered <- rbind(Room339,
                    RoomLL123,
                    RoomGeneral,
                    RoomLearningLabClassroom
)


###########
# Attendance data is created by combining Course Instruction attendance data from LibInsights and Outreach Instruction attendance data from LibInsights.  It can be filtered by columns such as date, year, month, instructor, and workshop topic.
###########

# Analysis requested by JD 2023/08/03 only 2021-2023.
last_two_years_attendance <- read.csv("./raw_data/postworkshop/WorkshopAttendanceJD.csv")
last_two_years_attendance$Other <- as.numeric(last_two_years_attendance$Other)
# Preliminary analysis requested by CMC 2023/summer (does not include course-integrated workshops)
# not split into academic year and includes all years.
# has "known three week column", dont' care about this anymore.
# has "topic" manually instead of automated from metadata csv.
# Location of outreach from actual location to just "in person" for sorting.
# Add to LibInsights for both course and outreach - emailed BN 11/28/2023 to talk to MB.
## multiple-day (series) yes or no.
## Requested yes/no is a column. - needs added into LibInsights. (for courses, always yes can add this in in R after import before combine)
## Name of workshop into LibInsights in both courses and outreach.
## This is outreach + courses combined with the manual cleaning listed above in comments.
## TODO: retroactively apply the workshop name field in course and outreach so data can be pulled automatically without manual formatting for future.

workshop_raw <- read.csv ("./raw_data/postworkshop/WorkshopAttendanceUpdate.csv",
                          header=TRUE,
                          stringsAsFactors=FALSE)

#TEMPORARY JOIN to pull "is.course" data from JD dataset to full dataset. TODO: add a step to input course and outreach data, add column at the import step automatically, then concatenate (row bind in dplyr)
workshop <- dplyr::full_join(workshop_raw,
                             last_two_years_attendance)
# full join is needed for temporary because one dataset contains courses and other contains more past scheduled workshops




###########
# Post-workshop survey data are from a subset of learners who attended and also filled out a survey.  In a few cases we forget to adminster the survey or there's not enough time, or individuals opt out even when physically present in the room.
###########

# Post-workshop surveys from libwizard only
## Used for both feedback AND marketing
FeedbackActuallyAttended <- read.csv("./raw_data/postworkshop/report.csv",
                                     na.strings=c("","NA"))


FeedbackActuallyAttended2 <- FeedbackActuallyAttended %>%
  separate_longer_delim(This.workshop.was...,
                        delim = ", ...") %>%
  separate_wider_delim(cols = This.workshop.was...,
                       delim = ". - ",
                       names = c("This_workshop_was",
                                 "Ranking"),
                       too_few = "align_start")

FeedbackActuallyAttended2$This_workshop_was <-
  str_remove(FeedbackActuallyAttended2$This_workshop_was,
             pattern = fixed("..."))

postworkshopsurveys_wide <- FeedbackActuallyAttended2 %>%
  pivot_wider(names_from = This_workshop_was,
              values_from = Ranking) %>%
  dplyr::select(-`NA`)



#rows <- as.numeric(count(postworkshopsurvey))

#postworkshopsurvey <- na.omit(postworkshopsurvey)




###########
# Changes in categories recorded over time have resulted in the need for some automated clean up code, which allows us to look at older categories if we need to, but use the updated equivalents for this report.
###########

# Creating and splitting current levels for marketing
marketing_current_categories <- c("Announcement in Class by Instructor",
                                  "Announcement in Class by Librarian",
                                  "Non-Library Calendar or Newsletter",
                                  "Another Libraries Workshop",
                                  "Walk-in",
                                  "Digital Signage in Non-Library Building",
                                  "Digital Signage in a Library Building",
                                  "Social Media - Other",
                                  "Social Media - Instagram",
                                  "Social Media - Facebook",
                                  "Social Media - Twitter",
                                  "Personal recommendation from OU Librarian",
                                  "Class requirement",
                                  "Departmental/program requirement",
                                  "OU Libraries website",
                                  "Direct Email from OU Librarian to Departmental Listserv",
                                  "Direct Email from OU Libraries Workshop Listserv",
                                  "Direct Email from OU Libraries (Source Unknown)",
                                  "Flyer (Paper or Digital Unspecified)",
                                  "Flyer (Paper or Paper Signage)",
                                  "Supervisor (Forwarded email, word of mouth)",
                                  "Colleagues (Forwarded email, word of mouth)")




## Convert old categories to equivalent new
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Email from University Libraries"] <- "Direct Email from OU Libraries (Source Unknown)"
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Colleagues"] <- "Colleagues (Forwarded email, word of mouth)"
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Supervisor"] <- "Supervisor (Forwarded email, word of mouth)"
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Department/Program Requirement"] <- "Departmental/program requirement"
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Flyer"] <- "Flyer (Paper or Digital Unspecified)"
Registered$How.did.you.hear.about.this.workshop.[Registered$How.did.you.hear.about.this.workshop.=="Walk-in"] <- "Other (please describe)" #correcting that walk-in should not be a pre-registration option

Registered$ReducedCategories <- Registered$How.did.you.hear.about.this.workshop.
Registered$ReducedCategories[!(Registered$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)] <- "Other (please describe)"
Registered$OtherDescriptions[!(Registered$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)] <- Registered$If.other..please.describe.[!(Registered$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)]

## Convert old categories to equivalent new in feedback/post-workshop surveyd ata too.
postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.[postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.=="Email from OU Libraries"] <- "Direct Email from OU Libraries (Source Unknown)"
postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.[postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.=="Colleagues"] <- "Colleagues (Forwarded email, word of mouth)"
postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.[postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.=="Supervisor"] <- "Supervisor (Forwarded email, word of mouth)"
postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.[postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.=="Flyer"] <- "Flyer (Paper or Digital Unspecified)"

postworkshopsurveys_wide$ReducedCategories <- postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.
postworkshopsurveys_wide$ReducedCategories[!(postworkshopsurveys_wide$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)] <- "Other (please describe)"
postworkshopsurveys_wide$OtherDescriptions[!(postworkshopsurveys_wide$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)] <- postworkshopsurveys_wide$How.did.you.hear.about.this.workshop.[!(postworkshopsurveys_wide$How.did.you.hear.about.this.workshop. %in% marketing_current_categories)]

###########
# Titles have varied over time with marketing experiments, typos, and accidental changes.  This code assigns a standardized code between each topic.
###########

topics_complete_variations <- read.csv("./workshop_metadata.csv")

# remove any accidental duplicates of name variations (otherwise will create a many-to-many join below in next step)
topics_complete_variations_cleaned <-topics_complete_variations %>%
  dplyr::distinct(WorkshopNameVariations, WorkshopCode)

# Must attach standard titles to all three datasets (registrations, attendance, and post-workshop surveys) to allow joining the three based on topic and date (date will be cleaned in next code chunk).

## registrations
registrations_named <- left_join(Registered,
                                 topics_complete_variations_cleaned,
                                 by =  c("Title" = "WorkshopNameVariations"))
## attendance
attendance_named <- left_join(workshop,
                                 topics_complete_variations_cleaned,
                                 by =  c("Topic" = "WorkshopNameVariations")) %>% 
  dplyr::filter(!is.na(id))



## postworkshop surveys
postworkshopsurveys_named <- left_join(postworkshopsurveys_wide,
                                       topics_complete_variations_cleaned,
                                       by =  c("What.workshop.did.you.attend." = "WorkshopNameVariations"))



###########
# We clean dates out by weekday, year, month, day, and times for later analysis and data subsetting.
###########

# Month, day, year, hour, minute, seconds
attendance_named$Event.Date.and.Time_2 <- mdy_hms(attendance_named$Event.Date.and.Time,
                                                           truncated = 3)


# separate year, month, date, times
attendance_named$year <- year(attendance_named$Event.Date.and.Time_2)
attendance_named$month <- month(attendance_named$Event.Date.and.Time_2)
attendance_named$day <- day(attendance_named$Event.Date.and.Time_2)
attendance_named$wday <- wday(attendance_named$Event.Date.and.Time_2,
                                      week_start = 1, #1 = Monday,
                                      label = TRUE    # days of weeks as characters
                                      )
attendance_named$hour <-as.numeric(
  format(attendance_named$Event.Date.and.Time_2, "%H"))

attendance_named$Date <-
  format.Date(attendance_named$Event.Date.and.Time_2, "%m/%d/%Y")

# convert 00 hour to NA
# attendance_named[attendance_named$hour==00, "hour"] <- NA

attendance_named$hour <- as.numeric(attendance_named$hour)




###########
# Month, day, year, hour, minute, seconds
###########
postworkshopsurveys_named$Date_2 <- ymd_hms(postworkshopsurveys_named$What.date.was.the.workshop.,
                                                           truncated = 3)


# separate year, month, date, times
postworkshopsurveys_named$year <- year(postworkshopsurveys_named$Date_2)
postworkshopsurveys_named$month <- month(postworkshopsurveys_named$Date_2)
postworkshopsurveys_named$day <- day(postworkshopsurveys_named$Date_2)
postworkshopsurveys_named$wday <- wday(postworkshopsurveys_named$Date_2,
                                      week_start = 1, #1 = Monday,
                                      label = TRUE    # days of weeks as characters
                                      )

# Overwrite original column with formatted date
postworkshopsurveys_named$Date <-
  format.Date(postworkshopsurveys_named$Date_2, "%m/%d/%Y")


###########
# Ordered scales require using R levels to order categories for later graphing or analysis.
###########

#This will need to be repeated for all three "valuable for" questions, so the levels are ordered in the plots.

# First, rename difficult columns
# rename(df, newname = oldname)
postworkshopsurveys_named <- postworkshopsurveys_named %>% 
  dplyr::rename(Valuable.for.Program = `valuable towards your program of study`, 
                Valuable.for.Teaching  = `valuable towards your teaching`,
                Valuable.for.Career = `valuable towards your career`,
                Using.New.Knowledge. = `Do.you.anticipate.that.this.new.knowledge.can.b...`)

## Next, order the levels for all three.
postworkshopsurveys_named$Valuable.for.Program <- factor(x = postworkshopsurveys_named$Valuable.for.Program,
                                                  levels = c("No answer",
                                                             "Not applicable", 
                                                             "Strongly agree",
                                                             "Agree",
                                                             "Somewhat agree",
                                                             "Neither agree nor disagree",
                                                             "Somewhat disagree",
                                                             "Disagree",
                                                             "Strongly disagree"
                                                             
                                                  ))


###########
# Finally, the tidied feedback and attendance data are combined to compare feedback with per-workshop characteristics such as format of instruction.  This is a smaller dataset as not all the workshops appear to have been entered into LibInsights, where the attendance and format data are stored.
###########

joined_post <- full_join(postworkshopsurveys_named, 
                         attendance_named,
                         by = c("Date_2" = "Event.Date.and.Time_2", "WorkshopCode"))
# It is necessary to join by both date and workshop code as occasionally 2 or more people will teach on the same day in different events or classes.
# needs to be a full join because there are some courses not entered and vice versa.

# Filtering by NA is necessary because not everyone has entered their workshops into LibInsights and past workshops did not use the same feedback form, resulting in no attendance or format data to associate with the feedback or vice versa.  filtering by workshop code removes non-workshop course visits.
joined_attendance_post_clean <- joined_post %>%
  dplyr::filter(!is.na(WorkshopCode))

# Later data analysis will remove observations with no feedback automatically.


###########
# Write to files for use in qmds.
###########

write.csv(attendance_named,
          file = "raw_data/processed_attendance_named.csv",
          row.names = FALSE)

write.csv(joined_post,
          file = "raw_data/processed_joined_post.csv",
          row.names = FALSE)

write.csv(postworkshopsurveys_named,
          file = "raw_data/processed_postworkshopsurveys_named.csv",
          row.names = FALSE)
