# viridis is for colorblind-friendly data visualizations!

library(tidyverse)
install.packages('viridis')
library(viridis)

workshop <- read.csv ("C:/Users/narr1655/Downloads/WorkshopAttendanceUpdate.csv", header=TRUE, stringsAsFactors=FALSE)

# First plotting attendance numbers by workshop title (regardless of format)
# Narrowed it down to Topic and Total Attendees because it's easier

attend1 <- workshop[,c('Topic', 'Total.Attendees')]

attend1fixed <- na.omit(attend1)

# Boxplot time!

attend1fixed %>%
  ggplot (aes(x=Topic, y=Total.Attendees, fill=Topic)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_discrete(labels=c('AfAm.Newspapers', 'Backups', 'Data Viz', 'Digi News', 'DMPTools', 'Formatting', 'ggplot2', 
                            'Shell', 'LaTeX', 'Research.Files', 'MATLAB', 'matplotlib', 'OSF', 'Presentations', 'Repetitive.R',
                            'Meetings', 'Bash', 'R', 'Git', 'OpenRefine', 'Python', 'SQL', 'Tableau', 'Troubleshoot', 
                            '2day Python', '2day R','Unknown', 'Computers', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2)
  )+
  geom_jitter(color='black', alpha = 0.55, size = 2, position = position_jitter (width = .1), show.legend = FALSE) +
  ggtitle('Attendees by Topic')

# Now plotting attendance numbers by workshop title by requested/not, by year
# Oops I forgot to omit NA from the whole dataset, doing that now

workshopfixed <- na.omit(workshop)

workshop2018 <- workshopfixed %>% filter(grepl('2018', Event.Date.and.Time))

workshop2019 <- workshopfixed %>% filter(grepl('2019', Event.Date.and.Time))

workshop2020 <- workshopfixed %>% filter(grepl('2020', Event.Date.and.Time))

workshop2021 <- workshopfixed %>% filter(grepl('2021', Event.Date.and.Time))

workshop2022 <- workshopfixed %>% filter(grepl('2022', Event.Date.and.Time))

workshop2023 <- workshopfixed %>% filter(grepl('2023', Event.Date.and.Time))

workshop2018 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d (aesthetics = "color") +
  scale_x_discrete(labels=c('OSF', '2day R', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2)
  )+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .01, height = .05), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2018 Attendees by Requested/Not')

workshop2019 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('Backups', 'Data Viz', 'DMPTool', 'Formatting', 'ggplot2', 'Shell', 'LaTeX', 'Research.Files',
                            'matplotlib', 'OSF', 'Presentations', 'Bash', 'Git', '2day Python', '2day R', 'Unknown', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2)
  )+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2019 Attendees by Requested/Not')

workshop2020 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('Backups', 'Data Viz', 'Formatting', 'ggplot2', 'LaTeX', 'Research.Files',
                            'matplotlib', 'OSF', 'Presentations', 'Repetitive.R', 'Bash', 'Git', 
                            '2day Python', '2day R', 'Unknown', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2))+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2020 Attendees by Requested/Not')

workshop2021 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('Backups', 'Data Viz', 'Digi.News', 'Formatting', 'ggplot2', 'LaTeX', 'Research.Files', 'MATLAB', 
                            'matplotlib', 'OSF', 'Presentations', 'Repetitive.R', 'Bash', 'Git', 'OpenRefine', 'SQL', 
                            'Tableau', '2day Python', 'Unknown', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2))+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2021 Attendees by Requested/Not')

workshop2022 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('Backups', 'Data Viz', 'Digi.News', 'Formatting', 'ggplot2', 'LaTeX', 'Research.Files', 'MATLAB',
                            'OSF', 'Presentations', 'Repetitive.R', 'Git', 'Unknown', 'Computers', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2))+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2022 Attendees by Requested/Not')

workshop2023 %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('AfAm.Newspapers', 'DMPTool', 'Formatting', 'ggplot2', 'LaTeX', 'Research.Files', 'OSF', 'Repetitive.R',
                            'Meetings', 'R', 'Troubleshoot', '2day R', 'Unknown', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2))+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2023 Attendees by Requested/Not')

################

#Last one! Plotting attendance for carpentries by multi-day VS single-topic

carpentries <- workshopfixed %>% filter(grepl('Standalone Bash|Standalone Carpentries R|Standalone Git|
                                              Standalone Openrefine|Standalone Python|Standalone SQL|Two-day Carpentries with R|Two-day Carpentries with Python', Topic))

carpentries %>%
  ggplot (aes(x=Topic, y=Total.Attendees, fill=Topic)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis(discrete = TRUE) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2)
  )+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE) +
  ggtitle('Multi-Day VS Single Day Workshops')

# A bonus visualization, of attendance numbers by workshop title by requested/not for all years!
  
workshopfixed %>%
  ggplot (aes(x=Topic, y=Total.Attendees, col=Requested.)) +
  geom_boxplot(outlier.shape = NA) +
  scale_fill_viridis_d(aesthetics = "color") +
  scale_x_discrete(labels=c('AfAm.Newspapers', 'Backups', 'Data Viz', 'Digi News', 'DMPTools', 'Formatting', 'ggplot2', 
                            'Shell', 'LaTeX', 'Research.Files', 'MATLAB', 'matplotlib', 'OSF', 'Presentations', 'Repetitive.R',
                            'Meetings', 'Bash', 'R', 'Git', 'OpenRefine', 'Python', 'SQL', 'Tableau', 'Troubleshoot', 
                            '2day Python', '2day R','Unknown', 'Computers', 'Zotero')) +
  theme(panel.background = element_rect(fill = "lightgray",
                                        color = "white",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = "solid",
                                        color = "darkgray"),
        panel.grid.minor = element_line(size = 0.25, linetype = "solid",
                                        color = "darkgray"),
        plot.title = element_text(size = 20, hjust = 0.5),
        axis.title = element_text(size = 14),
        axis.text = element_text(size = 12, angle = 90, hjust = 0.95, vjust = 0.2)
  )+
  geom_jitter(alpha = 0.55, size = 4, position = position_jitter (width = .08, height = .04), show.legend = FALSE, aes(color=Requested.)) +
  ggtitle('2018-2023 Workshop Attendees by Requested/Not')