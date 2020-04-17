
# declare dependencies
if (!exists("setup_sourced")) source(here::here("R", "setup.R"))

#---------------------------------------------------------------------

# load data
library(readxl)
covid_survey <- read_excel(here::here("data", "COVID_postdoc_survey.xlsx"), sheet = 1)
glimpse(covid_survey)

#---------------------------------------------------------------------

# Clean out provinces
levels(factor(covid_survey$`In which province are you doing your postdoctoral fellowship?`))

al <- c("Ab", "alberta", "Alberta", "ALBERTA", "Edmonton", "Calgary, Alberta", "Alberta prior to pandemic, BC since lockdown")
bc <- c("British Columbia", "BC")
qc <- "Quebec"
on <- c("Ontario", "Alberta/Ontario")
ns <- "Nova Scotia"
out <- "Outside of Canada, Dresden, Germany"

# clean out the province field
covid_survey_provinces <- covid_survey %>%
  mutate(province = `In which province are you doing your postdoctoral fellowship?`) %>%
  mutate(province = case_when(province %in% al ~ "AB",
                              province %in% bc ~ "BC",
                              province %in% qc ~ "QC",
                              province %in% on ~ "ON",
                              province %in% ns ~ "NS",
                              province %in% out ~ "Other")) 

#----------------------------------------------------------------------------------------

# Figure 1A-B-C ("work interruptions")
levels(factor(covid_survey$`Have you had to cancel any plans to attend a conference/meeting/lab visit this year due to the pandemic?`))
levels(factor(covid_survey$`Has your current fellowships/contracts have been impacted?`))
levels(factor(covid_survey$`Has any of your job applications/interviews been disrupted?`))

fig_1 <- covid_survey_provinces[,c(4, 9, 10, 19)]

fig_1 <- fig_1 %>% 
  rename(work_interruptions = `Have you had to cancel any plans to attend a conference/meeting/lab visit this year due to the pandemic?`,
         job_app_interruptions = `Has any of your job applications/interviews been disrupted?`,
         fellowship_interruptions = `Has your current fellowships/contracts have been impacted?`)

library(RColorBrewer)
display.brewer.all(10)

a <- fig_1 %>%
  group_by(work_interruptions) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /sum(count) * 100)

a_plot <- ggplot(a) + 
  aes(x = work_interruptions, y = percentage, fill =work_interruptions) +
  geom_bar(colour="black", stat = "identity", width=.5, 
           fill = brewer.pal(length(unique(a$work_interruptions)), "Spectral")) + 
  labs(title="Figure 1. Postdoc work interruptions", 
       subtitle="A", 
       caption="") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6)) +
  guides(fill=FALSE)



b <- fig_1 %>%
  group_by(fellowship_interruptions) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /sum(count) * 100)

b_plot <- ggplot(b) + 
  aes(x = fellowship_interruptions, y = percentage, fill =fellowship_interruptions)+
  geom_bar(colour="black", stat = "identity", width=.5,
           fill = brewer.pal(length(unique(b$fellowship_interruptions)), "Spectral")) + 
  labs(title="Figure 1. Postdoc fellowship interruptions", 
       subtitle="B", 
       caption="") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

c <- fig_1 %>%
  group_by(job_app_interruptions) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /sum(count) * 100) %>%
  na.omit()

c_plot <- ggplot(c) + 
  aes(x = job_app_interruptions, y = percentage, fill =job_app_interruptions) +
  geom_bar(colour="black", stat = "identity", width=.5,
           fill = brewer.pal(length(unique(c$job_app_interruptions)), "Spectral")) + 
  labs(title="Figure 1. Job application interruptions", 
       subtitle="C", 
       caption="source: CAPS-ACSP COVID-19 Postdoc Survey") + 
  theme(axis.text.x = element_text(angle=0, vjust=0.6))

library("gridExtra")
grid.arrange(a_plot,b_plot,c_plot, ncol = 2, nrow = 2)

#----------------------------------------------------------------------------------------

# Figure 2 ("impact on work")
levels(factor(covid_survey$`Are they working remotely or had to adapt, or was their research something amenable to remote working?`))

fig_2 <- covid_survey_provinces[,c(8, 19)]

fig_2 <- fig_2 %>% 
  rename(adaptation = `Are they working remotely or had to adapt, or was their research something amenable to remote working?`)

d <- fig_2 %>%
  group_by(adaptation) %>%
  filter(!adaptation == "Had to adapt working remotely, My research is not amenable to working remotely") %>%
  filter(!adaptation == "Can work remote, Had to adapt working remotely") %>%
  summarise(count = n()) %>%
  mutate(percentage = count /sum(count) * 100) %>%
  filter(percentage > 1) 

d_plot <- ggplot(d) + 
  aes(x = adaptation, y = percentage, fill =adaptation) +
  geom_bar(colour="black", stat = "identity", width=.5,
           fill = brewer.pal(length(unique(d$adaptation)), "Spectral")) + 
  labs(title="Figure 2. Amenability of postdocs research to remote work", 
       subtitle="", 
       caption="source: CAPS-ACSP COVID-19 Postdoc Survey") + 
  theme(axis.text.x = element_text(angle=90, hjust=1.0, vjust = 0.5))

#----------------------------------------------------------------------------------------

# Figure 3 ("work adaptations")
levels(factor(covid_survey$`In a 1-5 scale, was your research activities affected by COVID-19?`))

fig_3 <- covid_survey_provinces[,c(12, 19)]

fig_3 <- fig_3 %>% 
  rename(impact = `In a 1-5 scale, was your research activities affected by COVID-19?`)

e <- fig_3 %>%
  group_by(impact) %>%
  summarise(count = n()) %>%
  mutate(percentage = count /sum(count) * 100)

e_plot <- ggplot(e) + 
  aes(x = impact, y = percentage, fill =impact) +
  geom_bar(colour="black", stat = "identity", width=.5,
           fill = brewer.pal(length(unique(e$impact)), "Spectral")) + 
  labs(title="Figure 3. COVID-19's impact on postdoctoral work", 
       subtitle="Score of 5 being the most affected and 1 not very affected", 
       caption="source: CAPS-ACSP COVID-19 Postdoc Survey") + 
  theme(axis.text.x = element_text(angle=0, hjust=1.0, vjust = 0.5))


