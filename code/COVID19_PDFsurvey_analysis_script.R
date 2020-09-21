# Code was developed by Sarah Grasedieck and Noushin Nabavi
# declare dependencies
if (!exists("setup_sourced")) source(here::here("R", "setup.R"))
#---------------------------------------------------------------------

#Incorporate data files
res.cat <- read.delim(here::here("data", "COVID-19_PDFsurvey_categorized.txt"), 
                      row.names=1)

logistics_pros <- read.delim(here::here("data", "COVID-19_PDFsurvey_logistics_pros.txt"), 
                             comment.char="#", stringsAsFactors=FALSE)

logistics_contras <- read.delim(here::here("data", "COVID-19_PDFsurvey_logistics_contras.txt"), 
                                stringsAsFactors=FALSE)

logistics_Instit <- read.delim(here::here("data", "COVID-19_PDFsurvey_logistics_Institutionfeedback.txt"), 
                               row.names=1, comment.char="#", stringsAsFactors=FALSE)

# horizontal barplots
library(ggplot2)
theme_set(theme_classic())
library(forcats)
res.cat[res.cat == "NA"] <- NA
res.cat[res.cat == "NA "] <- NA
res.cat[res.cat == "N/A"] <- NA
res.cat[res.cat == " "] <- NA

prov_inst_plot <- ggplot(res.cat, aes(x = fct_rev(fct_infreq(Inst_categorized)))) + geom_bar()
prov_inst_plot + coord_flip()

# impact on work permit
wi <- as.data.frame(na.omit(res.cat$Impact_maintype_categorized))
wi <- data.frame(table(wi))
colnames(wi) <- c("class", "n")
wi$prop <- round(prop.table(table(na.omit(res.cat$Impact_maintype_categorized)))*100,2)
wi <- wi[-c(6,7), ]

wi_table <- table(na.omit(res.cat$Impact_maintype_categorized)) #reverse order of items on plot
wi_levels <- names(wi_table)[order(-wi_table)]
wi_levels <- wi_levels[!wi_levels %in% c("", "yes", "N/A", "NA ")]

workpermit_impact <- ggplot(wi, aes(x=class, y=n)) + 
  geom_bar(stat="identity", fill="#111b42")+
  theme_minimal()
workpermit_impact + coord_flip() + scale_x_discrete(limits=wi_levels)

# work permit sub categories
wisub <- as.data.frame(na.omit(res.cat$apll_processing_reason))
wisub <- data.frame(table(wisub))
colnames(wisub) <- c("class", "n")
wisub$prop <- round(prop.table(table(na.omit(res.cat$apll_processing_reason)))*100,2)
wisub <- wisub[-c(8,9), ]

wisub_table <- table(na.omit(res.cat$apll_processing_reason)) #reverse order of items on plot
wisub_levels <- names(wisub_table)[order(-wisub_table)]
wisub_levels <- wisub_levels[!wisub_levels %in% c("", "yes", "N/A", "NA ")]

workpermit_impact <- ggplot(wisub, aes(x=class, y=n)) + 
  geom_bar(stat="identity", fill="#7d89b9")+
  theme_minimal()
workpermit_impact + coord_flip() + scale_x_discrete(limits=wisub_levels)


# yes no pie charts
remotework <- data.frame(table(res.cat$research_amenable_to_remote_work))
colnames(remotework) <- c("class", "n")
remotework$prop <- round(prop.table(table(res.cat$research_amenable_to_remote_work))*100,2)
remotework

remotework_plot <- ggplot(remotework, aes(x=class, y=n)) + 
  geom_bar(stat="identity", fill="#111b42")+
  theme_minimal()
remotework_plot + coord_flip() + ggtitle("research amenable to remote work? (count)")

remotework_plotperc <- ggplot(remotework, aes(x=class, y=prop)) + 
  geom_bar(stat="identity", fill="#111b42")+
  theme_minimal()
remotework_plotperc + coord_flip() + ggtitle("research amenable to remote work? (percent)")


# effect size plot
library(tidyverse)
res.cat %>% 
  select(impact_on_research_activity_effectsize, 
         teaching_affected_effectsize,
         COVID19_work_logistics_satisfaction_effectsize) %>%
  na.omit -> data_item

data_item %>% 
  gather(key = items, value = answer) %>% 
  mutate(answer = factor(answer),
         items = factor(items)) -> resactivity

ggplot(resactivity, aes(x = items)) +
  geom_bar(aes(fill = answer), position = "fill") +
  coord_flip() +
  scale_x_discrete(limits = rev(levels(resactivity$items))) -> p2
p2 + scale_fill_brewer(palette = "Greys")


# doughnot plots yes no
library("ggplot2") 
library("dplyr")   

canceltravel <- data.frame(table(res.cat$Cancel_workrelatedtravel))
colnames(canceltravel) <- c("class", "n")
canceltravel$prop <- round(prop.table(table(res.cat$Cancel_workrelatedtravel))*100,2)
# Add label position
canceltravel <- canceltravel %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
canceltravel

mycols <- c("#7d89b9", "#111b42")
a <- ggplot(canceltravel, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
a + ggtitle("Did you have to cancel work related travel")


impworkpermit <- data.frame(table(res.cat$Impact_workpermit))
colnames(impworkpermit) <- c("class", "n")
impworkpermit$prop <- round(prop.table(table(res.cat$Impact_workpermit))*100,2)
# Add label position
impworkpermit <- impworkpermit %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
impworkpermit
b <- ggplot(impworkpermit, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
b + ggtitle("Did the pandemic impact on your work permit?")



EIapplicant <- data.frame(table(res.cat$EI_applicant))
colnames(EIapplicant) <- c("class", "n")
EIapplicant$prop <- round(prop.table(table(res.cat$EI_applicant))*100,2)
# Add label position
EIapplicant <- EIapplicant %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
EIapplicant
mycols <- c("#e1e2e6", "#7d89b9", "#111b42")
c <- ggplot(EIapplicant, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  #geom_text(aes(y = lab.ypos, label = prop), color = c("white", "white", "black"))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
c + ggtitle("Did you have to apply for EI benefits?")


insurance <- data.frame(table(res.cat$sufficient.insurance))
colnames(insurance) <- c("class", "n")
insurance$prop <- round(prop.table(table(res.cat$sufficient.insurance))*100,2)
#insurance <- insurance[-c(1),]
# Add label position
insurance <- insurance %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
insurance
mycols <- c("#7d89b9", "#e1e2e6", "#111b42")
d <- ggplot(insurance, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = c("white", "black", "white"))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
d + ggtitle("Did you have sufficient insurance?")


applprocess_interr <- data.frame(table(res.cat$interview_applicationprocess_disrupted))
colnames(applprocess_interr) <- c("class", "n")
applprocess_interr$prop <- round(prop.table(table(res.cat$interview_applicationprocess_disrupted))*100,2)
#applprocess_interr <- applprocess_interr[-c(1),]
# Add label position
applprocess_interr <- applprocess_interr %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
applprocess_interr
mycols <- c("#7d89b9", "#e1e2e6", "#111b42")
e <- ggplot(applprocess_interr, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = c("white", "black", "white"))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
e + ggtitle("Did you experience interruptions in application/interview process?")


fellowship_affected <- data.frame(table(res.cat$current_fellowship_contract_impacted))
colnames(fellowship_affected) <- c("class", "n")
fellowship_affected$prop <- round(prop.table(table(res.cat$current_fellowship_contract_impacted))*100,2)
# Add label position
fellowship_affected <- fellowship_affected %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
fellowship_affected
mycols <- c("#7d89b9", "#e1e2e6", "#111b42")
f <- ggplot(fellowship_affected, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = c("white", "black", "white"))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
f + ggtitle("Is your current fellowship affected?")


personally_affected <- data.frame(table(res.cat$personally_affected))
colnames(personally_affected) <- c("class", "n")
personally_affected$prop <- round(prop.table(table(res.cat$personally_affected))*100,2)
# Add label position
personally_affected <- personally_affected %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
personally_affected

mycols <- c("#7d89b9", "#e1e2e6", "#111b42")
g <- ggplot(personally_affected, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = c("white", "black", "white"))+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
g + ggtitle("Are you or your family personally affected?")


teaching <- data.frame(table(res.cat$teaching_part_of_work))
colnames(teaching) <- c("class", "n")
teaching$prop <- round(prop.table(table(res.cat$teaching_part_of_work))*100,2)
# Add label position
teaching <- teaching %>%
  arrange(desc(class)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
teaching

mycols <- c("#7d89b9", "#111b42")
h <- ggplot(teaching, aes(x = 2, y = prop, fill = class)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = prop), color = "white")+
  scale_fill_manual(values = mycols) +
  theme_void()+
  xlim(0.5, 2.5)
h + ggtitle("Is teaching a part of your work?")


province <- data.frame(table(res.cat$location))
colnames(province) <- c("class", "n")
province$prop <- round(prop.table(table(res.cat$location))*100,2)
province

university <- data.frame(table(res.cat$Inst_categorized))
colnames(university) <- c("class", "n")
university$prop <- round(prop.table(table(res.cat$Inst_categorized))*100,2)
university

#-------------------------------------------------------------------------------------

# negative sentiments
logistics_contras <- logistics_contras %>%
  mutate(percentage = round((frequency/sum(frequency) * 100),2))

logistics_contras_p <- logistics_contras %>%
  filter(frequency > 4) %>%
  ggplot(aes(x=reorder(logistics_contras, -percentage), y=percentage)) + 
  geom_bar(stat="identity", fill="red") +
  theme_minimal() + labs(title="Negative experiences",
                         y ="Percent", x = "")
logistics_contras_p + coord_flip() 


# positive sentiments
logistics_pros <- logistics_pros %>%
  mutate(percentage = round((frequency/sum(frequency) * 100),2))

logistics_pros_p <- logistics_pros %>%
  #filter(frequency > 1) %>%
  ggplot(aes(x=reorder(logistics_pros, -percentage), y=percentage)) + 
  geom_bar(stat="identity", fill="#7d89b9") +
  theme_minimal() + labs(title="Positive experiences",
                         y ="Percent", x = "")
logistics_pros_p + ggpubr::rotate()


# Show graphs side by side
require(gridExtra)
plot1 <- logistics_contras_p + coord_flip() 
plot2 <- logistics_pros_p + coord_flip() 
grid.arrange(arrangeGrob(plot1, plot2, ncol=1, heights=c(1,1)))
#-------------------------------------------------------------------------------------

git2r::repository()
