# declare dependencies
if (!exists("setup_sourced")) source(here::here("R", "setup.R"))
library(RColorBrewer)
library(viridis)
display.brewer.all(10)
#---------------------------------------------------------------------

# load data
covid_survey <- fread(here::here("data", "CAPS-ACSP survey on returning to workplace.csv"))
glimpse(covid_survey)

#---------------------------------------------------------------------

# summary of respondent demography 
summary <- covid_survey %>%
  rename(organization = `Which option(s) best explains your work organization?`,
         gender = `How do you identify your gender?`,
         disability_status = `Are you a Person with a Disability?`,
         LGBTQ_identity = `LGBTQ?`,
         aboriginal_status = `Are you an Aboriginal Person?`,
         racialized_identity = `Are you a Racialized Person?`,
         child_services = `Do you currently have/use childcare services (e.g. daycare, summer camp, pre-school, school, private childcare, other programs)? `, 
         commute = `How do you usually commute to your workplace?`)

levels(factor(summary$organization))
levels(factor(summary$gender))
levels(factor(summary$disability_status))
levels(factor(summary$LGBTQ_identity))
levels(factor(summary$aboriginal_status))
levels(factor(summary$racialized_identity))
levels(factor(summary$child_services))
levels(factor(summary$commute))



#---------------------------------------------------------------------

# gender summary

summary_gender <- summary %>%
  mutate(organization=strsplit(organization, ";")) %>% 
  unnest(organization) %>%
  select(organization, gender) %>%
  group_by(organization, gender) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(organization, gender, percentage)


# plot summary
 
ggplot(summary_gender, aes(x=reorder(organization, -percentage), y=percentage, fill = gender)) +
  geom_bar(aes(fill=gender),   
                        stat="identity",
                        colour="black",    
                        position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
  axis.title.x = element_text(color="black", size=14, face="bold"),
  axis.title.y = element_text(color="black", size=14, face="bold"),
  axis.text.y = element_text(size=14,  colour = "black"), 
  axis.text.x = element_text(size=14, face="bold", colour = "black"))


#---------------------------------------------------------------------

# other demographical summaries

table1(~ disability_status + LGBTQ_identity + aboriginal_status + racialized_identity, 
       data = summary, transpose = TRUE)

#---------------------------------------------------------------------

# child services

summary_child <- summary %>%
  select(child_services) %>%
  group_by(child_services) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(child_services, percentage) %>%
  filter(percentage > 5)

# visualize
ggplot(summary_child, aes(x=child_services, y=percentage, fill = child_services)) +
  geom_bar(aes(fill=child_services),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

#---------------------------------------------------------------------

# commute

summary_commute <- summary %>%
  select(commute) %>%
  mutate(commute=strsplit(commute, ";")) %>% 
  unnest(commute) %>%
  group_by(commute) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(commute, percentage) %>%
  filter(percentage > 3)

# visualize
ggplot(summary_commute, aes(x=reorder(commute, -percentage), y=percentage, fill = commute)) +
  geom_bar(aes(fill=commute),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

#---------------------------------------------------------------------

# back to work findings 
work <- covid_survey %>%
  rename(back_to_work = `Would you like to continue working from home at this time?`,
         safe_return = `Considering your return to workplace, which applies?`,
         policy_measures = `Does your organization have policies regarding the following, to support your return to work?  `,
         confidence_work = `When will you feel confident and comfortable going back to work?`,
         comfortable_work = `What will make you feel confident and comfortable going back to work?`,
         return = `When do you foresee yourself returning to the workplace?`,
         implemented_measures = `Which health and safety measures or interventions will be/are implemented at your workplace?`)


return1 <- work %>%
  mutate(safe_return=strsplit(safe_return, ";")) %>% 
  unnest(safe_return) %>%
  group_by(safe_return) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(safe_return, percentage) %>%
  filter(percentage > 3)

# visualize
ggplot(return1, aes(x=reorder(safe_return, -percentage), y=percentage, fill = safe_return)) +
  geom_bar(aes(fill=safe_return),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA))

return2 <- work %>%
  mutate(return=strsplit(return, ";")) %>% 
  unnest(return) %>%
  group_by(return) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(return, percentage) %>%
  filter(percentage > 3) %>%
  arrange(desc(percentage))

# visualize
ggplot(return2, aes(x=reorder(return, -percentage), y=percentage, fill = return)) +
  geom_bar(aes(fill=return),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)) 
   

return3 <- work %>%
  mutate(implemented_measures=strsplit(implemented_measures, ";")) %>% 
  unnest(implemented_measures) %>%
  group_by(implemented_measures) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(implemented_measures, percentage) %>%
  filter(percentage > 3) %>%
  arrange(desc(percentage))

# visualize
ggplot(return3, aes(x=reorder(implemented_measures, -percentage), y=percentage, fill = implemented_measures)) +
  geom_bar(aes(fill=implemented_measures),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)) 


return4 <- work %>%
  mutate(policy_measures=strsplit(policy_measures, ";")) %>% 
  unnest(policy_measures) %>%
  group_by(policy_measures) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(policy_measures, percentage) %>%
  filter(percentage > 3) %>%
  arrange(desc(percentage))

# visualize
ggplot(return4, aes(x=reorder(policy_measures, -percentage), y=percentage, fill = policy_measures)) +
  geom_bar(aes(fill=policy_measures),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)) 


return5 <- work %>%
  mutate(back_to_work=strsplit(back_to_work, ";")) %>% 
  unnest(back_to_work) %>%
  group_by(back_to_work) %>%
  summarise(count = n()) %>%
  mutate(percentage = round(count /sum(count) * 100),2) %>%
  distinct(back_to_work, percentage) %>%
  filter(percentage > 3) %>%
  arrange(desc(percentage))

# visualize
ggplot(return5, aes(x=reorder(back_to_work, -percentage), y=percentage, fill = back_to_work)) +
  geom_bar(aes(fill=back_to_work),   
           stat="identity",
           colour="black",    
           position="stack")+
  theme_minimal() + coord_flip()+
  scale_fill_viridis(discrete = T) +
  theme(
    axis.title.x = element_text(color="black", size=14, face="bold"),
    axis.title.y = element_text(color="black", size=14, face="bold"),
    axis.text.y = element_text(size=14,  colour = "black"), 
    axis.text.x = element_text(size=14, face="bold", colour = "black"),
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent", color = NA)) 

#---------------------------------------------------------------------

# likert scale
library(likert)
likert_data <- covid_survey %>%
  rename(preparedness = `how prepared are you for second wave? Preparedness for second wave`,
         safety = `I feel safe traveling to and from work.	`,
         confidence = `I feel confident my organization will ensure a safe return to work for me.`) %>%
  select(preparedness, safety, confidence)


levels(factor(likert_data$preparedness))
levels(factor(likert_data$safety))
levels(factor(likert_data$confidence))

likert_data1 <- lapply(likert_data, factor, levels = 1:5)
likert_data2 <- likert(as.data.frame(likert_data1))
plot(likert_data2, ordered = FALSE)
plot(likert_data2, ordered = FALSE, centered = FALSE)
plot(likert_data2, type = "heat")

#-------------------------------------------------------------------------------------
git2r::repository()

