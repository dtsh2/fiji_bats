## load libraries #####

library(RColorBrewer)
library(tidyverse)
library(ggplot2)
library(skimr)
library(dplyr)
library(tidyr)
library(visreg)
library(reshape2)
library(plyr)

## read in the data  #####

data<-read.csv('Fiji_Bat_Human_data.csv', stringsAsFactors=FALSE, fileEncoding="latin1", header=T, na.strings=c("","NA"))

## check/summarisae the data

summary(data)
str(data)
skim(data)

## tidy/change some values

data$Age<-revalue(data$Age, c("25-30"="25-29"))

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = length(levels(data$Religion))
cols = gg_color_hue(n)

# plot religion
ggplot(data, aes(Religion))+geom_bar()+#ggtitle('Religion') + 
  ylab("Count") +xlab("") + scale_fill_manual(values = cols)+
  theme(legend.position="none")+
  scale_x_discrete(guide = guide_axis(n.dodge = 3))
ggsave("religion_all.png", width = 20, height = 10, units = "cm")

data$Religion<-revalue(data$Religion, c("Anglican"="Other Christian",
                                                  "Pentecostal"="Other Christian",
                                                  "Seventh-day Adventist"="Other Christian"))
n = 4
cols = gg_color_hue(n)

cols <- c("Other Christian" = gg_color_hue(n)[1], "Catholic" = gg_color_hue(n)[1], "Christian" = gg_color_hue(n)[1], 
          "Hindu" = gg_color_hue(n)[2],"Methodist" = gg_color_hue(n)[1],"No religion" = gg_color_hue(n)[3])

ggplot(data, aes(Religion, fill = Religion))+geom_bar()+#ggtitle('Religion') + 
  ylab("Count") +xlab("") + scale_fill_manual(values = cols)+
  theme(legend.position="none", text = element_text(size = 16))+
  scale_x_discrete(guide = guide_axis(n.dodge = 2))
ggsave("religion.png", width = 20, height = 10, units = "cm")

data[is.na(data)] <- NA

## plot all columns

names(data)
for (i in 1:ncol(data)){
  plot(data[,i])  
}


## make a 'if sick column'
# plot sick data

cols_sick<-c("Leave_sick","Move_sick",
             "Care_sick","Kill_sick","Give_sick","Eat_sick")
data <- tidyr::unite(data, sick, cols_sick, sep = ",")
data$sick<-gsub('\\NA','',data$sick)
data$sick<-gsub('\\,','',data$sick)

ggplot(data, aes(sick))+geom_bar()

# plot ppopulation trend data

cols_pop<-c("Bats_increasing","Bats_decreasing",
             "Bats_same")
data <- tidyr::unite(data, trend, cols_pop, sep = ",")
data$trend<-gsub('\\NA','',data$trend)
data$trend<-gsub('\\,','',data$trend)
ggplot(data, aes(trend))+geom_bar()

data$trend<-str_replace(data$trend, "c", "None")
data$trend<-str_replace(data$trend, "a", "Increasing")
data$trend<-str_replace(data$trend, "b", "Decreasing")
data$trend<-str_replace(data$trend, "IncreasingDecreasing", "Inc/Dec")
data$trend<-str_replace(data$trend, "", "NA")

ggplot(data, aes(trend, col = trend, fill = trend))+geom_bar(width = 0.4) +#ggtitle('Reported population trend')+
   theme(legend.position="none",
         text = element_text(size=18)) + ylab('Count') + xlab('')+
  scale_x_discrete(guide = guide_axis(n.dodge = 2)) 
ggsave("trend.png", width = 20, height = 10, units = "cm")

# plot gender data with eating bats

data$Gender<-revalue(data$Gender, c("M"="Male",
                                              "F"="Female"))

df2 <- data %>% 
  group_by(Eat_bat, Past_eat_bat, Age, Gender) %>% 
  tally()

to_plot2<-gather(df2, Eat, Response, Eat_bat:Past_eat_bat, factor_key=TRUE)
levels(to_plot2$Eat) <- c("Current", "Past")
to_plot2$Eat <- relevel(to_plot2$Eat, "Past")

ggplot(data=subset(to_plot2,!is.na(Age)), aes(x=Age, y=n, fill=Response)) +   
  geom_bar(position='stack', stat='identity')+
  theme_bw() + facet_wrap(~Eat) + #ggtitle('Bat eating') +
  theme(text = element_text(size = 16),
        legend.title = element_blank())+
  ylab('Count') + 
  scale_fill_manual(values = c("skyblue","grey","orange"))

ggsave("eating_age_current.png", width = 20, height = 10, units = "cm")

ggplot(data=subset(to_plot2,!is.na(Age)), aes(x=Age, y=n, fill=Response)) +   
  geom_bar(position='stack', stat='identity')+
  theme_bw() + facet_wrap(~Gender)+ 
  ylab('Count') + 
  scale_fill_manual(values = c("skyblue","grey","orange")) 

ggsave("eating_gender_current.png", width = 20, height = 10, units = "cm")

ggplot(data=subset(to_plot2,!is.na(Age)), aes(x=Age, y=n, fill=Response)) +   
  geom_bar(position='stack', stat='identity',width = 0.6)+
  theme_bw() + facet_wrap(~Gender+Eat)+ #ggtitle('Bat eating') + 
  ylab('Count')+
  theme(text = element_text(16),
        legend.title = element_blank())+ 
  scale_fill_manual(values = c("skyblue","grey","orange"))

ggsave("eating.png", width = 20, height = 15, units = "cm")

ggplot(data, aes(Age))+geom_bar(width=0.4) +#ggtitle('Age')+
  theme(legend.position="none",
        text = element_text(size = 16)) + ylab('Count') + xlab('Age')
ggsave("age_grey.png", width = 20, height = 10, units = "cm")

# plot contact data by age and gender

df3 <- data %>% 
  group_by(Contact, Age, Gender) %>% 
  tally() 

to_plot3<-gather(df3, Contact, Response, Contact, factor_key=TRUE)

ggplot(data=subset(to_plot3,!is.na(Age)&!is.na(Gender)), aes(x=Age, y=n, fill=Response)) +
  geom_bar(position='stack', stat='identity')+
  theme_bw() + facet_wrap(~Gender)+ #ggtitle('Bat contacts') +
  ylab('Count')+
  theme(text = element_text(size = 16),
        legend.title = element_blank())+ 
  scale_fill_manual(values = c("skyblue","orange"))
ggsave("contacts_gender.png", width = 20, height = 10, units = "cm")

# plot education

data$Education_yrs <- factor(data$Education_yrs, levels =c("3","4","5","6","7","8","9","10","11","12","12+","13","13+"))
data$Education_yrs<-ordered(data$Education_yrs, levels = c("3","4","5","6","7","8","9","10","11","12","12+","13","13+"))
ggplot(data, aes(Education_yrs))+geom_bar()+#ggtitle('Education') + 
  ylab('Count') + xlab("Education in years")
ggsave("education.png", width = 20, height = 10, units = "cm")

# plot gender

ggplot(data, aes(Gender, fill = Gender))+geom_bar()+#ggtitle('Gender')+
  ylab("Count")+xlab("")+theme(legend.position="none")
ggsave('gender.png', width = 8, height = 8, units = "cm")

# plot preparing bats

ggplot(data, aes(Prepare_eat,fill = Prepare_eat))+geom_bar()+#ggtitle('Prepare bats')+
  ylab("Count")+xlab("")+theme(legend.position="none")
ggsave('Prepare_bats.png', width = 8, height = 8, units = "cm")

data_eat <- melt(data, id.vars="Person.ID.number",measure.vars=c("Past_eat_bat","Eat_bat"))
levels(data_eat$variable) <- c("Past", "Current")
# Everything on the same plot
ggplot(data_eat, aes(x=variable, fill = value)) + 
  geom_bar()+#ggtitle('Bat consumption')+
  ylab('Count') + xlab('')+ 
  scale_fill_manual(values = c("skyblue","grey","orange"))
ggsave('eat_bats.png', width = 10, height = 10, units = "cm")

# plot catching bats

data_catch <- melt(data, id.vars="Person.ID.number",measure.vars=c("Past_catch_bats","Catch_bats"),na.rm = T)

data_catch$value<-sub('No .*', '', data_catch$value)
data_catch$value<-sub('Will go -.*', '', data_catch$value)
data_catch[data_catch==""]  <- NA 
# Everything on the same plot
levels(data_catch$variable) <- c("Past", "Current")
colnames(data_catch)[3]<-'Response'
ggplot(data_catch, aes(x=variable, fill = Response)) + 
  geom_bar(width = 0.4)+#ggtitle('Bat catching')+
  theme(legend.title = element_blank())+#,
  #      text = element_text(size = 18))+
  ylab("Count") + xlab("")+ 
  scale_fill_manual(values = c("skyblue","grey","orange"))
ggsave('catch_bats.png', width = 10, height = 10, units = "cm")

# plot collecting guano

data_guano <- melt(data, id.vars="Person.ID.number",measure.vars=c("Past_mine_guano","Mine_guano"))
levels(data_guano$variable) <- c("Past_mine_guano", "Mine_guano")
data_guano$value<-sub('No .*', '', data_guano$value)
data_guano[data_guano==""]  <- NA 
data_guano[data_guano=="- "]  <- NA 
data_guano[data_guano=="a"]  <- 'No' 
# Everything on the same plot
levels(data_guano$variable) <- c("Past", "Current")
colnames(data_guano)[3]<-'Response'
# Everything on the same plot
ggplot(data_guano, aes(x=variable, fill = Response)) + 
  geom_bar(width = 0.5)+#ggtitle('Bat guano') +
  ylab('Count') + xlab('')+
  theme(#text = element_text(size = 26),
        legend.title = element_blank())+ 
  scale_fill_manual(values = c("skyblue","orange"))
ggsave('collect_guano.png', width = 10, height = 10, units = "cm")

# plot age and education

plot(data$Age,data$Education_yrs)
ggplot(data[!(is.na(data$Age)),],aes(x=Age, col=Education_yrs, fill = Education_yrs)) + 
  geom_bar() + facet_grid(~ Gender, drop = T)+ylab('Count')#+ggtitle('Education level by age')
ggsave('Education_age.png', width = 20, height = 10, units = "cm")

df4 <- data %>% 
  group_by(Education_yrs, Age, Gender) %>% 
  tally() 

colourCount = length(unique(data$Education_yrs))
getPalette = colorRampPalette(brewer.pal(9, "RdYlGn"))

ggplot(data=subset(df4,!is.na(Age)&!is.na(Gender))) +
  geom_bar(aes(x=Age, y=n, fill=Education_yrs),stat="identity")+
  theme_bw() + facet_wrap(~Gender)+# ggtitle('Education') + 
  ylab('Count') +
  guides(fill=guide_legend(ncol=2, title = "Years"))+
  scale_fill_manual(values = getPalette(colourCount),
                    na.value = "grey")
ggsave("edu_gender_raw.png", width = 20, height = 10, units = "cm")

ggplot(data=subset(df4,!is.na(Age)&!is.na(Gender))) +
  geom_bar(aes(x=Age, y=n, fill=Education_yrs),stat="identity", position="fill")+
  theme_bw() + facet_wrap(~Gender)+# ggtitle('Education') + 
  ylab('Proportion') +
  guides(fill=guide_legend(ncol=2, title = "Years"))+
  scale_fill_discrete()
ggsave("edu_gender.png", width = 20, height = 10, units = "cm")


## # logistic regression

data$Eat_bat<-as.factor(str_replace(data$Eat_bat, "Will eat", "Yes"))

# with interactions between age and gender

logit_eat <- glm(Eat_bat ~ Age * Gender,data=data,family="binomial", na.action = na.omit)
summary(logit_eat)

# without interactions between age and gender

logit_eat1 <- glm(Eat_bat ~ Age + Gender,data=data,family="binomial", na.action = na.omit)
summary(logit_eat1)
anova(logit_eat,logit_eat1, test = 'Chisq')

logit_eat <- glm(Eat_bat ~ Age + Gender,data=data,family="binomial")
summary(logit_eat)
logit_eat_i <- glm(Eat_bat ~ Age * Gender,data=data,family="binomial")
summary(logit_eat_i)

# compare AIC
AIC(logit_eat,logit_eat_i)
