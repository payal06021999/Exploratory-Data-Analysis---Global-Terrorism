# Exploratory-Data-Analysis---Global-Terrorism
Task 4: Exploratory Data Analysis - As Data Science and Business Analytics Intern at The Sparks Foundation

#The Sparks Foundation
#GRIP March 2021
#Payal Sharma
#Data Science and Business Analytics Intern
#Task 4: Exploratory Data Analysis (Global Terrorism)
#Level: Intermediate



library(readr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(purrr)

terrorism<-read.csv("C:/Users/HP/Downloads/globalterrorismdb_0718dist.csv")
head(terrorism)
str(terrorism)

globalterrorism<-terrorism %>% select(eventid,iyear,country,region_txt,attacktype1_txt,success,suicide,targtype1_txt)
names(globalterrorism)<-c("Event_ID","Year","Country","Region","Attack_type","Success","Suicide","Target_type")
globalterrorism

colSums(is.na(globalterrorism))
globalterrorism<-globalterrorism %>% filter(Year>2000)
summary (globalterrorism)
str(globalterrorism)

recentterrorism <- globalterrorism %>% group_by(Year) %>% summarise(Attacks = n())
ggplot(data=recentterrorism, aes(x=Year, y=Attacks))+geom_line()+labs(title="Global Terrorism per year")

region_wise_terrorism<-globalterrorism %>% group_by(Region) %>% summarise(Attacks = n())
ggplot(data=region_wise_terrorism, aes(x=Region, y=Attacks, fill=Region))+geom_bar(stat = "identity")+labs(title="Terrorist attack by Region", x="Region", y="Attacks")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))

countrycode_filter<-globalterrorism %>% filter(Country<500) 
country_wise_terrorism<-countrycode_filter %>% group_by(Country) %>% summarise(Attacks = n())
ggplot(data=country_wise_terrorism, aes(x=Country, y=Attacks, fill=Country))+geom_bar(stat = "identity")+labs(title="Terrorist attack by Country", x="Country", y="Attacks")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))

attacktype_wise_terrorism<-globalterrorism %>% group_by(Attack_type) %>% summarise(Attacks = n())
ggplot(data=attacktype_wise_terrorism, aes(x=Attack_type, y=Attacks, fill=Attack_type))+geom_bar(stat = "identity")+labs(title="Terrorist attack by type", x="Attack type", y="Attacks")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=1))


