# Packages used by this code
library(ggplot2)
library(ggthemes)
library(tidyverse)
library(readxl)
library(ggridges)

# Compile all the data into a single frame
data=NULL
for (y in 2004:2017){
  temp<-read_excel(paste0("csis_",y,".xlsx")) %>%
    mutate(nbr_students=as.numeric(nbr_students),
           sch_yr=as.numeric(sch_yr))
  data<-rbind(data,temp)
}

# Plot the distribution of all class sizes in Alberta
ggplot(data,aes(nbr_students,as.character(sch_yr)))+
  geom_density_ridges(scale=2,bandwidth=1,fill="dodgerblue",alpha=0.75)+
  theme_minimal()+
  coord_cartesian(xlim=c(0,45))+
  labs(x="Number of Students",y="School Year",
       title="Class Sizes in Alberta from the 2004/05 to 2017/18 School Year",
       caption="Graph by @trevortombe",
       subtitle="Source: Government of Alberta data https://open.alberta.ca/opendata/class-size-by-school-year-jurisdiction-and-grade-alberta")
ggsave("plot.png",width=7,height=7,dpi=300)

# Plot the distributions separately by grades (excluding splits)
plotdata<-data %>%
  mutate(grade=ifelse(grade=="K",0,grade),
         grade=as.numeric(grade)) %>%
  filter(!is.na(grade)) %>%
  mutate(group=ifelse(grade<=3,"K-3",NA),
         group=ifelse(grade>3 & grade<=6,"4-6",group),
         group=ifelse(grade>6 & grade<=9,"7-9",group),
         group=ifelse(grade>9 & grade<=12,"10-12",group))
ggplot(plotdata,aes(nbr_students,as.character(sch_yr)))+
  geom_density_ridges(scale=2,bandwidth=1,fill="dodgerblue",alpha=0.75)+
  coord_cartesian(xlim=c(0,45))+
  facet_wrap(~group)+
  theme_minimal()+
  theme(strip.background = element_rect(fill="gray90",color="transparent"),
        legend.position = "none")+
  labs(x="Number of Students",y="School Year",
       title="Class Sizes in Alberta from the 2004/05 to 2017/18 School Year",
       caption="Graph by @trevortombe",
       subtitle="Source: Government of Alberta data https://open.alberta.ca/opendata/class-size-by-school-year-jurisdiction-and-grade-alberta
Data illustrated here excludes split classes.")
ggsave("plot.png",width=8,height=8,dpi=300)

