library(RMySQL)
library(tidyverse)

########################### PHILIPPE FUNCTION CLEAN 

get_Colony <- function(DF1, DF2) {
  inner_join(DF1 %>% distinct (AnimalID,Date) #one only wants one colony for each day as individual cannot be measure in two colonies simultaneousls
             , DF2, by = "AnimalID") %>%
    filter(Date >= MemberFrom & Date <= MemberTo)%>% 
    select(-c(MemberFrom,MemberTo,AnimalRef))
}

#get_groupcomp(), extract the group composition of the animals which colony has been established 
#DF1 is the dataframe where a date and a colony are provided and that has has been obtained by get_colony() or get_colony_NoMismatch
#DF2 is Membership from the database

get_GroupComp<-function(DF1,DF2){
  inner_join(DF1 %>% 
  distinct(Date,Colony),DF2,by=c("Colony"="QueriedColony")) %>% 
  filter(Date >= MemberFrom & Date <=MemberTo) %>% 
  select(Colony,ColonyOrigin,AnimalID,Date)
}

#get_closestweight() returns the closest weight of all individuals present in a colony at a given time. It will only returns a row if a date was collected because a time difference cannot be computed without a date of weight
#DF1 is a dataframe with a column Date and a column AnimalId. will return the closest weight of all group members at a given date if use output get_groupcomp()
#DF2 is the weight extracted from the database
get_Weight<-function(DF1,DF2){
inner_join(DF1 ,DF2, by="AnimalID") %>%
mutate(DayDiff = abs(round(difftime(WeightDate,Date,units="days")))) %>% 
group_by(AnimalID,Date) %>% 
filter(DayDiff == min(DayDiff)) %>%
ungroup() %>% 
group_by(AnimalID,Date,DayDiff) %>% 
summarise(Weight=mean(Weight)) %>% 
ungroup() %>% 
rename(WeightDayDiff=DayDiff) %>% 
arrange(AnimalID,Date)
}

#get_IDinfo() extract relevant information from an individual at a given date
#DF1 provides the AnimalID and dates. will return the info of all group members at a given date if use output get_groupcomp()
#DF2 is the individual info extracted from the database
#the breeding status has yet to include the paternity of the wild-caught colony. 
get_IDinfo<-function(DF1,DF2){
inner_join(DF1,DF2) %>% 
###AGE
mutate(Age=as.numeric(round(difftime(Date,BirthDate,units="days"))/30.4166),
  DeathAge=as.numeric(round(difftime(DeathDate,BirthDate,units="days"))/30.4166)) %>% 
### BREEDING STATUS
mutate(BreedingStatus = ifelse((WildcaughtQueen == 1 |
                                    (!is.na(Mother_FirstLitter) & ((Mother_FirstLitter - 90) < Date))|
                                    (!is.na(Father_FirstLitter) & ((Father_FirstLitter - 90) < Date))),
                                 "Breeder",
                                 ifelse(Sex=="M"&& is.na(Age) && ColonyOrigin == "F",
                                        "Undetermined",
                                        "Helper"))) %>% 
    select(AnimalID,Date,Sex,Wildcaught,LitterRef,Age,BreedingStatus,DeathAge)
}



#I should try to get a function somehwere that returns the number of adults or heavy animals in the group. Perhaps one could have a look at who does seem to attack-defend more before making a decision

get_GroupID_Info<-function(DF1){DF1 %>%
  #GROUP BY COLONY AND DATE FOR FOLLOWING MUTATE CALLS: GET INDIVIDUAL AND GROUP CHARACTERISTIC 
  group_by(Colony,Date) %>% 
  mutate(GroupSize=n()) %>%
  # group size
  mutate(WeightRank=min_rank(desc(Weight)), 
         AgeRank=min_rank(desc(Age))) %>% # Weight rank, check what it does with NA
  mutate(CompNB_5=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
         CompNB_10=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
         CompNB_15=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
         CompNB_20=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
  mutate(PupNB=sum(Age<1, na.rm= TRUE)) %>% 
  mutate(PupPresence=ifelse(PupNB==0,"No","Yes")) %>% 
  mutate(MinAge=min(Age,na.rm= TRUE)) %>% 
  ungroup() %>% 
  # UNGROUP
  # GROUP BY DATE, COLONY AND SEX FOR FOLLOWING MUTATE CALLS
  group_by(Date,Colony,Sex) %>% 
  mutate(QueueSize=n()) %>% #queue size, that is number of males and females
  mutate(WeightRank_Queue=min_rank(desc(Weight))) %>% #Weight rank
  mutate(CompNB_5_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<5)-1),
         CompNB_10_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<10)-1),
         CompNB_15_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<15)-1), 
         CompNB_20_Queue=sapply(Weight, function(x) sum(abs(x-Weight)<20)-1)) %>% 
  ungroup() 
  # UNGROUP
}

