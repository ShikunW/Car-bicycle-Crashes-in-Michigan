library(dplyr)
library(reshape2)

## Population counts

setwd("/Users/shikunwang/Desktop/BIOSTAT682/final_project")
cpop <- read.csv('rawdata/MI_county_pop.csv', header = T)
cpop <- melt(cpop, id='County')
cpop$year <- as.numeric(gsub('X', cpop$variable, replacement='', fixed=T))
cpop$County <- gsub(' County', cpop$County, replacement='', fixed=T)
cpop$County.Population <- cpop$value
cpop$variable <- NULL
cpop$value <- NULL

citypop10 <- 
  read.csv('rawdata/city_pop_10.csv',header=T) %>% filter(STNAME=="Michigan") %>%
  select(NAME,STNAME,starts_with('POPESTIMATE'))

citypop15 <- 
  read.csv('rawdata/citypop1015.csv',header=T) %>%
  select(NAME,STNAME,starts_with('POPESTIMATE'))

mi_pop <- read.csv('rawdata/mipop90to15.csv',header=T)[,1:2]
names(mi_pop) <- c('year','pop')
mi_pop <- filter(mi_pop, year >= 2004)

# County per capita income and unemployment rates
#  cincome <- read.csv('rawdata/county_income_year.csv',header=T) # this was missing 2015
cincome <- read.csv('rawdata/county_income_year_04-15.csv',header=T) # this contains 2015
cincome <- 
  melt(cincome, id='County', value.name='county.percapita.income') %>%
  mutate(year = as.numeric(substr(variable, 2, 5))) %>%
  select(-variable)
cunemp <- read.csv('rawdata/county_unemployment.csv',header=T)

## Combine files for all years
ysuffix <- c('0405','0607','0809','10','1115')
crdat <- NULL
cydat <- NULL
for (fname in paste('rawdata/mi_crashes_',ysuffix,'.txt',sep='')){
  crdat <-
    bind_rows(crdat, read.table(fname, header=T, sep='\t', fill=TRUE))
}
for (fname in paste('rawdata/mi_cyclists_',ysuffix,'.txt',sep='')){
  cydat <-
    bind_rows(cydat, read.table(fname, header=T, sep='\t', fill=TRUE))
}

## Latitude and longitude columns were mislabeled!!!
crdat$lon <- crdat$Crash.Latitude
crdat$lat <- crdat$Crash.Longitude
crdat$Crash.Longitude <- NULL
crdat$Crash.Latitude <- NULL

# Get information about the cyclists involved in the crash
crdat <- 
  cydat %>% 
  group_by(Crash.Instance) %>%
  summarise(n_cyc=n(),
            n_male_cyc = sum(Person.Gender=="Male"),
            n_fem_cyc = sum(Person.Gender=="Female"),
            n_cyc_killed = sum(Person.Degree.of.Injury=="Killed"),
            n_cyc_incapac = sum(Person.Degree.of.Injury=="Incapacitating injury"),
            n_cyc_noinj = sum(Person.Degree.of.Injury=="No injury"),
            n_cyc_noninc = sum(Person.Degree.of.Injury=="Nonincapacitating injury"),
            n_cyc_possible = sum(Person.Degree.of.Injury=="Possible")) %>%ungroup %>%
  right_join(crdat,
             by=c('Crash.Instance'='Crash.Instance')) 

# Add state and county population
crdat <-
  crdat %>%
  left_join(cpop, 
            by=c('County'='County','Crash.Year'='year')) %>%
  left_join(mi_pop %>%
              mutate(MI.Population = pop),
            by=c('Crash.Year'='year')) %>%
  left_join(cincome %>%
      mutate(County=as.character(paste(County))),
      by=c('County'='County','Crash.Year'='year')) %>%
  left_join(cunemp %>% 
              mutate(County=as.character(paste(County))),
            by=c('County'='County','Crash.Year'='year'))

## Format dates
crdat <-
  crdat %>% 
  mutate(date = as.Date(paste(Crash.Year, Crash.Month, Crash.Day),'%Y %B %d'),
         Crash.Week = as.numeric(format(date, '%V')),
         Crash.Month.num = format(date, format='%m'))

# County land area
cland <- read.table('rawdata/mi_counties_gazette.txt',header=T,sep='\t')
cland$NAME <- do.call('rbind',strsplit(paste(cland$NAME),' County',fixed=T))

# County-level data frame
crdat %>% group_by(County, Crash.Year) %>% 
  summarise(n_crash = length(unique(Crash.Instance)),
            n_cyc_killed = sum(n_cyc_killed)) %>% ungroup %>%
  mutate(n_crash = ifelse(is.na(n_crash), 0, n_crash),
         n_cyc_killed = ifelse(is.na(n_cyc_killed), 0, n_cyc_killed)) %>%
  right_join(cunemp %>% mutate(County=as.character(paste(County))), 
             by=c('County'='County','Crash.Year'='year')) %>%
  right_join(cincome %>%
               mutate(County=as.character(paste(County))),
             by=c('County'='County','Crash.Year'='year')) %>%
  right_join(cpop %>%
               mutate(County=as.character(paste(County))),
             by=c('County'='County', 'Crash.Year'='year')) %>% 
  left_join(select(cland, NAME, county.land.sqmi = ALAND_SQMI), by=c('County'='NAME')) %>% 
  left_join(mi_pop %>%
               mutate(MI.Population = pop) %>%
              select(year, MI.Population),
             by=c('Crash.Year'='year')) %>%
  mutate(n_crash = ifelse(is.na(n_crash), 0, n_crash),
         n_cyc_killed = ifelse(is.na(n_cyc_killed), 0, n_cyc_killed)) %>%
  arrange(County,Crash.Year) -> crashes.year.county


crdat %>% group_by(Crash.Year) %>%
  summarise(n_crash = length(unique(Crash.Instance)),
            n_cyc_killed = sum(n_cyc_killed),
            MI.Population=first(MI.Population)) %>% ungroup %>%
  mutate(crash_rate_100k = n_crash * 100000 / MI.Population,
         fatal_rate_100k = n_cyc_killed * 100000 / MI.Population) -> crashes.year.mi

