#######################################
#Author: Paulo Brando
#March 30th, 2021
#Calculate tree growth rates
#######################################

#import libraries
library(lubridate)
library(tidyverse)

###
###
###

#set working directory
setwd("/Volumes/GoogleDrive/My Drive/Teaching/2021/Data_Visualization/R/DataVisEss")

#list files/folders in that directory
list.files()

#list files in the folder "Data"
(file_import <- list.files(path = "Data", pattern = "*.csv"))
#files <- lapply(listFiles, read_csv)
#files_dataFrame <- do.call(rbind, files)
###
###
###

#Importing tree growth
gr <- read_csv(file.path("Data", file_import))

#Replace all -9999 in the file by NA
gr[gr == -9999] = NA

#Take a quick look at the data
glimpse(gr)
#View(gr)
#print column names
names(gr)

###
###
###

#Create a subset only with trees diameters
dbh <- gr %>% 
  select(Treatment:Init_DBH_cm, starts_with("DAP")) %>% #select only columns that start with DAP
  #gr[,c(1:9, seq(16, ncol(gr), 4))]%>% #It does the same thing as line "38"
  #reshape the file from wide to long format
  pivot_longer(names_to = "Var", #create a new column
               values_to = "Value", #create a new column
               cols = DAP_1999:DAP..cm._3.3.2006) #columns to be "stacked"

#Same as above but instead of reshaping DAP (or dbhs) we are reshape dates
dates <- gr[,c(1:9, seq(14, ncol(gr), 4))] %>%
  pivot_longer(names_to = "Var", 
               values_to = "Date", 
               cols = MidDate19996.23.1999:date_3.3.2006) %>%
  select(-c(Treatment:Var))

###
###
###

#combine growth and date again
growth <- dates %>% 
  bind_cols(dbh) %>%
  mutate(Date2 = mdy(Date)) %>% #Create new column Date2
  arrange(Treatment, Tree_Number) %>% #sort the table - as tibble
  group_by(Treatment, Tree_Number) %>% # group the dataset
  mutate(Lag_dbh = lag(Value, 1), #new column lag dbh
         Growth = Value - Lag_dbh, #new column growth for each individual
         Growth_mo = (Growth/as.numeric(Date2 - lag(Date2, 1)))*30.5) %>% #growth per individual per month
  ungroup() %>% #remove grouping variables
  filter(Date2 < mdy("04-01-2010")) #filter dates 

###
###
###

#Summarize by Treatment and Date -- global stats
gr_avg <- growth%>%
  group_by(Treatment, Date2)%>%
  summarise(Avg_growth = mean(Growth_mo, na.rm = TRUE),
            Med_growth = median(Growth_mo, na.rm = TRUE),
            upper = Hmisc::smean.cl.boot(Growth_mo, na.rm=TRUE)[1],
            lower = Hmisc::smean.cl.boot(Growth_mo, na.rm=TRUE)[2])


###
###
###

#Plotting by stages 
ggplot()+ #create empty ggplot object
  #insert one line per individual
  geom_line(data = growth, 
            aes(x = Date2, 
                y = Growth,
                group=Tree_Number),
            alpha = .4, 
            color = 'lightblue')+
  #insert one line representing the average values from a different dataset
  geom_line(data = gr_avg, 
            aes(x = Date2,
                y = Avg_growth),
            color = 'red',
            lwd = 1.2)+
  #insert one line representing the average values from a different dataset
  geom_line(data = gr_avg, 
            aes(x = Date2,
                y = Med_growth),
            color = 'orange',
            lwd = 1.2)+
  #create one panel for each treatment
  facet_wrap(~Treatment) +
  theme_minimal(base_size = 16)
  
  
###
###
###

#cumulative tree growth
growth%>%
  group_by(Treatment, Tree_Number)%>% #group by treatment and individual
  mutate(Growth_cs = cumsum(ifelse(is.na(Growth), 0, Growth)))%>% #great new column - cumulative growth - replace NAs by 0
  ggplot(aes(x = Date2, 
             y = Growth_cs))+
  geom_line(aes(group=Tree_Number, 
                color = Growth_cs), 
            lwd = 0.5,
            
            #color = 'lightblue',
            alpha = 0.3)+
  #viridis::scale_color_viridis() +
  ylab("Cumulative Growth")+
  stat_summary(fun.data = "mean_cl_boot", fill = "gray", alpha = 0.3, #this function is slow but quite powerful
               geom = "crossbar") + 
  facet_wrap(~Treatment) +
  theme_minimal(base_size = 16)

###
###
##

#import library to plot distributions
library(ggridges)

growth %>%
  filter(abs(Growth_mo) < 0.01) %>%
  group_by(Tree_Number, Treatment, Family) %>% #grouping 
  summarise(G_mo = mean(Growth_mo, na.rm=TRUE)) %>%
  group_by(Family, Treatment) %>%
  mutate(NInd = n()) %>%#calculating how many individuals in each family
  filter(NInd > 15) %>% #filter by number of individuals within each family
  #pass the column names to ggplot
  ggplot(aes(x = G_mo, 
             fill = stat(x),
             y = Family)) +
  ggridges::geom_density_ridges_gradient(scale = 2, 
                                size = 0.25, 
                                rel_min_height = 0.01,
                                alpha = 0.1) +
  scale_fill_viridis_c(name = "Growth", option = "C") +
  facet_wrap(~Treatment) +
  theme_ridges(font_size = 13, grid = TRUE) 


#
