library(tidyverse)
library(dplyr)
library(ggthemes)
library(scales)


# Question of Interest: 

### What factors lead to the amount of bombs dropped on a mission in WW2?

ww2 <- read.csv("Analysis/operations.csv")
naniar::gg_miss_var(ww2) + ggtitle("Number of Missing Variables")

ww2 <- ww2 %>%
  dplyr::select(Mission.ID, Mission.Date, Theater.of.Operations, Country, Target.Country, Target.City, 
                Target.Industry, Target.Type, Altitude..Hundreds.of.Feet., Target.Priority, 
                High.Explosives.Weight..Tons., Total.Weight..Tons., Attacking.Aircraft, Bombing.Aircraft) %>%
  mutate_all(na_if, "")

ww2 <- ww2 %>%
  dplyr::select(Mission.ID, Mission.Date, Theater.of.Operations, Target.Country, Country, Target.Type, Total.Weight..Tons., Target.Industry, Altitude..Hundreds.of.Feet., Attacking.Aircraft, Bombing.Aircraft)
ww2 <- ww2 %>%
  drop_na(Total.Weight..Tons.) %>% #This will be my explainitory variable, so no missing 
  drop_na(Theater.of.Operations) %>% # no way to impute this
  drop_na(Country) %>%
  drop_na(Target.Type) %>%
  drop_na(Target.Industry) %>%
  group_by(Target.Industry) %>%
  mutate(Altitude..Hundreds.of.Feet. = 
           ifelse(is.na(Altitude..Hundreds.of.Feet.), mean(Altitude..Hundreds.of.Feet., na.rm = TRUE), 
                  Altitude..Hundreds.of.Feet.)) %>%
  mutate(Attacking.Aircraft = 
           ifelse(is.na(Attacking.Aircraft), mean(Attacking.Aircraft, na.rm = TRUE),
                  Attacking.Aircraft)) %>%
  mutate(Bombing.Aircraft = 
           ifelse(is.na(Bombing.Aircraft), mean(Bombing.Aircraft, na.rm = TRUE),
                  Bombing.Aircraft)) %>%
  drop_na(Altitude..Hundreds.of.Feet.) %>%
  drop_na(Attacking.Aircraft) %>%
  drop_na(Target.Country)

ww2 <- ww2 %>%
  rename(Date = Mission.Date,
         Theater = Theater.of.Operations,
         Target_Country = Target.Country,
         Target_Type = Target.Type,
         Total_Weight = Total.Weight..Tons.,
         Industry = Target.Industry,
         Altitude = Altitude..Hundreds.of.Feet.,
         Attacking_Aircraft = Attacking.Aircraft,
         Bombing_Aircraft = Bombing.Aircraft) 

ww2 <- ww2 %>%
  mutate(Total_Weight = round(Total_Weight)) %>%
  mutate_at(vars(Total_Weight), as.integer) %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  mutate(System_Time = as.numeric(as.POSIXct((Date)))) %>%
  mutate(Alliance = if_else(grepl("AUSTRIA", Target_Country), "Axis",
                            if_else(grepl("GERMANY", Target_Country), "Axis",
                                    if_else(grepl("BULGARIA", Target_Country), "Axis",
                                            if_else(grepl("SLOVAKIA", Target_Country), "Axis",
                                                    if_else(grepl("HUNGARY", Target_Country), "Axis",
                                                            if_else(grepl("ROMANIA", Target_Country), "Axis", 
                                                                    if_else(grepl("BULGARIA", Target_Country), "Axis",
                                                                            if_else(grepl("CROATIA", Target_Country), "Axis",
                                                                                    if_else(grepl("IRAQ", Target_Country), "Axis",
                                                                                            if_else(grepl("ITALY", Target_Country), "Axis",
                                                                                                    if_else(grepl("FINLAND", Target_Country), "Axis", 
                                                                                                            if_else(grepl("THAILAND", Target_Country), "Axis",
                                                                                                                    if_else(grepl("CROATIA", Target_Country), "Axis", "Non-Axis"))))))))))))))
ww2 <- ww2 %>% 
  mutate(Industry_Type = if_else(grepl("CITIES TOWNS AND URBAN AREAS", 
                                       Industry), "City", "Non-City")) %>%
  dplyr::select(-Industry, -Target_Type)


kableExtra::kable(head(ww2)) %>% kableExtra::kable_styling(latex_options="scale_down")

## Poisson Regression


first_model_report <- glm(Total_Weight ~ Attacking_Aircraft + 
                            Bombing_Aircraft + Altitude + System_Time + Theater
                          + Alliance, data = ww2, 
                          family = poisson(link = "log"))
summary(first_model_report)


## Negative Binomial


neg_model_report <- MASS::glm.nb(Total_Weight ~ Attacking_Aircraft + 
                                   Bombing_Aircraft + Altitude + System_Time + Theater + Alliance + Industry_Type, data = ww2)
summary(neg_model_report)




