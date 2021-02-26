# Notwendige Packages laden
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lme4)
library(lattice)
library(ggplot2)
library(scales)
library(Cairo)
CairoWin()

## COVID-Fallzahlen

# Liste der OECD-Länder erstellen
list_of_countries <- c("Australia", "Austria", "Belgium", "Canada", "Chile",
                       "Colombia", "Czech Republic", "Czechia", "Denmark", "Estonia", "Finland",
                       "France", "Germany", "Greece", "Hungary", "Iceland",
                       "Ireland", "Israel", "Italy", "Japan", "South Korea", "Korea, South", "Korea",
                       "Latvia", "Lithuania", "Luxembourg", "Mexico", "Netherlands",
                       "New Zealand", "Norway", "Poland", "Portugal", "Slovak Republic", "Slovakia",
                       "Slovenia", "Spain", "Sweden", "Switzerland", "Turkey",
                       "United Kingdom", "UK", "United States", "US")

# Bestätigte Fälle 2020 laden, selektieren und gruppieren
covid_confirmed <- read.csv("COVID_CONFIRMED_09_02_2021.csv")
covid_confirmed_oecd <- as.data.frame(covid_confirmed %>% select(Country.Region, 5:349) %>% 
                                        group_by(Country.Region) %>% summarise_each(funs(sum)))
covid_confirmed_oecd <- covid_confirmed_oecd[covid_confirmed_oecd$Country.Region %in% list_of_countries, ]

# Aggregierte Bestätigte Fälle der OECD-Länder berechnen
covid_confirmed_oecd_aggregiert <- covid_confirmed_oecd %>% select(2:346) %>%
  summarise_each(funs(sum))
covid_confirmed_oecd_aggregiert <- gather(covid_confirmed_oecd_aggregiert, key = "Datum", 
                                          value = "Anzahl")
date <- seq(as.Date("2020/01/22"), by = "day", length.out = 345)
covid_confirmed_oecd_aggregiert <- data.frame(date, covid_confirmed_oecd_aggregiert$Anzahl)
names(covid_confirmed_oecd_aggregiert) <-  c("Datum", "Anzahl")

# Genesene Fälle 2020 laden, selektieren und gruppieren
covid_recovered <- read.csv("COVID_RECOVERED_09_02_2021.csv")
covid_recovered_oecd <- as.data.frame(covid_recovered %>% select(Country.Region, 5:349) %>% 
                                        group_by(Country.Region) %>% summarise_each(funs(sum)))
covid_recovered_oecd <- covid_recovered_oecd[covid_recovered_oecd$Country.Region %in% list_of_countries, ]

# Zahl der Genesenen für USA und Belgien fortführen
covid_recovered_oecd[37,328:346] <- 6298082
covid_recovered_oecd[3,296:346] <- 31130

# Aggregierte Genesene Fälle der OECD-Länder berechnen
covid_recovered_oecd_aggregiert <- covid_recovered_oecd %>% select(2:346) %>%
  summarise_each(funs(sum))
covid_recovered_oecd_aggregiert <- gather(covid_recovered_oecd_aggregiert, key = "Datum", 
                                          value = "Anzahl")
covid_recovered_oecd_aggregiert <- data.frame(date, covid_recovered_oecd_aggregiert$Anzahl)
names(covid_recovered_oecd_aggregiert) <-  c("Datum", "Anzahl")

# Bestätigte Todesfälle 2020 laden, selektieren und gruppieren
covid_deaths <- read.csv("COVID_DEATHS_09_02_2021.csv")
covid_deaths_oecd <- as.data.frame(covid_deaths %>% select(Country.Region, 5:349) %>% 
                                     group_by(Country.Region) %>% summarise_each(funs(sum)))
covid_deaths_oecd <- covid_deaths_oecd[covid_deaths_oecd$Country.Region %in% list_of_countries, ]

# Aggregierte Todesfälle der OECD-Länder berechnen
covid_deaths_oecd_aggregiert <- covid_deaths_oecd %>% select(2:346) %>%
  summarise_each(funs(sum))
covid_deaths_oecd_aggregiert <- gather(covid_deaths_oecd_aggregiert, key = "Datum", 
                                       value = "Anzahl")
date <- seq(as.Date("2020/01/22"), by = "day", length.out = 345)
covid_deaths_oecd_aggregiert <- data.frame(date, covid_deaths_oecd_aggregiert$Anzahl)
names(covid_deaths_oecd_aggregiert) <-  c("Datum", "Anzahl")

# Aktive Fälle der OECD-Länder berechnen
covid_active_oecd_aggregiert <- (covid_confirmed_oecd_aggregiert$Anzahl 
                                 - covid_recovered_oecd_aggregiert$Anzahl
                                 - covid_deaths_oecd_aggregiert$Anzahl)
covid_active_oecd_aggregiert <- data.frame(date, covid_active_oecd_aggregiert)
names(covid_active_oecd_aggregiert) <-  c("Datum", "Anzahl")

Abbildung_1_Daten <- data.frame(date, covid_confirmed_oecd_aggregiert$Anzahl,
                          covid_recovered_oecd_aggregiert$Anzahl,
                          covid_deaths_oecd_aggregiert$Anzahl,
                          covid_active_oecd_aggregiert$Anzahl)

names(Abbildung_1_Daten) <-  c("Datum", "Bestätigte Fälle", "Genesene", "Todesfälle", "Aktive Fälle")

library(reshape2)
Abbildung_1_Daten <- reshape2::melt(Abbildung_1_Daten, id.var = "Datum")
detach("package:reshape2", unload=TRUE)
Abbildung_1_Daten$variable <- factor(Abbildung_1_Daten$variable, levels = c("Bestätigte Fälle", 
                                                                      "Aktive Fälle", 
                                                                      "Genesene",
                                                                      "Todesfälle"))

# Abbildung 1 erstellen
Abbildung_1 <- ggplot(Abbildung_1_Daten, aes(x = Datum, y = value/1000000)) + 
  geom_line(aes(color = variable), size=1)+
  scale_color_manual(values = c("#780375", "#C7198C", "#FB66A0", "#F4A3B6"))+
  scale_y_continuous(label = unit_format(unit = "Mio."))+
  theme(plot.title = element_text(hjust = 0.5, size = (20)),
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(),
        axis.text = element_text(face = "plain", size = (10)),
        legend.title = element_blank(),
        legend.text = element_text(face = "plain", size = (10)),
        legend.position = "bottom")+
        scale_x_date(date_breaks = "2 month",
                     date_labels = "%b. %Y",
                     limits = as.Date(c('2020-01-01','2020-12-31')))

Abbildung_1

ggsave(Abbildung_1, filename = 'Abbildung 1.png', dpi = 300, type = 'cairo',
       width = 8, height = 5, units = 'in')

# Akive COVID-19-Fälle pro 100 Tsd. Einwohner (Tabelle 1)
covid_active_oecd <- as.data.frame(covid_confirmed_oecd$Country.Region)
names(covid_active_oecd) <- c("Land")
covid_active_oecd$active_31.03.2020 <- covid_confirmed_oecd$X3.31.20 - covid_recovered_oecd$X3.31.20 - covid_deaths_oecd$X3.31.20
covid_active_oecd$active_30.06.2020 <- covid_confirmed_oecd$X6.30.20 - covid_recovered_oecd$X6.30.20 - covid_deaths_oecd$X6.30.20
covid_active_oecd$active_30.09.2020 <- covid_confirmed_oecd$X9.30.20 - covid_recovered_oecd$X9.30.20 - covid_deaths_oecd$X9.30.20
covid_active_oecd$active_31.12.2020 <- covid_confirmed_oecd$X12.31.20 - covid_recovered_oecd$X12.31.20 - covid_deaths_oecd$X12.31.20

bevoelkerung_oecd <- read_xlsx("WELTBEVOELKERUNG.xlsx")
library(plyr)
covid_active_oecd <- join(covid_active_oecd, bevoelkerung_oecd, by="Land")
detach("package:plyr", unload=TRUE)

covid_active_oecd_100k <- as.data.frame(covid_active_oecd$Land)
names(covid_active_oecd_100k) <- c("Land")
covid_active_oecd_100k$active_31.03.2020 <- covid_active_oecd$active_31.03.2020 / covid_active_oecd$Bevoelkerungszahl*100000
covid_active_oecd_100k$active_30.06.2020 <- covid_active_oecd$active_30.06.2020 / covid_active_oecd$Bevoelkerungszahl*100000
covid_active_oecd_100k$active_30.09.2020 <- covid_active_oecd$active_30.09.2020 / covid_active_oecd$Bevoelkerungszahl*100000
covid_active_oecd_100k$active_31.12.2020 <- covid_active_oecd$active_31.12.2020 / covid_active_oecd$Bevoelkerungszahl*100000

write_xlsx(covid_active_oecd_100k, "Tabelle_1.xlsx")

# Tabelle 2
covid_policy_tracker <- read.csv("COVID_POLICY_TRACKER_09_02_2021.csv")
covid_policy_tracker <- filter(covid_policy_tracker, Jurisdiction == "NAT_TOTAL")

covid_policy_tracker <- filter(covid_policy_tracker, CountryName %in% list_of_countries)
covid_policy_tracker_Q1 <- filter(covid_policy_tracker, Date == "20200331")
covid_policy_tracker_Q2 <- filter(covid_policy_tracker, Date == "20200630")
covid_policy_tracker_Q3 <- filter(covid_policy_tracker, Date == "20200930")
covid_policy_tracker_Q4 <- filter(covid_policy_tracker, Date == "20201231")

government_response_index_oecd <- as.data.frame(covid_policy_tracker_Q1$CountryName)
names(government_response_index_oecd) <- c("Land")
government_response_index_oecd$Q1 <- covid_policy_tracker_Q1$GovernmentResponseIndex
government_response_index_oecd$Q2 <- covid_policy_tracker_Q2$GovernmentResponseIndex
government_response_index_oecd$Q3 <- covid_policy_tracker_Q3$GovernmentResponseIndex
government_response_index_oecd$Q4 <- covid_policy_tracker_Q4$GovernmentResponseIndex

write_xlsx(government_response_index_oecd, "Tabelle_2.xlsx")

## Total bestätigte Fälle weltweit per 31.12.2020
total_covid <- covid_confirmed$X12.31.20
sum(total_covid)

## Total Todesfälle weltweit per 31.12.2020
total_deaths <- covid_deaths$X12.31.20
sum(total_deaths)

