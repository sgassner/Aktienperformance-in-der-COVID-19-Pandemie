## Berechnungen zur Aktienperformance

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

# Datensatz laden
data <- read_xlsx("AKTIEN_OECD.xlsx")

# Datensatz bereinigen
data[data==0] <- NA
data <- data[complete.cases(data), ]
data <- arrange(data, TICKER)

# Anzahl Unternehmen pro Industrie (Tabelle 3)
n_industries <- data %>% group_by(Industrie = BICS_L1) %>% summarize(Anzahl = n())
n_industries
write_xlsx(n_industries, "Tabelle_3.xlsx")

# Anzahl Unternehmen pro Land (Tabelle 4)
n_countries <- data %>% group_by(Land = COUNTRY) %>% summarize(Anzahl = n())
n_countries
write_xlsx(n_countries, "Tabelle_4.xlsx")

# Dummy Variablen für Industrien hinzufügen
data$BICS_L1 <- as.numeric(data$BICS_L1)

data$BICS10 <- 0
data$BICS11 <- 0
data$BICS12 <- 0
data$BICS13 <- 0
data$BICS14 <- 0
data$BICS15 <- 0
data$BICS16 <- 0
data$BICS17 <- 0
data$BICS18 <- 0
data$BICS19 <- 0
data$BICS20 <- 0

data$BICS10[data$BICS_L1 == 10] <- 1
data$BICS11[data$BICS_L1 == 11] <- 1
data$BICS12[data$BICS_L1 == 12] <- 1
data$BICS13[data$BICS_L1 == 13] <- 1
data$BICS14[data$BICS_L1 == 14] <- 1
data$BICS15[data$BICS_L1 == 15] <- 1
data$BICS16[data$BICS_L1 == 16] <- 1
data$BICS17[data$BICS_L1 == 17] <- 1
data$BICS18[data$BICS_L1 == 18] <- 1
data$BICS19[data$BICS_L1 == 19] <- 1
data$BICS20[data$BICS_L1 == 20] <- 1

#QOQ-Performance berechnen (in %)
data$QOQ_2010_Q2 <- ((data$'2010_Q2' - data$'2010_Q1')/data$'2010_Q1')*100
data$QOQ_2010_Q3 <- ((data$'2010_Q3' - data$'2010_Q2')/data$'2010_Q2')*100
data$QOQ_2010_Q4 <- ((data$'2010_Q4' - data$'2010_Q3')/data$'2010_Q3')*100

data$QOQ_2011_Q1 <- ((data$'2011_Q1' - data$'2010_Q4')/data$'2010_Q4')*100
data$QOQ_2011_Q2 <- ((data$'2011_Q2' - data$'2011_Q1')/data$'2011_Q1')*100
data$QOQ_2011_Q3 <- ((data$'2011_Q3' - data$'2011_Q2')/data$'2011_Q2')*100
data$QOQ_2011_Q4 <- ((data$'2011_Q4' - data$'2011_Q3')/data$'2011_Q3')*100

data$QOQ_2012_Q1 <- ((data$'2012_Q1' - data$'2011_Q4')/data$'2011_Q4')*100
data$QOQ_2012_Q2 <- ((data$'2012_Q2' - data$'2012_Q1')/data$'2012_Q1')*100
data$QOQ_2012_Q3 <- ((data$'2012_Q3' - data$'2012_Q2')/data$'2012_Q2')*100
data$QOQ_2012_Q4 <- ((data$'2012_Q4' - data$'2012_Q3')/data$'2012_Q3')*100

data$QOQ_2013_Q1 <- ((data$'2013_Q1' - data$'2012_Q4')/data$'2012_Q4')*100
data$QOQ_2013_Q2 <- ((data$'2013_Q2' - data$'2013_Q1')/data$'2013_Q1')*100
data$QOQ_2013_Q3 <- ((data$'2013_Q3' - data$'2013_Q2')/data$'2013_Q2')*100
data$QOQ_2013_Q4 <- ((data$'2013_Q4' - data$'2013_Q3')/data$'2013_Q3')*100

data$QOQ_2014_Q1 <- ((data$'2014_Q1' - data$'2013_Q4')/data$'2013_Q4')*100
data$QOQ_2014_Q2 <- ((data$'2014_Q2' - data$'2014_Q1')/data$'2014_Q1')*100
data$QOQ_2014_Q3 <- ((data$'2014_Q3' - data$'2014_Q2')/data$'2014_Q2')*100
data$QOQ_2014_Q4 <- ((data$'2014_Q4' - data$'2014_Q3')/data$'2014_Q3')*100

data$QOQ_2015_Q1 <- ((data$'2015_Q1' - data$'2014_Q4')/data$'2014_Q4')*100
data$QOQ_2015_Q2 <- ((data$'2015_Q2' - data$'2015_Q1')/data$'2015_Q1')*100
data$QOQ_2015_Q3 <- ((data$'2015_Q3' - data$'2015_Q2')/data$'2015_Q2')*100
data$QOQ_2015_Q4 <- ((data$'2015_Q4' - data$'2015_Q3')/data$'2015_Q3')*100

data$QOQ_2016_Q1 <- ((data$'2016_Q1' - data$'2015_Q4')/data$'2015_Q4')*100
data$QOQ_2016_Q2 <- ((data$'2016_Q2' - data$'2016_Q1')/data$'2016_Q1')*100
data$QOQ_2016_Q3 <- ((data$'2016_Q3' - data$'2016_Q2')/data$'2016_Q2')*100
data$QOQ_2016_Q4 <- ((data$'2016_Q4' - data$'2016_Q3')/data$'2016_Q3')*100

data$QOQ_2017_Q1 <- ((data$'2017_Q1' - data$'2016_Q4')/data$'2016_Q4')*100
data$QOQ_2017_Q2 <- ((data$'2017_Q2' - data$'2017_Q1')/data$'2017_Q1')*100
data$QOQ_2017_Q3 <- ((data$'2017_Q3' - data$'2017_Q2')/data$'2017_Q2')*100
data$QOQ_2017_Q4 <- ((data$'2017_Q4' - data$'2017_Q3')/data$'2017_Q3')*100

data$QOQ_2018_Q1 <- ((data$'2018_Q1' - data$'2017_Q4')/data$'2017_Q4')*100
data$QOQ_2018_Q2 <- ((data$'2018_Q2' - data$'2018_Q1')/data$'2018_Q1')*100
data$QOQ_2018_Q3 <- ((data$'2018_Q3' - data$'2018_Q2')/data$'2018_Q2')*100
data$QOQ_2018_Q4 <- ((data$'2018_Q4' - data$'2018_Q3')/data$'2018_Q3')*100

data$QOQ_2019_Q1 <- ((data$'2019_Q1' - data$'2018_Q4')/data$'2018_Q4')*100
data$QOQ_2019_Q2 <- ((data$'2019_Q2' - data$'2019_Q1')/data$'2019_Q1')*100
data$QOQ_2019_Q3 <- ((data$'2019_Q3' - data$'2019_Q2')/data$'2019_Q2')*100
data$QOQ_2019_Q4 <- ((data$'2019_Q4' - data$'2019_Q3')/data$'2019_Q3')*100

data$QOQ_2020_Q1 <- ((data$'2020_Q1' - data$'2019_Q4')/data$'2019_Q4')*100
data$QOQ_2020_Q2 <- ((data$'2020_Q2' - data$'2020_Q1')/data$'2020_Q1')*100
data$QOQ_2020_Q3 <- ((data$'2020_Q3' - data$'2020_Q2')/data$'2020_Q2')*100
data$QOQ_2020_Q4 <- ((data$'2020_Q4' - data$'2020_Q3')/data$'2020_Q3')*100

# Time Series für QOQ-Performance 2010-2019 (t = 1 bis 39 / 2010_Q2 bis 2019_Q4)
# und 2020 (t = 40 bis 43 / 2020_Q1 bis 2020_Q4 erstellen)

QOQ_time_series <- data %>% select(1, 64:102)
QOQ_time_series <- gather(QOQ_time_series, key = "PERIODE", value = "QOQ", c(-TICKER))

QOQ_time_series_2020 <- data %>% select(1, 103:106)
QOQ_time_series_2020 <- gather(QOQ_time_series_2020, key = "PERIODE", value = "QOQ", c(-TICKER))


QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2010_Q2"] <- 1
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2010_Q3"] <- 2
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2010_Q4"] <- 3

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2011_Q1"] <- 4
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2011_Q2"] <- 5
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2011_Q3"] <- 6
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2011_Q4"] <- 7

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2012_Q1"] <- 8
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2012_Q2"] <- 9
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2012_Q3"] <- 10
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2012_Q4"] <- 11

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2013_Q1"] <- 12
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2013_Q2"] <- 13
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2013_Q3"] <- 14
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2013_Q4"] <- 15

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2014_Q1"] <- 16
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2014_Q2"] <- 17
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2014_Q3"] <- 18
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2014_Q4"] <- 19

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2015_Q1"] <- 20
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2015_Q2"] <- 21
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2015_Q3"] <- 22
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2015_Q4"] <- 23

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2016_Q1"] <- 24
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2016_Q2"] <- 25
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2016_Q3"] <- 26
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2016_Q4"] <- 27

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2017_Q1"] <- 28
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2017_Q2"] <- 29
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2017_Q3"] <- 30
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2017_Q4"] <- 31

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2018_Q1"] <- 32
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2018_Q2"] <- 33
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2018_Q3"] <- 34
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2018_Q4"] <- 35

QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2019_Q1"] <- 36
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2019_Q2"] <- 37
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2019_Q3"] <- 38
QOQ_time_series$PERIODE[QOQ_time_series$PERIODE == "QOQ_2019_Q4"] <- 39

QOQ_time_series_2020$PERIODE[QOQ_time_series_2020$PERIODE == "QOQ_2020_Q1"] <- 40
QOQ_time_series_2020$PERIODE[QOQ_time_series_2020$PERIODE == "QOQ_2020_Q2"] <- 41
QOQ_time_series_2020$PERIODE[QOQ_time_series_2020$PERIODE == "QOQ_2020_Q3"] <- 42
QOQ_time_series_2020$PERIODE[QOQ_time_series_2020$PERIODE == "QOQ_2020_Q4"] <- 43

QOQ_time_series$PERIODE <- as.numeric(QOQ_time_series$PERIODE)
QOQ_time_series <- as.data.frame(QOQ_time_series)

QOQ_time_series_2020$PERIODE <- as.numeric(QOQ_time_series_2020$PERIODE)
QOQ_time_series_2020 <- as.data.frame(QOQ_time_series_2020)

# Schätzung des Performance-Trends (Tabelle 5)
fits <- lmList(QOQ ~ PERIODE | TICKER, data=QOQ_time_series)
sumfun <- function(x) c(coef(x),summary(x)$r.squared)
lm1 <- t(sapply(fits,sumfun))
lm1 <- cbind(rownames(lm1), data.frame(lm1, row.names=NULL))
names(lm1) <-  c("TICKER", "ACHSENABSCHNITT","STEIGUNG","R_SQUARED")

write_xlsx(lm1, "Tabelle_5.xlsx")


# Achsenabschnitt und Steigung der Regression 1 mit Datensatz verknüpfen
library(plyr)
data <- join(data, lm1, by="TICKER")
detach("package:plyr", unload=TRUE)

# Prognose für 2020 Q1-Q4
data$EST_QOQ_Q1_2020 <- (data$ACHSENABSCHNITT + (data$STEIGUNG * 40))
data$EST_QOQ_Q2_2020 <- (data$ACHSENABSCHNITT + (data$STEIGUNG * 41))
data$EST_QOQ_Q3_2020 <- (data$ACHSENABSCHNITT + (data$STEIGUNG * 42))
data$EST_QOQ_Q4_2020 <- (data$ACHSENABSCHNITT + (data$STEIGUNG * 43))

# Performance-Deltas berechnen
data$DELTA_QOQ_Q1_2020 <- (data$QOQ_2020_Q1 - data$EST_QOQ_Q1_2020)
data$DELTA_QOQ_Q2_2020 <- (data$QOQ_2020_Q2 - data$EST_QOQ_Q2_2020)
data$DELTA_QOQ_Q3_2020 <- (data$QOQ_2020_Q3 - data$EST_QOQ_Q3_2020)
data$DELTA_QOQ_Q4_2020 <- (data$QOQ_2020_Q4 - data$EST_QOQ_Q4_2020)

# Durchschnittliches Performance-Delta pro Industrie und Quartal (Tabelle 6)
DELTA_QOQ_AVG_2020_INDUSTRY <- data %>% group_by(Industrie = BICS_L1) %>% 
  summarize(Delta_Q1 = mean(DELTA_QOQ_Q1_2020),
            Delta_Q2 = mean(DELTA_QOQ_Q2_2020),
            Delta_Q3 = mean(DELTA_QOQ_Q3_2020),
            Delta_Q4 = mean(DELTA_QOQ_Q4_2020))

DELTA_QOQ_AVG_2020_INDUSTRY
avg_delta <- summarize_all(DELTA_QOQ_AVG_2020_INDUSTRY, mean)
DELTA_QOQ_AVG_2020_INDUSTRY <- rbind(DELTA_QOQ_AVG_2020_INDUSTRY, avg_delta) 
DELTA_QOQ_AVG_2020_INDUSTRY[12,1] <- 0 # 0 = Durchschnitt über alle BICS
DELTA_QOQ_AVG_2020_INDUSTRY

write_xlsx(DELTA_QOQ_AVG_2020_INDUSTRY, "Tabelle_10.xlsx")


## Time Series Regression Beispiel
trend_example <- as.data.frame(QOQ_time_series[grep("LHA GR Equity", QOQ_time_series$TICKER), ])
trend_example_2020 <- as.data.frame(QOQ_time_series_2020[grep("LHA GR Equity", QOQ_time_series_2020$TICKER), ])

fits2 <- lmList(QOQ ~ PERIODE | TICKER, data=trend_example)

library(ggpubr)

Abbildung_2 <- 
  ggplot(trend_example ,aes(x = PERIODE, y = QOQ)) + 
  geom_point(color="#2a86c7", size=2) + 
  geom_smooth(method = "lm", fullrange=TRUE, color="#244990")+
  stat_regline_equation(label.y = -70, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = -90, aes(label = ..rr.label..)) +
  xlim(0,44) +
  ylim(-100,+100)+
  labs(y="Quartalsperformance (%)  \n  ", x = "  \n  Beobachtungsperiode (2010-2020)") +
  theme(plot.title = element_text(face = "bold", size = (20)), 
        legend.title = element_blank(), 
        legend.text = element_text(face = "plain", size = (10)), 
        axis.title = element_text(size = (12)),
        axis.text = element_text(face = "plain", size = (10)))+
  geom_point(data=trend_example_2020, aes(x=PERIODE, y=QOQ), color="#C7198C", size=2) 

Abbildung_2

ggsave(Abbildung_2, filename = 'Abbildung 2.png', dpi = 300, type = 'cairo',
       width = 6.3, height = 3.6, units = 'in')

# USD Wechselkurse zum Datensatz hinzufügen
wechselkurse <- read_xlsx("WECHSELKURSE_19_01_2021.xlsx")
wechselkurse$`CRNCY/USD_04.01.2021` <- as.numeric(wechselkurse$`CRNCY/USD_04.01.2021`)
library(plyr)
data <- join(data, wechselkurse, by="EQY_FUND_CRNCY")
detach("package:plyr", unload=TRUE)

# Marktkapitalisierung in 100 Mia. USD berechnen
data$MKTCAP_USD <- data$MKTCAP * data$`CRNCY/USD_04.01.2021` / 1000000000000

# BICS Level 2 aus BICS Level 3 erstellen
data$BICS_L2 <- sub("^(\\d{4}).*$", "\\1", data$BICS_L3)

# Heterogenitätsfaktoren zum Datensatz hinzufügen
heterogenitaetsfaktoren <- read_xlsx("RATIOS_BICS.xlsx")
library(plyr)
data <- join(data, heterogenitaetsfaktoren, by="BICS_L2")
detach("package:plyr", unload=TRUE)

# Home-Office-Potential pro BICS Level 1 Sektor
home_office_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(HOME = mean(HOME))

write_xlsx(home_office_bics_level_1, "Tabelle_6.xlsx")

# Debt-to-Equity Ratio pro BICS Level 1 Sektor
de_ratio_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(DE_RATIO = mean(DE_RATIO))

write_xlsx(de_ratio_bics_level_1, "Tabelle_7.xlsx")

# Debt-to-Equity Ratio pro BICS Level 1 Sektor
q_ratio_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(Q_RATIO = mean(Q_RATIO))

write_xlsx(q_ratio_bics_level_1, "Tabelle_8.xlsx")

## Regression 2: Einfluss der Heterogenitätsfaktoren
lm2 <- lm(formula = DELTA_QOQ_Q1_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = data)

lm3 <- lm(formula = DELTA_QOQ_Q2_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = data)

lm4 <- lm(formula = DELTA_QOQ_Q3_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = data)

lm5 <- lm(formula = DELTA_QOQ_Q4_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = data)

library(memisc)
library(stargazer)

summary_lm_2_5 <- mtable('Model Q1' = lm2,
                         'Model Q2' = lm3,
                         'Model Q3' = lm4,
                         'Model Q4' = lm5,
                         summary.stats = c('R-squared','F','p','N'))

write.mtable(summary_lm_2_5,file="Tabelle_9.txt")
lm_2_5_txt <- read.delim("Tabelle_9.txt")
lm_2_5_txt %>% knitr::kable(caption = "Regression")
write_xlsx(lm_2_5_txt, "Tabelle_9.xlsx")

detach("package:memisc", unload=TRUE)
detach("package:stargazer", unload=TRUE)

# Grafik Performanceabweichung (Abbildung 3)
industry_names <- c("Communications", "Consumer Discretionary", 
                    "Consumer Staples", "Energy", 
                    "Financials", "Health Care",
                    "Industrials", "Materials", 
                    "Technology", "Utilities",
                    "Government", "Durchschnitt")

library(reshape2)
DELTA_QOQ_AVG_2020_INDUSTRY_long <- DELTA_QOQ_AVG_2020_INDUSTRY
names(DELTA_QOQ_AVG_2020_INDUSTRY_long) <- c("Industrie", "Q1 2020", "Q2 2020", "Q3 2020", "Q4 2020")
DELTA_QOQ_AVG_2020_INDUSTRY_long$Industrie <- industry_names
DELTA_QOQ_AVG_2020_INDUSTRY_long <- reshape2::melt(DELTA_QOQ_AVG_2020_INDUSTRY_long, id.var = "Industrie")
detach("package:reshape2", unload=TRUE)
DELTA_QOQ_AVG_2020_INDUSTRY_long$Industrie <- factor(DELTA_QOQ_AVG_2020_INDUSTRY_long$Industrie, 
                                                    levels = c("Durchschnitt","Government", "Utilities", "Technology",
                                                               "Materials", "Industrials","Health Care",
                                                               "Financials", "Energy", "Consumer Staples",
                                                               "Consumer Discretionary", "Communications"))

Abbildung_3 <- 
  ggplot(DELTA_QOQ_AVG_2020_INDUSTRY_long, aes(Industrie, value)) +
  geom_line(aes(group = Industrie), size=1.5, color = "grey") +
  geom_hline(yintercept=0, linetype="dashed", color = "black", size=1) +
  geom_point(aes(color = variable), size=3.5)+
  coord_flip()+
  scale_color_manual(values = c("#244990", "#2a86c7", "#74c8ca", "#b4e4bc"))+
  ggtitle("Performanceabweichung  \n  ") +
  labs(y="\n ??? Performance (%-Punkte) \n  ", x = "  \n Industrie") +
  theme(plot.title = element_blank(),
        axis.title.y = element_blank(),
        legend.title = element_blank(), 
        legend.text = element_text(face = "plain", size = (12)),
        legend.position = "right",
        axis.title = element_text(size = (15)),
        axis.text = element_text(face = "plain", size = (12)))+
  scale_y_continuous(limits = c(-50, 50))

Abbildung_3

ggsave(Abbildung_3, filename = 'Abbildung_3.png', dpi = 300, type = 'cairo',
       width = 8, height = 6, units = 'in')

