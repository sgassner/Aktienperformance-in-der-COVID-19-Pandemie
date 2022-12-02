# Berechnungen zur Aktienperformance

# Notwendige Packages laden
library(readxl)
library(writexl)
library(dplyr)
library(tidyr)
library(lme4)
library(lattice)
library(ggplot2)
library(scales)
library(ggpubr)
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

# USD Wechselkurse zum Datensatz hinzufügen
wechselkurse <- read_xlsx("WECHSELKURSE_19_01_2021.xlsx")
data <- data %>% inner_join(wechselkurse, by = c("EQY_FUND_CRNCY" = "EQY_FUND_CRNCY"))

# Marktkapitalisierung in 100 Mio. USD berechnen
data <- data %>% mutate(MKTCAP_USD = MKTCAP * USD_04_01_2021 / 100000000)

# BICS Level 2 aus BICS Level 3 erstellen
data$BICS_L2 <- sub("^(\\d{4}).*$", "\\1", data$BICS_L3)
data$BICS_L2 <- as.double(data$BICS_L2)

# Heterogenitätsfaktoren zum Datensatz hinzufügen
heterogenitaetsfaktoren <- read_xlsx("RATIOS_BICS.xlsx")
data <- data %>% inner_join(heterogenitaetsfaktoren, by = c("BICS_L2" = "BICS_L2"))

# Datensatz ins Long-Format umwandeln
data_long <- data
names(data_long) <- c("TICKER", "SHORT_NAME", "BICS_L1", "BICS_L1_NAME", "BICS_L3", 
                      "EQY_FUND_CRNCY", "MKTCAP", "COUNTRY", "0", "1", "2", "3", 
                      "4", "5", "6", "7", "8", "9", "10", "11", "12", "13", "14",
                      "15", "16", "17", "18", "19", "20", "21", "22", "23", "24",
                      "25", "26", "27", "28", "29", "30", "31", "32", "33", "34", 
                      "35", "36", "37", "38", "39", "40", "41", "42", "43", 
                      "USD_31_03_2020", "USD_30_06_2020", "USD_30_09_2020", 
                      "USD_31_12_2020", "USD_04_01_2021", "MKTCAP_USD", 
                      "BICS_L2", "HOME", "DE_RATIO", "Q_RATIO")

data_long <- data_long %>% gather(QUARTAL, PRICE, -c(TICKER, SHORT_NAME, BICS_L1, BICS_L1_NAME, BICS_L3, EQY_FUND_CRNCY, MKTCAP, COUNTRY,
                                                     USD_30_06_2020, USD_30_09_2020, USD_31_12_2020, USD_04_01_2021,
                                                     MKTCAP_USD, BICS_L2, HOME, DE_RATIO, Q_RATIO))

#QOQ-Performance berechnen (in %)
data_long <- data_long %>% group_by(TICKER) %>% mutate(QOQ = (((PRICE / lag(PRICE))-1)*100)) %>% ungroup()

# Data Cleaning
data_long <- data_long[complete.cases(data_long), ]
data_long$QUARTAL <- as.double(data_long$QUARTAL)

# Time Series für QOQ-Performance 2010-2019 (t = 1 bis 39 / 2010_Q2 bis 2019_Q4)
# und 2020 (t = 40 bis 43 / 2020_Q1 bis 2020_Q4 erstellen)
QOQ_time_series <- data_long %>% filter(QUARTAL %in% c(1:39))

# Schätzung des Performance-Trends (Tabelle 5)
fits <- lmList(QOQ ~ QUARTAL | TICKER, data = QOQ_time_series)
sumfun <- function(x) c(coef(x), summary(x)$r.squared)
lm1 <- t(sapply(fits, sumfun))
lm1 <- cbind(rownames(lm1), data.frame(lm1, row.names=NULL))
names(lm1) <-  c("TICKER", "ACHSENABSCHNITT","STEIGUNG","R_SQUARED")

write_xlsx(lm1, "Tabelle_5.xlsx")

# Achsenabschnitt und Steigung der Regression 1 mit Datensatz verknüpfen
data_long <- data_long %>% inner_join(lm1, by = c("TICKER" = "TICKER"))

# Prognose für 2020 Q1-Q4
prognose_Q1_2020 <- data_long %>% filter(QUARTAL == 40) %>% mutate(EST_QOQ_Q1_2020 = ACHSENABSCHNITT + (STEIGUNG * 40))
prognose_Q2_2020 <- data_long %>% filter(QUARTAL == 41) %>% mutate(EST_QOQ_Q2_2020 = ACHSENABSCHNITT + (STEIGUNG * 41))
prognose_Q3_2020 <- data_long %>% filter(QUARTAL == 42) %>% mutate(EST_QOQ_Q3_2020 = ACHSENABSCHNITT + (STEIGUNG * 42))
prognose_Q4_2020 <- data_long %>% filter(QUARTAL == 43) %>% mutate(EST_QOQ_Q4_2020 = ACHSENABSCHNITT + (STEIGUNG * 43))

# Performance-Deltas berechnen
prognose_Q1_2020 <- prognose_Q1_2020 %>% mutate(DELTA_QOQ_Q1_2020 = QOQ - EST_QOQ_Q1_2020)
prognose_Q2_2020 <- prognose_Q2_2020 %>% mutate(DELTA_QOQ_Q2_2020 = QOQ - EST_QOQ_Q2_2020)
prognose_Q3_2020 <- prognose_Q3_2020 %>% mutate(DELTA_QOQ_Q3_2020 = QOQ - EST_QOQ_Q3_2020)
prognose_Q4_2020 <- prognose_Q4_2020 %>% mutate(DELTA_QOQ_Q4_2020 = QOQ - EST_QOQ_Q4_2020)

# Durchschnittliches Performance-Delta pro Industrie und Quartal (Tabelle 10)
deltas_Q1 <- prognose_Q1_2020 %>% group_by(Industrie = BICS_L1) %>% summarize(Delta_Q1 = mean(DELTA_QOQ_Q1_2020))
deltas_Q2 <- prognose_Q2_2020 %>% group_by(Industrie = BICS_L1) %>% summarize(Delta_Q2 = mean(DELTA_QOQ_Q2_2020))  
deltas_Q3 <- prognose_Q3_2020 %>% group_by(Industrie = BICS_L1) %>% summarize(Delta_Q3 = mean(DELTA_QOQ_Q3_2020))
deltas_Q4 <- prognose_Q4_2020 %>% group_by(Industrie = BICS_L1) %>% summarize(Delta_Q4 = mean(DELTA_QOQ_Q4_2020))

deltas_Q1_Q4 <- deltas_Q1 %>% inner_join(deltas_Q2, by = c("Industrie" = "Industrie")) %>% 
                              inner_join(deltas_Q3, by = c("Industrie" = "Industrie")) %>% 
                              inner_join(deltas_Q4, by = c("Industrie" = "Industrie"))  

avg_delta <- summarize_all(deltas_Q1_Q4, mean)
DELTA_QOQ_AVG_2020_INDUSTRY <- rbind(deltas_Q1_Q4, avg_delta) 
DELTA_QOQ_AVG_2020_INDUSTRY[12,1] <- "Durchschnitt"

write_xlsx(DELTA_QOQ_AVG_2020_INDUSTRY, "Tabelle_10.xlsx")


# Beispiel Lufthansa (Abbildung 2)
trend_example <- data_long %>% filter(TICKER == "LHA GR Equity", QUARTAL %in% c(1:39)) %>% ungroup()
trend_example_2020 <- data_long %>% filter(TICKER == "LHA GR Equity", QUARTAL %in% c(40:43)) %>% ungroup()

fits2 <- lmList(QOQ ~ QUARTAL | TICKER, data = trend_example)

Abbildung_2 <- 
  ggplot(trend_example ,aes(x = QUARTAL, y = QOQ)) + 
  geom_point(color="#2a86c7", size=2) + 
  geom_smooth(method = "lm", fullrange=TRUE, color="#244990")+
  stat_regline_equation(label.y = -70, aes(label = ..eq.label..)) +
  stat_regline_equation(label.y = -90, aes(label = ..rr.label..)) +
  xlim(0,44) +
  ylim(-100,+100) +
  labs(y="Quartalsperformance (%)  \n  ", x = "  \n  Beobachtungsperiode (2010-2020)") +
  theme(plot.title = element_text(face = "bold", size = (20)), 
        legend.title = element_blank(), 
        legend.text = element_text(face = "plain", size = (10)), 
        axis.title = element_text(size = (12)),
        axis.text = element_text(face = "plain", size = (10)))+
  geom_point(data=trend_example_2020, aes(x=QUARTAL, y=QOQ), color="#C7198C", size=2) 

Abbildung_2

ggsave(Abbildung_2, filename = 'Abbildung 2.png', dpi = 300, type = 'cairo',
       width = 6.3, height = 3.6, units = 'in')

# Home-Office-Potential pro BICS Level 1 Sektor (Tabelle 6)
home_office_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(HOME = mean(HOME))

write_xlsx(home_office_bics_level_1, "Tabelle_6.xlsx")

# Debt-to-Equity Ratio pro BICS Level 1 Sektor (Tabelle 7)
de_ratio_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(DE_RATIO = mean(DE_RATIO))

write_xlsx(de_ratio_bics_level_1, "Tabelle_7.xlsx")

# Debt-to-Equity Ratio pro BICS Level 1 Sektor (Tabelle 8)
q_ratio_bics_level_1 <- data %>% group_by(BICS_L1) %>% 
  summarize(Q_RATIO = mean(Q_RATIO))

write_xlsx(q_ratio_bics_level_1, "Tabelle_8.xlsx")

# Regression 2: Einfluss der Heterogenitätsfaktoren
lm2 <- lm(formula = DELTA_QOQ_Q1_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = prognose_Q1_2020)

lm3 <- lm(formula = DELTA_QOQ_Q2_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = prognose_Q2_2020)

lm4 <- lm(formula = DELTA_QOQ_Q3_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = prognose_Q3_2020)

lm5 <- lm(formula = DELTA_QOQ_Q4_2020 ~ MKTCAP_USD + HOME +
            DE_RATIO + Q_RATIO,
          data = prognose_Q4_2020)

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

