# Analyse der Aktienperformance während der COVID-19-Pandemie

Dieses Repository enthält den R-Code für die Arbeit "Aktienperformance-in-der-COVID-19-Pandemie - eine Analyse von Industriezweigen in OECD-Ländern".

## Author

Sandro Gassner

## Datum

25.01.2021

## Methodik

### Datenerhebung
Die Analyse basiert auf historischen Aktienkursdaten, die vom Bloomberg Terminal am 4. Januar 2021 und 1. Februar 2021 abgerufen wurden. Der Datensatz umfasst die 1.000 größten Unternehmen nach Marktkapitalisierung in jedem OECD-Land, was zu insgesamt 7.725 Unternehmen führt.

### Abhängige Variable
Die abhängige Variable ist die Abweichung der Aktienperformance während der COVID-19-Pandemie im Jahr 2020 im Vergleich zu einem prognostizierten Trend basierend auf den letzten 10 Jahren. Diese Abweichung wird auf Quartalsbasis gemessen.

### Unabhängige Variablen
Zur Erklärung der Performanceabweichungen werden folgende Variablen herangezogen:

- **Home-Office-Potential**: Basierend auf der Arbeit von Dingel & Neiman (2020), die das Home-Office-Potential für verschiedene Industriezweige gemäß NAICS-Klassifikation berechnet haben. Diese Werte wurden auf BICS-Sektoren übertragen.
- **Marktkapitalisierung**: Gemessen in 100 Millionen US-Dollar, approximiert per 4. Januar 2021.
- **Verschuldungsgrad (Debt-to-Equity Ratio)**: Verwendet wurden industriespezifische Durchschnittswerte basierend auf Daten von Avdeev & Co. (2019), umgerechnet von SIC auf BICS.
- **Liquiditätsgrad 2 (Quick Ratio)**: Ebenfalls basierend auf den Daten von Avdeev & Co. (2019), umgerechnet von SIC auf BICS.

### Empirischer Ansatz
Ein multiples lineares Regressionsmodell wird verwendet, um die Abweichungen der Aktienperformance während der Pandemie zu erklären. Die abhängige Variable ist die Performanceabweichung, während die unabhängigen Variablen das Home-Office-Potential, die Marktkapitalisierung, der Verschuldungsgrad und der Liquiditätsgrad 2 sind.

### Regressionsmodell
Das Regressionsmodell zur Erklärung der Performanceabweichung (\(\Delta Performance_{i,t}\)) lautet:

\[
\Delta Performance_{i,t} = \beta_0 + \beta_1 \cdot HOME_i + \beta_2 \cdot MKTCAP_i + \beta_3 \cdot DE\_RATIO_i + \beta_4 \cdot Q\_RATIO_i + u_i
\]

## Datenquellen

Die Datenquellen umfassen:

1. **Bloomberg Terminal**: Historische Aktienkurse und Marktkapitalisierungsdaten.
2. **Dingel & Neiman (2020)**: Home-Office-Potential basierend auf O*NET-Datenbank und NAICS-Klassifikation.
3. **Avdeev & Co. (2019)**: Verschuldungsgrad und Liquiditätsgrad basierend auf SIC-Klassifikation, umgerechnet auf BICS.
4. **OANDA Business Information & Services Inc.**: Historische Wechselkurse zur Berechnung der Marktkapitalisierung in US-Dollar.
5. **Johns Hopkins University (CSSE)**: COVID-19-Fallzahlen.
6. **Blavatnik School of Government, University of Oxford**: Government Response Tracker.
7. **United Nations**: Bevölkerungsdaten.

## Verwendete Skripts

### `R_CODE_AKTIEN.R`
Dieses Skript umfasst die Berechnungen und Abbildungen zur Aktienperformance. Es bereitet die Daten vor, berechnet die Quartalsperformance, schätzt historische Trends und führt die Regressionsanalyse durch.

### `R_CODE_COVID.R`
Dieses Skript umfasst die Berechnungen und Abbildungen zu den COVID-19-Fallzahlen und politischen Maßnahmen in den OECD-Ländern. Es bereitet die COVID-19-Daten auf, aggregiert sie und erstellt entsprechende Grafiken.

### `R_CODE_SIC_NAICS_MAPPING.R`
Dieses Skript führt das Mapping von SIC- zu NAICS- und BICS-Klassifikationen durch. Es liest die Mapping-Daten ein, verknüpft sie und speichert die gemappten Daten.
