library(tidyverse)
library(here)
library(countrycode)
library(data.table)
library(plotly)
library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(shinyjs)
library(shinydashboard)

dim <- read.csv(here("dimensions", "six_dimenstions.csv"))

ses <- read.csv(here("ses", "soci_econ_country_profiles.csv"))

life <- read.csv(here("life", "who_life_exp.csv"))

covid <- read.csv(here("covid","worldometer_coronavirus_summary_data.csv"))

happy <- read.csv(here("happiness", "world_happiness_report.csv"))

suicide <- read.csv(here("suicide", "suicide_rate.csv"))

# Parse out cultural dimensions data
dim <- dim %>%
  separate(ctr.country.pdi.idv.mas.uai.ltowvs.ivr, 
           c("ctr", "country", "pdi", "idv", "mas", "uai", "lto", "ivr"),
           sep = ";")

dim$idv <- as.numeric(dim$idv)# Changes individualism class to numeric

# Selecting required data from each dataset

dim <- dim %>%
  select(country, idv)


ses <- ses %>%
  select(country,GDP.per.capita..current.US..)

life <- subset(life, year=="2016") # Remove rows not including year 2016

life <- life %>%
  select(country, life_expect)

covid <- covid %>%
  select(country, total_cases_per_1m_population, total_deaths_per_1m_population)


happy <- happy %>%
  select(Country.or.region, Score)


suicide <- suicide %>%
  select(Country, Sex, X2016)


# Re-naming columns of each dataset

colnames(dim)[2] <- "individualism"

colnames(ses)[2] <- "gdp_per_capita"

colnames(life)[1] <- "country"
colnames(life)[2] <- "life_expectancy"

colnames(covid)[2] <- "total_cases"
colnames(covid)[3] <- "total_deaths"

colnames(happy)[1] <- "country"
colnames(happy)[2] <- "happiness_score"

colnames(suicide)[1] <- "country"
colnames(suicide)[2] <- "sex"
colnames(suicide)[3] <- "suicide_rate"

#Converting country name to country codes

convert_code <-  function(x) {
  #converts country name to country codes
  countrycode(x, origin = "country.name", destination = "cowc")
}

dim$country <- convert_code(dim$country)

ses$country <- convert_code(ses$country)

life$country <- convert_code(life$country)

covid$country <- convert_code(covid$country)

happy$country <- convert_code(happy$country)

suicide$country <- convert_code(suicide$country)


# Adjoin data-frames with country codes

df <- inner_join(dim, ses, "country")

df <- inner_join(df, life, "country")

df<- inner_join(df, covid, "country")

df <- inner_join(df, happy, "country")

df <- inner_join(df, suicide, "country")

# Converting country codes back to country names
convert_name <- function(x) {
  #converts country code to country name
  countrycode(x, origin = "cowc", destination = "country.name")
}

df$country <- convert_name(df$country)

#Remove duplicated countries

df<- df%>% distinct(country,
                    sex,
                    .keep_all = TRUE) # Keep duplicated countries according to sex


# Delete country missing data

df <- df %>%
  na.omit(country)

#Delete individualism missing data

df <- df[df$individualism == "#NULL!", ] 
