library(tidyverse)
library(here)
library(countrycode)
library(data.table)
library(plotly)
library(shiny)
library(ggplot2)


## Loading Data
# Load in Covid data
covid <- read.csv(here("covid", "worldometer_coronavirus_summary_data.csv"))

# Load in cultural dimensions data
dim <- read.csv(here("dimensions", "six_dimenstions.csv"))

# Load socioeconomic data
ses <- read.csv(here("ses", "soci_econ_country_profiles.csv"))

# Load happiness data
happy <- read.csv(here("happiness", "world_happiness_report.csv"))

# Load life expectancy data
life <- read.csv(here("life", "who_life_exp.csv"))

# Load suicide data
suicide <- read.csv(here("suicide", "suicide_rate.csv"))

## Data Modification
# Parse out cultural dimensions data
dim <- dim %>%
    separate(ctr.country.pdi.idv.mas.uai.ltowvs.ivr, 
             c("ctr", "country", "pdi", "idv", "mas", "uai", "lto", "ivr"),
             sep = ";")

## Selecting required Data
# Selecting wanted Covid data

covid <- covid %>%
    select(country, total_cases_per_1m_population, total_deaths_per_1m_population)

# Selecting wanted cultural dimension data

dim <- dim %>%
    select(country, idv)

# Selecting wanted socioeconomic data

ses <- ses %>%
    select(country,GDP.per.capita..current.US..)

# Selecting wanted happiness data

happy <- happy %>%
    select(Country.or.region, Score, Overall.rank)

#Selecting wanted life exp data

life <- life %>%
    select(country, year, life_expect)

life <- subset(life, year=="2016")

#Select wanted suicide data

suicide <- suicide %>%
    select(Country, Sex, X2016)

## Re-naming Columns
# Re-naming Covid columns
colnames(covid)[2] <- "total_cases"
colnames(covid)[3] <- "total_deaths"

#Re-naming cultural dimension column
colnames(dim)[2] <- "individualism"

#Re-naming socio-economic column
colnames(ses)[2] <- "gdp_per_capita"

#Re-naming happiness columns
colnames(happy)[1] <- "country"
colnames(happy)[2] <- "happiness_score"
colnames(happy)[3] <- "rank"

#Re-naming life columns
colnames(life)[1] <- "country"
colnames(life)[2] <- "year"
colnames(life)[3] <- "life_expectancy"

#Re-naming suicide columns
colnames(suicide)[1] <- "country"
colnames(suicide)[2] <- "sex"
colnames(suicide)[3] <- "suicide_rate"

##Merging data into one data-frame
#Converting country ID to country country codes

ses$country <- countrycode(ses$country, origin = "country.name", destination = "cowc")

dim$country <- countrycode(dim$country, origin = "country.name", destination = "cowc")

covid$country <- countrycode(covid$country, origin = "country.name", destination = "cowc")

happy$country <- countrycode(happy$country, origin = "country.name", destination = "cowc")

life$country <- countrycode(life$country, origin = "country.name", destination = "cowc")

suicide$country <- countrycode(suicide$country, origin = "country.name", destination = "cowc")


#Adjoin data-frames with country codes

df_covid1 <- inner_join(covid, dim, "country")

df_covid <- inner_join(df_covid1, ses, "country")

df_happiness1 <- inner_join(happy, dim, "country")

df_happiness <- inner_join(df_happiness1, ses, "country")

df_life1 <- inner_join(life, dim, "country")

df_life <- inner_join(df_life1, ses, "country")

df_suicide1 <- inner_join(suicide, dim, "country")

df_suicide <- inner_join(df_suicide1, ses, "country")

df_sui_happ1 <- inner_join(suicide, dim, "country")

df_sui_happ2 <- inner_join(df_sui_happ1, happy, "country")

df_sui_happ <- inner_join(df_sui_happ2, ses, "country")

df_wealth <- inner_join(dim, ses, "country")

#Converting country codes to names

df_covid1$country <- countrycode(df_covid1$country, origin = "cowc", destination = "country.name")

df_covid$country <- countrycode(df_covid$country, origin = "cowc", destination = "country.name")

df_happiness1$country <- countrycode(df_happiness1$country, origin = "cowc", destination = "country.name")

df_happiness$country <- countrycode(df_happiness$country, origin = "cowc", destination = "country.name")

df_life1$country <- countrycode(df_life1$country, origin = "cowc", destination = "country.name")

df_life$country <- countrycode(df_life$country, origin = "cowc", destination = "country.name")

df_suicide1$country <- countrycode(df_suicide1$country, origin = "cowc", destination = "country.name")

df_suicide$country <- countrycode(df_suicide$country, origin = "cowc", destination = "country.name")

df_sui_happ$country <- countrycode(df_sui_happ$country, origin = "cowc", destination = "country.name")

df_wealth$country <- countrycode(df_wealth$country, origin = "cowc", destination = "country.name")

## Data Cleaning
#Remove duplicated countries

df_covid1 <- df_covid1[!duplicated
                       (df_covid1[ , "country"]
                       ),]

df_covid <- df_covid[!duplicated
                     (df_covid[ , "country"
                     ]),]

df_happiness1 <- df_happiness1[!duplicated
                               (df_happiness1[ , "country"
                               ]),]

df_happiness <- df_happiness[!duplicated
                             (df_happiness[ , "country"
                             ]),]

df_life1 <- df_life1[!duplicated
                     (df_life1[ , "country"
                     ]),]

df_life <- df_life[!duplicated
                   (df_life[ , "country"
                   ]),]

df_wealth <- df_wealth[!duplicated
                       (df_wealth[ , "country"
                       ]),]


df_suicide1 <- df_suicide1 %>% distinct(country,sex, .keep_all = TRUE)


df_suicide <- df_suicide %>% distinct(country,sex, .keep_all = TRUE)

df_sui_happ <- df_sui_happ[!duplicated
                           (df_sui_happ[ , "country"
                           ]),]

#Delete country missing data

df_covid1 <- df_covid1 %>%
    na.omit(country)

df_covid <- df_covid %>%
    na.omit(country)

df_happiness1 <- df_happiness1 %>%
    na.omit(country)

df_happiness <- df_happiness %>%
    na.omit(country)

df_life1 <- df_life1 %>%
    na.omit(country)

df_life <- df_life %>%
    na.omit(country)

df_suicide1 <- df_suicide1 %>%
    na.omit(country)

df_suicide <- df_suicide %>%
    na.omit(country)

df_sui_happ <- df_sui_happ %>%
    na.omit(country)

df_wealth <- df_wealth %>%
    na.omit(country)

#Delete individualism missing data

df_covid1 <- df_covid1[!df_covid1$individualism == "#NULL!", ] 

df_covid <- df_covid[!df_covid$individualism == "#NULL!", ]

df_happiness1 <- df_happiness1[!df_happiness1$individualism == "#NULL!", ]

df_happiness <- df_happiness[!df_happiness$individualism == "#NULL!", ]

df_life1 <- df_life1[!df_life1$individualism == "#NULL!", ]

df_life <- df_life[!df_life$individualism == "#NULL!", ]

df_suicide1 <- df_suicide1[!df_suicide1$individualism == "#NULL!", ]

df_suicide <- df_suicide[!df_suicide$individualism == "#NULL!", ]

df_sui_happ <- df_sui_happ[!df_sui_happ$individualism == "#NULL!", ]

df_wealth <- df_wealth[!df_wealth$individualism == "#NULL!", ]

#Change class of individualism data

df_covid1$individualism <- as.numeric(df_covid1$individualism)

df_covid$individualism <- as.numeric(df_covid$individualism)

df_happiness1$individualism <- as.numeric(df_happiness1$individualism)

df_happiness$individualism <- as.numeric(df_happiness$individualism)

df_life1$individualism <- as.numeric(df_life1$individualism)

df_life$individualism <- as.numeric(df_life$individualism)

df_suicide1$individualism <- as.numeric(df_suicide1$individualism)

df_suicide$individualism <- as.numeric(df_suicide$individualism)

df_sui_happ$individualism <- as.numeric(df_sui_happ$individualism)

df_wealth$individualism <- as.numeric(df_wealth$individualism)

#Wealth plot (delete country for line, need to sort labels)

plotw <- ggplot(df_wealth, aes (x = individualism, y = gdp_per_capita, shape = ifelse(individualism < 50, "Collectivistic Cultures", "Individualistic Cultures"),
                                color = individualism)) +
    geom_smooth((aes(shape=NA)), method = 'lm', color = "darkgreen", alpha = 0.2) +
    geom_point(size = 4, alpha = 0.8) +
    scale_colour_gradient(low = "blue", high = "orange") +
    ggtitle(
        "Countries GDP per Capita and Cultural Orientation") +
    scale_x_continuous(name="Individualism Index",
                       breaks= seq(0, 100, 10)) +
    scale_y_continuous(name="GDP per Capita ($)",
                       breaks= seq(0, 80000, 10000)) +
    labs(color = "Individualism Index", shape = "Cultural Orientation") +
    theme_light(base_size = 18) +
    theme(plot.caption = element_text(hjust = 0)) +
    theme(plot.title = element_text(hjust = 0.5))

plotw2 <- ggplotly(plotw, tooltip = c("country", "individualism", "gdp_per_capita")) %>% 
    config(displayModeBar = FALSE) 

# Life expect plot (delete country for line)
plotl <- ggplot(df_life, aes(x = individualism, y = life_expectancy, size = gdp_per_capita,
                             color = gdp_per_capita)) +
    geom_smooth(method = 'lm', color = "orange", alpha = 0.2) +
    geom_point(alpha = 0.8) +
    scale_size_continuous(range = c(3, 8), 
                          breaks = c(10000, 30000, 50000, 70000)) +
    scale_colour_gradient(low = "red", high = "green") +
    ggtitle(
        "Life Expectancy in countries according to Cultural Orientation and GDP per Capita") +
    scale_x_continuous(name="Individualism Index",
                       breaks= seq(0, 100, 10)) +
    scale_y_continuous(name="Life Expectancy 
(years)", breaks= seq(0, 100, 2)) +
    labs(color = "GDP per Capita ($)", size = "GDP per Capita ($)", caption = "*Life expectancy data is from 2016") +
    theme_gray(base_size = 18) +
    theme(plot.caption = element_text(hjust = 0)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    guides(size = guide_legend(order = 2, override.aes = list(linetype = c(0,0,0,0))))


# User Interface

ui <- fluidPage(
    titlePanel("My plots"),
    
    selectInput("graph", "Variables", c("plotw",
                                        "plotl",
                                        "graph3")),

conditionalPanel('input.graph=="plotw"', plotOutput("vis")),

conditionalPanel('input.graph=="plotl"', plotOutput("visa")),

conditionalPanel('input.graph=="graph3"', plotlyOutput("ly")) 


 
)

# Functionality

server <- function(input, output){ 
    
    output$vis <- renderPlot({
        validate(need(input$graph=="plotw", message=FALSE))
        print(plotw)
    })
    
    output$visa <- renderPlot({
        validate(need(input$graph=="plotl", message=FALSE))
        print(plotl)
    })
    
    output$ly <- renderPlotly({
        validate(need(input$graph=="graph3", message=FALSE))
        print(plotw2)
    }) 
}
shinyApp(ui, server)