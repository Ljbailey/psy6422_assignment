library(tidyverse)
library(here)
library(countrycode)
library(data.table)
library(plotly)
library(shiny)
library(ggplot2)
<<<<<<< HEAD
library(htmlwidgets)
library(rsconnect)


# Loading in the different datasets

covid <- read.csv(here("covid", "worldometer_coronavirus_summary_data.csv"))

dim <- read.csv(here("dimensions", "six_dimenstions.csv"))

ses <- read.csv(here("ses", "soci_econ_country_profiles.csv"))

happy <- read.csv(here("happiness", "world_happiness_report.csv"))

life <- read.csv(here("life", "who_life_exp.csv"))

suicide <- read.csv(here("suicide", "suicide_rate.csv"))

=======


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
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
# Parse out cultural dimensions data
dim <- dim %>%
    separate(ctr.country.pdi.idv.mas.uai.ltowvs.ivr, 
             c("ctr", "country", "pdi", "idv", "mas", "uai", "lto", "ivr"),
             sep = ";")

<<<<<<< HEAD
dim$idv <- as.numeric(dim$idv)# Changes individualism class to numeric

# Selecting required data from each dataset
=======
## Selecting required Data
# Selecting wanted Covid data
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56

covid <- covid %>%
    select(country, total_cases_per_1m_population, total_deaths_per_1m_population)

<<<<<<< HEAD
dim <- dim %>%
    select(country, idv)

ses <- ses %>%
    select(country,GDP.per.capita..current.US..)

happy <- happy %>%
    select(Country.or.region, Score, Overall.rank)

life <- life %>%
    select(country, year, life_expect)

life <- subset(life, year=="2016") # Remove rows not including year 2016
=======
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
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56

suicide <- suicide %>%
    select(Country, Sex, X2016)

<<<<<<< HEAD
# Re-naming columns of each dataset

colnames(covid)[2] <- "total_cases"
colnames(covid)[3] <- "total_deaths"

colnames(dim)[2] <- "individualism"

colnames(ses)[2] <- "gdp_per_capita"

=======
## Re-naming Columns
# Re-naming Covid columns
colnames(covid)[2] <- "total_cases"
colnames(covid)[3] <- "total_deaths"

#Re-naming cultural dimension column
colnames(dim)[2] <- "individualism"

#Re-naming socio-economic column
colnames(ses)[2] <- "gdp_per_capita"

#Re-naming happiness columns
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
colnames(happy)[1] <- "country"
colnames(happy)[2] <- "happiness_score"
colnames(happy)[3] <- "rank"

<<<<<<< HEAD
=======
#Re-naming life columns
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
colnames(life)[1] <- "country"
colnames(life)[2] <- "year"
colnames(life)[3] <- "life_expectancy"

<<<<<<< HEAD
=======
#Re-naming suicide columns
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
colnames(suicide)[1] <- "country"
colnames(suicide)[2] <- "sex"
colnames(suicide)[3] <- "suicide_rate"

<<<<<<< HEAD

#Converting country name to country codes

df.list <- list(ses, dim, covid, happy, life, suicide) # list of dataframes

convert_code <-  function(x) {
    #converts country name to country codes
    countrycode(x, origin = "country.name", destination = "cowc")
}

ses$country <- convert_code(ses$country)

dim$country <- convert_code(dim$country)

covid$country <- convert_code(covid$country)

happy$country <- convert_code(happy$country)

life$country <- convert_code(life$country)

suicide$country <- convert_code(suicide$country)


# Adjoin desired data-frames with country codes

df_covid <- inner_join(covid, dim, "country")

df_covid <- inner_join(df_covid, ses, "country")

df_happiness <- inner_join(happy, dim, "country")

df_happiness <- inner_join(df_happiness, ses, "country")

df_life <- inner_join(life, dim, "country")

df_life <- inner_join(df_life, ses, "country")

df_suicide <- inner_join(suicide, dim, "country")

df_suicide <- inner_join(df_suicide, ses, "country")

df_sui_happ <- inner_join(suicide, dim, "country")

df_sui_happ <- inner_join(df_sui_happ, happy, "country")

df_sui_happ <- inner_join(df_sui_happ, ses, "country")

df_wealth <- inner_join(dim, ses, "country")

df_all <- inner_join(df_sui_happ, covid, "country")

df_all <- inner_join(df_all, life, "country")

# Converting country codes back to country names
convert_name <- function(x) {
    #converts country code to country name
    countrycode(x, origin = "cowc", destination = "country.name")
}

df_covid$country <- convert_name(df_covid$country)

df_happiness$country <- convert_name(df_happiness$country)

df_life$country <- convert_name(df_life$country)

df_suicide$country <- convert_name(df_suicide$country)

df_sui_happ$country <- convert_name(df_sui_happ$country)

df_wealth$country <- convert_name(df_wealth$country)

df_all$country <-convert_name(df_all$country)

#Remove duplicated countries

=======
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
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56

df_covid <- df_covid[!duplicated
                     (df_covid[ , "country"
                     ]),]

<<<<<<< HEAD
=======
df_happiness1 <- df_happiness1[!duplicated
                               (df_happiness1[ , "country"
                               ]),]

>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
df_happiness <- df_happiness[!duplicated
                             (df_happiness[ , "country"
                             ]),]

<<<<<<< HEAD
=======
df_life1 <- df_life1[!duplicated
                     (df_life1[ , "country"
                     ]),]

>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
df_life <- df_life[!duplicated
                   (df_life[ , "country"
                   ]),]

df_wealth <- df_wealth[!duplicated
                       (df_wealth[ , "country"
                       ]),]

<<<<<<< HEAD
=======

df_suicide1 <- df_suicide1 %>% distinct(country,sex, .keep_all = TRUE)


df_suicide <- df_suicide %>% distinct(country,sex, .keep_all = TRUE)

>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
df_sui_happ <- df_sui_happ[!duplicated
                           (df_sui_happ[ , "country"
                           ]),]

<<<<<<< HEAD

df_suicide <- df_suicide %>% distinct(country,sex,
                                      .keep_all = TRUE) # Keep duplicated countries according to sex

df_all <- df_all %>% distinct(country,sex,
                              .keep_all = TRUE)

# Delete country missing data
=======
#Delete country missing data

df_covid1 <- df_covid1 %>%
    na.omit(country)
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56

df_covid <- df_covid %>%
    na.omit(country)

<<<<<<< HEAD
df_happiness <- df_happiness %>%
    na.omit(country)

df_life <- df_life %>%
    na.omit(country)

=======
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

>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
df_suicide <- df_suicide %>%
    na.omit(country)

df_sui_happ <- df_sui_happ %>%
    na.omit(country)

df_wealth <- df_wealth %>%
    na.omit(country)

<<<<<<< HEAD
df_all <- df_all %>%
    na.omit(country)

# Pre-defined aesthetics
line_alpha = 0.2
point_alpha = 0.8
point_alpha2 = 0.5
individualism_gradient = scale_colour_gradient(low = "blue", 
                                               high = "orange")
gdp_gradient = scale_colour_gradient(low = "#ef8d5d", 
                                     high = "#3fb68e")
gdp_size_scale = scale_size_continuous(range = c(3, 10), 
                                       breaks = c(10000, 30000, 50000, 70000))
individualism_xscale = scale_x_continuous(name="Individualism Index", 
                                          breaks= seq(0, 100, 10)) 
theme_type1 = theme_light(base_size = 18) 

theme_type2 = theme_light(base_size = 15) 

theme_type3 = theme_light(base_size = 12) 

adjust_writing = theme(plot.caption = element_text(hjust = 0)) +
    theme(plot.title = element_text(hjust = 0.5))
overide_line_aes = guide_legend(override.aes =list(linetype =     c(0,0,0,0)))
overide_size_aes = guide_legend(override.aes = list(size = 5))
overide_sizeline_aes = guide_legend(override.aes = list(size = 3,
                                                        linetype = c(0,0,0)))

# Country Wealth Plot
plot_wealth <- ggplot(df_all, aes (x = individualism, 
                                   y = gdp_per_capita, 
                                   shape = ifelse(individualism < 50, "Collectivistic Cultures", "Individualistic Cultures"),
                                   color = individualism)) +
    ggtitle("Countries GDP per Capita and Cultural Orientation") +
    labs(color = "Individualism Index", 
         shape = "Cultural Orientation") +
    individualism_xscale +
    scale_y_continuous(name="GDP per Capita ($)",
                       breaks= seq(0, 80000, 10000)) +
    geom_smooth((aes(shape=NA)), 
                method = 'lm', 
                color = "darkgreen", 
                alpha = line_alpha) +
    geom_point(size = 4, alpha = point_alpha) +
    individualism_gradient +
    theme_type1 +
    adjust_writing 


# Life Expectancy Plot
plot_life <- ggplot(df_all, aes(x = individualism, 
                                y = life_expectancy, 
                                size = gdp_per_capita,
                                color = gdp_per_capita)) +
    ggtitle("Countries Life Expectancy according to Cultural Orientation and GDP per Capita") +
    labs(color = "GDP per Capita ($)", 
         size = "GDP per Capita ($)", 
         caption = "*Life expectancy data is from 2016") +
    individualism_xscale +
    scale_y_continuous(name="Life Expectancy 
(years)", 
                       breaks= seq(0, 100, 2)) +
    gdp_gradient +
    gdp_size_scale +
    geom_smooth(method = 'lm',
                color = "orange", 
                alpha = line_alpha) +
    geom_point(alpha = point_alpha) +
    theme_type2 +
    guides(size = overide_line_aes) +
    adjust_writing

# COVID Plot
plot_covid <- ggplot(df_all, aes (x = individualism, 
                                  y = total_cases, 
                                  size = total_deaths,
                                  color = gdp_per_capita)) +
    ggtitle("COVID-19 Cases and Deaths according to Countries Cultural Orientation and Wealth") +
    labs(color = "GDP per Capita ($)", 
         size = "Total COVID-19 Deaths
(per 1 Million Population)", 
         caption = "* The 3rd quartile of GDP per Capita equals $42,033
** Data was retrieved on 11/05/2021") +
    individualism_xscale +
    scale_y_continuous(name="Total COVID-19 Cases
(per 1 Million Population)", breaks= seq(0, 155000, 10000)) +
    scale_size_continuous(range = c(3, 13), 
                          breaks = c(500, 1000, 1500, 2000)) +
    gdp_gradient +
    geom_smooth(method = 'lm', 
                color = 615, 
                alpha = line_alpha) +
    geom_point(alpha = point_alpha) +
    theme_type2 +
    guides(size = overide_line_aes) +
    adjust_writing


#Happiness plot

plot_happy <- ggplot(df_all, aes(x = individualism,
                                 y = happiness_score,
                                 size = gdp_per_capita, 
                                 color = ifelse(individualism < 50, "Collectivistic Cultures", "Individualistic Cultures"))) + 
    ggtitle(
        "Countries Happiness in relation to Cultural Orientation and Wealth") + 
    labs(color = "Cultural Orientation", 
         size = "GDP per Capita ($)", 
         caption = "* Happiness scores are produced from indicators in the World Happiness Report 2020") +
    individualism_xscale + 
    scale_y_continuous(name="Happiness Score",
                       breaks= seq(0, 10, 0.5))  +
    gdp_size_scale +
    geom_smooth((aes(colour=NA)), 
                color = "#E7C582", 
                alpha = line_alpha) +
    geom_point(alpha = point_alpha) + 
    theme_type1 +
    guides(size = overide_line_aes,
           color = overide_size_aes) +
    scale_color_manual(values= c("#00B0BA", "#0065A2")) + 
    adjust_writing


# Suicide Rates 

plot_suicide <- ggplot(df_all, aes(x = individualism, 
                                   y = suicide_rate,
                                   size = gdp_per_capita, 
                                   color = sex, 
                                   shape = sex)) +
    ggtitle("Suicide Rates of the Sexes in regards to Countries Wealth and Cultural Orientation") +
    labs(shape = "Sex", 
         color = "Sex", 
         linetype = "Sex", 
         size = "GDP per Capita ($)", 
         caption = "* Suicide rates are standardised by age and are from suicides reported in 2016") +
    individualism_xscale + 
    scale_y_continuous(name="Suicide Rate
(per 100000 Population)", 
                       breaks= seq(0, 50, 5)) +
    gdp_size_scale +
    geom_smooth(method = 'lm', 
                alpha = line_alpha, 
                aes(linetype = sex,
                    fill = sex)) +
    geom_point(alpha = point_alpha2) +  
    theme_type2 +
    guides(size = overide_line_aes,   
           shape = overide_sizeline_aes) +
    scale_linetype_manual(values=c("dashed", "solid", "solid")) +
    scale_color_manual(values=c("#5899DA", "#945ECF", "#19A979")) + 
    scale_fill_manual(values=c("#5899DA", "#945ECF", "#19A979")) + 
    guides(fill = FALSE) +
    adjust_writing


#Happiness and suicide rates

plot_sui_happ <- ggplot(df_all, aes(y = suicide_rate, 
                                    x = happiness_score,
                                    size = gdp_per_capita,
                                    color = individualism,
                                    shape = ifelse(individualism < 50, "Collectivistic Cultures", "Individualistic Cultures"))) + 
    ggtitle("Suicide Rates according to Countries Happiness, Wealth and Cultural Orientation") +
    labs(shape = "Cultural Orientation",
         color = "Individualism Index", 
         size = "GDP per Capita ($)",
         caption = "* Suicide rates are standardised by age and reported in 2016
** Happiness scores are produced from indicators in the World Happiness Report 2020") +
    scale_x_continuous(name="Happiness Score",
                       breaks= seq(0, 10, 0.5)) +
    scale_y_continuous(name="Suicide Rate
(per 100000 Population)",
                       breaks= seq(0, 50, 5)) +
    gdp_size_scale +
    individualism_gradient +
    geom_point(alpha = point_alpha2) + 
    theme_type3 +
    guides(shape = overide_size_aes) + 
    adjust_writing



shinyApp(
    
    ui <- fluidPage(
        titlePanel("Countries Cultural Orientation, Wealth, and Well-Being"),
        
        selectInput("graph", "Choose variables", c("Wealth and Individualism",
                                                   "Life Expectancy, Individualism and Wealth",
                                                   "COVID-19 Cases and Death, Individualism and Wealth", 
                                                   "Happiness, Individualism and Wealth",
                                                   "Suicide Rates, Sex, Individualism and Wealth",
                                                   "Suicide Rates, Happiness, Individualism and Wealth")),
        
        conditionalPanel('input.graph=="Wealth and Individualism"', plotOutput("wealth")),
        
        conditionalPanel('input.graph=="Life Expectancy, Individualism and Wealth"', plotOutput("life")),
        
        conditionalPanel('input.graph=="COVID-19 Cases and Death, Individualism and Wealth"', plotOutput("covid")),
        
        conditionalPanel('input.graph=="Happiness, Individualism and Wealth"', plotOutput("happy")),
        
        conditionalPanel('input.graph=="Suicide Rates, Sex, Individualism and Wealth"', plotOutput("suicide")),
        
        conditionalPanel('input.graph=="Suicide Rates, Happiness, Individualism and Wealth"', plotOutput("sui_happ"))
        
    ),
    # Functionality
    
    server <- function(input, output){ 
        
        output$wealth <- renderPlot({
            validate(need(input$graph=="Wealth and Individualism", message=FALSE))
            print(plot_wealth)
        })
        
        output$life <- renderPlot({
            validate(need(input$graph=="Life Expectancy, Individualism and Wealth", message=FALSE))
            print(plot_life)
        })
        
        output$covid <- renderPlot({
            validate(need(input$graph=="COVID-19 Cases and Death, Individualism and Wealth", message=FALSE))
            print(plot_covid)
        }) 
        
        output$happy <- renderPlot({
            validate(need(input$graph=="Happiness, Individualism and Wealth", message=FALSE))
            print(plot_happy)
        }) 
        
        output$suicide <- renderPlot({
            validate(need(input$graph=="Suicide Rates, Sex, Individualism and Wealth", message=FALSE))
            print(plot_suicide)
        }) 
        
        output$sui_happ <- renderPlot({
            validate(need(input$graph=="Suicide Rates, Happiness, Individualism and Wealth", message=FALSE))
            print(plot_sui_happ)
        }) 
        
    },
    
    options = list(height = 600)
)
=======
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
>>>>>>> 58531a394ed3c28f4fd43361cc4b59c9045e4f56
