library(tidyverse)
library(here)
library(countrycode)
library(data.table)
library(plotly)
library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)


# Loading in the different datasets

covid <- read.csv(here("covid", "worldometer_coronavirus_summary_data.csv"))

dim <- read.csv(here("dimensions", "six_dimenstions.csv"))

ses <- read.csv(here("ses", "soci_econ_country_profiles.csv"))

happy <- read.csv(here("happiness", "world_happiness_report.csv"))

life <- read.csv(here("life", "who_life_exp.csv"))

suicide <- read.csv(here("suicide", "suicide_rate.csv"))

# Parse out cultural dimensions data
dim <- dim %>%
    separate(ctr.country.pdi.idv.mas.uai.ltowvs.ivr, 
             c("ctr", "country", "pdi", "idv", "mas", "uai", "lto", "ivr"),
             sep = ";")

dim$idv <- as.numeric(dim$idv)# Changes individualism class to numeric

# Selecting required data from each dataset

covid <- covid %>%
    select(country, total_cases_per_1m_population, total_deaths_per_1m_population)

dim <- dim %>%
    select(country, idv)

ses <- ses %>%
    select(country,GDP.per.capita..current.US..)

happy <- happy %>%
    select(Country.or.region, Score, Overall.rank)

life <- life %>%
    select(country, year, life_expect)

life <- subset(life, year=="2016") # Remove rows not including year 2016

suicide <- suicide %>%
    select(Country, Sex, X2016)

# Re-naming columns of each dataset

colnames(covid)[2] <- "total_cases"
colnames(covid)[3] <- "total_deaths"

colnames(dim)[2] <- "individualism"

colnames(ses)[2] <- "gdp_per_capita"

colnames(happy)[1] <- "country"
colnames(happy)[2] <- "happiness_score"
colnames(happy)[3] <- "rank"

colnames(life)[1] <- "country"
colnames(life)[2] <- "year"
colnames(life)[3] <- "life_expectancy"

colnames(suicide)[1] <- "country"
colnames(suicide)[2] <- "sex"
colnames(suicide)[3] <- "suicide_rate"


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


df_covid <- df_covid[!duplicated
                     (df_covid[ , "country"
                     ]),]

df_happiness <- df_happiness[!duplicated
                             (df_happiness[ , "country"
                             ]),]

df_life <- df_life[!duplicated
                   (df_life[ , "country"
                   ]),]

df_wealth <- df_wealth[!duplicated
                       (df_wealth[ , "country"
                       ]),]

df_sui_happ <- df_sui_happ[!duplicated
                           (df_sui_happ[ , "country"
                           ]),]


df_suicide <- df_suicide %>% distinct(country,sex,
                                      .keep_all = TRUE) # Keep duplicated countries according to sex

df_all <- df_all %>% distinct(country,sex,
                              .keep_all = TRUE)

# Delete country missing data

df_covid <- df_covid %>%
    na.omit(country)

df_happiness <- df_happiness %>%
    na.omit(country)

df_life <- df_life %>%
    na.omit(country)

df_suicide <- df_suicide %>%
    na.omit(country)

df_sui_happ <- df_sui_happ %>%
    na.omit(country)

df_wealth <- df_wealth %>%
    na.omit(country)

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
