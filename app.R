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

# Loading in the different datasets

dim <- read.csv(here("dimensions", "six_dimenstions.csv"))

ses <- read.csv(here("ses", "soci_econ_country_profiles.csv"))

life <- read.csv(here("life", "who_life_exp.csv"))

covid <- read.csv(here("covid","worldometer_coronavirus_summary_data.csv"))

happy <- read.csv(here("happiness", "world_happiness_report.csv"))

suicide <- read.csv(here("suicide", "suicide_rate.csv"))



### Data Wrangling
#### Modifying Datasets

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


#### Merging Datasets


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

# Converting country code back to country names

df$country <- countrycode(df$country, origin = "cowc", destination = "country.name")


#### Data Cleaning


#Remove duplicated countries

df<- df%>% distinct(country,
                    sex,
                    .keep_all = TRUE) # Keep duplicated countries according to sex


# Delete country missing data

df <- df %>%
  na.omit(country)





## Data visualisations
### Pre-defining Aesthetics


# Pre-defined aesthetics

theme_type1 = theme_light(base_size = 15) 

adjust_writing = theme(plot.caption = element_text(hjust = 0)) +
  theme(plot.title = element_text(hjust = 0.5))

line_alpha = 0.2

point_alpha3 = 0.2

individualism_xscale = scale_x_continuous(name="Individualism Index", 
                                          breaks= seq(0, 100, 10)) 


suicide_yscale =  scale_y_continuous(name="Suicide Rate
(per 100000 Population)",
                                     breaks= seq(0, 50, 5))

individualism_gradient = scale_colour_gradient(low = "blue", 
                                               high = "orange")

gdp_gradient = scale_colour_gradient2(low = "magenta2",
                                      mid = "yellow2",
                                      high = "springgreen2",
                                      midpoint = 17675,
                                      breaks = seq(20000, 80000, by = 20000),
                                      limits = c(1000,90000 ))

gdp_size_scale = scale_size_continuous(limits = c(1000, 90000),
                                       range = c(3, 10), 
                                       breaks = seq(20000, 80000, by = 20000))

line_colour = "firebrick2"

sex_colour_aes = c("#5899DA", "#945ECF", "#19A979")

gdp_sizelegend_adj = guide_legend(reverse = TRUE,
                                  override.aes =list(linetype = c(0,0,0,0)))


### GDP per Capita Graph

# Country Wealth Plot
plot_wealth <- ggplot(df, aes (x = individualism, 
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
  geom_point(size = 7, alpha = point_alpha3) +
  individualism_gradient +
  theme_type1 +
  adjust_writing 


### Life Expectancy Graph


# Life Expectancy Plot
plot_life <- ggplot(df, aes(x = individualism, 
                            y = life_expectancy, 
                            size = gdp_per_capita,
                            color = gdp_per_capita)) +
  ggtitle("Countries Life Expectancy according to Cultural Orientation and GDP per Capita") +
  labs(color = "GDP per Capita ($)", 
       size = "GDP per Capita ($)", 
       caption = "*Life expectancy data is from 2016") +
  individualism_xscale +
  scale_y_continuous(name = "Life Expectancy 
(years)", 
                     breaks= seq(0, 100, 2)) +
  gdp_gradient +
  gdp_size_scale +
  geom_smooth(method = 'lm',
              color = line_colour, 
              alpha = line_alpha) +
  geom_point(alpha = point_alpha3) +
  theme_type1 +
  guides(size = gdp_sizelegend_adj) +
  adjust_writing

### COVID-19 Rates and Deaths Graph

# COVID Plot
plot_covid <- ggplot(df, aes (x = individualism, 
                              y = total_cases, 
                              size = total_deaths,
                              color = gdp_per_capita)) +
  ggtitle("COVID-19 Cases and Deaths according to Countries Cultural Orientation and Wealth") +
  labs(color = "GDP per Capita ($)", 
       size = "Total COVID-19 Deaths
(per 1 Million Population)", 
       caption = "* COVID-19 data was retrieved on 11/05/2021") +
  individualism_xscale +
  scale_y_continuous(name="Total COVID-19 Cases
(per 1 Million Population)",
                     breaks= seq(0, 155000, 20000)) +
  scale_size_continuous(range = c(3, 13), 
                        breaks = c(500, 1000, 1500, 2000)) +
  gdp_gradient +
  geom_smooth(method = 'lm', 
              color = line_colour, 
              alpha = line_alpha) +
  geom_point(alpha = point_alpha3) +
  theme_type1 +
  guides(size = guide_legend(override.aes =list(linetype = c(0,0,0,0)),
                             reverse =  TRUE)) +
  adjust_writing



### Happiness Graph
      

#Happiness plot

plot_happy <- ggplot(df, aes(x = individualism,
                             y = happiness_score,
                             size = gdp_per_capita, 
                             color = gdp_per_capita,
                             shape = ifelse(individualism < 50, "Collectivistic Cultures", "Individualistic Cultures"))) + 
  ggtitle(
    "Countries Happiness in relation to Cultural Orientation and Wealth") + 
  labs(color = "GDP per Capita ($)", 
       size = "GDP per Capita ($)", 
       shape = "Cultural Orientation", 
       caption = "* Happiness scores are produced from Cantril ladder scored reported in the World Happiness Report 2020") +
  individualism_xscale + 
  scale_y_continuous(name = "Happiness Score",
                     breaks = seq(0, 10, 0.5))  +
  gdp_size_scale +
  geom_smooth((aes(colour = NA,
                   shape = NA)), 
              color = line_colour, 
              alpha = line_alpha) +
  geom_point(alpha = point_alpha3) + 
  theme_type1 +
  guides(size = gdp_sizelegend_adj,
         shape =  guide_legend(override.aes = list(size = 7)))  + 
  adjust_writing +
  gdp_gradient



### Suicide Rates Graph

# Suicide Rates 

plot_suicide <- ggplot(df, aes(x = individualism, 
                               y = suicide_rate,
                               size = gdp_per_capita, 
                               color = sex, 
                               shape = sex)) +
  ggtitle("Suicide Rates of the Sexes in regards to Countries Wealth and Cultural Orientation") +
  labs(shape = "Sex", 
       color = "Sex", 
       linetype = "Sex", 
       size = "GDP per Capita ($)", 
       caption = "* Suicide rates reported in 2016 and are standardised by age") +
  individualism_xscale + 
  suicide_yscale +
  gdp_size_scale +
  geom_smooth(method = 'lm', 
              alpha = line_alpha, 
              aes(linetype = sex,
                  fill = sex)) +
  geom_point(alpha = point_alpha3) +  
  theme_type1 +
  scale_linetype_manual(values = c("dashed", "solid", "solid")) +
  scale_color_manual(values = sex_colour_aes) +                 scale_fill_manual(values = sex_colour_aes)  +
  guides(size = gdp_sizelegend_adj,   
         shape =  guide_legend(override.aes = list(size = 7,
                                                   linetype = c(0,0,0))),
         fill = FALSE) +
  adjust_writing



### Happiness and Suicide Rate Graph

#Happiness and suicide rates

plot_sui_happ <- ggplot(df, aes(y = suicide_rate, 
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
  suicide_yscale +
  gdp_size_scale +
  individualism_gradient +
  geom_point(alpha = point_alpha3) + 
  theme_type1 +
  guides(shape = guide_legend(override.aes = list(size = 7)),
         size = gdp_sizelegend_adj) + 
  adjust_writing

# Shiny application
  
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
    
  )
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
    
  }
  
shinyApp(ui, server)



