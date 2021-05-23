
library(tidyverse)
library(here)
library(countrycode)
library(data.table)
library(shiny)
library(ggplot2)
library(htmlwidgets)
library(rsconnect)
library(shinyjs)
library(shinydashboard)
library(lemon)
library(gtable)

# Loading in the different datasets

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

# Converting country code back to country names

df$country <- countrycode(df$country, origin = "cowc", destination = "country.name")

#Remove duplicated countries

df<- df%>% distinct(country,
                    sex,
                    .keep_all = TRUE) # Keep duplicated countries according to sex


# Delete country missing data

df <- df %>%
    na.omit(country)

# Pre-defined aesthetics

theme_type1 = theme_light(base_size = 15) +
    theme(legend.background = element_rect(fill = "transparent")) 

adjust_writing = theme(plot.caption = element_text(hjust = 0)) +
    theme(plot.title = element_text(hjust = 0.5))

line_alpha = 0.1

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
                                      breaks = seq(20000, 80000, by = 20000))

gdp_size_scale = scale_size_continuous(range = c(3, 15))


line_colour = "firebrick2"

sex_colour_aes = c("#5899DA", "#945ECF", "#19A979")

gdp_sizelegend_adj = guide_legend(reverse = TRUE,
                                  override.aes =list(linetype = c(0,0,0,0),
                                                     fill = NA))

legend_labels_adj <- guide_legend(title.position = "top",
                                  title.hjust = 0.5)

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
    adjust_writing +
    guides(colour = guide_colourbar(title.position = "top",
                                    title.hjust = 0.5),
           shape = legend_labels_adj) +
    theme(legend.direction  = "horizontal",
          legend.box = "vertical") 


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
    scale_size_continuous(range = c(3, 18), 
                          breaks = c(500, 1000, 1500, 2000)) +
    gdp_gradient +
    geom_smooth(method = 'lm', 
                color = line_colour, 
                alpha = line_alpha) +
    geom_point(alpha = point_alpha3) +
    theme_type1 +
    guides(size = guide_legend(override.aes =list(linetype = c(0,0,0,0),
                                                  fill = NA),
                               
                               reverse =  TRUE)) +
    adjust_writing

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
    scale_color_manual(values = sex_colour_aes) +
    scale_fill_manual(values = sex_colour_aes)  +
    guides(size = gdp_sizelegend_adj,   
           shape =  guide_legend(override.aes = list(size = 7, 
                                                     linetype = c(0,0,0),
                                                     fill = NA)), 
           fill = FALSE) +
    adjust_writing

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

ui <- fluidPage(
    fluidRow(column(width = 5, 
                    titlePanel("Countries Cultural Orientation, Wealth, and Well-Being"),
                    selectInput("graph", "Choose variables", c("Wealth and Individualism",
                                                               "Life Expectancy, Individualism and Wealth",
                                                               "COVID-19 Cases and Death, Individualism and Wealth", 
                                                               "Happiness, Individualism and Wealth",
                                                               "Suicide Rates, Sex, Individualism and Wealth",
                                                               "Suicide Rates, Happiness, Individualism and Wealth")))),
    
    fluidRow(
        column(width = 12,
               conditionalPanel('input.graph=="Wealth and Individualism"',
                                plotOutput("wealth", 
                                           hover = hoverOpts(id ="hover_wealth",
                                                             delay = 100, 
                                                             delayType = "debounce")),
                                uiOutput("hover_wealth_info")
               )),
        column(width = 12,
               conditionalPanel('input.graph=="Life Expectancy, Individualism and Wealth"', plotOutput("life", 
                                                                                                       hover = hoverOpts(id ="hover_life",
                                                                                                                         delay = 100, 
                                                                                                                         delayType = "debounce")),
                                uiOutput("hover_life_info")
               )),
        column(width = 12,
               conditionalPanel('input.graph=="COVID-19 Cases and Death, Individualism and Wealth"', plotOutput("covid",
                                                                                                                hover = hoverOpts(id ="hover_covid",
                                                                                                                                  delay = 100, 
                                                                                                                                  delayType = "debounce")),
                                uiOutput("hover_covid_info")
               )),
        column(width = 12,
               conditionalPanel('input.graph=="Happiness, Individualism and Wealth"', plotOutput("happy",
                                                                                                 hover = hoverOpts(id ="hover_happy",
                                                                                                                   delay = 100, 
                                                                                                                   delayType = "debounce")),
                                uiOutput("hover_happy_info")
               )),
        column(width = 12,
               conditionalPanel('input.graph=="Suicide Rates, Sex, Individualism and Wealth"', plotOutput("suicide",
                                                                                                          hover = hoverOpts(id ="hover_suicide",
                                                                                                                            delay = 100, 
                                                                                                                            delayType = "debounce")),
                                uiOutput("hover_suicide_info")
                                
               )),
        column(width = 12,
               conditionalPanel('input.graph=="Suicide Rates, Happiness, Individualism and Wealth"', plotOutput("sui_happ",
                                                                                                                hover = hoverOpts(id ="hover_sui_happ",
                                                                                                                                  delay = 100,
                                                                                                                                  delayType = "debounce")),
                                uiOutput("hover_sui_happ_info")
                                
               ))
    ), 
    
)
# Functionality

server <- function(input, output, session){ 
    
    output$wealth <- renderPlot({
        validate(need(input$graph=="Wealth and Individualism", message=FALSE))
        plot_wealth
    })
    
    output$hover_wealth_info <- renderUI({
        hover_wealth <- input$hover_wealth
        point_wealth <- nearPoints(df, hover_wealth, xvar = "individualism", yvar = "gdp_per_capita", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_wealth) == 0) return(NULL)
        
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_wealth$coords_css$x
        top_px <- hover_wealth$coords_css$y
        
        
        # create style property fot tooltip
        # background color is set so tooltip is a bit transparent
        # z-index is set so we are sure are tooltip will be on top
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_wealth$country, "<br/>",
                          "<b> Individualism Index: </b>", point_wealth$individualism, "<br/>",
                          "<b> GDP per Capita ($): </b>", point_wealth$gdp_per_capita, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    })
    
    
    output$life <- renderPlot({
        validate(need(input$graph=="Life Expectancy, Individualism and Wealth", message=FALSE))
        plot_life
    })
    
    output$hover_life_info <- renderUI({
        hover_life <- input$hover_life
        point_life <- nearPoints(df, hover_life, xvar = "individualism", yvar = "life_expectancy", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_life) == 0) return(NULL)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_life$coords_css$x
        top_px <- hover_life$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_life$country, "<br/>",
                          "<b> Individualism Index: </b>", point_life$individualism, "<br/>",
                          "<b> Life Expectancy (years): </b>", point_life$life_expectancy, "<br/>",
                          "<b> GDP per Capita ($): </b>", point_life$gdp_per_capita, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    })   
    
    output$covid <- renderPlot({
        validate(need(input$graph=="COVID-19 Cases and Death, Individualism and Wealth", message=FALSE))
        plot_covid
    }) 
    
    output$hover_covid_info <- renderUI({
        hover_covid <- input$hover_covid
        point_covid <- nearPoints(df, hover_covid, xvar = "individualism", yvar = "total_cases", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_covid) == 0) return(NULL)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_covid$coords_css$x
        top_px <- hover_covid$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_covid$country, "<br/>",
                          "<b> Individualism Index: </b>", point_covid$individualism, "<br/>",
                          "<b> COVID-19 cases (per 1 million population): </b>", point_covid$total_cases, "<br/>",
                          "<b> COVID-19 deaths (per 1 million population): </b>", point_covid$total_deaths, "<br/>",
                          "<b> GDP per Capita ($): </b>",  point_covid$gdp_per_capita, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    })   
    
    output$happy <- renderPlot({
        validate(need(input$graph=="Happiness, Individualism and Wealth", message=FALSE))
        plot_happy
    }) 
    
    output$hover_happy_info <- renderUI({
        hover_happy <- input$hover_happy
        point_happy <- nearPoints(df, hover_happy, xvar = "individualism", yvar = "happiness_score", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_happy) == 0) return(NULL)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_happy$coords_css$x
        top_px <- hover_happy$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_happy$country, "<br/>",
                          "<b> Individualism Index: </b>", point_happy$individualism, "<br/>",
                          "<b> Happiness Score: </b>", point_happy$happiness_score, "<br/>",
                          "<b> GDP per Capita ($): </b>",  point_happy$gdp_per_capita, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    }) 
    
    
    
    
    output$suicide <- renderPlot({
        validate(need(input$graph=="Suicide Rates, Sex, Individualism and Wealth", message=FALSE))
        plot_suicide
    }) 
    
    output$hover_suicide_info <- renderUI({
        hover_suicide <- input$hover_suicide
        point_suicide <- nearPoints(df, hover_suicide, xvar = "individualism", yvar = "suicide_rate", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_suicide) == 0) return(NULL)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_suicide$coords_css$x
        top_px <- hover_suicide$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_suicide$country, "<br/>",
                          "<b> Individualism Index: </b>", point_suicide$individualism, "<br/>",
                          "<b> Suicide Rate (per 100000 population): </b>", point_suicide$suicide_rate, "<br/>",
                          "<b> GDP per Capita ($): </b>", point_suicide$gdp_per_capita, "<br/>",
                          "<b> Sex: </b>", point_suicide$sex, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    })  
    
    
    
    
    output$sui_happ <- renderPlot({
        validate(need(input$graph=="Suicide Rates, Happiness, Individualism and Wealth", message=FALSE))
        plot_sui_happ
    }) 
    
    
    output$hover_sui_happ_info <- renderUI({
        hover_sui_happ <- input$hover_sui_happ
        point_sui_happ <- nearPoints(df, hover_sui_happ, xvar = "happiness_score", yvar = "suicide_rate", threshold = 5, maxpoints = 1, addDist = TRUE) 
        if (nrow(point_sui_happ) == 0) return(NULL)
        
        # calculate distance from left and bottom side of the picture in pixels
        left_px <- hover_sui_happ$coords_css$x
        top_px <- hover_sui_happ$coords_css$y
        
        style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                        "left:", left_px + 2, "px; top:", top_px + 2, "px;")
        # actual tooltip created as wellPanel
        wellPanel(
            style = style,
            p(HTML(paste0("<b> Country: </b>", point_sui_happ$country, "<br/>",
                          "<b> Happiness Score: </b>", point_sui_happ$happiness_score, "<br/>",
                          "<b> Suicide Rate (per 100000 population): </b>", point_sui_happ$suicide_rate, "<br/>",
                          "<b> Individualism Index: </b>", point_sui_happ$individualism, "<br/>",
                          "<b> GDP per Capita ($): </b>", point_sui_happ$gdp_per_capita, "<br/>",
                          "<b> Distance from left: </b>", left_px, "<br/>",
                          "<b> Distance from top: </b>", top_px, "</br>"
            )))
        )
    })  
    
    
    
    
}











shinyApp(ui, server)



