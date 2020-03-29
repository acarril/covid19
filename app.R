
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)
library(countrycode)
library(shiny)
library(shinythemes)
library(DT)




### Time series data on cases (John Hopkins)
# Read
tsCases  <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
tsDeaths <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")

# Make data long for plotting
tsCases  <- pivot_longer(tsCases,  `1/22/20`:tail(names(tsCases),  1), names_to = "Fecha", values_to = "Casos")
tsDeaths <- pivot_longer(tsDeaths, `1/22/20`:tail(names(tsDeaths), 1), names_to = "Fecha", values_to = "Muertes")

# Merge both datasets
tsDeaths <- tsDeaths %>% select(`Country/Region`, Fecha, Muertes)
df <- inner_join(tsCases, tsDeaths, by = c("Country/Region", "Fecha"))

#  & other fixes
df <- df %>% 
    # Change date to proper date format:
    mutate(Fecha = as.Date(Fecha, format = "%m/%d/%y")) %>%
    # Rename country column:
    rename(Country = `Country/Region`) %>% 
    # Sum cases/deaths by country (adding up smaller subregions):
    group_by(Country, Fecha) %>% 
    summarise(Casos = sum(Casos),
              Muertes = sum(Muertes)) %>% 
    ungroup() %>% 
    # Add column with cumulative sum of cases/deaths
    group_by(Country) %>%
    mutate(SumaCasos = cumsum(Casos),
           SumaMuertes = cumsum(Muertes)) %>% 
    ungroup()

# Add column with number of days since first case
df <- df %>% 
    ungroup() %>% 
    mutate(hascase = (Casos > 10)) %>% 
    group_by(Country) %>% 
    mutate(Días = cumsum(hascase)) %>% 
    select(-hascase) %>% 
    ungroup()

# Join with population data (World Bank 2018)
df <- df %>% mutate(Country = ifelse(Country == "US", "United States", Country))
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018) %>% 
    select(country, iso3c, Población = value)
pop_data$Países <- countrycode(pop_data$iso3c, origin = 'iso3c', destination = 'un.name.es')
df <- left_join(df, pop_data, by = c("Country" = "country")) %>% drop_na(Países)
df <- df %>% mutate(CasosPorMillon = SumaCasos*1000000/Población)
# Note: some countries don't merge. Check with x <- df %>% group_by(Países) %>% summarise(mean = mean(Población))

# Create dataset only with latest data (for display)
df_snapshot <- df %>% 
    # Filter only last date of each country
    group_by(Países) %>% 
    arrange(Fecha) %>% 
    filter(row_number() == n()) %>% 
    ungroup() %>% 
    # Select only relevant columns and reorder them
    select(Países, iso3c, Población, SumaCasos, SumaMuertes) %>%
    rename(Casos = SumaCasos, Muertes = SumaMuertes) %>% 
    arrange(-Casos)
# Extract update date
updateDate <- max(df$Fecha)

### Code below is now incorporated in the app itself ###
# Filter data by countries in focus:
# country_list <- c("Chile", "Italy", "Poland", "Spain", "US")
# tsCases <- tsCases %>% filter(Países %in% country_list)

# Plot
# ggplot(tsCases, aes(Días, CasesOverMillion, color = Países, group = Países)) + 
#     geom_line() +
#     theme_bw()
# ggsave("covid19.png", width = 6, height = 4)
########################################################

# Define UI for application that draws a histogram
ui <- fluidPage(
    # Theme
    theme = shinytheme("paper"),

    # Application title
    titlePanel("Evolución del COVID-19"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput(
                "comparisonCountries",
                "Países:",
                choices = unique(df$Países),
                selected = "Chile",
                multiple = TRUE
            ),
            selectInput(
                "xaxis",
                "Período (eje X):",
                choices = c("Fecha", "Días"),
                selected = "Días",
                multiple = FALSE
            ),
            selectInput(
                "yaxis",
                "Variable (eje Y):",
                choices = c("Casos", "SumaCasos", "CasosPorMillon", "Muertes", "SumaMuertes"),
                selected = "SumaCasos",
                multiple = FALSE
            ),
            # Input: Choose dataset ----
            selectInput("dataset", "Descarga una base de datos:",
                        choices = c("Datos consolidados", "Serie de tiempo", "Serie de tiempo casos", "Serie de tiempo muertes")),
            
            # Button
            downloadButton("downloadData", "Descargar")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("Explora", plotOutput("myplot"), DT::dataTableOutput("mytable")),
                tabPanel("Más información", plotOutput("myplot2"))
            )
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Reactive value for selected dataset ----
    datasetInput <- reactive({
        switch(input$dataset,
               "Datos consolidados" = df_snapshot,
               "Serie de tiempo" = df,
               "Serie de tiempo casos" = tsCases,
               "Serie de tiempo muertes" = tsDeaths,
               )
    })
    
    output$myplot = renderPlot({
        df <- df %>% filter(Países %in% input$comparisonCountries)
        ggplot(df, aes_string(input$xaxis, input$yaxis, color = "Países", group = "Países")) + 
            geom_line() +
            theme_bw(base_size = 18)
    })
    
    output$mytable = DT::renderDataTable(
        df_snapshot,
        options = list(
            scrollX = TRUE,
            language = list(url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',
                            info = 'Mostrando filas (países) _START_ a la _END_ de un total de _TOTAL_.',
                            lengthMenu = 'Mostrar _MENU_ filas (países)')
        )
    )
    
    # Downloadable csv of selected dataset ----
    output$downloadData <- downloadHandler(
        filename = function() {
            paste(input$dataset, ".csv", sep = "")
        },
        content = function(file) {
            write.csv(datasetInput(), file, row.names = FALSE)
        }
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
