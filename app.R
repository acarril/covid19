
library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(wbstats)
library(shiny)
library(DT)




### Time series data (John Hopkins)
# Read:
ts <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

# Make data long for plotting:
ts_long <- pivot_longer(ts, `1/22/20`:`3/24/20`, names_to = "Fecha", values_to = "Casos")

# Sum cases by country (adding up smaller subregions) & other fixes:
ts_long <- ts_long %>% 
    mutate(Fecha = as.Date(Fecha, format = "%m/%d/%y")) %>%
    rename(Paises = `Country/Region`) %>% 
    group_by(Paises, Fecha) %>% 
    summarise(Casos = sum(Casos)) %>% 
    ungroup() %>% 
    group_by(Paises) %>%
    mutate(SumaCasos = cumsum(Casos)) %>% 
    ungroup()

# Add column with number of days since first case:
ts_long <- ts_long %>% 
    ungroup() %>% 
    mutate(hascase = (Casos > 10)) %>% 
    group_by(Paises) %>% 
    mutate(Dias = cumsum(hascase)) %>% 
    select(-hascase) %>% 
    ungroup()

# Join with population data (World Bank 2018)
ts_long <- ts_long %>% mutate(Paises = ifelse(Paises == "US", "United States", Paises))
pop_data <- wb(indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2018)
ts_long <- left_join(ts_long, pop_data, by = c("Paises" = "country"))
ts_long <- ts_long %>% mutate(CasesOverMillion = Casos*1000000/value)

### Code below is now incorporated in the app itself ###
# Filter data by countries in focus:
# country_list <- c("Chile", "Italy", "Poland", "Spain", "US")
# ts_long <- ts_long %>% filter(Paises %in% country_list)

# Plot
# ggplot(ts_long, aes(Dias, CasesOverMillion, color = Paises, group = Paises)) + 
#     geom_line() +
#     theme_bw()
# ggsave("covid19.png", width = 6, height = 4)
########################################################

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("COVID-19"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            # sliderInput("bins",
            #             "Number of bins:",
            #             min = 1,
            #             max = 50,
            #             value = 30),
            selectInput(
                "comparisonCountries",
                "Países:",
                choices = unique(ts_long$Paises),
                selected = "Chile",
                multiple = TRUE
            ),
            selectInput(
                "xaxis",
                "Período (eje X):",
                choices = c("Fecha", "Dias"),
                selected = "Dias",
                multiple = FALSE
            ),
            selectInput(
                "yaxis",
                "Variable (eje Y):",
                choices = c("Casos", "CasesOverMillion"),
                selected = "Casos",
                multiple = FALSE
            ),
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("myplot"),
           # DT::dataTableOutput("mytable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    # output$distPlot <- renderPlot({
    #     # generate bins based on input$bins from ui.R
    #     x    <- faithful[, 2]
    #     bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    #     # draw the histogram with the specified number of bins
    #     hist(x, breaks = bins, col = 'darkgray', border = 'white')
    # })
    
    output$myplot = renderPlot({
        df <- ts_long %>% filter(Paises %in% input$comparisonCountries)
        ggplot(df, aes_string(input$xaxis, input$yaxis, color = "Paises", group = "Paises")) + 
            geom_line() +
            theme_bw(base_size = 18)
    })
    
    # output$mytable = DT::renderDataTable(
    #     df,
    #     options = list(
    #         scrollX = TRUE,
    #         language = list(#url = '//cdn.datatables.net/plug-ins/1.10.11/i18n/Spanish.json',
    #                         info = 'Mostrando filas (países) _START_ a la _END_ de un total de _TOTAL_.',
    #                         lengthMenu = 'Mostrar _MENU_ filas (países)')
    #     )
    # )
}

# Run the application 
shinyApp(ui = ui, server = server)
