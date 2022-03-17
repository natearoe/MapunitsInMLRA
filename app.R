#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mapunits in MLRA"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("MLRA", label = "MLRA", value = "18"),
            
            numericInput("Membership", label = "Membership (%)", value = "10")
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(
                tabPanel("General Information", tags$h3("Overview:"), "This ShinyApp is designed to help you determine which mapunits 
                                                                              are part of your MLRA. This is accomplished by determining what percent of the total mapunit 
                                                                              acreage is within your MLRA. The methodology was developed by Dylan Beaudette. The app accesses a 
                                                                              file that is maintained and will therefore cotinue to be accurate as MLRA and mapunit boundaries change.
                                                                              For more details, please visit", tags$a(href = "https://github.com/ncss-tech/SoilWeb-data",
                                                                                                    "Soil Web Data."),
                         tags$h3("Membership:"), "This methodology determines what mapunits belong to an MLRA based on the percentage of mapunit acreage within the MLRA of
                                                  interest. The membership criteria can be adjust using the 'Membership (%)' field. The 'Membership (%)' field defines the minimum 
                                                  percent of mapunit acreage that must be within the MLRA of interest to be considered a member. For example, the default
                                                  values state that 10% or more of a mapunits acreage must be within MLRA 18 for the mapunit to be listed.",
                         tags$h3("Tabs"), "There are two other tabs on this ShinyApp. The tab titled 'Group One' lists up to 
                                 2100 mapunits. If there are more than 2100 mapunits within the MLRA, the remaining mapunits will be listed in the tab titled 'Group Two'. 
                                 Results are partitioned because NASIS queries and reports frequently do not allow more than 2100 mapunits to be entered at once.",
                         tags$h3("SSURGO vs STATSGO (Alaska, take note!)"), "This methodology uses SSURGO delineations of mapunits and does not include STATSGO data. Given that SSURGO
                         data is not complete for all areas of the country, this product may not be appropriate for all users. This is likely the case for the majority of Alaska,
                         where SSURGO data is incomplete or absent in areas",
                         tags$h3("Are there two groups?"), textOutput("myGroups"), tags$head(tags$style("#myGroups{color: magenta;
                                                                                                        font-size: 17px;
                                                                                                        font-style: italic;
                                                                                                        }"))
                ),
                tabPanel("Group One", textOutput("myMapUnitsOne")),
                tabPanel("Group Two", textOutput("myMapUnitsTwo"))
            )
            
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    url <- "https://github.com/ncss-tech/SoilWeb-data/raw/main/files/mukey-mlra-overlap.csv.gz"
    tmp <- tempfile()
    download.file(url, tmp)
    mu_mlra_assignment <- read.csv(tmp, stringsAsFactors = FALSE)

    
    MLRA_in <- reactive({
        input$MLRA
    })
    
    Membership_in <- reactive({
        input$Membership
    })
    
    
    mu_df <- reactive({
        mu_mlra_assignment %>% mutate(membership_perc = membership * 100) %>% filter(mlra == MLRA_in()) %>% 
            filter(membership_perc >= Membership_in())
    })
    
    output$myGroups <- renderText({
        if(nrow(mu_df()) <= 2100){print("No. You only need to use the 'Group One' tab.")} else{print("Yes. Make sure to use the 'Group One' and 'Group Two' tabs.")}
    })
    
    output$myMapUnitsOne <- renderText({
       if(nrow(mu_df()) <= 2100){mu_df() %>% pull(mukey) %>% paste(collapse = ", ")} else{mu_df()[1:2100,] %>% pull(mukey) %>% paste(collapse = ", ")}
    
    })
    
    output$myMapUnitsTwo <- renderText({
        if(nrow(mu_df()) <= 2100){print("There are less than 2100 mapunits. The full list of mapunits are listed in the 'Group One' tab.")} else{
            mu_df()[2100:nrow(mu_df()),] %>% pull(mukey) %>% paste(collapse = ", ")
        }
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
