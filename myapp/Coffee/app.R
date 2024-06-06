#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#######################################################################

#######################################################################
############### R CODE before UI and Server to generate data "Coffee"
library(shiny)
library(dplyr)
library(reshape2)
library(lattice)
df<-read.csv("https://raw.githubusercontent.com/dawit3000/Data/main/psd_coffee.csv")
#Who were top ten "Coffee Arabica" producers in 2023?
df_10<-filter(df, df$Year == 2023)
df_10<-arrange(df_10, desc(df_10$Arabica.Production)) 
df_10 <- unique(df_10$Country)[1:10] 
##Filter df using d_10. This data is for all years (not just 2023)
df<-filter(df, df$Country %in% df_10)
## dcast and choose columns of interest 
df<-dcast(df, Country ~ Year, value.var = "Arabica.Production")
df<-df[, c(1, 56:65)]
# transpose and sharpen for convenience
df <- as.data.frame(t(df))
colnames(df) <- df[1, ]
df <- df[-1, ]
i=1
for (i in 1: length(df[1,])) {
  df[,i]<-as.numeric(df[,i])
}
#Rely on this data for UI and Server
Coffee <-df

#######################################################################
#  Define UI for application that draws a histogram
ui <- fluidPage(    
  
  # Give the page a title
  titlePanel("World's Top 10 Coffee Arabica Producers of 2023, and their
             last 10 years trajectory"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("country", "CHOOSE A COUNTRY FROM 2023 TOP 10:", 
                  choices=colnames(Coffee)),
      helpText("Original Coffee data is from Kaggle")
    ),
    
       # Create a spot for the barplot
       mainPanel(
       plotOutput("coffeePlot")  
    )
    
  )
)


#######################################################################
###############  Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Fill in input by region (input$region) in the slot created for a plot
  output$coffeePlot <- renderPlot({
    
    # Render Coffee barchart using lattice
    barchart(Coffee[,input$country] ~ rownames(Coffee), 
             data = Coffee,
             xlab="Year", 
             ylab="Production Amount (1000 60 KG BAGS)", 
             main=input$country, 
             col = "purple")
    
  })
}


#######################################################################
###############  Run the application
shinyApp(ui = ui, server = server)
