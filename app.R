palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
library(shiny)
library(plotly)
#setwd("/Users/Elise/DataViz/HW2/")

data1 <- read.csv("API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)[,1:59]
data2 <- read.csv("API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", skip = 4, header = TRUE)[,1:59]
data3 <- read.csv("population.csv", header = T, na.strings = c(NA, "", " "))[,1:59]
metadata <- read.csv("Metadata_Country_API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv", header = T)[,1:2]
data1 <- subset(data1,select=-c(Indicator.Name, Indicator.Code))
data2 <- subset(data2,select=-c(Indicator.Name, Indicator.Code))
data3 <- subset(data3,select=-c(Indicator.Name, Indicator.Code))
colnames(data1) <- c("Country.Name","Country.Code", unlist(lapply(colnames(data1[,3:57]), function(x) {gsub("X","",x)})))
colnames(data2) <- c("Country.Name","Country.Code", unlist(lapply(colnames(data2[,3:57]), function(x) {gsub("X","",x)})))
colnames(data3) <- c("Country.Name","Country.Code", unlist(lapply(colnames(data3[,3:57]), function(x) {gsub("X","",x)})))
merge1 <- merge(metadata, data1, by.x = "Country.Code", by.y = "Country.Code")
merge2 <- merge(metadata, data2, by.x = "Country.Code", by.y = "Country.Code")
merge3 <- merge(metadata, data3, by.x = "Country.Code", by.y = "Country.Code")

region <- merge3[,"Region"]
assign("region", region, envir = .GlobalEnv)

ui <- fluidPage(
  headerPanel('Life Expectancy and Fertility Rate'),
  fluidRow(
    column(12,
           div(
           plotlyOutput('plot1')
           )
    ),
    column(10, 
           wellPanel(
             sliderInput("year", "Select Year", 1960, 2014, 1, animate = TRUE)
           )),
    column(5, 
           wellPanel(
             sliderInput("size", "Select Bubble Size", 1, 5, 1, value = 3)
           ))
    )
  )
server <- function(input, output) {
  input_size <- reactive({input$size})
  output$plot1 <- renderPlotly({
    fertility_rate <- merge2[, as.character(input$year)]
    life_expectancy <- merge1[, as.character(input$year)]
    population <- merge3[, as.character(input$year)]
    data <- data.frame(cbind(fertility_rate, life_expectancy, population, merge3$Country.Name))
    region_vec <- rep(1,7)
    theme1 <- theme(plot.title = element_text(hjust = 0.5), legend.justification = c("right", "top"))
    ggplot(data=data, aes(life_expectancy, fertility_rate)) + geom_point(aes(size = population, colour = region, country= merge3$Country.Name)) +theme1 + scale_size(range = c(1, 6)*input_size())+ guides(size= "none")
  })
}

shinyApp(ui = ui, server = server)