library(shiny)
library(tidyverse)
data <- read.csv("USvideos.csv")

data_cleaned<- data %>%
  filter(category_id %in% c("1", "2", "10", "15", "17", "19", "22", "23", "24", "26", "27", "28")) %>%
  mutate(time_string = toString(publish_time)) %>%
  mutate(day = substr(time_string, 9, 10)) %>%
  select(title, category_id, tags, views, likes, comment_count, dislikes)

ui <- fluidPage(
  textInput("tags", 
            "Tags", 
            value = "", 
            placeholder = ""),
  selectInput("category",
              "Category",
              choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                             "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
  mainPanel(
    tabsetPanel(
      id = 'dataset',
      tabPanel("Likes", DT::dataTableOutput("mytable_likes")),
      tabPanel("Dislikes", DT::dataTableOutput("mytable_dislikes")),
      tabPanel("Comment Count", DT::dataTableOutput("mytable_comment_count")),
      tabPanel("Views", DT::dataTableOutput("mytable_views"))
    )
  )
)

server <- function(input, output) {
  
  output$mytable_likes <- DT::renderDataTable({
    data_cleaned %>%
      mutate(likes = as.numeric(likes)) %>%
      filter(category_id == input$category) %>%
      arrange(desc(likes)) %>%
      select(title, likes)
  })
  
  output$mytable_dislikes <- DT::renderDataTable({
    data_cleaned %>%
      mutate(dislikes = as.numeric(dislikes)) %>%
      filter(category_id == input$category) %>%
      arrange(dislikes) %>%
      select(title, dislikes)
  })
  
  output$mytable_comment_count <- DT::renderDataTable({
    data_cleaned %>%
      mutate(comment_count = as.numeric(comment_count)) %>%
      filter(category_id == input$category) %>%
      arrange(desc(comment_count)) %>%
      select(title, comment_count)
  })
  
  output$mytable_views <- DT::renderDataTable({
    data_cleaned %>%
      mutate(views = as.numeric(views)) %>%
      filter(category_id == input$category) %>%
      arrange(desc(views)) %>%
      select(title, views)
  })
  
}

shinyApp(ui = ui, server = server)

