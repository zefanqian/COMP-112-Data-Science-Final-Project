
# This is the absolute bare minimum of what I need to create a shiny app.
# Beware! ... This alone will be a REALLY boring app. A blank page :(

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
  selectInput("rankBy",
              "Recommendation Ranked by", 
              choices = list("Likes" = "likes", "Dislikes" = "dislikes", "Comment Count" = "comment_count", "Views" = "views")),
  DT::dataTableOutput("mytable")
)

server <- function(input, output) {
  output$mytable <- DT::renderDataTable({
    data_cleaned %>%
      mutate(likes = as.numeric(likes)) %>%
      filter(category_id == input$category) %>%
      arrange(desc(likes)) %>%
      select(title, likes)
  })
}

shinyApp(ui = ui, server = server)

