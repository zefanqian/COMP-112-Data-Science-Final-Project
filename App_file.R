library(shiny)
library(tidyverse)
data <- read.csv("USvideos.csv")

data_cleaned<- data %>%
  filter(category_id %in% c("1", "2", "10", "15", "17", "19", "22", "23", "24", "26", "27", "28")) %>%
  mutate(time_string = toString(publish_time)) %>%
  mutate(day = substr(time_string, 9, 10)) %>%
  mutate(likes = as.numeric(likes)) %>%
  mutate(dislikes = as.numeric(dislikes))
  mutate(like_dislike_ratio = likes/(dislikes+1))%>%
  select(title, category_id, tags, views, likes, comment_count, dislikes,like_dislike_ratio)


ui <- fluidPage(
  title = "Youtube Recommendation",
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        'input.dataset === "Users Manual"',
        helpText("Here is the user's manual")
      ),
      conditionalPanel(
        'input.dataset === "Likes"',
        selectInput("category1",
                    "Category",
                    choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                                   "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
        checkboxGroupInput("show_vars1","Columns in Likes to show:", 
                           list("Title" = "title", "Views" = "views", "Likes" = "likes", 
                                "Comment Count" = "comment_count", "Dislikes" = "dislikes", "Like Dislike Ratio" = "like_dislike_ratio"), 
                           selected = list("title", "likes"))
      ),
      conditionalPanel(
        'input.dataset === "Dislikes"',
        selectInput("category2",
                    "Category",
                    choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                                   "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
        checkboxGroupInput("show_vars2","Columns in Likes to show:", 
                          list("Title" = "title", "Views" = "views", "Likes" = "likes", 
                               "Comment Count" = "comment_count", "Dislikes" = "dislikes", "Like Dislike Ratio" = "like_dislike_ratio"), 
                          selected = list("title", "dislikes"))
      ),
      conditionalPanel(
        'input.dataset === "Comment Count"',
        selectInput("category3",
                    "Category",
                    choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                                   "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
        checkboxGroupInput("show_vars3","Columns in Likes to show:", 
                           list("Title" = "title", "Views" = "views", "Likes" = "likes", 
                                "Comment Count" = "comment_count", "Dislikes" = "dislikes", "Like Dislike Ratio" = "like_dislike_ratio"), 
                           selected = list("title", "comment_count"))
      ),
      conditionalPanel(
        'input.dataset === "Views"',
        selectInput("category4",
                    "Category",
                    choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                                   "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
        checkboxGroupInput("show_vars4","Columns in Likes to show:", 
                           list("Title" = "title", "Views" = "views", "Likes" = "likes", 
                                "Comment Count" = "comment_count", "Dislikes" = "dislikes", "Like Dislike Ratio" = "like_dislike_ratio"), 
                           selected = list("title", "views"))
      ),
      conditionalPanel(
        'input.dataset === "Like Dislike Ratio"',
        selectInput("category5",
                    "Category",
                    choices = list("Autos & Vehicles" = "1", "Music" = "2", "Comedy" = "10", "Science & Technology" = "15", "Movies" = "17", "Action/Adventure" = "19", 
                                   "Documentary" = "22", "Drama" = "23", "Family" = "24", "Horror" = "26", "Sci-Fi/Fantasy" = "27", "Thriller" = "28")),
        checkboxGroupInput("show_vars5","Columns in Likes to show:", 
                           list("Title" = "title", "Views" = "views", "Likes" = "likes", 
                                "Comment Count" = "comment_count", "Dislikes" = "dislikes", "Like Dislike Ratio" = "like_dislike_ratio"), 
                           selected = list("title", "like_dislike_ratio"))
      ),
      conditionalPanel(
        'input.dataset === "Visualization"'
      )
    ),
    mainPanel(
      tabsetPanel(
        id = 'dataset',
        tabPanel("Users Manual", tags$h2("User's Manual"), htmlOutput("manual")),
        tabPanel("Likes", DT::dataTableOutput("mytable_likes")),
        tabPanel("Dislikes", DT::dataTableOutput("mytable_dislikes")),
        tabPanel("Comment Count", DT::dataTableOutput("mytable_comment_count")),
        tabPanel("Views", DT::dataTableOutput("mytable_views")),
        tabPanel("Like Dislike Ratio", DT::dataTableOutput("mytable_like_dislike_ratio")),
        tabPanel("Visualization")
      )
    )
  )
)


server <- function(input, output) {
    
    output$mytable_likes <- DT::renderDataTable({
      data_cleaned %>%
        mutate(likes = as.numeric(likes)) %>%
        filter(category_id == input$category1) %>%
        arrange(desc(likes)) %>%
        select(input$show_vars1)
    })
    
    output$mytable_dislikes <- DT::renderDataTable({
      data_cleaned %>%
        mutate(dislikes = as.numeric(dislikes)) %>%
        filter(category_id == input$category2) %>%
        arrange(dislikes) %>%
        select(input$show_vars2)
    })
    
    output$mytable_comment_count <- DT::renderDataTable({
      data_cleaned %>%
        mutate(comment_count = as.numeric(comment_count)) %>%
        filter(category_id == input$category3) %>%
        arrange(desc(comment_count)) %>%
        select(input$show_vars3)
    })
    
    output$mytable_views <- DT::renderDataTable({
      data_cleaned %>%
        mutate(views = as.numeric(views)) %>%
        filter(category_id == input$category4) %>%
        arrange(desc(views)) %>%
        select(input$show_vars4)
    })
    
    output$mytable_like_dislike_ratio <- DT::renderDataTable({
      data_cleaned %>%
        mutate(like_dislike_ratio = as.numeric(like_dislike_ratio))%>%
        filter(category_id == input$category5)%>%
        arrange(desc(like_dislike_ratio)) %>%
        select(input$show_vars5)
    })
    
    output$manual <- renderText({
      paste("<b> Manual Here", "</b>")
    })
    
}

shinyApp(ui = ui, server = server)

