library(shiny)
library(rtweet)
library(tidyverse)
library(DT)
library(httr)
library(glue)
library(lubridate)
library(shinydashboard)

#twitter auth token 
TWEETS_MANAGE_UPDATES <- TRUE

TWEETS_MANAGE_TWITTER_PAT_RDS <- "twitter_token.rds"

TWEETS_MANAGE_UPDATE_INTERVAL <- 5 * 60

if (TWEETS_MANAGE_UPDATES) {
  # Check that TWITTER_PAT is correctly set
  if (Sys.getenv("TWITTER_PAT") == "") {
    if (file.exists(TWEETS_MANAGE_TWITTER_PAT_RDS)) {
      Sys.setenv(TWITTER_PAT = TWEETS_MANAGE_TWITTER_PAT_RDS)
    } else {
      warning(
        "I can't find the file containing your Twitter PAT, so live ",
        "updating with {rtweet} won't be possible. See {rtweet} for help ",
        "authenticating and set the config TWEETS_MANAGE_TWITTER_PAT_RDS in ",
        "'R/custom/00_settings.R'."
      )
      TWEETS_MANAGE_UPDATES <- FALSE
    }
  }
}

tweet_cleaner <- function (df) {
  df <- select(df, created_at,
               screen_name,
               text,
               is_quote,
               is_retweet,
               favorite_count,
               retweet_count,
               quote_count,
               reply_count,
               hashtags,
               verified) 
  df <- mutate(df, is_quote = ifelse(is_quote == "TRUE",
                                     "Yes",
                                     "No"),
               is_retweet = ifelse(is_retweet == "TRUE",
                                   "Yes",
                                   "No"),
               verified = ifelse(verified == "TRUE",
                                 "Yes",
                                 "No"),
               created_at = as.Date(created_at))  #converting to date format
  df <-  rename(df, "Tweet Date" = created_at,
                "Twitter User" = screen_name,
                "Tweet" = text,
                "Quote Tweet?" = is_quote,
                "Retweet?" = is_retweet,
                "Like Count" = favorite_count,
                "Retweet Count" = retweet_count,
                "Quote Count" = quote_count,
                "Reply Count" = reply_count,
                "Hashtags Used" = hashtags,
                "Verified?" = verified
  )
  
}


api_key <- "afYS4vbIlPAj096E60c4W1fiK"

ui <- dashboardPage(
  
  # Application title
  dashboardHeader(title = "Twitter Search"),
  
  # Sidebar 
  dashboardSidebar( 
    sidebarMenu(
      menuItem("Keyword Search",
               icon = icon("comment-dots"),
               menuSubItem(icon = NULL,
                           numericInput("num_tweets_to_download",
                                        "Number of Tweets to Download:",
                                        min = 100,
                                        max = 18000,
                                        value = 200,
                                        step = 100)),
               menuSubItem(icon = NULL,
                           textInput('search_terms',
                                     "Keywords Search:",
                                     placeholder = "Enter search query")),
               menuSubItem(icon = NULL,
                           checkboxInput("verified", "Verified accounts only?", FALSE)),
               menuSubItem(icon = NULL,
                           actionButton("keyword_search_button", "Keyword Search"))),
      
      menuItem("User Search",
               icon = icon("user"),
               menuSubItem(icon = NULL,
                           numericInput("num_tweets_to_download",
                                        "Number of Tweets to Download:",
                                        min = 100,
                                        max = 18000,
                                        value = 200,
                                        step = 100)),
               menuSubItem(icon = NULL,
                           textInput('user_search',
                                     'User Search:',
                                     placeholder = "Enter Twitter user")),
               menuSubItem(icon = NULL,
                           actionButton("user_search_button", "User Search"))),
      
      menuItem("Electeds Search",
               icon = icon("landmark"),
               menuSubItem(icon = NULL,
                           numericInput("num_tweets_to_download",
                                        "Number of Tweets to Download:",
                                        min = 100,
                                        max = 18000,
                                        value = 200,
                                        step = 100)),
               menuSubItem(icon = NULL,
                           selectInput(
                             "nyc_electeds",
                             "NYC Electeds",
                             c("Mayor Adams" = "NYCMayor",
                               "CM Hanif"= "CMShahanaHanif"),
                             selected = NULL,
                             multiple = TRUE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                             )),
               
               menuSubItem(icon = NULL,
                           selectInput(
                             "nys_electeds",
                             "NYS Electeds",
                             c("AM Niou" = "yuhline",
                               "Sen Kavanagh" = "BrianKavanaghNY",
                               "Sen Zellnor" = "zellnor4ny"),
                             selected = NULL,
                             multiple = TRUE,
                             selectize = TRUE,
                             width = NULL,
                             size = NULL
                           )),
               
               # menuSubItem(icon = NULL,
               #             checkboxInput("nyc_electeds", "NYC Electeds only?", FALSE)),
               # menuSubItem(icon = NULL,
               #             checkboxInput("nys_electeds", "NYS Electeds only", FALSE)),
               menuSubItem(icon = NULL,
                           actionButton("electeds_search", "Elected Search")))
      
      
      # DTOutput("most_liked_tweet"),
      # DTOutput("most_retweeted_tweet"),
      
      # DTOutput("summary_table"),
      
    )
  ),
  
  # Show results
  dashboardBody(
    
    DTOutput("tweet_table"),
    
    
  )
) 



# Define server logic 
server <- function(input, output) {
  
  #creating a blank reactive val to save the results of the Twitter searches in
  #need a reactive value because this needs to automatically re-execute when user inputs change 
  rv = reactiveVal()
  summary = reactiveVal()
  most_retweet = reactiveVal()
  most_liked = reactiveVal()
  
  #creating blank dataframe
  df <- data.frame()
  
  observeEvent(input$keyword_search_button, {
    
    user_input <- input$search_terms
    
    #checking if the verified check box was selected and returning only verified results if true
    if (input$verified) {
      filter <- ' filter:verified'
      user_input <- str_glue('{user_input}{filter}')
    } else {
      user_input
    }
    
    #setting df variable to the results of search_tweets
    df <- search_tweets(user_input, n = input$num_tweets_to_download) %>% 
      tweet_cleaner() #calling custom function from the helpers.R file
    
    #summary df
    #returning users with the most tweets 
    df_sum <- df %>%
      janitor::clean_names() %>% 
      count(twitter_user) %>% 
      filter(n > quantile(n, probs = .90)) %>% #taking the top 10%
      arrange(desc(n)) %>%
      rename('Number of Tweets' = n,
             'Twitter User' = twitter_user) %>%
      select('Twitter User', 'Number of Tweets') %>% 
      head(10)
    
    #most retweeted and most favorited tweets
    df_popular <- df %>% 
      janitor::clean_names() %>% 
      filter(like_count == max(like_count)) %>% 
      select(twitter_user, tweet, like_count) %>% 
      rename("Twitter User" = twitter_user,
             "Tweet" = tweet,
             "Like Count" = like_count)
    
    df_retweeted <- df %>% 
      janitor::clean_names() %>% 
      filter(retweet_count == max(retweet_count)) %>% 
      head(1) %>% 
      select(twitter_user, tweet, retweet_count) %>% 
      rename("Twitter User" = twitter_user,
             "Tweet" = tweet,
             "Retweet Count" = retweet_count)
    
    
    #adding the results of the search_tweets to the reactive value 
    rv(df)
    summary(df_sum)
    most_liked(df_popular)
    most_retweet(df_retweeted)
  })
  
  
  
  #get timelines for specific users 
  observeEvent(input$user_search_button, {
    
    user_input <- input$user_search
    
    if(str_detect(user_input, "[:space:]")) {
      user_input <- unlist(str_split(user_input, "[:space:]"))
    } else {
      user_input
    }
    
    df <- get_timeline(user_input, n = input$num_tweets_to_download) %>%
      tweet_cleaner()
    
    #adding the results of the search_tweets to the reactive value 
    rv(df)
    
  })
  
  #get timeline search for electeds
  observeEvent(input$electeds_search, {
    
    #setting blank variable
    user_input <- ""
    
    #all electeds
    #will probably be reading in a csv file at a later date 
    # all_nys_electeds <- c("yuhline", "BrianKavanaghNY", "zellnor4ny")
    # all_nyc_electeds <- c("NYCMayor", "CMShahanaHanif", "pisancheznyc")
    # nys_nyc_electeds <- c(all_nyc_electeds, all_nys_electeds)
    
    #checking if the verified check box was selected and returning only verified results if true
    if (input$nys_electeds & !input$nyc_electeds) {
      user_input <- input$nys_electeds
    } else if(!input$nys_electeds & input$nyc_electeds) {
      user_input <- input$nyc_electeds
    } else if (input$nys_electeds & input$nyc_electeds) {
      user_input <- unlist(paste(input$nys_electeds, input$nyc_electeds))
    } else {
      user_input
    }
    
    print(user_input)
    
    df <- get_timeline(user_input, n = input$num_tweets_to_download) %>%
      tweet_cleaner()
    
    #adding the results of the search_tweets to the reactive value 
    rv(df)
    
  })
  
  #saves the dfs generated from above to the tweet_table var on the UI
  output$tweet_table <- DT::renderDT(server = FALSE, #setting server as false will render all results at once, potentially affecting load times
                                     extensions = 'Buttons',
                                     options = list(dom = 'Bltipr',
                                                    buttons = list(list(extend = 'collection',
                                                                        buttons = c('csv', 'excel'), #files can be downloaded as a csv or excel
                                                                        text = "Download Results",
                                                                        exportOptions = list(
                                                                          modifiers = list(page = "all") #will download all results
                                                                        )))), 
                                     filter = "top", {
                                 
                                      #requiring one of the inputs to have been clicked
                                      if(input$keyword_search_button |
                                          input$user_search_button |
                                          input$electeds_search) {
                                       
                                      #creating empty df
                                       dat <- data.frame(x = numeric(0), y = numeric(0))
                                       
                                      #progress bar
                                       withProgress(message = 'Creating table', value = 0, {
                                         # Number of times we'll go through the loop
                                         n <- 10
                                         
                                         for (i in 1:n) {
                                           # Each time through the loop, add another row of data. This is
                                           # a stand-in for a long-running computation.
                                           dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
                                           
                                           # Increment the progress bar, and update the detail text.
                                           incProgress(1/n, detail = paste("Loading", i*10, "%"))
                                           
                                           # Pause for 0.1 seconds to simulate a long computation.
                                           Sys.sleep(0.1)
                                         }
                                         
                                       })
                                      } else {
                                      }
                                       
                                       #returning the dt 
                                       rv()
                                     })
  
  ###this section needs updating
  #generating tables for the most liked/retweeted tweets
  output$most_liked_tweet <- DT::renderDT(options = list(dom = 't'), {
    #returning other reactive value
    most_liked()
  })
  
  output$most_retweeted_tweet <- DT::renderDT(options = list(dom = 't'), {
    #returning other reactive value
    most_retweet()
  })
  
  output$summary_table <- DT::renderDT(options = list(dom = 't'), {
    #returning other reactive value
    summary()
  })
  
  
  
  #generate a summary table based on the tweet table
  output$summary_table <- DT::renderDT(options = list(dom = 't'), {
    #returning other reactive value
    summary()
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
