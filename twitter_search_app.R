library(shiny)
library(rtweet)
library(tidyverse)
library(DT)
library(httr)
library(glue)
library(lubridate)
library(shinydashboard)

##----TWITTER AUTHORIZATION TOKEN--------------------------------------------
#
# Description: The section checks that the Twitter auth token is available and 
# allows users to use Winnie's twitter auth code to call the Twitter API. 
# This section will be updated with a developer API, which is pending approval.
#
# Source: https://github.com/gadenbuie/tweet-conf-dash
#____________________________________________________________________________

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

##----TWEET CLEANER FUNCTION-------------------------------------------------
#
# Description: Custom function to clean the results of RTweets functions
#____________________________________________________________________________

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

##----FILTER FUNCTION---------------------------------------------------------
#
# Description: custom function to filter user search results
#____________________________________________________________________________

filter_func <- function(df, filter_flag, filter_input, user_input, num_tweets) {
  # checking if filter input has multiple strings
  if (filter_flag == TRUE & str_detect(filter_input, "[:space:]")) {
    filter_input <- unlist(str_split(filter_input, "[:space:]"))
    df <- get_timeline(user_input, n = num_tweets) %>%
    filter(grepl(paste(filter_input, collapse = "|"), text, ignore.case = TRUE)) %>% 
    tweet_cleaner()
  # checking if filter exists and is not multiple strings
    } else if (filter_flag == TRUE & !str_detect(filter_input, "[:space:]")) {
      df <- get_timeline(user_input, n = num_tweets) %>%
        filter(grepl(filter_input, text, ignore.case = TRUE)) %>% 
        tweet_cleaner()
  #if no filter, just do standard search
      } else {
        df <- get_timeline(user_input, n = num_tweets) %>%
          tweet_cleaner()
      }
  } 

##----MOST LIKED FUNCTION------------------------------------------------
#
# Description: Find most liked original tweet 
#____________________________________________________________________________

#most liked tweet
most_liked_func <- function(df) {
  renderTable(
    if(max(df$`Like Count`) > 0 & any("No" == df$`Retweet`)) {
      df %>%
        janitor::clean_names() %>%
        filter(retweet == "No") %>% 
        filter(like_count == max(like_count)) %>%
        select(twitter_user, tweet, like_count) %>%
        rename("Twitter User" = twitter_user,
               "Tweet" = tweet,
               "Like Count" = like_count)
    } else {
      return("No original tweets with more than 0 likes")
    }
  )
}

##----MOST RETWEETED FUNCTION------------------------------------------------
#
# Description: Find most retweeted original tweet 
#____________________________________________________________________________

#most retweeted tweet
most_rt_func <- function(df) {
renderTable(
  if(max(df$`Retweet Count`) > 0 & any("No" == df$`Retweet`)) {
    df %>%
      janitor::clean_names() %>%
      filter(retweet == "No") %>% 
      filter(retweet_count == max(retweet_count)) %>%
      select(twitter_user, tweet, retweet_count) %>%
      rename("Twitter User" = twitter_user,
             "Tweet" = tweet,
             "Retweet Count" = retweet_count)
  }else {
    return("No original tweets with more than 0 RTs")
  }
  )}


##----USER INPUT CHECK-------------------------------------------------------
#
# Description: Check validity of user input
#____________________________________________________________________________

# input_check <- function(user_input) {
#   if (is.null(user_input)) {
#     return()
#   }
# }

#things to check for are: is null, only punctuation 

###also need to check for if what a user enters returns an empty table
###to tell them that! 

##----USER INTERFACE----------------------------------------------------------
#
# Description: sets up UI
#____________________________________________________________________________


ui <- dashboardPage(
  
  # Application title
  dashboardHeader(
    title = span("Twitter Search",
                 style = "font: Avenir;")),
  
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
                          checkboxInput("org_only", "No retweets?", FALSE)),
               menuSubItem(icon = NULL,
                           actionButton("keyword_search_button", "Keyword Search"))),
      
      menuItem("User(s) Search",
               icon = icon("users"),
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
                                     placeholder = "Enter Twitter user(s)")),
               
               menuSubItem(icon = NULL,
                           textInput('filter_users',
                                     'Filter:',
                                     placeholder = "Enter keywords")),
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
                           shinyWidgets::pickerInput(
                           "nyc_electeds",
                           "NYC Electeds", 
                           choices=c("Mayor Adams" = "NYCMayor",
                                     "CM Hanif"= "CMShahanaHanif"), 
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE,
                                          `live-search-placeholder` = "Search name"),
                           multiple = T)),
               
               
               menuSubItem(icon = NULL,
                           shinyWidgets::pickerInput(
                             "nys_electeds",
                             "NYS Electeds", 
                             choices=c("AM Niou" = "yuhline",
                                       "Sen Kavanagh" = "BrianKavanaghNY",
                                       "Sen Zellnor" = "zellnor4ny"),
                             options = list(`actions-box` = TRUE,
                                            `live-search` = TRUE,
                                            `live-search-placeholder` = "Search name"),
                             multiple = T)),
               
               menuSubItem(icon = NULL,
                           textInput('filter_electeds',
                                     'Filter:',
                                     placeholder = "Enter keywords")),
               
               menuSubItem(icon = NULL,
                           actionButton("electeds_search", "Elected Search")))
      
    )
  ),
  
  # Show results

  dashboardBody(

   
    fluidRow(
      shinydashboard::box(
        width = 12,
        title = p("Tweet Table", style = 'font-size:24px;'),
        status = "primary",
        div(style = "overflow-x: scroll",
            DTOutput("tweet_table", width = "100%"))
        )
      ),
    fluidRow(
      column(4, 
             shinydashboard::box(
               width = 10,
               title = "Most Liked Tweet",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("most_liked"), width = "100%"))
             ),
      column(4,
             shinydashboard::box(
               width = 10,
               title = "Most RT'd Tweet",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("most_rt"), width ="100%"))
             )
    )
   )
)

##----SERVER LOGIC-----------------------------------------------------------
#
# Description: sets up server logic
#____________________________________________________________________________

server <- function(input, output) {
  
  #creating a blank reactive val to save the results of the Twitter searches in
  #need a reactive value because this needs to automatically re-execute when user inputs change 
  rv = reactiveVal()
  summary = reactiveVal()
  most_retweet = reactiveVal()
  most_liked = reactiveVal()
  
  #creating blank dataframe
  df <- data.frame()
  
  
##----KEYWORD SEARCH---------------------------------------------------------
#
# Description: sets up keyword search
#____________________________________________________________________________
  
  observeEvent(input$keyword_search_button, {
    
    user_input <- input$search_terms
    
    #checking if the verified check box was selected and returning only verified results if true
    if (input$verified & !input$org_only) {
      filter <- ' filter:verified'
      user_input <- str_glue('{user_input}{filter}')
    } else if (!input$verified & input$org_only) { 
      filter <- ' -filter:retweets'
      user_input <- str_glue('{user_input}{filter}')
    } else if (input$verified & input$org_only) {
      filter <- ' -filter:retweets filter:verified'
      user_input <- str_glue('{user_input}{filter}')
    } else {
      user_input
    }
    
   
    
    #setting df variable to the results of search_tweets
    df <- search_tweets(user_input, n = input$num_tweets_to_download) %>% 
      tweet_cleaner() #calling custom function 
  
##----SUMMARY TABLES---------------------------------------------------------
#
# Description: Calculates most liked org. tweet & most RT'd
#____________________________________________________________________________
    
    #summary df
    #returning users with the most tweets 
    # df_sum <- df %>%
    #   janitor::clean_names() %>% 
    #   count(twitter_user) %>% 
    #   filter(n > quantile(n, probs = .90)) %>% #taking the top 10%
    #   arrange(desc(n)) %>%
    #   rename('Number of Tweets' = n,
    #          'Twitter User' = twitter_user) %>%
    #   select('Twitter User', 'Number of Tweets') %>% 
    #   head(10)
    
    #most liked tweet
    output$most_liked <- most_liked_func(df)
    
    #most retweeted tweet
    output$most_rt <- most_rt_func(df)

    #adding the results of the search_tweets to the reactive value 
    rv(df)
  })
  

##----USER SEARCH------------------------------------------------------------
#
# Description: sets up user search & filter
#____________________________________________________________________________
  
  #get timelines for specific users 
  observeEvent(input$user_search_button, {
    
    user_input <- input$user_search
    num_tweets <- input$num_tweets_to_download
    
    #blank vars
    filter_flag <- FALSE
    filter_input <- ""
    df <- data.frame()
    
    if(str_detect(user_input, "[:space:]")) {
      user_input <- unlist(str_split(user_input, "[:space:]"))
    } else {
      user_input
    }
    
    # testing if there is a filter
    if (length(input$filter_users) > 0) {
      filter_flag <- TRUE
      filter_input <- input$filter_users
    } else {
    }
    
    #filter function
    df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
    
    #most liked tweet
    output$most_liked <- most_liked_func(df)
    
    #most retweeted tweet
    output$most_rt <- most_rt_func(df)
    
    #adding the results of the search_tweets to the reactive value 
    rv(df)
    
  })
  
##----ELECTEDS SEARCH--------------------------------------------------------
#
# Description: sets up electeds search & filter
#____________________________________________________________________________
  
  
  #get timeline search for electeds
  observeEvent(input$electeds_search, {
   
    num_tweets <- input$num_tweets_to_download
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #testing whether nys electeds and/or nyc electeds are selected 
    if (length(input$nys_electeds)>0 & length(input$nyc_electeds) == 0) {
      user_input <- input$nys_electeds
    } else if(length(input$nys_electeds)==0 & length(input$nyc_electeds) > 0) {
      user_input <- input$nyc_electeds
    } else if (length(input$nys_electeds)>0 & length(input$nyc_electeds) > 0) {
      #this line of code is pasting the selected nys and nyc electeds together
      #then it is splitting those into separate strings based on whitespace
      #finally it is converting the list into a vector
      #so that the get_timeline() function works
      user_input <- unlist(str_split(paste(input$nys_electeds, input$nyc_electeds), "[:space:]"))
    } else {
      user_input
    }
    
    # testing if there is a filter
    if (length(input$filter_electeds) > 0) {
      filter_flag <- TRUE
      filter_input <- input$filter_electeds
    } else {
    }
    
    #filter function
    df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
    
    #most liked tweet
    output$most_liked <- most_liked_func(df)
    
    #most retweeted tweet
    output$most_rt <- most_rt_func(df)
    
    #adding the results of the search_tweets to the reactive value
    rv(df)
    
  })
  
##----TWEET TABLE & LOADING BAR----------------------------------------------
#
# Description: sets up tweet table & creates a loading bar
#____________________________________________________________________________
  
  
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
                                           incProgress(1/n, detail = paste(i*10, "%", sep = ""))
                                           
                                           # Pause for 0.05 seconds to simulate a long computation.
                                           Sys.sleep(0.05)
                                         }
                                         
                                       })
                                      } else {
                                      }
                                       
                                       #returning the dt 
                                       rv()
                                     })
  

}

# Run the application 
shinyApp(ui = ui, server = server)
