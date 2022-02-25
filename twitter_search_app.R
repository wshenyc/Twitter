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
  
  if (filter_flag == TRUE & !str_detect(filter_input, "[:alpha:]")) {
    df <- data.frame(Message = c("Filter input must include at least 1 letter."))
  } else { 
  
  # checking if filter input has multiple strings
  if (filter_flag == TRUE & str_detect(filter_input, "[:space:]")) {
    filter_input <- unlist(str_split(filter_input, "[:space:]"))
    df <- get_timeline(user_input, n = num_tweets) 
    if (length(df) > 0) {
    df <- df %>% 
      filter(grepl(paste(filter_input, collapse = "|"), text, ignore.case = TRUE)) %>% 
      tweet_cleaner()
    } else {
      df <- data.frame(message = c("Your search returned no results."))
    }
  # checking if filter exists and is not multiple strings
    } else if (filter_flag == TRUE & !str_detect(filter_input, "[:space:]")) {
      df <- get_timeline(user_input, n = num_tweets)
      if (length(df) >0) {
        df <- df %>% 
          filter(grepl(filter_input, text, ignore.case = TRUE)) %>% 
        tweet_cleaner()
      } else {
        df <- data.frame(Message = c("Your search returned no results."))
      }
  #if no filter, just do standard search
      } else {
        df <- get_timeline(user_input, n = num_tweets) 
        if (length(df) > 0) {
          df <- df %>% 
            tweet_cleaner()
        }else {
          df <- data.frame(message = c("Your search returned no results."))
        }
      }
  } 
}

##----MOST LIKED FUNCTION------------------------------------------------
#
# Description: Find most liked original tweet 
#____________________________________________________________________________

#most liked tweet
most_liked_func <- function(df) {
  renderTable(
    if(max(df$`Like Count`) > 0 & any("No" == df$`Retweet?`)) {
      df %>%
        janitor::clean_names() %>%
        filter(retweet == "No") %>% 
        filter(like_count == max(like_count)) %>%
        distinct(tweet, .keep_all = T) %>% 
        select(twitter_user, tweet, like_count) %>%
        rename("Twitter User" = twitter_user,
               "Tweet" = tweet,
               "Like Count" = like_count)
    } else {
      df <- data.frame(Message = c("No original tweets with more than 0 likes"))
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
  if(max(df$`Retweet Count`) > 0 & any("No" == df$`Retweet?`)) {
    df %>%
      janitor::clean_names() %>%
      filter(retweet == "No") %>% 
      filter(retweet_count == max(retweet_count)) %>%
      distinct(tweet, .keep_all = T) %>% 
      select(twitter_user, tweet, retweet_count) %>%
      rename("Twitter User" = twitter_user,
             "Tweet" = tweet,
             "Retweet Count" = retweet_count)
  }else {
    df <- data.frame(Message = c("No original tweets with more than 0 RTs"))
  }
)}

##----MOST ENGAGEMENT -------------------------------------------------------
#
# Description: Find users with highest level of engagement.
# Engagement score is calculated as: Retweets*2 + Likes
#____________________________________________________________________________

most_eng_users <- function(df) {
  temp_df <- data.frame()
  renderTable(
  #checking that there's at least 1 org tweet and more than 1 twitter user
    if(any("No" == df$`Retweet?`) & n_distinct(df$`Twitter User`) > 10) {
      temp_df <- df %>%
        janitor::clean_names() %>% 
        filter(retweet == "No") %>% 
        group_by(twitter_user) %>% 
        mutate(eng_score = as.integer((sum(retweet_count)*2+sum(like_count)) / n())) %>% 
        distinct(twitter_user, .keep_all = T) %>% 
        subset(eng_score > quantile(eng_score, prob = .90)) %>%  #top 10%
        arrange(desc(eng_score)) %>%
        select(twitter_user, eng_score) %>% 
        head(10) %>% 
        rename("User" = twitter_user,
               "Engagement/Tweet" = eng_score)
    } else if (any("No" == df$`Retweet?`) & n_distinct(df$`Twitter User`) <= 10) {
      temp_df <- df %>%
        janitor::clean_names() %>% 
        filter(retweet == "No") %>% 
        group_by(twitter_user) %>% 
        mutate(eng_score = as.integer((sum(retweet_count)*2+sum(like_count)) / n())) %>% 
        distinct(twitter_user, .keep_all = T) %>% 
        arrange(desc(eng_score)) %>%
        select(twitter_user, eng_score) %>% 
        rename("User" = twitter_user,
               "Engagement/Tweet" = eng_score)
    } else {
      df <- data.frame(Message = c("Search results returned no original tweets"))
    }
  )
}

##----USER INPUT CHECK-------------------------------------------------------
#
# Description: Check validity of user input
#____________________________________________________________________________

input_check <- function(df, user_input) {
  if (user_input == "") {
    df <- data.frame(Message = c("Please enter a keyword"))
  } else if (!is.na(user_input) & !str_detect(user_input, "[:alpha:]")) {
    df <- data.frame(Message = c("Keyword must contain at least one letter."))
  } else {
    
  }
}


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
      id = "tabs",
      menuItem("Keyword Search",
               tabName = "keyword_menu_item",
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
                           actionButton("keyword_search_button", "Keyword Search")),
              menuSubItem(icon = icon("info"),
                          actionButton("show_keywords", "View instructions"))),
      
      menuItem("User(s) Search",
               tabName = "user_search_menu_item",
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
                           actionButton("user_search_button", "User Search")), 
               menuSubItem(icon = icon("info"),
                           actionButton("show_users", "View instructions"))),
      
      menuItem("Electeds Search",
               tabName = "electeds_search_menu_item",
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
                                     "CM Hanif"= "CMShahanaHanif",
                                     "CM Cabán" = "tiffany_caban"), 
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
                           actionButton("electeds_search", "Elected Search")),
               menuSubItem(icon = icon("info"),
                           actionButton("show_electeds", "View instructions")))
      

      
    )
  ),
  
  # Show results

  dashboardBody(
    shinyjs::useShinyjs(), 
    fluidRow(
      shinyjs::hidden(div(id = "tweet_wrapper", 
      shinydashboard::box(
        width = 12,
        title = p("Tweet Table", style = 'font-size:24px;'),
        status = "primary",
        div(style = "overflow-x: scroll",
            DTOutput("tweet_table"))
        ))
      )),
    
    
    
    fluidRow(
      column(2,
             shinyjs::hidden(div(id = "user_wrapper", 
             shinydashboard::box(
               width = NULL,
               title = "Most 'Popular' Users",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("pop_users")),
               "Weighted average of RT (2x) and favorites (1x) per tweet"
             ))
    )),
    
   
    
      column(5,
             shinyjs::hidden(div(id = "liked_wrapper", 
             shinydashboard::box(
               width = NULL,
               title = "Most Liked Tweet",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("most_liked"))
             ))
             )),
    
    
      column(5,
             shinyjs::hidden(div(id = "rt_wrapper", 
             shinydashboard::box(
               width = NULL,
               title = "Most RT'd Tweet",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("most_rt"), width ="100%"))
             ))
      )),
    

   
      shinyjs::hidden(div(id = "help_wrapper", 
                          shinydashboard::box(
                            width = 12,
                            title = p("Help Page", style = 'font-size:24px;'),
                            status = "primary",
                            "Some text"
                         
                          )
      ))
   )
)


##----SERVER LOGIC-----------------------------------------------------------
#
# Description: sets up server logic
#____________________________________________________________________________

server <- function(input, output, session) {
  

  
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
    shinyjs::hide("help_wrapper")
    
    user_input <- input$search_terms

    
    #require user_input to not be empty and have at least one letter
    validate(
      need(user_input != "", 'Please enter a keyword'),
      need(str_detect(user_input, "[:alpha:]"), 'Please enter at least one letter.')
    )
    
    
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
# Description: Calculates most liked org. tweet & most RT'd & users w/ high
# levels of engagement
#____________________________________________________________________________
    
    #users with highest engagement
    output$pop_users <- most_eng_users(df)
    
    #most liked tweet
    output$most_liked <- most_liked_func(df)
    
    #most retweeted tweet
    output$most_rt <- most_rt_func(df)

    #adding the results of the search_tweets to the reactive value 
    rv(df)
    
    shinyjs::show("tweet_wrapper")
    shinyjs::show("user_wrapper")
    shinyjs::show("liked_wrapper")
    shinyjs::show("rt_wrapper")
  })
  

##----USER SEARCH------------------------------------------------------------
#
# Description: sets up user search & filter
#____________________________________________________________________________
  
  #get timelines for specific users 
  observeEvent(input$user_search_button, {
    
    shinyjs::hide("help_wrapper")
    
    user_input <- input$user_search
    num_tweets <- input$num_tweets_to_download
    
    validate(
      need(user_input != "", 'Please enter a keyword'),
      need(str_detect(user_input, "[:alpha:]"), 'Please enter at least one letter.')
    )
    
    shinyjs::show("tweet_wrapper")
    shinyjs::show("user_wrapper")
    shinyjs::show("liked_wrapper")
    shinyjs::show("rt_wrapper")
    
    #blank vars
    filter_flag <- FALSE
    filter_input <- ""
    df <- data.frame()
    
    #testing if there's multiple chars 
    if(str_detect(user_input, "[:space:]")) {
      user_input <- unlist(str_split(user_input, "[:space:]"))
    } else {
      user_input
    }
    
    # testing if there is a filter
    if (input$filter_users != "") {
      filter_flag <- TRUE
      filter_input <- input$filter_users
    } else {
    }
    
    #filter function
    df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
    
    #users with highest engagement
    output$pop_users <- most_eng_users(df)
    
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
    
    shinyjs::hide("help_wrapper")
   
    num_tweets <- input$num_tweets_to_download
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #testing whether nys electeds and/or nyc electeds are selected 
    if (length(input$nys_electeds)==0 & length(input$nyc_electeds) == 0) {
      shiny::validate("Please enter an input.")
    }else if (length(input$nys_electeds)>0 & length(input$nyc_electeds) == 0) {
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
    if (input$filter_electeds != "") {
      filter_flag <- TRUE
      filter_input <- input$filter_electeds
    } else {
    }
    
    #filter function
    df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
    
    #users with highest engagement
    output$pop_users <- most_eng_users(df)
    
    #most liked tweet
    output$most_liked <- most_liked_func(df)
    
    #most retweeted tweet
    output$most_rt <- most_rt_func(df)
    
    #adding the results of the search_tweets to the reactive value
    rv(df)
    
    shinyjs::show("tweet_wrapper")
    shinyjs::show("user_wrapper")
    shinyjs::show("liked_wrapper")
    shinyjs::show("rt_wrapper")
    
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
  



##----HELP PAGE--------------------------------------------------------------
#
# Description: sets up help page
#____________________________________________________________________________

  observeEvent(input$show_keywords, {
    showModal(modalDialog(
      title = "Keywords Search Instructions",
      div(tags$b("Number of Tweets to Download")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Keywords Search")), 
      div("Must be a character string not to exceed maximum of 500 characters."),
      div('To search for tweets containing at least one of multiple possible terms, 
          separate each search term with spaces and "OR" (in caps). For example, the search q = "data science" 
          looks for tweets containing both "data" and "science" located anywhere in the tweets and in any order.'),
      div('When "OR" is entered between search terms, query = "data OR science", 
      Twitter should return any tweet that contains either "data" or "science."'),
      div('It is also possible to search for exact phrases using double quotes. 
      To do this, wrap double quotes around the query, such as "housing new york".'),
      br(),
      div(tags$b("Verified Accounts?")), 
      div('Checking this box will automatically filter the returned search results to 
      only include verified accounts. The number of tweets displayed will likely be less than the 
      "number of tweets to download" as a result'),
      br(),
      div(tags$b("No Retweets?")), 
      div('Checking this box will exclude any tweets that are replies. 
      The number of tweets displayed will likely be equal to the "number of tweets to download".'),
      easyClose = TRUE,
      footer = NULL
    ))
  })

  observeEvent(input$show_users, {
    showModal(modalDialog(
      title = "Users Search Instructions",
      div(tags$b("Number of Tweets to Download")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Users Search")), 
      div("Enter spaces between each screen name if searching for more than one user at once. 
      The number of tweets to download will return that number of tweets per user. 
      For example, if searching NYCMayor and NYCHousing and selecting 100 tweets to download will yield 200 tweets."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to only results that include the keywords."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$show_electeds, {
    showModal(modalDialog(
      title = "Electeds Search Instructions",
      div(tags$b("Number of Tweets to Download")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Electeds Search")), 
      div("Select or search for any elected official's name. Selection boxes can be used together or separately."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to only results that include the keywords."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
