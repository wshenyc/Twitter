library(shiny)
library(rtweet)
library(tidyverse)
library(DT)
library(gfonts)
library(glue)
library(lubridate)
library(shinydashboard)
library(dashboardthemes)

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
  
    df <- df %>% 
      janitor::clean_names() %>% 
      filter(retweet == "No") %>%
      filter(like_count > 0)
    
    if (nrow(df) != 0) {
      df <- df %>% 
        filter(like_count == max(like_count)) %>%
        distinct(tweet, .keep_all = T) %>%
        select(twitter_user, tweet, like_count) %>%
        rename("Twitter User" = twitter_user,
               "Tweet" = tweet,
               "Like Count" = like_count)
    } else {
      df <- data.frame(Message = c("No original tweets with more than 0 likes"))
    }
  renderTable(df)
}
    

##----MOST RETWEETED FUNCTION------------------------------------------------
#
# Description: Find most retweeted original tweet 
#____________________________________________________________________________

#most retweeted tweet
most_rt_func <- function(df) {
  df <- df %>% 
    janitor::clean_names() %>% 
    filter(retweet == "No") %>%
    filter(retweet_count > 0)
  
  if (nrow(df) != 0) {
    df <- df %>%
      filter(retweet_count == max(retweet_count)) %>%
      distinct(tweet, .keep_all = T) %>%
      select(twitter_user, tweet, retweet_count) %>%
      rename("Twitter User" = twitter_user,
             "Tweet" = tweet,
             "RT Count" = retweet_count)
  } else {
    df <- data.frame(Message = c("No original tweets with more than 0 RT's"))
  }
  renderTable(df)
}

##----MOST ENGAGEMENT -------------------------------------------------------
#
# Description: Find users with highest level of engagement.
# Engagement score is calculated as: Retweets*2 + Likes
#____________________________________________________________________________

most_eng_users <- function(df) {
  
  #creating temp df
  temp_df <- data.frame()
  
  #filtering out retweets
  df <- df %>% 
    janitor::clean_names() %>%
    filter(retweet == "No")
  
  #case 1: if resulting df has at least 1 row and more than 10 unique twitter users
  #then create an engagement score column 
  if(nrow(df) != 0 & n_distinct(df$twitter_user) > 10) {
    temp_df <- df %>%
      group_by(twitter_user) %>%
      mutate(eng_score = as.integer((sum(retweet_count)*2+sum(like_count)) / n()))
    
    # case 1a: if the engagement score total isn't 0, meaning at least one user has a
    #non-zero engagement zero, move forward with returning the top 10% users
    #capping the returned df to 10 unique users 
    if(sum(temp_df$eng_score) > 0) {
      temp_df <- temp_df %>% 
        distinct(twitter_user, .keep_all = T) %>%
        subset(eng_score > quantile(eng_score, prob = .90)) %>%  #top 10%
        arrange(desc(eng_score)) %>%
        select(twitter_user, eng_score) %>%
        head(10) %>%
        rename("User" = twitter_user,
               "Engagement/Tweet" = eng_score)
      
      #case 1b: if the engagement score total is zero, inform user 
    } else {
      temp_df <- data.frame(Message = c("Search returned no users with engagement scores higher than zero."))
    }
    
    #case 2: if resulting df has at least 1 row and less than or equal to 10 unique
    #users, generate engagement score column 
  } else if (nrow(df) != 0 & n_distinct(df$twitter_user) <= 10) {
    temp_df <- df %>%
      group_by(twitter_user) %>%
      mutate(eng_score = as.integer((sum(retweet_count)*2+sum(like_count)) / n()))
    
    #case 2a: check engagement score total is greater than 0
    #keeping all users' engagement scores to be displayed in descending order 
    if (sum(temp_df$eng_score) >0) {
      temp_df <- temp_df %>% 
        distinct(twitter_user, .keep_all = T) %>%
        arrange(desc(eng_score)) %>%
        select(twitter_user, eng_score) %>%
        rename("User" = twitter_user,
               "Engagement/Tweet" = eng_score)
      
      #case 2b: if engagement score total is zero, inform user 
    } else {
      temp_df <- data.frame(Message = c("Search returned no users with engagement scores higher than zero."))
    }
    
    #case 3: if resulting df has no rows, inform user all returned tweets were RTs
  } else {
    temp_df <- data.frame(Message = c("Search results returned no original tweets"))
  }
  
  #finally render results to UI 
renderTable(temp_df)
}

##----CUSTOM LOGO-----------------------------------------------------------
#
# Description: Creates custom logo
#____________________________________________________________________________

customLogo <- shinyDashboardLogoDIY(
  boldText = paste(icon("twitter"), "Twitter")
  ,mainText = "Search"
  ,textSize = 20
  ,badgeText = "v1.1"
  ,badgeTextColor = "white"
  ,badgeTextSize = 2
  ,badgeBackColor = "#88bed1"
  ,badgeBorderRadius = 3
)

##----CUSTOM THEME-----------------------------------------------------------
#
# Description: Creates custom theme
#____________________________________________________________________________


customTheme <- dashboardthemes::shinyDashboardThemeDIY(
  
  #general
  appFontFamily = 'Verdana'
  ,appFontColor = "rgb(0,0,0)"
  ,primaryFontColor = "rgb(0,0,0)"
  ,infoFontColor = "rgb(0,0,0)"
  ,successFontColor = "rgb(0,0,0)"
  ,warningFontColor = "rgb(0,0,0)"
  ,dangerFontColor = "rgb(0,0,0)"
  ,bodyBackColor = "rgb(248,248,248)"
  
  #header
  ,logoBackColor = "rgba(19,156,202,255)"
  
  ,headerButtonBackColor = "rgba(19,156,202,255)"
  ,headerButtonIconColor = "rgb(75,75,75)"
  ,headerButtonBackColorHover = "rgba(18, 140, 183, 1)"
  ,headerButtonIconColorHover = "rgb(0,0,0)"
  
  ,headerBackColor = "rgba(19,156,202,255)"
  ,headerBoxShadowColor = "#aaaaaa"
  ,headerBoxShadowSize = "2px 2px 2px"

  ### sidebar
  ,sidebarBackColor = "rgb(232, 232, 232, 1)"
  ,sidebarPadding = 0
  
  ,sidebarMenuBackColor = "transparent"
  ,sidebarMenuPadding = 0
  ,sidebarMenuBorderRadius = 0
  
  ,sidebarShadowRadius = "3px 5px 5px"
  ,sidebarShadowColor = "#aaaaaa"
  
  ,sidebarUserTextColor = "rgb(255,255,255)"
  
  ,sidebarSearchBackColor = "rgb(55,72,80)"
  ,sidebarSearchIconColor = "rgb(153,153,153)"
  ,sidebarSearchBorderColor = "rgb(55,72,80)"
  
  ,sidebarTabTextColor = "rgb(50,50,50)"
  ,sidebarTabTextSize = 13
  ,sidebarTabBorderStyle = "none none solid none"
  ,sidebarTabBorderColor = "rgb(35,106,135)"
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "rgb(209, 209, 209, 1)" #this changes the color of the tab after it's been clicked
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = "rgb(209, 209, 209, 1)" #changes color of tab on hover
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(75,126,151)"
  ,sidebarTabBorderWidthHover = 1
  ,sidebarTabRadiusHover = "0px 20px 20px 0px"
  
  ### boxes
  ,boxBackColor = "rgb(255,255,255)"
  ,boxBorderRadius = 5
  ,boxShadowSize = "0px 1px 1px"
  ,boxShadowColor = "rgba(0,0,0,.1)"
  ,boxTitleSize = 16
  ,boxDefaultColor = "rgb(210,214,220)"
  ,boxPrimaryColor = "rgb(20,155,203)" #this changes box tab color
  ,boxInfoColor = "rgb(210,214,220)"
  ,boxSuccessColor = "rgba(0,255,213,1)"
  ,boxWarningColor = "rgb(244,156,104)"
  ,boxDangerColor = "rgb(255,88,55)"
  
  ,tabBoxTabColor = "rgb(255,255,255)"
  ,tabBoxTabTextSize = 12
  ,tabBoxTabTextColor = "rgb(0,0,0)"
  ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
  ,tabBoxBackColor = "rgb(255,255,255)"
  ,tabBoxHighlightColor = "rgba(44,222,235,1)"
  ,tabBoxBorderRadius = 5
  
  ### inputs
  ,buttonBackColor = "rgb(245,245,245)"
  ,buttonTextColor = "rgb(0,0,0)"
  ,buttonBorderColor = "rgb(200,200,200)"
  ,buttonBorderRadius = 5
  
  ,buttonBackColorHover = "rgb(235,235,235)"
  ,buttonTextColorHover = "rgb(100,100,100)"
  ,buttonBorderColorHover = "rgb(200,200,200)"
  
  ,textboxBackColor = "rgb(255,255,255)"
  ,textboxBorderColor = "rgb(200,200,200)"
  ,textboxBorderRadius = 5
  ,textboxBackColorSelect = "rgb(245,245,245)"
  ,textboxBorderColorSelect = "rgb(200,200,200)"
  
  ### tables
  ,tableBackColor = "rgb(255,255,255)"
  ,tableBorderColor = "rgb(240,240,240)"
  ,tableBorderTopSize = 1
  ,tableBorderRowSize = 1
  
)

##----ELECTEDS TWITTERS------------------------------------------------------
#
# Description: Downloads electeds' Twitter handle data
#____________________________________________________________________________


electeds <- readxl::read_excel("R:/POLICY-STRATEGY-HOUSING POLICY/Data Projects/Twitter Project for IGA/NYC Electeds 2022.xlsx") %>% 
  arrange(Elected) %>% 
  na.omit(Handle)

nyc_electeds <- electeds %>% 
  filter(Type == "nyccouncil") 

nysa_electeds <- electeds %>% 
  filter(Type == "nycassembly")

nyssen_electeds <- electeds %>% 
  filter(Type == "nycsenate")

choices_nyc_electeds <- setNames(nyc_electeds$Handle, nyc_electeds$Elected)

choices_nysa_electeds <- setNames(nysa_electeds$Handle, nysa_electeds$Elected)

choices_nyss_electeds <- setNames(nyssen_electeds$Handle, nyssen_electeds$Elected)

##----FONTS------------------------------------------------------------------
#
# Description: sets up fonts
#____________________________________________________________________________

use_pkg_gfont(font = "roboto", selector = "body")

##----USER INTERFACE---------------------------------------------------------
#
# Description: sets up UI
#____________________________________________________________________________


ui <- 
  tagList(
  dashboardPage(
    
    # Browers Window title
    title = "Twitter Search",
    
  # Header 
  header = dashboardHeader(
    title = customLogo),

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
                           choices = choices_nyc_electeds,
                           options = list(`actions-box` = TRUE,
                                          `live-search` = TRUE,
                                          `live-search-placeholder` = "Search name",
                                          `size` = 10),
                           multiple = T)),
               
               
               menuSubItem(icon = NULL,
                           shinyWidgets::pickerInput(
                             "nysa_electeds",
                             "NYS Assemblymembers", 
                             choices= choices_nysa_electeds,
                             options = list(`actions-box` = TRUE,
                                            `live-search` = TRUE,
                                            `live-search-placeholder` = "Search name",
                                            `size` = 10),
                             multiple = T)),
               
               menuSubItem(icon = NULL,
                           shinyWidgets::pickerInput(
                             "nyss_electeds",
                             "NYS Senators", 
                             choices= choices_nyss_electeds,
                             options = list(`actions-box` = TRUE,
                                            `live-search` = TRUE,
                                            `dropup-auto` = F,
                                            `live-search-placeholder` = "Search name",
                                            `size` = 10),
                             multiple = T,
                             )),
               
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
    
    customTheme,
    
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
      ))
    ,
    
    

    fluidRow(
      column(3,
             shinyjs::hidden(div(id = "user_wrapper", 
             shinydashboard::box(
               width = NULL,
               title = "Most 'Popular' Users",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("pop_users")),
               "Weighted average of RT (2x) and favorites (1x) per tweet"
             ))
    ))
    ,
    
   
    
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
    
    
      column(4,
             shinyjs::hidden(div(id = "rt_wrapper", 
             shinydashboard::box(
               width = NULL,
               title = "Most RT'd Tweet",
               status = "primary",
               div(style = "overflow-x:scroll",
                   tableOutput("most_rt"), width ="100%"))
             ))
      ))
   )),



tags$footer(img(src="https://housingconnect.nyc.gov/PublicWeb/assets/images/hpd-logo@2x.png", height = "55px"), 
            img(src="https://www.fedex.com/content/dam/fedex/us-united-states/services/Gradient%20Delivery%20Icon-1.png", height = "75px"),
align = "right", style = "
              position:relative;
              bottom:0;
              width:100%;
              height:80px;   /* Height of the footer */
              color: white; #font color
              #padding: 10px;
              padding-right: 20px;
              padding-left: 20px;
              padding-top: 5px;
              background-color: #333;
              #z-index: 1000;
            ") #this will move the footer to be on top of the sidebar

)



##----SERVER LOGIC-----------------------------------------------------------
#
# Description: sets up server logic
#____________________________________________________________________________

server <- function(input, output, session) {
  
  # the modal dialog upon start up
  query_modal <- modalDialog(
    title = "Introduction",
    div(tags$b("General overview")), 
    br(),
    div("Twitter Search is an app that allows users to:"),
    div(tags$ul(
          tags$li("Search most recent Tweets containing specified key words from the last 6-9 days."), 
          tags$li("Return timelines for specified Twitter users"), 
          tags$li("Filter timeline results by specified key words")
        )),
    br(),
    div("To start, select one of the tabs on the left."),
    br(),
    easyClose = T,
    footer = modalButton("Close")
    )
  
  
  # Show the model on start up ...
  showModal(query_modal)
  
  
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

    
    #require user_input to not be empty and have at least one letter
    validate(
      need(user_input != "", 'Please enter a keyword'),
      need(str_detect(user_input, "[:alpha:]"), 'Please enter at least one letter.')
    )
    
    # print(user_input)
    
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
    df <- search_tweets(user_input, n = input$num_tweets_to_download) 
    
    if(length(df) > 1) {
      df <- df %>% 
        tweet_cleaner() #calling custom function 
    } else {
      df <- data.frame(Message = c("Search returned no results."))
    }
    
    
  
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
    
    #if no inputs selected
    if (length(input$nyc_electeds) == 0 & length(input$nysa_electeds) == 0 & length(input$nyss_electeds) == 0) {
      shiny::validate("Please enter an input.")
    
      #if just nyc councilmember selected
    } else if (length(input$nyc_electeds) > 0 & length(input$nysa_electeds) == 0 & length(input$nyss_electeds) == 0) {
      user_input <- input$nyc_electeds
      
      #if just nysa selected
    } else if (length(input$nyc_electeds) == 0 & length(input$nysa_electeds) > 0 & length(input$nyss_electeds) == 0) {
      user_input <- input$nysa_electeds
      
      #if just nys sen selected
    } else if (length(input$nyc_electeds) == 0 & length(input$nysa_electeds) == 0 & length(input$nyss_electeds) > 0) {
      user_input <- input$nyss_electeds 
      
      #if nycc and nysa selected 
    } else if (length(input$nyc_electeds) > 0 & length(input$nysa_electeds) > 0 & length(input$nyss_electeds) == 0) {
      #this line of code is pasting the selected nys and nyc electeds together
      #then it is splitting those into separate strings based on whitespace
      #finally it is converting the list into a vector
      #so that the get_timeline() function works
      user_input <- unlist(str_split(paste(input$nyc_electeds, input$nysa_electeds), "[:space:]"))
      
      #if nycc and nys sen selected 
    } else if (length(input$nyc_electeds) > 0 & length(input$nysa_electeds) == 0 & length(input$nyss_electeds) > 0) {
      user_input <- unlist(str_split(paste(input$nyc_electeds, input$nyss_electeds), "[:space:]"))
      
      #if nysa and nyss selected 
    } else if (length(input$nyc_electeds) == 0 & length(input$nysa_electeds) > 0 & length(input$nyss_electeds) > 0) {
      user_input <- unlist(str_split(paste(input$nysa_electeds, input$nyss_electeds), "[:space:]"))
      
      #if all three, nycc, nysa and nyssen selected 
    } else if (length(input$nyc_electeds) > 0 & length(input$nysa_electeds) > 0 & length(input$nyss_electeds) > 0) {
      user_input <- unlist(str_split(paste(input$nyc_electeds, input$nysa_electeds, input$nyss_electeds), "[:space:]"))
      
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
                                                    buttons = list(
                                                      list(extend = 'collection',
                                                           buttons = c('csv', 'excel'), #files can be downloaded as a csv or excel
                                                           text = "Download All Results",
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
                                           Sys.sleep(0.1)
                                         }
                                         
                                       })
                                      } else {
                                      }
                                       
                                       #returning the dt 
                                       rv()
                                     })
  



##----INSTRUCTIONS MODALS----------------------------------------------------
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
      br(),
      div('To search for tweets containing at least one of multiple possible terms, 
          separate each search term with spaces and "OR" (in caps). For example, the search q = "data science" 
          looks for tweets containing both "data" and "science" located anywhere in the tweets and in any order.'),
      br(),
      div('When "OR" is entered between search terms, query = "data OR science", 
      Twitter should return any tweet that contains either "data" or "science."'),
      br(),
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
