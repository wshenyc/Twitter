library(shiny)
library(rtweet)
library(tidyverse)
library(DT)
library(glue)
library(lubridate)
library(shinydashboard)
library(dashboardthemes)
library(shinyalert)
library(tidytext)
# library(syuzhet)


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
               hashtags,
               verified,
               status_url) 
  df <- mutate(df, is_quote = ifelse(is_quote == "TRUE",
                                     "Yes",
                                     "No"),
               is_retweet = ifelse(is_retweet == "TRUE",
                                   "Yes",
                                   "No"),
               verified = ifelse(verified == "TRUE",
                                 "Yes",
                                 "No"),
               created_at = as.Date(created_at), #converting to date format
               status_url = paste("<a href='", status_url, "' target = '_blank'>View Tweet</a>", sep = ""),
               hashtags = sapply(hashtags, toString))  
  
  df <-  rename(df, "Tweet Date" = created_at,
                "Twitter User" = screen_name,
                "Tweet" = text,
                "Quote Tweet?" = is_quote,
                "Retweet?" = is_retweet,
                "Like Count" = favorite_count,
                "Retweet Count" = retweet_count,
                "Hashtags Used" = hashtags,
                "Verified?" = verified,
                "Link" = status_url
  )
  
  
  
}

##----LOADING BAR------------------------------------------------------------
#
# Description: sets up tweet table & creates a loading bar
#____________________________________________________________________________

loading_bar <- function(is_loading) {
  
  if (is_loading == TRUE) {
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
        
        # Pause for 0.1 seconds to simulate a long computation.
        Sys.sleep(0.1)
      }
    })
  }
  
}

##----TWEET TABLE------------------------------------------------------------
#
# Description: sets up tweet table & creates a loading bar
#____________________________________________________________________________


#saves the dfs generated from above to the tweet_table var on the UI

tweet_table_gen <- function(df) { 
  
  
  if(!"Message" %in% colnames(df)) {
    
    #creating empty df
    dat <- data.frame(x = numeric(0), y = numeric(0))
    
    DT::renderDT(
      
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
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.1)
        }
        
        datatable(df,
                  extensions = 'Buttons',
                  escape = 10,
                  options = list(
                  #columnDefs = list(list(targets = c(8), searchable = FALSE)), 
                    dom = 'Bltipr',
                                 buttons = 
                                   list(
                                     list(extend = 'collection',
                                          buttons = c('csv', 'excel'),
                                          text = 'Download Page',
                                          exportOptions = list(
                                            modifiers = list(page = 'current')
                                          )))),
                  filter = 'top')
      }),
      server = TRUE
    )
    
  } else {
    DT::renderDT(df,
                 options = list(
                   dom = 't'
                 ))
  }
}

##----ELECTEDS SEARCH FUNCTION-----------------------------------------------
#
# Description: custom function to track multiple drop downs
#____________________________________________________________________________

electeds_search_fun <- function(user_input, inputone, inputtwo, inputthree) {
  
  #if just nyc councilmember selected
  if (length(inputone) > 0 & length(inputtwo) == 0 & length(inputthree) == 0) {
    user_input <- inputone
    
    #if just nysa selected
  } else if (length(inputone) == 0 & length(inputtwo) > 0 & length(inputthree) == 0) {
    user_input <- inputtwo
    
    #if just nys sen selected
  } else if (length(inputone) == 0 & length(inputtwo) == 0 & length(inputthree) > 0) {
    user_input <- inputthree 
    
    #if nycc and nysa selected 
  } else if (length(inputone) > 0 & length(inputtwo) > 0 & length(inputthree) == 0) {
    #this line of code is pasting the selected nys and nyc electeds together
    #then it is splitting those into separate strings based on whitespace
    #finally it is converting the list into a vector
    #so that the get_timeline() function works
    user_input <- unlist(str_split(paste(inputone, inputtwo), "[:space:]"))
    
    #if nycc and nys sen selected 
  } else if (length(inputone) > 0 & length(inputtwo) == 0 & length(inputthree) > 0) {
    user_input <- unlist(str_split(paste(inputone, inputthree), "[:space:]"))
    
    #if nysa and nyss selected 
  } else if (length(inputone) == 0 & length(inputtwo) > 0 & length(inputthree) > 0) {
    user_input <- unlist(str_split(paste(inputtwo, inputthree), "[:space:]"))
    
    #if all three, nycc, nysa and nyssen selected 
  } else if (length(inputone) > 0 & length(inputtwo) > 0 & length(inputthree) > 0) {
    user_input <- unlist(str_split(paste(inputone, inputtwo, inputthree), "[:space:]"))
    
  } else {
    user_input
  }
  
}

##----FILTER FUNCTION---------------------------------------------------------
#
# Description: custom function to filter user search results
#____________________________________________________________________________

filter_func <- function(df, filter_flag, filter_input, user_input, num_tweets) {
  
  # checking if filter input has multiple strings
  if (filter_flag == TRUE & str_detect(filter_input, "[:space:]")) {
    filter_input <- unlist(str_split(filter_input, "[:space:]"))
    df <- get_timeline(user_input, n = num_tweets, token = bearer_token())
    if (nrow(df) > 0) {
      df <- df %>%
        filter(grepl(paste(filter_input, collapse = "|"), text, ignore.case = TRUE)) %>%
        tweet_cleaner()
    } else {
      df <- data.frame(Message = c("Your search returned no results."))
    }
    # checking if filter exists and is not multiple strings
  } else if (filter_flag == TRUE & !str_detect(filter_input, "[:space:]")) {
    df <- get_timeline(user_input, n = num_tweets, token = bearer_token())
    if (nrow(df) >0) {
      df <- df %>%
        filter(grepl(filter_input, text, ignore.case = TRUE)) %>%
        tweet_cleaner()
    } else {
      df <- data.frame(Message = c("Your search returned no results."))
    }
    #if no filter, just do standard search
  } else {
    df <- get_timeline(user_input, n = num_tweets, token = bearer_token())
    if (nrow(df) > 1) {
      df <- df %>%
        tweet_cleaner()
    }else {
      df <- data.frame(Message = c("Your search returned no results."))
    }
  }
} 


##----MOST LIKED FUNCTION------------------------------------------------
#
# Description: Find most liked original tweet 
#____________________________________________________________________________

#most liked tweet
most_liked_func <- function(df) {
  
  if (!"Message" %in% colnames(df)) {
    df <- df %>% 
      janitor::clean_names() %>% 
      filter(retweet == "No") %>%
      filter(like_count > 0)
    if (nrow(df) > 0) {
      df <- df %>% 
        filter(like_count == max(like_count)) %>%
        distinct(tweet, .keep_all = T) %>%
        select(twitter_user, tweet, like_count, link) %>%
        rename("User" = twitter_user,
               "Tweet" = tweet,
               "Like Count" = like_count,
               "Link" = link)
    } else {
      df <- data.frame(Message = c("No original tweets with more than 0 likes"))
    }
  } else {
    df <- data.frame(Message = c("No tweets returned"))
  }
  
  if(!"Message" %in% colnames(df)) {
    DT::renderDT(df, 
                 escape = 4, 
                 options = list(
                   dom = 't'
                 ))
  }else {
    DT::renderDT(df,
                 options = list(
                   dom = 't'
                 ))
  }
}


##----MOST RETWEETED FUNCTION------------------------------------------------
#
# Description: Find most retweeted original tweet 
#____________________________________________________________________________

#most retweeted tweet
most_rt_func <- function(df) {
  
  #checking if it's the blank df
  if (!"Message" %in% colnames(df)) {
    df <- df %>% 
      janitor::clean_names() %>% 
      filter(retweet == "No") %>%
      filter(retweet_count > 0)
    
    if (nrow(df) != 0) {
      df <- df %>%
        filter(retweet_count == max(retweet_count)) %>%
        distinct(tweet, .keep_all = T) %>%
        select(twitter_user, tweet, retweet_count, link) %>%
        rename("User" = twitter_user,
               "Tweet" = tweet,
               "RT Count" = retweet_count,
               "Link" = link)
    } else {
      df <- data.frame(Message = c("No original tweets with more than 0 RT's"))
    }
  } else {
    df <- data.frame(Message = c("No tweets returned"))
  }
  
  if(!"Message" %in% colnames(df)) {
    DT::renderDT(df, 
                 escape = 4, 
                 options = list(
                   dom = 't'
                 ))
  }else {
    DT::renderDT(df,
                 options = list(
                   dom = 't'
                 ))
  }
}

##----MOST ENGAGEMENT FUNCTION-----------------------------------------------
#
# Description: Find users with highest level of engagement.
# Engagement score is calculated as: Retweets*2 + Likes
#____________________________________________________________________________

most_eng_users <- function(df) {
  
  #creating temp df
  temp_df <- data.frame()
  
  if (!"Message" %in% colnames(df)) {
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
          mutate(twitter_user = paste("<a href='https://twitter.com/", twitter_user, "' target = '_blank'>",twitter_user,"</a>", sep = "")) %>%  
          head(10) %>%
          rename("User" = twitter_user,
                 "Engagement" = eng_score)
        
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
          mutate(twitter_user = paste("<a href='https://twitter.com/", twitter_user, "' target = '_blank'>",twitter_user,"</a>", sep = "")) %>%
          rename("User" = twitter_user,
                 "Engagement" = eng_score)
        
        #case 2b: if engagement score total is zero, inform user 
      } else {
        temp_df <- data.frame(Message = c("Search returned no users with engagement scores higher than zero."))
      }
      
      #case 3: if resulting df has no rows, inform user all returned tweets were RTs
    } else {
      temp_df <- data.frame(Message = c("Search results returned no original tweets"))
    }
  } else {
    temp_df <- data.frame(Message = c("No tweets returned"))
  }
  
  #finally render results to UI 
  
  if(!"Message" %in% colnames(df)) {
    DT::renderDT(temp_df, 
                 escape = 1, 
                 options = list(
                   dom = 't'
                 ))
  }else {
    DT::renderDT(temp_df,
                 options = list(
                   dom = 't'
                 ))
  }
  
}


##----TWEET FREQ CHART FUNCTION----------------------------------------------
#
# Description: Find users with highest level of engagement.
# Engagement score is calculated as: Retweets*2 + Likes
#____________________________________________________________________________

tweet_freq <- function(df) {
  
  if(!"Message" %in% colnames(df)) {
    df <- df %>% 
      janitor::clean_names() %>% 
      group_by(tweet_date) %>% 
      mutate(count = n())
    
    plot<- ggplot(df, aes(x = tweet_date, y = count)) +
      theme_classic() +
      geom_line(color = "#1DA1F2", size = 1) +
      geom_point(color = "#1DA1F2", size = 3) +
      #scale_x_date(date_labels = "%b %d",date_breaks = "1 day") +
      ylab("") +
      xlab("") +
      theme(axis.title = element_text(size = 14),
            axis.text = element_text(size = 14))
    
  }
  else {
    
    plot <- ggplot() + theme_void()
  }
  renderPlot(plot)
  
}

# ##----SENTIMENT ANALYSIS----------------------------------------------
# #
# # Description: Find users with highest level of engagement.
# # Engagement score is calculated as: Retweets*2 + Likes
# #____________________________________________________________________________
# sent_analysis <- function(text) {
# 
# df <- data.frame()
# df <- data.frame(text = c(text))
#   
# ## clean up the text a bit (rm mentions and links)
# df$text <- gsub(
#   "^RT:?\\s{0,}|#|@\\S+|https?[[:graph:]]+", "", text)
# ## convert to lower case
# df$text <- tolower(df$text)
# ## trim extra white space
# df$text <- gsub("^\\s{1,}|\\s{1,}$", "", df$text)
# df$text <- gsub("\\s{2,}", " ", df$text)
# 
# ## estimate pos/neg sentiment for each tweet
# df$sentiment <- syuzhet::get_sentiment(df$text, "syuzhet")
# 
# ## write function to round time into rounded var
# round_time <- function(x, sec) {
#   as.POSIXct(hms::hms(as.numeric(x) %/% sec * sec))
# }
# 
# ## plot by specified time interval (1-hours)
# df <- df %>%
#   mutate(time = round_time(created_at, 60 * 60)) %>%
#   group_by(time) %>%
#   summarise(sentiment = mean(sentiment, na.rm = TRUE)) %>%
#   mutate(valence = ifelse(sentiment > 0L, "Positive", "Negative")) %>%
#   ggplot(aes(x = time, y = sentiment)) +
#   geom_smooth(method = "loess", span = .1,
#               colour = "#aa11aadd", fill = "#bbbbbb11") +
#   geom_point(aes(fill = valence, colour = valence), 
#              shape = 21, alpha = .6, size = 3.5) +
#   theme_minimal(base_size = 15, base_family = "Roboto Condensed") +
#   theme(legend.position = "none",
#         axis.text = element_text(colour = "#222222"),
#         plot.title = element_text(size = rel(1.7), face = "bold"),
#         plot.subtitle = element_text(size = rel(1.3)),
#         plot.caption = element_text(colour = "#444444")) +
#   scale_fill_manual(
#     values = c(Positive = "#2244ee", Negative = "#dd2222")) +
#   scale_colour_manual(
#     values = c(Positive = "#001155", Negative = "#550000")) +
#   labs(x = NULL, y = NULL,
#        title = "Sentiment (valence) of rstudio::conf tweets over time",
#        subtitle = "Mean sentiment of tweets aggregated in one-hour intervals",
#        caption = "\nSource: Data gathered using rtweet. Sentiment analysis done using syuzhet")
# 
# renderPlot(df)
# 
# }

##----VALIDATION FUNCTION----------------------------------------------------
#
# Description: Validate user input to not be empty & have 1 alpha
#____________________________________________________________________________

validateRequiredInput <- function(inputData, inputName) {
  if(is.null(inputData) || is.na(inputData) || inputData == "") {
    
    #Create error pop up
    shinyalert(
      title = "Error",
      text = paste("Please provide a value for", inputName),
      type = "error"
    )
    
    return(FALSE)
  } else if (!str_detect(inputData, "[:alpha:]")) {
    
    #Create error pop up
    shinyalert(
      title = "Error",
      text = paste("Query for", inputName, "must contain at least one letter"),
      type = "error"
    )
    
    return(FALSE)
    
  }
  
  shinyalert(
    title = "Success",
    text = paste("Successful search! Please wait up to 1 min if a large number of tweets were requested."),
    type = "success"
  )
  
  return(TRUE)
}

##----VALIDATION ELECTEDS FUNCTION-------------------------------------------
#
# Description: Validate electeds input
#____________________________________________________________________________

validateElectedInput <- function(inputone, inputtwo, inputthree) {
  if((length(inputone) == 0 & length(inputtwo) == 0 & length(inputthree) == 0)) {
    shinyalert(
      title = "Error",
      text = paste("Please select at least one elected from the dropdown menus"),
      type = "error"
    )
    
    return(FALSE)
  }
  
  shinyalert(
    title = "Success",
    text = paste("Successful search! Please wait up to 1 min if a large number of tweets were requested."),
    type = "success"
  )
  
  return(TRUE)
}

##----VALIDATION FILTER FUNCTION-------------------------------------------
#
# Description: Validate electeds input
#____________________________________________________________________________

validateFilterInput <- function(filter, inputData, inputName) {
  if (filter & !str_detect(inputData, "[:alpha:]")) {
    
    #Create error pop up
    shinyalert(
      title = "Error",
      text = paste("Query for", inputName, "must contain at least one letter"),
      type = "error"
    )
    
    return(FALSE)
    
  }
  
  return(TRUE)
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
  ,sidebarTabBorderColor = "rgb(209, 209, 209, 1)" ###border tab color 
  ,sidebarTabBorderWidth = 1
  
  ,sidebarTabBackColorSelected = "rgb(209, 209, 209, 1)" #this changes the color of the tab after it's been clicked
  ,sidebarTabTextColorSelected = "rgb(0,0,0)"
  ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
  
  ,sidebarTabBackColorHover = "rgb(209, 209, 209, 1)" #changes color of tab on hover
  ,sidebarTabTextColorHover = "rgb(50,50,50)"
  ,sidebarTabBorderStyleHover = "none none solid none"
  ,sidebarTabBorderColorHover = "rgb(209, 209, 209, 1)"
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

nyscong_electeds <- electeds %>% 
  filter(Type == "nyccongress")

nyc_electeds_exec <- electeds %>% 
  filter(Level == "City" & Type == "nycexecutive")

nys_electeds_exec <- electeds %>% 
  filter(Level == "State" & Type == "nycexecutive")

fed_electeds_exec <- electeds %>% 
  filter(Level == "Federal" & Type == "nycexecutive")

nyc_agencies <- electeds %>% 
  filter(Level == "City" & Type == "agency")

fed_agencies <- electeds %>% 
  filter(Level == "Federal" & Type == "agency")


choices_nyc_electeds <- setNames(nyc_electeds$Handle, nyc_electeds$Elected)

choices_nysa_electeds <- setNames(nysa_electeds$Handle, nysa_electeds$Elected)

choices_nyss_electeds <- setNames(nyssen_electeds$Handle, nyssen_electeds$Elected)

choices_fed_electeds <- setNames(nyscong_electeds$Handle, nyscong_electeds$Elected)

choices_nyc_exec_electeds <- setNames(nyc_electeds_exec$Handle, nyc_electeds_exec$Elected)

choices_nys_exec_electeds <- setNames(nys_electeds_exec$Handle, nys_electeds_exec$Elected)

choices_fed_exec_electeds <- setNames(fed_electeds_exec$Handle, fed_electeds_exec$Elected)

choices_nyc_agencies <- setNames(nyc_agencies$Handle, nyc_agencies$Elected)

choices_fed_agencies <- setNames(fed_agencies$Handle, fed_agencies$Elected)

##----COMMUNITY GROUP TWITTERS-----------------------------------------------

# Description: Downloads electeds' Twitter handle data
#____________________________________________________________________________

cbo_twitter <- readxl::read_excel("R:/POLICY-STRATEGY-HOUSING POLICY/Data Projects/Twitter Project for IGA/advocacy groups.xlsx") %>% 
  arrange(Name)

local_groups <- cbo_twitter %>% 
  filter(Level == "local") 

national_groups <- cbo_twitter %>% 
  filter(Level == "national")

reporters <- cbo_twitter %>% 
  filter(Level == "reporter")

choices_local_groups <- setNames(local_groups$Handle, local_groups$Name)

choices_national_groups <- setNames(national_groups$Handle, national_groups$Name)

choices_reporters <- setNames(reporters$Handle, reporters$Name)


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
      sidebar =  dashboardSidebar( 
        sidebarMenu(
          id = "tabs",
          
          menuItem("Keyword Search",
                   tabName = "keyword_menu_item",
                   icon = icon("comment-dots"),
                   menuSubItem(icon = NULL,
                               numericInput("keywords_num_tweets",
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
                               numericInput("user_num_tweets",
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
          
          
          ##----electeds menu items------------------------------------------------------
          
          menuItem("City Gov Search",
                   tabName = "city_gov_menu_item",
                   icon = icon("city"),
                   menuSubItem(icon = NULL,
                               numericInput("city_gov_num_tweets",
                                            "Number of Tweets to Download (Per Account):",
                                            min = 100,
                                            max = 18000,
                                            value = 200,
                                            step = 100)),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "nyc_electeds",
                                 "NYC Councilmembers", 
                                 choices = choices_nyc_electeds,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T)),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "nyc_electeds_exec", ###nyc executives 
                                 "NYC Citywide", 
                                 choices = choices_nyc_exec_electeds,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T)),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "nyc_agencies", 
                                 "NYC Agencies", 
                                 choices = choices_nyc_agencies,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T)),
                   
                   menuSubItem(icon = NULL,
                               textInput('filter_nyc_electeds',
                                         'Filter:',
                                         placeholder = "Enter keywords")),
                   
                   menuSubItem(icon = NULL,
                               actionButton("nyc_electeds_search", "City Gov Search")),
                   menuSubItem(icon = icon("info"),
                               actionButton("show_nyc_electeds", "View instructions"))),
          
          
          
          menuItem("State Gov Search",
                   tabName = "state_gov_menu_item",
                   icon = icon("landmark"),
                   menuSubItem(icon = NULL,
                               numericInput("state_gov_num_tweets",
                                            "Number of Tweets to Download (Per Account)",
                                            min = 100,
                                            max = 18000,
                                            value = 200,
                                            step = 100)),
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
                               shinyWidgets::pickerInput(
                                 "nys_electeds_exec", #nys executives 
                                 "NYS Executives", 
                                 choices= choices_nys_exec_electeds,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   menuSubItem(icon = NULL,
                               textInput('filter_nys_electeds',
                                         'Filter:',
                                         placeholder = "Enter keywords")),
                   
                   menuSubItem(icon = NULL,
                               actionButton("nys_electeds_search", "State Gov Search")),
                   menuSubItem(icon = icon("info"),
                               actionButton("show_nys_electeds", "View instructions"))),
          
          menuItem("Fed Gov Search",
                   tabName = "fed_gov_menu_item",
                   icon = icon("monument"),
                   menuSubItem(icon = NULL,
                               numericInput("fed_gov_num_tweets",
                                            "Number of Tweets to Download (Per Account)",
                                            min = 100,
                                            max = 18000,
                                            value = 200,
                                            step = 100)),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "nyscong_electeds",
                                 "Congressmembers",
                                 choices= choices_fed_electeds,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "fed_electeds_exec",
                                 "Fed Executives",
                                 choices= choices_fed_exec_electeds,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "fed_agencies",
                                 "Fed Agencies",
                                 choices= choices_fed_agencies,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   menuSubItem(icon = NULL,
                               textInput('filter_fed_electeds',
                                         'Filter:',
                                         placeholder = "Enter keywords")),
                   
                   menuSubItem(icon = NULL,
                               actionButton("fed_electeds_search", "Fed Gov Search")),
                   menuSubItem(icon = icon("info"),
                               actionButton("show_fed_electeds", "View instructions"))),
          
          menuItem("Advocacy Groups",
                   tabName = "advocacy_menu_item",
                   icon = icon("monument"),
                   menuSubItem(icon = NULL,
                               numericInput("advocacy_num_tweets",
                                            "Number of Tweets to Download (Per Account)",
                                            min = 100,
                                            max = 18000,
                                            value = 200,
                                            step = 100)),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "local_groups",
                                 "Local Groups",
                                 choices= choices_local_groups,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "national_groups",
                                 "National Groups",
                                 choices= choices_national_groups,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   
                   menuSubItem(icon = NULL,
                               shinyWidgets::pickerInput(
                                 "reporters",
                                 "Reporters",
                                 choices= choices_reporters,
                                 options = list(`actions-box` = TRUE,
                                                `live-search` = TRUE,
                                                `dropup-auto` = F,
                                                `live-search-placeholder` = "Search name",
                                                `size` = 10),
                                 multiple = T,
                               )),
                   
                   menuSubItem(icon = NULL,
                               textInput('filter_advocacy_groups',
                                         'Filter:',
                                         placeholder = "Enter keywords")),
                   
                   menuSubItem(icon = NULL,
                               actionButton("advocacy_search", "Advocacy Search")),
                   menuSubItem(icon = icon("info"),
                               actionButton("show_advocacy_groups", "View instructions")))
          
        ) #sidebar menu closer
      ), #dashboardsidebar coser 
      
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
                                    DTOutput("tweet_table"), width = "100%")
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
                                           DTOutput("pop_users")),
                                       "Weighted average of RT (2x) and favorites (1x) per tweet"
                                     ))
                 ))
          ,
          
          
          column(5,
                 fluidRow(
                   shinyjs::hidden(div(id ="liked_wrapper",
                                       shinydashboard::box(
                                         width = NULL,
                                         title = "Most Liked Tweet",
                                         status = "primary",
                                         div(style = "overflow-x:scroll",
                                             DTOutput("most_liked"))
                                       )))
                 ), #row
                 
                 fluidRow(
                   shinyjs::hidden(div(id = "rt_wrapper",
                                       shinydashboard::box(
                                         width = NULL,
                                         title = "Most RT'd Tweet",
                                         status = "primary",
                                         div(style = "overflow-x:scroll",
                                             DTOutput("most_rt"), width ="100%"))
                   )))
          ), #column closer 
          
          
          column(4,
                 shinyjs::hidden(div(id = "tweet_freq_wrapper",
                                     shinydashboard::box(
                                       width = NULL,
                                       title = "Tweet Frequency",
                                       status = "primary",
                                       div(plotOutput("tweet_freq_chart"), width = "100%")
                                     ))))
        ) #2nd fluid row closer 
        , fluidRow(
          shinyjs::hidden(div(id = "sent_analysis_wrapper",
                              shinydashboard::box(
                                width = 5,
                                title = "Sentiment Analysis",
                                status = "primary",
                                div(plotOutput("sent_analysis_chart"), width = "100%")
                              )))
        )
        
      ) #dashboard body
    ), #dashboard page 
    
    
    # tags$script('Shiny.addCustomMessageHandler("message", function(message) {
    #             alert(message); 
    # });'),
    
    tags$footer(img(src="https://housingconnect.nyc.gov/PublicWeb/assets/images/hpd-logo@2x.png", height = "55px"), 
                img(src="https://raw.githubusercontent.com/wshenyc/Twitter/main/fedex%20day.png", height = "55px"),
                align = "right", style = "
              position:relative;
              bottom:0;
              width:100%;
              height:65px;   /* Height of the footer */
              color: white; #font color
              padding-right: 5px;
              padding-left: 5px;
              padding-top: 5px;
              background-color: #333;
              #z-index: 1000;#this will move the footer to be on top of the sidebar
            ") 
    
  ) #tag list, there should only be one parenthesis here 



##----SERVER LOGIC-----------------------------------------------------------
#
# Description: sets up server logic
#____________________________________________________________________________

server <- function(input, output, session) {
  
  # the modal dialog upon start up
  
shinyalert(
    title = "Introduction",
    text = 
      '<div align = "left">Twitter Search is an app that allows users to:</div>
        </br>
        <div align = "left"><ul>
              <li>Search most recent Tweets containing specified key words from the last 6-9 days.</li>
              <li>Return timelines for specified Twitter users</li>
              <li>Filter timeline results by specified key words</li>
            <ul></div>
        </br>
        <div align = "left">To start, select one of the tabs on the left.</div>', 
    html = TRUE,
    type = "info",
    closeOnClickOutside = TRUE
  )
  

  
  
  ##----KEYWORD SEARCH---------------------------------------------------------
  #
  # Description: sets up keyword search
  #____________________________________________________________________________
  
  observeEvent(input$keyword_search_button, {
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "search_tweets") 
    
    if(lim$remaining == 0){
      shinyjs::disable("keyword_search_button")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("keyword_search_button"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    
    user_input <- input$search_terms
    
    #require user_input to not be empty and have at least one letter
    
    if(lim$remaining != 0) {
      
      validationResult <- (
        validateRequiredInput(inputData = user_input, inputName = "keywords search")
      )
      
      #check validation result
      
      if(lim$remaining != 0) {
        
        if (validationResult == TRUE) {
          
          #disable submit button after successful search 
          shinyjs::disable("keyword_search_button")  
          
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
          df <- search_tweets(user_input, n = input$keywords_num_tweets, token = bearer_token()) 
          
          shinyjs::enable("keyword_search_button")
          
          
          if(nrow(df) > 0) {
            df <- df %>% 
              tweet_cleaner() #calling custom function 
          } else {
            df <- data.frame(Message = c("Search returned no results."))
          }
          
          
          
          #users with highest engagement
          output$pop_users <- most_eng_users(df)
          
          #most liked tweet
          output$most_liked <- most_liked_func(df)
          
          #most retweeted tweet
          output$most_rt <- most_rt_func(df)
          
          #tweet freq chart
          output$tweet_freq_chart <- tweet_freq(df)
          
          #adding the results of the search_tweets to the reactive value 
          output$tweet_table <- tweet_table_gen(df)
          
          #sent analysis
          # output$sent_analysis_chart <- sent_analysis(df$`Tweet`)
          
          shinyjs::show("tweet_wrapper")
          shinyjs::show("user_wrapper")
          shinyjs::show("liked_wrapper")
          shinyjs::show("rt_wrapper")
          shinyjs::show("tweet_freq_wrapper")
          # shinyjs::show("sent_analysis_wrapper")
        } #validation result
      }
    }
  })
  
  
  ##----USER SEARCH------------------------------------------------------------
  #
  # Description: sets up user search & filter
  #____________________________________________________________________________
  
  #get timelines for specific users 
  observeEvent(input$user_search_button, {
    
    user_input <- input$user_search
    num_tweets <- input$user_num_tweets
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "get_timeline") 
    
    if (nrow(lim) == 0) {
      #if(lim$remaining == 0){
      shinyjs::disable("user_search_button")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("user_search_button"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    
    #validate search results 
    validationResult <- (
      validateRequiredInput(inputData = user_input, inputName = "users search")
    )
    
    if (validationResult == TRUE) {
      
      #disable submit button after successful search 
      shinyjs::disable("user_search_button")
      
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
      
      #testing filter validlity  
      validationInputResult <- (
        validateFilterInput(filter = filter_flag, inputData = filter_input, inputName = "filter")
      )
      
      if (validationInputResult == TRUE) {
        
        #filter function
        df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
        
        #enable submit button after table generates 
        shinyjs::enable("user_search_button")
        
        #users with highest engagement
        output$pop_users <- most_eng_users(df)
        
        #most liked tweet
        output$most_liked <- most_liked_func(df)
        
        #most retweeted tweet
        output$most_rt <- most_rt_func(df)
        
        #tweet freq chart
        output$tweet_freq_chart <- tweet_freq(df)
        
        #adding the results of the search_tweets to the reactive value 
        output$tweet_table <- tweet_table_gen(df)
        
        shinyjs::show("tweet_wrapper")
        shinyjs::show("user_wrapper")
        shinyjs::show("liked_wrapper")
        shinyjs::show("rt_wrapper")
        shinyjs::show("tweet_freq_wrapper")
        
      } #validation result bracket
    } #other validation result bracket
  })
  
  ##----CITY ELECTEDS SEARCH----------------------------------------------------
  #
  # Description: sets up electeds search & filter
  #____________________________________________________________________________
  
  
  #get timeline search for electeds
  observeEvent(input$nyc_electeds_search, {
    
    num_tweets <- input$city_gov_num_tweets
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "get_timeline") 
    
    if(lim$remaining == 0){
      shinyjs::disable("nyc_electeds_search")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("nyc_electeds_search"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    
    #if no inputs selected
    validationResult <- (
      validateElectedInput(input$nyc_electeds, input$nyc_electeds_exec, input$nyc_agencies)
    )
    
    
    #check validation result
    
    if (validationResult == TRUE) {
      
      #enable submit button after table generates 
      shinyjs::disable("nyc_electeds_search")
      
      user_input <- electeds_search_fun(user_input, input$nyc_electeds, input$nyc_electeds_exec, input$nyc_agencies)    
      
      # testing if there is a filter
      if (input$filter_nyc_electeds != "") {
        filter_flag <- TRUE
        filter_input <- input$filter_nyc_electeds
      } else {
      }
      
      #validating filter 
      validationInputResult <- (
        validateFilterInput(filter = filter_flag, inputData = filter_input, inputName = "filter")
      )
      
      if (validationInputResult == TRUE) {
        
        #filter function
        df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
        
        #enable submit button after table generates 
        shinyjs::enable("nyc_electeds_search")
        
        #users with highest engagement
        output$pop_users <- most_eng_users(df)
        
        #most liked tweet
        output$most_liked <- most_liked_func(df)
        
        #most retweeted tweet
        output$most_rt <- most_rt_func(df)
        
        #tweet freq chart
        output$tweet_freq_chart <- tweet_freq(df)
        
        #adding the results of the search_tweets to the reactive value
        output$tweet_table <- tweet_table_gen(df)
        
        shinyjs::show("tweet_wrapper")
        shinyjs::show("user_wrapper")
        shinyjs::show("liked_wrapper")
        shinyjs::show("rt_wrapper")
        shinyjs::show("tweet_freq_wrapper")
        
      }#validation result bracket 
    }#other validation result bracket 
  })
  
  ##----STATE ELECTEDS SEARCH----------------------------------------------------
  #
  # Description: sets up electeds search & filter
  #____________________________________________________________________________
  
  
  #get timeline search for electeds
  observeEvent(input$nys_electeds_search, {
    
    num_tweets <- input$state_gov_num_tweets
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "get_timeline") 
    
    print(lim)
    print(lim$remaining)
    
    if(lim$remaining == 0){
      shinyjs::disable("nys_electeds_search")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("nys_electeds_search"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    #if no inputs selected
    validationResult <- (
      validateElectedInput(input$nysa_electeds, input$nyss_electeds, input$nys_electeds_exec)
    )
    
    
    #check validation result
    
    if (validationResult == TRUE) {
      
      #disable submit button after table generates 
      shinyjs::disable("nys_electeds_search")
      
      user_input <- electeds_search_fun(user_input, input$nysa_electeds, input$nyss_electeds, input$nys_electeds_exec)   
      
      # testing if there is a filter
      if (input$filter_nys_electeds != "") {
        filter_flag <- TRUE
        filter_input <- input$filter_nys_electeds
      } else {
      }
      
      #validating filter 
      validationInputResult <- (
        validateFilterInput(filter = filter_flag, inputData = filter_input, inputName = "filter")
      )
      
      if (validationInputResult == TRUE) {
        
        #filter function
        df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
        
        #enable submit button after table generates 
        shinyjs::enable("nys_electeds_search")
        
        #users with highest engagement
        output$pop_users <- most_eng_users(df)
        
        #most liked tweet
        output$most_liked <- most_liked_func(df)
        
        #most retweeted tweet
        output$most_rt <- most_rt_func(df)
        
        #tweet freq chart
        output$tweet_freq_chart <- tweet_freq(df)
        
        #adding the results of the search_tweets to the reactive value
        output$tweet_table <- tweet_table_gen(df)
        
        shinyjs::show("tweet_wrapper")
        shinyjs::show("user_wrapper")
        shinyjs::show("liked_wrapper")
        shinyjs::show("rt_wrapper")
        shinyjs::show("tweet_freq_wrapper")
        
      }#validation result bracket 
    }#other validation result bracket 
  })
  
  
  ##----FED ELECTEDS SEARCH----------------------------------------------------
  #
  # Description: sets up electeds search & filter
  #____________________________________________________________________________
  
  
  #get timeline search for electeds
  observeEvent(input$fed_electeds_search, {
    
    num_tweets <- input$fed_gov_num_tweets
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "get_timeline") 
    
    if(lim$remaining == 0){
      shinyjs::disable("fed_electeds_search")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("fed_electeds_search"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    
    #if no inputs selected
    validationResult <- (
      validateElectedInput(input$nyscong_electeds, input$fed_electeds_exec, input$fed_agencies)
    )
    
    
    #check validation result
    
    if (validationResult == TRUE) {
      
      #enable submit button after table generates 
      shinyjs::disable("fed_electeds_search")
      
      user_input <- electeds_search_fun(user_input, input$nyscong_electeds, input$fed_electeds_exec, input$fed_agencies)   
      
      # testing if there is a filter
      if (input$filter_fed_electeds != "") {
        filter_flag <- TRUE
        filter_input <- input$filter_fed_electeds
      } else {
      }
      
      #validating filter 
      validationInputResult <- (
        validateFilterInput(filter = filter_flag, inputData = filter_input, inputName = "filter")
      )
      
      if (validationInputResult == TRUE) {
        
        #filter function
        df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
        
        #enable submit button after table generates 
        shinyjs::enable("fed_electeds_search")
        
        #users with highest engagement
        output$pop_users <- most_eng_users(df)
        
        #most liked tweet
        output$most_liked <- most_liked_func(df)
        
        #most retweeted tweet
        output$most_rt <- most_rt_func(df)
        
        #tweet freq chart
        output$tweet_freq_chart <- tweet_freq(df)
        
        #adding the results of the search_tweets to the reactive value
        output$tweet_table <- tweet_table_gen(df)
        
        shinyjs::show("tweet_wrapper")
        shinyjs::show("user_wrapper")
        shinyjs::show("liked_wrapper")
        shinyjs::show("rt_wrapper")
        shinyjs::show("tweet_freq_wrapper")
        
      }#validation result bracket 
    }#other validation result bracket 
  })
  
  ##----ADVOCACY GROUP SEARCH---------------------------------------------------
  #
  # Description: sets up advocacy group search & filter
  #____________________________________________________________________________
  
  
  #get timeline search for electeds
  observeEvent(input$advocacy_search, {
    
    num_tweets <- input$advocacy_num_tweets
    
    #setting blank variables
    user_input <- ""
    filter_input <- ""
    filter_flag <- FALSE
    df <- data.frame()
    
    #checking rate limit 
    token <- bearer_token()
    lim <- rate_limit(token, "get_timeline") 
    
    if(lim$remaining == 1){
      shinyjs::disable("advocacy_search")
      shinyjs::delay(difftime(Sys.time(), lim$reset_at, units = "secs") * 1000, shinyjs::enable("advocacy_search"))
      time <- difftime(Sys.time(), lim$reset_at, units = "mins")
      time <- paste(abs(ceiling(time)),"mins")
      showModal(
        modalDialog(
          title = "Rate limit hit!",
          "You have hit the rate limit, wait for",
          time 
          , "to make another search.",
          easyClose = TRUE,
          footer = NULL
        )
      )
    } 
    
    
    #if no inputs selected
    validationResult <- (
      validateElectedInput(input$local_groups, input$national_groups, input$reporters)
    )
    
    
    #check validation result
    
    if (validationResult == TRUE) {
      
      #enable submit button after table generates 
      shinyjs::disable("advocacy_search")
      
      user_input <- electeds_search_fun(user_input, input$national_groups, input$local_groups, input$reporters)   
      
      # testing if there is a filter
      if (input$filter_advocacy_groups != "") {
        filter_flag <- TRUE
        filter_input <- input$filter_advocacy_groups
      } else {
      }
      
      #validating filter 
      validationInputResult <- (
        validateFilterInput(filter = filter_flag, inputData = filter_input, inputName = "filter")
      )
      
      if (validationInputResult == TRUE) {
        
        #filter function
        df <- filter_func(df, filter_flag, filter_input, user_input, num_tweets)
        
        #enable submit button after table generates 
        shinyjs::enable("advocacy_search")
        
        #users with highest engagement
        output$pop_users <- most_eng_users(df)
        
        #most liked tweet
        output$most_liked <- most_liked_func(df)
        
        #most retweeted tweet
        output$most_rt <- most_rt_func(df)
        
        #tweet freq chart
        output$tweet_freq_chart <- tweet_freq(df)
        
        #adding the results of the search_tweets to the reactive value
        output$tweet_table <- tweet_table_gen(df)
        
        shinyjs::show("tweet_wrapper")
        shinyjs::show("user_wrapper")
        shinyjs::show("liked_wrapper")
        shinyjs::show("rt_wrapper")
        shinyjs::show("tweet_freq_wrapper")
        
      }#validation result bracket 
    }#other validation result bracket 
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
      div('If searching for specific hashtags, type a # before the term. Ex: #goodcause.'),
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
      div(tags$b("Number of Tweets to Download (Per Account)")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Users Search")), 
      div("Enter spaces between each screen name if searching for more than one user at once. 
      The number of tweets to download will return that number of tweets per user. 
      For example, if searching NYCMayor and NYCHousing and selecting 100 tweets to download will yield 200 tweets."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to results that include any of the keywords and in
          any order."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$show_nyc_electeds, {
    showModal(modalDialog(
      title = "Electeds Search Instructions",
      div(tags$b("Number of Tweets to Download (Per Account)")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Electeds Search")), 
      div("Select or search for any elected official's name. Selection boxes can be used together or separately."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to results that include any of the keywords and in
          any order."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$show_nys_electeds, {
    showModal(modalDialog(
      title = "Electeds Search Instructions",
      div(tags$b("Number of Tweets to Download (Per Account)")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Electeds Search")), 
      div("Select or search for any elected official's name. Selection boxes can be used together or separately."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to results that include any of the keywords and in
          any order."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
  observeEvent(input$show_fed_electeds, {
    showModal(modalDialog(
      title = "Electeds Search Instructions",
      div(tags$b("Number of Tweets to Download (Per Account)")), 
      div("Minimum number of tweets returned is 100. The maximum is 18,000. The number of tweets is incremented by 100."),
      br(),
      div(tags$b("Electeds Search")), 
      div("Select or search for any elected official's name. Selection boxes can be used together or separately."),
      br(),
      div(tags$b("Filter")), 
      div("Entering keywords here will filter the user search to results that include any of the keywords and in
          any order."),
      easyClose = TRUE,
      footer = NULL
    ))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
