#
# This is a Shiny web application for the Nexus Lab's Sentigram.
library(shiny)
library(shinythemes)
library(shinycssloaders)

source(file = "./appSource.R")

# Define UI for application that draws a histogram
ui <- navbarPage( "InTo", theme = shinytheme("united"),
                  
                  # Sidebar with a slider input for number of bins 
                  tabPanel(title = "Healthcare Pressure",
                           
                           div(tags$head(includeCSS("styles.css"))),
                           
                           absolutePanel(id = "controls", class = "panel panel-default",
                                         top = 250, left = 55, right = 0, bottom = 0, 
                                         width = 300, height = 850, fixed=TRUE, draggable = TRUE,
                                         
                                         selectInput(inputId = "city", label = "Choose a Location", 
                                                     choices = c( "New Delhi",
                                                                  "Bangkok", "Jakarta", "Mumbai"
                                                     ), 
                                                     selected = "Jakarta"),
                                         br(),
                                         actionButton("display", "Select"),
                                         br(),
                                         plotOutput("sentiMeter", height="183px", width="275px"),
                                         br(),
                                         plotOutput("hospTime", height="183px", width="275px"),
                                         br(),
                                         plotOutput("caseTime", height="183px", width="275px")
                           ),
                           
                           h6("To get started, choose your Location and Date Range in the panel on the right and
                                 then click Select."),
                           
                           google_mapOutput("krigingMap", height = 800)
                  ),
                  
                  tabPanel(title = "Tweet Analysis",
                           
                           fluidRow(
                             
                             radioButtons("tweetSet", 'Select Tweet Content',
                                          choices = c("All", "Misinformation", "Healthcare"),
                                          inline = T),
                             
                             h3("Tweet Emotions over Time"),
                             
                             plotOutput("emoTime") %>% withSpinner()
                           ),
                           fluidRow(
                             
                             column(width = 6,
                                    
                                    DT::dataTableOutput("Tweets")
                                    ),
                             column(width = 4,
                                    fluidRow(
                                      column(width = 4,dateInput(inputId = "topWord_dates", label = "Select a Date",
                                                                 value = "2020-04-24")),
                                      column(width = 4,sliderInput(inputId = "bigram_n", label = "Select number to show", 
                                                         value = 10, min = 10, max=30, step = 5)),
                                      column(width = 4, actionButton(inputId = "filter", "Filter"))
                                      ),
                                    fluidRow(plotOutput("bigrams") %>% withSpinner())
                                    )
                             
                             )
                           ),
                  
                  tabPanel(title = "Predictability",
                           
                           column(width = 5,
                                  h3(strong("Cases")),
                                  DT::dataTableOutput("case_predictors")),
                           
                           column(width = 5,
                                  h3(strong("Hospitalization")),
                                  DT::dataTableOutput("hosp_predictors"))
                           
                           ),
                  
                  # tabPanel(title = "Potential Misinformation",
                  #          
                  #          fluidRow(
                  #            h3("Tweet Emotions over Time"),
                  #            plotOutput("misinformation_emotion") %>% withSpinner()
                  #            ),
                  #          
                  #          fluidRow(
                  #            column(width = 7,
                  #                   h4("Tweets about misinformation or fake news"),
                  #                   DT::dataTableOutput("misinformingTweets") %>% withSpinner()              
                  #          ),
                  #          column(width = 4,
                  #                 # h4("Selected Misinformation"),
                  #                 # DT::dataTableOutput("misinform_selected"),
                  #                 # actionButton("email", "Send")
                  #                 h4("Top Pairs of Words from Misinforming tweets"),
                  #                 plotOutput("misinforming_bigrams") %>% withSpinner()
                  #          ))
                  #          ),
                  # 
                  # tabPanel(title = "Healthcare Satisfaction",
                  #          fluidRow(h3("Tweet Emotions over Time"),
                  #                   
                  #                   plotOutput("healthcare_emotion") %>% withSpinner()),
                  #          fluidRow(
                  #            column(width = 7,
                  #                   h4("Tweets about healthcare"),
                  #                   DT::dataTableOutput("healthcareTweets") %>% withSpinner()              
                  #            ),
                  #            column(width = 4,
                  #                   # h4("Selected Misinformation"),
                  #                   # DT::dataTableOutput("misinform_selected"),
                  #                   # actionButton("email", "Send")
                  #                   h4("Top Pairs of Words from Healthcare tweets"),
                  #                   plotOutput("healthcare_bigrams") %>% withSpinner()
                  #            ))
                  #          ),
                  
                  tabPanel(title = "About",
                           
                           h3(strong("What are you InTo?")),
                           p("The Covid 19 pandemic provides a unique opportunity to investigate the relationship between
                           social chatter and health-seeking behaviour. We here at the Nexus Lab believe that the sentiments 
                           in the words we use can help public health officials forecast the expected demand on hospital resources.
                           Imagine, analyzing the billions of tweets on Twitter and not only getting an understanding of what people
                           know or how they feel about a disease, but using that information to inform how well local hospitals 
                           need to prepare, in the days, weeks or months ahead. So we created this tool: a infodemiological tomograph 
                             called InTo."),
                           br(),
                           
                           h3(strong("Elements")),
                           h4("Positivity, Healthcare Pressure and Incidence"),
                           p("Geostatistical kriging and non-linear transfer entropy are used to estimate weekly infection cases and 
                           hospitalized. Non-linear and relationships used to perform predictions are adjusted considering weekly data.
                           The variogram is assessing the statistical dependence over space for the estimated healthcare pressure. 
                             A constant value of the semivariance determines the characteristic spatial distance at which HP looses 
                             statistical dependence and healthcare fluxes (related to HP gradients) are no more expected."),
                           br(),
                           h4("Risk Predictability and Gap Indices"),
                           p("Risk index = Percentage variation in new weekly cases considering previous week"),
                           p("Predictability index = Weekly updated Uncertainty reduction in hospitalized as a function of positivity"),
                           p("Gap index = Weekly updated departure of observations and model estimates considering average Root Mean Square Error"),
                           br(),
                           h4("Emotional States and Most Popular Word-Pair Shifts and Extremes Events Detections"),
                           p("The volume of tweets associated to certain emotions linked to top most popular word pairs is calculated. 
                           This is different than a pure emotional assessment performed by other groups in the way we determine only 
                           the emotions from the most popular Tweeted pairs. The basic model used to infer emotions is at …"),
                           
                           h3(strong("Validity")),
                           h4("Spatial Validation and HP Characteristic Scale"),
                           p("Heatzones of healthcare pressure (HP) are predicted in hospital-rich areas without imposing location of 
                           hospitals (or other healthcare infrastructure) as a constraint with in the predictions. Small-/medium-range 
                           autocorrelation of geolocated tweets and hospitals is observed. This is an additional form of validation of 
                           the positivity-hospitalization functional relationship since it is expected to have heatzones in 
                           hospital-rich areas where people are hospitalized. Then, the distance between geolocated tweets (and 
                           associated gradient in positivity related to hospitalization) is physically meaningful and potentially 
                           indicating fluxes of healthcare pressure (from zones of low HP to high HP)."),
                           br(),
                           p("This form of spatial validation of the model (i.e. in this case about the validity of Tweets as a spatial 
                             transfer function of hospitalized) is provided by the independent prediction of expected hospitalized in 
                             hospital-rich areas. Therefore, the higher the geographical overlap between hospitals and heatzones the 
                             higher the accuracy of the positivity-hospitalization relationship. The correlation coefficient (magnitude 
                             and sign) between positivity and hospitalization is revealing the potential risk aversion (or perception) 
                             of populations: positive and negative correlation implies risk-taking (or misinformed) and risk-aversion 
                             (or well-informed), respectively. Levels of positivity reflect risk magnitude of hospitalized and not risk 
                             perception."),
                           br(),
                           h4("Spatial Scalability"),
                           p("Scaling function based on population density or population can be used to scale up volumetric variables 
                           (cases and hospitalization as well as Tweet volume) from the major city to the country scale. The major city 
                           in a country is representative of at least 80% of events of the whole system. In term of positivity and 
                             emotion fluctuations the same patterns over time are observed for major cities and the country."),
                           br(),
                           h4("Representativeness"),
                           p("spatial influence representativeness: Tweets in a city contains information of spatially separated events 
                             about the same process analyzed; thus spatial spread of COVID and top tweeted pairs can be calculated over 
                             geolocated Tweets"),
                           p("unreported information representativeness: Tweets reports information that may not be reported by 
                             official media (see previous Tweet I sent for instance) and/or that may circulate in the general 
                             population (very likely to be honest, e.g. just spoken information)"),
                           p("technological representativeness: more often than not Twitter users are also using other widely popular 
                             or ''local'' social media platforms (e.g. Facebook) where the same information is reported. This occurs 
                             because of the explicit technological linkage between social media platforms (e.g. Facebook and Instagram 
                             are linked) or simply because the implicit information perception and emotional state of people is 
                             invariant at the moment of posting. Yet, posting time and content (related to volume and positivity) is 
                             very weakly dependent on the social media platform."),
                           p("population representativeness: despite Twitter penetration is different for different countries Tweets 
                             show high relevance for predicting spatio-temporal patterns of infections and hospitalization. 
                             Additionally, emotional states are highly linked to local non-Twitter media, yet they reflect the 
                             volumetric and emotional states of the population investigated. Certainly, demographic and other features 
                             of the tweeting population do not represent features of the whole population but those are not features 
                             that are currently analyzed or used."),
                          h3(strong("Data Sources")),
                          h4("Epidemiological Data"),
                          h4("Hospitalization Data and Inference"),
                          h4("Twitter Data"),
                          
                          h3(strong("Who are we?")),
                          p("We are the NEXUS Lab, a team of scientists lead by Dr. Matteo Convertino. 
                             At the Nexus Lab we are broadly interested in quantifying connections in complex socio-ecological 
                             and biological systems (the Nexus!) for understanding how ecosystem works, and designing them 
                             considering the desired objectives (”ecosystem services”).  
                             This can also lead to learning bio-inspired rules and models to adopt for realizing bioinspired technology, 
                             management solutions and design. "),
                           br(),
                           
                           h3(strong("Disclaimers")),
                           p("We do not plan to Tweet or Retweet any content. The aforementioned information from our analysis 
                           (predictability indicators over time and space, inferred sentiments, pressure on healthcare system, 
                           events and Tweets popularity, and vulnerability classes) will be shared on a public dashboard without 
                           revealing private information of users and information banned by Twitter for public disclosure (e.g. Tweet text). 
                           Thus, the only information that will be shared is about our post-processing data indicators 
                           (cross correlation functions and transfer entropy) that have health policy value for WHO and potentially for 
                           the healthcare system at smaller administrative scales as well as for research community. The top ten words in 
                           terms of frequency shared by Tweets in the sample populations will be shared. These words will not be attached 
                             to a specific user because in fact they are representative of the sampled population.")
                           
                  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  # TweetMap
  observeEvent(input$display, {
    
    # Generate kriging data
    ifelse(input$city == "Bangkok",
           kriginHosp <- krigingBan_hosp,
           ifelse(input$city == "New Delhi",
                  kriginHosp <- krigingDel_hosp,
                  ifelse(input$city == "Jakarta",
                         kriginHosp <- krigingJak_hosp,
                         kriginHosp <- krigingMum_hosp))
    )
    
    # ifelse(input$city == "Bangkok",
    #        kriginCase <- krigingBan_case,
    #        ifelse(input$city == "New Delhi",
    #               kriginCase <- krigingDel_case,
    #               ifelse(input$city == "Jakarta",
    #                      kriginCase <- krigingJak_case,
    #                      kriginCase <- krigingMum_case))
    # )
    
    # Krigin output
    output$krigingMap <- renderGoogle_map({
      
      google_map(key = ggmap_key, data = kriginHosp, search_box = T) %>% 
        add_circles(lat = "lat", lon = "lng", draggable = F, mouse_over = "predShow") %>%
        add_heatmap(lat = "lat",lon = "lng", weight = "pred0", option_opacity = 0.5, option_radius = 0.009,
                    option_gradient = gradient,
                    legend = T, legend_options = list(title  = "Healthcare Pressure",
                                                      position = "TOP_RIGHT")) 
      
      # # hospitalization prediction
      # mPredHosp = mean(kriginHosp$var1.pred, na.rm = T) 
      # 
      # hosp_kde <- bkde2D(kriginHosp[ , c("lng", "lat")],
      #                    bandwidth=c(.0045, .0068), gridsize = c(100,100))
      # 
      # hosp_CL <- contourLines(hosp_kde$x1 , 
      #                         hosp_kde$x2 , 
      #                         hosp_kde$fhat)
      # 
      # # EXTRACT CONTOUR LINE LEVELS
      # hosp_LEVS <- as.factor(sapply(hosp_CL, 
      #                               `[[`, "level"))
      # hosp_NLEV <- length(levels(hosp_LEVS))
      # 
      # # CONVERT CONTOUR LINES TO POLYGONS
      # hosp_pgons <- lapply(1:length(hosp_CL), function(i)
      #   Polygons(list(Polygon(cbind(hosp_CL[[i]]$x, 
      #                               hosp_CL[[i]]$y))), 
      #            ID=i))
      # hosp_spgons = SpatialPolygons(hosp_pgons)
      # 
      # hosp_pal <- colorFactor(palette = "YlOrRd", 
      #                         levels = hosp_LEVS, 
      #                         reverse = F)
      # 
      # # cases prediction
      # mPredCase = mean(kriginCase$var1.pred, na.rm = T)
      # 
      # case_kde <- bkde2D(kriginCase[ , c("lng", "lat")],
      #                    bandwidth=c(.0045, .0068), 
      #                    gridsize = c(100,100))
      # 
      # case_CL <- contourLines(case_kde$x1 , 
      #                         case_kde$x2 , 
      #                         case_kde$fhat)
      # 
      # # EXTRACT CONTOUR LINE LEVELS
      # case_LEVS <- as.factor(sapply(case_CL, 
      #                               `[[`, "level"))
      # case_NLEV <- length(levels(case_LEVS))
      # 
      # # CONVERT CONTOUR LINES TO POLYGONS
      # case_pgons <- lapply(1:length(case_CL), function(i)
      #   Polygons(list(Polygon(cbind(case_CL[[i]]$x, 
      #                               case_CL[[i]]$y))), 
      #            ID=i))
      # case_spgons = SpatialPolygons(case_pgons)
      # 
      # case_pal <- colorFactor(palette = "YlOrRd", 
      #                         levels = case_LEVS, 
      #                         reverse = F)
      # 
      # leaflet() %>% 
      #   addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      #   addProviderTiles(providers$OpenStreetMap.Mapnik, group = "OSM") %>%
      #   addPolygons(group = "Hospitalization", 
      #               data = hosp_spgons,
      #               color = hosp_pal(hosp_LEVS),
      #               popup = htmlEscape(hosp_LEVS
      #                 # paste0(
      #                 # "Approximately ", 
      #                 # ifelse(as.numeric(as.character(hosp_LEVS)) - mPredHosp > 0,
      #                 #        round(as.numeric(as.character(hosp_LEVS)) - mPredHosp), 0),
      #                 # " people here may require hospitalization next week.")
      #               )
      #   ) %>%
      #   addPolygons(group = "Cases", 
      #               data = case_spgons,
      #               color = case_pal(case_LEVS),
      #               popup = htmlEscape(case_LEVS
      #                 # paste0(
      #                 # "Approximately ", 
      #                 # ifelse(as.numeric(as.character(case_LEVS)) - mPredCase > 0,
      #                 #        round(as.numeric(as.character(case_LEVS)) - mPredCase), 0),
      #                 # " people here may require hospitalization next week.")
      #               )
      #   ) %>%
      #   addLegend(pal = hosp_pal, values = hosp_LEVS, group = "Hospitalization",
      #             position = "bottomright", 
      #             title = "Healthcare Pressure Indicator") %>%
      #   addLegend(pal = case_pal, values = case_LEVS, group = "Cases",
      #             position = "bottomright", 
      #             title = "Healthcare Pressure Indicator") %>%
      #   addLayersControl(position = "bottomright",
      #                    baseGroups = c("Satellite", "OSM"),
      #                    overlayGroups = c("Hospitalization", "Cases"),
      #                    options = layersControlOptions(collapsed = FALSE)
        # )
      
    })
    
    #Generate tweet data
    ifelse(input$city == "Bangkok",
           tweetCoord <- tweetCoord_Bang,
           ifelse(input$city == "New Delhi",
                  tweetCoord <- tweetCoord_Del,
                  ifelse(input$city == "Jakarta",
                         tweetCoord <- tweetCoord_Jak,
                         tweetCoord <- tweetCoord_Mum)
                  )
               )
    
    # Tweet Timeline
    output$sentiMeter <- renderPlot({

      p <- tweetCoord %>%
        ggplot(aes(x = as.Date(recordDate), y = mean_daily_stm, group = 1)) +
        geom_line(size = 1, color = "black") +
        geom_point(size = 5) +
        scale_color_gradient2(mid = "yellow",midpoint = 5) +
        guides(color = F) +
        labs(x = "", y = "Positivity")+
        tweetPlotTheme
      p
    })
    
    # Generate cases data
    ifelse(input$city == "Bangkok",
           covidCases <- read.csv("./data/epi_data_bangkok.csv"),
           ifelse(input$city == "New Delhi",
                  covidCases <- read.csv("./data/epi_data_new delhi.csv"),
                  ifelse(input$city == "Jakarta",
                         covidCases <- read.csv("./data/epi_data_mumbai.csv"),
                         covidCases <- read.csv("./data/epi_data_jakarta.csv")))
    )
    # 
    # Cases
    output$caseTime <- renderPlot({
      
      cases_plot <- covidCases %>%
        ggplot(aes(x = as.Date(recordDate), y = daily_case, group  = 1)) +
        geom_point() +
        geom_line() +
        labs(x = "", y = "New Cases") +
        tweetPlotTheme
      
      cases_plot
      
    })
    
    # Hospitalization
    output$hospTime <- renderPlot({
      
      hosp_plot <- covidCases  %>%
        ggplot(aes(x = as.Date(recordDate), y = hospital, group = 1)) +
        geom_point() +
        geom_line() +
        labs(x = "", y = "Hospitalizations") +
        tweetPlotTheme
      
      hosp_plot
      
    })
    
    ## Text Analysis
    observeEvent(input$tweetSet,{
      
      # Top Emotions
      ifelse(input$city == "Bangkok" & input$tweetSet == "All",
             tweetEmotions <- read.csv("./data/tweetEmo_bangkok.csv"),
             ifelse(input$city == "New Delhi" & input$tweetSet == "All",
                    tweetEmotions <- read.csv("./data/tweetEmo_delhi.csv") ,
                    ifelse(input$city == "Jakarta" & input$tweetSet == "All",
                           tweetEmotions <- read.csv("./data/tweetEmo_jakarta.csv"),
                           ifelse(input$city == "Mumbai" & input$tweetSet == "All",
                                  tweetEmotions <- read.csv("./data/tweetEmo_mumbai.csv"),
                                  ifelse(input$city == "Bangkok" & input$tweetSet == "Misinformation",
                                         tweetEmotions <- read.csv("./data/misinformation_tweet_emotions_bangkok.csv"),
                                         ifelse(input$city == "New Delhi" & input$tweetSet == "Misinformation",
                                                tweetEmotions <- read.csv("./data/misinformation_tweet_emotions_delhi.csv"),
                                                ifelse(input$city == "Jakarta" & input$tweetSet == "Misinformation",
                                                       tweetEmotions <- read.csv("./data/misinformation_tweet_emotions_jakarta.csv"),
                                                       ifelse(input$city == "Mumbai" & input$tweetSet == "Misinformation",
                                                              tweetEmotions <- read.csv("./data/misinformation_tweet_emotions_mumbai.csv"),
                                                              ifelse(input$city == "Bangkok" & input$tweetSet == "Healthcare",
                                                                     tweetEmotions <- read.csv("./data/healthcare_tweet_emotions_bangkok.csv"),
                                                                     ifelse(input$city == "New Delhi" & input$tweetSet == "Healthcare",
                                                                            tweetEmotions <- read.csv("./data/healthcare_tweet_emotions_delhi.csv"),
                                                                            ifelse(input$city == "Jakarta" & input$tweetSet == "Healthcare",
                                                                                   tweetEmotions <- read.csv("./data/healthcare_tweet_emotions_jakarta.csv"),
                                                                                   tweetEmotions <- read.csv("./data/healthcare_tweet_emotions_mumbai.csv"))))))))))))
      # tweet emotions over time
      output$emoTime <- renderPlot({
        
        emoPlot <- tweetEmotions %>%
          filter(!sentiment %in% c("positive", "negative")) %>%
          ggplot(aes(x = as.Date(day_created), y = n, color = sentiment, group = sentiment)) +
          geom_line(size = 1) +
          geom_point(size = 3) +
          scale_color_manual(values = emoColors,
                             labels = c("Anger",
                                        "Anticipation",
                                        "Disgust",
                                        "Fear",
                                        "Joy",
                                        "Sadness",
                                        "Surprise",
                                        "Trust")) +
          labs(x = "Date", y = "Number of words", color = "Emotion") +
          tweetPlotTheme +
          theme(text = element_text(size = 18))
        
        emoPlot
        
      })
      
      ifelse(input$city == "Bangkok" & input$tweetSet == "All",
             tweets <- tweetSent_ban,
             ifelse(input$city == "New Delhi" & input$tweetSet == "All",
                    tweets <- tweetSent_del,
                    ifelse(input$city == "Jakarta" & input$tweetSet == "All",
                           tweets <- tweetSent_jak,
                           ifelse(input$city == "Mumbai" & input$tweetSet == "All",
                                  tweets <- tweetSent_mum,
                                  ifelse(input$city == "Bangkok" & input$tweetSet == "Misinformation",
                                         tweets <- filter(tweetSent_ban, str_detect(text, "fake|misinformation|lie|false")),
                                         ifelse(input$city == "New Delhi" & input$tweetSet == "Misinformation",
                                                tweets <- filter(tweetSent_del, str_detect(text, "fake|misinformation|lie|false")),
                                                ifelse(input$city == "Jakarta" & input$tweetSet == "Misinformation",
                                                       tweets <- filter(tweetSent_jak, str_detect(text, "fake|misinformation|lie|false")),
                                                       ifelse(input$city == "Mumbai" & input$tweetSet == "Misinformation",
                                                              tweets <- filter(tweetSent_mum, str_detect(text, "fake|misinformation|lie|false")),
                                                              ifelse(input$city == "Bangkok" & input$tweetSet == "Healthcare",
                                                                     tweets <- filter(tweetSent_ban, str_detect(text, "hospital|test")),
                                                                     ifelse(input$city == "New Delhi" & input$tweetSet == "Healthcare",
                                                                            tweets <- filter(tweetSent_del, str_detect(text, "hospital|test")),
                                                                            ifelse(input$city == "Jakarta" & input$tweetSet == "Healthcare",
                                                                                   tweets <- filter(tweetSent_jak, str_detect(text, "hospital|test")),
                                                                                   tweets <- filter(tweetSent_mum, str_detect(text, "hospital|test")))))))))))))
      
      output$Tweets <-DT::renderDataTable({
        
        DT::datatable(select(tweets,
                             "Date" = day_created,
                             "User" = screen_name,
                             "Tweet" = text,
                             "Retweet Count" = retweet_count,
                             "Positivity" = Sent),
                      rownames = F,
                      options = list(dom = "ftp", pageLength = 5,
                                     ordering = T,
                                     initComplete = JS(
                                       "function(settings, json) {",
                                       "$('th').css({'background-color': '#000', 'color': '#fff'});",
                                       "}")))
        
      })
      
      observeEvent(input$filter,{
        
        ifelse(input$city == "Bangkok" & input$tweetSet == "All",
               tweetBigrams <- read.csv("./data/tweetBigrams_bangkok.csv") %>%
                 filter(as.Date(day_created) == input$topWord_dates) %>%
                 top_n(input$bigram_n, wt = n),
               ifelse(input$city == "New Delhi" & input$tweetSet == "All",
                      tweetBigrams <- read.csv("./data/tweetBigrams_delhi.csv")%>%
                        filter(as.Date(day_created) == input$topWord_dates) %>%
                        top_n(input$bigram_n, wt = n),
                      ifelse(input$city == "Jakarta" & input$tweetSet == "All",
                             tweetBigrams <- read.csv("./data/tweetBigrams_jakarta.csv")%>%
                               filter(as.Date(day_created) == input$topWord_dates) %>%
                               top_n(input$bigram_n, wt = n),
                             ifelse(input$city == "Mumbai" & input$tweetSet == "All",
                                    tweetBigrams <- read.csv("./data/tweetBigrams_mumbai.csv")%>%
                                      filter(as.Date(day_created) == input$topWord_dates) %>%
                                      top_n(input$bigram_n, wt = n),
                                    ifelse(input$city == "Bangkok" & input$tweetSet == "Misinformation",
                                           tweetBigrams <- read.csv("./data/misinformation_Bigrams_bangkok.csv") %>%
                                             filter(as.Date(day_created) == input$topWord_dates) %>%
                                             top_n(input$bigram_n, wt = n),
                                           ifelse(input$city == "New Delhi" & input$tweetSet == "Misinformation",
                                                  tweetBigrams <- read.csv("./data/misinformation_Bigrams_delhi.csv") %>%
                                                    filter(as.Date(day_created) == input$topWord_dates) %>%
                                                    top_n(input$bigram_n, wt = n),
                                                  ifelse(input$city == "Jakarta" & input$tweetSet == "Misinformation",
                                                         tweetBigrams <- read.csv("./data/misinformation_Bigrams_jakarta.csv") %>%
                                                           filter(as.Date(day_created) == input$topWord_dates) %>%
                                                           top_n(input$bigram_n, wt = n),
                                                         ifelse(input$city == "Mumbai" & input$tweetSet == "Misinformation",
                                                                tweetBigrams <- read.csv("./data/misinformation_Bigrams_mumbai.csv") %>%
                                                                  filter(as.Date(day_created) == input$topWord_dates) %>%
                                                                  top_n(input$bigram_n, wt = n),
                                                                ifelse(input$city == "Bangkok" & input$tweetSet == "Healthcare",
                                                                       tweetBigrams <- read.csv("./data/healthcare_Bigrams_bangkok.csv") %>%
                                                                         filter(as.Date(day_created) == input$topWord_dates) %>%
                                                                         top_n(input$bigram_n, wt = n),
                                                                       ifelse(input$city == "New Delhi" & input$tweetSet == "Healthcare",
                                                                              tweetBigrams <- read.csv("./data/healthcare_Bigrams_delhi.csv")%>%
                                                                                filter(as.Date(day_created) == input$topWord_dates) %>%
                                                                                top_n(input$bigram_n, wt = n),
                                                                              ifelse(input$city == "Jakarta" & input$tweetSet == "Healthcare",
                                                                                     tweetBigrams <- read.csv("./data/healthcare_Bigrams_jakarta.csv")%>%
                                                                                       filter(as.Date(day_created) == input$topWord_dates) %>%
                                                                                       top_n(input$bigram_n, wt = n),
                                                                                     tweetBigrams <- read.csv("./data/healthcare_Bigrams_mumbai.csv")%>%
                                                                                       filter(as.Date(day_created) == input$topWord_dates) %>%
                                                                                       top_n(input$bigram_n, wt = n))))))))))))
        
        # Top Bigrams
      output$bigrams <- renderPlot({
        
        tweetBigramsPlot <- tweetBigrams %>%
          ggplot(aes(x = reorder(pairs, n), y = n)) +
          geom_bar(stat = "identity") +
          coord_flip() +
          labs(x = "", y = "Frequency", title = "Most Frequent Pairs") +
          tweetPlotTheme
        
        tweetBigramsPlot
        
      })
        
      })
  
    })
    
    # Select Data for Predictability indicators
    ifelse(input$city == "Bangkok",
           predictors <-gaps_bang,
           ifelse(input$city == "New Delhi",
                  predictors <- gaps_delhi,
                  ifelse(input$city == "Jakarta",
                         predictors <- gaps_jak,
                         predictors <- gaps_mum)))
    
    # Present correlation and time series values
    output$case_predictors <- DT::renderDataTable(datatable(select(predictors, "Week" = week_no, 
                                                                   "Risk" = case_risk, 
                                                                   "Gap" = gap_case, 
                                                                   "Divergence" = case_pred,
                                                                   "Correlation" = cc_stm_case),rownames = F,
                                                            options = list(dom = "t", ordering = F, 
                                                                           initComplete = JS(
                                                                             "function(settings, json) {",
                                                                             "$('th').css({'background-color': '#000', 'color': '#fff'});",
                                                                             "}" )))%>%
                                                    formatPercentage(c("Risk", "Gap"), 2))
    
    output$hosp_predictors <- DT::renderDataTable(datatable(select(predictors, "Week" = week_no, 
                                                                   "Risk" = hosp_risk, 
                                                                   "Gap" = gap_hosp, 
                                                                   "Divergence" = hosp_pred,
                                                                   "Correlation" = cc_stm_hosp),rownames = F,
                                                            options = list(dom = "t", ordering = F, 
                                                                           initComplete = JS(
                                                                             "function(settings, json) {",
                                                                             "$('th').css({'background-color': '#000', 'color': '#fff'});",
                                                                             "}" ))) %>%
                                                    formatPercentage(c("Risk", "Gap"), 2))
    
    # Top tweets and tweeters
    # ifelse(input$city == "New Delhi",
    #        tweets <- select(tweetSent_del,
    #                         "User" = screen_name,
    #                         "Date" = day_created, 
    #                         "Tweet" = text, 
    #                         "Positivity" = Sent,
    #                         "Retweet Count" = retweet_count),
    #        ifelse(input$city == "Mumbai",
    #               tweets <- select(tweetSent_mum,
    #                                "User" = screen_name, 
    #                                "Date" = day_created, 
    #                                "Tweet" = text, 
    #                                "Positivity" = Sent,
    #                                "Retweet Count" = retweet_count),
    #               ifelse(input$city == "Jakarta",
    #                      tweets <- select(tweetSent_jak,
    #                                       "User" = screen_name, 
    #                                       "Date" = day_created, 
    #                                       "Tweet" = text, 
    #                                       "Positivity" = Sent,
    #                                       "Retweet Count" = retweet_count),
    #                      tweets <- select(tweetSent_ban,
    #                                       "User" = screen_name, 
    #                                       "Date" = day_created, 
    #                                       "Tweet" = text, 
    #                                       "Positivity" = Sent,
    #                                       "Retweet Count" = retweet_count))))
    
    # ifelse(input$city == "Bangkok",
    #        misinformation_tweet_emotions <- misinformation_tweet_emotions_ban,
    #        ifelse(input$city == "New Delhi",
    #               misinformation_tweet_emotions <- misinformation_tweet_emotions_del,
    #               ifelse(input$city == "Jakarta",
    #                      misinformation_tweet_emotions <- misinformation_tweet_emotions_jak,
    #                      misinformation_tweet_emotions <- misinformation_tweet_emotions_mum)))
    
    
    
    # output$misinformingTweets <- DT::renderDataTable({
    # 
    #   datatable(filter(tweets, str_detect(Tweet, "fake|misinformation|lie|false")),rownames = F,
    #             options = list(dom = "ftp",
    #                            ordering = T,
    #                            initComplete = JS(
    #                              "function(settings, json) {",
    #                              "$('th').css({'background-color': '#000', 'color': '#fff'});",
    #                              "}")))
    # 
    # }, server = T)

    
    # output$misinforming_bigrams <- renderPlot({
    #   
    #   misinfoBigramsPlot <- misinformation_Bigrams %>%
    #     ggplot(aes(x = reorder(pairs, n), y = n)) +
    #     geom_bar(stat = "identity") +
    #     coord_flip() +
    #     labs(x = "", y = "Frequency", title = "Most Frequent Pairs") +
    #     tweetPlotTheme
    #   
    #   misinfoBigramsPlot
    #   
    # })

    # output$misinform_selected <- DT::renderDataTable({
    # 
    #   datatable(select(tweets[c(input$topTweets_rows_selected),], Tweet),rownames = F,
    #             options = list(dom = "tp", ordering = F))
    # 
    # })
    # 
    # observeEvent(input$email,{
    # 
    #   select(misinform[c(input$topTweets_rows_selected),], Tweet) %>%
    #   write.csv("./data/misinformation.csv")
    # 
    #   send.mail(
    #     from = "elroy.galbraith@gmail.com",
    #     to = c("nexuslabhokkaido@gmail.com"),
    #     subject = "Selected misinforming tweets",
    #     body = "Hi Nexus Lab, \n Attached are some misinforming tweets to track. \n Regards.",
    #     smtp = list(host.name = "aspmx.l.google.com", port = 25),
    #     authenticate = F,
    #     attach.files = "./data/misinformation.csv",
    #     send = T
    #   )
    # 
    # })
    
    # Healthcare Satisfaction
    # ifelse(input$city == "Bangkok",
    #        healthcare_tweet_emotions <- healthcare_tweet_emotions_ban,
    #        ifelse(input$city == "New Delhi",
    #               healthcare_tweet_emotions <- healthcare_tweet_emotions_del,
    #               ifelse(input$city == "Jakarta",
    #                      healthcare_tweet_emotions <- healthcare_tweet_emotions_jak,
    #                      healthcare_tweet_emotions <- healthcare_tweet_emotions_mum)))
    
    
    
    # output$healthcareTweets <-DT::renderDataTable({
    #   
    #   DT::datatable(filter(tweets, str_detect(Tweet, "hospital|test")),rownames = F,
    #                 options = list(dom = "ftp",
    #                                ordering = T,
    #                                initComplete = JS(
    #                                  "function(settings, json) {",
    #                                  "$('th').css({'background-color': '#000', 'color': '#fff'});",
    #                                  "}")))
    #   
    # })
    # output$healthcare_bigrams <- renderPlot({
    #   
    #   healthBigramsPlot <- healthcare_Bigrams %>%
    #     ggplot(aes(x = reorder(pairs, n), y = n)) +
    #     geom_bar(stat = "identity") +
    #     coord_flip() +
    #     labs(x = "", y = "Frequency", title = "Most Frequent Pairs") +
    #     tweetPlotTheme
    #   
    #   healthBigramsPlot
    #   
    # })
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
