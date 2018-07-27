# Please note the app should be ran in MAC OS system due to 
# coding involves fixing errors caused by encoding of the file.
# If you cannot run the code, visit https://cfon0004.shinyapps.io/visualization_project/
# It is much slower and easier to crash

# Title: FIT5147 Visualization project
# Student : FONG CHEOK KIN,  28692381
# Date: 03/06/2018

# Import libraries
library(shiny)
library(scales)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tm)
library(wordcloud)
library(RColorBrewer)

# Define icons for the markers on leaflet map
leafIcons <- iconList(
  l1 = makeIcon(iconUrl = "./www/deadconlv1.png", iconWidth = 10, iconHeight = 10),
  l2 = makeIcon(iconUrl = "./www/deadconlv2.png", iconWidth= 15, iconHeight= 15),
  l3 = makeIcon(iconUrl = "./www/deadconlv3.png", iconWidth= 15, iconHeight = 15),
  l4 = makeIcon(iconUrl = "./www/deadconlv4.png", iconWidth= 20, iconHeight =20)
)

# read file
df <- read.csv('globalterrorismdb_0617dist.csv')

# Remove the multiple bytes values, else it will cause errors in printing summary and motive
# Warning : the encoding convertion allow Mac OS works properly, so the codes can only run in MAC OS system
df$motive <- iconv(enc2utf8(as.character(df$motive)),sub="byte")
df$summary <- iconv(enc2utf8(as.character(df$summary)),sub="byte")

# Write a function to assign level of markers, 
# If kill > 10 , highest level, else if with any kill second highest, only injured second last,lowest level with no kill/injured
assign_level <- function(k, i){
  if (is.na(k)) { return ("l1")}
  if (k >= 10) {
    return ("l4")
  }
  else if  (k > 0) {
    return ("l3")
  }
  else if (is.na(i)) {
    return ("l1")
  }
  else if (i > 0) {
    return ("l2")
  }
  else {return ("l1")}
}

# Assign levels to the attacks
levels = c()
for(i in (1:nrow(df))) {
  levels[i] <- assign_level(df$nkill[i],df$nwound[i])
}
df$level <- levels

# Extract country names and cities names for UI selection
country_names <- sort(as.character(unique(df$country_txt)))
city_names <- sort(as.character(unique(df$city)))

# UI is put in the same R script, because it always uses the column values as input choices.
ui <- shinyUI(
  tabsetPanel(
    # Create tab one - called attacks visualizer
  tabPanel("Attacks Visualizer",
  fluidPage(
          # Set the styles of the interface
          theme = shinytheme("cyborg"),
          tags$style(HTML("#injury{
                     display:inline;
                     color: white;
                     font-size: 35px; 
                     font-style: bold;
                                       }",
                  "#kill{
                     display:inline;
                     color: white;
                     font-size: 35px; 
                     font-style: bold;
                     }",
                  "#info{
                    color: white;
                    font-size: 14px; 
                  }"
                  , "h4{
                    color: red;
                    font-style: bold;
                    display:inline;
                  }",
                  ".shiny-output-error-validation {
                    color: white;
                    font-style: bold;
                    font-size: 20px; 
                  }",
                  
                  ".tabbable > .nav > li > a{
                    background-color: black; 
                    color:white}"
                  , 
                  "h1{ font-size: 50px;}"
                    ) ),
          # Title of the page
          tags$i( h1(titlePanel("How are Terrorists Threatening our Lives?"))),
          tags$h4( "Warning: DO NOT include a large dataset to avoid system crash, 
                   follow the selection procedure in the guide!"),
          
          # Divide the page
          fluidRow(
              column(8,
                     
                  fluidRow(
                  column(3,
                  # Insert the selection filters : Country
                  selectInput(inputId = "Country",
                              label = "Country: ",
                              choices = c("All",country_names), multiple = F,
                                          selected = "All"),
                  # City is based on the selection of City
                  uiOutput("cities"),
                  # Attack type
                  selectInput(
                  inputId = "atk_type",
                  label   = "Select the attack type(s):",
                  choices =  c("All",sort(as.character(unique(df$attacktype1_txt)))),
                  multiple = TRUE,
                  selected = "All"
                              ),
                  # Year control, set 1995 to 2005 to limit data points
                  sliderInput(
                    inputId = "yr_range", "Year range:",
                    min = min(as.integer(df$iyear)), max = max(as.integer(df$iyear)),
                    value = c(1995,2005),
                    step = 1),
                  
               # Kills range is based on the above selections, to avoid oversize, meaningless bar
                    uiOutput("nkill"),
                    uiOutput("nwound")
                       ),
               # Divide another column for output
                 column(9,
                    # Output the map  
                    leafletOutput("map"),
                    # Detail of the event if clicked
                    htmlOutput("info"),
                    # Number of kills and deaths in total
                    fluidRow(img(src = 'deadconlv4.png', height = '100px', width = '100px'),
                    textOutput("kill")),
                    fluidRow(
                    img(src = 'deadconlv2.png', height = '100px', width = '100px'),
                    textOutput("injury"))
                   )
              )),
              
              # Rightest column for the other two points, one scatter, one line graph
              column(4,
                     plotOutput("point",click = "plot_click", height = "285px",width = "100%"),
                     br(),
                     plotOutput("line", height = "285px",width = "100%")
              )
            )
          
  )),
  # The second panel called Attacks Analysizer
  tabPanel( "Attacks Analyzer",
            fluidRow(
            column(2, br()," ",
                   # Select filters: Country
              selectInput(
                         inputId = "Country2",
                         label = "Country: ",
                         choices = c("All",country_names), multiple = T,
                         selected = "All")," ",
                  # Year filter
              sliderInput(
                        inputId = "yr_range2", "Year range:",
                         min = min(as.integer(df$iyear)), max = max(as.integer(df$iyear)),
                         value = c(1970,2016),
                         step = 1)," ",
                  # Attack type
              selectInput(
                         inputId = "atk_type2",
                         label   = "Select the attack type(s):",
                         choices =  c("All",sort(as.character(unique(df$attacktype1_txt)))),
                         multiple = TRUE,
                         selected = "All"
             )," ",
              # Text allows user to enter key to search motive
              textInput(
                   inputId = "keyword", 
                   label="Enter one key word to search about the motive:",
                   value = "",
                   width = NULL, placeholder = NULL)
                      ), column(10, 
                                # Print instruction for user
       "This analyzer tells you about the motive of the attacks and the related terrorist groups.",br(),
        "1. Select the Country, time interval, attack type(s) you would like to study.",br(),
        "2. Enter the key word (can be based on your observation) to further study a particular motive topic.",br(),
              #  Print the word columns, by splitting into columns
              column(5, h3("Motive: "),plotOutput("word",width = "100%")) , 
              column(5, h3("Terrorist Group:"), plotOutput("killer",width ="100%")))
            )
      ),
  # Last Panel for Reference and Quick Guide
  tabPanel(
   # Print the words for copyright
        "Reference and Quick Guide",
        h3(" I. Database and Copy Right:"),
        " This system uses Global Terrorism Database (GTD), which is an open-source database including
        information on terrorist events around the world from 1970 through 2016. GTD reserves all usage rights
        of this dataset. Refer to",
        tags$a(href="https://www.start.umd.edu/gtd/terms-of-use/", "Term of Use"),".", br(),
        h3(" II. Symbols:"),
        img(src = 'deadconlv4.png', height = '40px', width = '40px'),"Attack killing 10 or more people.",br(),
        img(src = 'deadconlv3.png', height = '40px', width = '40px'),"Attack killing 1 or more people.",br(),
        img(src = 'deadconlv2.png', height = '40px', width = '40px'),"Attack with 1 or more injured.",br(),
        img(src = 'deadconlv1.png', height = '40px', width = '40px',"No one was killed or injured."),br(),
        hr(),
        h3(" III. Selection procedure for Attacks Visualizer:"),
        " 1. Select the country you want to investigate.",br(),
        " 2. Select the city, if applicable.",br(),
        " 3. Select the attack type(s).",br(),
        " 4. Adjust the time period, death, injured based on your need.",br(),
        " 5. DO NOT include all data, this causes system crash,
        apply filters to reduce datasize if nothing is shown on the map.",
        hr(),
        h3(" IV. Interactions:"),
        " 1. Click the symbols on the map, basic information of the attack will be shown.",br(),
        " 2. To study extreme events, click the point on the scatter, details will be printed.",br(),
        tags$b(tags$i(" For more explanation, refer to the user guide in the report."))
      )
  ))

# Sever of the App
server <-shinyServer ( function(input, output, session) {
  # Reactive data for panel 1
  data <- reactive({ 
        req(input$Country)
        # Filter other selections if country is All
        if (input$Country == "All") {
          # filter year
          temp_df <- df %>% filter(iyear >= input$yr_range[1] & iyear <= input$yr_range[2])
          # Attack types
            if(!"All" %in% input$atk_type) {
              temp_df %>% filter(attacktype1_txt %in% input$atk_type)}
            else {temp_df }
          }
    else {
        # Else filter the country as well. 
          temp_df <- subset(df, country_txt == input$Country) %>%
          filter(iyear >= input$yr_range[1] & iyear <= input$yr_range[2])
          if(!"All" %in% input$atk_type) {
            temp_df %>% filter(attacktype1_txt %in% input$atk_type)}
          else { temp_df }
    }
  })

  # Plot Scatter of kills vs injured
  output$line <- renderPlot({
    req(input$city)
    # Since city is reactive UI to country, so cannot apply filter in  the reactive data() function
    if (input$city != "All") {
      temp_df  <- subset(data(),city == input$city & 
                           nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                           nwound >= input$nwound[1] &  nwound <= input$nwound[2] 
                           )}  
    else{
      temp_df <- subset (data(), nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                          nwound >= input$nwound[1] &  nwound <= input$nwound[2]) 
      }
    # Group by the attack type and year, find out the counts of each type
    temp_df <- temp_df %>% group_by(iyear,attacktype1_txt) %>% summarise(atk_freq = n())
    # Plot line graph and set the styles
    ggplot(temp_df,aes(x=iyear,y=atk_freq, color=attacktype1_txt)) + geom_line() +
    labs(color='Attack Type') +
    labs(title="Number of Attacks", x ="Year", y = "Frequency") + theme_bw()  + 
    theme(legend.position="bottom", axis.text=element_text(size=12, face="bold"),
          plot.title = element_text(hjust = 0.5, size = 15,face="bold"),
          axis.title = element_text(size=12, face = "bold"),
          legend.title=element_text(size=10) , 
          legend.text=element_text(size=10), legend.justification = c(0,0)) +
    guides(colour=guide_legend(nrow= 5, ncol=2,byrow = T, title.position = "top")) +
    theme(legend.key.height=unit(0.25,"cm")) + scale_x_continuous(breaks= pretty_breaks())
  })
    
  output$point <- renderPlot({
    req(input$city)
    # Similar to above, check city filter
    if (input$city != "All") {
      temp_df  <- subset(data(),city == input$city & 
                  nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                   nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }  
    else{
      temp_df <- subset (data(), nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                        nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }
    # plot scatter and set style
    ggplot(temp_df,aes(x=nwound,y=nkill)) + geom_point(aes(size=nwound+nkill)) +
    labs(title="Victims in Terrorist Attacks", x ="Injured", y = "Death")  + 
    theme_bw() + theme(plot.title = element_text(hjust = 0.5, size = 15, face="bold"),
                 axis.text= element_text(size=12, face = "bold"),
                 axis.title = element_text(size=12, face = "bold"),legend.position="none") +
    scale_x_continuous(breaks= pretty_breaks())
  })
  
  # Text printed when clicked 
  output$info <- renderText({
    # request plot click
    req(input$plot_click)
    dat <- nearPoints(data(), input$plot_click, xvar = "nwound", yvar = "nkill",
                      threshold = 10,
                      maxpoints = 2)
    # Replace \x92 for "'" encoding in R to avoid print error
    text <- dat %>% pull(summary)
    text <- gsub("\x92", "\'", text)
    # Orgranize the informations that will be printed
    attack_type <- dat%>% pull(attacktype1_txt)
    kill <- ( dat%>% pull(nkill))
    nwound <- ( dat%>% pull(nwound))
    country <- dat%>% pull(country_txt)
    city <- dat%>% pull(city)
    motive <- dat %>% pull(motive)
    # Check if a point exist
    if ((length(attack_type) != 0 ) ) {
      l1 <- paste("Attack type:", attack_type)
      l2 <- paste("Kill:", kill)
      l3 <- paste("Injured:", nwound)
      l6<- paste ("Motive:", motive)
      l4 <- paste("Summary:", text)
      l5 <- paste("Location:",country,",",city)
      HTML(paste(l5,l1,l2,l3,l6,l4,"<br/>", sep="<br/>"))
    }
    else{
      # if clicking empty area
      HTML(paste("No detail information selected / recorded."))
    }
  })
  
  # Print the total kills
  output$kill <- renderText({
    req(input$city)
    if (input$city != "All") {
      temp_df  <- subset(data(),city == input$city & 
                           nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                           nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }  
    else{
      temp_df <- subset (data(), nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                          nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }
    kills <- temp_df %>% pull(nkill) %>%  sum(na.rm = T) %>% round(2)
    paste("Total Deaths:",kills)
  })
  
  # Print the total Injured
  output$injury <- renderText({
    req(input$city)
    if (input$city != "All") {
      temp_df  <- subset(data(),city == input$city & 
                           nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                           nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }  
    else{
      temp_df <- subset (data(), nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                          nwound >= input$nwound[1] &  nwound <= input$nwound[2])
    }
    injury <- temp_df %>% pull(nwound) %>% sum(na.rm = T) %>% round(2)
    paste("Total Injured:",injury)
  })
  
  # City is reactive to the city selected, only city with attacks will be shown
  output$cities <- renderUI({
    selectInput(inputId = "city", "City:", 
                choices =  c("All",as.character(sort(unique(data() %>% pull(city))) ,multiple = F))) })
  
  # kill is reactive other filers to decide the min and max value on the bar
    output$nkill <- renderUI({
                req(input$Country)
                req(input$city)
                req(input$atk_type)
                temp_df <- data()
                if (input$city != "All") {
                  temp_df  <- subset(data(),city == input$city)}
                
                min_kil = min(temp_df %>% pull(nkill),na.rm = T)
                max_kil = max(temp_df %>% pull(nkill),na.rm = T)
                # If no record found , avoid printing NaN
                shiny::validate(
                  need(nrow(temp_df) >0, "No death recorded.")
                )
            sliderInput(inputId = "nkill", "Number of death:", 
                        min = min_kil , max = max_kil, 
                        value = c(min_kil,max_kil)
                        ,step = 1)  })
    
    # injury  is reactive other filers to decide the min and max value on the bar
    output$nwound <- renderUI({
              req(input$Country)
              req(input$city)
              req(input$atk_type)
              temp_df <- data()
              if (input$city != "All") {
                temp_df  <- subset(data(),city == input$city)}
              
              min_w = min(temp_df$nwound,na.rm = T)
              max_w = max(temp_df$nwound,na.rm = T)
             
              shiny::validate(
                need(nrow(temp_df) >0, "No injured recorded.")
              )
              
              
          sliderInput(inputId = "nwound", "Number of Injured:", 
                      min = min_w , 
                      max = max_w,
                      value = c(min_w,max_w),
                      step = 1)  })
    
# Plot the data on the map
  output$map <- renderLeaflet({
      req(input$city)
      req(input$Country)
      # Apply city filter if necessary
      if (input$city != "All") {
        df_temp  <- subset(data(),city == input$city &
                             nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                             nwound >= input$nwound[1] &  nwound <= input$nwound[2])
      }  
      else{
        df_temp <- subset(data() ,nkill >= input$nkill[1] &  nkill <= input$nkill[2] &
                                 nwound >= input$nwound[1] &  nwound <= input$nwound[2] )
      }
      # trial and test find out the limit is around this number can be supported by leaflet.
      # Valid the data points less than 62000 for leaflet map
      shiny::validate(
        need(nrow(df_temp) < 62000, "Too much data to plot on map, please reduce data size.")
      )
  
      # Use provider tiel
      map <- leaflet(df_temp,options = leafletOptions(maxZoom = 10)) %>% 
      addProviderTiles(providers$Stamen.TonerLite, options = providerTileOptions(noWrap = F))  %>%
      # Add mini map so user can know the location after zoom
      addMiniMap(
        minimized = T,
        tiles = providers$Stamen.TonerLite,
        toggleDisplay = T,autoToggleDisplay = T) %>%
        addMarkers(
          clusterOptions = markerClusterOptions(disableClusteringAtZoom = 5),
          # Pop up after user click , and info is shown
                   popup = ~as.character(paste("Location:", country_txt,",", city,"<br>",
                                                "Attack type:",attacktype1_txt,"<br>",
                                                "Date:", iday,"/",
                                                imonth,"/",iyear, "<br>",
                                                "Kills:",nkill,"<br>",
                                                "Injuried:",nwound )
                                                        ), 
      lat = ~latitude, lng =  ~longitude, icon = ~leafIcons[level])
    })
  
  # Reactive data for panel2
  data2 <- reactive({
      req(input$Country2)
        # If all countries are studied
        if (input$Country2 == "All") {
          temp_df <- df %>%
          filter(iyear >= input$yr_range2[1] & iyear <= input$yr_range2[2]) 
          # Check if key word is used, only select the related data
          if (input$keyword != ""){ 
            temp_df$motive <- tolower(temp_df$motive)
            temp_df <-temp_df %>% filter(grepl(tolower(paste("",input$keyword,"")), motive )) }
          # Check if attack type is selected
          if(!"All" %in% input$atk_type2) {
            temp_df <- temp_df %>% filter(attacktype1_txt %in% input$atk_type2)
            }
            temp_df
        }
      else {
        # filter out the other countries
        temp_df <- subset(df, country_txt == input$Country2 )
        temp_df %>% filter(iyear >= input$yr_range2[1] & iyear <= input$yr_range2[2])
        # Applly filter
        if (input$keyword != ""){ temp_df <- temp_df %>% filter(grepl(tolower(input$keyword), motive)) }
        if(!"All" %in% input$atk_type2) {
        temp_df <- temp_df %>% filter(attacktype1_txt %in% input$atk_type2)
        }
        temp_df
      }
  })
  
  # Create word cloud for the motive
  output$word  <- renderPlot({
    set.seed(1234)
    text = data2()
    # to avoid causing erros, remove empty and NA text
    text <- text[!text$motive == '',] 
    text<- text[!is.na(text$motive),] %>% pull(motive)
    words <- Corpus(VectorSource(text))
    words <- tm_map(words, stripWhitespace)
    words <- tm_map(words, content_transformer(tolower))
    words <- tm_map(words, removeNumbers)
    words <- tm_map(words, removePunctuation)
    words <- tm_map(words, removeWords, stopwords("english"))
    words <- tm_map(words, removeWords, c("unknown", "part", "claimed","incident", "responsibility","targeted","stated",
                                          "however", "motive", "specific","sources","attack"))
    # Set the threhold to two words to draw
    shiny::validate(
      need(length(words) > 2, "Need more data to plot a word cloud!")
    )
    # Set max words to 180 to save running time
    wordcloud(words, max.words =180, min.freq = 10, random.order=FALSE, rot.per=0.35,colors=brewer.pal(8, "Dark2"))
  })
   
  # word cloud for terrorist group
  output$killer <- renderPlot({
    # Filter out Unknown group, as it is not informative
    temp_df <- data2() %>% filter (gname != "Unknown") %>% 
    # group and count the frequency
    group_by(gname) %>% summarise(atkker = n())
    temp_df <- temp_df[order(temp_df$atkker,decreasing = T),]
    # Threshold of drawing is 2 again.
    shiny::validate(
      need(nrow(temp_df) > 2, "Need more data to plot a word cloud!")
    )
    wordcloud(words=temp_df$gname,freq = temp_df$atkker, max.words = 180, min.freq = 10, 
              random.order=FALSE, rot.per=0.35, colors=brewer.pal(8, "Dark2"))
  })
})
# Run App
shinyApp(ui=ui,server=server)


