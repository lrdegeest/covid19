# about -------------------------------------------------------------------
# live updates of covid-19 cases from john hopkins
# author: @lrdegeest

# LIBRARIES ---------------------------------------------------------------
library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(data.table)
library(ggrepel)
library(urbnmapr)
library(cowplot)
library(RCurl)


# TIME SERIES DATA SETUP --------------------------------------------------
# get the filenames from github
dead_data_path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
confirmed_data_path = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"

# reshape data function
reshape_data = function(d,name) {
  date_cols = names(d)[grepl('X', names(d))]
  d.long = 
    d %>% 
    gather_(key="date_raw", value = name, date_cols) %>% 
    mutate(date = gsub("X", "", date_raw)) %>% 
    mutate(date = as.Date(date, format = "%m.%d.%y")) %>% 
    arrange(Country.Region)
  return(d.long)
}
# names to pass to reshape data function
names = c("Confirmed", "Dead")

# plot theme
time_series_theme = 
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "#060606",
                                    colour = "#060606",
                                    size = 0.5, linetype = "solid"),
    plot.background = element_rect(fill = "#060606"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "grey20"), 
    panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                    colour = "grey20")
  ) 

# USA MAP DATA SETUP ------------------------------------------------------
# load territory+county map data
territories_counties = urbnmapr::get_urbn_map("territories_counties", sf=TRUE)
# clean it
territories_counties = territories_counties %>% 
  arrange(state_name, county_fips) %>% 
  mutate(county_fips = as.numeric(county_fips)) %>% 
  select(county_fips)
# no mapfiles for these guys:
no_map_files = c("Wuhan Evacuee", "Puerto Rico", "American Samoa", "Diamond Princess", "Grand Princess", "Guam", "Northern Mariana Islands", "Recovered", "Virgin Islands")

# now just state maps
states_sf = urbnmapr::get_urbn_map("states", sf=TRUE)

# USER INTERFACE ----------------------------------------------------------
ui <- navbarPage("COVID-19 Updates",
                 theme = shinytheme("cyborg"),
                 tabPanel("Global",
                          sidebarLayout(
                            sidebarPanel(
                              tags$style(".well {background-color:#060606;}"),
                              uiOutput("user_global_choice"), width = 2),
                            mainPanel(
                              br(),br(),
                              plotOutput("global_plot", width = "100%"))
                          ),
                 ),
                 tabPanel("United States",
                          sidebarLayout(
                            sidebarPanel(
                              tags$style("
                              .well {background-color:#060606;}
                              "),
                              uiOutput("user_state_choice", width = 1),
                              plotOutput("map_plot")
                            ),
                            
                            mainPanel(
                              br(),br(),
                              DT::dataTableOutput("state_county_table", width = "80%"))
                          ),
                 ),
                 tabPanel("About",
                          uiOutput("about")
                 )
)

# SERVER ------------------------------------------------------------------
server <- function(input, output, server) {
  
  ## 60*60*12*1000 = 12hr updates
  update_frequency = 60*60*12*1000
  
  
  # TIME SERIES DATA + PLOT -------------------------------------------------
  
  ## confirmed cases
  df.confirmed.loaded = reactiveFileReader(intervalMillis = update_frequency, 
                                           session = NULL,
                                           filePath = confirmed_data_path,
                                           readFunc = read.csv)
  ## deaths
  df.dead.loaded = reactiveFileReader(intervalMillis = update_frequency, 
                                      session = NULL,
                                      filePath = dead_data_path,
                                      readFunc = read.csv)
  # set-up the plot data
  plot_data = reactive({
    # reshape the data
    data = list(df.confirmed.loaded(), df.dead.loaded())    
    long_data = vector(mode = "list", length = 2)
    for(i in 1:length(names)) {
      long_data[[i]] = reshape_data(data[[i]], names[[i]])
    }
    # merge that data
    d = long_data[[1]]
    d$Dead = long_data[[2]]$Dead
    # re-gather that data
    d.long = d %>% 
      gather(variable, value, c(Confirmed,Dead)) %>% 
      group_by(date, Country.Region)
    # get the latest date
    latest_date = max(d.long$date)
    # labels for plot
    d.long = d.long %>% 
      mutate(label = ifelse(date == latest_date, as.character(variable), NA_character_))
    return(d.long)
  }) # end plot data
  
  # filter the plot data based on user input
  filter_plot_data = reactive({
    req(input$user_global_choice)
    req(input$plot_type)
    # filter the data
    if(input$plot_type == "Total cases") {
      if(input$user_global_choice == "Global") {
        filtered_data = plot_data() %>% 
          group_by(date, variable, label) %>% 
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          mutate(max_value = ifelse(date == max(plot_data()$date), paste(variable,scales::comma(value), sep=": "), NA_character_))
      } else {
        filtered_data = plot_data() %>% 
          filter(Country.Region == input$user_global_choice) %>% 
          group_by(date, variable, input$user_global_choice) %>% 
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          mutate(max_value = ifelse(date == max(plot_data()$date), paste(variable,scales::comma(value), sep=": "), NA_character_))
      }
    } else {
      if(input$user_global_choice == "Global") {
        filtered_data = plot_data() %>% 
          group_by(date, variable, label) %>% 
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          group_by(variable) %>% 
          arrange(date, .by_group = TRUE) %>% 
          mutate(value = value - lag(value)) %>% 
          mutate(max_value = ifelse(date == max(plot_data()$date), paste(variable,scales::comma(value), sep=": "), NA_character_))
      } else {
        filtered_data = plot_data() %>% 
          filter(Country.Region == input$user_global_choice) %>% 
          group_by(date, variable, input$user_global_choice) %>% 
          summarise(value = sum(value, na.rm=TRUE)) %>% 
          group_by(variable) %>% 
          arrange(date, .by_group = TRUE) %>% 
          mutate(value = value - lag(value)) %>% 
          mutate(max_value = ifelse(date == max(plot_data()$date), paste(variable,scales::comma(value), sep=": "), NA_character_))
      }
      
    }
    return(filtered_data)
  }) # end plot data filter
  
  # BEGIN GLOBAL/COUNTRY PLOT(S)
  output$global_plot <- renderPlot({
    p1 = filter_plot_data() %>% 
      filter(variable == "Confirmed") %>% 
      ggplot(., aes(date, value, group=variable,color=variable)) + 
        geom_point(size=2, alpha=0.6) +
        geom_line() +
        time_series_theme + 
        scale_color_manual(values=c("#FF4136")) + 
        scale_fill_manual(values=c("#FF4136")) +
        labs(x="",y="") + 
        scale_y_continuous(labels=scales::comma) +
        scale_x_date(date_breaks = "1 week", date_labels ="%b %d") +
        theme(text = element_text(size=18), legend.position = "none") +
        theme(axis.text.x=element_text(angle=45,vjust=.8, hjust=0.8, color="white")) + 
        theme(axis.text.y=element_text(color="white")) + 
        ggrepel::geom_label_repel(aes(label = max_value,fill=variable),
                                  color="white",size=5,fontface="bold", na.rm = T,
                                  nudge_x = 10,
                                  segment.size  = 0.2,
                                  segment.color = "grey50",
                                  direction     = "y")
    p2 = filter_plot_data() %>% 
      filter(variable == "Dead") %>% 
      ggplot(., aes(date, value, group=variable,color=variable)) + 
      geom_point(size=2, alpha=0.6) +
      geom_line() +
      time_series_theme + 
      scale_color_manual(values=c("#85144b")) + 
      scale_fill_manual(values=c("#85144b")) +
      labs(x="",y="") + 
      scale_y_continuous(labels=scales::comma) +
      scale_x_date(date_breaks = "1 week", date_labels ="%b %d") +
      theme(text = element_text(size=18), legend.position = "none") +
      theme(axis.text.x=element_text(angle=45,vjust=.8, hjust=0.8, color="white")) + 
      theme(axis.text.y=element_text(color="white", angle=0)) + 
      ggrepel::geom_label_repel(aes(label = max_value,fill=variable),
                                color="white",size=5,fontface="bold", na.rm = T,
                                nudge_x = 10,
                                segment.size  = 0.2,
                                segment.color = "grey50",
                                direction     = "y")
    cowplot::plot_grid(p1, p2, ncol=2)
  }) # END GLOBAL/COUNTRY PLOT(S)
  # END TIME SERIES DATA + PLOT
  
  
  # USA MAP DATA + PLOT -----------------------------------------------------
  
  # BEGIN LOAD DATA
  df_usa = reactive({
    # first get the correct file path
    file_paths = list(
      paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", format(Sys.Date(), "%m-%d-%Y"), ".csv", sep=""),
      paste("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_daily_reports/", format(Sys.Date()-1, "%m-%d-%Y"), ".csv", sep="")
    )
    f = ifelse(url.exists(file_paths[[1]]) == TRUE, 
               file_paths[[1]],
               file_paths[[2]])
    # now load
    df = reactiveFileReader(intervalMillis = update_frequency, 
                            session = NULL,
                            filePath = f, # note: no parentheses
                            readFunc = read.csv)
    # filter and clean
    df = df() %>% 
      filter(Country_Region == "US") %>% 
      droplevels() %>% 
      arrange(Province_State, FIPS) %>% 
      rename("county_fips" = FIPS, "county_name" = Admin2, "state_name" = Province_State) %>% 
      mutate("county_fips" = as.numeric(county_fips)) %>% # FIPS must be numeric for left_join to work
      select(county_fips, state_name, county_name, Confirmed, Deaths, Last_Update)
    return(df)
  }) # END LOAD DATA
  
  # BEGIN MAP DATA
  map_data = reactive({
    map_data = left_join(df_usa(), territories_counties, by="county_fips")
    return(map_data)
  })
  
  dt = reactive({
    dt = data.table(df_usa())
    return(dt)
  })
  
  # BEGIN USA/STATE SUMMARY TABLES
  output$state_county_table = DT::renderDataTable(({
    req(input$user_state_choice)
    if(input$user_state_choice == "All States") {
      d = df_usa() %>% 
        select(state_name, Confirmed, Deaths) %>% 
        group_by(state_name) %>% 
        gather(key = "variable", value, -c(state_name)) %>% 
        group_by(state_name, variable) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        spread(variable, value) %>% 
        select(state_name, Confirmed, Deaths) %>% 
        rename("State/Territory" = state_name) %>% 
        arrange(desc(Confirmed))
      d = DT::datatable(d,rownames= FALSE, 
                        options = list(
                          columnDefs = list(list(className = 'dt-center', targets = "_all")))
      )
      d = d %>% 
        DT::formatStyle(1:4,  color = 'white', backgroundColor = '#202020') %>% 
        DT::formatCurrency('Confirmed', "",
                           interval = 3, mark = ',', before = FALSE, digits = 0)
      return(d)
    } else {
      d = df_usa() %>%
        select(state_name, county_name, Confirmed, Deaths) %>% 
        filter(state_name == input$user_state_choice) %>% 
        droplevels() %>% 
        group_by(county_name) %>% 
        gather(key = "variable", value, -c(state_name, county_name)) %>% 
        group_by(state_name, county_name, variable) %>% 
        summarise(value = sum(value, na.rm = TRUE)) %>% 
        spread(variable, value) %>% 
        select(state_name, county_name, Confirmed, Deaths) %>% 
        rename("State" = state_name, "County" = county_name) %>% 
        arrange(desc(Confirmed))
      d = DT::datatable(d,rownames= FALSE, 
                        options = list(
                          columnDefs = list(list(className = 'dt-center', targets = "_all")))
      )
      d = d %>% 
        DT::formatStyle(1:5,  color = 'white', backgroundColor = '#202020') %>% 
        DT::formatCurrency('Confirmed', "",
                           interval = 3, mark = ',', before = FALSE, digits = 0)
      #colors: https://www.colorhexa.com/a9a9a9
      return(d)
    }
  })) # END USA/STATE SUMMARY TABLES
  
  # BEGIN USA/STATE MAPS
  output$map_plot = renderPlot({
    req(map_data())
    req(input$user_state_choice)
    if(input$user_state_choice %in% no_map_files) {
      # hack: empty plot to fill the background
      ggplot() + 
        theme_void() + 
        theme(plot.background = element_rect(fill = "#060606")) 
    } else if(input$user_state_choice == "All States"){
      ## MAP
      df_states_summary = df_usa() %>% 
        group_by(state_name, Last_Update) %>% 
        summarise(Confirmed = sum(Confirmed), Dead = sum(Deaths))
      map_data = left_join(df_states_summary, states_sf, by = "state_name")
      map_plot = map_data %>% 
        ggplot(aes(geometry = geometry)) +
        geom_sf(mapping = aes(fill = Confirmed),
                color = "#3D9970", size = 0.05) +
        coord_sf(datum = NA) + 
        scale_fill_gradient(low = "gray", high = "#FF4136", na.value = NA) + 
        theme_void() + 
        labs(title = paste0(scales::comma(sum(map_data$Confirmed)), " Confirmed"),
             subtitle = paste0(scales::comma(sum(map_data$Dead))," Dead\n"))+
        theme(plot.title = element_text(colour = "#FF4136", size=20, face="bold"),
              plot.subtitle = element_text(colour = "#85144b", size=20, face="bold")) + 
        theme(plot.background = element_rect(fill = "#060606")) + 
        theme(legend.text = element_text(colour="white", size = 10, face = "bold", angle=30, vjust=0.75),
              legend.title = element_blank(),
              legend.key.width=unit(1,"cm")) + 
        theme(legend.position="bottom")
      cowplot::plot_grid(map_plot,ncol=1) + 
        theme(plot.background = element_rect(fill = "#060606"))
    } else {
      state_map_data = map_data() %>% 
        filter(state_name == input$user_state_choice) 
      map_plot = state_map_data %>%
        ggplot(aes(geometry = geometry)) +
        geom_sf(mapping = aes(fill = Confirmed),
                color = "#3D9970", size = 0.05) +
        coord_sf(datum = NA) + 
        scale_fill_gradient(low = "gray", high = "#FF4136", na.value = NA) + 
        theme_void() + 
        labs(title = paste0(scales::comma(sum(state_map_data$Confirmed)), " Confirmed"), 
        subtitle = paste0(scales::comma(sum(state_map_data$Deaths)), " Dead")
        ) +
        theme(plot.title = element_text(colour = "#FF4136", size=20),
              plot.subtitle = element_text(colour = "#85144b", size=20, face="bold")
              ) + 
        theme(plot.background = element_rect(fill = "#060606")) + 
        theme(legend.text = element_text(colour="white", size = 10, face = "bold", angle=0, vjust=0.75),
              legend.key.width=unit(1,"cm")) + 
        theme(legend.position="bottom")
      cowplot::plot_grid(map_plot,ncol=1) + 
        theme(plot.background = element_rect(fill = "#060606"))
    }
  }) # END USA/STATE MAPS
  # USA MAP DATA + PLOT
  
  # USER INTERFACES ---------------------------------------------------------
  
  # BEGIN the GLOBAL UI drop-down menu
  output$user_global_choice <- renderUI({
    choices = c("Global",unique(as.character(unique(plot_data()$Country.Region))))
    names(choices) = c("Global",unique(as.character(unique(plot_data()$Country.Region))))
    fluidRow(
      p("Last updated on", a(href="https://github.com/CSSEGISandData/COVID-19/commits/master/csse_covid_19_data", max(plot_data()$date))),
      selectInput("user_global_choice", 
                  label = "",
                  choices = choices,
                  selected="Global"),
      selectInput("plot_type", 
                  label = "",
                  choices = c("Total cases", "New cases"),
                  selected="Total cases")
    )
  }) # end GLOBAL UI drop down menus
  
  # BEGIN USA UI
  output$user_state_choice <- renderUI({
    state_choices = c("All States", unique(as.character(unique(df_usa()$state_name))))
    names(state_choices) = c("All States" , unique(as.character(df_usa()$state_name)))
    fluidRow(
      p("Last updated on", a(href="https://github.com/CSSEGISandData/COVID-19/commits/master/csse_covid_19_data", max(plot_data()$date))),
      selectInput("user_state_choice", 
                  label = "",
                  choices = state_choices,
                  selected="All States")
    )
  }) # end USA UI
  
  # BEGIN ABOUT UI
  output$about <- renderUI({
    fluidRow(
      column(12,
             br(),
             p("Daily data from", a(href = 'https://github.com/CSSEGISandData/COVID-19/tree/master/csse_covid_19_data', 'Johns Hopkins CSSEGIS')),
             p("Note: confirmed cases vary due to testing procedures and other factors.", 
              a(href = "https://www.cdc.gov/coronavirus/2019-ncov/", "Check the CDC for more info.")),
             p("Last updated on", a(href="https://github.com/CSSEGISandData/COVID-19/commits/master/csse_covid_19_data", max(plot_data()$date))),
             p("App by", a(href = 'https://lrdegeest.github.io/', 'Lawrence De Geest')),
             p("Code on", a(href = 'https://github.com/lrdegeest/covid19', 'Github.'), "MIT License. Feel free to clone/fork and modify.")
      ),
    )
  }) # end UI about
  # END USER INTERFACES
  
} # end server


# RUN THE APP -------------------------------------------------------------
shinyApp(ui = ui, server = server)
