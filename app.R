library(shiny)
library(shinydashboard)
library(tidyverse)
library(nflverse)
library(hoopR)
library(baseballr)
library(mlbplotR)
library(hockeyR)
library(fastRhockey)
library(worldfootballR)
library(dplyr)
library(tmaptools)
library(fuzzyjoin)
library(gt)
library(gtExtras)

ui <- fluidPage(
  title = "American Fan Analysis",
  tags$head(
    tags$style('
    ul.nav-tabs{
      display: flex !important;
      justify-content: center !important;
    }')
  ),
  titlePanel(
    div(
      HTML("What <b>American</b> Teams Should You Support?"),
      align = "center"
    )
  ),
  sidebarLayout(
    sidebarPanel(
      textInput("loc_input", "Enter Location (Preferably CITY and STATE)", value = ""),
      actionButton("go_button", "SUBMIT"),
      div(style = "margin-top: 15px"),
      wellPanel(
        strong(helpText("Specific addresses may not work properly but sometimes you can just put the number and street name and it will work. Using city and state is easier for a general idea of proximity!")),
      ),
      wellPanel(
        strong(helpText("No input data is recorded!")),
      ),
      hr(),  # Add a horizontal line for separation
      h4("SOCIALS (Amrit Vignesh):", style = "font-size: 18px;"),
      tags$a(href = "https://linkedin.com/in/amritvignesh/", "LinkedIn", style = "font-size: 16px;"),
      br(),  # Add a line break for better spacing
      tags$a(href = "https://twitter.com/avsportsanalyst", "X or Twitter", style = "font-size: 16px;"),
      br(),
      tags$a(href = "https://medium.com/@amritvignesh", "Medium", style = "font-size: 16px;"),
      br(),
      tags$a(href = "https://github.com/amritvignesh", "GitHub", style = "font-size: 16px;")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("NFL", tableOutput("nfl")),
        tabPanel("NBA", tableOutput("nba")),
        tabPanel("MLB", tableOutput("mlb")),
        tabPanel("NHL", tableOutput("nhl")),
        tabPanel("MLS", tableOutput("mls")),
        tabPanel(
          "Fan Score",
          fluidPage(
            titlePanel("How True of a Fan Are You?"),
            sidebarLayout(
              sidebarPanel(
                p(strong("Select your favorite teams to get a fan score!")),
                selectInput("nflFav", "NFL:", choices = c("None" = "None"), selected = NULL),
                selectInput("nbaFav", "NBA", choices = c("None" = "None"), selected = NULL),
                selectInput("mlbFav", "MLB", choices = c("None" = "None"), selected = NULL),
                selectInput("nhlFav", "NHL", choices = c("None" = "None"), selected = NULL),
                selectInput("mlsFav", "MLS", choices = c("None" = "None"), selected = NULL),
                actionButton("create_score", "CREATE"),
              ),
              mainPanel(
                tableOutput("fanScoreTable"),
                h1(textOutput("fanScore"), style = "text-align: center; margin-top: 50px;"),
              )
            )
          )
        )
      )
    )
  )
)

calculate_dist <- function(lat, lon, lat_input, lon_input) {
  distances <- approx_distances(c(lat, lon), c(lat_input, lon_input))
  return(distances)
}

# nfl_stadiums <- load_schedules(seasons = most_recent_season()) %>%
#   group_by(team = home_team) %>%
#   summarize(stadium = names(table(stadium))[which.max(table(stadium))]) 
# 
# nfl_stadium_coords <- geocode_OSM(nfl_stadiums$stadium, as.data.frame = TRUE) %>% select(stadium = query, lat, lon) %>% distinct(stadium, .keep_all = TRUE)
# 
# # change wrong ones and include ones not included
# 
# nfl_stadium_coords[which(nfl_stadium_coords$stadium == "AT&T Stadium"), c("lat", "lon")] <- c(32.747841, -97.093628)
# nfl_stadium_coords[which(nfl_stadium_coords$stadium == "M&T Bank Stadium"), c("lat", "lon")] <- c(39.278088, -76.623322)
# 
# nfl_stadium_coords <- rbind(nfl_stadium_coords, data.frame(stadium = "GEHA Field at Arrowhead Stadium", lat = 39.048786, lon = -94.484566), data.frame(stadium = "Mercedes-Benz Superdome", lat = 29.951439, lon = -90.081970))
# 
# nfl_stadiums <- left_join(nfl_stadiums, nfl_stadium_coords, by = "stadium")
# 
# write_csv(nfl_stadiums, "nfl_stadiums.csv")

nfl_data <- function(loc) {
  nfl_stadiums <- read_csv("nfl_stadiums.csv")
  
  nfl_stadiums <- nfl_stadiums %>%
    mutate(dist = mapply(calculate_dist, lat, lon, loc$coords[2], loc$coords[1]), support = 100 * min(dist)/dist) %>%
    arrange(dist)
  
  nfl_stadiums$support[which(is.nan(nfl_stadiums$support))] <- 100
  
  nfl_teams <- teams_colors_logos %>% select(team = team_abbr, name = team_name, logo = team_logo_espn)
  
  nfl_stadiums <- left_join(nfl_stadiums, nfl_teams, by = "team") %>% 
    mutate(support = round(support, 2)) %>%
    select(logo, name, support)
  
  return(nfl_stadiums)
}

# nba_stadiums <- load_nba_schedule(seasons = most_recent_nba_season()) %>%
#   group_by(team = home_abbreviation) %>%
#   summarize(stadium = names(table(venue_full_name))[which.max(table(venue_full_name))]) %>%
#   filter(team != "EAST")
# 
# nba_stadium_coords <- geocode_OSM(nba_stadiums$stadium, as.data.frame = TRUE) %>% select(stadium = query, lat, lon) %>% distinct(stadium, .keep_all = TRUE)
# 
# nba_stadiums <- left_join(nba_stadiums, nba_stadium_coords, by = "stadium")
# 
# write_csv(nba_stadiums, "nba_stadiums.csv")

nba_data <- function(loc) {
  nba_stadiums <- read_csv("nba_stadiums.csv")
  
  nba_stadiums <- nba_stadiums %>%
    mutate(dist = mapply(calculate_dist, lat, lon, loc$coords[2], loc$coords[1]), support = 100 * min(dist)/dist) %>%
    arrange(dist)
  
  nba_stadiums$support[which(is.nan(nba_stadiums$support))] <- 100
  
  nba_teams <- espn_nba_teams() %>% select(team = abbreviation, name = display_name, logo)
  
  nba_stadiums <- left_join(nba_stadiums, nba_teams, by = "team") %>% 
    mutate(support = round(support, 2)) %>%
    select(logo, name, support)
  
  return(nba_stadiums)
}

# mlb_stadiums <- mlb_schedule(season = most_recent_mlb_season(), level_ids = "1") %>%
#   group_by(team = teams_home_team_name) %>%
#   summarize(stadium = names(table(venue_name))[which.max(table(venue_name))]) %>%
#   filter(!(team %in% c("American League All-Stars", "Sugar Land Space Cowboys")))
# 
# mlb_stadium_coords <- geocode_OSM(mlb_stadiums$stadium, as.data.frame = TRUE) %>% select(stadium = query, lat, lon) %>% distinct(stadium, .keep_all = TRUE)
# 
# mlb_stadiums <- left_join(mlb_stadiums, mlb_stadium_coords, by = "stadium")
# 
# write_csv(mlb_stadiums, "mlb_stadiums.csv")

mlb_data <- function(loc) {
  mlb_stadiums <- read_csv("mlb_stadiums.csv")
  
  mlb_stadiums <- mlb_stadiums %>%
    mutate(dist = mapply(calculate_dist, lat, lon, loc$coords[2], loc$coords[1]), support = 100 * min(dist)/dist) %>%
    arrange(dist)
  
  mlb_stadiums$support[which(is.nan(mlb_stadiums$support))] <- 100
  
  mlb_teams <- load_mlb_teams() %>% select(team = team_name, logo = team_logo_espn)
  
  mlb_stadiums <- left_join(mlb_stadiums, mlb_teams, by = "team") %>% 
    mutate(support = round(support, 2)) %>%
    select(logo, name = team, support)
  
  return(mlb_stadiums)
}

# nhl_stadiums <- load_nhl_schedule(season = most_recent_nhl_season()) %>%
#   group_by(team = home_team_name) %>%
#   summarize(stadium = names(table(venue_name))[which.max(table(venue_name))]) 
# 
# nhl_stadium_coords <- geocode_OSM(nhl_stadiums$stadium, as.data.frame = TRUE) %>% select(stadium = query, lat, lon) %>% distinct(stadium, .keep_all = TRUE)
# 
# nhl_stadiums <- left_join(nhl_stadiums, nhl_stadium_coords, by = "stadium")
# 
# write_csv(nhl_stadiums, "nhl_stadiums.csv")

nhl_data <- function(loc) {
  nhl_stadiums <- read_csv("nhl_stadiums.csv")
  
  nhl_stadiums <- nhl_stadiums %>%
    mutate(dist = mapply(calculate_dist, lat, lon, loc$coords[2], loc$coords[1]), support = 100 * min(dist)/dist) %>%
    arrange(dist)
  
  nhl_stadiums$support[which(is.nan(nhl_stadiums$support))] <- 100
  
  nhl_teams <- espn_nhl_teams() %>% select(team = display_name, logo)
  
  nhl_teams$team[which(nhl_teams$team == "Montreal Canadiens")] <- "Montréal Canadiens"
  
  nhl_stadiums <- left_join(nhl_stadiums, nhl_teams, by = "team") %>% 
    mutate(support = round(support, 2)) %>%
    select(logo, name = team, support)
  
  return(nhl_stadiums)
}

# mls_stadiums <- load_match_results("USA", "M", 2024, "1st") %>%
#   group_by(team = Home) %>%
#   summarize(stadium = names(table(Venue))[which.max(table(Venue))])
# 
# mls_stadium_coords <- geocode_OSM(mls_stadiums$stadium, as.data.frame = TRUE) %>% select(stadium = query, lat, lon) %>% distinct(stadium, .keep_all = TRUE)
# 
# # change wrong ones 
# 
# mls_stadium_coords[which(mls_stadium_coords$stadium == "Q2 Stadium"), c("lat", "lon")] <- c(30.388545, -97.719313)
# mls_stadium_coords[which(mls_stadium_coords$stadium == "Toyota Stadium"), c("lat", "lon")] <- c(33.154320, -96.835426)
# mls_stadium_coords[which(mls_stadium_coords$stadium == "Citypark"), c("lat", "lon")] <- c(38.63016, -90.21058)
# mls_stadium_coords[which(mls_stadium_coords$stadium == "Red Bull Arena"), c("lat", "lon")] <- c(40.736851, -74.15036)
# 
# mls_stadiums <- left_join(mls_stadiums, mls_stadium_coords, by = "stadium")
# 
# write_csv(mls_stadiums, "mls_stadiums.csv")

mls_data <- function(loc) {
  mls_stadiums <- read_csv("mls_stadiums.csv")
  
  mls_stadiums <- mls_stadiums %>%
    mutate(dist = mapply(calculate_dist, lat, lon, loc$coords[2], loc$coords[1]), support = 100 * min(dist)/dist) %>%
    arrange(dist)
  
  mls_stadiums$support[which(is.nan(mls_stadiums$support))] <- 100
  
  mls_teams <- load_fotmob_match_details("USA", "MLS") %>%
    group_by(home_team) %>%
    summarize(id = first(home_team_id)) %>%
    mutate(logo = paste0("https://images.fotmob.com/image_resources/logo/teamlogo/", id, ".png")) %>%
    select(-id)
  
  mls_stadiums$team[which(mls_stadiums$team == "Dynamo FC")] <- "Houston Dynamo FC"
  
  mls_stadiums <- stringdist_left_join(mls_stadiums, mls_teams, by = c("team"="home_team"), method = "jw", max_dist = 1, distance_col = 'distance') %>% 
    group_by(team) %>%
    filter(distance == min(distance)) %>%
    mutate(support = round(support, 2)) %>%
    ungroup() %>%
    select(logo, name = home_team, support)
  
  return(mls_stadiums)
}

apply_gt <- function(df, league) {
  df %>% gt() %>% 
    gt_img_rows(columns = logo) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(logo, name, support)
    ) %>%
    gt_hulk_col_numeric(support) %>%
    cols_label(
      logo = md("**Logo**"),
      name = md("**Name**"),
      support = md("**Support**")
    ) %>%
    tab_header(
      title = add_text_img(url = league, "", 100, left = TRUE),
      subtitle = md("**Teams Ranked by Support Potential Based on Geographical Distance**")
    ) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = support
      )
    ) 
}


test_address <- "1600 Pennsylvania Avenue, Washington DC"
nfl_teams_select <- sort(unique(nfl_data(geocode_OSM(test_address))$name))
nba_teams_select <- sort(unique(nba_data(geocode_OSM(test_address))$name))
mlb_teams_select <- sort(unique(mlb_data(geocode_OSM(test_address))$name))
nhl_teams_select <- sort(unique(nhl_data(geocode_OSM(test_address))$name))
mls_teams_select <- sort(unique(mls_data(geocode_OSM(test_address))$name))

apply_gt_fan_score <- function(df, all_img) {
  df %>% gt() %>% 
    gt_img_rows(columns = league, height = 40) %>%
    gt_img_rows(columns = logo, height = 40) %>%
    gt_theme_538() %>%
    cols_align(
      align = "center",
      columns = c(league, logo, name, support)
    ) %>%
    gt_hulk_col_numeric(support) %>%
    cols_label(
      league = md("**League**"),
      logo = md("**Logo**"),
      name = md("**Name**"),
      support = md("**Support**")
    ) %>%
    tab_header(
      title = add_text_img(url = all_img, "", 100, left = TRUE),
      subtitle = md("**Favorite Teams With Support Potential Scores**")
    ) %>%
    opt_align_table_header(align = "center") %>%
    tab_style(
      style = list(
        cell_text(weight = "bold")
      ),
      locations = cells_body(
        columns = support
      )
    ) 
}



server <- function(input, output, session) {
  nfl_logo <- "https://upload.wikimedia.org/wikipedia/en/a/a2/National_Football_League_logo.svg"
  nba_logo <- "https://upload.wikimedia.org/wikipedia/en/0/03/National_Basketball_Association_logo.svg"
  mlb_logo <- "https://upload.wikimedia.org/wikipedia/commons/a/a6/Major_League_Baseball_logo.svg"
  nhl_logo <- "https://upload.wikimedia.org/wikipedia/en/3/3a/05_NHL_Shield.svg"
  mls_logo <- "https://upload.wikimedia.org/wikipedia/commons/7/76/MLS_crest_logo_RGB_gradient.svg"
  question <- "https://upload.wikimedia.org/wikipedia/commons/d/d2/Question_mark.svg"
  all_leagues <- "https://pbs.twimg.com/media/EhkbAchWkAAX3Wu?format=jpg&name=900x900"
  
  observe({
    updateSelectInput(session, "nflFav", choices = c("None" = "None", nfl_teams_select))
    updateSelectInput(session, "nbaFav", choices = c("None" = "None", nba_teams_select))
    updateSelectInput(session, "mlbFav", choices = c("None" = "None", mlb_teams_select))
    updateSelectInput(session, "nhlFav", choices = c("None" = "None", nhl_teams_select))
    updateSelectInput(session, "mlsFav", choices = c("None" = "None", mls_teams_select))
  })
  
  input_coords <- eventReactive(input$go_button, {
    geocode_OSM(input$loc_input)
  })
  
  nfl_stadiums <- reactive({
    loc <- input_coords()
    nfl_data(loc)
  })
  
  output$nfl <- render_gt(expr = apply_gt(nfl_stadiums(), nfl_logo))
  
  nba_stadiums <- reactive({
    loc <- input_coords()
    nba_data(loc)
  })
  
  output$nba <- render_gt(expr = apply_gt(nba_stadiums(), nba_logo))
  
  mlb_stadiums <- reactive({
    loc <- input_coords()
    mlb_data(loc)
  })
  
  output$mlb <- render_gt(expr = apply_gt(mlb_stadiums(), mlb_logo))
  
  nhl_stadiums <- reactive({
    loc <- input_coords()
    nhl_data(loc)
  })
  
  output$nhl <- render_gt(expr = apply_gt(nhl_stadiums(), nhl_logo))
  
  mls_stadiums <- reactive({
    loc <- input_coords()
    mls_data(loc)
  })
  
  output$mls <- render_gt(expr = apply_gt(mls_stadiums(), mls_logo))
  
  fav_teams <- eventReactive(input$create_score, {
    fav_nfl <- if (input$nflFav == "None") data.frame(logo = question, name = "None", support = NA) else nfl_stadiums() %>% filter(name %in% input$nflFav)
    fav_nba <- if (input$nbaFav == "None") data.frame(logo = question, name = "None", support = NA) else nba_stadiums() %>% filter(name %in% input$nbaFav)
    fav_mlb <- if (input$mlbFav == "None") data.frame(logo = question, name = "None", support = NA) else mlb_stadiums() %>% filter(name %in% input$mlbFav)
    fav_nhl <- if (input$nhlFav == "None") data.frame(logo = question, name = "None", support = NA) else nhl_stadiums() %>% filter(name %in% input$nhlFav)
    fav_mls <- if (input$mlsFav == "None") data.frame(logo = question, name = "None", support = NA) else mls_stadiums() %>% filter(name %in% input$mlsFav)
    
    fav_nfl <- fav_nfl %>% mutate(league = nfl_logo) %>% select(league, logo, name, support)
    fav_nba <- fav_nba %>% mutate(league = nba_logo) %>% select(league, logo, name, support)
    fav_mlb <- fav_mlb %>% mutate(league = mlb_logo) %>% select(league, logo, name, support)
    fav_nhl <- fav_nhl %>% mutate(league = nhl_logo) %>% select(league, logo, name, support)
    fav_mls <- fav_mls %>% mutate(league = mls_logo) %>% select(league, logo, name, support)
    
    bind_rows(fav_nfl, fav_nba, fav_mlb, fav_nhl, fav_mls)
  })
  
  fan_score <- eventReactive(input$create_score, {
    fav_teams() %>%
      filter(!is.na(support)) %>%
      summarise(fan_score = mean(support, na.rm = TRUE))
  })
  
  output$fanScoreTable <- render_gt(expr = apply_gt_fan_score(fav_teams(), all_leagues))
  
  output$fanScore <- renderText({
    sprintf("Fan Score: %.2f", fan_score()$fan_score)
  })
}

shinyApp(ui = ui, server = server)









