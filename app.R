library(shiny)
library(dplyr)
library(bslib)
library(jsonlite)
library(lubridate)

# --- MASTER APP BRANDING & CSS ---
# Deep Jets Navy background with subtle glassmorphism and the signature Red accent
app_theme <- bs_theme(
  version = 5,
  bg = "#0A0A0C", fg = "#FFFFFF",
  base_font = font_collection("system-ui", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "sans-serif")
) %>%
  bs_add_rules("
    body { 
      background: radial-gradient(circle at top, #01183F 0%, #0A0A0C 60%); 
      background-attachment: fixed; 
      font-feature-settings: 'tnum'; /* Forces numbers to perfectly align vertically */
    }
    
    /* The Master Title */
    .master-title {
      font-weight: 900; 
      font-size: 1.8rem; 
      letter-spacing: 6px; 
      color: #FFFFFF; 
      margin-bottom: 30px; 
      text-transform: uppercase; 
      text-shadow: 0 4px 20px rgba(77, 184, 255, 0.4);
    }
    
    /* Perfected Glassmorphism & Uniform Padding */
    .apple-card { 
      background: rgba(255, 255, 255, 0.03); 
      backdrop-filter: blur(20px); 
      -webkit-backdrop-filter: blur(20px); 
      border: 1px solid rgba(255, 255, 255, 0.1); 
      border-radius: 28px; 
      padding: 24px; 
      box-shadow: 0 10px 30px rgba(0, 0, 0, 0.4); 
      height: 100%; 
      display: flex;
      flex-direction: column;
      justify-content: space-between; 
      transition: transform 0.3s ease, box-shadow 0.3s ease;
    }
    
    /* Native App Hover Physics */
    .apple-card:hover {
      transform: translateY(-4px);
      box-shadow: 0 15px 40px rgba(0, 0, 0, 0.6);
    }
    
    .card-title { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 2px; color: #8F9191; font-weight: 700; margin-bottom: 24px; text-align: center; }
    
    /* The Classic Red Drop Shadow VS */
    .vs-text { 
      font-size: 3.5rem; 
      font-weight: 900; 
      color: #FFFFFF; 
      text-shadow: 4px 4px 0px #AD0E28; 
      line-height: 1;
      margin: 10px 0;
    }
    
    .game-date { font-size: 1.2rem; font-weight: 700; margin-top: 10px; letter-spacing: 1px;}
    .game-time { font-size: 0.95rem; color: #8F9191; font-weight: 600; }
    .tv-badge { background: #AD0E28; border-radius: 8px; padding: 4px 14px; font-size: 0.8rem; font-weight: 800; color: #fff; box-shadow: 0 4px 10px rgba(173, 14, 40, 0.4); display: inline-block; margin-top: 12px; }
    
    /* 3x2 Team Profile Bubbles */
    .profile-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; text-align: center; flex-grow: 1; align-items: center; }
    .profile-stat { background: rgba(0,0,0,0.25); border-radius: 12px; padding: 14px 6px; display: flex; flex-direction: column; justify-content: center; border: 1px solid rgba(255,255,255,0.03);}
    .profile-val { font-size: 1.15rem; font-weight: 800; color: #fff; }
    .profile-label { font-size: 0.65rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1px; margin-top: 4px; font-weight: 600;}
    
    /* Scorers */
    .scorer-row { display: flex; align-items: center; margin-bottom: 18px; }
    .scorer-row:last-child { margin-bottom: 0; }
    .scorer-img { width: 50px; height: 50px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.15); margin-right: 16px; background: #1c1c1e; }
    .scorer-info { flex-grow: 1; }
    .scorer-text { display: flex; justify-content: space-between; font-size: 1rem; font-weight: 700; margin-bottom: 8px; }
    .scorer-pts { color: #8F9191; font-weight: 600; }
    .progress-track { height: 6px; background: rgba(255, 255, 255, 0.05); border-radius: 4px; overflow: hidden; }
    .progress-fill { height: 100%; border-radius: 4px; transition: width 1s ease-in-out; }
    .home-gradient { background: linear-gradient(90deg, #01183F, #4DB8FF); }
    .away-gradient { background: linear-gradient(90deg, #8F9191, #FFFFFF); }
    
    /* Goalies */
    .goalie-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 15px; text-align: center; margin-top: 20px;}
    .stat-box { background: rgba(0,0,0,0.25); border-radius: 16px; padding: 16px; border: 1px solid rgba(255,255,255,0.03);}
    .stat-val { font-size: 1.9rem; font-weight: 800; color: #fff; }
    .stat-label { font-size: 0.75rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1.5px; font-weight: 700;}
  ")

ui <- fluidPage(
  theme = app_theme,
  tags$head(tags$meta(name="viewport", content="width=device-width, initial-scale=1.0")),
  
  div(style = "max-width: 900px; margin: 40px auto; display: flex; flex-direction: column; gap: 24px;",
      
      # --- THE MASTER HEADER ---
      div(class = "text-center master-title", "✈️ NOW BOARDING: THE JETS"),
      
      # --- ROW 1: THE MATCHUP ---
      div(class = "apple-card text-center",
          div(class = "card-title", "Flight Details"),
          uiOutput("matchup_header_ui")
      ),
      
      # --- ROW 2: TEAM PROFILES (3x2 Grid) ---
      layout_columns(
        col_widths = c(6, 6),
        gap = "24px",
        div(class = "apple-card",
            div(class = "card-title", textOutput("away_profile_title")),
            uiOutput("away_profile_ui")
        ),
        div(class = "apple-card",
            div(class = "card-title", textOutput("home_profile_title")),
            uiOutput("home_profile_ui")
        )
      ),
      
      # --- ROW 3: SCORERS ---
      layout_columns(
        col_widths = c(6, 6),
        gap = "24px",
        div(class = "apple-card",
            div(class = "card-title", textOutput("away_scorers_title")),
            uiOutput("away_scorers_ui")
        ),
        div(class = "apple-card",
            div(class = "card-title", textOutput("home_scorers_title")),
            uiOutput("home_scorers_ui")
        )
      ),
      
      # --- ROW 4: EXPECTED GOALIES ---
      div(class = "apple-card",
          div(class = "card-title", "Expected Starting Goaltenders"),
          layout_columns(
            col_widths = c(6, 6),
            gap = "24px",
            uiOutput("away_goalie_ui"),
            uiOutput("home_goalie_ui")
          )
      )
  )
)

server <- function(input, output, session) {
  
  # --- 1. FETCH LIVE SCHEDULE ---
  next_game <- reactive({
    url <- "https://api-web.nhle.com/v1/club-schedule-season/WPG/now"
    sched <- jsonlite::fromJSON(url)$games
    
    sched %>%
      filter(as.Date(gameDate) >= Sys.Date()) %>%
      arrange(gameDate) %>%
      slice(1)
  })
  
  # --- 2. FETCH STANDINGS ---
  team_standings <- reactive({
    url <- "https://api-web.nhle.com/v1/standings/now"
    jsonlite::fromJSON(url)$standings
  })
  
  # --- 3. FETCH ROSTER STATS ---
  get_team_stats <- function(team_abbr) {
    url <- paste0("https://api-web.nhle.com/v1/club-stats/", team_abbr, "/now")
    stats <- jsonlite::fromJSON(url)
    
    top_skaters <- stats$skaters %>%
      arrange(desc(points)) %>%
      head(4) %>%
      mutate(
        full_name = paste(firstName$default, lastName$default),
        photo = paste0("https://assets.nhle.com/mugs/nhl/latest/", playerId, ".png")
      )
    
    top_goalie <- stats$goalies %>%
      arrange(desc(gamesPlayed)) %>%
      head(1) %>%
      mutate(
        full_name = paste(firstName$default, lastName$default),
        photo = paste0("https://assets.nhle.com/mugs/nhl/latest/", playerId, ".png")
      )
    
    list(skaters = top_skaters, goalie = top_goalie)
  }
  
  game_data <- reactive({
    req(next_game(), team_standings())
    game <- next_game()
    home_abbr <- game$homeTeam$abbrev
    away_abbr <- game$awayTeam$abbrev
    
    list(
      home_stats = get_team_stats(home_abbr),
      away_stats = get_team_stats(away_abbr),
      standings = team_standings(),
      info = game
    )
  })
  
  # --- UI: MATCHUP HEADER ---
  output$matchup_header_ui <- renderUI({
    req(game_data())
    game <- game_data()$info
    
    utc_time <- as.POSIXct(game$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
    wpg_time <- with_tz(utc_time, tzone = "America/Winnipeg")
    
    game_time <- format(wpg_time, "%I:%M %p %Z")
    game_date <- toupper(format(wpg_time, "%A, %B %d"))
    
    tv_networks <- "TBA"
    if ("tvBroadcasts" %in% names(game) && is.data.frame(game$tvBroadcasts[[1]])) {
      tv_networks <- paste(game$tvBroadcasts[[1]]$network, collapse = " | ")
    }
    
    layout_columns(
      col_widths = c(4, 4, 4),
      class = "align-items-center",
      div(
        tags$img(src = game$awayTeam$logo, style = "width: 110px; filter: drop-shadow(0 0 15px rgba(255, 255, 255, 0.15));"),
        div(game$awayTeam$abbrev, style = "margin-top: 15px; font-weight: 800; letter-spacing: 1px; font-size: 1.2rem;")
      ),
      div(
        div("VS", class = "vs-text"),
        div(game_date, class = "game-date"),
        div(game_time, class = "game-time"),
        div(tv_networks, class = "tv-badge")
      ),
      div(
        tags$img(src = game$homeTeam$logo, style = "width: 110px; filter: drop-shadow(0 0 15px rgba(77, 184, 255, 0.25));"),
        div(game$homeTeam$abbrev, style = "margin-top: 15px; font-weight: 800; letter-spacing: 1px; font-size: 1.2rem;")
      )
    )
  })
  
  # --- UI: TEAM PROFILES ---
  build_profile_html <- function(team_abbr, standings_df) {
    team_info <- standings_df[standings_df$teamAbbrev$default == team_abbr, ]
    if(nrow(team_info) == 0) return(div("Data Unavailable"))
    
    record <- paste0(team_info$wins, "-", team_info$losses, "-", team_info$otLosses)
    l10 <- paste0(team_info$l10Wins, "-", team_info$l10Losses, "-", team_info$l10OtLosses)
    
    gp <- team_info$gamesPlayed
    if(is.null(gp)) gp <- team_info$wins + team_info$losses + team_info$otLosses
    pt_pct <- paste0(round((team_info$points / (gp * 2)) * 100, 1), "%")
    
    row_val <- "N/A"
    if("regulationPlusOtWins" %in% names(team_info)) row_val <- team_info$regulationPlusOtWins
    else if("rowWins" %in% names(team_info)) row_val <- team_info$rowWins
    
    gd <- team_info$goalDifferential
    if(is.numeric(gd) && gd > 0) gd <- paste0("+", gd)
    
    HTML(paste0('
      <div class="profile-grid">
        <div class="profile-stat"><div class="profile-val">', record, '</div><div class="profile-label">Record</div></div>
        <div class="profile-stat"><div class="profile-val">', team_info$points, '</div><div class="profile-label">Points</div></div>
        <div class="profile-stat"><div class="profile-val">', pt_pct, '</div><div class="profile-label">Point %</div></div>
        <div class="profile-stat"><div class="profile-val">', l10, '</div><div class="profile-label">Last 10</div></div>
        <div class="profile-stat"><div class="profile-val">', row_val, '</div><div class="profile-label">ROW</div></div>
        <div class="profile-stat"><div class="profile-val">', gd, '</div><div class="profile-label">Goal Diff</div></div>
      </div>
    '))
  }
  
  output$away_profile_title <- renderText({ req(game_data()); paste(game_data()$info$awayTeam$abbrev, "Profile") })
  output$home_profile_title <- renderText({ req(game_data()); paste(game_data()$info$homeTeam$abbrev, "Profile") })
  output$away_profile_ui <- renderUI({ req(game_data()); build_profile_html(game_data()$info$awayTeam$abbrev, game_data()$standings) })
  output$home_profile_ui <- renderUI({ req(game_data()); build_profile_html(game_data()$info$homeTeam$abbrev, game_data()$standings) })
  
  # --- UI: SCORERS ---
  output$away_scorers_title <- renderText({ req(game_data()); paste(game_data()$info$awayTeam$abbrev, "Scoring Leaders") })
  output$home_scorers_title <- renderText({ req(game_data()); paste(game_data()$info$homeTeam$abbrev, "Scoring Leaders") })
  
  build_skater_html <- function(skaters_df, gradient_class) {
    max_pts <- max(skaters_df$points, na.rm = TRUE)
    if(max_pts == 0) max_pts <- 1
    
    HTML(paste0(
      lapply(1:nrow(skaters_df), function(i) {
        row <- skaters_df[i, ]
        pct <- (row$points / max_pts) * 100 
        safe_name <- gsub("[^A-Za-z0-9 ]", "", row$full_name)
        fallback_img <- paste0("https://ui-avatars.com/api/?name=", URLencode(safe_name), "&background=1c1c1e&color=8F9191")
        
        paste0('
          <div class="scorer-row">
            <img class="scorer-img" src="', row$photo, '" onerror="this.onerror=null; this.src=\'', fallback_img, '\';">
            <div class="scorer-info">
              <div class="scorer-text">
                <span>', row$full_name, '</span>
                <span class="scorer-pts">', row$points, ' PTS</span>
              </div>
              <div class="progress-track">
                <div class="progress-fill ', gradient_class, '" style="width: ', pct, '%;"></div>
              </div>
            </div>
          </div>
        ')
      }), collapse = ""
    ))
  }
  
  output$away_scorers_ui <- renderUI({ req(game_data()); build_skater_html(game_data()$away_stats$skaters, "away-gradient") })
  output$home_scorers_ui <- renderUI({ req(game_data()); build_skater_html(game_data()$home_stats$skaters, "home-gradient") })
  
  # --- UI: GOALIES ---
  build_goalie_html <- function(goalie_df) {
    if(nrow(goalie_df) == 0) return(div("No Goalie Data"))
    row <- goalie_df[1, ]
    safe_name <- gsub("[^A-Za-z0-9 ]", "", row$full_name)
    fallback_img <- paste0("https://ui-avatars.com/api/?name=", URLencode(safe_name), "&background=1c1c1e&color=8F9191")
    
    div(class = "text-center d-flex flex-column h-100",
        div(
          HTML(paste0('<img src="', row$photo, '" onerror="this.onerror=null; this.src=\'', fallback_img, '\';" style="width:85px; height:85px; border-radius:50%; margin-bottom:15px; background:#1c1c1e; border: 2px solid rgba(255,255,255,0.2); box-shadow: 0 4px 15px rgba(0,0,0,0.3);">')),
          div(row$full_name, style="font-weight:800; font-size: 1.15rem; margin-bottom:10px; letter-spacing: 0.5px;")
        ),
        div(class = "goalie-grid",
            div(class = "stat-box", div(class = "stat-val", sprintf("%.2f", row$goalsAgainstAverage)), div(class = "stat-label", "GAA")),
            div(class = "stat-box", div(class = "stat-val", sprintf("%.3f", row$savePercentage)), div(class = "stat-label", "SV%"))
        )
    )
  }
  
  output$away_goalie_ui <- renderUI({ req(game_data()); build_goalie_html(game_data()$away_stats$goalie) })
  output$home_goalie_ui <- renderUI({ req(game_data()); build_goalie_html(game_data()$home_stats$goalie) })
}

shinyApp(ui, server)



