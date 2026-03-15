library(shiny)
library(dplyr)
library(bslib)
library(jsonlite)
library(lubridate)
library(purrr)

# --- MASTER APP BRANDING & CSS ---
app_theme <- bs_theme(
  version = 5,
  bg = "#0A0A0C", fg = "#FFFFFF",
  base_font = font_collection("system-ui", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "sans-serif")
) %>%
  bs_add_rules("
    body { background: radial-gradient(circle at top, #01183F 0%, #0A0A0C 60%); background-attachment: fixed; font-feature-settings: 'tnum'; }
    .master-title { font-weight: 900; font-size: 1.8rem; letter-spacing: 6px; color: #FFFFFF; margin-bottom: 30px; text-transform: uppercase; text-shadow: 0 4px 20px rgba(77, 184, 255, 0.4); }
    
    .apple-card { background: rgba(255, 255, 255, 0.03); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px); border: 1px solid rgba(255, 255, 255, 0.1); border-radius: 28px; padding: 24px; box-shadow: 0 10px 30px rgba(0, 0, 0, 0.4); height: 100%; display: flex; flex-direction: column; justify-content: space-between; transition: transform 0.3s ease, box-shadow 0.3s ease; }
    .apple-card:hover { transform: translateY(-4px); box-shadow: 0 15px 40px rgba(0, 0, 0, 0.6); }
    .card-title { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 2px; color: #8F9191; font-weight: 700; margin-bottom: 24px; text-align: center; }
    
    .vs-text { font-size: 3.5rem; font-weight: 900; color: #FFFFFF; text-shadow: 4px 4px 0px #AD0E28; line-height: 1; margin: 10px 0; }
    .game-date { font-size: 1.2rem; font-weight: 700; margin-top: 10px; letter-spacing: 1px;}
    .game-time { font-size: 0.95rem; color: #8F9191; font-weight: 600; }
    .tv-badge { background: #AD0E28; border-radius: 8px; padding: 4px 14px; font-size: 0.8rem; font-weight: 800; color: #fff; box-shadow: 0 4px 10px rgba(173, 14, 40, 0.4); display: inline-block; margin-top: 12px; }
    
    /* LIVE SCORE & SOG */
    .live-score { font-size: 4.5rem; font-weight: 900; color: #fff; text-shadow: 0px 0px 20px rgba(255,255,255,0.3); margin: 0 15px; }
    .live-badge { background: #ff2a2a; color: white; padding: 5px 12px; border-radius: 12px; font-weight: 800; font-size: 0.9rem; letter-spacing: 2px; animation: pulse 2s infinite; }
    .sog-text { font-size: 0.85rem; font-weight: 800; color: #8F9191; letter-spacing: 1px; }
    .sog-val { font-size: 1.1rem; font-weight: 900; color: #fff; }
    @keyframes pulse { 0% { box-shadow: 0 0 0 0 rgba(255, 42, 42, 0.7); } 70% { box-shadow: 0 0 0 10px rgba(255, 42, 42, 0); } 100% { box-shadow: 0 0 0 0 rgba(255, 42, 42, 0); } }
    
    /* SCORING LOG */
    .period-header { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 2px; color: #AD0E28; font-weight: 800; margin-bottom: 10px; border-bottom: 2px solid rgba(173,14,40,0.5); padding-bottom: 4px; margin-top: 15px; }
    .goal-row { display: flex; align-items: center; justify-content: space-between; padding: 12px 0; border-bottom: 1px solid rgba(255,255,255,0.05); }
    .goal-time { font-weight: 700; color: #8F9191; width: 45px; font-size: 0.9rem; }
    .goal-dot { width: 8px; height: 8px; border-radius: 50%; margin-right: 12px; }
    .goal-scorer { font-weight: 800; color: #fff; font-size: 1.05rem; }
    .goal-assists { font-size: 0.75rem; color: #8F9191; font-weight: 600; margin-top: 2px; }
    .goal-team { font-weight: 900; color: rgba(255,255,255,0.2); font-size: 1.2rem; }

    /* SPATIAL RINK MAP CSS WITH NETS & CREASES */
    .rink-container { position: relative; width: 100%; aspect-ratio: 2.35; background: #e8eaed; border-radius: 40px; border: 4px solid #8F9191; overflow: hidden; margin-top: 10px;}
    .rink-center-line { position: absolute; left: 50%; top: 0; bottom: 0; width: 4px; background: #AD0E28; transform: translateX(-50%); z-index: 1;}
    .rink-blue-line-left { position: absolute; left: 35%; top: 0; bottom: 0; width: 4px; background: #01183F; z-index: 1;}
    .rink-blue-line-right { position: absolute; right: 35%; top: 0; bottom: 0; width: 4px; background: #01183F; z-index: 1;}
    .rink-center-circle { position: absolute; left: 50%; top: 50%; width: 15%; aspect-ratio: 1; border: 4px solid #01183F; border-radius: 50%; transform: translate(-50%, -50%); z-index: 1;}
    
    .goal-line-left { position: absolute; left: 5.5%; top: 0; bottom: 0; width: 2px; background: #AD0E28; z-index: 1; }
    .goal-line-right { position: absolute; right: 5.5%; top: 0; bottom: 0; width: 2px; background: #AD0E28; z-index: 1; }
    .crease-left { position: absolute; left: 5.5%; top: 45%; width: 2.5%; height: 10%; background: rgba(77, 184, 255, 0.4); border: 2px solid #AD0E28; border-left: none; border-radius: 0 100px 100px 0; z-index: 0; }
    .crease-right { position: absolute; right: 5.5%; top: 45%; width: 2.5%; height: 10%; background: rgba(77, 184, 255, 0.4); border: 2px solid #AD0E28; border-right: none; border-radius: 100px 0 0 100px; z-index: 0; }
    
    .shot-dot { position: absolute; width: 10px; height: 10px; border-radius: 50%; transform: translate(-50%, -50%); z-index: 10; box-shadow: 0 0 5px rgba(0,0,0,0.5); opacity: 0.85;}
    .shot-goal { width: 18px; height: 18px; border: 2px solid #fff; z-index: 15; opacity: 1; animation: pulse 1.5s infinite;}
    
    /* DIVISION STANDINGS */
    .standings-row { display: grid; grid-template-columns: 2fr 1fr 1fr 1.5fr 1fr; padding: 12px 0; border-bottom: 1px solid rgba(255,255,255,0.05); align-items: center; font-size: 0.95rem;}
    .standings-header { font-weight: 800; color: #AD0E28; border-bottom: 2px solid rgba(173,14,40,0.5); padding-bottom: 8px;}
    .standings-team { font-weight: 800; color: #fff; display: flex; align-items: center; gap: 10px;}
    .standings-val { color: #8F9191; font-weight: 600; text-align: center;}

    /* PROFILES, SCORERS, GOALIES */
    .profile-grid { display: grid; grid-template-columns: repeat(3, 1fr); gap: 12px; text-align: center; flex-grow: 1; align-items: center; }
    .profile-stat { background: rgba(0,0,0,0.25); border-radius: 12px; padding: 14px 6px; border: 1px solid rgba(255,255,255,0.03);}
    .profile-val { font-size: 1.15rem; font-weight: 800; color: #fff; }
    .profile-label { font-size: 0.65rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1px; margin-top: 4px; font-weight: 600;}
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
    .goalie-row { display: flex; align-items: center; margin-bottom: 12px; padding: 12px; background: rgba(0,0,0,0.2); border-radius: 16px; border: 1px solid rgba(255,255,255,0.03); }
    .goalie-img { width: 55px; height: 55px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.15); margin-right: 15px; background: #1c1c1e; }
    .goalie-info { flex-grow: 1; text-align: left; }
    .goalie-name { font-size: 1.05rem; font-weight: 800; color: #fff; margin-bottom: 4px; }
    .goalie-stats { font-size: 0.8rem; color: #8F9191; font-weight: 700; letter-spacing: 0.5px; }
    .goalie-stat-hl { color: #fff; }

    /* NAV BUTTONS */
    .nav-btn { background: rgba(255,255,255,0.1); border: 1px solid rgba(255,255,255,0.2); color: white; border-radius: 50%; width: 50px; height: 50px; display: flex; align-items: center; justify-content: center; font-size: 1.5rem; cursor: pointer; transition: 0.2s; z-index: 1000; margin: 0 10px;}
    .nav-btn:hover { background: rgba(255,255,255,0.3); transform: scale(1.1); }
    .nav-container { display: flex; justify-content: center; align-items: center; margin-bottom: 20px; }
  ")

# --- UI ARCHITECTURE ---
ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$meta(name="viewport", content="width=device-width, initial-scale=1.0"),
    tags$script(HTML("
      document.addEventListener('touchstart', handleTouchStart, false);        
      document.addEventListener('touchmove', handleTouchMove, false);
      var xDown = null;                                                        
      function handleTouchStart(evt) { xDown = evt.touches[0].clientX; };                                                
      function handleTouchMove(evt) {
          if ( ! xDown ) return;
          var xUp = evt.touches[0].clientX;                                    
          var xDiff = xDown - xUp;
          if ( Math.abs( xDiff ) > 50 ) { 
              if ( xDiff > 0 ) { Shiny.setInputValue('swipe', 'left', {priority: 'event'}); } 
              else { Shiny.setInputValue('swipe', 'right', {priority: 'event'}); }                       
          }
          xDown = null;                                             
      };
    "))
  ),
  
  div(style = "max-width: 900px; margin: 40px auto; display: flex; flex-direction: column; gap: 24px;",
      
      div(class = "text-center master-title", "✈️ NOW BOARDING: THE JETS"),
      
      div(class = "nav-container",
          actionButton("btn_prev", "←", class = "nav-btn"),
          h4(textOutput("current_view_text"), style="margin: 0 20px; font-weight: 800; color: #8F9191; width: 180px; text-align: center;"),
          actionButton("btn_next", "→", class = "nav-btn")
      ),
      
      navset_hidden(
        id = "main_nav",
        
        # --- SCREEN 1: GAMEHUB ---
        nav_panel("GameHub",
                  div(style = "display: flex; flex-direction: column; gap: 24px;",
                      div(class = "apple-card text-center", div(class = "card-title", "Flight Details"), uiOutput("next_matchup_ui")),
                      
                      uiOutput("next_scoring_log_ui"),
                      uiOutput("next_live_goalies_ui"), # Bubble below summary
                      uiOutput("next_shot_map_ui"),
                      
                      layout_columns(col_widths = c(6, 6), gap = "24px",
                                     div(class = "apple-card", div(class = "card-title", textOutput("away_profile_title")), uiOutput("away_profile_ui")),
                                     div(class = "apple-card", div(class = "card-title", textOutput("home_profile_title")), uiOutput("home_profile_ui"))
                      ),
                      layout_columns(col_widths = c(6, 6), gap = "24px",
                                     div(class = "apple-card", div(class = "card-title", textOutput("away_scorers_title")), uiOutput("away_scorers_ui")),
                                     div(class = "apple-card", div(class = "card-title", textOutput("home_scorers_title")), uiOutput("home_scorers_ui"))
                      ),
                      div(class = "apple-card", div(class = "card-title", "Pre-Game Goaltending Tandems"),
                          layout_columns(col_widths = c(6, 6), gap = "24px", uiOutput("away_goalie_ui"), uiOutput("home_goalie_ui")))
                  )
        ),
        
        # --- SCREEN 2: POST-GAME ---
        nav_panel("Post-Game",
                  div(style = "display: flex; flex-direction: column; gap: 24px;",
                      div(class = "apple-card text-center", div(class = "card-title", "Final Box Score"), uiOutput("last_matchup_ui")),
                      uiOutput("last_scoring_log_ui"),
                      uiOutput("last_live_goalies_ui"), # Bubble below summary
                      uiOutput("last_shot_map_ui")
                  )
        ),
        
        # --- SCREEN 3: SCHEDULE & STANDINGS ---
        nav_panel("Schedule",
                  div(style = "display: flex; flex-direction: column; gap: 24px;",
                      div(class = "apple-card", div(class = "card-title", "Upcoming Flight Path"), uiOutput("schedule_ui")),
                      div(class = "apple-card", div(class = "card-title", "Central Division Race"), uiOutput("standings_ui"))
                  )
        )
      )
  )
)

server <- function(input, output, session) {
  
  # --- BULLETPROOF NAME EXTRACTOR ---
  safe_name <- function(name_col) {
    if (is.null(name_col)) return("")
    if (is.data.frame(name_col) && "default" %in% names(name_col)) return(name_col$default)
    if (is.list(name_col)) return(sapply(name_col, function(x) { if (is.list(x) && "default" %in% names(x)) return(x$default); if (is.character(x)) return(x); return("") }))
    return(as.character(name_col))
  }
  
  # --- 3-SCREEN NAVIGATION LOGIC ---
  views <- c("GameHub", "Post-Game", "Schedule")
  current_view <- reactiveVal(1)
  observeEvent(input$btn_next, { current_view(min(current_view() + 1, 3)) })
  observeEvent(input$btn_prev, { current_view(max(current_view() - 1, 1)) })
  observeEvent(input$swipe, {
    if(input$swipe == "left") current_view(min(current_view() + 1, 3))
    if(input$swipe == "right") current_view(max(current_view() - 1, 1))
  })
  observe({ nav_select("main_nav", views[current_view()]) })
  
  output$current_view_text <- renderText({ 
    if(current_view() == 1) "GAMEHUB" else if(current_view() == 2) "POST-GAME" else "SCHEDULE" 
  })
  
  # --- API STATE ROUTING ---
  raw_schedule <- reactive({ jsonlite::fromJSON("https://api-web.nhle.com/v1/club-schedule-season/WPG/now")$games })
  
  next_game <- reactive({
    req(raw_schedule())
    raw_schedule() %>% filter(!gameState %in% c("FINAL", "OFF")) %>% arrange(gameDate) %>% slice(1)
  })
  last_game <- reactive({
    req(raw_schedule())
    raw_schedule() %>% filter(gameState %in% c("FINAL", "OFF")) %>% tail(1)
  })
  
  # DATA FETCHING: Next Game
  next_gc <- reactive({
    req(next_game())
    if(next_game()$gameState %in% c("LIVE", "CRIT")) { invalidateLater(30000, session) }
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/landing")) }, error = function(e) NULL)
  })
  next_pbp <- reactive({
    req(next_game())
    if(next_game()$gameState %in% c("LIVE", "CRIT")) { invalidateLater(30000, session) }
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/play-by-play")) }, error = function(e) NULL)
  })
  next_bx <- reactive({
    req(next_game())
    if(next_game()$gameState %in% c("LIVE", "CRIT")) { invalidateLater(30000, session) }
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/boxscore")) }, error = function(e) NULL)
  })
  
  # DATA FETCHING: Last Game
  last_gc <- reactive({
    req(last_game())
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/landing")) }, error = function(e) NULL)
  })
  last_pbp <- reactive({
    req(last_game())
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/play-by-play")) }, error = function(e) NULL)
  })
  last_bx <- reactive({
    req(last_game())
    tryCatch({ jsonlite::fromJSON(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/boxscore")) }, error = function(e) NULL)
  })
  
  # --- REUSABLE UI BUILDERS ---
  
  build_matchup_header <- function(game, gc) {
    if(is.null(game)) return(div("No Game Scheduled"))
    state <- game$gameState 
    utc_time <- as.POSIXct(game$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
    wpg_time <- with_tz(utc_time, tzone = "America/Winnipeg")
    
    tv_networks <- "TBA"
    if ("tvBroadcasts" %in% names(game) && is.list(game$tvBroadcasts) && length(game$tvBroadcasts) > 0) {
      if (is.data.frame(game$tvBroadcasts[[1]]) && nrow(game$tvBroadcasts[[1]]) > 0) tv_networks <- paste(game$tvBroadcasts[[1]]$network, collapse = " | ")
    }
    
    center_block <- div(
      div("VS", class = "vs-text"), div(toupper(format(wpg_time, "%A, %B %d")), class = "game-date"),
      div(format(wpg_time, "%I:%M %p %Z"), class = "game-time"), div(tv_networks, class = "tv-badge")
    )
    
    if(!is.null(gc) && state %in% c("LIVE", "CRIT", "FINAL", "OFF")) {
      away_sog <- gc$awayTeam$sog %||% 0
      home_sog <- gc$homeTeam$sog %||% 0
      
      if(state %in% c("LIVE", "CRIT")) {
        p_num <- gc$periodDescriptor$number %||% 1
        p_type <- gc$periodDescriptor$periodType %||% "REG"
        in_int <- gc$clock$inIntermission %||% FALSE
        t_rem <- gc$clock$timeRemaining %||% "00:00"
        
        p_text <- if(p_type == "OT") "OT" else if(p_type == "SO") "SO" else paste0("P", p_num)
        time_text <- if(in_int) "INT" else t_rem
        status_badge <- div(paste(p_text, time_text, sep = " - "), class = "live-badge")
      } else {
        status_badge <- div("FINAL", class = "tv-badge", style="background: #8F9191;")
      }
      
      center_block <- div(class="d-flex flex-column align-items-center",
                          status_badge,
                          div(class="d-flex align-items-center", div(gc$awayTeam$score %||% 0, class="live-score"), div("-", style="font-size: 2rem; color: #8F9191; font-weight: 800;"), div(gc$homeTeam$score %||% 0, class="live-score")),
                          div(style="display: flex; align-items: center; justify-content: space-between; width: 150px; margin-top: 10px; background: rgba(0,0,0,0.3); padding: 5px 15px; border-radius: 12px; border: 1px solid rgba(255,255,255,0.05);",
                              div(away_sog, class="sog-val"), div("SOG", class="sog-text"), div(home_sog, class="sog-val"))
      )
    }
    
    layout_columns(col_widths = c(4, 4, 4), class = "align-items-center",
                   div(tags$img(src = game$awayTeam$logo, style = "width: 110px; filter: drop-shadow(0 0 15px rgba(255, 255, 255, 0.15));"), div(game$awayTeam$abbrev, style = "margin-top: 15px; font-weight: 800; letter-spacing: 1px; font-size: 1.2rem;")),
                   center_block,
                   div(tags$img(src = game$homeTeam$logo, style = "width: 110px; filter: drop-shadow(0 0 15px rgba(77, 184, 255, 0.25));"), div(game$homeTeam$abbrev, style = "margin-top: 15px; font-weight: 800; letter-spacing: 1px; font-size: 1.2rem;"))
    )
  }
  
  # --- COMPLETELY BULLETPROOF GOALIE BUILDER WITH PHOTOS ---
  build_live_goalies <- function(bx) {
    if(is.null(bx) || is.null(bx$playerByGameStats)) return(NULL) 
    
    extract_goalies <- function(goalies_df, color) {
      if (is.null(goalies_df) || !is.data.frame(goalies_df) || nrow(goalies_df) == 0) return(NULL)
      
      g_played <- goalies_df
      
      # SAFE FILTERING: Dynamically find which column exists to avoid "not found" errors
      if ("timeOnIce" %in% names(g_played)) {
        g_played <- g_played %>% filter(!is.na(timeOnIce) & timeOnIce != "00:00")
      } else if ("toi" %in% names(g_played)) {
        g_played <- g_played %>% filter(!is.na(toi) & toi != "00:00")
      } else if ("shotsAgainst" %in% names(g_played)) {
        g_played <- g_played %>% filter(!is.na(shotsAgainst) & as.numeric(shotsAgainst) > 0)
      }
      
      if (nrow(g_played) == 0) return(NULL)
      
      # unname() stops jsonlite from complaining about named vectors
      html_items <- lapply(1:nrow(g_played), function(i) {
        g <- g_played[i, ]
        name <- safe_name(g$name)
        
        # Construct Photo URL using playerId
        photo_url <- "https://assets.nhle.com/mugs/nhl/latest/placeholder.png" # Safe default
        if("playerId" %in% names(g) && !is.na(g$playerId)) {
          photo_url <- paste0("https://assets.nhle.com/mugs/nhl/latest/", g$playerId, ".png")
        }
        
        # Fallback image logic for CSS onerror (requires clean name for URLencode)
        clean_name <- gsub("[^A-Za-z0-9 ]", "", name)
        fallback_img <- paste0("https://ui-avatars.com/api/?name=", URLencode(clean_name), "&background=1c1c1e&color=8F9191")
        
        # Safely extract save percentage
        sv_val <- NA
        if("savePctg" %in% names(g)) sv_val <- suppressWarnings(as.numeric(g$savePctg))
        sv_pct <- if(is.na(sv_val)) "0.000" else sprintf("%.3f", sv_val)
        
        saves <- if("saves" %in% names(g) && !is.na(g$saves)) g$saves else 0
        shots <- if("shotsAgainst" %in% names(g) && !is.na(g$shotsAgainst)) g$shotsAgainst else 0
        
        HTML(unname(paste0('
          <div style="display: flex; align-items: center; justify-content: space-between; padding: 10px 14px; background: rgba(0,0,0,0.25); border: 1px solid rgba(255,255,255,0.03); border-radius: 14px; margin-bottom: 8px;">
            <div style="display: flex; align-items: center;">
              <div style="width: 10px; height: 10px; border-radius: 50%; background: ', color, '; margin-right: 12px; box-shadow: 0 0 5px ', color, ';"></div>
              
              <img class="goalie-img" src="', photo_url, '" onerror="this.onerror=null; this.src=\'', fallback_img, '\';" style="width: 40px; height: 40px; margin-right: 10px;">
              
              <div style="font-weight: 800; font-size: 1rem; color: #fff;">', name, '</div>
            </div>
            <div style="text-align: right;">
              <div style="font-weight: 900; color: #fff; font-size: 1.1rem; letter-spacing: 0.5px;">', sv_pct, '</div>
              <div style="font-size: 0.75rem; color: #8F9191; font-weight: 700; letter-spacing: 1px;">', saves, '/', shots, ' SV</div>
            </div>
          </div>
        ')))
      })
      return(html_items)
    }
    
    away_html <- extract_goalies(bx$playerByGameStats$awayTeam$goalies, "#FFFFFF")
    home_html <- extract_goalies(bx$playerByGameStats$homeTeam$goalies, "#4DB8FF")
    if(is.null(away_html) && is.null(home_html)) return(NULL)
    
    div(class = "apple-card",
        div(class = "card-title", "Live Goaltending Performance"),
        layout_columns(col_widths = c(6, 6), gap = "16px", div(unname(away_html)), div(unname(home_html)))
    )
  }
  
  build_scoring_log <- function(gc) {
    if(is.null(gc) || !gc$gameState %in% c("LIVE", "CRIT", "FINAL", "OFF")) return(NULL)
    scoring_periods <- gc$summary$scoring
    if(is.null(scoring_periods) || nrow(scoring_periods) == 0) return(NULL)
    
    period_blocks <- lapply(1:nrow(scoring_periods), function(i) {
      p_data <- scoring_periods[i, ]
      period_title <- ifelse(p_data$periodDescriptor$periodType == "OT", "Overtime", paste("Period", p_data$periodDescriptor$number))
      
      goals_list <- p_data$goals
      if(is.null(goals_list) || length(goals_list) == 0) return(NULL)
      goals <- goals_list[[1]]
      if(is.null(goals) || !is.data.frame(goals) || nrow(goals) == 0) return(NULL)
      
      goal_rows <- lapply(1:nrow(goals), function(j) {
        g <- goals[j, , drop = FALSE]
        color <- ifelse(g$teamAbbrev == gc$homeTeam$abbrev, "#4DB8FF", "#FFFFFF")
        scorer_first <- safe_name(g$firstName); scorer_last <- safe_name(g$lastName)
        
        ast_text <- "Unassisted"
        if ("assists" %in% names(g) && is.list(g$assists) && length(g$assists) > 0) {
          asts <- g$assists[[1]]
          if (is.data.frame(asts) && nrow(asts) > 0) {
            ast_firsts <- safe_name(asts$firstName); ast_lasts <- safe_name(asts$lastName)
            ast_names <- trimws(paste(ast_firsts, ast_lasts))
            ast_names <- ast_names[ast_names != ""]
            if (length(ast_names) > 0) ast_text <- paste("Assists:", paste(ast_names, collapse = ", "))
          }
        }
        HTML(unname(paste0('<div class="goal-row"><div style="display: flex; align-items: center;"><div class="goal-time">', g$timeInPeriod, '</div><div class="goal-dot" style="background:', color, ';"></div><div><div class="goal-scorer">', scorer_first, ' ', scorer_last, ' (', g$goalsToDate, ')</div><div class="goal-assists">', ast_text, '</div></div></div><div class="goal-team">', g$teamAbbrev, '</div></div>')))
      })
      div(div(class="period-header", period_title), div(unname(goal_rows)))
    })
    if (all(sapply(period_blocks, is.null))) return(NULL)
    div(class = "apple-card", div(class = "card-title", "Box Score Summary"), div(unname(period_blocks)))
  }
  
  build_shot_map <- function(gc, pbp) {
    if(is.null(gc) || is.null(pbp) || !gc$gameState %in% c("LIVE", "CRIT", "FINAL", "OFF")) return(NULL)
    plays <- pbp$plays
    if (is.null(plays) || nrow(plays) == 0) return(NULL)
    
    shots <- plays %>% filter(typeDescKey %in% c("shot-on-goal", "goal"))
    if(nrow(shots) == 0) return(NULL)
    
    home_id <- pbp$homeTeam$id
    away_abbrev <- pbp$awayTeam$abbrev
    home_abbrev <- pbp$homeTeam$abbrev
    
    dots <- lapply(1:nrow(shots), function(i) {
      s <- shots[i, ]
      x <- s$details$xCoord; y <- s$details$yCoord
      if(is.null(x) || is.na(x) || is.null(y) || is.na(y)) return(NULL)
      
      team_id <- s$details$eventOwnerTeamId
      if (team_id == home_id) { if (x < 0) { x <- -x; y <- -y } } else { if (x > 0) { x <- -x; y <- -y } }
      
      left_pct <- ((x + 100) / 200) * 100
      top_pct <- ((y + 42.5) / 85) * 100
      
      color <- ifelse(team_id == home_id, "#4DB8FF", "#FFFFFF")
      is_goal <- (s$typeDescKey == "goal")
      class_name <- ifelse(is_goal, "shot-dot shot-goal", "shot-dot")
      bg_color <- ifelse(is_goal, "#AD0E28", color)
      
      HTML(unname(paste0('<div class="', class_name, '" style="left:', left_pct, '%; top:', top_pct, '%; background:', bg_color, ';"></div>')))
    })
    
    div(class = "apple-card",
        div(class = "card-title", "Spatial Shot Map"),
        div(style="display:flex; justify-content: space-between; margin-bottom: 10px; font-size: 0.8rem; font-weight: 700; color: #8F9191;",
            div(tags$span(style="display:inline-block; width:10px; height:10px; background:#FFFFFF; border-radius:50%; margin-right:5px;"), away_abbrev, " Shots"),
            div(tags$span(style="display:inline-block; width:10px; height:10px; background:#AD0E28; border-radius:50%; margin-right:5px; border:1px solid #fff;"), "Goals"),
            div(tags$span(style="display:inline-block; width:10px; height:10px; background:#4DB8FF; border-radius:50%; margin-right:5px;"), home_abbrev, " Shots")
        ),
        div(class = "rink-container",
            div(class="rink-center-line"), div(class="rink-blue-line-left"), div(class="rink-blue-line-right"), div(class="rink-center-circle"),
            div(class="goal-line-left"), div(class="goal-line-right"), div(class="crease-left"), div(class="crease-right"),
            div(unname(dots))
        )
    )
  }
  
  # --- ASSIGN BUILDERS TO SCREENS ---
  output$next_matchup_ui <- renderUI({ build_matchup_header(next_game(), next_gc()) })
  output$next_scoring_log_ui <- renderUI({ build_scoring_log(next_gc()) })
  output$next_live_goalies_ui <- renderUI({ build_live_goalies(next_bx()) })
  output$next_shot_map_ui <- renderUI({ build_shot_map(next_gc(), next_pbp()) })
  
  output$last_matchup_ui <- renderUI({ build_matchup_header(last_game(), last_gc()) })
  output$last_scoring_log_ui <- renderUI({ build_scoring_log(last_gc()) })
  output$last_live_goalies_ui <- renderUI({ build_live_goalies(last_bx()) })
  output$last_shot_map_ui <- renderUI({ build_shot_map(last_gc(), last_pbp()) })
  
  # --- UI: PRE-GAME PROFILES (Only shown on GameHub) ---
  team_standings <- reactive({ jsonlite::fromJSON("https://api-web.nhle.com/v1/standings/now")$standings })
  
  get_team_stats <- function(team_abbr) {
    url <- paste0("https://api-web.nhle.com/v1/club-stats/", team_abbr, "/now")
    stats <- jsonlite::fromJSON(url)
    skaters <- stats$skaters %>% arrange(desc(points)) %>% head(4) %>% mutate(full_name = paste(firstName$default, lastName$default), photo = paste0("https://assets.nhle.com/mugs/nhl/latest/", playerId, ".png"))
    goalies <- stats$goalies %>% arrange(desc(gamesPlayed)) %>% head(2) %>% mutate(full_name = paste(firstName$default, lastName$default), photo = paste0("https://assets.nhle.com/mugs/nhl/latest/", playerId, ".png"))
    list(skaters = skaters, goalies = goalies)
  }
  
  next_game_data <- reactive({
    req(next_game(), team_standings())
    list(home_stats = get_team_stats(next_game()$homeTeam$abbrev), away_stats = get_team_stats(next_game()$awayTeam$abbrev), standings = team_standings(), info = next_game())
  })
  
  build_profile_html <- function(team_abbr, standings_df) {
    team_info <- standings_df[standings_df$teamAbbrev$default == team_abbr, ]
    if(nrow(team_info) == 0) return(div("Data Unavailable"))
    record <- paste0(team_info$wins, "-", team_info$losses, "-", team_info$otLosses); l10 <- paste0(team_info$l10Wins, "-", team_info$l10Losses, "-", team_info$l10OtLosses); gp <- team_info$gamesPlayed %||% (team_info$wins + team_info$losses + team_info$otLosses); pt_pct <- paste0(round((team_info$points / (gp * 2)) * 100, 1), "%"); row_val <- if("regulationPlusOtWins" %in% names(team_info)) team_info$regulationPlusOtWins else team_info$rowWins; gd <- team_info$goalDifferential; if(is.numeric(gd) && gd > 0) gd <- paste0("+", gd)
    HTML(unname(paste0('<div class="profile-grid"><div class="profile-stat"><div class="profile-val">', record, '</div><div class="profile-label">Record</div></div><div class="profile-stat"><div class="profile-val">', team_info$points, '</div><div class="profile-label">Points</div></div><div class="profile-stat"><div class="profile-val">', pt_pct, '</div><div class="profile-label">Point %</div></div><div class="profile-stat"><div class="profile-val">', l10, '</div><div class="profile-label">Last 10</div></div><div class="profile-stat"><div class="profile-val">', row_val, '</div><div class="profile-label">ROW</div></div><div class="profile-stat"><div class="profile-val">', gd, '</div><div class="profile-label">Goal Diff</div></div></div>')))
  }
  output$away_profile_title <- renderText({ req(next_game_data()); paste(next_game_data()$info$awayTeam$abbrev, "Profile") })
  output$home_profile_title <- renderText({ req(next_game_data()); paste(next_game_data()$info$homeTeam$abbrev, "Profile") })
  output$away_profile_ui <- renderUI({ req(next_game_data()); build_profile_html(next_game_data()$info$awayTeam$abbrev, next_game_data()$standings) })
  output$home_profile_ui <- renderUI({ req(next_game_data()); build_profile_html(next_game_data()$info$homeTeam$abbrev, next_game_data()$standings) })
  
  build_skater_html <- function(skaters_df, gradient_class) {
    max_pts <- max(skaters_df$points, na.rm = TRUE); if(max_pts == 0) max_pts <- 1
    HTML(unname(paste0(lapply(1:nrow(skaters_df), function(i) {
      row <- skaters_df[i, ]; pct <- (row$points / max_pts) * 100; safe_name <- gsub("[^A-Za-z0-9 ]", "", row$full_name); fallback_img <- paste0("https://ui-avatars.com/api/?name=", URLencode(safe_name), "&background=1c1c1e&color=8F9191")
      paste0('<div class="scorer-row"><img class="scorer-img" src="', row$photo, '" onerror="this.onerror=null; this.src=\'', fallback_img, '\';"><div class="scorer-info"><div class="scorer-text"><span>', row$full_name, '</span><span class="scorer-pts">', row$points, ' PTS</span></div><div class="progress-track"><div class="progress-fill ', gradient_class, '" style="width: ', pct, '%;"></div></div></div></div>')
    }), collapse = "")))
  }
  output$away_scorers_title <- renderText({ req(next_game_data()); paste(next_game_data()$info$awayTeam$abbrev, "Scoring Leaders") })
  output$home_scorers_title <- renderText({ req(next_game_data()); paste(next_game_data()$info$homeTeam$abbrev, "Scoring Leaders") })
  output$away_scorers_ui <- renderUI({ req(next_game_data()); build_skater_html(next_game_data()$away_stats$skaters, "away-gradient") })
  output$home_scorers_ui <- renderUI({ req(next_game_data()); build_skater_html(next_game_data()$home_stats$skaters, "home-gradient") })
  
  build_tandem_html <- function(goalies_df) {
    if(nrow(goalies_df) == 0) return(div("No Goalie Data"))
    HTML(unname(paste0(lapply(1:nrow(goalies_df), function(i) {
      row <- goalies_df[i, ]; safe_name <- gsub("[^A-Za-z0-9 ]", "", row$full_name); fallback_img <- paste0("https://ui-avatars.com/api/?name=", URLencode(safe_name), "&background=1c1c1e&color=8F9191")
      paste0('<div class="goalie-row"><img class="goalie-img" src="', row$photo, '" onerror="this.onerror=null; this.src=\'', fallback_img, '\';"><div class="goalie-info"><div class="goalie-name">', row$full_name, '</div><div class="goalie-stats">GP: <span class="goalie-stat-hl">', row$gamesPlayed, '</span> | GAA: <span class="goalie-stat-hl">', sprintf("%.2f", row$goalsAgainstAverage), '</span> | SV%: <span class="goalie-stat-hl">', sprintf("%.3f", row$savePercentage), '</span></div></div></div>')
    }), collapse = "")))
  }
  output$away_goalie_ui <- renderUI({ req(next_game_data()); build_tandem_html(next_game_data()$away_stats$goalies) })
  output$home_goalie_ui <- renderUI({ req(next_game_data()); build_tandem_html(next_game_data()$home_stats$goalies) })
  
  # --- UI: CENTRAL DIVISION STANDINGS ---
  output$standings_ui <- renderUI({
    st <- team_standings()
    req(st)
    cen <- st %>% filter(divisionAbbrev == "C" | divisionName == "Central") %>% arrange(desc(points))
    rows <- lapply(1:nrow(cen), function(i) {
      t <- cen[i,]
      HTML(unname(paste0('<div class="standings-row"><div class="standings-team"><img src="', t$teamLogo, '" width="24px"> ', t$teamAbbrev$default, '</div><div class="standings-val">', t$gamesPlayed, '</div><div class="standings-val" style="color:#fff; font-weight:900; font-size: 1.1rem;">', t$points, '</div><div class="standings-val">', t$wins, '-', t$losses, '-', t$otLosses, '</div><div class="standings-val">', ifelse(t$goalDifferential > 0, paste0("+", t$goalDifferential), t$goalDifferential), '</div></div>')))
    })
    div(div(class="standings-row standings-header", div("TEAM"), div("GP", class="standings-val"), div("PTS", class="standings-val"), div("REC", class="standings-val"), div("DIFF", class="standings-val")), div(unname(rows)))
  })
  
  # --- UI: SCHEDULE LIST ---
  output$schedule_ui <- renderUI({
    sched <- raw_schedule() %>% filter(as.Date(gameDate) >= lubridate::today(tzone = "America/Winnipeg")) %>% head(10)
    items <- lapply(1:nrow(sched), function(i) {
      g <- sched[i,]; utc <- as.POSIXct(g$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC"); wpg <- with_tz(utc, tzone = "America/Winnipeg")
      div(style = "display: flex; justify-content: space-between; align-items: center; padding: 15px 0; border-bottom: 1px solid rgba(255,255,255,0.05);",
          div(style="font-weight: 700;", format(wpg, "%b %d")), div(style="display:flex; align-items:center; gap: 10px;", tags$img(src = g$awayTeam$logo, width="30px"), "vs", tags$img(src = g$homeTeam$logo, width="30px")), div(style="color: #8F9191; font-weight: 600;", format(wpg, "%I:%M %p")))
    })
    div(unname(items))
  })
}

shinyApp(ui, server)