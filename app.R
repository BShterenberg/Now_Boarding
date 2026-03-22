library(shiny)
library(dplyr)
library(bslib)
library(jsonlite)
library(lubridate)
library(purrr)

# ── HELPERS ────────────────────────────────────────────────────────────────────
season_id <- function() {
  yr <- as.integer(format(Sys.Date(), "%Y"))
  mo <- as.integer(format(Sys.Date(), "%m"))
  if (mo >= 10L) yr * 10000L + (yr + 1L) else (yr - 1L) * 10000L + yr
}

safe_name <- function(x) {
  if (is.null(x)) return("")
  if (is.data.frame(x) && "default" %in% names(x)) return(x$default)
  if (is.list(x)) return(sapply(x, function(v) if (is.list(v) && "default" %in% names(v)) v$default else if (is.character(v)) v else ""))
  as.character(x)
}

safe_fetch <- function(url) tryCatch(jsonlite::fromJSON(url), error = function(e) NULL)
safe_csv   <- function(url) tryCatch(read.csv(url, stringsAsFactors = FALSE), error = function(e) NULL)
fb_img     <- function(name) { nm <- gsub("[^A-Za-z0-9 ]", "", name); paste0("https://ui-avatars.com/api/?name=", URLencode(nm), "&background=1c1c1e&color=8F9191") }
p_img      <- function(pid)  paste0("https://assets.nhle.com/mugs/nhl/latest/", pid, ".png")

# ── THEME & CSS ────────────────────────────────────────────────────────────────
app_theme <- bs_theme(
  version = 5, bg = "#0A0A0C", fg = "#FFFFFF",
  base_font = font_collection("system-ui", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto", "sans-serif")
) %>% bs_add_rules("
  body { background: radial-gradient(circle at top, #01183F 0%, #0A0A0C 60%); background-attachment: fixed; font-feature-settings: 'tnum'; }
  .master-title { font-weight: 900; font-size: 1.8rem; letter-spacing: 6px; color: #FFFFFF; margin-bottom: 16px; text-transform: uppercase; text-shadow: 0 4px 20px rgba(77,184,255,0.4); }
  .main-content { padding-bottom: 90px; }

  /* CARDS */
  .apple-card { background: rgba(255,255,255,0.03); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px); border: 1px solid rgba(255,255,255,0.1); border-radius: 28px; padding: 24px; box-shadow: 0 10px 30px rgba(0,0,0,0.4); display: flex; flex-direction: column; transition: transform 0.3s ease, box-shadow 0.3s ease; }
  .apple-card:hover { transform: translateY(-4px); box-shadow: 0 15px 40px rgba(0,0,0,0.6); }
  .card-title { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 2px; color: #8F9191; font-weight: 700; margin-bottom: 20px; text-align: center; }

  /* MATCHUP HEADER */
  .vs-text { font-size: 3.5rem; font-weight: 900; color: #FFFFFF; text-shadow: 4px 4px 0px #AD0E28; line-height: 1; margin: 10px 0; }
  .game-date { font-size: 1.2rem; font-weight: 700; margin-top: 10px; letter-spacing: 1px; }
  .game-time { font-size: 0.95rem; color: #8F9191; font-weight: 600; }
  .tv-badge { background: #AD0E28; border-radius: 8px; padding: 4px 14px; font-size: 0.8rem; font-weight: 800; color: #fff; box-shadow: 0 4px 10px rgba(173,14,40,0.4); display: inline-block; margin-top: 12px; }

  /* LIVE SCORE */
  .live-score { font-size: 4.5rem; font-weight: 900; color: #fff; text-shadow: 0 0 20px rgba(255,255,255,0.3); margin: 0 15px; }
  .live-badge { background: #ff2a2a; color: white; padding: 5px 12px; border-radius: 12px; font-weight: 800; font-size: 0.9rem; letter-spacing: 2px; animation: pulse 2s infinite; }
  .sog-text { font-size: 0.85rem; font-weight: 800; color: #8F9191; letter-spacing: 1px; }
  .sog-val  { font-size: 1.1rem; font-weight: 900; color: #fff; }
  @keyframes pulse { 0% { box-shadow: 0 0 0 0 rgba(255,42,42,0.7); } 70% { box-shadow: 0 0 0 10px rgba(255,42,42,0); } 100% { box-shadow: 0 0 0 0 rgba(255,42,42,0); } }

  /* SCORING LOG */
  .period-header { font-size: 0.85rem; text-transform: uppercase; letter-spacing: 2px; color: #AD0E28; font-weight: 800; margin-bottom: 10px; border-bottom: 2px solid rgba(173,14,40,0.5); padding-bottom: 4px; margin-top: 15px; }
  .goal-row { display: flex; align-items: center; justify-content: space-between; padding: 12px 0; border-bottom: 1px solid rgba(255,255,255,0.05); }
  .goal-time { font-weight: 700; color: #8F9191; width: 45px; font-size: 0.9rem; }
  .goal-dot  { width: 8px; height: 8px; border-radius: 50%; margin-right: 12px; }
  .goal-scorer  { font-weight: 800; color: #fff; font-size: 1.05rem; }
  .goal-assists { font-size: 0.75rem; color: #8F9191; font-weight: 600; margin-top: 2px; }
  .goal-team    { font-weight: 900; color: rgba(255,255,255,0.2); font-size: 1.2rem; }

  /* RINK */
  .rink-container { position: relative; width: 100%; aspect-ratio: 2.35; background: #e8eaed; border-radius: 40px; border: 4px solid #8F9191; overflow: hidden; margin-top: 10px; }
  .rink-center-line  { position: absolute; left: 50%; top: 0; bottom: 0; width: 4px; background: #AD0E28; transform: translateX(-50%); z-index: 1; }
  .rink-blue-line-left  { position: absolute; left: 35%; top: 0; bottom: 0; width: 4px; background: #01183F; z-index: 1; }
  .rink-blue-line-right { position: absolute; right: 35%; top: 0; bottom: 0; width: 4px; background: #01183F; z-index: 1; }
  .rink-center-circle   { position: absolute; left: 50%; top: 50%; width: 15%; aspect-ratio: 1; border: 4px solid #01183F; border-radius: 50%; transform: translate(-50%,-50%); z-index: 1; }
  .goal-line-left  { position: absolute; left: 5.5%; top: 0; bottom: 0; width: 2px; background: #AD0E28; z-index: 1; }
  .goal-line-right { position: absolute; right: 5.5%; top: 0; bottom: 0; width: 2px; background: #AD0E28; z-index: 1; }
  .crease-left  { position: absolute; left: 5.5%; top: 45%; width: 2.5%; height: 10%; background: rgba(77,184,255,0.4); border: 2px solid #AD0E28; border-left: none; border-radius: 0 100px 100px 0; z-index: 0; }
  .crease-right { position: absolute; right: 5.5%; top: 45%; width: 2.5%; height: 10%; background: rgba(77,184,255,0.4); border: 2px solid #AD0E28; border-right: none; border-radius: 100px 0 0 100px; z-index: 0; }
  .shot-dot  { position: absolute; width: 10px; height: 10px; border-radius: 50%; transform: translate(-50%,-50%); z-index: 10; box-shadow: 0 0 5px rgba(0,0,0,0.5); opacity: 0.85; }
  .shot-goal { width: 18px; height: 18px; border: 2px solid #fff; z-index: 15; opacity: 1; animation: pulse 1.5s infinite; }

  /* STANDINGS */
  .standings-row { display: grid; grid-template-columns: 2fr 0.8fr 1fr 1.5fr 0.8fr; padding: 10px 0; border-bottom: 1px solid rgba(255,255,255,0.05); align-items: center; font-size: 0.88rem; }
  .standings-header { font-weight: 800; color: #AD0E28; border-bottom: 2px solid rgba(173,14,40,0.5); padding-bottom: 8px; }
  .standings-team { font-weight: 800; color: #fff; display: flex; align-items: center; gap: 8px; }
  .standings-val  { color: #8F9191; font-weight: 600; text-align: center; }
  .standings-fav  { background: rgba(77,184,255,0.06); border-radius: 8px; padding-left: 4px; }
  .div-section { margin-bottom: 28px; }
  .div-title { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 2px; color: #AD0E28; font-weight: 800; margin-bottom: 8px; padding-bottom: 4px; border-bottom: 1px solid rgba(173,14,40,0.3); }

  /* PROFILES / SCORERS / GOALIES */
  .profile-grid  { display: grid; grid-template-columns: repeat(3,1fr); gap: 12px; text-align: center; }
  .profile-stat  { background: rgba(0,0,0,0.25); border-radius: 12px; padding: 14px 6px; border: 1px solid rgba(255,255,255,0.03); }
  .profile-val   { font-size: 1.15rem; font-weight: 800; color: #fff; }
  .profile-label { font-size: 0.65rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1px; margin-top: 4px; font-weight: 600; }
  .scorer-row { display: flex; align-items: center; margin-bottom: 18px; }
  .scorer-row:last-child { margin-bottom: 0; }
  .scorer-img  { width: 50px; height: 50px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.15); margin-right: 16px; background: #1c1c1e; }
  .scorer-info { flex-grow: 1; }
  .scorer-text { display: flex; justify-content: space-between; font-size: 1rem; font-weight: 700; margin-bottom: 8px; }
  .scorer-pts  { color: #8F9191; font-weight: 600; }
  .progress-track { height: 6px; background: rgba(255,255,255,0.05); border-radius: 4px; overflow: hidden; }
  .progress-fill  { height: 100%; border-radius: 4px; transition: width 1s ease-in-out; }
  .home-gradient  { background: linear-gradient(90deg, #01183F, #4DB8FF); }
  .away-gradient  { background: linear-gradient(90deg, #8F9191, #FFFFFF); }
  .goalie-row  { display: flex; align-items: center; margin-bottom: 12px; padding: 12px; background: rgba(0,0,0,0.2); border-radius: 16px; border: 1px solid rgba(255,255,255,0.03); }
  .goalie-img  { width: 55px; height: 55px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.15); margin-right: 15px; background: #1c1c1e; }
  .goalie-info { flex-grow: 1; text-align: left; }
  .goalie-name { font-size: 1.05rem; font-weight: 800; color: #fff; margin-bottom: 4px; }
  .goalie-stats   { font-size: 0.8rem; color: #8F9191; font-weight: 700; letter-spacing: 0.5px; }
  .goalie-stat-hl { color: #fff; }

  /* SEASON MOMENTUM */
  .mom-grid  { display: grid; grid-template-columns: repeat(3,1fr); gap: 10px; }
  .mom-stat  { background: rgba(0,0,0,0.25); border-radius: 14px; padding: 14px 8px; text-align: center; border: 1px solid rgba(255,255,255,0.03); }
  .mom-val   { font-size: 1.1rem; font-weight: 900; color: #fff; }
  .mom-label { font-size: 0.6rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1px; margin-top: 4px; font-weight: 600; }
  .streak-badge { display: inline-block; padding: 3px 12px; border-radius: 20px; font-weight: 900; font-size: 0.9rem; letter-spacing: 1px; }
  .streak-w { background: rgba(0,200,100,0.2);  color: #00C864; border: 1px solid rgba(0,200,100,0.3); }
  .streak-l { background: rgba(173,14,40,0.2);  color: #AD0E28; border: 1px solid rgba(173,14,40,0.3); }
  .streak-o { background: rgba(255,170,0,0.2);  color: #FFAA00; border: 1px solid rgba(255,170,0,0.3); }

  /* TEAM STATS PILLS */
  .stat-grid      { display: grid; grid-template-columns: repeat(4,1fr); gap: 8px; }
  .stat-pill      { background: rgba(0,0,0,0.3); border-radius: 12px; padding: 12px 6px; text-align: center; border: 1px solid rgba(255,255,255,0.04); }
  .stat-pill-val  { font-size: 0.95rem; font-weight: 900; color: #fff; }
  .stat-pill-lbl  { font-size: 0.56rem; color: #8F9191; text-transform: uppercase; letter-spacing: 1px; margin-top: 3px; font-weight: 600; }

  /* ADVANCED STATS */
  .adv-row   { display: flex; justify-content: space-between; align-items: center; padding: 10px 0; border-bottom: 1px solid rgba(255,255,255,0.05); }
  .adv-label { font-size: 0.85rem; color: #8F9191; font-weight: 700; }
  .adv-val   { font-size: 1rem; font-weight: 900; color: #fff; }

  /* THREE STARS */
  .three-stars-grid { display: grid; grid-template-columns: repeat(3,1fr); gap: 12px; text-align: center; }
  .star-item   { background: rgba(0,0,0,0.2); border-radius: 16px; padding: 14px 8px; border: 1px solid rgba(255,255,255,0.04); }
  .star-num    { font-size: 0.68rem; color: #FFAA00; font-weight: 900; text-transform: uppercase; letter-spacing: 1px; margin-bottom: 8px; }
  .star-img    { width: 60px; height: 60px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.15); }
  .star-name   { font-size: 0.85rem; font-weight: 800; color: #fff; margin-top: 8px; }
  .star-detail { font-size: 0.7rem; color: #8F9191; font-weight: 600; margin-top: 3px; }

  /* SHOTS BY PERIOD */
  .shots-per-row   { display: flex; align-items: center; gap: 10px; margin-bottom: 10px; }
  .shots-per-label { font-size: 0.72rem; color: #AD0E28; font-weight: 900; width: 26px; text-align: center; flex-shrink: 0; }
  .shots-per-bars  { flex: 1; display: flex; height: 26px; gap: 2px; }
  .shots-per-away  { background: rgba(255,255,255,0.65); display: flex; align-items: center; justify-content: flex-end; padding: 0 7px; font-size: 0.76rem; font-weight: 900; color: #000; border-radius: 6px 0 0 6px; min-width: 24px; }
  .shots-per-home  { background: rgba(77,184,255,0.75); display: flex; align-items: center; padding: 0 7px; font-size: 0.76rem; font-weight: 900; color: #000; border-radius: 0 6px 6px 0; min-width: 24px; }

  /* MOMENTUM CHART */
  .momentum-svg { width: 100%; height: 110px; overflow: visible; display: block; }

  /* TODAY'S GAMES */
  .today-game-row  { display: flex; align-items: center; justify-content: space-between; padding: 12px 0; border-bottom: 1px solid rgba(255,255,255,0.05); }
  .today-score     { font-size: 1.15rem; font-weight: 900; color: #fff; min-width: 52px; text-align: center; }
  .today-live-badge { background: #ff2a2a; color: white; padding: 2px 8px; border-radius: 8px; font-weight: 800; font-size: 0.68rem; letter-spacing: 1px; animation: pulse 2s infinite; }
  .today-final     { font-size: 0.7rem; color: #8F9191; font-weight: 700; }

  /* SCORING LEADERS */
  .leader-row  { display: flex; align-items: center; padding: 10px 0; border-bottom: 1px solid rgba(255,255,255,0.05); gap: 12px; }
  .leader-rank { font-size: 0.85rem; color: #AD0E28; font-weight: 900; width: 18px; }
  .leader-img  { width: 42px; height: 42px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.1); background: #1c1c1e; flex-shrink: 0; }
  .leader-info { flex: 1; }
  .leader-name { font-size: 0.9rem; font-weight: 800; color: #fff; }
  .leader-team { font-size: 0.7rem; color: #8F9191; font-weight: 600; }
  .leader-stat { font-size: 1.05rem; font-weight: 900; color: #4DB8FF; min-width: 34px; text-align: right; }

  /* ROSTER */
  .roster-sec-title { font-size: 0.72rem; text-transform: uppercase; letter-spacing: 2px; color: #AD0E28; font-weight: 800; margin: 18px 0 8px; border-bottom: 2px solid rgba(173,14,40,0.35); padding-bottom: 5px; }
  .roster-row  { display: flex; align-items: center; padding: 9px 0; border-bottom: 1px solid rgba(255,255,255,0.04); gap: 10px; }
  .roster-img  { width: 44px; height: 44px; border-radius: 50%; object-fit: cover; border: 2px solid rgba(255,255,255,0.1); background: #1c1c1e; flex-shrink: 0; }
  .roster-num  { font-size: 0.78rem; color: #8F9191; font-weight: 800; width: 24px; text-align: center; flex-shrink: 0; }
  .roster-name { font-size: 0.9rem; font-weight: 800; color: #fff; }
  .roster-detail { font-size: 0.68rem; color: #8F9191; font-weight: 600; margin-top: 2px; }
  .pos-pill { display: inline-block; padding: 1px 7px; border-radius: 5px; font-size: 0.6rem; font-weight: 900; background: rgba(77,184,255,0.15); color: #4DB8FF; letter-spacing: 0.5px; }
  .pos-pill-g { background: rgba(173,14,40,0.2); color: #AD0E28; }

  /* RECENT RESULTS */
  .result-row { display: flex; align-items: center; justify-content: space-between; padding: 10px 0; border-bottom: 1px solid rgba(255,255,255,0.05); }
  .result-w { color: #00C864; font-weight: 900; font-size: 0.85rem; }
  .result-l { color: #AD0E28; font-weight: 900; font-size: 0.85rem; }
  .result-o { color: #FFAA00; font-weight: 900; font-size: 0.85rem; }

  /* BOTTOM NAV */
  .bottom-nav { position: fixed; bottom: 0; left: 0; right: 0; background: rgba(10,10,12,0.96); backdrop-filter: blur(20px); -webkit-backdrop-filter: blur(20px); display: flex; justify-content: space-around; padding: 8px 0 16px; border-top: 1px solid rgba(255,255,255,0.08); z-index: 1000; }
  .nav-tab-btn { display: flex; flex-direction: column; align-items: center; gap: 3px; cursor: pointer; padding: 6px 8px; border-radius: 10px; transition: 0.2s; color: #8F9191; font-size: 0.55rem; font-weight: 700; letter-spacing: 0.5px; text-transform: uppercase; background: none; border: none; }
  .nav-tab-btn.active { color: #4DB8FF; }
  .nav-icon { font-size: 1.2rem; line-height: 1; }
")

# ── UI ─────────────────────────────────────────────────────────────────────────
ui <- fluidPage(
  theme = app_theme,
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1.0"),
    tags$script(HTML("
      // Swipe
      document.addEventListener('touchstart', function(e){ window._xt = e.touches[0].clientX; }, false);
      document.addEventListener('touchend',   function(e){
        if(window._xt == null) return;
        var dx = window._xt - e.changedTouches[0].clientX;
        if(Math.abs(dx) > 50) Shiny.setInputValue('swipe', dx > 0 ? 'left' : 'right', {priority:'event'});
        window._xt = null;
      }, false);
      // Keyboard arrows
      document.addEventListener('keydown', function(e){
        if(e.key==='ArrowRight') Shiny.setInputValue('swipe','left',{priority:'event'});
        if(e.key==='ArrowLeft')  Shiny.setInputValue('swipe','right',{priority:'event'});
      });
      // Nav tab style updater
      Shiny.addCustomMessageHandler('updateNavStyles', function(msg) {
        var ids = ['nav_gamehub','nav_teamhq','nav_postgame','nav_league','nav_standings','nav_roster'];
        ids.forEach(function(id, i) {
          var el = document.getElementById(id);
          if(el) el.className = (i+1 === msg.active) ? 'nav-tab-btn active' : 'nav-tab-btn';
        });
      });
    "))
  ),

  div(class = "main-content",
    div(style = "max-width: 900px; margin: 30px auto; display: flex; flex-direction: column; gap: 24px; padding: 0 16px;",
      div(class = "text-center master-title", "✈️ NOW BOARDING: THE JETS"),

      navset_hidden(
        id = "main_nav",

        # ── SCREEN 1: GAMEHUB ──────────────────────────────────────────────────
        nav_panel("GameHub",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card text-center", div(class="card-title","Flight Details"), uiOutput("next_matchup_ui")),
            uiOutput("gamehub_pregame_ui"),   # momentum + scout (pre-game only)
            uiOutput("next_scoring_log_ui"),
            uiOutput("next_live_goalies_ui"),
            uiOutput("next_shots_period_ui"),
            uiOutput("next_momentum_ui"),
            uiOutput("next_shot_map_ui"),
            uiOutput("gamehub_scorers_ui"),   # scoring leaders (always)
            uiOutput("gamehub_goalie_ui")     # goalie tandem (always)
          )
        ),

        # ── SCREEN 2: TEAM HQ ──────────────────────────────────────────────────
        nav_panel("TeamHQ",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card", div(class="card-title","Season Momentum"), uiOutput("hq_momentum_ui")),
            div(class="apple-card", div(class="card-title","Team Stats"),      uiOutput("hq_stats_ui")),
            uiOutput("hq_adv_ui"),
            div(class="apple-card", div(class="card-title","WPG Scoring Leaders"), uiOutput("hq_scorers_ui")),
            div(class="apple-card", div(class="card-title","Goalie Tandem"),        uiOutput("hq_goalies_ui")),
            div(class="apple-card", div(class="card-title","Recent Results"),       uiOutput("hq_results_ui")),
            div(class="apple-card", div(class="card-title","Upcoming Schedule"),    uiOutput("schedule_ui"))
          )
        ),

        # ── SCREEN 3: POST-GAME ────────────────────────────────────────────────
        nav_panel("Post-Game",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card text-center", div(class="card-title","Final Box Score"), uiOutput("last_matchup_ui")),
            uiOutput("last_three_stars_ui"),
            uiOutput("last_scoring_log_ui"),
            uiOutput("last_live_goalies_ui"),
            uiOutput("last_shots_period_ui"),
            uiOutput("last_momentum_ui"),
            uiOutput("last_shot_map_ui")
          )
        ),

        # ── SCREEN 4: LEAGUE ───────────────────────────────────────────────────
        nav_panel("League",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card", div(class="card-title","Today's Games"),    uiOutput("today_games_ui")),
            layout_columns(col_widths = c(6,6), gap = "24px",
              div(class="apple-card", div(class="card-title","Points Leaders"), uiOutput("pts_leaders_ui")),
              div(class="apple-card", div(class="card-title","Goals Leaders"),  uiOutput("goals_leaders_ui"))
            )
          )
        ),

        # ── SCREEN 5: STANDINGS ────────────────────────────────────────────────
        nav_panel("Standings",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card", div(class="card-title","League Standings"), uiOutput("standings_ui"))
          )
        ),

        # ── SCREEN 6: ROSTER ───────────────────────────────────────────────────
        nav_panel("Roster",
          div(style = "display:flex; flex-direction:column; gap:24px;",
            div(class="apple-card", div(class="card-title","WPG Roster"), uiOutput("roster_ui"))
          )
        )
      )
    )
  ),

  # ── BOTTOM NAV BAR ──────────────────────────────────────────────────────────
  div(class = "bottom-nav",
    actionButton("nav_gamehub",  HTML('<span class="nav-icon">🏒</span>GameHub'),  class="nav-tab-btn active"),
    actionButton("nav_teamhq",   HTML('<span class="nav-icon">✈️</span>Team HQ'),  class="nav-tab-btn"),
    actionButton("nav_postgame", HTML('<span class="nav-icon">📊</span>Post-Game'), class="nav-tab-btn"),
    actionButton("nav_league",   HTML('<span class="nav-icon">🌐</span>League'),    class="nav-tab-btn"),
    actionButton("nav_standings",HTML('<span class="nav-icon">📋</span>Standings'), class="nav-tab-btn"),
    actionButton("nav_roster",   HTML('<span class="nav-icon">👥</span>Roster'),    class="nav-tab-btn")
  )
)

# ── SERVER ─────────────────────────────────────────────────────────────────────
server <- function(input, output, session) {

  views <- c("GameHub","TeamHQ","Post-Game","League","Standings","Roster")
  current_view <- reactiveVal(1)

  # Nav button clicks
  observeEvent(input$nav_gamehub,   current_view(1))
  observeEvent(input$nav_teamhq,    current_view(2))
  observeEvent(input$nav_postgame,  current_view(3))
  observeEvent(input$nav_league,    current_view(4))
  observeEvent(input$nav_standings, current_view(5))
  observeEvent(input$nav_roster,    current_view(6))

  # Swipe
  observeEvent(input$swipe, {
    if (input$swipe == "left")  current_view(min(current_view() + 1, 6))
    if (input$swipe == "right") current_view(max(current_view() - 1, 1))
  })

  # Sync nav panel + highlight active tab
  observe({
    nav_select("main_nav", views[current_view()])
    session$sendCustomMessage("updateNavStyles", list(active = current_view()))
  })

  # ── DATA: SCHEDULE ─────────────────────────────────────────────────────────
  raw_schedule <- reactive({
    invalidateLater(300000, session)
    safe_fetch("https://api-web.nhle.com/v1/club-schedule-season/WPG/now")$games
  })

  next_game <- reactive({
    req(raw_schedule())
    raw_schedule() %>% filter(!gameState %in% c("FINAL","OFF")) %>% arrange(gameDate) %>% slice(1)
  })
  last_game <- reactive({
    req(raw_schedule())
    raw_schedule() %>% filter(gameState %in% c("FINAL","OFF")) %>% tail(1)
  })

  # ── DATA: GAME CENTER ──────────────────────────────────────────────────────
  fetch_game <- function(id, ep, live_reactive = NULL) reactive({
    req(id())
    if (!is.null(live_reactive) && !is.null(live_reactive()) &&
        live_reactive()$gameState %in% c("LIVE","CRIT")) invalidateLater(30000, session)
    safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", id()$id, "/", ep))
  })

  next_gc  <- reactive({
    req(next_game())
    if (!is.null(next_game()) && next_game()$gameState %in% c("LIVE","CRIT")) invalidateLater(30000, session)
    safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/landing"))
  })
  next_pbp <- reactive({
    req(next_game())
    if (!is.null(next_game()) && next_game()$gameState %in% c("LIVE","CRIT")) invalidateLater(30000, session)
    safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/play-by-play"))
  })
  next_bx  <- reactive({
    req(next_game())
    if (!is.null(next_game()) && next_game()$gameState %in% c("LIVE","CRIT")) invalidateLater(30000, session)
    safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", next_game()$id, "/boxscore"))
  })

  last_gc  <- reactive({ req(last_game()); safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/landing")) })
  last_pbp <- reactive({ req(last_game()); safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/play-by-play")) })
  last_bx  <- reactive({ req(last_game()); safe_fetch(paste0("https://api-web.nhle.com/v1/gamecenter/", last_game()$id, "/boxscore")) })

  # ── DATA: STANDINGS ────────────────────────────────────────────────────────
  team_standings <- reactive({
    invalidateLater(300000, session)
    safe_fetch("https://api-web.nhle.com/v1/standings/now")$standings
  })

  # ── DATA: TEAM STATS ───────────────────────────────────────────────────────
  get_team_stats <- function(abbr) {
    s <- safe_fetch(paste0("https://api-web.nhle.com/v1/club-stats/", abbr, "/now"))
    if (is.null(s)) return(list(skaters = data.frame(), goalies = data.frame()))
    skaters <- tryCatch(
      s$skaters %>% arrange(desc(points)) %>% head(5) %>%
        mutate(full_name = paste(firstName$default, lastName$default),
               photo = p_img(playerId)),
      error = function(e) data.frame()
    )
    goalies <- tryCatch(
      s$goalies %>% arrange(desc(gamesPlayed)) %>% head(2) %>%
        mutate(full_name = paste(firstName$default, lastName$default),
               photo = p_img(playerId)),
      error = function(e) data.frame()
    )
    list(skaters = skaters, goalies = goalies)
  }

  wpg_stats <- reactive({
    invalidateLater(300000, session)
    get_team_stats("WPG")
  })

  next_game_stats <- reactive({
    req(next_game(), team_standings())
    list(
      home_stats = get_team_stats(next_game()$homeTeam$abbrev),
      away_stats = get_team_stats(next_game()$awayTeam$abbrev),
      standings  = team_standings(),
      info       = next_game()
    )
  })

  # ── DATA: ADVANCED STATS (MoneyPuck) ──────────────────────────────────────
  adv_stats <- reactive({
    yr <- season_id() %/% 10000
    safe_csv(paste0("https://moneypuck.com/moneypuck/playerData/seasonSummary/", yr, "/regular/teams.csv"))
  })

  # ── DATA: NHL REST API team summary (PP%, PK%, etc.) ─────────────────────
  wpg_summary <- reactive({
    sid <- season_id()
    safe_fetch(paste0("https://api.nhle.com/stats/rest/en/team/summary?cayenneExp=seasonId%3D", sid, "%20and%20teamId%3D52"))
  })

  # ── DATA: TODAY'S GAMES ────────────────────────────────────────────────────
  today_games_data <- reactive({
    if (current_view() == 4) invalidateLater(60000, session)
    safe_fetch("https://api-web.nhle.com/v1/score/now")
  })

  # ── DATA: SCORING LEADERS ─────────────────────────────────────────────────
  pts_leaders_data <- reactive({
    safe_fetch("https://api-web.nhle.com/v1/skater-stats-leaders/current?categories=points&limit=5")
  })
  goals_leaders_data <- reactive({
    safe_fetch("https://api-web.nhle.com/v1/skater-stats-leaders/current?categories=goals&limit=5")
  })

  # ── DATA: ROSTER ──────────────────────────────────────────────────────────
  roster_data <- reactive({
    safe_fetch("https://api-web.nhle.com/v1/roster/WPG/current")
  })

  # ────────────────────────────────────────────────────────────────────────────
  # UI BUILDER FUNCTIONS
  # ────────────────────────────────────────────────────────────────────────────

  # ── MATCHUP HEADER ─────────────────────────────────────────────────────────
  build_matchup_header <- function(game, gc) {
    if (is.null(game) || nrow(game) == 0) return(div("No game scheduled"))
    state    <- game$gameState
    utc_time <- as.POSIXct(game$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
    loc_time <- with_tz(utc_time, tzone="America/Winnipeg")

    tv <- "TBA"
    if ("tvBroadcasts" %in% names(game) && is.list(game$tvBroadcasts) && length(game$tvBroadcasts) > 0) {
      bdf <- game$tvBroadcasts[[1]]
      if (is.data.frame(bdf) && nrow(bdf) > 0) tv <- paste(bdf$network, collapse=" | ")
    }

    center_block <- div(
      div("VS", class="vs-text"),
      div(toupper(format(loc_time, "%A, %B %d")), class="game-date"),
      div(format(loc_time, "%I:%M %p %Z"), class="game-time"),
      div(tv, class="tv-badge")
    )

    if (!is.null(gc) && state %in% c("LIVE","CRIT","FINAL","OFF")) {
      away_sog <- gc$awayTeam$sog %||% 0
      home_sog <- gc$homeTeam$sog %||% 0
      if (state %in% c("LIVE","CRIT")) {
        p_num  <- gc$periodDescriptor$number %||% 1
        p_type <- gc$periodDescriptor$periodType %||% "REG"
        in_int <- gc$clock$inIntermission %||% FALSE
        t_rem  <- gc$clock$timeRemaining %||% "00:00"
        p_text <- if (p_type=="OT") "OT" else if (p_type=="SO") "SO" else paste0("P",p_num)
        time_t <- if (in_int) "INT" else t_rem
        badge  <- div(paste(p_text, time_t, sep=" - "), class="live-badge")
      } else {
        badge <- div("FINAL", class="tv-badge", style="background:#8F9191;")
      }
      center_block <- div(class="d-flex flex-column align-items-center",
        badge,
        div(class="d-flex align-items-center",
          div(gc$awayTeam$score %||% 0, class="live-score"),
          div("-", style="font-size:2rem;color:#8F9191;font-weight:800;"),
          div(gc$homeTeam$score %||% 0, class="live-score")
        ),
        div(style="display:flex;align-items:center;justify-content:space-between;width:150px;margin-top:10px;background:rgba(0,0,0,0.3);padding:5px 15px;border-radius:12px;border:1px solid rgba(255,255,255,0.05);",
          div(away_sog, class="sog-val"), div("SOG", class="sog-text"), div(home_sog, class="sog-val"))
      )
    }

    layout_columns(col_widths=c(4,4,4), class="align-items-center",
      div(tags$img(src=game$awayTeam$logo, style="width:110px;filter:drop-shadow(0 0 15px rgba(255,255,255,0.15));"),
          div(game$awayTeam$abbrev, style="margin-top:15px;font-weight:800;letter-spacing:1px;font-size:1.2rem;")),
      center_block,
      div(tags$img(src=game$homeTeam$logo, style="width:110px;filter:drop-shadow(0 0 15px rgba(77,184,255,0.25));"),
          div(game$homeTeam$abbrev, style="margin-top:15px;font-weight:800;letter-spacing:1px;font-size:1.2rem;"))
    )
  }

  # ── SEASON MOMENTUM CARD ───────────────────────────────────────────────────
  build_season_momentum <- function(abbr, standings_df, title = NULL) {
    if (is.null(standings_df)) return(NULL)
    ti <- standings_df[standings_df$teamAbbrev$default == abbr, ]
    if (nrow(ti) == 0) return(NULL)
    gp     <- ti$gamesPlayed %||% 1
    record <- paste0(ti$wins,"-",ti$losses,"-",ti$otLosses)
    pts    <- ti$points
    l10    <- paste0(ti$l10Wins,"-",ti$l10Losses,"-",ti$l10OtLosses)
    gfpg   <- round(ti$goalFor / gp, 2)
    gapg   <- round(ti$goalAgainst / gp, 2)
    home_r <- paste0(ti$homeWins %||% "?","-",ti$homeLosses %||% "?","-",ti$homeOtLosses %||% "?")
    road_r <- paste0(ti$roadWins %||% "?","-",ti$roadLosses %||% "?","-",ti$roadOtLosses %||% "?")
    streak_raw <- ti$streakCode %||% ""
    if (nchar(streak_raw) > 0 && !grepl("[0-9]", streak_raw)) streak_raw <- paste0(streak_raw, "1")
    streak_cls <- if (grepl("^W", streak_raw)) "streak-badge streak-w" else if (grepl("^L", streak_raw)) "streak-badge streak-l" else "streak-badge streak-o"

    tagList(
      if (!is.null(title)) div(style="font-size:0.8rem;font-weight:800;color:#fff;margin-bottom:14px;text-align:center;", title),
      div(class="mom-grid",
        div(class="mom-stat", div(class="mom-val", record),    div(class="mom-label","Record")),
        div(class="mom-stat", div(class="mom-val", pts),       div(class="mom-label","Points")),
        div(class="mom-stat", div(class="mom-val", HTML(paste0('<span class="',streak_cls,'">',streak_raw,'</span>'))), div(class="mom-label","Streak")),
        div(class="mom-stat", div(class="mom-val", l10),       div(class="mom-label","Last 10")),
        div(class="mom-stat", div(class="mom-val", home_r),    div(class="mom-label","Home")),
        div(class="mom-stat", div(class="mom-val", road_r),    div(class="mom-label","Away"))
      )
    )
  }

  # ── OPPONENT SCOUT ─────────────────────────────────────────────────────────
  build_opponent_scout <- function(opp_abbr, standings_df) {
    if (is.null(standings_df)) return(NULL)
    ti <- standings_df[standings_df$teamAbbrev$default == opp_abbr, ]
    if (nrow(ti) == 0) return(NULL)
    gp     <- ti$gamesPlayed %||% 1
    record <- paste0(ti$wins,"-",ti$losses,"-",ti$otLosses)
    pts    <- ti$points
    gfpg   <- sprintf("%.2f", ti$goalFor / gp)
    gapg   <- sprintf("%.2f", ti$goalAgainst / gp)
    l10    <- paste0(ti$l10Wins,"-",ti$l10Losses,"-",ti$l10OtLosses)
    streak_raw <- ti$streakCode %||% ""
    if (nchar(streak_raw) > 0 && !grepl("[0-9]", streak_raw)) streak_raw <- paste0(streak_raw, "1")

    div(class="apple-card",
      div(class="card-title", paste("Opponent Scout —", opp_abbr)),
      div(class="mom-grid",
        div(class="mom-stat", div(class="mom-val", record), div(class="mom-label","Record")),
        div(class="mom-stat", div(class="mom-val", pts),    div(class="mom-label","Points")),
        div(class="mom-stat", div(class="mom-val", HTML(paste0('<span class="streak-badge ',if(grepl("^W",streak_raw))"streak-w"else if(grepl("^L",streak_raw))"streak-l"else"streak-o",'">',streak_raw,'</span>'))), div(class="mom-label","Streak")),
        div(class="mom-stat", div(class="mom-val", gfpg),   div(class="mom-label","GF/G")),
        div(class="mom-stat", div(class="mom-val", gapg),   div(class="mom-label","GA/G")),
        div(class="mom-stat", div(class="mom-val", l10),    div(class="mom-label","Last 10"))
      )
    )
  }

  # ── SCORING LOG ────────────────────────────────────────────────────────────
  build_scoring_log <- function(gc) {
    if (is.null(gc) || !gc$gameState %in% c("LIVE","CRIT","FINAL","OFF")) return(NULL)
    sp <- gc$summary$scoring
    if (is.null(sp) || nrow(sp) == 0) return(NULL)

    blocks <- lapply(1:nrow(sp), function(i) {
      p    <- sp[i,]
      ptitle <- if (p$periodDescriptor$periodType=="OT") "Overtime" else paste("Period", p$periodDescriptor$number)
      goals  <- p$goals[[1]]
      if (is.null(goals) || !is.data.frame(goals) || nrow(goals)==0) return(NULL)
      rows <- lapply(1:nrow(goals), function(j) {
        g     <- goals[j,,drop=FALSE]
        color <- if (g$teamAbbrev == gc$homeTeam$abbrev) "#4DB8FF" else "#FFFFFF"
        sf    <- safe_name(g$firstName); sl <- safe_name(g$lastName)
        ast   <- "Unassisted"
        if ("assists" %in% names(g) && is.list(g$assists) && length(g$assists)>0) {
          adf <- g$assists[[1]]
          if (is.data.frame(adf) && nrow(adf)>0) {
            nms <- trimws(paste(safe_name(adf$firstName), safe_name(adf$lastName)))
            nms <- nms[nms != ""]
            if (length(nms)>0) ast <- paste("Assists:", paste(nms, collapse=", "))
          }
        }
        HTML(unname(paste0('<div class="goal-row"><div style="display:flex;align-items:center;"><div class="goal-time">',g$timeInPeriod,'</div><div class="goal-dot" style="background:',color,';"></div><div><div class="goal-scorer">',sf,' ',sl,' (',g$goalsToDate,')</div><div class="goal-assists">',ast,'</div></div></div><div class="goal-team">',g$teamAbbrev,'</div></div>')))
      })
      div(div(class="period-header", ptitle), div(unname(rows)))
    })
    if (all(sapply(blocks, is.null))) return(NULL)
    div(class="apple-card", div(class="card-title","Box Score Summary"), div(unname(blocks)))
  }

  # ── THREE STARS ────────────────────────────────────────────────────────────
  build_three_stars <- function(gc) {
    if (is.null(gc) || !gc$gameState %in% c("FINAL","OFF")) return(NULL)
    stars <- tryCatch(gc$summary$threeStars, error = function(e) NULL)
    if (is.null(stars) || !is.data.frame(stars) || nrow(stars)==0) return(NULL)
    items <- lapply(1:min(3, nrow(stars)), function(i) {
      s    <- stars[i,]
      pid  <- tryCatch(s$playerId, error=function(e) NA)
      img  <- if (!is.na(pid)) p_img(pid) else ""
      fn   <- tryCatch(safe_name(s$firstName), error=function(e) "")
      ln   <- tryCatch(safe_name(s$lastName),  error=function(e) "")
      team <- tryCatch(s$teamAbbrev, error=function(e) "")
      pos  <- tryCatch(s$position,   error=function(e) "")
      lbl  <- paste0(c("1st Star","2nd Star","3rd Star")[i])
      fb   <- fb_img(paste(fn, ln))
      HTML(unname(paste0(
        '<div class="star-item">',
          '<div class="star-num">',lbl,'</div>',
          '<img class="star-img" src="',img,'" onerror="this.src=\'',fb,'\';">',
          '<div class="star-name">',fn,' ',ln,'</div>',
          '<div class="star-detail">',team,' · ',pos,'</div>',
        '</div>'
      )))
    })
    div(class="apple-card",
      div(class="card-title","Three Stars"),
      div(class="three-stars-grid", HTML(paste(unname(items), collapse="")))
    )
  }

  # ── LIVE GOALIES ───────────────────────────────────────────────────────────
  build_live_goalies <- function(bx) {
    if (is.null(bx) || is.null(bx$playerByGameStats)) return(NULL)
    extract <- function(df, color) {
      if (is.null(df) || !is.data.frame(df) || nrow(df)==0) return(NULL)
      gp <- df
      if      ("timeOnIce"    %in% names(gp)) gp <- gp %>% filter(!is.na(timeOnIce) & timeOnIce!="00:00")
      else if ("toi"          %in% names(gp)) gp <- gp %>% filter(!is.na(toi) & toi!="00:00")
      else if ("shotsAgainst" %in% names(gp)) gp <- gp %>% filter(!is.na(shotsAgainst) & as.numeric(shotsAgainst)>0)
      if (nrow(gp)==0) return(NULL)
      lapply(1:nrow(gp), function(i) {
        g    <- gp[i,]
        nm   <- safe_name(g$name)
        pid  <- if ("playerId" %in% names(g) && !is.na(g$playerId)) g$playerId else NULL
        img  <- if (!is.null(pid)) p_img(pid) else fb_img(nm)
        sv_val <- if ("savePctg" %in% names(g)) suppressWarnings(as.numeric(g$savePctg)) else NA
        sv_pct <- if (is.na(sv_val)) "0.000" else sprintf("%.3f", sv_val)
        saves  <- if ("saves"        %in% names(g) && !is.na(g$saves))        g$saves        else 0
        shots  <- if ("shotsAgainst" %in% names(g) && !is.na(g$shotsAgainst)) g$shotsAgainst else 0
        HTML(unname(paste0(
          '<div style="display:flex;align-items:center;justify-content:space-between;padding:10px 14px;background:rgba(0,0,0,0.25);border:1px solid rgba(255,255,255,0.03);border-radius:14px;margin-bottom:8px;">',
            '<div style="display:flex;align-items:center;">',
              '<div style="width:10px;height:10px;border-radius:50%;background:',color,';margin-right:12px;"></div>',
              '<img class="goalie-img" src="',img,'" onerror="this.src=\'',fb_img(nm),'\';" style="width:40px;height:40px;margin-right:10px;">',
              '<div style="font-weight:800;font-size:1rem;color:#fff;">',nm,'</div>',
            '</div>',
            '<div style="text-align:right;">',
              '<div style="font-weight:900;color:#fff;font-size:1.1rem;">',sv_pct,'</div>',
              '<div style="font-size:0.75rem;color:#8F9191;font-weight:700;">',saves,'/',shots,' SV</div>',
            '</div>',
          '</div>'
        )))
      })
    }
    away_h <- extract(bx$playerByGameStats$awayTeam$goalies, "#FFFFFF")
    home_h <- extract(bx$playerByGameStats$homeTeam$goalies, "#4DB8FF")
    if (is.null(away_h) && is.null(home_h)) return(NULL)
    div(class="apple-card",
      div(class="card-title","Live Goaltending"),
      layout_columns(col_widths=c(6,6), gap="16px",
        div(unname(away_h)), div(unname(home_h)))
    )
  }

  # ── SHOTS BY PERIOD ────────────────────────────────────────────────────────
  build_shots_period <- function(bx, gc) {
    if (is.null(bx) || is.null(gc)) return(NULL)
    if (!gc$gameState %in% c("LIVE","CRIT","FINAL","OFF")) return(NULL)
    bp <- tryCatch(bx$linescore$byPeriod, error=function(e) NULL)
    if (is.null(bp) || !is.data.frame(bp) || nrow(bp)==0) return(NULL)

    away_abbrev <- gc$awayTeam$abbrev
    home_abbrev <- gc$homeTeam$abbrev

    rows <- lapply(1:nrow(bp), function(i) {
      p       <- bp[i,]
      pd      <- tryCatch(p$periodDescriptor, error=function(e) list(number=i, periodType="REG"))
      plbl    <- if (!is.null(pd$periodType) && pd$periodType=="OT") "OT" else paste0("P",pd$number %||% i)
      away_s  <- tryCatch(as.numeric(p$away[[1]]$shotsOnGoal %||% p$away$shotsOnGoal %||% 0), error=function(e) 0)
      home_s  <- tryCatch(as.numeric(p$home[[1]]$shotsOnGoal %||% p$home$shotsOnGoal %||% 0), error=function(e) 0)
      total   <- max(away_s + home_s, 1)
      aw_pct  <- round((away_s / total) * 100)
      hm_pct  <- 100 - aw_pct

      HTML(unname(paste0(
        '<div class="shots-per-row">',
          '<div class="shots-per-label">',plbl,'</div>',
          '<div class="shots-per-bars">',
            '<div class="shots-per-away" style="width:',aw_pct,'%;">',away_s,'</div>',
            '<div class="shots-per-home" style="width:',hm_pct,'%;">',home_s,'</div>',
          '</div>',
        '</div>'
      )))
    })

    div(class="apple-card",
      div(class="card-title","Shots by Period"),
      div(style="display:flex;justify-content:space-between;font-size:0.72rem;font-weight:700;color:#8F9191;margin-bottom:10px;",
        div(HTML(paste0('<span style="display:inline-block;width:10px;height:10px;background:#FFF;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>',away_abbrev))),
        div(HTML(paste0('<span style="display:inline-block;width:10px;height:10px;background:#4DB8FF;border-radius:50%;margin-right:5px;vertical-align:middle;"></span>',home_abbrev)))
      ),
      div(unname(rows))
    )
  }

  # ── MOMENTUM CHART (SVG — two lines, one per team) ────────────────────────
  build_momentum_chart <- function(pbp, gc) {
    if (is.null(pbp) || is.null(gc)) return(NULL)
    if (!gc$gameState %in% c("LIVE","CRIT","FINAL","OFF")) return(NULL)
    plays <- tryCatch(pbp$plays, error=function(e) NULL)
    if (is.null(plays) || nrow(plays)==0) return(NULL)

    shot_types <- c("shot-on-goal","missed-shot","blocked-shot","goal")
    sa <- plays %>% filter(typeDescKey %in% shot_types)
    if (nrow(sa) < 2) return(NULL)

    wpg_id <- tryCatch(
      if (pbp$homeTeam$abbrev == "WPG") pbp$homeTeam$id else pbp$awayTeam$id,
      error=function(e) -1L
    )
    opp_abbrev <- tryCatch(
      if (pbp$homeTeam$abbrev == "WPG") pbp$awayTeam$abbrev else pbp$homeTeam$abbrev,
      error=function(e) "OPP"
    )

    team_ids <- tryCatch(sa$details$eventOwnerTeamId, error=function(e) rep(NA_integer_, nrow(sa)))
    wpg_cum  <- cumsum(ifelse(!is.na(team_ids) & team_ids == wpg_id, 1L, 0L))
    opp_cum  <- cumsum(ifelse(!is.na(team_ids) & team_ids != wpg_id, 1L, 0L))

    n     <- length(wpg_cum)
    y_max <- max(max(wpg_cum), max(opp_cum), 1L)

    # SVG canvas: 500 wide, 120 tall; y=110 is bottom (0 shots), y=10 is top (y_max shots)
    SVG_W <- 500; SVG_H <- 120; PAD_T <- 10; PAD_B <- 10
    plot_h <- SVG_H - PAD_T - PAD_B

    xs      <- round(seq(0, SVG_W, length.out = n))
    wpg_ys  <- round((SVG_H - PAD_B) - (wpg_cum / y_max) * plot_h)
    opp_ys  <- round((SVG_H - PAD_B) - (opp_cum / y_max) * plot_h)

    wpg_pts <- paste(paste0(xs, ",", wpg_ys), collapse=" ")
    opp_pts <- paste(paste0(xs, ",", opp_ys), collapse=" ")

    # Period separator lines: estimate P1/P2 break at ~1/3 and P2/P3 at ~2/3 of plays
    # More accurate: find plays where period changes
    period_xs <- c()
    if ("periodDescriptor" %in% names(sa)) {
      periods <- tryCatch(sa$periodDescriptor$number, error=function(e) NULL)
      if (!is.null(periods)) {
        breaks <- which(diff(periods) > 0)
        period_xs <- round(breaks / n * SVG_W)
      }
    }
    sep_lines <- paste(sapply(period_xs, function(px) {
      paste0('<line x1="',px,'" y1="',PAD_T,'" x2="',px,'" y2="',SVG_H - PAD_B,
             '" stroke="rgba(255,255,255,0.12)" stroke-width="1" stroke-dasharray="3 3"/>')
    }), collapse="")

    # Final score labels at end of each line
    wpg_end_y <- tail(wpg_ys, 1)
    opp_end_y <- tail(opp_ys, 1)

    div(class="apple-card",
      div(class="card-title","Game Momentum — Shot Attempts"),
      div(style="display:flex;justify-content:space-between;align-items:center;margin-bottom:10px;",
        div(style="display:flex;align-items:center;gap:6px;",
          tags$span(style="display:inline-block;width:20px;height:3px;background:#4DB8FF;border-radius:2px;"),
          tags$span(style="font-size:0.72rem;font-weight:800;color:#4DB8FF;", "WPG")
        ),
        div(style="display:flex;align-items:center;gap:6px;",
          tags$span(style="font-size:0.72rem;font-weight:800;color:rgba(255,255,255,0.6);", opp_abbrev),
          tags$span(style="display:inline-block;width:20px;height:3px;background:rgba(255,255,255,0.6);border-radius:2px;")
        )
      ),
      HTML(unname(paste0(
        '<svg style="width:100%;height:130px;overflow:visible;display:block;" viewBox="0 0 ',SVG_W,' ',SVG_H,'" preserveAspectRatio="none">',
          # Grid lines
          '<line x1="0" y1="',SVG_H - PAD_B,'" x2="',SVG_W,'" y2="',SVG_H - PAD_B,
            '" stroke="rgba(255,255,255,0.08)" stroke-width="1"/>',
          sep_lines,
          # OPP fill area (under the line, light)
          '<polygon points="0,',SVG_H - PAD_B,' ',opp_pts,' ',SVG_W,',',SVG_H - PAD_B,
            '" fill="rgba(255,255,255,0.04)"/>',
          # WPG fill area
          '<polygon points="0,',SVG_H - PAD_B,' ',wpg_pts,' ',SVG_W,',',SVG_H - PAD_B,
            '" fill="rgba(77,184,255,0.1)"/>',
          # OPP line
          '<polyline points="',opp_pts,'" fill="none" stroke="rgba(255,255,255,0.55)" stroke-width="2" stroke-linejoin="round"/>',
          # WPG line (on top)
          '<polyline points="',wpg_pts,'" fill="none" stroke="#4DB8FF" stroke-width="2.5" stroke-linejoin="round"/>',
          # End-point dots
          '<circle cx="',SVG_W,'" cy="',wpg_end_y,'" r="4" fill="#4DB8FF"/>',
          '<circle cx="',SVG_W,'" cy="',opp_end_y,'" r="4" fill="rgba(255,255,255,0.6)"/>',
        '</svg>'
      )))
    )
  }

  # ── SHOT MAP (danger-zone dots: size = danger, color = team) ─────────────
  build_shot_map <- function(gc, pbp) {
    if (is.null(gc) || is.null(pbp)) return(NULL)
    if (!gc$gameState %in% c("LIVE","CRIT","FINAL","OFF")) return(NULL)
    plays <- pbp$plays
    if (is.null(plays) || nrow(plays)==0) return(NULL)

    shots <- plays %>% filter(typeDescKey %in% c("shot-on-goal","goal","missed-shot"))
    if (nrow(shots)==0) return(NULL)
    home_id  <- pbp$homeTeam$id
    away_abb <- pbp$awayTeam$abbrev
    home_abb <- pbp$homeTeam$abbrev

    # Danger classification after coordinate normalisation
    # NHL coords: goal lines at x=±89, rink is ±100 wide, ±42.5 tall (feet)
    # After normalisation home attacks right (+x), away attacks left (−x)
    classify_danger <- function(x, y) {
      goal_x <- if (x >= 0) 89 else -89
      dist   <- sqrt((goal_x - x)^2 + y^2)
      in_slot <- abs(y) <= 9
      if (dist <= 20 && in_slot)  return("high")
      if (dist <= 40 && abs(x) >= 54) return("medium")
      return("low")
    }

    dots <- lapply(1:nrow(shots), function(i) {
      s  <- shots[i,]
      x  <- s$details$xCoord;  y  <- s$details$yCoord
      if (is.null(x)||is.na(x)||is.null(y)||is.na(y)) return(NULL)

      tid <- s$details$eventOwnerTeamId
      # Normalise: home attacks right, away attacks left
      if (!is.na(tid) && tid == home_id) {
        if (x < 0) { x <- -x; y <- -y }
      } else {
        if (x > 0) { x <- -x; y <- -y }
      }

      lp  <- ((x + 100) / 200) * 100
      tp  <- ((y + 42.5) / 85)  * 100

      is_goal <- (s$typeDescKey == "goal")
      danger  <- classify_danger(x, y)

      danger_col <- switch(danger,
        high   = "#FF3B30",
        medium = "#FFD60A",
                   "rgba(255,255,255,0.55)"
      )
      sz <- switch(danger, high="13px", medium="10px", "7px")

      if (is_goal) {
        style <- paste0('width:18px;height:18px;background:',danger_col,
                        ';border:2px solid #fff;box-shadow:0 0 8px ',danger_col,';',
                        'animation:pulse 1.5s infinite;')
      } else {
        style <- paste0('width:',sz,';height:',sz,';background:',danger_col,';')
      }

      HTML(unname(paste0(
        '<div class="shot-dot" style="left:',lp,'%;top:',tp,'%;',style,'"></div>'
      )))
    })

    # Legend
    legend <- HTML(paste0(
      '<div style="display:flex;flex-wrap:wrap;justify-content:center;gap:16px;margin-bottom:12px;">',
        '<div style="display:flex;align-items:center;gap:5px;font-size:0.72rem;font-weight:800;color:#FF3B30;">',
          '<span style="display:inline-block;width:13px;height:13px;background:#FF3B30;border-radius:50%;"></span>High Danger',
        '</div>',
        '<div style="display:flex;align-items:center;gap:5px;font-size:0.72rem;font-weight:800;color:#FFD60A;">',
          '<span style="display:inline-block;width:10px;height:10px;background:#FFD60A;border-radius:50%;"></span>Medium',
        '</div>',
        '<div style="display:flex;align-items:center;gap:5px;font-size:0.72rem;font-weight:800;color:#8F9191;">',
          '<span style="display:inline-block;width:7px;height:7px;background:rgba(255,255,255,0.55);border-radius:50%;"></span>Low',
        '</div>',
        '<div style="display:flex;align-items:center;gap:5px;font-size:0.72rem;font-weight:800;color:#8F9191;">',
          '<span style="display:inline-block;width:14px;height:14px;background:#FF3B30;border-radius:50%;border:2px solid #fff;"></span>Goal',
        '</div>',
      '</div>'
    ))

    div(class="apple-card",
      div(class="card-title","Spatial Shot Map"),
      legend,
      div(class="rink-container",
        div(class="rink-center-line"), div(class="rink-blue-line-left"), div(class="rink-blue-line-right"),
        div(class="rink-center-circle"), div(class="goal-line-left"), div(class="goal-line-right"),
        div(class="crease-left"), div(class="crease-right"),
        div(unname(dots))
      )
    )
  }

  # ── SKATER SCORING LEADERS ─────────────────────────────────────────────────
  build_skater_html <- function(df, gradient_class) {
    if (is.null(df) || nrow(df)==0) return(NULL)
    max_pts <- max(df$points, na.rm=TRUE); if (max_pts==0) max_pts <- 1
    HTML(unname(paste0(lapply(1:nrow(df), function(i) {
      r   <- df[i,]
      pct <- (r$points / max_pts) * 100
      fb  <- fb_img(r$full_name)
      paste0('<div class="scorer-row"><img class="scorer-img" src="',r$photo,'" onerror="this.src=\'',fb,'\';">',
             '<div class="scorer-info"><div class="scorer-text"><span>',r$full_name,'</span><span class="scorer-pts">',r$points,' PTS</span></div>',
             '<div class="progress-track"><div class="progress-fill ',gradient_class,'" style="width:',pct,'%;"></div></div></div></div>')
    }), collapse="")))
  }

  # ── GOALIE TANDEM ──────────────────────────────────────────────────────────
  build_tandem_html <- function(df) {
    if (is.null(df) || nrow(df)==0) return(div("No goalie data"))
    HTML(unname(paste0(lapply(1:nrow(df), function(i) {
      r  <- df[i,]
      fb <- fb_img(r$full_name)
      paste0('<div class="goalie-row"><img class="goalie-img" src="',r$photo,'" onerror="this.src=\'',fb,'\';">',
             '<div class="goalie-info"><div class="goalie-name">',r$full_name,'</div>',
             '<div class="goalie-stats">GP: <span class="goalie-stat-hl">',r$gamesPlayed,'</span> | ',
             'GAA: <span class="goalie-stat-hl">',sprintf("%.2f",r$goalsAgainstAverage),'</span> | ',
             'SV%: <span class="goalie-stat-hl">',sprintf("%.3f",r$savePercentage),'</span> | ',
             'W: <span class="goalie-stat-hl">',r$wins %||% "?",'</span></div></div></div>')
    }), collapse="")))
  }

  # ── TEAM STATS PANEL (Team HQ) ─────────────────────────────────────────────
  build_team_stats_panel <- function(standings_df, summary_data) {
    ti <- standings_df[standings_df$teamAbbrev$default == "WPG", ]
    if (nrow(ti)==0) return(NULL)
    gp   <- ti$gamesPlayed %||% 1
    gfpg <- sprintf("%.2f", ti$goalFor   / gp)
    gapg <- sprintf("%.2f", ti$goalAgainst / gp)

    pp_pct <- "–"; pk_pct <- "–"; fo_pct <- "–"; sfpg <- "–"
    if (!is.null(summary_data) && !is.null(summary_data$data) && length(summary_data$data) > 0) {
      d <- if (is.data.frame(summary_data$data)) summary_data$data[1,] else summary_data$data[[1]]
      pp_pct <- tryCatch(paste0(round(d$powerPlayPct * 100, 1), "%"),  error=function(e) "–")
      pk_pct <- tryCatch(paste0(round(d$penaltyKillPct * 100, 1), "%"),error=function(e) "–")
      fo_pct <- tryCatch(paste0(round(d$faceoffWinPct * 100, 1), "%"), error=function(e) "–")
      sfpg   <- tryCatch(sprintf("%.1f", d$shotsForPerGame),            error=function(e) "–")
    }

    mk <- function(v, l) HTML(paste0('<div class="stat-pill"><div class="stat-pill-val">',v,'</div><div class="stat-pill-lbl">',l,'</div></div>'))
    div(class="stat-grid",
      mk(ti$gamesPlayed,"GP"), mk(ti$wins,"W"), mk(ti$losses,"L"), mk(ti$otLosses,"OTL"),
      mk(ti$points,"PTS"),     mk(gfpg,"GF/G"), mk(gapg,"GA/G"), mk(sfpg,"SF/G"),
      mk(pp_pct,"PP%"), mk(pk_pct,"PK%"), mk(fo_pct,"FO%"),
      mk(ti$goalDifferential %||% "–","DIFF")
    )
  }

  # ── ADVANCED STATS (MoneyPuck) ─────────────────────────────────────────────
  build_adv_stats <- function(mp) {
    if (is.null(mp)) return(NULL)
    wpg_row <- tryCatch(
      mp %>% filter(toupper(team) == "WPG" & situation == "all") %>% slice(1),
      error=function(e) NULL
    )
    if (is.null(wpg_row) || nrow(wpg_row)==0) return(NULL)

    # MoneyPuck stores percentages as decimals (0.506 = 50.6%) — multiply by 100
    get_pct <- function(df, ...) {
      for (cn in c(...)) if (cn %in% names(df)) {
        v <- suppressWarnings(as.numeric(df[[cn]]))
        if (!is.na(v)) return(sprintf("%.1f", v * 100))
      }
      "–"
    }
    # PDO is stored near 1.0 (e.g. 1.005) — multiply by 100 to get ~100.5
    get_pdo <- function(df) {
      for (cn in c("pdo","PDO","onIceShotAttemptsPercentage")) if (cn %in% names(df)) {
        v <- suppressWarnings(as.numeric(df[[cn]]))
        if (!is.na(v)) return(sprintf("%.1f", if (v < 5) v * 100 else v))
      }
      "–"
    }
    cf   <- get_pct(wpg_row, "corsiPercentage","CF.","corsiForPercent")
    xgf  <- get_pct(wpg_row, "xGoalsPercentage","xGF.","xGoalsForPercent")
    hdcf <- get_pct(wpg_row, "highDangerShotsForPercentage","HDCF.","highDangerShotAttemptsForPercent")
    pdo  <- get_pdo(wpg_row)

    mk <- function(l, v) HTML(paste0('<div class="adv-row"><div class="adv-label">',l,'</div><div class="adv-val">',v,'%</div></div>'))
    div(class="apple-card",
      div(class="card-title","Advanced Analytics (MoneyPuck)"),
      mk("CF%  — Corsi For %", cf),
      mk("xGF% — Expected Goals For %", xgf),
      mk("HDCF% — High-Danger Chances For %", hdcf),
      HTML(paste0('<div class="adv-row"><div class="adv-label">PDO</div><div class="adv-val">',pdo,'</div></div>'))
    )
  }

  # ── ALL STANDINGS ──────────────────────────────────────────────────────────
  build_all_standings <- function(standings_df) {
    if (is.null(standings_df)) return(div("No standings data"))
    div_order <- c("Atlantic","Metropolitan","Central","Pacific")
    div_map   <- list(Atlantic="A", Metropolitan="M", Central="C", Pacific="P")

    div_blocks <- lapply(div_order, function(dn) {
      sub <- tryCatch(
        standings_df %>% filter(divisionName == dn | divisionAbbrev == div_map[[dn]]) %>% arrange(desc(points)),
        error=function(e) data.frame()
      )
      if (nrow(sub)==0) return(NULL)
      rows <- lapply(1:nrow(sub), function(i) {
        t    <- sub[i,]
        abbr <- safe_name(t$teamAbbrev)
        gd   <- t$goalDifferential
        gd_s <- if (is.numeric(gd) && gd > 0) paste0("+",gd) else as.character(gd)
        is_wpg <- abbr == "WPG"
        fav_cls <- if (is_wpg) " standings-fav" else ""
        HTML(unname(paste0(
          '<div class="standings-row',fav_cls,'">',
            '<div class="standings-team"><img src="',t$teamLogo,'" width="22px"> ',abbr,'</div>',
            '<div class="standings-val">',t$gamesPlayed,'</div>',
            '<div class="standings-val" style="color:#fff;font-weight:900;font-size:1rem;">',t$points,'</div>',
            '<div class="standings-val">',t$wins,'-',t$losses,'-',t$otLosses,'</div>',
            '<div class="standings-val">',gd_s,'</div>',
          '</div>'
        )))
      })
      div(class="div-section",
        div(class="div-title", dn),
        div(class="standings-row standings-header",
          div("TEAM"), div("GP",class="standings-val"), div("PTS",class="standings-val"),
          div("W-L-OT",class="standings-val"), div("DIFF",class="standings-val")
        ),
        div(unname(rows))
      )
    })
    div(unname(div_blocks))
  }

  # ── RECENT RESULTS ─────────────────────────────────────────────────────────
  build_recent_results <- function(schedule_df) {
    if (is.null(schedule_df)) return(NULL)
    done <- schedule_df %>% filter(gameState %in% c("FINAL","OFF")) %>% tail(5)
    if (nrow(done)==0) return(div("No recent games"))
    rows <- lapply(nrow(done):1, function(i) {
      g       <- done[i,]
      is_home <- g$homeTeam$abbrev == "WPG"
      wpg_s   <- if (is_home) g$homeTeam$score else g$awayTeam$score
      opp_s   <- if (is_home) g$awayTeam$score else g$homeTeam$score
      opp_abb <- if (is_home) g$awayTeam$abbrev else g$homeTeam$abbrev
      opp_logo<- if (is_home) g$awayTeam$logo   else g$homeTeam$logo
      ha_lbl  <- if (is_home) "HOME" else "AWAY"
      wpg_s_n <- suppressWarnings(as.numeric(wpg_s))
      opp_s_n <- suppressWarnings(as.numeric(opp_s))
      result  <- if (!is.na(wpg_s_n) && !is.na(opp_s_n) && wpg_s_n > opp_s_n) "W" else "L"
      res_cls <- if (result=="W") "result-w" else "result-l"
      dt      <- tryCatch(format(as.Date(g$gameDate), "%b %d"), error=function(e) "")
      HTML(unname(paste0(
        '<div class="result-row">',
          '<div style="display:flex;align-items:center;gap:10px;">',
            '<img src="',opp_logo,'" width="28px">',
            '<div>',
              '<div style="font-weight:800;color:#fff;font-size:0.9rem;">WPG vs ',opp_abb,'</div>',
              '<div style="font-size:0.7rem;color:#8F9191;">',dt,' · ',ha_lbl,'</div>',
            '</div>',
          '</div>',
          '<div style="display:flex;align-items:center;gap:10px;">',
            '<div style="font-size:1rem;font-weight:900;color:#fff;">',wpg_s,' – ',opp_s,'</div>',
            '<div class="',res_cls,'">',result,'</div>',
          '</div>',
        '</div>'
      )))
    })
    div(unname(rows))
  }

  # ── TODAY'S GAMES ──────────────────────────────────────────────────────────
  build_today_games <- function(score_data) {
    if (is.null(score_data) || is.null(score_data$games)) return(div("No games today"))
    games <- score_data$games
    if (!is.data.frame(games) || nrow(games)==0) return(div("No games today"))
    rows <- lapply(1:nrow(games), function(i) {
      g     <- games[i,]
      state <- g$gameState
      away_a <- tryCatch(g$awayTeam$abbrev, error=function(e)"?")
      home_a <- tryCatch(g$homeTeam$abbrev, error=function(e)"?")
      away_l <- tryCatch(g$awayTeam$logo,   error=function(e)"")
      home_l <- tryCatch(g$homeTeam$logo,   error=function(e)"")
      away_s <- tryCatch(g$awayTeam$score,  error=function(e)"")
      home_s <- tryCatch(g$homeTeam$score,  error=function(e)"")

      if (state %in% c("LIVE","CRIT")) {
        score_html <- paste0('<div class="today-score">',away_s,' – ',home_s,'</div>')
        status_html<- paste0('<div class="today-live-badge">LIVE</div>')
      } else if (state %in% c("FINAL","OFF")) {
        score_html  <- paste0('<div class="today-score">',away_s,' – ',home_s,'</div>')
        status_html <- paste0('<div class="today-final">FINAL</div>')
      } else {
        utc  <- tryCatch(as.POSIXct(g$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ",tz="UTC"), error=function(e) Sys.time())
        loc  <- with_tz(utc, "America/Winnipeg")
        score_html  <- paste0('<div class="today-score" style="font-size:0.85rem;color:#8F9191;">',format(loc,"%I:%M %p"),'</div>')
        status_html <- paste0('<div class="today-final">PRE</div>')
      }
      HTML(unname(paste0(
        '<div class="today-game-row">',
          '<div style="display:flex;align-items:center;gap:8px;">',
            '<img src="',away_l,'" width="28px"><span style="font-weight:800;font-size:0.9rem;color:#fff;">',away_a,'</span>',
          '</div>',
          score_html,
          '<div style="display:flex;align-items:center;gap:8px;">',
            '<span style="font-weight:800;font-size:0.9rem;color:#fff;">',home_a,'</span><img src="',home_l,'" width="28px">',
          '</div>',
          status_html,
        '</div>'
      )))
    })
    div(unname(rows))
  }

  # ── SCORING LEADERS (League tab) ───────────────────────────────────────────
  build_leaders <- function(data, cat_key, label) {
    if (is.null(data)) return(div("No data"))
    leaders <- tryCatch(data[[cat_key]], error=function(e) NULL)
    if (is.null(leaders) || !is.data.frame(leaders) || nrow(leaders)==0) return(div("No data"))
    rows <- lapply(1:nrow(leaders), function(i) {
      p   <- leaders[i,]
      fn  <- safe_name(tryCatch(p$firstName, error=function(e) ""))
      ln  <- safe_name(tryCatch(p$lastName,  error=function(e) ""))
      team<- tryCatch(p$teamAbbrev, error=function(e)"")
      hs  <- tryCatch(p$headshot,   error=function(e)"")
      val <- tryCatch(p$value,      error=function(e)"")
      if (nchar(hs)==0) hs <- fb_img(paste(fn, ln))
      HTML(unname(paste0(
        '<div class="leader-row">',
          '<div class="leader-rank">',i,'</div>',
          '<img class="leader-img" src="',hs,'" onerror="this.src=\'',fb_img(paste(fn,ln)),'\';">',
          '<div class="leader-info">',
            '<div class="leader-name">',fn,' ',ln,'</div>',
            '<div class="leader-team">',team,'</div>',
          '</div>',
          '<div class="leader-stat">',val,'</div>',
        '</div>'
      )))
    })
    div(div(style="font-size:0.7rem;font-weight:700;color:#8F9191;text-align:right;margin-bottom:8px;text-transform:uppercase;letter-spacing:1px;",label),
        div(unname(rows)))
  }

  # ── ROSTER ─────────────────────────────────────────────────────────────────
  build_roster <- function(rdata) {
    if (is.null(rdata)) return(div("Roster unavailable"))
    render_section <- function(players, title, pos_class = "pos-pill") {
      if (is.null(players) || !is.data.frame(players) || nrow(players)==0) return(NULL)
      rows <- lapply(1:nrow(players), function(i) {
        p   <- players[i,]
        fn  <- safe_name(tryCatch(p$firstName, error=function(e)""))
        ln  <- safe_name(tryCatch(p$lastName,  error=function(e)""))
        num <- tryCatch(p$sweaterNumber, error=function(e)"")
        pos <- tryCatch(p$positionCode,  error=function(e)"")
        ht  <- tryCatch(paste0(p$heightInInches %/% 12,"'",p$heightInInches %% 12,'"'), error=function(e)"")
        wt  <- tryCatch(paste0(p$weightInPounds," lbs"), error=function(e)"")
        pid <- tryCatch(p$id, error=function(e) NA)
        img <- if (!is.na(pid)) p_img(pid) else fb_img(paste(fn,ln))
        HTML(unname(paste0(
          '<div class="roster-row">',
            '<img class="roster-img" src="',img,'" onerror="this.src=\'',fb_img(paste(fn,ln)),'\';">',
            '<div class="roster-num">#',num,'</div>',
            '<div style="flex:1;">',
              '<div class="roster-name">',fn,' ',ln,' <span class="',pos_class,'">',pos,'</span></div>',
              '<div class="roster-detail">',ht,' · ',wt,'</div>',
            '</div>',
          '</div>'
        )))
      })
      tagList(div(class="roster-sec-title", title), div(unname(rows)))
    }
    tagList(
      render_section(rdata$forwards,   "Forwards",   "pos-pill"),
      render_section(rdata$defensemen, "Defensemen", "pos-pill"),
      render_section(rdata$goalies,    "Goalies",    "pos-pill pos-pill-g")
    )
  }

  # ────────────────────────────────────────────────────────────────────────────
  # RENDER OUTPUTS
  # ────────────────────────────────────────────────────────────────────────────

  # ── GAMEHUB ─────────────────────────────────────────────────────────────────
  output$next_matchup_ui      <- renderUI({ build_matchup_header(next_game(), next_gc()) })

  output$gamehub_pregame_ui   <- renderUI({
    req(next_game(), team_standings())
    state <- next_game()$gameState
    if (state %in% c("LIVE","CRIT","FINAL","OFF")) return(NULL)
    wpg_is_home <- next_game()$homeTeam$abbrev == "WPG"
    opp_abbr    <- if (wpg_is_home) next_game()$awayTeam$abbrev else next_game()$homeTeam$abbrev
    tagList(
      div(class="apple-card", div(class="card-title","WPG Season Momentum"),
          build_season_momentum("WPG", team_standings())),
      build_opponent_scout(opp_abbr, team_standings())
    )
  })

  output$next_scoring_log_ui  <- renderUI({ build_scoring_log(next_gc()) })
  output$next_live_goalies_ui <- renderUI({ build_live_goalies(next_bx()) })
  output$next_shots_period_ui <- renderUI({ build_shots_period(next_bx(), next_gc()) })
  output$next_momentum_ui     <- renderUI({ build_momentum_chart(next_pbp(), next_gc()) })
  output$next_shot_map_ui     <- renderUI({ build_shot_map(next_gc(), next_pbp()) })

  output$gamehub_scorers_ui <- renderUI({
    req(wpg_stats())
    div(class="apple-card", div(class="card-title","WPG Scoring Leaders"),
        build_skater_html(wpg_stats()$skaters, "home-gradient"))
  })
  output$gamehub_goalie_ui  <- renderUI({
    req(wpg_stats())
    div(class="apple-card", div(class="card-title","WPG Goalie Tandem"),
        build_tandem_html(wpg_stats()$goalies))
  })

  # ── TEAM HQ ─────────────────────────────────────────────────────────────────
  output$hq_momentum_ui <- renderUI({
    req(team_standings())
    build_season_momentum("WPG", team_standings())
  })
  output$hq_stats_ui <- renderUI({
    req(team_standings())
    build_team_stats_panel(team_standings(), wpg_summary())
  })
  output$hq_adv_ui <- renderUI({
    build_adv_stats(adv_stats())
  })
  output$hq_scorers_ui <- renderUI({
    req(wpg_stats())
    build_skater_html(wpg_stats()$skaters, "home-gradient")
  })
  output$hq_goalies_ui <- renderUI({
    req(wpg_stats())
    build_tandem_html(wpg_stats()$goalies)
  })
  output$hq_results_ui <- renderUI({
    req(raw_schedule())
    build_recent_results(raw_schedule())
  })

  # ── SCHEDULE (Team HQ) ──────────────────────────────────────────────────────
  output$schedule_ui <- renderUI({
    req(raw_schedule())
    sched <- raw_schedule() %>%
      filter(as.Date(gameDate) >= lubridate::today(tzone="America/Winnipeg")) %>% head(10)
    items <- lapply(1:nrow(sched), function(i) {
      g   <- sched[i,]
      utc <- as.POSIXct(g$startTimeUTC, format="%Y-%m-%dT%H:%M:%SZ", tz="UTC")
      loc <- with_tz(utc, tzone="America/Winnipeg")
      ha  <- if (g$homeTeam$abbrev == "WPG") "HOME" else "AWAY"
      opp_logo <- if (g$homeTeam$abbrev=="WPG") g$awayTeam$logo else g$homeTeam$logo
      opp_abbr <- if (g$homeTeam$abbrev=="WPG") g$awayTeam$abbrev else g$homeTeam$abbrev
      div(style="display:flex;justify-content:space-between;align-items:center;padding:12px 0;border-bottom:1px solid rgba(255,255,255,0.05);",
        div(style="font-weight:700;font-size:0.9rem;", format(loc,"%b %d")),
        div(style="display:flex;align-items:center;gap:8px;",
            tags$img(src=opp_logo, width="28px"),
            div(opp_abbr, style="font-weight:800;color:#fff;"),
            div(ha, style=paste0("font-size:0.65rem;font-weight:900;padding:2px 8px;border-radius:6px;",
                if(ha=="HOME")"background:rgba(77,184,255,0.15);color:#4DB8FF;" else "background:rgba(143,145,145,0.15);color:#8F9191;"))
        ),
        div(style="color:#8F9191;font-weight:600;font-size:0.85rem;", format(loc,"%I:%M %p"))
      )
    })
    div(unname(items))
  })

  # ── POST-GAME ───────────────────────────────────────────────────────────────
  output$last_matchup_ui      <- renderUI({ build_matchup_header(last_game(), last_gc()) })
  output$last_three_stars_ui  <- renderUI({ build_three_stars(last_gc()) })
  output$last_scoring_log_ui  <- renderUI({ build_scoring_log(last_gc()) })
  output$last_live_goalies_ui <- renderUI({ build_live_goalies(last_bx()) })
  output$last_shots_period_ui <- renderUI({ build_shots_period(last_bx(), last_gc()) })
  output$last_momentum_ui     <- renderUI({ build_momentum_chart(last_pbp(), last_gc()) })
  output$last_shot_map_ui     <- renderUI({ build_shot_map(last_gc(), last_pbp()) })

  # ── LEAGUE ──────────────────────────────────────────────────────────────────
  output$today_games_ui  <- renderUI({ build_today_games(today_games_data()) })
  output$pts_leaders_ui  <- renderUI({ build_leaders(pts_leaders_data(),   "points", "PTS") })
  output$goals_leaders_ui<- renderUI({ build_leaders(goals_leaders_data(), "goals",  "G")   })

  # ── STANDINGS ───────────────────────────────────────────────────────────────
  output$standings_ui <- renderUI({
    req(team_standings())
    build_all_standings(team_standings())
  })

  # ── ROSTER ──────────────────────────────────────────────────────────────────
  output$roster_ui <- renderUI({
    build_roster(roster_data())
  })
}

shinyApp(ui, server)
