library(shiny)
library(tidyverse)
library(bslib)
library(DT)
library(thematic)
library(DBI)
library(RPostgres)
library(bit64) 
library(plotly) 

# --- Initialize Disk Cache (Auto-deletes files older than 4 hours) ---
shinyOptions(cache = cachem::cache_disk("./app_cache", max_age = 4 * 60 * 60))

master_dict <- read_csv("app_data/target_cards_with_epids2.csv", show_col_types = FALSE) %>%
  mutate(
    id = as.character(id), 
    tcgplayer_id = as.integer(tcgplayer_id), 
    cardname = paste(name, replace_na(version, ""), rarity, sep = " - "),
    folder_name = str_replace_all(set_name, "[ ']", "_")
  ) %>%
  select(id, tcgplayer_id, cardname, set_name, folder_name, rarity) %>%
  distinct(id, .keep_all = TRUE)

thematic_shiny()
addResourcePath("card_photos", "app_data/enchanteds/images")

ui <- page_navbar(
  title = tags$span(style = "color: #18bc9c; font-weight: bold; font-size: 22px;", "Lorcana Forecasting"),
  id = "main_nav",
  theme = bs_theme(version = 5, bootswatch = "darkly"),
  
  nav_spacer(),
  nav_item(actionButton("refresh_db", " Refresh Data", icon = icon("sync"), class = "btn-info btn-sm")),
  
  header = tags$head(
    tags$style(HTML("
      .navbar { background-color: #0f171e !important; border-bottom: 2px solid #18bc9c; }
      .navbar .nav-link { color: #ecf0f1 !important; font-size: 16px; opacity: 0.7; transition: 0.3s ease; }
      .navbar .nav-link:hover { opacity: 1; color: #18bc9c !important; }
      .navbar .nav-link.active { color: #18bc9c !important; font-weight: bold; opacity: 1; }
      .nav-underline .nav-link.active { color: #18bc9c !important; font-weight: bold; border-bottom: 3px solid #18bc9c !important; opacity: 1; }
      .momentum-box { background: linear-gradient(135deg, #2b3e50, #1a252f); border-left: 5px solid #f39c12; padding: 15px; border-radius: 8px; margin-bottom: 15px; color: #ecf0f1; font-size: 15px;}
      .green-text { color: #2ecc71; font-weight: bold; }
      .red-text { color: #e74c3c; font-weight: bold; }
      
      /* FLIP CARD LOGIC */
      .flip-card { perspective: 1000px; cursor: pointer; }
      .flip-card-inner { position: relative; width: 100%; height: 100%; transition: transform 0.6s; transform-style: preserve-3d; }
      .flip-card-inner.is-flipped { transform: rotateY(180deg); }
      .flip-card-front, .flip-card-back { position: absolute; width: 100%; height: 100%; backface-visibility: hidden; border-radius: 10px; }
      .flip-card-back { transform: rotateY(180deg); background-color: #1a252f; border: 2px solid #18bc9c; display: flex; flex-direction: column; justify-content: flex-start; align-items: center; padding: 15px 10px; text-align: center; }
      
      /* HIJACKED NATIVE SHINY LOADER */
      .shiny-progress-container {
        position: fixed !important;
        top: 0 !important; left: 0 !important; right: 0 !important; bottom: 0 !important;
        width: 100vw !important; height: 100vh !important;
        background: rgba(15, 23, 30, 0.8) !important;
        backdrop-filter: blur(3px) !important;
        z-index: 9999 !important;
        display: flex !important;
        justify-content: center !important;
        align-items: center !important;
      }
      .shiny-progress {
        background: #1a252f !important;
        padding: 40px 60px !important;
        border-radius: 10px !important;
        border: 2px solid #18bc9c !important;
        text-align: center !important;
        box-shadow: 0 10px 40px rgba(0,0,0,0.9) !important;
        top: auto !important; left: auto !important; right: auto !important;
        width: auto !important;
        position: relative !important;
      }
      .shiny-progress .progress-message { color: #18bc9c !important; font-weight: bold !important; font-size: 18px !important; padding: 0 !important; }
      .shiny-progress::before {
        content: ''; display: block; border: 5px solid #34495e; border-top: 5px solid #18bc9c; border-radius: 50%;
        width: 50px; height: 50px; animation: spin 1s linear infinite; margin: 0 auto 15px auto;
      }
      .shiny-progress .progress-detail { display: none !important; }
      @keyframes spin { 0% { transform: rotate(0deg); } 100% { transform: rotate(360deg); } }
      
      /* THE TRUE BSLIB DROPDOWN FIX */
      .filter-card, .filter-card .card-body { overflow: visible !important; }
      .selectize-dropdown { z-index: 99999 !important; }
    "))
  ),

  nav_panel(title = "Market Overview", value = "Market Overview",
    div(style = "max-width: 1200px; margin: 0 auto; padding-top: 20px;",
      uiOutput("momentum_statement")
    )
  ),
  
  nav_panel(title = "ML Forecasts", value = "Pricing",
    layout_sidebar(
      sidebar = sidebar(
        title = "Card Stats", 
        uiOutput("sidebar_pricing_image")
      ),
      div(
        card(
          class = "filter-card",
          style = "padding: 10px; margin-bottom: 15px; background-color: #2b3e50; border: 1px solid #18bc9c;",
          layout_columns(
            col_widths = c(4, 4, 4),
            selectInput("pricing_set_filter", "Filter by Set:", choices = c("All Sets", sort(unique(master_dict$set_name)))),
            selectizeInput("pricing_selected_card", "Select a Card:", 
                           choices = sort(unique(master_dict$cardname)), 
                           selected = "Stitch - Carefree Surfer - Enchanted"),
            div(
              checkboxGroupInput("show_models", "Select Models:", 
                                 choices = c("Chronos", "15-Day Hybrid GRU"), 
                                 selected = c("Chronos", "15-Day Hybrid GRU"), inline = TRUE),
              checkboxInput("show_ci", "Show Chronos Confidence Interval", value = FALSE)
            )
          )
        ),
        uiOutput("pricing_current_status"),
        
        card(
          card_header("Micro View: 30-Day History & Backtest (Auto-Scaled)"), 
          p(style = "color: #bbb; font-size: 13px; margin-bottom: 0px;", "Dotted/Dashed lines = Live Forecast | Faded solid lines = Past shadow forecasts"),
          plotlyOutput("pricing_zoom_plot", height = "450px")
        ),
        card(
          card_header("Model Accuracy vs. Baseline (Card Specific)"),
          p(style = "color: #bbb; font-size: 13px;", "Comparing Median Absolute Percentage Error (MdAPE) of our models against a 'Persistence Baseline' (assuming the price never changes). Models should consistently fall below the transparent backdrop column."),
          plotlyOutput("error_horizon_plot", height = "300px")
        ),
        card(
          card_header("Macro View: All-Time History & Long Term Forecast"),
          plotlyOutput("pricing_plot", height = "450px"),
          accordion(
            open = FALSE,
            accordion_panel(
              title = "Time-Series Metrics Guide", icon = icon("info-circle"),
              HTML("<div style='color: #ecf0f1; font-size: 14px;'><p><span style='color:#18bc9c;'>Sample Entropy:</span> Lower = Predictable trend. Higher = Erratic noise.</p><p><span style='color:#18bc9c;'>Hurst Exponent:</span> > 0.5 = Trending. < 0.5 = Mean-reverting. ~0.5 = Random walk.</p><p><span style='color:#18bc9c;'>Vol (CV):</span> Standard deviation relative to mean. Standardizes risk comparison.</p><p><span style='color:#18bc9c;'>Skew:</span> Positive = Prone to spikes. Negative = Prone to flash crashes.</p></div>")
            )
          )
        )
      )
    )
  ),

  nav_panel(title = "ML Deeper Dive", value = "ML Deeper Dive",
    layout_sidebar(
      sidebar = sidebar(
        title = "Diagnostic Actions",
        actionButton("calc_diagnostics", " Fetch Latest Run", icon = icon("database"), class = "btn-info btn-sm")
      ),
      div(
        card(
          card_header("Set-Level Market Trajectory & Forecasts"),
          p(style = "color: #bbb; font-size: 14px;", "Comparing median actual price trajectory over the past 30 days alongside the median 30-day forecast for the Chronos and Hybrid GRU models."),
          DTOutput("set_diagnostics_table")
        ),
        card(
          card_header("Card-Level Forecasts & Entropy"),
          p(style = "color: #bbb; font-size: 14px;", "Comparing 30-day projected changes against historical Sample Entropy. Sorted by Entropy to highlight the most unpredictable, erratic pricing behaviors."),
          DTOutput("card_diagnostics_table")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  observeEvent(input$go_to_pricing, {
    updateSelectInput(session, "pricing_set_filter", selected = "All Sets")
    updateSelectizeInput(session, "pricing_selected_card", selected = input$go_to_pricing)
    nav_select("main_nav", "Pricing")
  })

  get_neon_con <- function(retries = 3) {
    for (i in 1:retries) {
      con <- tryCatch({
        dbConnect(RPostgres::Postgres(), host = "ep-frosty-unit-amykrca9.c-5.us-east-1.aws.neon.tech", dbname = "neondb", user = "neondb_owner", password = Sys.getenv("NEON_PASSWORD"), port = 5432, sslmode = "require", connect_timeout = 10)
      }, error = function(e) {
        message(paste("Database connection attempt", i, "failed:", e$message))
        if (i < retries) Sys.sleep(1.5)
        NULL
      })
      if (!is.null(con)) return(con)
    }
    stop("Failed to connect to Neon database after multiple attempts.")
  }

  my_dark_theme <- function() {
    theme_minimal() +
    theme(
      text = element_text(color = "#ecf0f1"), axis.text = element_text(color = "#ecf0f1", size = 14), axis.title = element_text(color = "#ecf0f1", size = 16),
      panel.grid.major = element_line(color = "#34495e", linewidth = 0.3), panel.grid.minor = element_blank(),
      plot.background = element_rect(fill = "#1a252f", color = NA), panel.background = element_rect(fill = "#1a252f", color = NA),
      legend.text = element_text(color = "#ecf0f1", size = 14), legend.title = element_blank(), legend.background = element_rect(fill = "transparent", color = NA)
    )
  }

  clean_plotly_tooltips <- function(p_ly) {
    for (i in seq_along(p_ly$x$data)) {
      trace <- p_ly$x$data[[i]]
      if (!is.null(trace$fill) && trace$fill != "none") {
        p_ly$x$data[[i]]$hoverinfo <- "skip"
      } else if (!is.null(trace$text) && any(grepl("Shadow", trace$text))) {
        p_ly$x$data[[i]]$hoverinfo <- "skip"
        p_ly$x$data[[i]]$connectgaps <- FALSE
      } else {
        p_ly$x$data[[i]]$hovertemplate <- "%{text}<extra></extra>"
        p_ly$x$data[[i]]$connectgaps <- FALSE
      }
    }
    return(p_ly)
  }

  force_pure_date <- function(date_col) { as.Date(substr(as.character(date_col), 1, 10)) }

  observeEvent(input$pricing_set_filter, ignoreInit = TRUE, {
    if (input$pricing_set_filter == "All Sets") {
      choices <- sort(unique(master_dict$cardname))
    } else {
      choices <- master_dict %>% filter(set_name == input$pricing_set_filter) %>% pull(cardname) %>% sort()
    }
    updateSelectizeInput(session, "pricing_selected_card", choices = choices, selected = choices[1], server = TRUE)
  })

  # --- CACHED REACTIVE: General Summary Data (Lean Version) ---
  summary_data <- reactive({
    withProgress(message = 'Crunching Market Summaries...', value = 0.5, {
      con <- get_neon_con()
      
      latest_prices <- dbGetQuery(con, "SELECT DISTINCT ON (tcgplayer_id) tcgplayer_id, market_price, pull_date FROM justtcg_prices ORDER BY tcgplayer_id, pull_date DESC")
      past_prices <- dbGetQuery(con, "SELECT DISTINCT ON (tcgplayer_id) tcgplayer_id, market_price, pull_date FROM justtcg_prices WHERE pull_date <= CURRENT_DATE - INTERVAL '7 days' ORDER BY tcgplayer_id, pull_date DESC")
      past_30_prices <- dbGetQuery(con, "SELECT DISTINCT ON (tcgplayer_id) tcgplayer_id, market_price as price_30d_ago, pull_date FROM justtcg_prices WHERE pull_date <= CURRENT_DATE - INTERVAL '30 days' ORDER BY tcgplayer_id, pull_date DESC")
      dbDisconnect(con)
        
      list(latest = latest_prices, past = past_prices, past_30 = past_30_prices)
    })
  }) %>% 
  bindCache(Sys.Date()) %>% 
  bindEvent(input$refresh_db, ignoreNULL = FALSE)

  # --- CACHED REACTIVE: Heavy Pricing & ML Forecast Data ---
  pricing_details <- reactive({
    req(input$pricing_selected_card)
    ids <- master_dict$tcgplayer_id[master_dict$cardname == input$pricing_selected_card]
    id_str_list <- paste0("'", ids, "'", collapse = ",")
    
    withProgress(message = "Pulling Deep Forecasting Data...", {
      con <- get_neon_con()
      hist <- dbGetQuery(con, sprintf("SELECT tcgplayer_id, market_price, pull_date FROM justtcg_prices WHERE tcgplayer_id = %s", ids[1]))
      metrics <- tryCatch(dbGetQuery(con, sprintf("SELECT * FROM card_ts_metrics WHERE tcgplayer_id = '%s'", ids[1])), error = function(e) data.frame())
      runs_tbl <- tryCatch(dbGetQuery(con, "SELECT run_id, run_date FROM model_runs"), error = function(e) data.frame(run_id = integer(), run_date = as.Date(character())))
      runs_tbl <- runs_tbl %>% mutate(run_date = force_pure_date(run_date))

      c_pred <- tryCatch(dbGetQuery(con, sprintf("SELECT card_id as tcgplayer_id, target_date, pred_price, conf_low, conf_high, run_id FROM chronos_predictions WHERE card_id IN (%s)", id_str_list)), error = function(e) data.frame())
      g_pred <- tryCatch(dbGetQuery(con, sprintf("SELECT card_id as tcgplayer_id, target_date, pred_price, run_id FROM gru_predictions WHERE card_id IN (%s)", id_str_list)), error = function(e) data.frame())
      dbDisconnect(con)
      
      hist <- hist %>% mutate(pull_date = force_pure_date(pull_date)) %>% group_by(tcgplayer_id, pull_date) %>% slice_tail(n = 1) %>% ungroup() %>% left_join(master_dict, by = "tcgplayer_id")

      if(nrow(c_pred) > 0) {
        c_pred <- c_pred %>% mutate(target_date = force_pure_date(target_date), tcgplayer_id = as.integer(tcgplayer_id)) %>% left_join(runs_tbl, by = "run_id") %>% group_by(tcgplayer_id, target_date, run_id) %>% slice_tail(n = 1) %>% ungroup() %>% left_join(master_dict, by = "tcgplayer_id")
        max_c_run <- max(c_pred$run_id, na.rm = TRUE)
        chronos_cur <- c_pred %>% filter(run_id == max_c_run)
        chronos_shadow <- c_pred %>% filter(run_id < max_c_run)
      } else { chronos_cur <- data.frame(); chronos_shadow <- data.frame() }
      
      if(nrow(g_pred) > 0) {
        g_pred <- g_pred %>% mutate(target_date = force_pure_date(target_date), tcgplayer_id = as.integer(tcgplayer_id)) %>% left_join(runs_tbl, by = "run_id") %>% group_by(tcgplayer_id, target_date, run_id) %>% slice_tail(n = 1) %>% ungroup() %>% left_join(master_dict, by = "tcgplayer_id")
        max_g_run <- max(g_pred$run_id, na.rm = TRUE)
        gru_cur <- g_pred %>% filter(run_id == max_g_run)
        gru_shadow <- g_pred %>% filter(run_id < max_g_run)
      } else { gru_cur <- data.frame(); gru_shadow <- data.frame() }

      list(hist = hist, chronos = chronos_cur, chronos_shadow = chronos_shadow, gru = gru_cur, gru_shadow = gru_shadow, metrics = metrics)
    })
  }) %>% bindCache(input$pricing_selected_card)

  # --- CACHED REACTIVE: Deep Dive Unified Data ---
  ml_diag_data <- reactive({
    withProgress(message = 'Crunching Global Unified Data...', value = 0.5, {
      con <- get_neon_con()
      
      metrics_df <- tryCatch(dbGetQuery(con, "SELECT tcgplayer_id, samp_ent_30d FROM card_ts_metrics"), error = function(e) data.frame())
      c_pred <- tryCatch(dbGetQuery(con, "SELECT card_id as tcgplayer_id, target_date, pred_price FROM chronos_predictions WHERE run_id = (SELECT MAX(run_id) FROM chronos_predictions)"), error = function(e) data.frame())
      g_pred <- tryCatch(dbGetQuery(con, "SELECT card_id as tcgplayer_id, target_date, pred_price FROM gru_predictions WHERE run_id = (SELECT MAX(run_id) FROM gru_predictions)"), error = function(e) data.frame())
      dbDisconnect(con)
      
      if(nrow(metrics_df) > 0) metrics_df$tcgplayer_id <- as.integer(metrics_df$tcgplayer_id)
      
      c_30 <- data.frame(tcgplayer_id=integer(), chronos_pred=numeric())
      g_30 <- data.frame(tcgplayer_id=integer(), gru_pred=numeric())
      
      if(nrow(c_pred) > 0) {
        c_30 <- c_pred %>% mutate(tcgplayer_id = as.integer(tcgplayer_id)) %>% group_by(tcgplayer_id) %>% filter(target_date == max(target_date)) %>% slice_tail(n=1) %>% ungroup() %>% select(tcgplayer_id, chronos_pred = pred_price)
      }
      if(nrow(g_pred) > 0) {
        g_30 <- g_pred %>% mutate(tcgplayer_id = as.integer(tcgplayer_id)) %>% group_by(tcgplayer_id) %>% filter(target_date == max(target_date)) %>% slice_tail(n=1) %>% ungroup() %>% select(tcgplayer_id, gru_pred = pred_price)
      }
      
      req(summary_data())
      actuals <- summary_data()$latest %>% select(tcgplayer_id, current_price = market_price)
      actuals_30d <- summary_data()$past_30 %>% select(tcgplayer_id, price_30d_ago)
      
      unified <- master_dict %>%
        left_join(actuals, by = "tcgplayer_id") %>%
        left_join(actuals_30d, by = "tcgplayer_id")
        
      if(nrow(metrics_df) > 0) {
         unified <- unified %>% left_join(metrics_df, by = "tcgplayer_id")
      } else {
         unified <- unified %>% mutate(samp_ent_30d = NA)
      }
      
      if(nrow(c_30) > 0) unified <- unified %>% left_join(c_30, by = "tcgplayer_id") else unified <- unified %>% mutate(chronos_pred = NA)
      if(nrow(g_30) > 0) unified <- unified %>% left_join(g_30, by = "tcgplayer_id") else unified <- unified %>% mutate(gru_pred = NA)
      
      unified <- unified %>%
        rowwise() %>%
        mutate(
          blended_pred = mean(c(chronos_pred, gru_pred), na.rm = TRUE)
        ) %>%
        ungroup() %>%
        mutate(
          actual_30d_change_abs = current_price - price_30d_ago,
          actual_30d_change_pct = (current_price - price_30d_ago) / price_30d_ago,
          chronos_change_pct = (chronos_pred - current_price) / current_price,
          gru_change_pct = (gru_pred - current_price) / current_price,
          blended_change_pct = (blended_pred - current_price) / current_price
        )
        
      set_summary <- unified %>%
        filter(set_name != "Promo Set 2", rarity != "Epic") %>%
        group_by(Set = set_name) %>%
        summarise(
          `Median 30d Trajectory ($)` = median(actual_30d_change_abs, na.rm = TRUE),
          `Median 30d Trajectory (%)` = median(actual_30d_change_pct, na.rm = TRUE),
          `Chronos 30d Forecast (%)` = median(chronos_change_pct, na.rm = TRUE),
          `GRU 30d Forecast (%)` = median(gru_change_pct, na.rm = TRUE),
          `Avg 30d Trend (%)` = median(blended_change_pct, na.rm = TRUE),
          `Avg Sample Entropy` = mean(samp_ent_30d, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        arrange(desc(`Avg Sample Entropy`))
        
      card_table_df <- unified %>%
        mutate(
          Image = paste0('<img src="card_photos/', folder_name, '/', id, '.avif" style="height: 75px; border-radius: 4px; box-shadow: 0 2px 4px rgba(0,0,0,0.5);">'),
          Action = paste0('<button class="btn btn-sm" style="background-color:#18bc9c; color:white; font-weight:bold;" onclick="Shiny.setInputValue(\'go_to_pricing\', \'', str_replace_all(cardname, "'", "\\\\'"), '\', {priority: \'event\'})">View Forecast</button>')
        ) %>%
        select(Action, Image, Card = cardname, `Current Price` = current_price,
               `Avg 30d Forecast ($)` = blended_pred,
               `Avg 30d Trend (%)` = blended_change_pct,
               `Chronos 30d Est.` = chronos_change_pct,
               `GRU 30d Est.` = gru_change_pct,
               `Sample Entropy` = samp_ent_30d) %>%
        arrange(desc(`Sample Entropy`))
        
      list(set_data = set_summary, card_data = card_table_df)
    })
  }) %>% 
  bindCache(Sys.Date()) %>% 
  bindEvent(input$calc_diagnostics, ignoreNULL = FALSE)
  
  output$set_diagnostics_table <- renderDT({
    df <- ml_diag_data()$set_data
    req(df)
    datatable(df, options = list(pageLength = 10, dom = 't'), rownames = FALSE) %>%
      formatCurrency("Median 30d Trajectory ($)", currency = "$") %>%
      formatPercentage(c("Median 30d Trajectory (%)", "Chronos 30d Forecast (%)", "GRU 30d Forecast (%)", "Avg 30d Trend (%)"), 2) %>%
      formatRound("Avg Sample Entropy", 3) %>%
      formatStyle('Median 30d Trajectory (%)', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Chronos 30d Forecast (%)', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('GRU 30d Forecast (%)', color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle('Avg 30d Trend (%)', color = styleInterval(0, c('#e74c3c', '#2ecc71')))
  })

  output$card_diagnostics_table <- renderDT({
    df <- ml_diag_data()$card_data
    req(df)
    datatable(df, escape = FALSE, options = list(pageLength = 15, dom = 'tip'), rownames = FALSE) %>%
      formatCurrency(c("Current Price", "Avg 30d Forecast ($)"), currency = "$") %>%
      formatPercentage(c("Chronos 30d Est.", "GRU 30d Est.", "Avg 30d Trend (%)"), 2) %>%
      formatRound("Sample Entropy", 3) %>%
      formatStyle("Chronos 30d Est.", color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle("GRU 30d Est.", color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle("Avg 30d Trend (%)", color = styleInterval(0, c('#e74c3c', '#2ecc71'))) %>%
      formatStyle("Avg 30d Forecast ($)", valueColumns = "Avg 30d Trend (%)", color = styleInterval(0, c('#e74c3c', '#2ecc71')))
  })
  
  market_movers <- reactive({
    req(summary_data())
    s <- summary_data()
    momentum <- s$latest %>% inner_join(s$past, by = "tcgplayer_id", suffix = c("_cur", "_past")) %>% mutate(pct = (market_price_cur - market_price_past)/market_price_past * 100, abs = market_price_cur - market_price_past) %>% left_join(master_dict, by = "tcgplayer_id") %>% arrange(desc(pct))
    if(nrow(momentum) == 0) return(NULL)
    top_pct_g <- momentum %>% arrange(desc(pct)) %>% slice(1) %>% mutate(Category = "Top % Gainer")
    top_pct_l <- momentum %>% arrange(pct) %>% slice(1) %>% mutate(Category = "Top % Loser")
    top_abs_g <- momentum %>% arrange(desc(abs)) %>% slice(1) %>% mutate(Category = "Top $ Gainer")
    top_abs_l <- momentum %>% arrange(abs) %>% slice(1) %>% mutate(Category = "Top $ Loser")
    bind_rows(top_pct_g, top_pct_l, top_abs_g, top_abs_l)
  })

  # --- Unified Database Pull for the 4 Sparklines ---
  market_movers_hist <- reactive({
    info <- market_movers(); req(info)
    con <- get_neon_con()
    id_list <- paste(unique(info$tcgplayer_id), collapse=",")
    hist_data <- dbGetQuery(con, sprintf("SELECT tcgplayer_id, pull_date, market_price FROM justtcg_prices WHERE tcgplayer_id IN (%s) AND pull_date >= CURRENT_DATE - INTERVAL '7 days'", id_list))
    dbDisconnect(con)
    hist_data %>% mutate(pull_date = force_pure_date(pull_date))
  })

  # --- Sparkline Build Function (FIXED: Added hovermode = "closest") ---
  build_sparkline <- function(h_data, cat_name) {
    if(nrow(h_data) == 0) return(plotly_empty(type = "scatter", mode = "lines"))
    
    h_data <- h_data %>% arrange(pull_date)
    line_color <- ifelse(grepl("Gainer", cat_name), "#2ecc71", "#e74c3c")
    
    p <- ggplot(h_data, aes(x = pull_date, y = market_price, group = 1, text = paste0(format(pull_date, "%m/%d"), "<br>", scales::dollar(market_price)))) +
      geom_line(color = line_color, linewidth = 1.5) +
      geom_point(color = line_color, size = 3) +
      my_dark_theme() +
      theme(
        axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(),
        panel.grid = element_blank(), plot.margin = margin(0,0,0,0)
      )
      
    ggplotly(p, tooltip = "text") %>%
      style(hovertemplate = "%{text}<extra></extra>") %>%
      layout(
        xaxis = list(visible = FALSE, fixedrange = TRUE),
        yaxis = list(visible = FALSE, fixedrange = TRUE),
        margin = list(t=10, b=10, l=10, r=10),
        hovermode = "closest",
        showlegend = FALSE,
        plot_bgcolor = "rgba(0,0,0,0)",
        paper_bgcolor = "rgba(0,0,0,0)"
      ) %>% config(displayModeBar = FALSE)
  }

  output$spark_pct_g <- renderPlotly({
    info <- market_movers(); req(info)
    hist <- market_movers_hist(); req(hist)
    row <- info %>% filter(Category == "Top % Gainer")
    h <- hist %>% filter(tcgplayer_id == row$tcgplayer_id)
    build_sparkline(h, "Top % Gainer")
  })

  output$spark_pct_l <- renderPlotly({
    info <- market_movers(); req(info)
    hist <- market_movers_hist(); req(hist)
    row <- info %>% filter(Category == "Top % Loser")
    h <- hist %>% filter(tcgplayer_id == row$tcgplayer_id)
    build_sparkline(h, "Top % Loser")
  })

  output$spark_abs_g <- renderPlotly({
    info <- market_movers(); req(info)
    hist <- market_movers_hist(); req(hist)
    row <- info %>% filter(Category == "Top $ Gainer")
    h <- hist %>% filter(tcgplayer_id == row$tcgplayer_id)
    build_sparkline(h, "Top $ Gainer")
  })

  output$spark_abs_l <- renderPlotly({
    info <- market_movers(); req(info)
    hist <- market_movers_hist(); req(hist)
    row <- info %>% filter(Category == "Top $ Loser")
    h <- hist %>% filter(tcgplayer_id == row$tcgplayer_id)
    build_sparkline(h, "Top $ Loser")
  })

  output$momentum_statement <- renderUI({
    info <- market_movers(); req(info)
    req(summary_data())
    
    max_d <- max(summary_data()$latest$pull_date, na.rm=TRUE)
    min_d <- max_d - 7
    date_str <- paste0(format(min_d, "%m/%d"), " - ", format(max_d, "%m/%d"))
    
    t_pct_g <- info %>% filter(Category == "Top % Gainer") %>% slice(1)
    t_pct_l <- info %>% filter(Category == "Top % Loser") %>% slice(1)
    t_abs_g <- info %>% filter(Category == "Top $ Gainer") %>% slice(1)
    t_abs_l <- info %>% filter(Category == "Top $ Loser") %>% slice(1)
    
    build_mover_card <- function(row, lab, plot_id) {
      p_c <- ifelse(row$pct >= 0, "green-text", "red-text")
      
      front <- tags$div(class = "flip-card-front", style = "background-color: #2b3e50; border: 2px solid #34495e; display: flex; flex-direction: column; align-items: center; justify-content: center; padding: 15px 10px;",
        tags$div(style = "font-size: 15px; font-weight: bold; color: #18bc9c; text-transform: uppercase; margin-bottom: 10px;", lab),
        tags$img(src = paste0("card_photos/", row$folder_name, "/", row$id, ".avif"), style = "height: 180px; border-radius: 5px; box-shadow: 0 4px 8px rgba(0,0,0,0.5);"),
        tags$div(style = "margin-top: 12px; font-size: 14px; font-weight: bold; color: #ecf0f1; height: 36px; line-height: 1.2;", row$cardname),
        tags$div(style = "margin-top: 8px; font-size: 16px; font-weight: bold; color: #f1c40f;", scales::dollar(row$market_price_cur)),
        tags$div(class = p_c, style = "font-size: 15px; font-weight: bold; margin-top: 2px;", sprintf("%s%s (%.1f%%)", ifelse(row$abs >= 0, "+", ""), scales::dollar(row$abs), row$pct))
      )
      
      back <- tags$div(class = "flip-card-back",
        tags$span(style = "border-bottom: 1px solid #18bc9c; padding-bottom: 5px; margin-bottom: 10px; font-weight: bold; font-size: 18px;", lab),
        tags$span(style = "font-size: 14px; color: #bbb;", "Current Price"),
        tags$span(style = "font-size: 26px; font-weight: bold; color: #f1c40f; margin-bottom: 10px;", scales::dollar(row$market_price_cur)),
        tags$div(style = "flex-grow: 1; display: flex; align-items: center; justify-content: center; width: 100%;",
          plotlyOutput(plot_id, width = "220px", height = "160px")
        )
      )
      
      tags$div(style = "display: flex; flex-direction: column; align-items: center; width: 260px; text-align: center; margin: 10px;",
        tags$div(class = "flip-card", onclick = "this.querySelector('.flip-card-inner').classList.toggle('is-flipped');", style = "width: 260px; height: 400px;",
          tags$div(class = "flip-card-inner", front, back)
        )
      )
    }
    
    tagList(
      div(class="momentum-box", 
          tags$span(sprintf("7-Day Market Momentum (%s): ", date_str)), 
          sprintf("The biggest jump was %s (+%.1f%%).", t_pct_g$cardname, t_pct_g$pct)
      ), 
      tags$div(style = "display: flex; justify-content: space-around; background: #1a252f; padding: 20px 10px; border-radius: 8px; margin-bottom: 20px;", 
        build_mover_card(t_pct_g, "Top % Gainer", "spark_pct_g"), 
        build_mover_card(t_pct_l, "Top % Loser", "spark_pct_l"), 
        build_mover_card(t_abs_g, "Top $ Gainer", "spark_abs_g"), 
        build_mover_card(t_abs_l, "Top $ Loser", "spark_abs_l")
      )
    )
  })

  output$sidebar_pricing_image <- renderUI({ 
    req(input$pricing_selected_card, pricing_details())
    info <- master_dict %>% filter(cardname == input$pricing_selected_card) %>% slice(1)
    metrics_data <- pricing_details()$metrics
    m_row <- if(!is.null(metrics_data) && nrow(metrics_data) > 0) metrics_data %>% filter(tcgplayer_id == as.character(info$tcgplayer_id)) else data.frame()
    
    d <- pricing_details()
    c_target <- if(nrow(d$chronos) > 0) scales::dollar(d$chronos$pred_price[nrow(d$chronos)]) else "N/A"
    g_target <- if(nrow(d$gru) > 0) scales::dollar(d$gru$pred_price[nrow(d$gru)]) else "N/A"
    
    tags$div(
      style = "display:flex; flex-direction:column; align-items:center; background: #1a252f; border-radius: 8px; padding: 15px; border: 1px solid #34495e;",
      tags$img(src=paste0("card_photos/", info$folder_name, "/", info$id, ".avif"), style="width:100%; max-width: 160px; border-radius:8px; box-shadow: 0 4px 8px rgba(0,0,0,0.8); margin-bottom: 15px;"), 
      tags$div(style="font-size:14px; color:#ecf0f1; margin-bottom: 8px; border-bottom: 1px solid #18bc9c; padding-bottom: 4px; text-align: center; width: 100%;", info$cardname),
      
      tags$div(
        style = "width: 100%; text-align: left; margin-top: 5px; margin-bottom: 10px;",
        tags$div(style = "display: flex; justify-content: space-between; font-size: 14px;", 
                 tags$span(style="color: #f1c40f;", "Chronos 30-Day:"), c_target),
        tags$div(style = "display: flex; justify-content: space-between; font-size: 14px;", 
                 tags$span(style="color: #2ecc71;", "GRU 30-Day:"), g_target)
      ),

      if(nrow(m_row) > 0) {
        tags$div(
          style = "width: 100%; text-align: left;",
          tags$div(style = "font-size: 13px; color: #18bc9c; margin-top: 5px; border-bottom: 1px solid #34495e; padding-bottom: 2px;", "30-Day Metrics"),
          tags$div(
            style = "font-size: 13px; color: #bbb; line-height: 1.6; padding-top: 5px; margin-bottom: 10px;",
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "Entropy: "), round(m_row$samp_ent_30d, 4)),
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "Hurst: "), round(m_row$hurst_30d, 4)),
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "Vol (CV): "), round(m_row$cv_30d, 4)),
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "Skew: "), round(m_row$skewness_30d, 4))
          ),
          
          tags$div(style = "font-size: 13px; color: #18bc9c; margin-top: 5px; border-bottom: 1px solid #34495e; padding-bottom: 2px;", "Tracking Stats"),
          tags$div(
            style = "font-size: 13px; color: #bbb; line-height: 1.6; padding-top: 5px;",
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "30d Data Points: "), m_row$days_in_30d),
            tags$div(style="display: flex; justify-content: space-between;", tags$span(style="color:#f39c12;", "Lifetime Tracked: "), paste(m_row$lifetime_days, "days"))
          )
        )
      } else {
        tags$div(style = "font-size: 12px; font-style: italic; color: #bbb;", "Metrics pipeline not executed.")
      }
    )
  })

  output$pricing_current_status <- renderUI({
    req(pricing_details())
    d <- pricing_details()
    req(nrow(d$hist) > 0)
    
    latest_pull <- max(d$hist$pull_date, na.rm = TRUE)
    curr_price <- d$hist %>% filter(pull_date == latest_pull) %>% pull(market_price) %>% .[1]
    
    c_val <- if(nrow(d$chronos) > 0) d$chronos$pred_price[nrow(d$chronos)] else NA
    g_val <- if(nrow(d$gru) > 0) d$gru$pred_price[nrow(d$gru)] else NA
    
    preds <- c(c_val, g_val)
    preds <- preds[!is.na(preds)]
    
    if(length(preds) > 0) {
      avg_pred <- mean(preds)
      trend_pct <- (avg_pred - curr_price) / curr_price
      trend_color <- ifelse(trend_pct >= 0, "#2ecc71", "#e74c3c")
      trend_icon <- ifelse(trend_pct >= 0, "▲", "▼")
      
      ensemble_html <- tags$div(style = "text-align: center;",
        tags$span(style = "color: #bbb; font-size: 14px; text-transform: uppercase; letter-spacing: 1px;", "30-Day Ensemble Forecast"),
        tags$br(),
        tags$span(style = paste0("color: ", trend_color, "; font-size: 28px; font-weight: bold;"), 
                  scales::dollar(avg_pred)),
        tags$span(style = paste0("color: ", trend_color, "; font-size: 16px; margin-left: 8px;"), 
                  sprintf("%s %s", trend_icon, scales::percent(abs(trend_pct), accuracy = 0.1)))
      )
    } else {
      ensemble_html <- tags$div(style = "text-align: center;",
        tags$span(style = "color: #bbb; font-size: 14px; text-transform: uppercase; letter-spacing: 1px;", "30-Day Ensemble Forecast"),
        tags$br(),
        tags$span(style = "color: #7f8c8d; font-size: 20px; font-weight: bold;", "N/A")
      )
    }
    
    tags$div(
      style = "display: flex; justify-content: space-between; align-items: center; background: linear-gradient(135deg, #2b3e50, #1a252f); padding: 15px 20px; border-radius: 8px; border-left: 5px solid #3498db; margin-bottom: 15px;",
      tags$div(
        tags$span(style = "color: #bbb; font-size: 14px; text-transform: uppercase; letter-spacing: 1px;", "Current Market Price"),
        tags$br(),
        tags$span(style = "color: #3498db; font-size: 28px; font-weight: bold;", scales::dollar(curr_price))
      ),
      ensemble_html,
      tags$div(style = "text-align: right;",
        tags$span(style = "color: #bbb; font-size: 14px;", "Last Updated"),
        tags$br(),
        tags$span(style = "color: #ecf0f1; font-size: 16px; font-weight: bold;", format(latest_pull, "%B %d, %Y"))
      )
    )
  })

  output$pricing_zoom_plot <- renderPlotly({
    req(pricing_details()); d <- pricing_details()
    latest_pull <- max(d$hist$pull_date, na.rm = TRUE)
    
    z_hist <- d$hist %>% filter(pull_date >= latest_pull - 30) %>% rename(plot_date = pull_date)
    current_anchors <- z_hist %>% filter(plot_date == latest_pull) %>% select(tcgplayer_id, cardname, plot_date, pred_price = market_price)

    p <- ggplot()
      
    if("Chronos" %in% input$show_models) {
      if(nrow(d$chronos) > 0) {
        z_chronos <- d$chronos %>% filter(target_date > latest_pull & target_date <= latest_pull + 30) %>% rename(plot_date = target_date)
        if(nrow(z_chronos)>0){
          c_anchor <- current_anchors %>% filter(cardname %in% z_chronos$cardname) %>% mutate(conf_low = pred_price, conf_high = pred_price)
          z_c_bridged <- bind_rows(c_anchor, z_chronos) %>% arrange(cardname, plot_date)
          
          p <- p + geom_line(data=z_c_bridged, aes(x=plot_date, y=pred_price, group=cardname, 
                                                 text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>Chronos Forecast: ", scales::dollar(pred_price))), color="#f1c40f", linetype="dashed", linewidth=1.2)
          if(input$show_ci) {
             p <- p + geom_ribbon(data=z_c_bridged, aes(x=plot_date, ymin=conf_low, ymax=conf_high, group=cardname), fill="#f1c40f", alpha=0.15)
          }
        }
      }
      
      if(nrow(d$chronos_shadow) > 0) {
        z_c_shadow <- d$chronos_shadow %>% filter(target_date > run_date & target_date <= latest_pull + 30) %>% rename(plot_date = target_date)
        if(nrow(z_c_shadow)>0){
          c_shadow_anchors <- z_c_shadow %>% distinct(cardname, run_id, run_date) %>% left_join(d$hist %>% select(cardname, pull_date, market_price), by = c("cardname", "run_date" = "pull_date")) %>% filter(!is.na(market_price)) %>% select(cardname, run_id, plot_date = run_date, pred_price = market_price)
          z_c_shadow_bridged <- bind_rows(c_shadow_anchors, z_c_shadow) %>% arrange(cardname, run_id, plot_date)

          p <- p + geom_line(data=z_c_shadow_bridged, aes(x=plot_date, y=pred_price, group=interaction(cardname, run_id), 
                                           text="Shadow"), color="#f1c40f", linewidth=0.5, alpha=0.2)
        }
      }
    }
    
    if("15-Day Hybrid GRU" %in% input$show_models) {
      if(nrow(d$gru) > 0) {
        z_gru <- d$gru %>% filter(target_date > latest_pull & target_date <= latest_pull + 30) %>% rename(plot_date = target_date)
        if(nrow(z_gru)>0){
          g_anchor <- current_anchors %>% filter(cardname %in% z_gru$cardname)
          z_g_bridged <- bind_rows(g_anchor, z_gru) %>% arrange(cardname, plot_date)
          
          p <- p + geom_line(data=z_g_bridged, aes(x=plot_date, y=pred_price, group=cardname, 
                                             text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>GRU Forecast: ", scales::dollar(pred_price))), color="#2ecc71", linetype="dotted", linewidth=1.2)
        }
      }

      if(nrow(d$gru_shadow) > 0) {
        z_g_shadow <- d$gru_shadow %>% filter(target_date > run_date & target_date <= latest_pull + 30) %>% rename(plot_date = target_date)
        if(nrow(z_g_shadow)>0){
          g_shadow_anchors <- z_g_shadow %>% distinct(cardname, run_id, run_date) %>% left_join(d$hist %>% select(cardname, pull_date, market_price), by = c("cardname", "run_date" = "pull_date")) %>% filter(!is.na(market_price)) %>% select(cardname, run_id, plot_date = run_date, pred_price = market_price)
          z_g_shadow_bridged <- bind_rows(g_shadow_anchors, z_g_shadow) %>% arrange(cardname, run_id, plot_date)

          p <- p + geom_line(data=z_g_shadow_bridged, aes(x=plot_date, y=pred_price, group=interaction(cardname, run_id), 
                                           text="Shadow"), color="#2ecc71", linewidth=0.5, alpha=0.2)
        }
      }
    }
    
    p <- p + 
      geom_line(data=z_hist, aes(x=plot_date, y=market_price, group=cardname, 
                                 text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>Actual Price: ", scales::dollar(market_price))), color="#3498db", linewidth=1.5) +
      geom_point(data=current_anchors, aes(x=plot_date, y=pred_price, 
                                 text=paste0("Today (Anchor): ", format(plot_date, "%b %d, %Y"), "<br>Current Price: ", scales::dollar(pred_price))), color="#3498db", size=4, shape=18)
    
    p_ly <- ggplotly(p + my_dark_theme() + labs(x="Date", y="Market Price"), dynamicTicks = TRUE, tooltip = "text")
    p_ly <- clean_plotly_tooltips(p_ly)
    
    p_ly %>% 
      layout(
        showlegend = FALSE, hovermode = "x unified", hoverdistance = 5,
        plot_bgcolor = "#1a252f", paper_bgcolor = "#1a252f",
        font = list(color = "#ecf0f1"), xaxis = list(fixedrange = FALSE, showspikes = TRUE, spikemode = "across", spikethickness = 1, spikedash = "dot", spikecolor = "rgba(255,255,255,0.3)", hoverformat = "%b %d, %Y"),
        yaxis = list(tickprefix = "$", fixedrange = FALSE) 
      ) %>% config(displayModeBar = FALSE)
  })

  output$pricing_plot <- renderPlotly({
    req(pricing_details()); d <- pricing_details()
    latest_pull <- max(d$hist$pull_date, na.rm = TRUE)
    
    m_hist <- d$hist %>% rename(plot_date = pull_date) 
    current_anchors <- m_hist %>% filter(plot_date == latest_pull)

    p <- ggplot()
      
    if("Chronos" %in% input$show_models) {
      if(nrow(d$chronos) > 0) {
        m_chronos <- d$chronos %>% filter(target_date > latest_pull) %>% rename(plot_date = target_date)
        if(nrow(m_chronos) > 0) {
          c_anchor <- current_anchors %>% select(cardname, plot_date, market_price) %>% rename(pred_price = market_price) %>% mutate(conf_low = pred_price, conf_high = pred_price)
          m_c_bridged <- bind_rows(c_anchor, m_chronos) %>% arrange(cardname, plot_date)
          
          p <- p + geom_line(data=m_c_bridged, aes(x=plot_date, y=pred_price, group=cardname, 
                                                 text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>Chronos Forecast: ", scales::dollar(pred_price))), color="#f1c40f", linetype="dashed", linewidth=1)
          if(input$show_ci) {
             p <- p + geom_ribbon(data=m_c_bridged, aes(x=plot_date, ymin=conf_low, ymax=conf_high, group=cardname), fill="#f1c40f", alpha=0.15)
          }
        }
      }
      
      if(nrow(d$chronos_shadow) > 0) {
        m_c_shadow <- d$chronos_shadow %>% filter(target_date > run_date & target_date <= latest_pull) %>% rename(plot_date = target_date)
        if(nrow(m_c_shadow)>0){
          c_shadow_anchors <- m_c_shadow %>% distinct(cardname, run_id, run_date) %>% left_join(d$hist %>% select(cardname, pull_date, market_price), by = c("cardname", "run_date" = "pull_date")) %>% filter(!is.na(market_price)) %>% select(cardname, run_id, plot_date = run_date, pred_price = market_price)
          m_c_shadow_bridged <- bind_rows(c_shadow_anchors, m_c_shadow) %>% arrange(cardname, run_id, plot_date) 
          
          p <- p + geom_line(data=m_c_shadow_bridged, aes(x=plot_date, y=pred_price, group=interaction(cardname, run_id), text="Shadow"), color="#f1c40f", linewidth=0.5, alpha=0.2)
        }
      }
    }

    if("15-Day Hybrid GRU" %in% input$show_models) {
      if(nrow(d$gru) > 0) {
        m_gru <- d$gru %>% filter(target_date > latest_pull) %>% rename(plot_date = target_date)
        if(nrow(m_gru) > 0) {
          g_anchor <- current_anchors %>% select(cardname, plot_date, market_price) %>% rename(pred_price = market_price)
          m_g_bridged <- bind_rows(g_anchor, m_gru) %>% arrange(cardname, plot_date)
          p <- p + geom_line(data=m_g_bridged, aes(x=plot_date, y=pred_price, group=cardname, 
                                             text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>GRU Forecast: ", scales::dollar(pred_price))), color="#2ecc71", linetype="dotted", linewidth=1.2)
        }
      }

      if(nrow(d$gru_shadow) > 0) {
        m_g_shadow <- d$gru_shadow %>% filter(target_date > run_date & target_date <= latest_pull) %>% rename(plot_date = target_date)
        if(nrow(m_g_shadow)>0){
          g_shadow_anchors <- m_g_shadow %>% distinct(cardname, run_id, run_date) %>% left_join(d$hist %>% select(cardname, pull_date, market_price), by = c("cardname", "run_date" = "pull_date")) %>% filter(!is.na(market_price)) %>% select(cardname, run_id, plot_date = run_date, pred_price = market_price)
          m_g_shadow_bridged <- bind_rows(g_shadow_anchors, m_g_shadow) %>% arrange(cardname, run_id, plot_date) 
          
          p <- p + geom_line(data=m_g_shadow_bridged, aes(x=plot_date, y=pred_price, group=interaction(cardname, run_id), text="Shadow"), color="#2ecc71", linewidth=0.5, alpha=0.2)
        }
      }
    }
    
    p <- p + 
      geom_line(data=m_hist, aes(x=plot_date, y=market_price, group=cardname, 
                                 text=paste0("Date: ", format(plot_date, "%b %d, %Y"), "<br>Actual Price: ", scales::dollar(market_price))), color="#3498db", linewidth=1) +
      geom_point(data=current_anchors, aes(x=plot_date, y=market_price, 
                                 text=paste0("Today (Anchor): ", format(plot_date, "%b %d, %Y"), "<br>Current Price: ", scales::dollar(market_price))), color="#3498db", size=4, shape=19)
    
    p_ly <- ggplotly(p + my_dark_theme() + labs(x="Date", y="Market Price"), dynamicTicks = TRUE, tooltip = "text")
    p_ly <- clean_plotly_tooltips(p_ly)
    
    p_ly %>% layout(hovermode = "x unified", hoverdistance = 5, plot_bgcolor = "#1a252f", paper_bgcolor = "#1a252f",
                    xaxis = list(rangeslider = list(visible = TRUE, thickness = 0.08, bgcolor = "#34495e"), hoverformat = "%b %d, %Y"),
                    yaxis = list(tickprefix = "$", fixedrange = TRUE)) %>% config(displayModeBar = FALSE)
  })

  card_error_metrics <- reactive({
    req(pricing_details())
    d <- pricing_details()
    
    shadows <- bind_rows(
      if(nrow(d$chronos_shadow) > 0) d$chronos_shadow %>% mutate(Model = "Chronos") else data.frame(),
      if(nrow(d$gru_shadow) > 0) d$gru_shadow %>% mutate(Model = "15-Day Hybrid GRU") else data.frame()
    )
    
    shadows <- shadows %>% filter(Model %in% input$show_models)
    
    if(nrow(shadows) == 0) return(NULL)
    
    error_df <- shadows %>%
      mutate(horizon_day = as.numeric(target_date - run_date)) %>%
      inner_join(d$hist %>% select(pull_date, actual_price = market_price), 
                 by = c("target_date" = "pull_date")) %>%
      inner_join(d$hist %>% select(pull_date, anchor_price = market_price), 
                 by = c("run_date" = "pull_date")) %>%
      mutate(
        ape = abs(pred_price - actual_price) / actual_price,
        ape_naive = abs(anchor_price - actual_price) / actual_price
      ) %>%
      group_by(Model, horizon_day) %>%
      summarise(
        mdape = median(ape, na.rm = TRUE),
        min_ape = min(ape, na.rm = TRUE),
        max_ape = max(ape, na.rm = TRUE),
        naive_mdape = median(ape_naive, na.rm = TRUE),
        n_samples = n(), 
        .groups = "drop"
      )
    
    return(error_df)
  })

  output$error_horizon_plot <- renderPlotly({
    df <- card_error_metrics()
    
    if(is.null(df) || nrow(df) == 0) {
      return(plotly_empty(type = "scatter", mode = "markers") %>%
               layout(title = list(text = "Not enough historical data to calculate error yet.", font=list(color="#bbb", size=14)),
                      plot_bgcolor = "#1a252f", paper_bgcolor = "#1a252f"))
    }
    
    baseline_df <- df %>% distinct(horizon_day, naive_mdape)
    
    p <- ggplot() +
      geom_col(data = baseline_df, aes(x = horizon_day, y = naive_mdape,
                                       text = paste0("Persistence Baseline: ", scales::percent(naive_mdape, accuracy=0.1),
                                                     "<br>(Error if you assumed price never changed)")),
               fill = "#ecf0f1", alpha = 0.4, width = 0.9) +
      geom_col(data = df, aes(x = horizon_day, y = mdape, fill = Model, 
                              text = paste0("Horizon Day: ", horizon_day, 
                                            "<br>", Model, " Error (MdAPE): ", scales::percent(mdape, accuracy=0.1),
                                            "<br>Min Error: ", scales::percent(min_ape, accuracy=0.1),
                                            "<br>Max Error: ", scales::percent(max_ape, accuracy=0.1),
                                            "<br>Sample Size: ", n_samples, " past predictions")),
               position = "dodge", alpha = 0.85, width = 0.7) +
      scale_fill_manual(values = c("Chronos" = "#f1c40f", "15-Day Hybrid GRU" = "#2ecc71")) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_continuous(limits = c(0, 31), breaks = seq(1, 30, by = 2)) + 
      my_dark_theme() +
      labs(x = "Days Out (Forecast Horizon)", y = "Median Error Rate (MdAPE)") +
      theme(panel.grid.major.x = element_blank()) 
    
    ggplotly(p, tooltip = "text") %>%
      layout(
        hovermode = "x unified",
        plot_bgcolor = "#1a252f", paper_bgcolor = "#1a252f",
        legend = list(orientation = "h", x = 0.5, y = 1.15, xanchor = "center")
      ) %>%
      config(displayModeBar = FALSE)
  })

}

shinyApp(ui, server)