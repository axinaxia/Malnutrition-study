library(shiny)
library(dplyr)
library(DT)
library(heemod)

# =========================
# Translations
# =========================
txt <- list(
  en = list(
    app_title = "Economic model of interventions for malnutrition in Swedish older adults",
    language = "Language",
    lang_en = "English",
    lang_sv = "Swedish",
    n_pop = "Population size",
    rr_red = "Effect of intervention on malnutrition (risk reduction)",
    rr_conv = "Effect of intervention on recovery from malnutrition (probability increase)",
    rr_mort = "Effect of intervention on mortality (risk reduction)",
    cost_int_gen = "Annual intervention cost (general)",
    cost_int_geri_add = "Additional annual intervention cost for geriatric patients",
    n_cycles = "Number of cycles (years)",
    run_model = "Run model",
    tab_icer = "ICER",
    tab_summary = "Summary",
    strategy_soc = "SoC",
    strategy_int = "Intervention",
    strategy_diff = "Intervention vs SoC",
    col_strategy = "Strategy",
    col_cost_care = "Background costs of care",
    col_cost_rx = "Costs of intervention",
    col_cost_total = "Total costs",
    col_qaly = "Quality-Adjusted Life Year",
    col_ly = "Life years",
    col_dc = "Incremental cost",
    col_de = "Incremental effect",
    col_icer = "ICER"
  ),
  sv = list(
    app_title = "Hälsoekonomisk modell av insatser mot undernäring hos äldre i Sverige",
    language = "Språk",
    lang_en = "Engelska",
    lang_sv = "Svenska",
    n_pop = "Populationsstorlek",
    rr_red = "Effekt av intervention på undernäring (riskreduktion)",
    rr_conv = "Effekt av intervention på återhämtning från undernäring (ökad sannolikhet)",
    rr_mort = "Effekt av intervention på mortalitet (riskreduktion)",
    cost_int_gen = "Årlig interventionskostnad (generell)",
    cost_int_geri_add = "Ytterligare årlig interventionskostnad för geriatriska patienter",
    n_cycles = "Antal cykler (år)",
    run_model = "Kör modell",
    tab_icer = "ICER",
    tab_summary = "Sammanfattning",
    strategy_soc = "Standardvård",
    strategy_int = "Intervention",
    strategy_diff = "Intervention jämfört med standardvård",
    col_strategy = "Strategi",
    col_cost_care = "Bakgrundskostnader för vård",
    col_cost_rx = "Interventionskostnader",
    col_cost_total = "Totala kostnader",
    col_qaly = "Kvalitetsjusterade levnadsår",
    col_ly = "Levnadsår",
    col_dc = "Inkrementell kostnad",
    col_de = "Inkrementell effekt",
    col_icer = "ICER"
  )
)

# =========================
# Transition probabilities
# =========================
tp_table <- data.frame(
  state_start = c(1, 1, 2, 2, 2, 3, 3),
  state_next  = c(2, 4, 1, 3, 4, 2, 4),
  tp          = c(0.062, 0.049, 0.079, 0.020, 0.103, 0.066, 0.277)
)

get_tp <- function(s1, s2, data) {
  data %>%
    filter(state_start == s1, state_next == s2) %>%
    pull(tp)
}

pairs <- list(
  c(1, 2), c(1, 4),
  c(2, 1), c(2, 3), c(2, 4),
  c(3, 2), c(3, 4)
)

get_tp_values <- function(tp_table) {
  vals <- list()
  for (p in pairs) {
    var_name <- paste0("tp", p[1], p[2])
    vals[[var_name]] <- get_tp(p[1], p[2], tp_table)
  }
  vals
}

# =========================
# heemod model
# =========================
malnu_model <- function(n_pop, rr_red, rr_conv, rr_mort, cost_int_gen, cost_int_geri_add, n_cycles, lang = "en") {
  tr <- txt[[lang]]
  
  tp_vals <- get_tp_values(tp_table)
  tp12 <- tp_vals$tp12
  tp14 <- tp_vals$tp14
  tp21 <- tp_vals$tp21
  tp23 <- tp_vals$tp23
  tp24 <- tp_vals$tp24
  tp32 <- tp_vals$tp32
  tp34 <- tp_vals$tp34
  
  n_state1 <- round(n_pop * 0.6776, digits = 0)
  n_state2 <- round(n_pop * 0.2858, digits = 0)
  n_state3 <- round(n_pop * 0.0366, digits = 0)
  
  para <- define_parameters(
    prop_geri_state1 = 0.01,
    prop_geri_state2 = 0.04,
    prop_geri_state3 = 0.13,
    
    state1_cost_gen = 27546,
    state2_cost_gen = 29894,
    state3_cost_gen = 30220,
    
    state1_cost_geri = 596834,
    state2_cost_geri = 712126,
    state3_cost_geri = 761223,
    
    state1_utility = 0.783,
    state2_utility = 0.757,
    state3_utility = 0.720,
    
    geri_disutility = 0.311,
    
    disc_fac = exp(-0.03 * (model_time - 1)),
    
    cost_rx = dispatch_strategy(
      strat_soc = 0,
      strat_int = cost_int_gen
    )
  )
  
  mat_soc <- define_transition(
    state_names = c("state1", "state2", "state3", "state4"),
    C,    tp12, 0,    tp14,
    tp21, C,    tp23, tp24,
    0,    tp32, C,    tp34,
    0,    0,    0,    1
  )
  
  mat_int <- define_transition(
    state_names = c("state1", "state2", "state3", "state4"),
    C,
    tp12 * (1 - rr_red),
    0,
    tp14 * (1 - rr_mort),
    
    pmin(tp21 * (1 + rr_conv), 1),
    C,
    tp23 * (1 - rr_red),
    tp24 * (1 - rr_mort),
    
    0,
    pmin(tp32 * (1 + rr_conv), 1),
    C,
    tp34 * (1 - rr_mort),
    
    0, 0, 0, 1
  )
  
  state1 <- define_state(
    cost_care = ((state1_cost_gen) * (1 - prop_geri_state1) +
                   state1_cost_geri * prop_geri_state1) * disc_fac,
    
    cost_rx = (cost_rx * (1 - prop_geri_state1) +
                 (cost_rx + cost_int_geri_add) * prop_geri_state1) * disc_fac,
    
    cost_total = ((state1_cost_gen) * (1 - prop_geri_state1) +
                    state1_cost_geri * prop_geri_state1 +
                    cost_rx * (1 - prop_geri_state1) +
                    (cost_rx + cost_int_geri_add) * prop_geri_state1) * disc_fac,
    
    utility = (state1_utility * (1 - prop_geri_state1) +
                 (state1_utility - geri_disutility) * prop_geri_state1) * disc_fac,
    
    life_year = 1
  )
  
  state2 <- define_state(
    cost_care = ((state2_cost_gen) * (1 - prop_geri_state2) +
                   state2_cost_geri * prop_geri_state2) * disc_fac,
    
    cost_rx = (cost_rx * (1 - prop_geri_state2) +
                 (cost_rx + cost_int_geri_add) * prop_geri_state2) * disc_fac,
    
    cost_total = ((state2_cost_gen) * (1 - prop_geri_state2) +
                    state2_cost_geri * prop_geri_state2 +
                    cost_rx * (1 - prop_geri_state2) +
                    (cost_rx + cost_int_geri_add) * prop_geri_state2) * disc_fac,
    
    utility = (state2_utility * (1 - prop_geri_state2) +
                 (state2_utility - geri_disutility) * prop_geri_state2) * disc_fac,
    
    life_year = 1
  )
  
  state3 <- define_state(
    cost_care = ((state3_cost_gen) * (1 - prop_geri_state3) +
                   state3_cost_geri * prop_geri_state3) * disc_fac,
    
    cost_rx = (cost_rx * (1 - prop_geri_state3) +
                 (cost_rx + cost_int_geri_add) * prop_geri_state3) * disc_fac,
    
    cost_total = ((state3_cost_gen) * (1 - prop_geri_state3) +
                    state3_cost_geri * prop_geri_state3 +
                    cost_rx * (1 - prop_geri_state3) +
                    (cost_rx + cost_int_geri_add) * prop_geri_state3) * disc_fac,
    
    utility = (state3_utility * (1 - prop_geri_state3) +
                 (state3_utility - geri_disutility) * prop_geri_state3) * disc_fac,
    
    life_year = 1
  )
  
  state4 <- define_state(
    cost_care = 0,
    cost_rx = 0,
    cost_total = 0,
    utility = 0,
    life_year = 0
  )
  
  strat_soc <- define_strategy(
    transition = mat_soc,
    state1 = state1,
    state2 = state2,
    state3 = state3,
    state4 = state4
  )
  
  strat_int <- define_strategy(
    transition = mat_int,
    state1 = state1,
    state2 = state2,
    state3 = state3,
    state4 = state4
  )
  
  res_mod <- run_model(
    strat_soc = strat_soc,
    strat_int = strat_int,
    parameters = para,
    init = c(state1 = n_state1, state2 = n_state2, state3 = n_state3, state4 = 0),
    cycles = n_cycles,
    cost = cost_total,
    effect = utility,
    method = "life-table"
  )
  
  summary_tbl <- res_mod$run_model %>%
    select(cost_care:life_year) %>%
    rbind(
      res_mod$run_model %>%
        select(cost_care:life_year) %>%
        summarise(across(everything(), ~ .[2] - .[1]))
    ) %>%
    as.data.frame() %>%
    mutate(strategy = c(tr$strategy_soc, tr$strategy_int, tr$strategy_diff)) %>%
    mutate(
      dc   = ifelse(strategy == tr$strategy_diff, cost_total / n_pop, NA_real_),
      de   = ifelse(strategy == tr$strategy_diff, utility / n_pop, NA_real_),
      ICER = ifelse(strategy == tr$strategy_diff, cost_total / utility, NA_real_)
    ) %>%
    mutate(
      across(
        c(cost_care, cost_rx, cost_total, utility, life_year, dc, ICER),
        ~ ifelse(is.na(.x), "-", format(round(.x, 0), big.mark = ","))
      ),
      de = ifelse(is.na(de), "-", format(round(de, 2), nsmall = 2))
    ) %>%
    select(strategy, everything()) %>%
    setNames(c(
      tr$col_strategy, tr$col_cost_care,
      tr$col_cost_rx, tr$col_cost_total,
      tr$col_qaly, tr$col_ly,
      tr$col_dc, tr$col_de, tr$col_icer
    ))
  
  list(
    model = res_mod,
    summary = summary_tbl %>%
      select(-c(all_of(c(tr$col_dc, tr$col_de, tr$col_icer)))),
    icer = summary_tbl %>%
      filter(.data[[tr$col_strategy]] == tr$strategy_diff) %>%
      select(all_of(c(tr$col_strategy, tr$col_dc, tr$col_de, tr$col_icer)))
  )
}

# =========================
# UI
# =========================
ui <- fluidPage(
  uiOutput("app_ui")
)

# =========================
# Server
# =========================
server <- function(input, output, session) {
  
  tr <- reactive({
    lang <- if (is.null(input$lang)) "en" else input$lang
    txt[[lang]]
  })
  
  output$app_ui <- renderUI({
    fluidPage(
      titlePanel(tr()$app_title),
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "lang",
            tr()$language,
            choices = setNames(c("en", "sv"), c(tr()$lang_en, tr()$lang_sv)),
            selected = if (is.null(input$lang)) "en" else input$lang
          ),
          numericInput("n_pop", tr()$n_pop, value = 1000, min = 1),
          numericInput("rr_red", tr()$rr_red, value = 0.20, min = 0, max = 1, step = 0.01),
          numericInput("rr_conv", tr()$rr_conv, value = 0.20, min = 0, step = 0.01),
          numericInput("rr_mort", tr()$rr_mort, value = 0.00, min = 0, max = 1, step = 0.01),
          numericInput("cost_int_gen", tr()$cost_int_gen, value = 10000, min = 0),
          numericInput("cost_int_geri_add", tr()$cost_int_geri_add, value = 0, min = 0),
          numericInput("n_cycles", tr()$n_cycles, value = 40, min = 1),
          actionButton("run_model", tr()$run_model)
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(tr()$tab_icer, DTOutput("icer")),
            tabPanel(tr()$tab_summary, DTOutput("summary_tbl"))
          )
        )
      )
    )
  })
  
  observe({
    if (is.null(input$lang)) {
      updateSelectInput(session, "lang", selected = "sv")
    }
  })
  
  results <- eventReactive(input$run_model, {
    malnu_model(
      n_pop = input$n_pop,
      rr_red = input$rr_red,
      rr_conv = input$rr_conv,
      rr_mort = input$rr_mort,
      cost_int_gen = input$cost_int_gen,
      cost_int_geri_add = input$cost_int_geri_add,
      n_cycles = input$n_cycles,
      lang = input$lang
    )
  })
  
  output$summary_tbl <- renderDT({
    req(results())
    datatable(
      results()$summary,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
  
  output$icer <- renderDT({
    req(results())
    datatable(
      results()$icer,
      rownames = FALSE,
      options = list(pageLength = 10, scrollX = TRUE)
    )
  })
}

shinyApp(ui, server)