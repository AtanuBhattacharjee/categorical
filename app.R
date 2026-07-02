# ============================================================================
#  Biostatistics for Oncologists  |  Proportions, Agreement & Rates
#  Categorical methods beyond chi-square
#  Dr. Atanu Bhattacharjee  |  StatsCure Network
#
#  Each test is one page, walked through in numbered steps:
#    Step 1  Understand the test (what & why)
#    Step 2  Get your data - use the example or upload your own (always shown)
#    Step 3  Set options and click Run
#    Step 4  Read the results (estimate + CI + p) - hidden until you Run
#    Step 5  See it visually                       - hidden until you Run
#
#  Requirements:  install.packages(c("shiny", "bslib", "DT"))
#  Run:           shiny::runApp("app.R")   (or "Run App" in RStudio)
#  Stats & plots: base R only.
# ============================================================================

for (p in c("shiny", "bslib", "DT")) {
  if (!requireNamespace(p, quietly = TRUE))
    stop(sprintf("Package '%s' is required. install.packages('%s')", p, p), call. = FALSE)
}
library(shiny)
library(bslib)
library(DT)

# ----------------------------------------------------------------- palette --
COL <- list(
  op = "#2563EB", ni = "#0D9488", ka = "#7C3AED",
  sp = "#EA580C", cmh = "#DB2777", po = "#059669", ink = "#0F2A43"
)
tint  <- function(hex, a = 0.14) { v <- grDevices::col2rgb(hex); sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], a) }
light <- function(hex, f = 0.5) { v <- grDevices::col2rgb(hex); w <- round(v + (255 - v) * f); grDevices::rgb(w[1], w[2], w[3], maxColorValue = 255) }

theme_app <- bs_theme(
  version = 5, primary = COL$op, secondary = COL$ni, success = COL$po,
  "body-bg" = "#EEF3FA", "border-radius" = "0.7rem", font_scale = 1.15
)

pct <- function(x, d = 1) ifelse(is.na(x), "-", paste0(formatC(100 * x, format = "f", digits = d), "%"))
fmt <- function(x, d = 3) ifelse(is.na(x), "-", formatC(x, format = "f", digits = d))

# ------------------------------------------------------------- UI helpers ---
statCard <- function(id, title, color) {
  div(class = "stat-card", style = paste0("border-top:6px solid ", color, ";"),
      div(class = "stat-title", title),
      div(class = "stat-value", style = paste0("color:", color, ";"), textOutput(id, inline = TRUE)))
}
topicHead <- function(title, subtitle, color) {
  div(class = "topic-hero", style = paste0("background:linear-gradient(100deg,", color, ",", tint(color, 0.65), ");"),
      div(class = "topic-title", title),
      div(class = "topic-sub", subtitle))
}
blockHead <- function(color, icon_name, label) {
  card_header(class = "card-accent",
    style = paste0("background:", tint(color, 0.18), ";border-left:6px solid ", color, ";color:", COL$ink, ";"),
    tagList(icon(icon_name), " ", label))
}
stepLabel <- function(n, text, color) {
  div(class = "step-label",
      span(class = "step-chip", style = paste0("background:", color, ";"), n),
      span(class = "step-text", text))
}
promptBox <- function(color) {
  div(class = "prompt", style = paste0("border-color:", color, ";color:", color, ";"),
      tagList(icon("hand-pointer"), " Press ", tags$b("Run analysis"),
              " above to compute and reveal the results."))
}
demoCard <- function(color, what, why, how, rfun) {
  card(class = "shadow-card",
    blockHead(color, "circle-info", "What this does & why"),
    layout_columns(col_widths = c(4, 4, 4),
      div(class = "demo-col", div(class = "demo-h", style = paste0("color:", color), "WHAT"), p(what)),
      div(class = "demo-col", div(class = "demo-h", style = paste0("color:", color), "WHY"),  p(why)),
      div(class = "demo-col", div(class = "demo-h", style = paste0("color:", color), "HOW TO READ"), p(how))),
    div(class = "demo-r", tagList(tags$b("R:"), " ", tags$code(rfun))))
}
dataCard <- function(pre, color, help) {
  card(class = "shadow-card",
    blockHead(color, "table", tagList("Your data  ",
              tags$span(class = "badge-src", textOutput(paste0(pre, "_src"), inline = TRUE)))),
    layout_columns(col_widths = c(6, 6),
      div(fileInput(paste0(pre, "_file"), "Upload a CSV to replace the example", accept = c(".csv", "text/csv"), width = "100%")),
      div(class = "pt-1",
          downloadButton(paste0(pre, "_dl"), "Download example CSV",
                         class = "btn btn-sm", style = paste0("background:", color, ";color:#fff;border:none;font-weight:700;")),
          p(class = "help-txt mt-2 mb-0", help))),
    div(class = "edit-hint", tagList(icon("pen-to-square"),
        " Double-click any cell to change a value, then click ", tags$b("Run"), " to update the results.")),
    DTOutput(paste0(pre, "_table")))
}
runBar <- function(id, color, label = "Run analysis") {
  div(class = "run-bar",
      actionButton(id, tagList(icon("play"), " ", label), class = "btn",
                   style = paste0("background:", color, ";color:#fff;border:none;font-weight:800;font-size:1.15rem;padding:10px 26px;")))
}
resultCard <- function(color, ..., header = "Results") card(class = "shadow-card", blockHead(color, "square-poll-vertical", header), ...)
plotCard   <- function(id, color, header, height = "330px") card(class = "shadow-card", blockHead(color, "chart-column", header), plotOutput(id, height = height))

# ---------------------------------------------------------- example data ----
ex_op  <- data.frame(patient = 1:16, response = c(0,1,0,0,0,0,1,0,0,0,1,0,0,1,0,0))
ex_ni  <- data.frame(arm = c("New", "Standard"), responders = c(128, 132), non_responders = c(72, 68))
ex_ka  <- data.frame(readerA = c(rep("Pos", 25), rep("Neg", 25)),
                     readerB = c(rep("Pos", 20), rep("Neg", 5), rep("Pos", 3), rep("Neg", 22)))
ex_sp  <- data.frame(biomarker = c(12,25,31,44,52,60,73,85), shrinkage = c(8,15,22,19,31,38,52,47))
ex_cmh <- data.frame(stratum = c("Centre 1","Centre 1","Centre 2","Centre 2"),
                     arm = c("A","B","A","B"), resp = c(8,5,3,2), no = c(2,5,17,18))
ex_po  <- data.frame(arm = c("A","A","A","B","B","B"), months = c(40,40,40,37,37,36), aes = c(6,6,6,10,10,10))

read_csv_or <- function(file, example) {
  if (is.null(file)) return(list(df = example, src = "Example data"))
  df <- tryCatch(read.csv(file$datapath, stringsAsFactors = FALSE, check.names = FALSE), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(list(df = example, src = "Upload failed - showing example"))
  names(df) <- tolower(trimws(names(df)))
  list(df = df, src = paste0("Uploaded: ", file$name))
}
dt <- function(df, color) {
  DT::datatable(df, rownames = FALSE, editable = list(target = "cell"),
    class = "stripe hover row-border",
    options = list(pageLength = 6, dom = "tip", scrollX = TRUE,
      initComplete = DT::JS(sprintf(
        "function(s,j){$(this.api().table().header()).css({'background-color':'%s','color':'#fff','font-size':'1.02rem'});}", color))))
}
bg_setup <- function() par(bg = "white", mar = c(4.5, 4.8, 1.6, 1.2),
                           cex.axis = 1.1, cex.lab = 1.25, font.lab = 2,
                           col.axis = COL$ink, col.lab = COL$ink)

# --- one full test page from parts (keeps the 6 tabs consistent) -----------
opts_block <- function(color, ...) card(class = "shadow-card", blockHead(color, "sliders", "Options"), ...)

# ============================================================================
#  UI
# ============================================================================
ui <- page_navbar(
  title = tags$span(tags$b("Proportions, Agreement & Rates"), " \u00b7 StatsCure"),
  theme = theme_app, fillable = FALSE,
  header = tags$head(tags$style(HTML(paste0("
    body{background:#EEF3FA;font-size:1.02rem;color:#1f2d3d;}
    .navbar{background:linear-gradient(90deg,", COL$op, ",", COL$po, ")!important;}
    .navbar .nav-link,.navbar-brand{color:#fff!important;font-size:1.08rem;font-weight:600;}
    .navbar-brand{font-size:1.25rem!important;font-weight:800;}
    .navbar .nav-link.active{font-weight:800;border-bottom:3px solid #fff;}
    .topic-hero{border-radius:16px;padding:22px 26px;margin-bottom:18px;color:#fff;box-shadow:0 6px 18px rgba(15,42,67,.18);}
    .topic-title{font-size:2rem;font-weight:900;letter-spacing:.2px;}
    .topic-sub{font-size:1.15rem;font-weight:600;opacity:.97;margin-top:4px;}
    .shadow-card{box-shadow:0 4px 14px rgba(15,42,67,.09);border:none;margin-bottom:16px;}
    .card-accent{font-weight:800;font-size:1.2rem;border-radius:12px 12px 0 0!important;}
    .step-label{display:flex;align-items:center;gap:12px;margin:14px 2px 8px;}
    .step-chip{display:inline-flex;width:38px;height:38px;border-radius:50%;color:#fff;
       font-weight:900;font-size:1.25rem;align-items:center;justify-content:center;box-shadow:0 3px 8px rgba(15,42,67,.2);}
    .step-text{font-size:1.4rem;font-weight:900;color:#0F2A43;}
    .stat-card{background:#fff;border-radius:14px;padding:16px 18px;margin-bottom:10px;
       box-shadow:0 3px 10px rgba(15,42,67,.09);min-height:112px;}
    .stat-title{font-size:.92rem;color:#41556b;font-weight:800;text-transform:uppercase;letter-spacing:.5px;}
    .stat-value{font-size:2.15rem;font-weight:900;line-height:2.3rem;margin-top:6px;}
    .badge-src{font-size:.82rem;font-weight:800;background:#0F2A43;color:#fff;padding:3px 10px;border-radius:20px;}
    .concl{background:#F4F8FF;border-left:5px solid #94a3b8;padding:14px 16px;border-radius:10px;
       font-size:1.15rem;font-weight:600;color:#1f3550;margin-top:8px;}
    .demo-h{font-size:.95rem;font-weight:900;letter-spacing:.6px;margin-bottom:4px;}
    .demo-col p{font-size:1.05rem;font-weight:500;color:#26384c;margin-bottom:0;line-height:1.5;}
    .demo-r{margin-top:12px;font-size:1.05rem;font-weight:600;color:#26384c;}
    .demo-r code{background:#eef2f7;color:#0F2A43;padding:3px 8px;border-radius:6px;font-size:1rem;}
    .help-txt{font-size:1rem;font-weight:600;color:#41556b;}
    .edit-hint{font-size:1.05rem;font-weight:700;color:#0F2A43;margin:6px 2px 2px;}
    .run-bar{margin:2px 0 8px 0;}
    .prompt{background:#fff;border:2px dashed;border-radius:14px;padding:22px;font-size:1.25rem;
       font-weight:800;text-align:center;margin-bottom:16px;}
    .form-label{font-weight:800;font-size:1.05rem;color:#0F2A43;}
    table.dataTable{font-size:1.02rem;}
    table.dataTable thead th{border:none;}
    .dataTables_wrapper .dataTables_info,.dataTables_wrapper .dataTables_paginate{font-size:1rem;font-weight:600;}
  ")))),

  # ---------------------------------------------------------------- Overview
  nav_panel("Overview", icon = icon("compass"),
    topicHead("Six categorical tests \u00b7 one interactive workbench",
              "On each tab: read what the test is for, get your data, click Run, and get the full analysis with a confidence interval - not just a p-value.", COL$ink),
    layout_columns(col_widths = c(4,4,4),
      card(class="shadow-card", blockHead(COL$op, "percent", "Proportion / Risk"),
           withMathJax(helpText("$$p=\\dfrac{x}{n}$$")), p(class="demo-col", "Part of a whole. No time. Always in [0, 1]. e.g. ORR = responders / patients.")),
      card(class="shadow-card", blockHead(COL$ka, "divide", "Odds"),
           withMathJax(helpText("$$o=\\dfrac{x}{n-x}$$")), p(class="demo-col", "Events to non-events. Can exceed 1. e.g. 30 : 20 = 1.5.")),
      card(class="shadow-card", blockHead(COL$po, "clock", "Rate"),
           withMathJax(helpText("$$r=\\dfrac{\\text{events}}{\\text{person-time}}$$")), p(class="demo-col", "Has a time unit. Not capped at 1. e.g. 8 AEs per 100 patient-years."))
    ),
    resultCard(COL$op, header = "Which test answers which question?",
      tags$table(class = "table mb-0", style = "font-size:1.1rem;",
        tags$thead(tags$tr(tags$th("Your question"), tags$th("Go to tab"))),
        tags$tbody(
          tags$tr(tags$td("One rate vs a fixed benchmark"),          tags$td(tags$b(style=paste0("color:",COL$op),  "1. One-proportion"))),
          tags$tr(tags$td("\"Not worse than\" standard by a margin"),  tags$td(tags$b(style=paste0("color:",COL$ni),  "2. Non-inferiority"))),
          tags$tr(tags$td("Do two raters agree (beyond chance)?"),    tags$td(tags$b(style=paste0("color:",COL$ka),  "3. Cohen's kappa"))),
          tags$tr(tags$td("Do two markers move together?"),           tags$td(tags$b(style=paste0("color:",COL$sp),  "4. Spearman"))),
          tags$tr(tags$td("Response adjusted for strata / centre"),   tags$td(tags$b(style=paste0("color:",COL$cmh), "5. Mantel-Haenszel"))),
          tags$tr(tags$td("Events per patient-time"),                 tags$td(tags$b(style=paste0("color:",COL$po),  "6. Poisson rate ratio")))
        )))
  ),

  # ----------------------------------------------------------- 1. one-prop
  nav_panel("1. One-proportion", icon = icon("percent"),
    topicHead("One-proportion exact test", "Is a single response rate better than a fixed benchmark?", COL$op),
    stepLabel(1, "Understand the test", COL$op),
    demoCard(COL$op,
      what = "Compares one observed proportion (e.g. ORR) against a fixed benchmark \u03c0\u2080 using the exact binomial tail.",
      why  = "The standard single-arm phase II analysis. With few patients the normal approximation is unreliable, so the exact test keeps the p-value honest.",
      how  = "p = P(X \u2265 x | \u03c0\u2080). A small p means the response rate is unlikely this high by chance. The CI is exact (Clopper-Pearson).",
      rfun = "binom.test(x, n, p0, alternative = \"greater\")"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$op),
    dataCard("op", COL$op, "Format: one column 'response' coded 0/1 (1 = responder)."),
    stepLabel(3, "Set the benchmark, then click Run", COL$op),
    layout_columns(col_widths = c(5,7),
      opts_block(COL$op,
        numericInput("op_p0", "Benchmark \u03c0\u2080", value = 0.05, min = 0, max = 1, step = 0.01),
        sliderInput("op_conf", "CI level", 0.80, 0.99, 0.95, 0.01)),
      runBar("op_run", COL$op)),
    conditionalPanel("input.op_run == 0", promptBox(COL$op)),
    conditionalPanel("input.op_run > 0",
      stepLabel(4, "Read the results", COL$op),
      resultCard(COL$op,
        layout_columns(col_widths = c(3,3,3,3),
          statCard("op_rate", "Observed rate", COL$op),
          statCard("op_p",    "p (one-sided)",  COL$op),
          statCard("op_ci",   "Clopper-Pearson CI", COL$op),
          statCard("op_exp",  "Expected under H0", COL$op)),
        div(class="concl", textOutput("op_concl"))),
      stepLabel(5, "See it visually", COL$op),
      plotCard("op_plot", COL$op, "Null distribution - tail X \u2265 x shaded"))
  ),

  # ---------------------------------------------------------- 2. non-infer
  nav_panel("2. Non-inferiority", icon = icon("scale-balanced"),
    topicHead("Non-inferiority for two proportions", "Not 'is it better?' but 'is it not worse by more than the margin \u0394?'", COL$ni),
    stepLabel(1, "Understand the test", COL$ni),
    demoCard(COL$ni,
      what = "Tests whether a new treatment's response is no worse than the standard by more than a pre-set margin \u0394.",
      why  = "When the new option is gentler, cheaper or simpler, we don't need it to be better - only to lose no clinically meaningful efficacy.",
      how  = "Look at the lower bound of the 95% CI for (new - standard). If it stays above -\u0394, non-inferiority holds. The point estimate alone is not enough.",
      rfun = "prop.test / DescTools::BinomDiffCI (Newcombe)"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$ni),
    dataCard("ni", COL$ni, "Format: columns 'arm' (New/Standard), 'responders', 'non_responders' - one row per arm."),
    stepLabel(3, "Set the margin, then click Run", COL$ni),
    layout_columns(col_widths = c(5,7),
      opts_block(COL$ni, numericInput("ni_margin", "NI margin \u0394 (points)", value = 12.5, min = 0, step = 0.5)),
      runBar("ni_run", COL$ni)),
    conditionalPanel("input.ni_run == 0", promptBox(COL$ni)),
    conditionalPanel("input.ni_run > 0",
      stepLabel(4, "Read the results", COL$ni),
      resultCard(COL$ni,
        layout_columns(col_widths = c(4,4,4),
          statCard("ni_diff", "Difference (new - std)", COL$ni),
          statCard("ni_low",  "95% CI lower bound", COL$ni),
          statCard("ni_dec",  "Decision", COL$ni)),
        div(class="concl", textOutput("ni_concl"))),
      stepLabel(5, "See it visually", COL$ni),
      plotCard("ni_plot", COL$ni, "Confidence interval vs the margin", "260px"))
  ),

  # ---------------------------------------------------------- 3. kappa
  nav_panel("3. Cohen's kappa", icon = icon("handshake"),
    topicHead("Cohen's kappa - agreement beyond chance", "How much do two raters agree, once luck is removed?", COL$ka),
    stepLabel(1, "Understand the test", COL$ka),
    demoCard(COL$ka,
      what = "Measures agreement between two raters on a categorical call (e.g. PD-L1 positive/negative), corrected for chance.",
      why  = "Raw % agreement is inflated when one category is common - two raters can agree a lot by luck. Kappa strips that luck out.",
      how  = "\u03ba = (Po - Pe)/(1 - Pe). Bands: <0 poor, .21-.40 fair, .41-.60 moderate, .61-.80 substantial, >.80 near-perfect.",
      rfun = "irr::kappa2()  /  irr::kappam.fleiss() for \u22653 raters"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$ka),
    dataCard("ka", COL$ka, "Format: columns 'readerA', 'readerB' with values Pos/Neg (or 1/0) - one row per case."),
    stepLabel(3, "Click Run", COL$ka),
    runBar("ka_run", COL$ka),
    conditionalPanel("input.ka_run == 0", promptBox(COL$ka)),
    conditionalPanel("input.ka_run > 0",
      stepLabel(4, "Read the results", COL$ka),
      layout_columns(col_widths = c(5,7),
        resultCard(COL$ka, header = "Cross-tabulation", tableOutput("ka_xtab")),
        resultCard(COL$ka,
          layout_columns(col_widths = c(3,3,3,3),
            statCard("ka_po",   "Observed Po", COL$ka),
            statCard("ka_pe",   "Chance Pe",   COL$ka),
            statCard("ka_kap",  "Cohen's \u03ba", COL$ka),
            statCard("ka_band", "Strength",    COL$ka)),
          div(class="concl", textOutput("ka_concl")))),
      stepLabel(5, "See it visually", COL$ka),
      plotCard("ka_plot", COL$ka, "Raw vs chance-corrected agreement", "270px"))
  ),

  # ---------------------------------------------------------- 4. spearman
  nav_panel("4. Spearman", icon = icon("arrow-trend-up"),
    topicHead("Spearman rank correlation - monotonic association", "Do two markers move together, in any one-directional way?", COL$sp),
    stepLabel(1, "Understand the test", COL$sp),
    demoCard(COL$sp,
      what = "Correlates the ranks of two variables, capturing any monotonic (one-directional) relationship.",
      why  = "Marker levels are often skewed and the link may be curved but still one-directional. Pearson assumes a straight line and normal data; Spearman does not.",
      how  = "\u03c1 runs from -1 to +1. Near \u00b11 = strong monotonic association; the sign gives the direction. It resists outliers.",
      rfun = "cor.test(x, y, method = \"spearman\")"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$sp),
    dataCard("sp", COL$sp, "Format: two numeric columns (e.g. 'biomarker', 'shrinkage') - one row per patient."),
    stepLabel(3, "Click Run", COL$sp),
    runBar("sp_run", COL$sp),
    conditionalPanel("input.sp_run == 0", promptBox(COL$sp)),
    conditionalPanel("input.sp_run > 0",
      stepLabel(4, "Read the results", COL$sp),
      resultCard(COL$sp,
        layout_columns(col_widths = c(4,4,4),
          statCard("sp_rho", "Spearman \u03c1", COL$sp),
          statCard("sp_p",   "p-value", COL$sp),
          statCard("sp_n",   "n pairs", COL$sp)),
        div(class="concl", textOutput("sp_concl"))),
      stepLabel(5, "See it visually", COL$sp),
      layout_columns(col_widths = c(6,6),
        plotCard("sp_raw",  COL$sp, "Raw values", "300px"),
        plotCard("sp_rank", COL$sp, "Ranks - what Spearman correlates", "300px")))
  ),

  # ---------------------------------------------------------- 5. CMH
  nav_panel("5. Mantel-Haenszel", icon = icon("layer-group"),
    topicHead("Cochran-Mantel-Haenszel - response adjusted for strata", "One adjusted odds ratio, pooled across centres or risk groups.", COL$cmh),
    stepLabel(1, "Understand the test", COL$cmh),
    demoCard(COL$cmh,
      what = "Pools 2x2 response tables across strata (centres, randomisation factors) into a single common odds ratio.",
      why  = "Naively pooling can reverse the effect seen within every stratum (Simpson's paradox). CMH adjusts for the stratifier without a full regression model.",
      how  = "It finds the OR within each stratum, then a weighted common OR. Report it with the CMH p-value; OR > 1 favours arm A response.",
      rfun = "mantelhaen.test(array(...), correct = FALSE)"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$cmh),
    dataCard("cmh", COL$cmh, "Format: columns 'stratum', 'arm' (A/B), 'resp', 'no' - one row per stratum x arm."),
    stepLabel(3, "Click Run", COL$cmh),
    runBar("cmh_run", COL$cmh),
    conditionalPanel("input.cmh_run == 0", promptBox(COL$cmh)),
    conditionalPanel("input.cmh_run > 0",
      stepLabel(4, "Read the results", COL$cmh),
      layout_columns(col_widths = c(5,7),
        resultCard(COL$cmh, header = "Per-stratum odds ratios", tableOutput("cmh_tab")),
        resultCard(COL$cmh,
          layout_columns(col_widths = c(6,6),
            statCard("cmh_mh", "Pooled MH OR", COL$cmh),
            statCard("cmh_p",  "CMH p-value",  COL$cmh)),
          div(class="concl", textOutput("cmh_concl")))),
      stepLabel(5, "See it visually", COL$cmh),
      plotCard("cmh_plot", COL$cmh, "Per-stratum ORs vs pooled common OR", "280px"))
  ),

  # ---------------------------------------------------------- 6. poisson
  nav_panel("6. Poisson rate ratio", icon = icon("clock"),
    topicHead("Poisson rate ratio - events per patient-time", "Compare event rates fairly when follow-up differs.", COL$po),
    stepLabel(1, "Understand the test", COL$po),
    demoCard(COL$po,
      what = "Compares adverse-event (or any count) rates between arms as an incidence rate ratio, using person-time.",
      why  = "Raw counts mislead when follow-up differs: an arm followed longer accrues more events even at a lower rate. The offset log(time) fixes this.",
      how  = "IRR = rateA / rateB. IRR < 1 means arm A has fewer events per unit time. Report the IRR with its CI; over-dispersed? use negative binomial.",
      rfun = "poisson.test(c(xA, xB), T = c(tA, tB))"),
    stepLabel(2, "Get your data - edit the cells below, or upload a CSV", COL$po),
    dataCard("po", COL$po, "Format: columns 'arm' (A/B), 'months' (person-time), 'aes' (event count) - rows summed per arm."),
    stepLabel(3, "Click Run", COL$po),
    runBar("po_run", COL$po),
    conditionalPanel("input.po_run == 0", promptBox(COL$po)),
    conditionalPanel("input.po_run > 0",
      stepLabel(4, "Read the results", COL$po),
      layout_columns(col_widths = c(5,7),
        resultCard(COL$po, header = "Aggregated rates by arm", tableOutput("po_tab")),
        resultCard(COL$po,
          layout_columns(col_widths = c(4,4,4),
            statCard("po_irr", "IRR (A / B)", COL$po),
            statCard("po_ci",  "95% CI", COL$po),
            statCard("po_p",   "p-value", COL$po)),
          div(class="concl", textOutput("po_concl")))),
      stepLabel(5, "See it visually", COL$po),
      plotCard("po_plot", COL$po, "Rate per patient-month by arm", "280px"))
  ),

  nav_spacer(),
  nav_item(tags$span(class = "navbar-text", style = "color:#eaf2fb;font-weight:700;", "Dr. Atanu Bhattacharjee \u00b7 StatsCure Network"))
)

# ============================================================================
#  SERVER
# ============================================================================
server <- function(input, output, session) {

  pos_set <- c("pos","positive","1","yes","y","true","resp","response","r")
  is_pos  <- function(v) tolower(trimws(as.character(v))) %in% pos_set

  mk_dl <- function(name, df) downloadHandler(filename = function() name,
                                              content = function(f) write.csv(df, f, row.names = FALSE))
  output$op_dl  <- mk_dl("example_one_proportion.csv", ex_op)
  output$ni_dl  <- mk_dl("example_non_inferiority.csv", ex_ni)
  output$ka_dl  <- mk_dl("example_kappa.csv", ex_ka)
  output$sp_dl  <- mk_dl("example_spearman.csv", ex_sp)
  output$cmh_dl <- mk_dl("example_cmh.csv", ex_cmh)
  output$po_dl  <- mk_dl("example_poisson.csv", ex_po)

  # ---- live data store: driven by BOTH uploads and in-table cell edits -----
  store  <- reactiveValues(op = ex_op, ni = ex_ni, ka = ex_ka, sp = ex_sp, cmh = ex_cmh, po = ex_po)
  srcv   <- reactiveValues(op = "Example data", ni = "Example data", ka = "Example data",
                           sp = "Example data", cmh = "Example data", po = "Example data")
  tblver <- reactiveValues(op = 0, ni = 0, ka = 0, sp = 0, cmh = 0, po = 0)
  COLmap <- list(op = COL$op, ni = COL$ni, ka = COL$ka, sp = COL$sp, cmh = COL$cmh, po = COL$po)
  EXmap  <- list(op = ex_op, ni = ex_ni, ka = ex_ka, sp = ex_sp, cmh = ex_cmh, po = ex_po)

  for (pre in names(COLmap)) local({
    p <- pre; col <- COLmap[[p]]; exdf <- EXmap[[p]]
    # (a) upload a CSV -> replace the data and re-render the table
    observeEvent(input[[paste0(p, "_file")]], {
      res <- read_csv_or(input[[paste0(p, "_file")]], exdf)
      store[[p]] <- res$df; srcv[[p]] <- res$src; tblver[[p]] <- tblver[[p]] + 1
    })
    # (b) edit a cell -> update the stored data (client already shows the edit)
    observeEvent(input[[paste0(p, "_table_cell_edit")]], {
      store[[p]] <- DT::editData(store[[p]], input[[paste0(p, "_table_cell_edit")]], rownames = FALSE)
      srcv[[p]]  <- "Edited in table"
    })
    output[[paste0(p, "_src")]] <- renderText(srcv[[p]])
    output[[paste0(p, "_table")]] <- DT::renderDT({
      tblver[[p]]                     # re-render only on upload...
      dt(isolate(store[[p]]), col)    # ...not on every keystroke edit
    })
  })

  # =============================== 1. one-proportion ========================
  op_res <- eventReactive(input$op_run, {
    df <- store$op; rc <- if ("response" %in% names(df)) df$response else df[[ncol(df)]]
    y <- suppressWarnings(as.numeric(rc)); y <- y[!is.na(y)]
    n <- length(y); x <- sum(y == 1); p0 <- min(max(input$op_p0, 1e-6), 1 - 1e-6)
    bt <- binom.test(x, n, p0, alternative = "greater")
    ci <- binom.test(x, n, conf.level = input$op_conf)$conf.int
    list(x = x, n = n, p0 = p0, rate = x / n, p = bt$p.value, ci = ci, exp = n * p0)
  }, ignoreNULL = TRUE)
  output$op_rate <- renderText({ r <- op_res(); paste0(pct(r$rate), " (", r$x, "/", r$n, ")") })
  output$op_p    <- renderText(fmt(op_res()$p, 4))
  output$op_ci   <- renderText({ r <- op_res(); paste0(pct(r$ci[1]), "-", pct(r$ci[2])) })
  output$op_exp  <- renderText(fmt(op_res()$exp, 2))
  output$op_concl <- renderText({
    r <- op_res()
    sprintf("%d of %d responded (%s). Against the %s benchmark the one-sided exact p = %s - %s.",
            r$x, r$n, pct(r$rate), pct(r$p0), fmt(r$p, 4),
            if (r$p < 0.05) "the activity signal exceeds chance" else "not distinguishable from chance")
  })
  output$op_plot <- renderPlot({
    r <- op_res(); k <- 0:r$n; d <- dbinom(k, r$n, r$p0)
    cols <- ifelse(k >= r$x, COL$op, light(COL$op, .62)); bg_setup()
    bp <- barplot(d, names.arg = k, col = cols, border = NA, ylim = c(0, max(d) * 1.15),
                  xlab = "Number of responders", ylab = "P(X = k) under H0")
    grid(nx = NA, ny = NULL, col = "grey90"); barplot(d, col = cols, border = NA, add = TRUE, axes = FALSE)
    abline(v = bp[r$x + 1], col = COL$ink, lwd = 2.5, lty = 2)
    legend("topright", bty = "n", cex = 1.2, fill = c(COL$op, light(COL$op, .62)),
           legend = c(expression(X >= x), "rest"), border = NA)
  })

  # =============================== 2. non-inferiority =======================
  ni_res <- eventReactive(input$ni_run, {
    df <- store$ni; a <- tolower(df$arm)
    gi <- function(pat, fb) { i <- which(grepl(pat, a)); if (length(i)) i[1] else fb }
    i1 <- gi("new|exp|test", 1); i2 <- gi("standard|control|std|ref", 2)
    r1 <- df$responders[i1]; n1 <- r1 + df$non_responders[i1]
    r2 <- df$responders[i2]; n2 <- r2 + df$non_responders[i2]
    p1 <- r1 / n1; p2 <- r2 / n2; diff <- p1 - p2
    se <- sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)
    list(diff = diff, lo = diff - 1.96 * se, hi = diff + 1.96 * se,
         marg = -input$ni_margin / 100, met = (diff - 1.96 * se) > -input$ni_margin / 100)
  }, ignoreNULL = TRUE)
  output$ni_diff <- renderText(pct(ni_res()$diff))
  output$ni_low  <- renderText(pct(ni_res()$lo))
  output$ni_dec  <- renderText(if (ni_res()$met) "NI met" else "NI not met")
  output$ni_concl <- renderText({
    r <- ni_res()
    sprintf("Difference %s, 95%% CI [%s, %s]. Margin is %s. Because the lower bound %s the margin, non-inferiority is %s.",
            pct(r$diff), pct(r$lo), pct(r$hi), pct(r$marg),
            if (r$met) "stays above" else "crosses", if (r$met) "MET" else "NOT met")
  })
  output$ni_plot <- renderPlot({
    r <- ni_res(); xr <- range(c(r$lo, r$hi, r$marg, 0)); pad <- max(diff(xr) * 0.15, 0.02); xr <- xr + c(-pad, pad)
    par(bg = "white", mar = c(4.5, 1, 1.4, 1), cex.axis = 1.1, cex.lab = 1.25, font.lab = 2, col.axis = COL$ink, col.lab = COL$ink)
    plot(NA, xlim = xr, ylim = c(0, 1), axes = FALSE, xlab = "New - Standard", ylab = "")
    axis(1, at = pretty(xr), labels = paste0(round(100 * pretty(xr)), "%"))
    abline(v = 0, col = "grey70", lty = 3); abline(v = r$marg, col = COL$cmh, lwd = 2.5)
    text(r$marg, 0.9, "margin -\u0394", col = COL$cmh, pos = 4, font = 2, cex = 1.2)
    segments(r$lo, 0.5, r$hi, 0.5, lwd = 10, col = if (r$met) COL$ni else COL$cmh, lend = 1)
    points(r$diff, 0.5, pch = 18, cex = 3.4, col = COL$ink)
  })

  # =============================== 3. kappa =================================
  ka_res <- eventReactive(input$ka_run, {
    df <- store$ka
    ca <- if ("readera" %in% names(df)) df$readera else df[[1]]
    cb <- if ("readerb" %in% names(df)) df$readerb else df[[2]]
    A <- is_pos(ca); B <- is_pos(cb)
    a <- sum(A & B); b <- sum(A & !B); c <- sum(!A & B); d <- sum(!A & !B)
    N <- a + b + c + d; if (N == 0) N <- 1
    po <- (a + d) / N; pe <- ((a + b) * (a + c) + (c + d) * (b + d)) / N^2
    list(a = a, b = b, c = c, d = d, N = N, po = po, pe = pe,
         kap = if ((1 - pe) == 0) NA else (po - pe) / (1 - pe))
  }, ignoreNULL = TRUE)
  ka_band <- function(k) { if (is.na(k)) return("-"); if (k < 0) return("poor")
    if (k <= .2) "slight" else if (k <= .4) "fair" else if (k <= .6) "moderate" else if (k <= .8) "substantial" else "near-perfect" }
  output$ka_xtab <- renderTable({
    r <- ka_res()
    data.frame(check.names = FALSE, ` ` = c("A: Pos", "A: Neg", "Total"),
      `B: Pos` = c(r$a, r$c, r$a + r$c), `B: Neg` = c(r$b, r$d, r$b + r$d), Total = c(r$a + r$b, r$c + r$d, r$N))
  }, striped = TRUE, bordered = TRUE, align = "lccc", width = "100%")
  output$ka_po   <- renderText(pct(ka_res()$po))
  output$ka_pe   <- renderText(pct(ka_res()$pe))
  output$ka_kap  <- renderText(fmt(ka_res()$kap))
  output$ka_band <- renderText(ka_band(ka_res()$kap))
  output$ka_concl <- renderText({ r <- ka_res()
    sprintf("Raw agreement %s, chance agreement %s. Chance-corrected kappa = %s (%s agreement).",
            pct(r$po), pct(r$pe), fmt(r$kap), ka_band(r$kap)) })
  output$ka_plot <- renderPlot({
    r <- ka_res(); vals <- c(`Raw Po` = r$po, `Chance Pe` = r$pe, `Kappa` = ifelse(is.na(r$kap), 0, r$kap))
    cols <- c(light(COL$ka, .55), "#E9C46A", COL$ka); bg_setup()
    bp <- barplot(vals, col = cols, border = NA, ylim = c(min(0, vals) - .05, 1.08), ylab = "Proportion / \u03ba")
    text(bp, vals, labels = sprintf("%.2f", vals), pos = 3, font = 2, cex = 1.3, col = COL$ink); abline(h = 0, col = "grey80")
  })

  # =============================== 4. spearman =============================
  sp_res <- eventReactive(input$sp_run, {
    df <- store$sp; num <- df[, sapply(df, is.numeric), drop = FALSE]
    validate(need(ncol(num) >= 2 && nrow(num) >= 3, "Need >= 2 numeric columns and >= 3 rows."))
    x <- num[[1]]; y <- num[[2]]; ok <- complete.cases(x, y); x <- x[ok]; y <- y[ok]
    ct <- suppressWarnings(cor.test(x, y, method = "spearman"))
    list(x = x, y = y, rho = unname(ct$estimate), p = ct$p.value, n = length(x), nm = names(num)[1:2])
  }, ignoreNULL = TRUE)
  output$sp_rho <- renderText(fmt(sp_res()$rho))
  output$sp_p   <- renderText(fmt(sp_res()$p, 4))
  output$sp_n   <- renderText(as.character(sp_res()$n))
  output$sp_concl <- renderText({ r <- sp_res()
    sprintf("Spearman \u03c1 = %s (p = %s) between %s and %s - a %s%s monotonic association.",
            fmt(r$rho), fmt(r$p, 4), r$nm[1], r$nm[2],
            ifelse(abs(r$rho) > .7, "strong ", ifelse(abs(r$rho) > .4, "moderate ", "weak ")),
            ifelse(r$rho >= 0, "positive", "negative")) })
  output$sp_raw <- renderPlot({
    r <- sp_res(); bg_setup()
    plot(r$x, r$y, pch = 19, col = COL$sp, cex = 2, xlab = r$nm[1], ylab = r$nm[2]); grid(col = "grey90")
    lo <- tryCatch(lowess(r$x, r$y), error = function(e) NULL); if (!is.null(lo)) lines(lo, col = "#9A3412", lwd = 3)
  })
  output$sp_rank <- renderPlot({
    r <- sp_res(); bg_setup()
    plot(rank(r$x), rank(r$y), pch = 19, col = COL$sp, cex = 2,
         xlab = paste0("rank(", r$nm[1], ")"), ylab = paste0("rank(", r$nm[2], ")"))
    abline(0, 1, col = "grey70", lty = 3); grid(col = "grey90")
  })

  # =============================== 5. CMH ==================================
  cmh_res <- eventReactive(input$cmh_run, {
    df <- store$cmh; st <- unique(df$stratum)
    arr <- array(0, dim = c(2, 2, length(st))); ors <- numeric(length(st))
    for (i in seq_along(st)) {
      s <- st[i]; rA <- df[df$stratum == s & toupper(df$arm) == "A", ]; rB <- df[df$stratum == s & toupper(df$arm) == "B", ]
      a <- rA$resp[1]; b <- rA$no[1]; c <- rB$resp[1]; d <- rB$no[1]
      arr[, , i] <- matrix(c(a, c, b, d), 2); ors[i] <- (a * d) / (b * c)
    }
    mh <- tryCatch(mantelhaen.test(arr, correct = FALSE), error = function(e) NULL)
    list(st = st, ors = ors, mh = if (is.null(mh)) NA else unname(mh$estimate), p = if (is.null(mh)) NA else mh$p.value)
  }, ignoreNULL = TRUE)
  output$cmh_tab <- renderTable({ r <- cmh_res()
    data.frame(Stratum = as.character(r$st), OR = round(r$ors, 2), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, align = "lc", width = "100%")
  output$cmh_mh <- renderText(fmt(cmh_res()$mh, 2))
  output$cmh_p  <- renderText(fmt(cmh_res()$p, 4))
  output$cmh_concl <- renderText({ r <- cmh_res()
    sprintf("Stratum ORs range %s-%s; pooled across strata the common MH OR = %s (p = %s).",
            fmt(min(r$ors), 2), fmt(max(r$ors), 2), fmt(r$mh, 2), fmt(r$p, 4)) })
  output$cmh_plot <- renderPlot({
    r <- cmh_res(); vals <- c(r$ors, r$mh); labs <- c(as.character(r$st), "Pooled MH")
    cols <- c(rep(light(COL$cmh, .55), length(r$st)), COL$cmh); bg_setup()
    bp <- barplot(vals, names.arg = labs, col = cols, border = NA, ylim = c(0, max(vals, na.rm = TRUE) * 1.2), ylab = "Odds ratio")
    text(bp, vals, labels = sprintf("%.2f", vals), pos = 3, font = 2, cex = 1.3, col = COL$ink); abline(h = 1, col = "grey70", lty = 3)
  })

  # =============================== 6. poisson ==============================
  po_res <- eventReactive(input$po_run, {
    df <- store$po; ag <- aggregate(cbind(events = df$aes, time = df$months), by = list(arm = df$arm), FUN = sum)
    ag <- ag[order(toupper(ag$arm)), ]; ag$rate <- ag$events / ag$time
    xa <- ag$events[1]; ta <- ag$time[1]; xb <- ag$events[2]; tb <- ag$time[2]
    pt <- tryCatch(poisson.test(c(xa, xb), T = c(ta, tb)), error = function(e) NULL)
    list(ag = ag, irr = if (is.null(pt)) NA else unname(pt$estimate),
         ci = if (is.null(pt)) c(NA, NA) else pt$conf.int, p = if (is.null(pt)) NA else pt$p.value)
  }, ignoreNULL = TRUE)
  output$po_tab <- renderTable({ a <- po_res()$ag
    data.frame(Arm = a$arm, Events = a$events, `Person-months` = a$time, `Rate/pt-mo` = round(a$rate, 3), check.names = FALSE)
  }, striped = TRUE, bordered = TRUE, align = "lccc", width = "100%")
  output$po_irr <- renderText(fmt(po_res()$irr))
  output$po_ci  <- renderText({ r <- po_res(); paste0(fmt(r$ci[1], 2), "-", fmt(r$ci[2], 2)) })
  output$po_p   <- renderText(fmt(po_res()$p, 4))
  output$po_concl <- renderText({ r <- po_res(); a <- r$ag
    sprintf("Rate %s = %s vs %s = %s per pt-month. IRR = %s (95%% CI %s-%s) - arm %s has the %s rate.",
            a$arm[1], fmt(a$rate[1]), a$arm[2], fmt(a$rate[2]), fmt(r$irr), fmt(r$ci[1], 2), fmt(r$ci[2], 2),
            a$arm[1], ifelse(r$irr < 1, "lower", "higher")) })
  output$po_plot <- renderPlot({
    a <- po_res()$ag; cols <- c(COL$po, light(COL$po, .5))[seq_len(nrow(a))]; bg_setup()
    bp <- barplot(a$rate, names.arg = a$arm, col = cols, border = NA, ylim = c(0, max(a$rate) * 1.25),
                  ylab = "Rate per patient-month", xlab = "Arm")
    text(bp, a$rate, labels = sprintf("%.3f", a$rate), pos = 3, font = 2, cex = 1.3, col = COL$ink)
  })
}

shinyApp(ui, server)
