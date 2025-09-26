setwd("/Users/liyaung/Desktop/MIG666")

# install.packages(c("shiny", "shinydashboard", "plotly", "ggplot2", "dplyr"))
library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)

# --- load your CSV data (adjust path if needed) ---
site_df <- read.csv("mig666_data/site_df.csv")
dm_df   <- read.csv("mig666_data/dm_df.csv")
ae_df   <- read.csv("mig666_data/ae_df.csv")
ex_df   <- read.csv("mig666_data/ex_df.csv")
ts_df   <- read.csv("mig666_data/ts_df.csv")
ta_df   <- read.csv("mig666_data/ta_df.csv")
te_df   <- read.csv("mig666_data/te_df.csv")
tv_df   <- read.csv("mig666_data/tv_df.csv")

# --- shiny app ---
ui <- dashboardPage(
  dashboardHeader(title = "MIG666 Study Dashboard"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Demographics", tabName = "demo", icon = icon("users")),
      menuItem("Adverse Events", tabName = "ae", icon = icon("exclamation-triangle")),
      menuItem("Exposure", tabName = "ex", icon = icon("pills")),
      menuItem("Sites", tabName = "sites", icon = icon("hospital")),
      menuItem("Visits", tabName = "visits", icon = icon("calendar"))
    )
  ),
  dashboardBody(
    tabItems(
      # Demographics
      tabItem(tabName = "demo",
              fluidRow(
                box(plotlyOutput("age_hist"), width = 6),
                box(plotlyOutput("sex_bar"), width = 6),
                box(plotlyOutput("race_bar"), width = 6),
                box(plotlyOutput("age_sex"), width = 6),
                box(plotlyOutput("arm_counts"), width = 6)
              )),
      # Adverse Events
      tabItem(tabName = "ae",
              fluidRow(
                box(plotlyOutput("ae_terms"), width = 6),
                box(plotlyOutput("ae_severity"), width = 6),
                box(plotlyOutput("ae_outcome"), width = 6),
                box(plotlyOutput("ae_by_arm"), width = 6),
                box(plotlyOutput("ae_timeline"), width = 12)
              )),
      # Exposure
      tabItem(tabName = "ex",
              fluidRow(
                box(plotlyOutput("dose_by_arm"), width = 6),
                box(plotlyOutput("duration_hist"), width = 6),
                box(plotlyOutput("exposure_box"), width = 6)
              )),
      # Sites
      tabItem(tabName = "sites",
              fluidRow(
                box(plotlyOutput("sites_per_country"), width = 6),
                box(plotlyOutput("subjects_per_site"), width = 6),
                box(plotlyOutput("map_placeholder"), width = 12)
              )),
      # Visits
      tabItem(tabName = "visits",
              fluidRow(
                box(plotlyOutput("visit_schedule"), width = 12)
              ))
    )
  )
)

server <- function(input, output) {
  # --- demographics ---
  output$age_hist <- renderPlotly({
    ggplot(dm_df, aes(x = AGE)) + geom_histogram(binwidth = 5, fill = "steelblue") +
      labs(title = "Age Distribution")
  })
  output$sex_bar <- renderPlotly({
    ggplot(dm_df, aes(x = SEX, fill = SEX)) + geom_bar() + labs(title = "Sex Distribution")
  })
  output$race_bar <- renderPlotly({
    ggplot(dm_df, aes(x = RACE, fill = RACE)) + geom_bar() + labs(title = "Race Distribution")
  })
  output$age_sex <- renderPlotly({
    ggplot(dm_df, aes(x = SEX, y = AGE, fill = SEX)) + geom_boxplot() + labs(title = "Age by Sex")
  })
  output$arm_counts <- renderPlotly({
    ggplot(dm_df, aes(x = ARM, fill = ARM)) + geom_bar() + labs(title = "Subjects per Arm")
  })

  # --- adverse events ---
  output$ae_terms <- renderPlotly({
    ggplot(ae_df, aes(x = AETERM)) + geom_bar(fill = "darkred") +
      labs(title = "Most Common AE Terms") + coord_flip()
  })
  output$ae_severity <- renderPlotly({
    ggplot(ae_df, aes(x = AESEV, fill = AESEV)) + geom_bar() + labs(title = "AE Severity")
  })
  output$ae_outcome <- renderPlotly({
    ggplot(ae_df, aes(x = AEOUT, fill = AEOUT)) + geom_bar() + labs(title = "AE Outcome") +
      coord_flip()
  })
  output$ae_by_arm <- renderPlotly({
    ggplot(ae_df %>% left_join(dm_df, by="USUBJID"),
           aes(x = ARM, fill = AESEV)) + geom_bar(position="dodge") +
      labs(title = "AE by Arm and Severity")
  })
  output$ae_timeline <- renderPlotly({
    ggplot(ae_df, aes(x = as.Date(AESTDTC), fill = AESEV)) +
      geom_histogram(binwidth = 3) + labs(title = "AE Timeline (Study Days)")
  })

  # --- exposure ---
  output$dose_by_arm <- renderPlotly({
    ggplot(ex_df, aes(x = ARMCD, y = EXDOSE, fill = ARMCD)) + geom_col() +
      labs(title = "Planned Dose per Arm")
  })
  output$duration_hist <- renderPlotly({
    ggplot(ex_df, aes(x = EXDUR)) + geom_histogram(fill = "purple", binwidth = 1) +
      labs(title = "Exposure Duration (days)")
  })
  output$exposure_box <- renderPlotly({
    ggplot(ex_df, aes(x = ARMCD, y = EXDUR, fill = ARMCD)) + geom_boxplot() +
      labs(title = "Exposure Duration by Arm")
  })

  # --- sites ---
  output$sites_per_country <- renderPlotly({
    ggplot(site_df, aes(x = COUNTRY, fill = COUNTRY)) + geom_bar() +
      labs(title = "Sites per Country") + coord_flip()
  })
  output$subjects_per_site <- renderPlotly({
    ggplot(dm_df, aes(x = SITEID)) + geom_bar(fill = "darkgreen") +
      labs(title = "Subjects per Site") + coord_flip()
  })
  output$map_placeholder <- renderPlotly({
    plot_ly(type="scattergeo", mode="markers",
            locations=site_df$COUNTRY,
            locationmode="country names",
            marker=list(size=10, color="red")) %>%
      layout(title="Participating Countries (Placeholder Map)")
  })

  # --- visits ---
  output$visit_schedule <- renderPlotly({
    ggplot(tv_df, aes(x = VISITNUM, y = TVSTRL, label = VISIT)) +
      geom_line() + geom_point(size=3, color="blue") +
      geom_text(vjust=-1) + labs(title="Visit Schedule (Days)", x="Visit #", y="Study Day")
  })
}

shinyApp(ui, server)
