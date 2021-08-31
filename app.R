library(shiny)
library(tibble)
library(ggplot2)
library(DentalAge)

C_P3_P4_Scores <- c("Missing", "C.i", "C.co", "C.oc", "Cr.5", "Cr.75", "Cr.c",
                    "R.i", "R.25", "R.5", "R.75", "R.c", "A.5")

M1_M2_M3_Scores <- c("Missing","C.i", "C.co", "C.oc", "Cr.5", "Cr.75", "Cr.c",
                     "R.i", "Cl.i", "R.25", "R.5", "R.75", "R.c", "A.5")

# d <- list(
#   Sex = "M",
#   Canine = "R.25",
#   P3 = "R.25",
#   P4 = "R.i",
#   M1 = "A.5",
#   M2 = "R.25",
#   M3 = NA
# )

############################################################################

ui <- fluidPage(
  fluidRow(
    column(2, selectInput("Sex", label = "Sex", choices = c("Female", "Male"),
                          selected = "Male"))
  ),

  fluidRow(
    column(2,
           selectInput("Canine", label = "Canine", choices = C_P3_P4_Scores,
                       selected = "R.25")),

    column(2,
           selectInput("P3", label = "P3", choices = C_P3_P4_Scores,
                       selected = "R.25")),
    column(2,
           selectInput("P4", label = "P4", choices = C_P3_P4_Scores,
                       selected = "R.i")),

    column(2,
           selectInput("M1", label = "M1", choices = M1_M2_M3_Scores,
                       selected = "A.5")),

    column(2,
           selectInput("M2", label = "M2", choices = M1_M2_M3_Scores,
                       selected = "R.25")),

    column(2,
           selectInput("M3", label = "M3", choices = M1_M2_M3_Scores,
                       selected = "Missing"))
  ),

  fluidRow(
    column(12,
           plotOutput("plot", height = "600px", width = "100%")
    )
  )

)

server <- function(input, output, session) {
  # Get the stages
  d <- reactive(
    list(Sex = ifelse(input$Sex == "Female", "F", "M"),
         Canine = ifelse(input$Canine == "Missing", NA, input$Canine),
         P3 = ifelse(input$P3 == "Missing", NA, input$P3),
         P4 = ifelse(input$P4 == "Missing", NA, input$P4),
         M1 = ifelse(input$M1 == "Missing", NA, input$M1),
         M2 = ifelse(input$M2 == "Missing", NA, input$M2),
         M3 = ifelse(input$M3 == "Missing", NA, input$M3))
  )

  # Generate estimates and plot
  output$plot <- renderPlot({
    interval <- 0.5

    means <- get_means_for_scores(x = d())
    dental_age <- estimate_dental_age(means)
    age_CI <- estimate_age_hdi(dental_age, n = 1e5, interval = interval)

    samp <- age_samples(dental_age, n = 1e5)

    ggplot() +
      geom_density(data = data.frame(samp),
                   aes(samp), fill = "gray90") +
      geom_vline(xintercept = dental_age["dental_age"], color = "darkred",
                 size = 2) +
      geom_vline(xintercept = age_CI["lower_bound"], color = "darkred",
                 linetype = "dotted", size = 1) +
      geom_vline(xintercept = age_CI["upper_bound"], color = "darkred",
                 linetype = "dotted", size = 1) +
      labs(x = "Age (y)", y = "Probability Density",
           title = paste0("Dental age estimate: ",
                          round(dental_age["dental_age"], 2),
                          " y"),
           subtitle = paste0(sprintf("%0.1f%%", interval * 100),
                             " interval: ",
                             round(age_CI["lower_bound"], 2),
                             "-",
                             round(age_CI["upper_bound"], 2),
                             " y")) +
      theme_bw()
  })
}

# Run the application
shinyApp(ui = ui, server = server)
