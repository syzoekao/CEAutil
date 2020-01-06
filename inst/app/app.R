library(shiny)
library(CEAutil)

dist_all <- c("normal", "log-normal", "truncated-normal",
              "beta", "gamma", "dirichlet", "bootstrap", "constant")

all_param_types <- c("mean, sd", "a, b", "shape, scale",
                     "value, mean_prop, sd", "value, n",
                     "value, alpha", "mean, sd, ll, ul", "val", "meanlog, sdlog")
all_param_types <- unique(strsplit(paste(all_param_types, collapse = ', '), ", ")[[1]])

ui <- fluidPage(
  titlePanel("Generate gen_psa_samp code for dampack"),
  sidebarLayout(

    sidebarPanel(
      numericInput("nsamp", "number of sample", 100),
      textInput("param", "param"),
      selectInput("dist", "dist", choices = dist_all),

      conditionalPanel(
        condition = "input.dist == \"normal\"",
        numericInput("norm.mean", "mean", value = NA),
        numericInput("norm.sd", "sd", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"log-normal\"",
        verbatimTextOutput("lnorm.text"),
        numericInput("lnorm.mean", "mean", value = NA),
        numericInput("lnorm.sd", "sd", value = NA),
        numericInput("meanlog", "log mean", value = NA),
        numericInput("sdlog", "log sd", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"truncated-normal\"",
        numericInput("tnorm.mean", "mean", value = NA),
        numericInput("tnorm.sd", "sd", value = NA),
        numericInput("ll", "lower lim", value = NA),
        numericInput("ul", "upper lim", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"beta\"",
        verbatimTextOutput("beta.text"),
        numericInput("a", "a", value = NA),
        numericInput("b", "b", value = NA),
        numericInput("beta.mean", "mean", value = NA),
        numericInput("beta.sd", "sd", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"gamma\"",
        verbatimTextOutput("gamma.text"),
        numericInput("shape", "shape", value = NA),
        numericInput("scale", "scale", value = NA),
        numericInput("gamma.mean", "mean", value = NA),
        numericInput("gamma.sd", "sd", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"dirichlet\"",
        verbatimTextOutput("diri.text"),
        numericInput("value", "value", value = NA),
        numericInput("mean.prop", "mean prop", value = NA),
        numericInput("diri.sd", "sd", value = NA),
        numericInput("n", "n", value = NA),
        numericInput("alpha", "alpha", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"constant\"",
        numericInput("val", "val", value = NA)
      ),

      conditionalPanel(
        condition = "input.dist == \"bootstrap\"",
        verbatimTextOutput("bootstrap.text")
      ),

      actionButton("update", "update table"),
      width = 3),

    mainPanel(
      tableOutput("table"),
      actionButton("gen.code", "generate code"),
      textOutput("show.code")
    )
  )
)


server <- function(input, output, session) {
  output$bootstrap.text <- renderText({
    "boostrap is not incorporated"
  })

  output$lnorm.text <- renderText({
    paste("inputs could be\n(1) log(mean) and log(sd) or\n(2) mean and sd")
  })

  output$beta.text <- renderText({
    paste("inputs could be\n(1) a and b or\n(2) mean and sd")
  })

  output$gamma.text <- renderText({
    paste("inputs could be\n(1) shape and scale or\n(2) mean and sd")
  })

  output$diri.text <- renderText({
    # paste("inputs could be\n(1) value, mean_prop, and sd \n(2) value, n, or\n(3) value, alpha")
    "dirichlet is not working yet"
  })

  values <- reactiveValues()
  values$df <- data.frame(param = character(),
                          dist = character(),
                          mean = numeric(0), sd = numeric(0),
                          a = numeric(0), b = numeric(0),
                          shape = numeric(0), scale = numeric(0),
                          value = numeric(0), mean_prop = numeric(0),
                          n = numeric(0),
                          alpha = numeric(0),
                          ll = numeric(0), ul = numeric(0),
                          val = numeric(0), meanlog = numeric(0),
                          sdlog = numeric(0),
                          stringsAsFactors=FALSE)

  newEntry <- observeEvent(input$update, {
    if (input$update > 0) {
      if (input$dist == "normal") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      input$norm.mean, input$norm.sd,
                                                      rep(NA, 13)))
      }

      if (input$dist == "log-normal") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      input$lnorm.mean, input$lnorm.sd,
                                                      rep(NA, 11),
                                                      input$meanlog, input$sdlog))
      }

      if (input$dist == "truncated-normal") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      input$tnorm.mean, input$tnorm.sd,
                                                      NA, NA, NA, NA, NA, NA, NA, NA,
                                                      input$ll, input$ul, NA, NA, NA))
      }

      if (input$dist == "beta") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      input$beta.mean, input$beta.sd,
                                                      input$a, input$b, rep(NA, 11)))
      }

      if (input$dist == "gamma") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      input$gamma.mean, input$gamma.sd,
                                                      NA, NA, input$shape, input$scale,
                                                      rep(NA, 9)))
      }

      if (input$dist == "dirichlet") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      NA, input$diri.sd,
                                                      NA, NA, NA, NA,
                                                      input$value, input$mean.prop,
                                                      input$n, input$alpha, NA, NA, NA, NA, NA))
      }

      if (input$dist == "constant") {
        isolate(values$df[nrow(values$df) + 1, ] <- c(input$param, input$dist,
                                                      rep(NA, 12), input$val,
                                                      NA, NA))
      }

      updateTextInput(session, "norm.mean", value = NA)
      updateTextInput(session, "norm.sd", value = NA)
      updateTextInput(session, "lnorm.mean", value = NA)
      updateTextInput(session, "lnorm.sd", value = NA)
      updateTextInput(session, "tnorm.mean", value = NA)
      updateTextInput(session, "tnorm.sd", value = NA)
      updateTextInput(session, "beta.mean", value = NA)
      updateTextInput(session, "beta.sd", value = NA)
      updateTextInput(session, "gamma.mean", value = NA)
      updateTextInput(session, "gamma.sd", value = NA)
      updateTextInput(session, "diri.sd", value = NA)

      updateTextInput(session, "a", value = NA)
      updateTextInput(session, "b", value = NA)
      updateTextInput(session, "shape", value = NA)
      updateTextInput(session, "scale", value = NA)
      updateTextInput(session, "value", value = NA)
      updateTextInput(session, "mean.prop", value = NA)
      updateTextInput(session, "n", value = NA)
      updateTextInput(session, "alpha", value = NA)
      updateTextInput(session, "ll", value = NA)
      updateTextInput(session, "ul", value = NA)
      updateTextInput(session, "val", value = NA)
      updateTextInput(session, "meanlog", value = NA)
      updateTextInput(session, "sdlog", value = NA)
    }
  })

  output$table <- renderTable({
    values$df
  })

  output$show.code <- renderPrint({
    if (input$gen.code > 0) {
        gen_psa_samp_code(values$df, n_samp = input$nsamp)
    }
  })
}

shinyApp(ui, server)


