# app.R
library(shiny)
library(bslib)
library(ggplot2)

ui <- page_fillable(
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly",
    base_font = font_google("Roboto")
  ),
  
  layout_sidebar(
    sidebar = sidebar(
      h4("Simulation Controls"),
      sliderInput("n", "Sample size per group:", min = 5, max = 100, value = 20, step = 5),
      sliderInput("effect", "Effect size (mean difference):", min = 0, max = 5, value = 1, step = 0.1),
      sliderInput("sd", "Standard deviation:", min = 0.1, max = 5, value = 2, step = 0.1),
      actionButton("resample", "Resample", class = "btn-primary")
    ),
    
    card(
      card_header("Group Comparison"),
      card_body(
        htmlOutput("ttest"),
        plotOutput("boxplot", height = "350px")
      )
    )
  )
)

server <- function(input, output) {
  
  data_gen <- reactive({
    input$resample
    isolate({
      n <- input$n
      sd <- input$sd
      eff <- input$effect
      
      group1 <- rnorm(n, mean = 0, sd = sd)
      group2 <- rnorm(n, mean = eff, sd = sd)
      data.frame(
        value = c(group1, group2),
        group = factor(rep(c("Group 1", "Group 2"), each = n))
      )
    })
  })
  
  output$boxplot <- renderPlot({
    dat <- data_gen()
    ggplot(data = dat,
           aes(x = group,
               y = value,
               fill = group))+
      geom_boxplot(outlier.shape = NA)+
      geom_jitter(size = 4,
                  width = .2,
                  shape = 21,
                  colour = "white")+
      scale_fill_manual(values = c("#4DAF4A", "#377EB8"))+
      labs( y = "Value",
            x = "",
            title = "Two-Sample Comparison")+
      theme_classic(base_size = 20)+
      theme(legend.position = "none")
  })
  
  output$ttest <- renderUI({
    dat <- data_gen()
    t_res <- t.test(value ~ group, data = dat)
    pval <- t_res$p.value
    
    sig <- if (pval < 0.05) {
      "<span style='color:green;font-size:1.5em;'>✅ Significant difference (p < 0.05)</span>"
    } else {
      "<span style='color:red;font-size:1.5em;'>❌ Not significant (p ≥ 0.05)</span>"
    }
    
    HTML(paste0(
      "<b>Two-sample t-test p-value:</b> ", sig
    ))
  })
}

shinyApp(ui, server)
