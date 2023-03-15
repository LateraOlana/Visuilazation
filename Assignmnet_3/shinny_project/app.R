library(here)
source(here("scripts/source_code_for_shiny.R"))

ui <- fluidPage(
  
  title = "Forced expiratory volume (FEV) with other factors",
  
  # meta tags
  meta() %>%
    meta_social(
      title = "Forced expiratory volume (FEV)",
      description = "Exercise with shiny scrollytelling",
      image = "https://raw.githubusercontent.com/LateraOlana/BLM/main/1200px-Black_Lives_Matter_logo.svg%5B1%5D.png?token=GHSAT0AAAAAAB4ZZAKTRW7KVREPWF2RCLWSY5ZCSJA",
      image_alt = "BLM"
    ),
  
  # suppress warning messages while data is loading on-screen
  tags$style(
    type = "text/css",
    ".shiny-output-error { visibility: hidden; }",
    ".shiny-output-error:before { visibility: hidden; }"
  ),
  tags$head(includeCSS("www/style.css")),
  
  # article title & name
  fluidRow(
    HTML(
      "<center>
                <h1>Studying variables impacting Forced Expiratory Volume (FEV) through visualization</h1>
                <p style='font-size:26px'> by <a href='https://github.com/LateraOlana' target='_blank'>Latera Tesfaye</a></p>
                </center>"
    )
  ),
  
  br(),
  
  fluidRow(column(1),
           
           column(
             10,
             # intro text
             fluidRow(id = 'text',
                      column(1),
                      column(
                        10,
                        br(),
                        text0,
                        hr(),
                        h1(
                          class = "instructions",
                          "How to read this graph?",
                          br(),
                          br(),
                          "Hover over each points to see details of forced expiratory volume (FEV) in liters per second, 
                          height in centimeters, age in years, and creatinine level in milligrams / decilitre.",
                         
                          "Double click on a each legend labels to focus on a specific category of smoking level. In addition, you can use your mouse to select area you want to see."
                        )
                      ),
                      column(1)),
             # plot object for intro
             plotlyOutput("introPlot", height = '400px')
           ),
           
           column(1)),
  
  # scrollytelling plot
  scrolly_container(
    "scr"
    ,
    scrolly_graph(
      br(),
      br(),
      textOutput("section"),
      br(),
      HTML('<center>'),
      plotlyOutput("plot", height = '600px'),
      HTML('</center>')
      
    )
    ,
    scrolly_sections(
      HTML('<center>'),
      scrolly_section(id = 0, render_text(0)),
      scrolly_section(id = 1, render_text(1)),
      scrolly_section(id = 2, render_text(2)),
      scrolly_section(id = 3, render_text(3)),
      scrolly_section(id = 4, render_text(4)),
      scrolly_section(id = 5, render_text(5)),
      # add a scrolly_section with nothing in it;
      # this buffer prevents the plot from disappearing while reading last section
      scrolly_section(id = "buffer", br()),
      HTML('</center>')
    )
    
  ),
  
  # concluding text
  div(fluidRow(
    id = 'text',
    column(2),
    column(8,
           concludingtext,
           br()),
    column(2)
  ), style = 'margin-top: -300px;'),
  
  br(),
  br(),
  br(),
  hr(),
  
  fluidRow(column(1),
           column(10,
                  technicalnotes),
           column(1)),
  br(),
  br(),
  column(1)
  
)

# server
server <- function(input, output, session) {
  output$plot <- renderPlotly({
    add <- input$scr
    
    plot <- data %>%
      filter(if (add != 5)
        add >= reveal
        else
          reveal %in% c(1:5)) %>%
      ggplot(aes(x=height, y=fev, col = smoking_label)) +
      geom_point(
        mapping = aes(
          
          alpha = ifelse(add == reveal, 1 / 5, 1 / 10),
          
          text = glue::glue(
            '<span style = "font-size:1.0em">Patient ID: {ptid}</span><br>
                                                <i>Forced Expiratory Volume (FEV) </i>: {comma(fev, digits = 0)} liters/second
                                                <i>Height</i>: {comma(height, digits = 0)} centimeters
                                                <i>Age</i>: {comma(age, digits = 0)} years
                                                <i>Creatinine level</i>: {comma(crt)} milligrams / decilitre'
          )
        )
      ) +
      geom_smooth(method = "lm", alpha=(1 / 4), linewidth=1)+
      
      scale_size(range = c(1, 20)) +
      xlab("\nHeight in centimeters") +
      ylab("Forced expiratory volume (FEV) \n liters per second") +
      labs(size = "",
           col = "",
           alpha = "") +
      scale_color_manual(values = cols, breaks = legend_ord) +
      scale_x_continuous(
        trans='log10',
        
        limits = c(138, 190)
      ) +
      scale_y_continuous(
                         limits = c(0, 5)) +
      # cr::drop_axis(axis = "y") +
      theme(
        axis.line.x = ggplot2::element_line(
          colour = NULL,
          size = NULL,
          linetype = NULL,
          lineend = NULL
        ),
        axis.line.y = ggplot2::element_blank(),
        panel.grid.major.x = element_blank()
      )
    
    ggplotly(plot, tooltip = 'text') %>%
      layout(
        title = list(element_blank()),
        legend = list(x = 0.1, y = 0.985),
        font = list(family = 'Lato'),
        margin = list(t = 50),
        hoverlabel = list(bgcolor = 'whitesmoke', color = 'darkGray')
      ) %>%
      config(
        displaylogo = F,
        showSendToCloud = F,
        displayModeBar = F
      )
    
  })
  
  output$introPlot <- renderPlotly({
    introPlot
  })
  output$scr <- renderScrollytell({
    scrollytell()
  })
  renderText(paste0("Section: ", input$scr))
  observe({
    cat("section:", input$scr, "\n")
  })
  
}
# Run the application
shinyApp(ui = ui, server = server)
