shiny::tabPanel(
  title = "Parameters manipulation",
  fluidRow(
    column(
      width = 6,offset = 0, style='padding:0px;',
      h3("Population parameters"),
      dataTableOutput('pop_params'),
      h3("Transition parameters"),
      dataTableOutput('transition_params')
    ),
    column(
      width = 6,offset = 0, style='padding:0px;',
      plotlyOutput("p_all"),
      plotlyOutput("p_r0")
    )
  )
)
