dat <- eventReactive({
  input$pop_params_cell_edit
  input$transition_params_cell_edit
  1
}  ,{
  process()
})



output$pop_params = render_dt({
  colnames(pop_params) = ifelse(str_detect(colnames(pop_params),"_"),
                                str_replace(colnames(pop_params),"_","\n"),
                                colnames(pop_params)) 
  pop_params %>% mutate_if(is.numeric, round,digits = 2)},
  'cell',options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '150px',height = "50px", targets = "_all")), 
    paging = FALSE,searching = FALSE,info = FALSE
  ),rownames= FALSE)
observeEvent(input$pop_params_cell_edit, {
  pop_params <<- editData(pop_params, input$pop_params_cell_edit, 'pop_params')
  
})



output$transition_params = render_dt(transition_params %>% mutate_if(is.numeric, round,digits = 2), 'cell',options = list(
  autoWidth = TRUE,
  columnDefs = list(list(width = '100px',height = "50px", targets = "_all")), 
  paging = FALSE,searching = FALSE,info = FALSE
),rownames= FALSE)
observeEvent(input$transition_params_cell_edit, {
  transition_params <<- editData(transition_params, input$transition_params_cell_edit, 'transition_params')
  transition_params <<- transition_params %>%
    mutate(mu = log(med),
           sigma_sqre = log((1 + sqrt(1+4*SD^2/med^2))/2),
           mean_est = med*exp(sigma_sqre/2),
           prob = c(1/mean_est))
  replaceData(dataTableProxy("transition_params"),transition_params)
})



# first output
output$p_all <- renderPlotly({
  print(
    ggplotly(dat() %>% 
               select(t,susceptible,exposed,infectious,symp,ssick,critical,recovered_completely,died,tot_alive) %>% 
               gather(state,count,-t) %>% 
               group_by(t,state) %>% 
               summarise(count = sum(count)) %>% 
               ggplot(aes(x=t, y=count/9001, color = state)) + 
               geom_line() + 
               scale_x_continuous(breaks = seq(0, 100, by = 10))+
               labs(title = "Epidemic dynamics according to the Episim (fraction of initial population)",
                    y = "Fraction of initial population"))
  )
})

# second output
output$p_r0 <- renderPlotly({
  print(
    ggplotly(dat() %>% 
               select(t,infectious) %>% 
               group_by(t) %>% 
               summarise(infectious = sum(infectious)) %>% 
               ungroup() %>% 
               mutate(r0 = infectious/lag(infectious)) %>% 
               ggplot(aes(x=t,y=r0)) +
               geom_line() +
               labs(title = "r0") + 
               scale_y_continuous(breaks = seq(0, 1.2, by = 0.2))+
               scale_x_continuous(breaks = seq(0, 100, by = 10))+
               expand_limits(y=0))
  )
})



