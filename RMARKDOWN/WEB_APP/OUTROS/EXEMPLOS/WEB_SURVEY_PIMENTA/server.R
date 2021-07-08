library(shiny)

# Read the survey questions
Qlist <- read.csv("Qlist.csv")
# Qlist <- Qlist[1,]

shinyServer(function(input, output) {
  
  # Create an empty vector to hold survey results
  results <<- rep("", nrow(Qlist))
  # Name each element of the vector based on the
  # second column of the Qlist
  names(results)  <<- Qlist[,2]
  
  # Hit counter
  output$counter <- 
    renderText({
      if (!file.exists("counter.Rdata")) counter <- 0
      if (file.exists("counter.Rdata")) load(file="counter.Rdata")
      counter <- counter <<- counter + 1
      
      save(counter, file="counter.Rdata")     
      paste0("Acessos: ", counter)
    })
  
  # This renderUI function holds the primary actions of the
  # survey area.
  output$MainAction <- renderUI( {
    dynamicUi()
  })
  
  # Dynamic UI is the interface which changes as the survey
  # progresses.  
  dynamicUi <- reactive({
    # Initially it shows a welcome message. 
    if (input$Click.Counter==0) 
      return(
        list(
          h5("Olá, esse questionário tem como intuito, de coletar dados para a Pesquisa Sobre Humanização e Papel de Gatos no Domicílio (PSHPGD)!"),
          h6("Projeto de pesquisa de Dayana Silva, Ewerson Pimenta e José Maurício Karl. da disciplina de Planejamento de Pesquisas (EST065) do segundo semestre de 2018 do curso de Graduação em Estatística para aquisição de nota na disciplina"),
          br(),
          img(src = "https://ih0.redbubble.net/image.512525295.6965/stf,small,600x600-c,0,0,1000,1000.u3.jpg", width = 400),
          br(),
          p("Para iniciar o questionário, clique a seguir.")
        )
      )
    
    # Once the next button has been clicked once we see each question
    # of the survey.
    if (input$Click.Counter>0 & input$Click.Counter<=nrow(Qlist))  
      return(
        list(
          h3(code(textOutput("question"))),
          radioButtons("survey", em("Por favor selecione uma opção a seguir:"), 
                       c(option.list(), "NA"))
        )
      )
    
    # Finally we see results of the survey as well as a
    # download button.
    if (input$Click.Counter>nrow(Qlist))
      return(
        list(
          h4("Resultados agregados"),
          tableOutput("surveyresults"),
          h4("Obrigado por participar da pesquisa!"),
          downloadButton('downloadData', 'Download'),
          br()
          #,h6("Desconsiderar o botão ")
        )
      )    
  })
  
  # This reactive function is concerned primarily with
  # saving the results of the survey for this individual.
  output$save.results <- renderText({
    # After each click, save the results of the radio buttons.
    if ((input$Click.Counter>0)&(input$Click.Counter>!nrow(Qlist)))
      try(results[input$Click.Counter] <<- input$survey)
    # try is used because there is a brief moment in which
    # the if condition is true but input$survey = NULL
    
    # If the user has clicked through all of the survey questions
    # then R saves the results to the survey file.
    if (input$Click.Counter==nrow(Qlist)+1) {
      if (file.exists("survey.results.Rdata")) 
        load(file="survey.results.Rdata")
      if (!file.exists("survey.results.Rdata")) 
        presults<-NULL
      presults <- presults <<- rbind(presults, results)
      rownames(presults) <- rownames(presults) <<- 
        paste("User", 1:nrow(presults))
      save(presults, file="survey.results.Rdata")
    }
    # Because there has to be a UI object to call this
    # function I set up render text that distplays the content
    # of this funciton.
    ""
  })
  
  # This function renders the table of results from the
  # survey.
  output$surveyresults <- renderTable({
    t(summary(presults))
  })
  
  # This renders the data downloader
  output$downloadData <- downloadHandler(
    filename = "IndividualData.csv",
    content = function(file) {
      write.csv(presults, file)
    }
  )
  
  # The option list is a reative list of elements that
  # updates itself when the click counter is advanced.
  option.list <- reactive({
    qlist <- Qlist[input$Click.Counter,3:ncol(Qlist)]
    # Remove items from the qlist if the option is empty.
    # Also, convert the option list to matrix. 
    as.matrix(qlist[qlist!=""])
  })
  
  # This function show the question number (Q:)
  # Followed by the question text.
  output$question <- renderText({
    paste0(
      "Q", input$Click.Counter,":", 
      Qlist[input$Click.Counter,2]
    )
  })
  
})
