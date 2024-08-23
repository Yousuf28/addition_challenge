#!/usr/bin/env Rscript

# This code is for creating
# -----------------------------------------------------------------------------
# Date                     Programmer
#----------   --------------------------------------------------------------
# Aug-22-2024    Md Yousuf Ali (md.ali@fda.hhs.gov)



library(shiny)

ui <- fluidPage(
  titlePanel("Addition Challenge"),
  sidebarLayout(
    sidebarPanel(width = 2,
                 numericInput("num_digits", "Number of Digits:", value = 2, min = 1, max = 3, step = 1),
                 actionButton('generate','new addition')
    ),
    mainPanel(
      h4("Enter the sum of the following two numbers:"),
      h2(textOutput("num1")),
      h2("+"),
      h2(textOutput("num2")),
      textInput("answer", "Your Answer:", placeholder = "Enter sum here"),
      actionButton("submit", "Check Result"),
      br(),
      ## shiny::tableOutput('resutl')
      verbatimTextOutput("result")
    )
  )
)

server <- function(input, output) {

  digits <- reactive({
    val <- input$num_digits
    if(val>3){
val <- 3
    }else if(val==0){
val <- 1
    }else{
val <- input$num_digits
    }

  })

  number1 <- eventReactive(input$generate,{
   num <-  paste0(sample(0:9, digits(), replace = TRUE), collapse = "")
   num
  })

  number2 <- eventReactive(input$generate,{
   num <-  paste0(sample(0:9, digits(), replace = TRUE), collapse = "")
   num
  })

  output$num1 <- renderText({
    number1()
    ## paste0(sample(0:9, input$num_digits, replace = TRUE), collapse = "")
  })

  output$num2 <- renderText({
    number2()
    ## paste0(sample(0:9, input$num_digits, replace = TRUE), collapse = "")
  })

  result_final <- eventReactive(input$submit,{

   ## answer <-  paste0(sample(0:9, input$num_digits, replace = TRUE), collapse = "")
    answer <- as.numeric(number1()) + as.numeric(number2())
    answer

  })

  given_ans <- eventReactive(input$submit,{

    res <- as.numeric(input$answer)
    res

  })

  output$result <- shiny::renderText({
    req(input$submit)
    ## req(input$answer)
    ## input$answer
    ans <- result_final()
    user_ans <- given_ans()
    if(user_ans==ans){

paste0("you are correct. answer is: ", as.character(ans))
    }else{
paste0("incorrect. correct answer is: ", as.character(ans))
    }


  })



  ## observeEvent(input$generate, {
  ##   output$result <- renderText({
  ##     ""
  ##   })
  ## })

  ## output$result <- shiny::renderDataTable({
  ##   result_final <- as.numeric(result_final())
  ##   result_final <- data.frame(k=result_final)
  ##   result_final
  ## })


}

shinyApp(ui = ui, server = server)
