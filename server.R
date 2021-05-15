library(shiny)
library(DT)
library(data.table)
library(generator)

# n = 10000
# d <- data.table(name = toupper(r_full_names(n))
#              , alias_name_1 = toupper(r_full_names(n))
#              , alias_name_2 = toupper(r_full_names(n))
#              , alias_name_3 = toupper(r_full_names(n))
#              , phone_1 = r_phone_numbers(n)
#              , phone_2 = r_phone_numbers(n)
#              , phone_3 = r_phone_numbers(n)
#              , dob = r_date_of_births(n, start = as.Date("1900-01-01"), end = Sys.Date()))

# d <- iris
# d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
# setDT(d)


app = shinyApp(
 ui =
 fluidPage(
   sidebarLayout(
     sidebarPanel(width = 1, # HTML("This button will open Panel 1 using <code>updateCollapse</code>."),
                  actionButton("p1Button", "Push Me!") #,
                #   selectInput("styleSelect", "Select style for Panel 1",
                #    c("default", "primary", "danger", "warning", "info", "success"))
     ),
     mainPanel(
       bsCollapse(id = "collapseExample", multiple = T, #open = "Panel 2",
                  bsCollapsePanel("Panel 1", "This is a panel with just text ",
                    "and has the default style. You can change the style in ",
                    "the sidebar.", style = "info"
                    # ---- Subset Vars ----
                    , "Subset vars:"
                    , selectInput("sub_vars", NULL, names(d), selected = names(d), multiple = T, selectize = T)
                    , "Search"
                    
                    # ---- First search
                    , fluidRow(
                        column(1,
                        selectInput("var_1", NULL, names(d), selected = names(d)[1], multiple = T, width = "200px")
                        ),
                        column(10, # offset = 4,
                        textInput("val_1", NULL, value = "", width = NULL, placeholder = NULL)
                        )      
                    )

                    # ---- Second search
                    , fluidRow(
                        column(1,
                        selectInput("var_2", NULL, names(d), selected = names(d)[2], multiple = T, width = "200px")
                        ),
                        column(2, # offset = 4,
                        textInput("val_2", NULL, value = "", width = NULL, placeholder = NULL)
                        )      
                    )

                    # ---- Third search
                    , fluidRow(
                        column(1,
                        selectInput("var_3", NULL, names(d), selected = names(d)[3], multiple = T, width = "200px")
                        ),
                        column(2, # offset = 4,
                        textInput("val_3", NULL, value = "", width = NULL, placeholder = NULL)
                        )      
                    )

                    , textInput("threshold", "Threshold:", value = "1", width = NULL, placeholder = NULL)
                    , actionButton("action", label = "Action")

                    # End of panel 1 
                  ),
                  bsCollapsePanel("Panel 2", "This panel has a generic plot. ", "and a 'success' style.", 
                   # Panel two shit                  
                  DT::dataTableOutput("mytable")
       )
     )
   )
 )),
 server =
 function(input, output, session) {
   output$genericPlot <- renderPlot(plot(rnorm(100)))
   observeEvent(input$p1Button, ({
     updateCollapse(session, "collapseExample", open = "Panel 1")
   }))
   observeEvent(input$styleSelect, ({
     updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
   }))
   
   observeEvent(input$styleSelect, ({
     updateCollapse(session, "collapseExample", style = list("Panel 1" = input$styleSelect))
   }))

    
    observeEvent(input$action, {
        # browser()
        l <- rep(F, nrow(d))
        d[, l := F]

        if(input$val_1 != ""){
            cnt_1 <- c()
            for(i in 1:length(input$var_1)){
                browser()
                d[, l := l + (x <<- (eval(as.name(input$var_1)) %ilike% input$val_1))]
                cnt_1 <- c(cnt_1, sum(x))
            }
            output$results_1 <- renderText({as.character(paste(cnt_1, collapse = ", "))})
        }

        if(input$val_2 != ""){
            for(i in 1:length(input$var_2)){
                d[, l := l + (eval(as.name(input$var_2)) %ilike% input$val_2)]
            }
        }

        if(input$val_3 != ""){
            for(i in 1:length(input$var_3)){
                d[, l := l + (eval(as.name(input$var_3)) %ilike% input$val_3)]
            }
        }


        vars <- input$sub_vars
        # browser()
        if(is.null(length(vars))){
            vars <- names(d)[1:2]
        }

        output$mytable = DT::renderDataTable({
            d[l >= as.numeric(input$threshold), ..vars]
        })

    })


 }
)


## Not run: 
runApp(app)
