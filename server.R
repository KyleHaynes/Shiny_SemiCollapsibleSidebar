library(shiny)
library(DT)
library(data.table)

d <- iris
d <- data.frame(lapply(d, as.character), stringsAsFactors=FALSE)
setDT(d)

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
                    , "Subset vars:"
                    , selectInput("sub_vars", NULL, names(d), selected = names(d)[1], multiple = T)
                    , "Search"
                    , selectInput("var_1", NULL, names(d), selected = names(d)[1])
                    , textInput("val_1", NULL, value = "", width = NULL, placeholder = NULL)
                    , selectInput("var_2", NULL, names(d), selected = names(d)[2])
                    , textInput("val_2", NULL, value = "", width = NULL, placeholder = NULL)
                    , selectInput("var_3", NULL, names(d), selected = names(d)[3])
                    , textInput("val_3", NULL, value = "", width = NULL, placeholder = NULL)
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
            d[, l := l + (eval(as.name(input$var_1)) %ilike% input$val_1)]
        }

        if(input$val_2 != ""){
            d[, l := l + (eval(as.name(input$var_2)) %ilike% input$val_2)]
        }
        
        if(input$val_3 != ""){
            d[, l := l + (eval(as.name(input$var_3)) %ilike% input$val_3)]
        }
                


        vars <- input$sub_vars

        output$mytable = DT::renderDataTable({
            d[l >= 1, ..vars]
        })

    })


 }
)


## Not run: 
runApp(app)
