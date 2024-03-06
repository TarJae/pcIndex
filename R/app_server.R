#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {

  # Your application server logic

  #PCI output
  output$txt_pci <- renderText({
    paste(sum(sapply(my_ids, function(x) as.numeric(input[[x]]))))
  })

  # shortened columns names vector:
  my_short_names <- paste0(rep("R", 13), 0:12)


  pci_data <- reactive({
    #browser()
    df <- data.frame(
      Name = my_labels,
      Value = as.character(sapply(my_ids, function(x) input[[x]])),
      stringsAsFactors = FALSE) |>
      pivot_wider(names_from = Name, values_from = Value) |>
      mutate(ID = input$pat_id,
             Date = as.character(input$date_pci), .before=1,
             OP = input$procedure,
             Ascites = input$ascites,
      ) |>
      rename_with(.fn = ~my_short_names, .cols = Central:`Lower ileum`) |>
      mutate(PCI = sum(sapply(my_ids, function(x) as.integer(input[[x]]))
      )
      )

    # Include fields conditionally based on the value of input$procedure
    if (input$procedure %in% c('HIPEC', 'HITOC')) {
      df$CCS <- input$CCS
      df$Na2S2O3 <- input$sodium_thiosulfate

    } else if (input$procedure %in% c('PIPAC', 'PITAC')) {
      df$Flow <- input$flowrate
      df$Pen <- input$pen

    } else if (input$procedure %in% c('CRS', 'Exploration')) {
      df$CCS <- input$CCS
    }

    return(df)
  })


  # output table
  output$pci_table <- renderTable(pci_data())

  # Downloadable csv of selected dataset ----
  output$downloadData <- downloadHandler(
    filename = function() {
      # Extracting pat_id and current date
      pat_id <- input$pat_id
      current_date <- format(Sys.Date(), "%Y-%m-%d")
      # Constructing the filename
      paste("pci_", pat_id, "_", current_date, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(pci_data(), file, row.names = FALSE)
    }
  )


  # print
  observeEvent(input$print, {
    js$winprint()
  })

  #reset pat_id and  radioGroupButtons

  observeEvent(input$update, {
    updateTextInput(session = session, inputId = "pat_id", value = "")
    updateRadioGroupButtons(session = session, inputId = my_ids[1], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[2], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[3], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[4], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[5], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[6], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[7], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[8], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[9], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[10], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[11], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[12], selected = 0)
    updateRadioGroupButtons(session = session, inputId = my_ids[13], selected = 0)
  }, ignoreInit = TRUE)

}
