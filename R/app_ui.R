#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @import shinyWidgets
#' @import dplyr
#' @import tidyr
#' @import shinyjs
#' @importFrom shinyjs alert
#' @importFrom shinyjs runExample
#' @noRd

# js to print screen
jsCode <- 'shinyjs.winprint = function(){
window.print();
}'

# my functions later own file
linebreaks <- function(n){HTML(strrep(br(), n))}

my_ids <- c("central", "right_upper", "epigastrium", "left_upper", "left_flank", "left_lower", "pelvis",
            "right_lower", "right_flank", "upper_jejunum", "lower_jejunum", "upper_ileum", "lower_ileum")

my_labels <- c("Central", "Right upper", "Epi gastrium",
               "Left upper", "Left flank", "Left lower",
               "Pelvis", "Right lower",	"Right flank",
               "Upper jejunum",	"Lower jejunum", "Upper ileum", "Lower ileum")

app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    ui <- fixedPage(
      #  css for button 0 to 3 #a2ff45 -> green
      tags$style(
        ".btn-zero {background-color: white; color: black;}",
        ".btn-zero.active {background-color: #a2ff45; color: black;}",
        ".btn-one {background-color: white; color: black;}",
        ".btn-one.active {background-color: #a2ff45; color: black;}",
        ".btn-two {background-color: white; color: black;}",
        ".btn-two.active {background-color: #a2ff45; color: black;}",
        ".btn-three {background-color: white; color: black;}",
        ".btn-three.active {background-color: #a2ff45; color: black;}"
      ),


      # positioning the widgets and avoiding overriding
      tags$head(
        tags$style(HTML("
      #container1 > .form-group {
        height: 25px;
        margin-bottom: 5px;
        font-size:25px
      }"))
      ),

      useShinyjs(),
      tags$style(HTML("
  @media print {
    @page {
      size: landscape; /* Set the page orientation to landscape */
      /* Removed the color: auto; as it's not necessary */
      margin: 1cm; /* Adds a margin around the page, adjust as needed */
    }
    body * {
      visibility: hidden; /* Hide everything in the body by default */
    }
    #printArea, #printArea * {
      visibility: visible; /* Make content within #printArea visible */
    }
    #printArea {
      position: absolute;
      left: 0;
      top: 0;
      width: 100%;
      /* Adjust the size of the content to fit the first page */
      /* This might require adjusting the scale or the content's layout */
    }
  }
")),



extendShinyjs(text = jsCode, functions = c("winprint")),

# This div contains the content you want to print
div(id = "printArea",

    fixedRow(
      column(2,
             wellPanel(
               h3(style="text-decoration:underline; font-weight:bold;", "Data"),
               br(),
               dateInput("date_pci", "Date:", value = Sys.Date(), format = "dd.mm.yyyy", width = 200),
               br(),
               textInput("pat_id", label="Patient AZL", width=200),
               selectInput("procedure","OP", choices = c("HIPEC", "PIPAC", "HITOC", "PITAC", "CRS", "Exploration"), width = 200),
               numericInput("ascites", label="Ascites [ml]", value=0, width = 200),
               conditionalPanel(
                 condition = "input.procedure ==  'HIPEC' || input.procedure == 'HITOC'",
                 selectInput("CCS","Completeness of Cytoreduction", choices = c("n/a", "CC-0", "CC-1", "CC-2", "CC-3"), width = 200),
                 selectInput("sodium_thiosulfate","Sodium thiosulfate", choices = c("n/a", "yes", "no"), width = 200)
               ),
               conditionalPanel(
                 condition = "input.procedure ==  'PIPAC' || input.procedure == 'PITAC'",
                 numericInput("flowrate", label="Flow rate", value=0, width = 200),
                 selectInput("pen","Used Pen", choices = c("n/a", "Capnopharm", "Topol", "Capnomed", "Other"), width = 200)
               ),
               conditionalPanel(
                 condition = "input.procedure == 'CRS' || input.procedure == 'Exploration'",
                 selectInput("CCS","Completeness of Cytoreduction", choices = c("n/a", "CC-0", "CC-1", "CC-2", "CC-3"), width = 200)
               )
             )
      ),
      column(2,
             h3(style="text-decoration:underline; font-weight:bold;", ""),
             br(),
             br(),
             br(),
             img(src='www/pci_score_transparent_torso.png', height="90%", width="90%", align="center"),
             br(),
             br(),
             column(6,
                    h1(style="font-weight:bold;", "PCI")
             ),
             column(6,
                    div(
                      textOutput('txt_pci'),
                      style = "border: 3px solid purple; padding: 0px 5px; text-align: center; margin: 0 auto; width: fit-content;"
                    ),
                    tags$head(tags$style("#txt_pci{color: red; font-size: 50px; font-weight: bold;}"))
             )
      ),
      column(2,
             h3(style="text-decoration:underline; font-weight:bold;", "Regions"),
             br(),
             br(),
             h4("0   Central"),
             h4("1   Right upper"),
             h4("2   Epigastrium"),
             h4("3   Left upper"),
             h4("4   Left flank"),
             h4("5   Left lower"),
             h4("6   Pelvis"),
             h4("7   Right lower"),
             h4("8   Right flank"),
             h4("9   Upper jejunum"),
             h4("10  Lower jejunum"),
             h4("11  Upper ileum"),
             h4("12  Lower ileum")

      ),
      column(2,
             h3(style="text-decoration:underline; font-weight:bold;", "Lesion size"),
             div(id = "container1",
                 style="display: inline-block;vertical-align:middle; width: 300px;",
                 shinyWidgets::radioGroupButtons(inputId = my_ids[1], label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[2], label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[3],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[4],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[5],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[6],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[7],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[8],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[9],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[10],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[11],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[12],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs'),
                 shinyWidgets::radioGroupButtons(inputId = my_ids[13],  label = "", choices = 0:3, selected = 0, checkIcon = list(yes = icon("check")), status = c("zero", "one", "two", "three"), size = 'xs')
             ),
             br(),
             br(),
             br()
      ),
      column(3,
             tags$h3(style="text-decoration:underline; font-weight:bold;", "Lesions size score"),
             br(),
             h4("LS 0:   No tumor seen"),
             h4("LS 1:   Tumor up to 0.5 cm"),
             h4("LS 2:   Tumor up to 5.0 cm"),
             h4("LS 3:   Tumor > 5.0 cm",
                br(),
                "or confluence"),
             img(src='www/pci_score_transparent_small_intestine.png', height="90%", width="90%", align="center")
      )
    )
), # End of This div contains the content you want to print


fixedRow(
  column(12,
         wellPanel(
           tableOutput("pci_table"),
           downloadButton("downloadData", "Download CSV"),
           actionButton("print", "PRINT"),
           tags$head(
             tags$style(HTML('#update{background-color:orange}'))
           ),
           actionButton(inputId = "update", label = "Reset")
         )
  )
),

# buy me a coffee
div(style = 'position: absolute;left: 1760px; top:730px;',
    HTML('<a href="https://www.buymeacoffee.com/tarkanjaeger" target="_blank">
               <img src="https://cdn.buymeacoffee.com/buttons/bmc-new-btn-logo.svg"
               alt="Buy me a coffee"></a>')
),

# Info buy me a coffee

div(style = 'position: absolute;left: 1580px; top:760px;',
    HTML("<p>Support my work with a tea!</p>")
)

    )
  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "pci"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
