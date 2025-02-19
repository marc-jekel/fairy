if (!require("dplyr")) install.packages("dplyr")
library("dplyr")
if (!require("DT")) install.packages("DT")
library("DT")
if (!require("MASS")) install.packages("MASS")
library("MASS")
if (!require("plotly")) install.packages("plotly")
library("plotly")
if (!require("rcdd")) install.packages("rcdd")
library("rcdd")
if (!require("shiny")) install.packages("shiny")
library("shiny")
if (!require("shinyalert")) install.packages("shinyalert")
library("shinyalert")
if (!require("shinyBS")) install.packages("shinyBS")
library("shinyBS")
if (!require("shinycssloaders")) install.packages("shinycssloaders")
library("shinycssloaders")
if (!require("shinyjs")) install.packages("shinyjs")
library("shinyjs")
if (!require("shinyvalidate")) install.packages("shinyvalidate")
library("shinyvalidate")
if (!require("stringr")) install.packages("stringr")
library("stringr")
if (!require("tidyr")) install.packages("tidyr")
library("tidyr")
if (!require("volesti")) install.packages("volesti")
library("volesti")
if (!require("shinyWidgets")) install.packages("shinyWidgets")
library("shinyWidgets")
if (!require("rhandsontable")) install.packages("rhandsontable")
library("rhandsontable")
if (!require("writexl")) install.packages("writexl")
library("writexl")
if (!require("readxl")) install.packages("readxl")
library("readxl")
if (!require("shinybusy")) install.packages("shinybusy")
library("shinybusy")
if (!require("forstringr")) install.packages("forstringr")
library("forstringr") 

options(warn = -1)

ui <- shinyUI(fluidPage(
  tags$head(tags$style(
    "
    .grey-out {
        background-color: #eee;
        opacity: 0.8;
    }
    "
  )),
  tags$style(
    type = "text/css",
    ".navbar { background-color: #000000;
                                               font-family: Arial;
                                               font-size: 13px;
                                               color: #FF0000; }",
    ".navbar-dropdown { background-color: #262626;
                                                    font-family: Arial;
                                                    font-size: 13px;
                                                    color: #FF0000; }",
    "body {padding-top: 60px;padding-bottom: 95px;}"
  ),
  navbarPage(
    add_busy_gif(src = "https://www.marc-jekel.de/fairy.gif", height = 40, width = 40),
    position = "fixed-top",
    id = "tabs",
    fluid = T,
    inverse = T,
    windowTitle = "Modeling Fairy App",
    tabPanel(
      "Input",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        tags$head(
          tags$style(type = "text/css", "select { max-width: 200px; }"),
          tags$style(type = "text/css", ".span4 { max-width: 200px; }"),
          tags$style(type = "text/css", ".well { max-width: 200px; }"),
          tags$style(type = "text/css", "select { min-width: 200px; }"),
          tags$style(type = "text/css", ".span4 { min-width: 200px; }"),
          tags$style(type = "text/css", ".well { min-width: 200px; }")
        ),
        width = 2,
        fluidRow(column(
          12,
          offset = 0,
          h5("Compute minimal description(s)")
        )),
        fluidRow(column(12, offset = 0, actionBttn("go_v_h", "Go", style = "jelly", color = "primary", size = "md"))),
        fluidRow(
          column(12,
                 hr(),
                 offset = 0,
                 h5("Remove last probability or add probability")
          ),
          column(
            12,
            offset = 0,
            actionBttn("rm_btn", "",
                       icon = icon("fa-regular fa-square-minus"), size = "sm",
                       style = "jelly", color = "default"
            ),
            actionBttn("add_btn", "",
                       icon = icon("fa-regular fa-square-plus"), size = "sm",
                       style = "jelly", color = "primary"
            )
          ),
        ),
        fluidRow(
          column(12,
                 offset = 0,
                 h5("Remove last model or add model")
          ),
          column(
            12,
            offset = 0,
            actionBttn("rm_ie", "",
                       icon = icon("fa-regular fa-square-minus"), size = "sm",
                       style = "jelly", color = "default"
            ),
            actionBttn("add_ie", "",
                       icon = icon("fa-regular fa-square-plus"), size = "sm",
                       style = "jelly", color = "primary"
            )
          )
        ),
        hr(),
        h5("Settings"),
        fluidRow(column(
          12,
          offset = 0,
          actionBttn("show_approx_erros", "Approximate equalities",
                     style = "unite", color = "default", size = "xs"
          ),
          condition = "model_sel_multiple_reactive$value=='None'",
          actionBttn("show_v_rep", "V-representation(s)",
                     style = "unite", color = "default", size = "xs"
          )
        )),
        fluidRow(column(12,
                        offset = 0,
                        hr(),
                        h5("Download or upload app input")
        )),
        fluidRow(column(
          12,
          offset = 0,
          downloadButton("download", "")
        )),
        fluidRow(column(
          12,
          offset = 0,
          fileInput("upload", "",
                    multiple = FALSE,
                    accept = c(
                      ".xlsx"
                    )
          ),
          div(style = "margin-top: -30px"),
        )),
      ),
      mainPanel(
        add_busy_gif(src = "https://www.marc-jekel.de/fairy.gif", height = 40, width = 40),
        shinyBS::bsTooltip("textbox_ui_rel",
                           'Use +, -, *, fractional and decimal numbers, p1, p2, p3, <, >, and =. Separate constraints with ";" such as "p1 < p2; p2 < p3".<br><br> "{p1,p2} < {p3,3*p4}" is a shortcut for "p1 < p3; p1 < 3 * p4; p2 < p3; p2 < 3*p4".<br><br>Limitations: (1) Put constants on one side of the in/equalities and p*s on the other side, e.g., instead of p1 > p2 + .05, write p1 - p2 > .05. (2) Do not use parantheses, e.g., instead of "(p1+p2)/2 < p3", write "1/2 * p1 + 1/2 * p2 < p3".',
                           placement = "bottom", trigger = "hover"
        ),
        shinyBS::bsTooltip(
          "textbox_approx",
          ".05 means... <br><br> ... p1 = .5 will be set to p1 < .55 and p1 > .45, <br> ...  p1 = 1 will be set to p1 < 1 and p1 > .95,  <br> ... p1 = p2 will be set to p1 - p2 < .05 and  - p1 + p2 < .05.",
        ),
        fluidRow(
          useShinyjs(),
          splitLayout(
            h3(),
            h3(),
            h3(),
            textAreaInput(
              inputId = "add_for_all_models",
              label = "Shared Model Specification",
              value = "",
              width = "100%",
              height = "80px",
              placeholder = "p1 < .5; p2 < .5; ...",
              resize = "none",
            ),
            cellWidths = c(150, 150, 300, 300),
            cellArgs = list(style = "padding: 0px")
          ),
          align = "center",
          splitLayout(
            uiOutput("textbox_ui_name", style = "background-color:#5188FB;border:dotted;border-width:1px"),
            uiOutput("textbox_ui_name_rel"),
            uiOutput("textbox_ui_rel"),
            uiOutput("textbox_ui_rel_complete"),
            cellWidths = c(150, 150, 300, 300),
            cellArgs = list(style = "padding: 2px")
          )
        )
      )
    ),
    tabPanel(
      "H-representation",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 2,
        fluidRow(
          column(12,
                 offset = 0,
                 h4("Downloads"),
                 h5("H-representation for QTEST")
          ),
          column(12,
                 offset = 0,
                 downloadButton("d_h", "")
          ),
          column(12,
                 offset = 0,
                 h5("LaTeX file of H-representation")
          ),
          column(12,
                 offset = 0,
                 downloadButton("d_latex", "")
          ),
        )
      ),
      mainPanel(
        id = "outP", fluidPage(
          withMathJax(),
          uiOutput("h"),
          uiOutput("h_repl")
        ),
        tags$script(
          '
            $("#go_v_h").click(function(){
                            $("#outP").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#outP").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#outP").addClass("grey-out");
                        });
                        '
        )
      )
    ),
    tabPanel(
      "V-representation",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 2,
        fluidRow(
          column(
            12,
            offset = 0,
            h5("Download V-representation for QTEST")
          ),
          column(12,
                 offset = 0,
                 downloadButton("d_v", "")
          )
        ),
      ),
      mainPanel(
        fluidRow(column(
          id = "v_rep",
          12, uiOutput("v_representation_table")
        )),
        tags$script(
          '
         $("#go_v_h").click(function(){
                            $("#v_rep").removeClass("grey-out");
                        });
             $("#approx_equal").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        $("#rm_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#v_rep").addClass("grey-out");
                        });
                        '
        )
      )
    ),
    # navbarPage
    tabPanel(
      "Parsimony",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 2,
        h5(("Compute (hyper-) volume")),
        fluidRow(
          column(12,
                 offset = 0,
                 actionBttn("go", "Go", style = "jelly", color = "primary", size = "md")
          )
        ),
        bsTooltip("go", "Choose at least one algorithm to enable this option.",
                  "right",
                  options = list(container = "body")
        ),
        hr(),
        h5(("Include algorithm(s)")),
        fluidRow(
          column(
            12,
            offset = 0,
            materialSwitch("CB", "Cooling Bodies", value = F, status = "primary", right = T),
            materialSwitch("SoB", "Sequ. of Balls", value = F, status = "primary", right = T),
            materialSwitch("CG", "Cooling Gaussian", value = F, status = "primary", right = T)
          ),
          column(12, offset = 0, actionBttn("show", "Settings",
                                            size = "xs",
                                            style = "jelly", color = "default"
          )),
          hr(),
          column(12,
                 offset = 0,
                 h5("Download results as csv-table")
          ),
          column(12,
                 offset = 0,
                 downloadButton("download_volume", "")
          ),
        )
      ),
      mainPanel(
        fluidRow(
          column(
            id = "parsim_gr_out",
            12,
            div(uiOutput("parsimony_spinner"), align = "center")
          )
        ),
        fluidRow(column(
          id = "parsim_out",
          12, div(uiOutput("parsimony_spinner_table"), align = "center")
        )),
        tags$script(
          '
            $("#go_v_h").click(function(){
                            $("#parsim_table").removeClass("grey-out");
                        });
              $("#approx_equal").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#parsim_table").addClass("grey-out");
                        });


                    $("#go_v_h").click(function(){
                            $("#parsim_gr_out").removeClass("grey-out");
                        });
        $("#add_ie").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
           $("#rm_btn").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
        $("#add_btn").click(function(){
                            $("#parsim_gr_out").addClass("grey-out");
                        });
                        '
        )
      )
    ), tabPanel(
      "Plot Edge Cases",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 2,
        fluidRow(
          column(12,
                 offset = 0,
                 h5("Choose a model")
          ),
          column(12,
                 offset = 0,
                 pickerInput("go_example", "", choices = NA)
          )
        ),
      ),
      mainPanel(fluidRow(
        column(
          id = "example_out",
          12, div(plotlyOutput("plot_example"), align = "center")
        )
      ))
    ),
    tabPanel(
      "Plot Polytope(s)",
      sidebarPanel(
        style = "position:fixed;width:inherit;",
        width = 2,
        fluidRow(
          column(12,
                 offset = 0,
                 h5("Remove or add all models")
          ),
          column(
            12,
            offset = 0,
            actionBttn("subtr_models", "",
                       icon = icon("fa-regular fa-square-minus"), size = "sm",
                       style = "jelly", color = "default"
            ),
            actionBttn("add_models", "",
                       icon = icon("fa-regular fa-square-plus"), size = "sm",
                       style = "jelly", color = "primary"
            )
          )
        ),
        fluidRow(column(
          12,
          offset = 0,
          hr(),
          textAreaInput(
            inputId = "name_model_plot",
            label = "Input field for model names",
            value = "",
            width = "100%",
            height = "120px",
            resize = "none"
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          hr(),
          h5("Select probabilities")
        )),
        fluidRow(column(
          12,
          offset = 0,
          pickerInput("dim_1", "p #",
                      list(1, 2, 3),
                      selected = 1
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          pickerInput("dim_2", "p #",
                      list(1, 2, 3),
                      selected = 2
          )
        )),
        fluidRow(column(
          12,
          offset = 0,
          pickerInput("dim_3", "p #",
                      list(1, 2, 3),
                      selected = 3
          )
        ))
      ),
      mainPanel(fluidRow(column(
        12, div(
          
          plotlyOutput("plot"),
          align = "center"
        )
      )))
    )
  ),
  navbarPage(
    title = "",
    position = "fixed-bottom",
    fluid = T,
    inverse = F,
    id = 'banner'
    )
))

server <- shinyServer(function(input, output, session) {
  
  shinyjs::html(id = "banner", 
                html = "<SPAN STYLE='color:#FFFFFF'><p><center> &#129668; The Shiny app is under active development (Beta release 02/18/25), please click <a href='mailto:mjekel@uni-koeln.de?subject=bug-report 🧚' style='color: red;'>here</a> to report bugs. 
                &#128009; We encourage you to download the Shiny app <a target='_blank' href='https://github.com/marc-jekel/fairy/blob/main/app.R' style='color: red;'>here</a> to run it on your machine for more computationally demanding tasks. &#127984; </center></p></SPAN>", 
                add = TRUE)

  
  hide("hidden_b")
  hide("open_b")
  
  
  hideTab(inputId = "tabs", target = "H-representation")
  hideTab(inputId = "tabs", target = "V-representation")
  hideTab(inputId = "tabs", target = "Parsimony")
  hideTab(inputId = "tabs", target = "Plot Polytope(s)")
  hideTab(inputId = "tabs", target = "Plot Edge Cases")
  
  ##### Global Variables ####
  
  h_reactive <- reactiveValues()
  v_reactive <- reactiveValues()
  
  h_pars_reactive <- reactiveValues()
  mytable_v_reactive <- reactiveValues(value = NULL)
  mytable_approx_reactive <- reactiveValues(value = NULL)
  
  model_sel_multiple_reactive <- reactiveValues(value = "None")
  
  equation_all_total_reactive <- reactiveValues(value = NULL)
  parsim_wide_table_reactive <- reactiveValues(value = NULL)
  
  input_multi_knob_reactive <- reactiveValues(value = 0)
  
  ineq_eq_left_reactive <- reactiveValues(value = NA)
  ineq_eq_right_reactive <- reactiveValues(value = NA)
  numb_p_reactive <- reactiveValues(value = NA)
  probs_reactive <- reactiveValues(value = NA)
  upload_reactive <- reactiveValues(value = 0)
  
  names_available_models_reactive <- reactiveValues(value = NA)
  names_models_reactive <- reactiveValues(value = NA)
  
  all_operators_reactive <- reactiveValues(value = NA)
  
  input_volume_reactive <- reactiveValues(value = c(
    rep("default", 5), "none",
    rep("default", 3), "none",
    rep("default", 4), "none"
  ))
  
  counter <- reactiveValues(n = 3)
  
  counter_input <- reactiveValues(n = 1)
  
  
  AllInputs <- reactive({
    x <- reactiveValuesToList(input)
  })
  
  
  
  #### Button Events ####
  
  observeEvent(input$remove_inter, {
    counter_inter$n <- 1
  })
  
  observeEvent(input$more_inter, {
    counter_inter$n <- isolate(counter_inter$n + 1)
  })
  
  observeEvent(input$less_inter, {
    if (counter_inter$n > 1) {
      counter_inter$n <- counter_inter$n - 1
    }
  })
  
  observeEvent(input$add_btn, {
    counter$n <- counter$n + 1
  })
  
  observeEvent(input$add_ie, {
    counter_input$n <- counter_input$n + 1
  })
  
  observeEvent(input$rm_btn, {
    if (counter$n > 3) {
      counter$n <- counter$n - 1
    }
  })
  
  observeEvent(input$rm_ie, {
    if (counter_input$n > 1) {
      counter_input$n <- counter_input$n - 1
    }
  })
  
  observeEvent(input$add_models, {
    names_available_models <- isolate(names_available_models_reactive$value)
    
    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- str_replace_all(models_to_plot, " ", "")
    models_to_plot <- unlist(models_to_plot)
    
    models_to_plot <- unique(c(models_to_plot, names_available_models))
    models_to_plot <- models_to_plot[models_to_plot != ""]
    
    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })
  
  observeEvent(input$subtr_models, {
    models_to_plot <- input$name_model_plot
    models_to_plot <- unlist(str_split(models_to_plot, ";"))
    models_to_plot <- str_replace_all(models_to_plot, " ", "")
    models_to_plot <- unlist(models_to_plot)
    
    models_to_plot <- ""
    
    updateTextAreaInput(
      inputId = "name_model_plot",
      value = paste(models_to_plot, collapse = ";")
    )
  })
  
  observeEvent(input$submit_inter, {
    removeModal()
  })
  
  observeEvent(input$submit_v_repr, {
    removeModal()
  })
  
  
  ###
  
  observeEvent(input$CB | input$SoB | input$CG, {
    if (input$CB == 0 & input$SoB == 0 & input$CG == 0) {
      disable("go")
    } else {
      enable("go")
    }
  })
  
  
  
  #### Checkbox Events ####
  
  textboxes_min <- reactive({
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_min_", i),
            label = paste0("Minimum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_min_", i)]]) == TRUE,
                           0, AllInputs()[[paste0("textin_min_", i)]]
            )
          )
        })
      })
    }
  })
  
  textboxes_max <- reactive({
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          numericInput(
            min = 0,
            max = 1,
            step = .01,
            inputId = paste0("textin_max_", i),
            label = paste0("Maximum of p", i),
            value = ifelse(is.null(AllInputs()[[paste0("textin_max_", i)]]) == TRUE,
                           1, AllInputs()[[paste0("textin_max_", i)]]
            )
          )
        })
      })
    }
  })
  
  textboxes_names <- reactive({
    n <- counter$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          (textAreaInput(
            inputId = paste0("textin_name_", i),
            label = paste0("Name of p", i),
            value = ifelse(
              is.null(AllInputs()[[paste0("textin_name_", i)]]) == TRUE,
              paste0("p_{", i, "}"),
              AllInputs()[[paste0("textin_name_", i)]]
            ),
            width = "80px",
            height = "50px",
            resize = "none"
          ))
        })
      })
    }
  })
  
  textboxes_relations <- reactive({
    n <- counter_input$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          textAreaInput(
            inputId = paste0("textin_relations_", i),
            label = paste0("Unique Model Specification"),
            value = AllInputs()[[paste0("textin_relations_", i)]],
            width = "100%",
            height = "95px",
            placeholder = "p1 > p2; ...",
            resize = "none"
          )
        })
      })
    }
  })
  
  ####
  
  observeEvent(c(counter_input$n, input$add_for_all_models), {
    for (loop_n in 1:counter_input$n) {
      full_descri <- paste("observeEvent(input$textin_relations_", loop_n, ", {for(loop_conjung in 1 : counter_input$n){updateTextAreaInput(inputId = paste0('textin_relations_complete_', loop_conjung),value =  paste(AllInputs()[[paste0('textin_relations_', loop_conjung)]],input$add_for_all_models,sep='',collapse='`'))}})", sep = "")
      eval(parse(text = full_descri))
    }
  })
  
  ####
  
  textboxes_relations_complete <- reactive({
    n <- counter_input$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          (textAreaInput(
            inputId = paste0("textin_relations_complete_", i),
            label = paste0("Model Specification"),
            value = AllInputs()[[paste0("textin_relations_", i)]],
            width = "100%",
            height = "95px",
            resize = "none"
          ))
        })
      })
    }
  })
  
  
  textboxes_check <- reactive({
    n <- counter_input$n
    if (n > 0) {
      ({
        lapply(seq_len(n), function(i) {
          materialSwitch(paste0("textin_include_v_", i),
                         label = HTML(paste("Include V-repres. for ",
                                            (AllInputs()[[paste("textin_relations_name", i, sep = "")]]),
                                            sep = ""
                         )),
                         status = "primary",
                         value = AllInputs()[[paste0("textin_include_v_", i)]],
                         right = T
          )
        })
      })
    }
  })
  
  textboxes_inter_check <- reactive({
    if ((input$show_inter) != 0) {
      inters_models <- hot_to_r(input$mytable_inter)
      
      n <- nrow(inters_models)
      
      if (n > 0) {
        ({
          lapply(seq_len(n), function(i) {
            materialSwitch(paste0("textin_include_inter_v_", i),
                           label = HTML(paste("Include V-repres. for ",
                                              inters_models[i, 1],
                                              sep = ""
                           )),
                           status = "primary",
                           value = AllInputs()[[paste0("textin_include_inter_v_", i)]],
                           right = T
            )
          })
        })
      }
    }
  })
  
  
  textboxes_relations_name <- reactive({
    n <- counter_input$n
    
    if (n > 0) {
      isolate({
        lapply(seq_len(n), function(i) {
          (textAreaInput(
            inputId = paste0("textin_relations_name", i),
            label = paste0("Model Name"),
            value = ifelse(
              is.null(AllInputs()[[paste0("textin_relations_name", i)]]) == TRUE,
              paste0("m", i),
              AllInputs()[[paste0("textin_relations_name", i)]]
            ),
            width = "100%",
            height = "95px",
            resize = "none"
          ))
        })
      })
    }
  })
  
  output$textbox_ui_min <- renderUI({
    textboxes_min()
  })
  output$textbox_ui_name <- renderUI({
    textboxes_names()
  })
  output$textbox_ui_max <- renderUI({
    textboxes_max()
  })
  output$textbox_ui_rel <- renderUI({
    textboxes_relations()
  })
  output$textbox_ui_rel_complete <- renderUI({
    textboxes_relations_complete()
  })
  output$textbox_ui_check <- renderUI({
    textboxes_check()
  })
  
  output$textbox_ui_inter_check <- renderUI({
    textboxes_inter_check()
  })
  
  
  output$textbox_ui_repl <- renderUI({
    textboxes_repl()
  })
  
  #### Display Intersection
  
  output$checkbox_ui_row <- renderUI({
    check_box_row()
  })
  output$textbox_ui_name_rel <-
    renderUI({
      textboxes_relations_name()
    })
  
  
  #### Download ####
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("user_input_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      missing_table <- data.frame("none")
      colnames(missing_table) <- ""
      
      mytable_v <- hot_to_r(input$mytable_v)
      if (is.null(mytable_v) == T) {
        mytable_v <- missing_table
      }
      
      name_probs <- numeric()
      
      for (loop_save in 1:counter$n) {
        name_probs <- c(name_probs, eval(parse(text = paste("AllInputs()$textin_name_", loop_save, sep = ""))))
      }
      
      name_models <- numeric()
      
      for (loop_save in 1:counter_input$n) {
        name_models <- c(name_models, eval(parse(text = paste("AllInputs()$textin_relations_name", loop_save, sep = ""))))
      }
      
      input_models <- numeric()
      
      for (loop_save in 1:counter_input$n) {
        input_models <- c(input_models, eval(parse(text = paste("AllInputs()$textin_relations_", loop_save, sep = ""))))
      }
      
      shared_input_model <- AllInputs()$add_for_all_models
      
      mytable_approx <- hot_to_r(input$mytable_approx)
      
      out <- list(
        "V-description" = mytable_v,
        "Number of models" = data.frame(counter_input$n),
        "Number of probabilitiy" = data.frame(counter$n),
        "Names of probabilities" = data.frame(name_probs),
        "Name of models" = data.frame(name_models),
        "Unique input models" = data.frame(input_models),
        "Shared input models" = data.frame(shared_input_model),
        "Approximately identical equalities" = data.frame(mytable_approx)
      )
      
      write_xlsx(out, file)
    }
  )
  
  #### Upload ####
  
  observeEvent(input$upload, {
    mytable_v_reactive$value <- read_excel(input$upload$datapath, 1)
    counter_input$n <- unlist(read_excel(input$upload$datapath, 2))
    
    
    if (length(mytable_v_reactive$value) == 1) {
      mytable_v_reactive$value <- NULL
    }
    
    counter$n <- unlist(read_excel(input$upload$datapath, 3))
    
    name_probs <- unlist(read_excel(input$upload$datapath, 4))
    names(name_probs) <- NULL
    
    for (loop_load in 1:length(name_probs)) {
      updateTextAreaInput(session,
                          inputId = paste0("textin_name_", loop_load),
                          value = (name_probs[loop_load])
      )
    }
    
    name_models <- unlist(read_excel(input$upload$datapath, 5))
    names(name_models) <- NULL
    
    for (loop_load in 1:length(name_models)) {
      updateTextAreaInput(session,
                          inputId = paste0("textin_relations_name", loop_load),
                          value = (name_models[loop_load])
      )
    }
    
    input_models <- unlist(read_excel(input$upload$datapath, 6))
    names(input_models) <- NULL
    
    for (loop_load in 1:length(input_models)) {
      updateTextAreaInput(session,
                          inputId = paste0("textin_relations_", loop_load),
                          value = (input_models[loop_load])
      )
    }
    
    shared_input_model <- unlist(read_excel(input$upload$datapath, 7))
    names(shared_input_model) <- NULL
    
    updateTextAreaInput(session,
                        inputId = "add_for_all_models",
                        value = shared_input_model
    )
    
    
    mytable_approx_reactive$value <- (read_excel(input$upload$datapath, 8))
    
    ####
    
    upload_reactive$value <- 1
    
    
    if (length(mytable_approx_reactive$value) > 0) {
      click("show_approx_erros")
    } else {
      if (upload_reactive$value == 1 & sum(mytable_v_reactive$value[, 2]) > 0) {
        click("show_v_rep")
      }
    }
  })
  
  #### Function LaTeX ####
  
  latex <- function(latex_object) {
    formula_h <- ""
    
    latex_object[, 3:ncol(latex_object)] <- (-1 * latex_object[, 3:ncol(latex_object)])
    h_representation_pl_minus <- latex_object
    
    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) < 0, "-", "+")
    sign_p <- ifelse(data.frame(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)]) == 0, "", sign_p)
    
    h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)] <-
      abs(h_representation_pl_minus[, 3:ncol(h_representation_pl_minus)])
    
    for (loop_pl in 1:nrow(latex_object)) {
      formula_h_act <- ((as.character(h_representation_pl_minus[loop_pl, ])))
      
      sign_p_act <- sign_p[loop_pl, ]
      
      sign_pl <- ifelse(formula_h_act[1] == "1", "=", "\\leq")
      
      col_names_p <- colnames(h_representation_pl_minus)[3:ncol(h_representation_pl_minus)]
      
      p_with_factors <- paste(formula_h_act[3:(length(formula_h_act))], " \\times  ", col_names_p,
                              sep =
                                ""
      )
      
      p_with_factors[formula_h_act[3:(length(formula_h_act))] == "0"] <- ""
      
      
      
      p_with_factors <- paste(sign_p_act, " & ", p_with_factors, sep = "")
      
      
      p_with_factors <- ifelse(col_names_p == "", " & ", p_with_factors)
      
      
      p_with_factors <- paste(p_with_factors, " & ", collapse = "")
      
      p_with_factors <- paste(p_with_factors, sign_pl, " & ", formula_h_act[2],
                              collapse =
                                ""
      )
      
      p_with_factors <- paste(paste(p_with_factors, collapse = " "), " \\\\",
                              collapse =
                                " "
      )
      
      
      formula_h <- paste(c(formula_h, p_with_factors), collapse = " ")
    }
    
    formula_h
  }
  
  #### Function Plot ####
  
  function_plot <- function(pl_obj, n_submodels) {
    showTab(inputId = "tabs", target = "Plot Polytope(s)")
    showTab(inputId = "tabs", target = "Plot Edge Cases")
    
    
    
    output$plot <- renderPlotly({
      all_v_rep_in_list <- pl_obj
      
      models_to_plot <- input$name_model_plot
      models_to_plot <- unlist(str_split(models_to_plot, ";"))
      models_to_plot <- str_replace_all(models_to_plot, " ", "")
      models_to_plot <- unlist(models_to_plot)
      
      names_available_models <- rep(NA, n_submodels)
      
      matrix_pl_all <- numeric()
      
      for (loop_pl in 1:(n_submodels)) {
        extract_name <- all_v_rep_in_list[[loop_pl]]
        names_available_models[loop_pl] <- extract_name[1, 1]
        
        all_plot_actual <- all_v_rep_in_list[[loop_pl]]
        
        all_plot_actual <- all_plot_actual[, 2:ncol(all_plot_actual)]
        colnames_v_representation_plot_all <- colnames(all_plot_actual)
        
        all_plot_actual <- matrix(as.numeric(q2d(unlist(all_plot_actual))), ncol = ncol(all_plot_actual))
        
        matrix_pl_all <- rbind(
          matrix_pl_all,
          data.frame(names_available_models[loop_pl], (all_plot_actual))
        )
      }
      
      colnames(matrix_pl_all) <- c("models", colnames_v_representation_plot_all)
      
      names_available_models_reactive$value <- names_available_models
      
      
      select_models_to_plot <- names_available_models[names_available_models %in% models_to_plot]
      
      matrix_pl_all <- matrix_pl_all[matrix_pl_all[, 1] %in% select_models_to_plot, ]
      
      if (length(select_models_to_plot) > 0) {
        for (loop_pl in 1:length(select_models_to_plot)) {
          all_plot_actual <- matrix_pl_all[matrix_pl_all[, 1] == select_models_to_plot[loop_pl], 2:ncol(matrix_pl_all)]
          
          mixture_v_plot_names <- colnames(all_plot_actual)
          
          name_model <- select_models_to_plot[loop_pl]
          matrix_pl <- data.frame(all_plot_actual)
          colnames(matrix_pl) <- colnames(all_plot_actual)
          
          select_plot <- as.numeric(c(input$dim_1, input$dim_2, input$dim_3))
          
          updatePickerInput(
            session,
            inputId = "dim_1",
            choices = (1:ncol(matrix_pl))[-select_plot[c(2, 3)]],
            selected = input$dim_1
          )
          updatePickerInput(
            session,
            inputId = "dim_2",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 3)]],
            selected = input$dim_2
          )
          updatePickerInput(
            session,
            inputId = "dim_3",
            choices = (1:ncol(matrix_pl))[-select_plot[c(1, 2)]],
            selected = input$dim_3
          )
          
          matrix_pl <- matrix_pl[, select_plot]
          
          
          dim_names <- mixture_v_plot_names[c(
            as.numeric(input$dim_1),
            as.numeric(input$dim_2),
            as.numeric(input$dim_3)
          )]
          
          colnames(matrix_pl) <- dim_names
          
          
          axx <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[1]
          )
          
          axy <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[2]
          )
          
          axz <- list(
            nticks = .1,
            range = c(0, 1),
            title = dim_names[3]
          )
          
          ###
          
          trace1 <- list(
            mode = "markers",
            type = "scatter3d",
            x = (matrix_pl[, 1]),
            y = (matrix_pl[, 2]),
            z = (matrix_pl[, 3])
          )
          
          trace2 <- list(
            type = "mesh3d",
            x = (matrix_pl[, 1]),
            y = (matrix_pl[, 2]),
            z = (matrix_pl[, 3]),
            opacity = 0.05,
            alphahull = 0
          )
          
          ### check dimensionality using principal component analysis
          
          prcomp_sol <- summary(prcomp(matrix_pl))$importance[2, ]
          shape_point <- ifelse(sum(prcomp_sol != 0) == 1, 1, 0)
          shape_point <- ifelse(nrow(matrix_pl) == 1, 1, shape_point)
          shape_cube <- ifelse(sum(prcomp_sol != 0) == 3, 1, 0)
          shape_cube <- ifelse(shape_point == 1, 0, shape_cube)
          
          if (shape_cube == 1) {
            trace2 <- list(
              type = "mesh3d",
              x = (matrix_pl[, 1]),
              y = (matrix_pl[, 2]),
              z = (matrix_pl[, 3]),
              opacity = 0.05,
              alphahull = 0
            )
          }
          
          if (loop_pl == 1) {
            p <- plot_ly(
              colors = c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#999999"),
              width = 750,
              height = 750,
              name = name_model
            )
          }
          
          if (shape_point != 1) {
            p <-
              add_trace(
                p,
                mode = trace1$mode,
                type = trace1$type,
                x = trace1$x,
                y = trace1$y,
                z = trace1$z,
                name = name_model
              )
            
            if (shape_cube == 1) {
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = trace2$opacity,
                  alphahull = trace2$alphahull,
                  delaunayaxis = trace2$delaunayaxis,
                  name = name_model
                )
            } else {
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "x",
                  name = name_model
                )
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "y",
                  name = name_model
                )
              p <-
                add_trace(
                  p,
                  type = trace2$type,
                  x = trace2$x,
                  y = trace2$y,
                  z = trace2$z,
                  opacity = .05,
                  alphahull = -1,
                  delaunayaxis = "z",
                  name = name_model
                )
            }
          } else {
            p <-
              add_trace(
                p,
                x = trace1$x,
                y = trace1$y,
                z = trace1$z,
                type = "scatter3d",
                mode = "lines+markers",
                opacity = 1,
                line = list(width = 4),
                name = name_model
              )
          }
        }
        
        p %>% layout(
          scene = list(
            xaxis = axx,
            yaxis = axy,
            zaxis = axz,
            aspectmode = "manual",
            aspectratio = list(x = 1, y = 1, z = 1),
            camera = list(eye = list(
              x = 2, y = 2, z = 2
            ))
          )
        )
      }
    })
  }
  
  #### Function to extract In/equalities from Input ####
  
  extract_info <- function(test = input_relations$rel1, loop_numb_models, func_mytable_input) {
    
    
    check_input <- paste(func_mytable_input[, 2], collapse = ";")
    
    
    max_p <- max(as.numeric(unlist(str_extract_all(check_input, "(?<=p)[0-9]*"))))
    
    counter$n <- ifelse(counter$n < max_p, max_p, counter$n)
    
    probs <- data.frame()
    
    
    for (loop in 1:counter$n) {
      name_p <- str_replace_all(eval(parse(
        text = paste("unlist(AllInputs()$textin_name_", loop, ")", sep = "")
      )), fixed(" "), "_")
      name_p <- ifelse(length(name_p) == 0, paste("p_{", loop, "}", sep = ""), name_p)
      
      
      probs <- rbind(
        probs,
        c(
          paste("p_", loop, sep = ""),
          name_p
        )
      )
    }
    
    ####
    
    probs <- data.frame(probs, rep(0, nrow(probs)), rep(1, nrow(probs)))
    colnames(probs) <- c("variable", "name", "p_min", "p_max")
    min_values <- rep(0, nrow(probs))
    max_values <- rep(1, nrow(probs))
    
    ####* split and remove ####
    
    limits <- paste(paste(paste("p", 1:counter$n, sep = ""), "< 1", collapse = ";"),
                    paste(paste("p", 1:counter$n, sep = ""), "> 0", collapse = ";"),
                    sep = ";"
    )
    
    
    test <- paste0(test, ";", limits, collapse = "")
    
    test <- unlist(str_split(test, ";"))
    test <- str_replace_all(test, "\u2013", "-")
    test <- str_replace_all(test, "\u2014", "-")
    test <- str_replace_all(test, " ", "")
    
    test <- unlist(test)
    
    if (length(which(test %in% "")) > 0) {
      test <- test[-which(test %in% "")]
    }
    
    #### new ####
    
    numb_test <- length(test)
    
    
    for (loop_test in 1:numb_test) {
      pos_signs <- str_locate_all(test[loop_test], "[=><]")[[1]][, 1]
      
      signs <- numeric()
      
      for (loop_string in 1:length(pos_signs)) {
        signs <- c(signs, substr(test[loop_test], pos_signs[loop_string], pos_signs[loop_string]))
      }
      
      arguments <- strsplit(test[loop_test], "[=><]")[[1]]
      
      for (loop_length_arguments in 1:(length(arguments) - 1)) {
        test <- c(test, paste(arguments[loop_length_arguments],
                              signs[loop_length_arguments],
                              arguments[loop_length_arguments + 1],
                              sep = ""
        ))
      }
    }
    
    test <- test[-(1:numb_test)]
    
    #### {p1,p2} > {p3,p4}  ####
    
    expand_scalar <- function(scalar) {
      # Extract the first and second batches of 'p' variables
      batches <- strsplit(gsub("[{} ]", "", scalar), "[><=]")[[1]]
      batch1 <- unlist(strsplit(batches[1], ","))
      batch2 <- unlist(strsplit(batches[2], ","))
      
      # Determine the separator used in the original scalar
      separator <- gsub("[p{}]", "", gsub("[^><=]", "", scalar))
      
      comparisons <- character()
      
      # Generate comparisons between the first and second batch of 'p' variables
      for (p1 in batch1) {
        for (p2 in batch2) {
          comparisons <- c(comparisons, paste0(p1, separator, p2))
        }
      }
      
      output <- paste(comparisons, collapse = ";")
      return(output)
    }
    
    test_new <- numeric()
    
    for (loop_sep in 1:length(test)) {
      if (grepl("{", test[loop_sep], fixed = TRUE) == T) {
        test_new <- c(test_new, expand_scalar(test[loop_sep]))
      } else {
        test_new <- c(test_new, test[loop_sep])
      }
    }
    
    test <- test_new
    test <- unlist(str_split(test, ";"))
    
    #### cut string ####
    
    test_new <- numeric()
    
    for (loop_check in 1:length(test)) {
      lr_side <- unlist(str_split(test[loop_check], ">|=|<"))
      
      if (lr_side[1] != lr_side[2]) {
        test_new <- c(test_new, test[loop_check])
      }
    }
    
    test <- test_new
    
    for(loop_point in 1 : length(test)){
      
      if(substr(test[loop_point],1,1) == "."){
        
        test[loop_point] = paste("0",test[loop_point],sep="")
        
      }
      
    }
    
    
    ####* extract structure ####
    
    loc_above <- str_locate_all(test, ">")
    loc_equal <- str_locate_all(test, "=")
    loc_below <- str_locate_all(test, "<")
    
    loc_add <- str_locate_all(test, fixed("+"))
    loc_subtract <- str_locate_all(test, "-")
    loc_divide <- str_locate_all(test, "/")
    loc_multiply <- str_locate_all(test, fixed("*"))
    
    loc_digits <- str_locate_all(test, "[:digit:]")
    loc_prob <- str_locate_all(test, fixed("p"))
    
    loc_paren_open <- str_locate_all(test, fixed("("))
    loc_paren_closed <- str_locate_all(test, fixed(")"))
    loc_point <- str_locate_all(test, fixed("."))
    
    #** loop here
    
    loop_all <- 1
    all_extracted <- list()
    
    for (loop_bit in 1:length(test)) {
      test_actual <- test[loop_bit]
      test_actual_single <- unlist(str_split(test_actual, ""))
      
      pos <- rbind(
        "above" = data.frame(loc_above[loop_bit]),
        "below" = data.frame(loc_below[loop_bit]),
        "equal" = data.frame(loc_equal[loop_bit]),
        "add" = data.frame(loc_add[loop_bit]),
        "subtract" = data.frame(loc_subtract[loop_bit]),
        "multiply" = data.frame(loc_multiply[loop_bit]),
        "divide" = data.frame(loc_divide[loop_bit]),
        "digit" = data.frame(loc_digits[loop_bit]),
        "p" = data.frame(loc_prob[loop_bit]),
        "point" = data.frame(loc_point[loop_bit]),
        "open" = data.frame(loc_paren_open[loop_bit]),
        "closed" = data.frame(loc_paren_closed[loop_bit])
      )
      pos <- pos[order(pos[, 1]), ]
      
      length_str <- str_length(test_actual)
      
      pos <- data.frame(t(pos))
      
      info_type <- colnames(pos)
      
      info_type <- str_replace_all(info_type, fixed("."), "")
      info_type <- str_replace_all(info_type, "[:digit:]", "")
      
      #####* extract sub inequalities and equalities
      
      ineq_equ_parts_loc <- which(info_type == "above" |
                                    info_type == "below" |
                                    info_type == "equal")
      
      ineq_equ_parts_loc <- c(1, ineq_equ_parts_loc, length(info_type))
      
      loc_subparts <- numeric()
      
      for (loop_loc in 1:(length(ineq_equ_parts_loc) - 2)) {
        loc_act <- ineq_equ_parts_loc[c(1, 3) + (loop_loc - 1)]
        
        if (loop_loc == 1) {
          loc_act[2] <- loc_act[2] - 1
        }
        
        if (loop_loc < (length(ineq_equ_parts_loc) - 2) &
            loop_loc != 1) {
          loc_act[1] <- loc_act[1] + 1
          loc_act[2] <- loc_act[2] - 1
        }
        
        if (loop_loc == (length(ineq_equ_parts_loc) - 2)) {
          loc_act[1] <- loc_act[1] + 1
          loc_act[2] <- loc_act[2]
        }
        
        loc_subparts <- c(loc_subparts, loc_act)
      }
      
      loc_subparts <- t(matrix(loc_subparts, nrow = 2))
      
      subparts <- list()
      
      if (nrow(loc_subparts) == 1) {
        subparts[[paste0("part", loop_loc)]] <-
          rbind(
            info_type[1:length(info_type)],
            test_actual_single[1:length(info_type)]
          )
      } else {
        for (loop_loc in 1:nrow(loc_subparts)) {
          subparts[[paste0("part", loop_loc)]] <-
            rbind(
              info_type[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]],
              test_actual_single[loc_subparts[loop_loc, 1]:loc_subparts[loop_loc, 2]]
            )
        }
      }
      
      ###* loop over subparts
      
      for (loop_sub_part in 1:length(subparts)) {
        subparts_actual <- data.frame(subparts[loop_sub_part])
        
        
        pos_p <- which(subparts_actual[1, ] == "p")
        pos_before_p <- pos_p - 1
        
        if (pos_before_p[1] == 0) {
          subparts_actual <- cbind(cbind(c("add", "+"), c("digit", "1"), c("multiply", "*")), subparts_actual)
        }
        
        if (subparts_actual[1, 1] == "digit") {
          subparts_actual <- cbind(cbind(c("add", "+")), subparts_actual)
        }
        
        ####
        
        loc_after_relation <- which(
          subparts_actual[1, ] == "below" |
            subparts_actual[1, ] == "above" |
            subparts_actual[1, ] == "equal"
        ) + 1
        
        if (subparts_actual[1, loc_after_relation] == "digit" |
            subparts_actual[1, loc_after_relation] == "point") {
          subparts_actual <- cbind(
            subparts_actual[, 1:(loc_after_relation - 1)],
            c("add", "+"),
            subparts_actual[, loc_after_relation:ncol(subparts_actual)]
          )
        }
        
        
        add_multiply_function <- function() {
          pos_p <- which(subparts_actual[1, ] == "p")
          pos_before_p <- pos_p - 1
          
          add_multiply <- ifelse(
            subparts_actual[1, pos_before_p] == "add" |
              subparts_actual[1, pos_before_p] ==
              "below" |
              subparts_actual[1, pos_before_p] ==
              "above" |
              subparts_actual[1, pos_before_p] ==
              "equal",
            1,
            0
          )
          
          add_multiply <- ifelse(
            subparts_actual[1, pos_before_p] == "below" |
              subparts_actual[1, pos_before_p] ==
              "above" |
              subparts_actual[1, pos_before_p] ==
              "equal",
            2,
            add_multiply
          )
          
          add_multiply <- ifelse(subparts_actual[1, pos_before_p] == "subtract", -1,
                                 add_multiply
          )
          
          
          fix_factors <- pos_before_p[add_multiply != 0]
          add_multiply <- add_multiply[add_multiply != 0]
          
          return(list(fix_factors, add_multiply))
        }
        
        res_cadd_multiply_function <- add_multiply_function()
        
        fix_factors <- unlist(res_cadd_multiply_function[1])
        add_multiply <- unlist(res_cadd_multiply_function[2])
        
        if (length(add_multiply) > 0) {
          for (loop_add_multiply in 1:length(add_multiply)) {
            if (add_multiply[1] == -1) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(c("digit", "1"), c("multiply", "*")),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }
            
            if (add_multiply[1] == 1) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(c("digit", "1"), c("multiply", "*")),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }
            
            if (add_multiply[1] == 2) {
              subparts_actual <-
                cbind(
                  subparts_actual[, 1:fix_factors[1]],
                  cbind(
                    c("add", "+"),
                    c("digit", "1"),
                    c("multiply", "*")
                  ),
                  subparts_actual[, (fix_factors[1] + 1):ncol(subparts_actual)]
                )
            }
            
            res_cadd_multiply_function <- add_multiply_function()
            fix_factors <- unlist(res_cadd_multiply_function[1])
            add_multiply <- unlist(res_cadd_multiply_function[2])
          }
        }
        
        #####* extract p ####
        
        keep <- numeric()
        
        for (loop_extract in 1:ncol(subparts_actual)) {
          if (subparts_actual[1, loop_extract] == "p") {
            keep <- c(keep, 1)
          }
          
          if (loop_extract > 1 &
              subparts_actual[1, loop_extract] != "p") {
            if (subparts_actual[1, loop_extract] != "p" &
                subparts_actual[1, loop_extract] == "digit" &
                keep[loop_extract - 1] == 1) {
              keep <- c(keep, 1)
            } else {
              keep <- c(keep, 0)
            }
          }
          
          if (loop_extract == 1 &
              subparts_actual[1, loop_extract] != "p") {
            keep <- c(keep, 0)
          }
        }
        
        extract_p <- subparts_actual[2, keep == 1]
        extract_p <- paste0(extract_p, collapse = "")
        extract_p <- unlist(str_split(extract_p, "p"))
        extract_p <- extract_p[2:length(extract_p)]
        extract_p <- as.numeric(extract_p)
        
        ####* extract relation ####
        
        location_relation <- which(
          subparts_actual[1, ] == "below" |
            subparts_actual[1, ] == "above" |
            subparts_actual[1, ] == "equal"
        )
        
        relation_subpart <- subparts_actual[1, location_relation]
        relation_subpart <- unlist(relation_subpart)
        
        ####* extract factors ####
        
        left_side <- data.frame(subparts_actual[, 1:(location_relation -
                                                       1)])
        right_side <- data.frame(subparts_actual[, (location_relation +
                                                      1):ncol(subparts_actual)])
        
        
        location_add_subtract_left <- which(left_side[1, ] == "subtract" |
                                              left_side[1, ] == "add")
        
        location_p_left <- which(left_side[1, ] == "p")
        
        
        location_add_subtract_right <- which(right_side[1, ] == "subtract" |
                                               right_side[1, ] == "add")
        
        location_p_right <- which(right_side[1, ] == "p")
        
        
        ####** left side
        
        factor_left <- numeric()
        
        if (length(location_p_left) == 0) {
          factor_left <- paste0(left_side[2, ], collapse = "")
          extract_p <- c("numb", extract_p)
        } else {
          for (loop_loc in 1:length(location_add_subtract_left)) {
            factor_left <- c(
              factor_left,
              paste0(left_side[2, location_add_subtract_left[loop_loc]:(location_p_left[loop_loc] -
                                                                          2)], collapse = "")
            )
          }
        }
        
        ####** right side
        
        factor_right <- numeric()
        
        if (length(location_p_right) == 0) {
          factor_right <- paste0(right_side[2, ], collapse = "")
          
          extract_p <- c(extract_p, "numb")
        } else {
          for (loop_loc in 1:length(location_add_subtract_right)) {
            factor_right <- c(
              factor_right,
              paste0(right_side[2, location_add_subtract_right[loop_loc]:(location_p_right[loop_loc] -
                                                                            2)], collapse = "")
            )
          }
        }
        
        ####** combine extract
        
        extracted <- rbind(
          extract_p,
          c(
            rep("left", length(
              factor_left
            )),
            rep("right", length(
              factor_right
            ))
          ),
          c(factor_left, factor_right),
          relation_subpart
        )
        
        row.names(extracted) <- c("p", "left/right", "factor", "relation")
        colnames(extracted) <- paste("sub_", 1:ncol(extracted), sep = "")
        extracted <- data.frame(extracted)
        
        all_extracted[[paste0("line", loop_all)]] <- extracted
        
        loop_all <- loop_all + 1
      }
    }
    
    if (length(all_extracted) > 0) {
      numb_p <- numeric()
      
      for (loop_numb_p in 1:length(all_extracted)) {
        numb_p <- c(numb_p, (unlist(data.frame(
          all_extracted[loop_numb_p]
        )[1, ])))
      }
      
      numb_p <- numb_p[numb_p != "numb"]
      numb_p <- as.numeric(numb_p)
      numb_p <- max(numb_p)
    } else {
      all_extracted <- list()
      numb_p <- 0
    }
    
    ###
    
    numb_equl_ineq <- length(all_extracted)
    
    ineq_eq_left <- matrix(NA, ncol = numb_p, nrow = numb_equl_ineq)
    ineq_eq_right <- rep(NA, numb_equl_ineq)
    all_operators <- rep(NA, numb_equl_ineq)
    
    ###
    
    for (loop_relations in 1:length(all_extracted)) {
      actual_relations <- data.frame(all_extracted[loop_relations])
      
      extract_factors <- numeric()
      
      for (loop_factors in 1:ncol(actual_relations)) {
        extract_factors <- c(extract_factors, eval(parse(text = actual_relations[3, loop_factors])))
      }
      
      actual_relations[3, ] <- (extract_factors)
      
      actual_relations_left <- actual_relations[, actual_relations[2, ] == "left"]
      actual_relations_right <- actual_relations[, actual_relations[2, ] == "right"]
      actual_relations_operator <- actual_relations[4, 1]
      
      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)
      
      numb_left <- which(actual_relations_left[1, ] == "numb")
      numb_right <- which(actual_relations_right[1, ] == "numb")
      
      non_numb_left <- which(actual_relations_left[1, ] != "numb")
      non_numb_right <- which(actual_relations_right[1, ] != "numb")
      
      #### change signs
      
      if (actual_relations_operator == "equal" &
          length(non_numb_right) > 0 & length(non_numb_left) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
      }
      
      if (actual_relations_operator == "below" &
          length(non_numb_right) > 0) {
        actual_relations_right[3, non_numb_right] <- -1 * as.numeric(actual_relations_right[3, non_numb_right])
        actual_relations_left[3, numb_left] <- -1 * as.numeric(actual_relations_left[3, numb_left])
      }
      
      if (actual_relations_operator == "above" &
          length(non_numb_left) > 0) {
        actual_relations_left[3, non_numb_left] <- -1 * as.numeric(actual_relations_left[3, non_numb_left])
        actual_relations_right[3, numb_right] <- -1 * as.numeric(actual_relations_right[3, numb_right])
        actual_relations_operator <- "below"
        actual_relations_left[4, ] <- "below"
        actual_relations_right[4, ] <- "below"
      }
      
      ## do fractions
      
      actual_relations_left <- data.frame(actual_relations_left)
      actual_relations_right <- data.frame(actual_relations_right)
      
      actual_relations_left[3, ] <- (d2q(as.numeric(actual_relations_left[3, ])))
      actual_relations_right[3, ] <- (d2q(as.numeric(actual_relations_right[3, ])))
      
      
      ## translate into RCDD input
      
      total_rcdd <- data.frame(actual_relations_left, actual_relations_right)
      
      total_rcdd_leftside <- data.frame(total_rcdd[, total_rcdd[1, ] != "numb"])
      total_rcdd_rightside <- data.frame(total_rcdd[, total_rcdd[1, ] == "numb"])
      
      if (length(total_rcdd_rightside) == 0) {
        total_rcdd_rightside <- t(data.frame("numb", "right", "0", actual_relations_operator))
      }
      
      ###
      
      ineq_eq_left[loop_relations, as.numeric(unlist(total_rcdd_leftside[1, ]))] <- unlist(total_rcdd_leftside[3, ])
      ineq_eq_left[loop_relations, -as.numeric(unlist(total_rcdd_leftside[1, ]))] <- "0"
      ineq_eq_right[loop_relations] <- total_rcdd_rightside[3, ]
      all_operators[loop_relations] <- actual_relations_operator
    }
    
    #### approx equal
    
    
    add_ineq_eq_left <- numeric()
    add_ineq_eq_right <- numeric()
    add_all_operators <- numeric()
    
    ####*** add approx equalities when selected ####
    
    input_approx <- numeric()
    
    if (length(mytable_approx_reactive$value) == 0) {
      tune_knob <- 0
    } else {
      tune_knob <- unlist(mytable_approx_reactive$value[loop_numb_models, 2])
    }
    
    
    ####
    
    if (tune_knob != 0 | is.na(tune_knob)) {
      for (loop_tune in 1:length(all_operators)) {
        if (all_operators[loop_tune] == "equal") {
          add_ineq_eq_left <- rbind(add_ineq_eq_left, (ineq_eq_left[loop_tune, ]))
          add_ineq_eq_left <- rbind(add_ineq_eq_left, d2q(-1 * q2d(ineq_eq_left[loop_tune, ])))
          
          add_ineq_eq_right <- c(
            add_ineq_eq_right,
            q2d(ineq_eq_right[loop_tune]) + tune_knob,
            -(q2d(ineq_eq_right[loop_tune])) + tune_knob
          )
          
          add_all_operators <- c(add_all_operators, rep("below", 2))
        }
      }
      
      add_ineq_eq_right <- d2q(add_ineq_eq_right)
      
      ineq_eq_left <- ineq_eq_left[all_operators != "equal", ]
      ineq_eq_right <- ineq_eq_right[all_operators != "equal"]
      all_operators <- all_operators[all_operators != "equal"]
      
      ineq_eq_left <- rbind(ineq_eq_left, add_ineq_eq_left)
      ineq_eq_right <- c(ineq_eq_right, add_ineq_eq_right)
      all_operators <- c(all_operators, add_all_operators)
    }
    
    
    
    all_operators_reactive$value <- all_operators
    ineq_eq_left_reactive$value <- ineq_eq_left
    ineq_eq_right_reactive$value <- ineq_eq_right
    numb_p_reactive$value <- numb_p
    probs_reactive$value <- probs
  }
  
  #### Function for Representations ####
  
  ####* H-representation input models ####
  
  observeEvent(input$go_v_h, {
    
    n_input_models <- counter_input$n
    
    ####** input
    
    mytable_input <- data.frame()
    
    for (loop in 1:counter_input$n) {
      mytable_input <- rbind(
        mytable_input,
        c(
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_name",
              loop,
              ")",
              sep = ""
            )
          )),
          eval(parse(
            text = paste(
              "unlist(AllInputs()$textin_relations_complete_",
              loop,
              ")",
              sep = ""
            )
          ))
        )
      )
    }
    
    colnames(mytable_input) <- c("model name", "input_models")
    #mytable_input[, 2] = str_replace_all( mytable_input[, 2],",",";") # !!!!!!!!
    
    names_models_reactive$value <- mytable_input$`model name`
    
    ####** start conversion ####
    
    empty_set = numeric()
    
    if (sum(mytable_input[, 2] == "") == 0) {
      showTab(inputId = "tabs", target = "H-representation")
      showTab(inputId = "tabs", target = "Parsimony")
      
      ####** loop models ####
      
      numb_models_to_convert <- n_input_models
      
      feedback_h_models <- numeric()
      
      feedback_name <- numeric()
      
      for (loop_numb_models in 1:numb_models_to_convert) {
        name_m <- mytable_input[loop_numb_models, 1]
        
        if(loop_numb_models > 1){
          
          shinyalert(
            
            title =  paste0("Creating H-representation of model ", name_m, ". Done for model(s): ",paste(feedback_name, collapse = ", "),"."),
            showConfirmButton = F,
            type="info",
            animation=F
            
          )
          
          
        }else{
          
          shinyalert(
            
            title = paste0("Creating H-representation of model ", name_m, "."),
            showConfirmButton = F,
            type="info"
     
            
          )
          
        }
        

        
        feedback_name <- c(feedback_name, name_m)
      

        extract_info(mytable_input[loop_numb_models, 2], loop_numb_models, mytable_input)
        
        feedback_h_models <- paste(feedback_h_models,
                                   paste(
                                     "#", loop_numb_models, " of ",
                                     numb_models_to_convert,
                                     ": Creating H-representation of ",
                                     name_m
                                   ),
                                   sep = "<br>"
        )
        
        probs <- isolate(probs_reactive$value)
        
        ineq_eq_left <- isolate(ineq_eq_left_reactive$value)
        ineq_eq_right <- isolate(ineq_eq_right_reactive$value)
        all_operators <- isolate(all_operators_reactive$value)
        
        if (sum(all_operators != "equal") > 0 &
            sum(all_operators == "equal") > 0) {
          h_representation <- makeH(
            ineq_eq_left[all_operators != "equal", ],
            ineq_eq_right[all_operators != "equal"],
            ineq_eq_left[all_operators == "equal", ],
            ineq_eq_right[all_operators == "equal"]
          )
        }
        
        if (sum(all_operators != "equal") == 0 &
            sum(all_operators == "equal") > 0) {
          h_representation <- makeH(
            a2 = ineq_eq_left,
            b2 = ineq_eq_right
          )
        }
        
        if (sum(all_operators != "equal") > 0 &
            sum(all_operators == "equal") == 0) {
          h_representation <- makeH(
            ineq_eq_left,
            ineq_eq_right
          )
        }
        
        names_p_multi <- c("", "", probs[, 2])
        
        
        if (nrow(h_representation) > 1) {
          h_representation <- redundant(h_representation)$output
        }
        
        colnames(h_representation) <- names_p_multi
        
        valid_hrep <- lpcdd(h_representation, rep("0", ncol(h_representation) - 2))
        valid_hrep <- ifelse(valid_hrep$solution.type == "Inconsistent", 0, 1)
        
        
        ####* Save h-represenation ####
        
        
        h_reactive[[mytable_input[loop_numb_models, 1]]] <- h_representation
        
        
        if (valid_hrep == 0) {
          

          if (loop_numb_models < (nrow(mytable_input) + 1)) {
            
            empty_set = c(empty_set,mytable_input[loop_numb_models, 1])
            

          } else {
            
            empty_set = c(empty_set,mytable_inter[loop_numb_models, 1])

          }
          
        }
        
        Sys.sleep(1)
        
        shinyalert::closeAlert()
        
        if(loop_numb_models == numb_models_to_convert){
          
          if(length(empty_set) > 0){
            
            show_text = paste0("Created H-representation of model(s): ",paste(feedback_name, collapse = ", "),
                               ". 
                               
                               Warning: Do not interpet the H-description of model(s) ",paste0(empty_set, collapse = ", "),". They form an empty set.")
            
            add_text =                                
              "What does this mean?
              
              For instance, the input 'p1 > .5; p1 < .1' would result in an empty set: p1 cannot be simultaneously greater than or equal to .5 and less than or equal to .1.
            
            You entered a set of in/equalities like in the example, they cannot be simultaneously satisfied."
            
          }else{
            
            show_text = paste0("Created H-representation of model(s): ",paste(feedback_name, collapse = ", "),".")
            
            add_text = ""
            
          }
          
          if(is.null(mytable_v_reactive$value) == FALSE){
            
            show_text = paste0("Created H-representation of model(s): ",paste(feedback_name, collapse = ", "),".")
            add_text = ""
            
            shinyalert(
              
              title = show_text,
              text = add_text,
              showConfirmButton = F,
              type="success",
              animation=F

              
            )
            
            Sys.sleep(3)
            
            shinyalert::closeAlert()
            
          }else{
            
            shinyalert(
              
              title =  show_text,
              text = add_text,
              showConfirmButton = T,
              type="success",
              closeOnClickOutside = T,
              animation=F
              
            )
            
            
          }

          
        }
        

      }
      
      ####* V-representation ####
      
      if (is.null(mytable_v_reactive$value) == FALSE) {
        mytable_v_reactive$value$`Model name` <-
          mytable_input$`model name`[1:length(mytable_v_reactive$value$`Model name`)]
      }
      
      mytable_v <- mytable_v_reactive$value
      
      #### start v-conversion
      
      if (sum(mytable_v[, 2]) > 0) {
        mytable_v_reactive$value$`Model name` <- mytable_input$`model name`[1:length(mytable_v$`Model name`)]
        mytable_v$`Model name` <- mytable_input$`model name`[1:length(mytable_v$`Model name`)]
        
        models_picked_for_v_representation <- mytable_v[which(mytable_v[, 2]), 1]
        
        models_picked_for_v_representation = 
          models_picked_for_v_representation[is.na(models_picked_for_v_representation) == F]
        
        invalid_model <- numeric()
        
        for (loop_numb_models in 1:length(models_picked_for_v_representation)) {
          
          if(loop_numb_models > 1){
            
            shinyalert(
              
              title =  paste0("Creating V-representation of model ", models_picked_for_v_representation[loop_numb_models], ". Done for model(s): ",paste(models_picked_for_v_representation[1:(loop_numb_models-1)], collapse = ", "),"."),
              showConfirmButton = F,
              type="info",
              animation=F
              
            )
            
          }else{
            
            shinyalert(
              
              title = paste0("Creating V-representation of model ", models_picked_for_v_representation[loop_numb_models], "."),
              showConfirmButton = F,
              type="info",
              animation=F
              
              
            )
            
          }
          

          v_rep_actual <- scdd(eval(parse(text = paste("h_reactive$'", models_picked_for_v_representation[loop_numb_models], "'", sep = ""))))
          colnames(v_rep_actual$output) <- names_p_multi
          
          if (nrow(data.frame(v_rep_actual)) == 0) {
            invalid_model <- c(invalid_model, models_picked_for_v_representation[loop_numb_models])
          } else {
            v_reactive[[models_picked_for_v_representation[loop_numb_models]]] <- v_rep_actual
          }
          
          
          #####
        
          
          Sys.sleep(1)
          
          shinyalert::closeAlert()
          
          if(loop_numb_models == length(models_picked_for_v_representation)){
            
            if(length(empty_set) > 0){
              
              show_text = paste0("Created V-representation of model(s): ",paste(models_picked_for_v_representation, collapse = ", "),
                                 ". 
                               
                               Warning: Do not interpet the H and V-descriptions of model(s) ",paste0(empty_set, collapse = ", "),". They form an empty set.")
              
              add_text =                                
                "What does this mean?
              
              For instance, the input 'p1 > .5; p1 < .1' would result in an empty set: p1 cannot be simultaneously greater than or equal to .5 and less than or equal to .1.
            
            You entered a set of in/equalities like in the example, they cannot be simultaneously satisfied."
              
            }else{
              
              show_text = paste0("Created V-representation of model(s): ",paste(models_picked_for_v_representation, collapse = ", "),".")
              
              add_text = ""
              
            }
            
            shinyalert(
              
              title = show_text,
              text = add_text,
              showConfirmButton = T,
              type="success",
              closeOnClickOutside = T,
              animation=F
              
            )
            
          }
   
        }
        
      } else {
        hideTab(inputId = "tabs", target = "V-representation")
        hideTab(inputId = "tabs", target = "Plot Polytope(s)")
        hideTab(inputId = "tabs", target = "Plot Edge Cases")
      }
      
      ####* Display Formula H-Representation ####
      
      equation_all_total <- numeric()
      
      for (loop_latex in 1:length(names_models_reactive$value)) {
        current_name <- names_models_reactive$value[loop_latex]
        
        h_representation_latex <- q2d(h_reactive[[current_name]])
        h_representation_latex <- fractions(h_representation_latex)
        
        begin_eq <- paste(current_name, " $$\\begin{eqnarray} ", sep = "")
        end_eq <- " \\end{eqnarray}$$"
        
        if (nrow(h_representation_latex) < 91) {
          equation_all <- paste(begin_eq, latex(h_representation_latex), end_eq,
                                sep = ""
          )
        } else {
          n_equal <- sum(data.frame(h_representation_latex)[, 1] == 1)
          n_inequal <- sum(data.frame(h_representation_latex)[, 1] == 0)
          
          equation_all <- paste(begin_eq, "\\text{H-representation consists of ", n_equal, " equalities and ",
                                n_inequal, " inequalities. Models with more than 90 in/equalities are not displayed.}", end_eq,
                                sep = ""
          )
        }
        
        equation_all_total <- c(equation_all_total, equation_all)
      }
      
      output$h <- renderUI({
        withMathJax(helpText({
          paste(equation_all_total, collapse = "")
        }))
      })
      
      equation_all_total_reactive$value <- equation_all_total
      
      ####* Tables V-representation ####
      
      show_table <- names(v_reactive)
      
      show_table <- show_table[show_table %in% names_models_reactive$value]
      show_table <- names_models_reactive$value[names_models_reactive$value %in% show_table]
      
      updatePickerInput(session,
                        inputId = "go_example",
                        choices = show_table
      )
      
      if (length(show_table) > 0) {
        showTab(inputId = "tabs", target = "V-representation")
        
        counter_table <- 1
        v_table_list <- list()
        
        for (loop_table in show_table) {
          v_table <- data.frame(v_reactive[[loop_table]])
          
          if (length(v_table) > 0) {
            v_table <- v_table[, 3:ncol(v_table)]
            v_table <- cbind(loop_table, v_table)
            
            rownames(v_table) <-
              paste("V_", 1:nrow(v_table), sep = "")
            
            colnames(v_table) <- c("model_name", names_p_multi[3:length(names_p_multi)])
            
            v_table_list[[counter_table]] <- v_table
          }
          counter_table <- counter_table + 1
        }
        
        output$v_representation_table <- renderUI({
          lapply(as.list(seq_len(length(v_table_list))), function(i) {
            id <- paste0("v_representation_table", i)
            DT::dataTableOutput(id)
          })
        })
        
        for (loop_table in seq_len(length(v_table_list))) {
          if (is.null(v_table_list[[loop_table]]) == F) {
            local({
              id <- paste0("v_representation_table", loop_table)
              pl_t <- v_table_list[[loop_table]]
              colnames_pl_t <- colnames(pl_t)
              pl_t <- cbind(pl_t[, 1], matrix(as.character(fractions(q2d(unlist(pl_t[, 2:ncol(pl_t)])))), ncol = ncol(pl_t) - 1))
              colnames(pl_t) <- colnames_pl_t
              colnames(pl_t)[1] <- "Model Name"
              output[[id]] <- DT::renderDataTable(pl_t)
            })
          }
        }
        
        function_plot(v_table_list, length(v_table_list))
      }
    } else {
      shinyalert("Error", "Please type in all model specifications.",
                 type = "error", closeOnClickOutside = T
      )
    }
  })
  
  #### Parsimony ####
  
  observeEvent(input$go, {
    input_volume <- isolate(input_volume_reactive)
    
    output$plot_parsimony <- renderPlotly({
      setClass(
        "model_s4",
        representation(
          A = "matrix", b = "numeric",
          type = "character"
        )
      )
      
      names_h_rep <- names_models_reactive$value
      
      n_rep <- 1
      parsim_rep <- numeric()
      
      
      for (loop_repeat_parsimony in 1:n_rep) {
        
        parsim <- numeric()
        act_dim <- numeric()
        
        for (loop_parsimony in 1:length(names_h_rep)) {
          act_pars <- (isolate(h_reactive[[names_h_rep[loop_parsimony]]]))
          act_pars <- matrix(as.character(act_pars), ncol = ncol(act_pars))
          
          
          #  not_full_dim <- ifelse(sum(q2d(act_pars)[, 1]) > 0, 1, 0)
          not_full_dim <- 0
          
          # summary(prcomp( data.frame(((v_representation_reactive$value)[1]))[,2:4]))$importance[2,]
          
          if (not_full_dim == 0) {
            left_pars <- q2d((act_pars[, 3:ncol(act_pars)]))
            right_pars <- q2d((act_pars[, 2]))
            
            model_s4 <- new("model_s4",
                            A = -left_pars,
                            b = right_pars,
                            type = "Hpolytope"
            )
            
            
            ####* CB ####
            
            if (isolate(input$CB) == TRUE) {
              #####* settings strings ####
              
              
              settings_string_CB <- rep(NA, 6)
              
              settings_string_CB[1] <- ifelse(input_volume$value[1] == "default", "",
                                              paste("'error' =", input_volume$value[1], sep = "")
              )
              
              
              rand_w <- ifelse(input_volume$value[2] == "default", "default", input_volume_reactive$value[2])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)
              
              
              settings_string_CB[2] <- ifelse(rand_w == "default", "",
                                              paste("'random_walk' = '", rand_w, "'", sep = "")
              )
              
              settings_string_CB[3] <- ifelse(input_volume$value[3] == "default", "",
                                              paste("'walk_length' =", input_volume$value[3], sep = "")
              )
              
              settings_string_CB[4] <- ifelse(input_volume$value[4] == "default", "",
                                              paste("'win_len' =", input_volume$value[4], sep = "")
              )
              
              
              settings_string_CB[5] <- ifelse(input_volume$value[5] == "default", "",
                                              paste("'hpoly' =", input_volume$value[5], sep = "")
              )
              
              settings_string_CB[6] <- ifelse(input_volume$value[6] == "none", "",
                                              paste("'seed' =", input_volume$value[6], sep = "")
              )
              
              settings_string_CB <- settings_string_CB[settings_string_CB != ""]
              
              settings_string_CB <- paste(settings_string_CB, collapse = ",")
              
              if (settings_string_CB[1] != "") {
                settings_string_final_CB <- paste("list('algorithm' = 'CB',", settings_string_CB, ")")
              } else {
                settings_string_final_CB <- "list('algorithm' = 'CB')"
              }
              
              #####* calc volume ####
              
              act_vol_CB <- volume(model_s4,
                                   settings =
                                     eval(parse(
                                       text = (settings_string_final_CB)
                                     ))
              )
            } else {
              act_vol_CB <- NA
            }
            
            
            ####* SoB ####
            
            if (isolate(input$SoB) == TRUE) {
              #####* settings strings ####
              
              
              settings_string_SoB <- rep(NA, 4)
              
              settings_string_SoB[1] <- ifelse(input_volume$value[7] == "default", "",
                                               paste("'error' =", input_volume$value[7], sep = "")
              )
              
              
              rand_w <- ifelse(input_volume$value[8] == "default", "default", input_volume_reactive$value[8])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)
              
              
              settings_string_SoB[2] <- ifelse(rand_w == "default", "",
                                               paste("'random_walk' = '", rand_w, "'", sep = "")
              )
              
              settings_string_SoB[3] <- ifelse(input_volume$value[9] == "default", "",
                                               paste("'walk_length' =", input_volume$value[9], sep = "")
              )
              
              
              settings_string_SoB[4] <- ifelse(input_volume$value[10] == "none", "",
                                               paste("'seed' =", input_volume$value[10], sep = "")
              )
              
              settings_string_SoB <- settings_string_SoB[settings_string_SoB != ""]
              
              settings_string_SoB <- paste(settings_string_SoB, collapse = ",")
              
              if (settings_string_SoB[1] != "") {
                settings_string_final_SoB <- paste("list('algorithm' = 'SOB',", settings_string_SoB, ")")
              } else {
                settings_string_final_SoB <- "list('algorithm' = 'SOB')"
              }
              
              
              #####* calc volume ####
              
              act_vol_SoB <- volume(model_s4,
                                    settings = eval(parse(
                                      text = (settings_string_final_SoB)
                                    ))
              )
            } else {
              act_vol_SoB <- NA
            }
            
            ####* CG ####
            
            if (isolate(input$CG) == TRUE) {
              #####* settings strings ####
              
              
              settings_string_CG <- rep(NA, 5)
              
              settings_string_CG[1] <- ifelse(input_volume$value[11] == "default", "",
                                              paste("'error' =", input_volume$value[11], sep = "")
              )
              
              
              rand_w <- ifelse(input_volume$value[12] == "default", "default", input_volume_reactive$value[12])
              rand_w <- ifelse(rand_w == "Coordinate Directions Hit-and-Run", "CDHR", rand_w)
              rand_w <- ifelse(rand_w == "Random Directions Hit-and-Run", "RDHR", rand_w)
              rand_w <- ifelse(rand_w == "Ball Walk", "BaW", rand_w)
              rand_w <- ifelse(rand_w == "Billiard Walk", "BiW", rand_w)
              
              
              settings_string_CG[2] <- ifelse(rand_w == "default", "",
                                              paste("'random_walk' = '", rand_w, "'", sep = "")
              )
              
              settings_string_CG[3] <- ifelse(input_volume$value[13] == "default", "",
                                              paste("'walk_length' =", input_volume$value[13], sep = "")
              )
              
              settings_string_CG[4] <- ifelse(input_volume$value[14] == "default", "",
                                              paste("'win_len' =", input_volume$value[14], sep = "")
              )
              
              
              settings_string_CG[5] <- ifelse(input_volume$value[15] == "none", "",
                                              paste("'seed' =", input_volume$value[15], sep = "")
              )
              
              settings_string_CG <- settings_string_CG[settings_string_CG != ""]
              
              settings_string_CG <- paste(settings_string_CG, collapse = ",")
              
              if (settings_string_CG[1] != "") {
                settings_string_final_CG <- paste("list('algorithm' = 'CG',", settings_string_CG, ")")
              } else {
                settings_string_final_CG <- "list('algorithm' = 'CG')"
              }
              
              #####* calc volume ####
              
              act_vol_CG <- volume(model_s4,
                                   settings = eval(parse(
                                     text = (settings_string_final_CG)
                                   ))
              )
            } else {
              act_vol_CG <- NA
            }
            
            
            act_dim <- c(act_dim, "Full-dimensional")
          } else {
            if (isolate(input$CB) == TRUE) {
              act_vol_CB <- 0
            } else {
              act_vol_CB <- NA
            }
            
            
            if (isolate(input$SoB) == TRUE) {
              act_vol_SoB <- 0
            } else {
              act_vol_SoB <- NA
            }
            
            
            if (isolate(input$CG) == TRUE) {
              act_vol_CG <- 0
            } else {
              act_vol_CG <- NA
            }
            
            act_dim <- c(act_dim, "Not full-dimensional")
          }
          
          parsim <- c(parsim, c(act_vol_CB, act_vol_SoB, act_vol_CG))
        }
        
        
        parsim <- data.frame(
          "Model" = rep(as.factor(unlist(names_h_rep)), each = 3),
          "Algorithm" = as.factor(rep(
            c("Cooling Bodies", "Sequence of Balls", "Cooling Gaussian"), length(names_h_rep)
          )),
          "Volume" = parsim,
          "Dimensionality" = rep(act_dim, each = 3)
        )
        
        
        parsim <- parsim[complete.cases(parsim) == T, ]
        
        parsim_rep <- rbind(parsim_rep, parsim)
      }
      
      
      parsim_raw <- data.frame( parsim_rep[order(parsim_rep$Model, parsim_rep$Algorithm), ])
      
      parsim <- parsim_rep %>%
        group_by(Model, Algorithm, Dimensionality) %>%
        summarize(
          SD = sd(Volume), Volume = mean(Volume),
          Max_BF = 1 / Volume
        )
      
      
      parsim_wide <- parsim %>%
        pivot_wider(
          names_from = c("Algorithm"),
          values_from = c(Volume, SD, Max_BF)
        )
      
      parsim_wide_table <- (parsim_wide %>% select(-contains("SD")))
      parsim_wide_table[3:ncol(parsim_wide_table)] <- (parsim_wide_table[3:ncol(parsim_wide_table)])
      
      parsim_wide_table_reactive$value <- parsim_wide_table
      
      parsim_wide_table_pl <- parsim_wide_table
      parsim_wide_table_pl[, 3:ncol(parsim_wide_table_pl)] <- round(parsim_wide_table_pl[, 3:ncol(parsim_wide_table_pl)], 2)
      
      output$parsim_table <- DT::renderDataTable(parsim_wide_table_pl)
      
      output$download_volume <- downloadHandler(
        filename = function() {
          paste("volume_",
                str_replace_all(Sys.Date(), "-", "_"),
                ".csv",
                sep = ""
          )
        },
        content = function(file) {
          write.csv(parsim_raw, file)
        }
      )
      
      
      fig <-
        plot_ly(
          parsim[order(parsim$Algorithm), ],
          x = ~Model,
          color = ~Algorithm,
          y = ~Volume,
          type = "bar",
          colors = c("#009E73", "#E69F00", "#CC79A7")
        )
      
      fig <-
        fig %>% layout(
          yaxis = list(
            title = "Approximate volume"
          ),
          xaxis = list(title = "Model", tickangle = 45),
          barmode = "group"
        )
    })
    
    #### Parsimony output ####
    
    output$parsimony_spinner_table <- renderUI({
      DT::dataTableOutput("parsim_table")
    })
    
    output$parsimony_spinner <- renderUI({
      withSpinner(
        plotlyOutput("plot_parsimony"),
        type = 8,
        color = "black",
        color.background = "black"
      )
    })
  })
  
  #####* Download H-representation ####
  
  output$d_h <- downloadHandler(
    filename = function() {
      paste("h_representation_",
            str_replace_all(Sys.Date(), "-", "_"),
            ".tar",
            sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      all_h_rep_in_matrix <- numeric()
      
      numb_models <- length(names_models_reactive$value)
      
      for (loop_qtest_h in 1:numb_models) {
        add_repl_mod <- data.frame(
          isolate(names_models_reactive$value[loop_qtest_h]),
          isolate(h_reactive[[names_models_reactive$value[loop_qtest_h]]])
        )
        
        colnames(add_repl_mod) <- rep("", ncol(add_repl_mod))
        
        all_h_rep_in_matrix <- rbind(
          all_h_rep_in_matrix,
          add_repl_mod
        )
      }
      
      col_names_h <- c("model name", "equ/ineq", "right", paste("p", 1:(ncol(all_h_rep_in_matrix) - 3), sep = ""))
      
      colnames(all_h_rep_in_matrix) <- col_names_h
      
      
      unique_v <- unique(all_h_rep_in_matrix$`model name`)
      n_v <- length(unique_v)
      
      for (loop_v in unique_v) {
        act_v <- all_h_rep_in_matrix[all_h_rep_in_matrix$`model name` == loop_v, ]
        act_v_ineq <- act_v[act_v$`equ/ineq` == 0, ]
        
        header_v <- dim(act_v_ineq) - c(0, 3)
        
        act_v_left <- act_v_ineq[, 4:ncol(act_v_ineq)]
        act_v_left <- -matrix(q2d(unlist(act_v_left)), ncol = ncol(act_v_left))
        
        act_v_right <- q2d(unlist(act_v_ineq[, 3]))
        act_v_right <- data.frame(act_v_right)
        
        act_v_eq <- act_v[act_v$`equ/ineq` == 1, ]
        
        ### 
        
        act_v_left = fractions(act_v_left)
        act_v_right = fractions(unlist(act_v_right))
        
        nom_left = str_extract_part(act_v_left, "/", before = TRUE)
        denom_left = str_extract_part(act_v_left, "/", before = FALSE)
        
        nom_right = str_extract_part(act_v_right, "/", before = TRUE)
        denom_right = str_extract_part(act_v_right, "/", before = FALSE)
        
        pos_ratios_left = str_detect(act_v_left, "/")
        pos_ratios_right = str_detect(act_v_right, "/")
        
        nom_left[pos_ratios_left == F] = (c(act_v_left)[pos_ratios_left == F])
        denom_left = ifelse(is.na(denom_left) == T, "1",denom_left)
        
        nom_right[pos_ratios_right == F] = (c(act_v_right)[pos_ratios_right == F])
        denom_right = ifelse(is.na(denom_right) == T, "1",denom_right)
        
        nom_left = as.numeric(nom_left)
        denom_left = as.numeric(denom_left)
        
        nom_right = as.numeric(nom_right)
        denom_right = as.numeric(denom_right)
        
        prod_denom_all = prod(unique(c(denom_left[denom_left!=0],denom_right[denom_right!=0])))
        
        act_v_left = act_v_left * prod_denom_all
        act_v_right = act_v_right * prod_denom_all
        
        act_v_left = format(act_v_left,scientific = FALSE)
        act_v_right = format(act_v_right,scientific = FALSE)
        ###
        
        total_file <- paste(
          paste(as.character(header_v), collapse = " "),
          "\n",
          "\n",
          paste(apply((act_v_left), 1, paste, collapse = " "), collapse = "\n"),
          "\n",
          "\n",
          paste((act_v_right), collapse = "\n"),
          sep = ""
        )
        
        file_name_addendum <- ""
        
        if (dim(act_v_eq)[1] > 0) {
          
          total_file <-
            paste("The model includes equalities. Use V-representation of model for QTest."
                  
            )
          
          file_name_addendum <- "_no_QTest_file_available"
        }
        
        fileName <-
          paste(loop_v, file_name_addendum, ".txt", sep = "")
        
        write.table(
          total_file,
          fileName,
          quote = FALSE,
          col.names = FALSE,
          row.names = FALSE
        )
        
        
        files <- c(fileName, files)
      }
      
      # zip::zip(file, files)
      tar(file, files)
    }
  )
  
  #####* V-representation ####
  
  output$d_v <- downloadHandler(
    filename = function() {
      paste("v_representation_",
            str_replace_all(Sys.Date(), "-", "_"),
            ".tar",
            sep = ""
      )
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL
      
      download_v_names <- names(v_reactive)
      
      download_v_names <- download_v_names[download_v_names %in% names_models_reactive$value]
      download_v_names <- names_models_reactive$value[names_models_reactive$value %in% download_v_names]
      
      for (loop_v_qtest in 1:length(download_v_names)) {
        csv_file <- data.frame(v_reactive[[download_v_names[loop_v_qtest]]])
        
        if (is.null(csv_file) == F) {
          
          csv_file_dec = csv_file
          csv_file_dec = unlist(csv_file_dec)
          
          for(loop_eval in 1 : length(csv_file_dec)){
            
            right_dec =
              eval(parse(text=csv_file_dec[loop_eval]))
            
            numb_dec = str_count(gsub(".*[.]","",
                                      format(right_dec,scientific=F)), "0")
            
            csv_file_dec[loop_eval] = format(round(right_dec, digits=6),scientific=F)
          }
          
          csv_file = matrix(csv_file_dec,ncol=ncol(csv_file))
          
          ####
          
          csv_file <- csv_file[, 3:ncol(csv_file)]
          csv_file <- t(csv_file)
          
          header_v <- paste("V", 1:ncol(csv_file), sep = "")
          line_2 <- rep(1, ncol(csv_file))
          
          total_file <- paste(
            paste((header_v), collapse = ","),
            "\n",
            paste((line_2), collapse = ","),
            "\n",
            "\n",
            paste(apply((csv_file), 1, paste, collapse = ","), collapse = "\n"),
            sep = ""
          )
          
          fileName <-
            paste(download_v_names[loop_v_qtest], ".csv", sep = "")
          
          write.table(
            total_file,
            fileName,
            quote = FALSE,
            col.names = FALSE,
            row.names = FALSE
          )
          
          files <- c(fileName, files)
        }
      }
      
      tar(file, files)
    }
  )
  
  ####* LaTeX-file of H-representation ####
  
  output$d_latex <- downloadHandler(
    filename = function() {
      paste("models_",
            str_replace_all(Sys.Date(), "-", "_"),
            ".tex",
            sep = ""
      )
    },
    content = function(file) {
      header_latex <- (
        "\\documentclass{article}
\\usepackage{amsmath}
\\begin{document}
\\section*{Models}
"
      )
      
      footer_latex <- ("
\\end{document}")
      
      equation_all_total <- unlist(equation_all_total_reactive$value)
      equation_all_total <- paste(equation_all_total, collapse = "")
      
      formula_h_all_download <- str_replace_all(equation_all_total, "eqnarray", "align*")
      formula_h_all_download <- str_replace_all(formula_h_all_download, "\\$", "")
      
      formula_h_all_download <- paste(header_latex,
                                      formula_h_all_download,
                                      footer_latex,
                                      sep = ""
      )
      
      write.table(
        formula_h_all_download,
        file,
        quote = FALSE,
        col.names = FALSE,
        row.names = FALSE
      )
    }
  )
  
  ##### Settings for Approximate Equalities ####
  
  observeEvent(input$show_approx_erros, {
    matrix_approx <- mytable_approx_reactive$value
    
    name_models <- numeric()
    
    for (loop_save in 1:counter_input$n) {
      name_models <- c(name_models, eval(parse(text = paste("AllInputs()$textin_relations_name", loop_save, sep = ""))))
    }
    
    if (is.null(matrix_approx) == T | length(mytable_approx_reactive$value) == 0) {
      matrix_approx <- data.frame(name_models, rep(0, counter_input$n))
    }
    
    if (nrow(matrix_approx) < counter_input$n) {
      matrix_approx <- data.frame(
        name_models,
        c(matrix_approx[, 2], rep(0, counter_input$n - nrow(matrix_approx)))
      )
    }
    
    if (nrow(matrix_approx) > counter_input$n) {
      matrix_approx <- matrix_approx[1:counter_input$n, ]
    }
    
    colnames(matrix_approx) <- c("Models", "Approximate parameter value")
    
    dont_freeze <- numeric()
    
    for (loop_approx in 1:counter_input$n) {
      dont_freeze <- c(
        dont_freeze,
        ifelse(grepl("=" , AllInputs()[[paste0("textin_relations_complete_", loop_approx)]]), 1, 0)
      )
    }
    
    if(AllInputs()$textin_relations_1 != ""){
      matrix_approx[(dont_freeze != 1), 2] <- NA
      matrix_approx[(dont_freeze == 1), 2] =
        ifelse(is.na(matrix_approx[(dont_freeze == 1), 2]) == T,0,matrix_approx[(dont_freeze == 1), 2])
      
    }
    
    output$mytable_approx <- renderRHandsontable({
      rhandsontable(matrix_approx, rowHeaders = F, height = 200, stretchH = "all") %>%
        hot_row(which(dont_freeze != 1), readOnly = TRUE) %>%
        hot_col("Models", readOnly = TRUE) %>%
        hot_col("Approximate parameter value", type = "numeric", halign = "htCenter", format = "0") %>%
        hot_validate_numeric(col = "Approximate parameter value", min = 0, max = 1, allowInvalid = TRUE) %>%
        hot_context_menu(
          allowRowEdit = F,
          allowColEdit = F
        )
    })
    
    
    showModal(modalDialog(
      h3("Settings for approximate equalities"),
      helpText(HTML("<i>Parameter values can be made approximately identical by changing the value from zero. For example, .05 means... <br> ... p1 = .5 will be set to p1 ≤  .55 and p1 ≥ .45, <br> ...  p1 = 1 will be set to p1 ≤ 1 and p1 ≥ .95,  <br> ... p1 = p2 will be set to p1 - p2 ≤ .05 and  - p1 + p2 ≤  .05, <br> ... p1 = p2 = .5 will be set to p1 - p2 ≤ .05,  - p1 + p2 ≤  .05, p2 ≤ .55, and p2 ≥  .45.</i>")),
      hr(),
      rHandsontableOutput("mytable_approx"),
      footer = tagList(
        actionBttn("submit_approx_equal", "Submit", style = "material-flat", color = "primary", size = "xs")
      ),
      easyClose = FALSE,
      fade = T
    ))
  })
  
  observeEvent(input$submit_approx_equal, {
    removeModal()
    n <- counter_input$n
    
    mytable_approx_reactive$value <- hot_to_r(input$mytable_approx)
    
    if (upload_reactive$value == 1 & sum(mytable_v_reactive$value[, 2]) > 0) {
      upload_reactive$value <- 0
      click("show_v_rep")
    }
  })
  
  ##### Settings for V-representation ####
  
  observeEvent(input$show_v_rep, {
    n_input_models <- (counter_input$n)
    
    matrix_v <- mytable_v_reactive$value
    
    if (is.null(matrix_v) == T) {
      matrix_v <- data.frame(
        c(unlist(eval(parse(text = paste("c(", paste("AllInputs()[", "'textin_relations_name", 1:counter_input$n, "']", sep = "", collapse = ","), ")"))))),
        rep(F, n_input_models)
      )
    }
    
    if ((nrow(matrix_v) != n_input_models)) {
      if (n_input_models > nrow(matrix_v)) {
        matrix_v <- data.frame(
          c(unlist(eval(parse(text = paste("c(", paste("AllInputs()[", "'textin_relations_name", 1:counter_input$n, "']", sep = "", collapse = ","), ")"))))),
          c(matrix_v$`Include V-representation`, rep(F, n_input_models - nrow(matrix_v)))
        )
      } else {
        matrix_v <- data.frame(
          c(unlist(eval(parse(text = paste("c(", paste("AllInputs()[", "'textin_relations_name", 1:counter_input$n, "']", sep = "", collapse = ","), ")"))))),
          (matrix_v$`Include V-representation`)[1:n_input_models]
        )
      }
    }
    
    colnames(matrix_v) <- c("Model name", "Include V-representation")
    
    name_models <- numeric()
    
    for (loop_save in 1:counter_input$n) {
      name_models <- c(name_models, eval(parse(text = paste("AllInputs()$textin_relations_name", loop_save, sep = ""))))
    }
    
    matrix_v$`Model name` <- name_models
    
    if (is.null(matrix_v) == F) {
      matrix_v[matrix_v[, 1] %in% names(v_reactive), 2] <- TRUE
    }
    
    output$mytable_v <- renderRHandsontable({
      rhandsontable(matrix_v, rowHeaders = F, height = 400, stretchH = "all") %>%
        hot_col("Model name", readOnly = TRUE) %>%
        hot_col("Include V-representation", halign = "htCenter") %>%
        hot_context_menu(
          allowRowEdit = F,
          allowColEdit = F
        )
    })
    
    showModal(modalDialog(
      h3("Settings for V-representation(s)"),
      hr(),
      helpText("Specify the models for which you want to build a V-representation by selecting the appropriate checkbox."),
      hr(),
      rHandsontableOutput("mytable_v"),
      footer = tagList(
        actionBttn("remove_all_V", "Remove all V-representations", style = "material-flat", color = "default", size = "xs"),
        actionBttn("check_all_V", paste("Include all", n_input_models, "V-representations"), style = "material-flat", color = "primary", size = "xs"),
        actionBttn("submit_v_repr", "Submit", style = "material-flat", color = "primary", size = "xs")
      ),
      easyClose = FALSE,
      fade = T
    ))
  })
  
  observeEvent(input$check_all_V, {
    matrix_v <- hot_to_r(input$mytable_v)
    
    matrix_v[, 2] <- T
    
    output$mytable_v <- renderRHandsontable({
      rhandsontable(matrix_v, rowHeaders = F, height = 400, stretchH = "all") %>%
        hot_col("Model name", readOnly = TRUE) %>%
        hot_col("Include V-representation", halign = "htCenter") %>%
        hot_context_menu(
          allowRowEdit = F,
          allowColEdit = F
        )
    })
  })
  
  observeEvent(input$remove_all_V, {
    matrix_v <- hot_to_r(input$mytable_v)
    
    matrix_v[, 2] <- F
    
    output$mytable_v <- renderRHandsontable({
      rhandsontable(matrix_v, rowHeaders = F, height = 400, stretchH = "all") %>%
        hot_col("Model name", readOnly = TRUE) %>%
        hot_col("Include V-representation", halign = "htCenter") %>%
        hot_context_menu(
          allowRowEdit = F,
          allowColEdit = F
        )
    })
  })
  
  observeEvent(input$submit_v_repr, {
    matrix_v <- hot_to_r(input$mytable_v)
    n_total_v <- sum(matrix_v[, 2])
    
    
    if (n_total_v > 0) {
      updateActionButton(session, "show_v_rep",
                         label = paste("V-representation(s), n = ", n_total_v, sep = "")
      )
    } else {
      updateActionButton(session, "show_v_rep",
                         label = "V-representation(s)"
      )
    }
    
    
    
    mytable_v_reactive$value <- hot_to_r(input$mytable_v)
  })
  
  ##### Settings for Volume #####
  
  observeEvent(input$show, {
    input_volume <- isolate(input_volume_reactive)
    
    showModal(modalDialog(
      helpText(
        "Consult the description of the settings of the function 'volume' in the",
        tags$a(href = "https://cran.r-project.org/web/packages/volesti/volesti.pdf", "manual", target = "_blank"),
        "of the package 'volesti' for details."
      ),
      tags$h3("Settings for 'Cooling Bodies'"),
      textInput("error_cb", "A numeric value to set the upper bound for the approximation error", value = input_volume$value[1]),
      pickerInput("random_walk_cb", "Random walk method",
                  choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk", "Billiard Walk"),
                  selected = input_volume$value[2]
      ),
      textInput("walk_length_cb",
                "An integer to set the number of the steps for the random walk",
                value = input_volume$value[3]
      ),
      textInput("win_len_cb",
                "The length of the sliding window",
                value = input_volume$value[4]
      ),
      pickerInput("hpoly_cb", "A boolean parameter to use H-polytopes in MMC when the input polytope is a zonotope",
                  choices = c("default", "TRUE", "FALSE"),
                  selected = input_volume$value[5]
      ),
      textInput("seed_cb",
                "A fixed seed for the number generator",
                value = input_volume$value[6]
      ),
      tags$h3("Settings for 'Sequence of Balls'"),
      textInput("error_sob", "A numeric value to set the upper bound for the approximation error", value = input_volume$value[7]),
      pickerInput("random_walk_sob", "Random walk method",
                  choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk", "Billiard Walk"),
                  selected = input_volume$value[8]
      ),
      textInput("walk_length_sob",
                "An integer to set the number of the steps for the random walk",
                value = input_volume$value[9]
      ),
      textInput("seed_sob",
                "A fixed seed for the number generator",
                value = input_volume$value[10]
      ),
      tags$h3("Settings for 'Cooling Gaussian'"),
      textInput("error_cg", "A numeric value to set the upper bound for the approximation error", value = input_volume$value[11]),
      pickerInput("random_walk_cg", "Random walk method",
                  choices = c("default", "Coordinate Directions Hit-and-Run", "Random Directions Hit-and-Run", "Ball Walk"),
                  selected = input_volume$value[12]
      ),
      textInput("walk_length_cg",
                "An integer to set the number of the steps for the random walk",
                value = input_volume$value[13]
      ),
      textInput("win_len_cg",
                "The length of the sliding window for Cooling Gaussian algorithm",
                value = input_volume$value[14]
      ),
      textInput("seed_cg",
                "A fixed seed for the number generator",
                value = input_volume$value[15]
      ),
      footer = tagList(
        actionBttn("submit", "Submit", style = "material-flat", color = "primary", size = "xs")
      ),
      easyClose = TRUE,
      fade = T
    ))
  })
  
  observeEvent(input$submit, {
    removeModal()
    
    input_volume_reactive$value[1] <- isolate(input$error_cb)
    input_volume_reactive$value[2] <- isolate(input$random_walk_cb)
    input_volume_reactive$value[3] <- isolate(input$walk_length_cb)
    input_volume_reactive$value[4] <- isolate(input$win_len_cb)
    input_volume_reactive$value[5] <- isolate(input$hpoly_cb)
    input_volume_reactive$value[6] <- isolate(input$seed_cb)
    
    input_volume_reactive$value[7] <- isolate(input$error_sob)
    input_volume_reactive$value[8] <- isolate(input$random_walk_sob)
    input_volume_reactive$value[9] <- isolate(input$walk_length_sob)
    input_volume_reactive$value[10] <- isolate(input$seed_sob)
    
    input_volume_reactive$value[11] <- isolate(input$error_cg)
    input_volume_reactive$value[12] <- isolate(input$random_walk_cg)
    input_volume_reactive$value[13] <- isolate(input$walk_length_cg)
    input_volume_reactive$value[14] <- isolate(input$win_len_cg)
    input_volume_reactive$value[15] <- isolate(input$seed_cg)
  })
  
  #### Plot examples ####
  
  observeEvent(input$go_example, {
    output$plot_example <- renderPlotly({
      select_v <- input$go_example
      
      select_v <- ifelse(select_v == "NA", names(v_reactive)[1], select_v)
      
      v_pl <- q2d(v_reactive[[select_v]]$output)
      
      parameter_names <- colnames(v_pl)[3:ncol(v_pl)]
      
      v_pl <- data.frame(
        rep(parameter_names, nrow(v_pl)),
        as.numeric(t(v_pl[, 3:ncol(v_pl)])),
        rep(1:nrow(v_pl),
            each =
              length(parameter_names)
        )
      )
      
      colnames(v_pl) <- c("parameter_names", "parameters", "vert_no")
      
      p <- ggplot(v_pl, aes(parameter_names, parameters)) +
        geom_point(aes(frame = vert_no), size = 4) +
        xlab("Parameters") +
        ylab("Parameter values") +
        scale_y_continuous(breaks = (seq(0, 1, by = 0.1)))
      
      p <- ggplotly(p, height = 600) %>% animation_opts(transition = 0)
      p <- p %>%
        animation_slider(
          currentvalue = list(prefix = "Vertex ")
        )
    })
  })
})

shinyApp(ui, server)
