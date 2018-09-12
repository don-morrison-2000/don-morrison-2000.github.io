#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# source('D:/CRTI/r_projects/shinyapp/chicago_tree_ui/app.R')
# runApp('chicago_tree_ui')

#
# To run from an R command line
# library(shiny)
# shiny::runGitHub('don-morrison-2000.github.io','don-morrison-2000', subdir='source/shiny/chicago_tree_ui/')

# Load packages
library(shiny)
library(shinythemes)
library(shinyjs)
library(ggplot2)
library(foreign)
library(nnet)
library(reshape2)
library(stringr)
library(Hmisc)


# Set this to 1 for fast multinomial testing (100 for production)
TEST <- 0
#MAX_ITER <- ifelse (TEST==0, 100, 1)
TOP_TEN <- ifelse (TEST==0, 10, 3)
#DO_P_VALUES <- ifelse (TEST==0, TRUE, FALSE)
DO_SCALE <- ifelse(TEST==0, TRUE, TRUE)
MAX_ABUNDANCE_LEVEL <- 10


g_land_use <- read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)
g_common_species <- sort(unique(read.delim('https://don-morrison-2000.github.io/data/common_species.csv', as.is=TRUE, header=FALSE)$V1))
g_species_sets <- c("Top 10 species", "Top 10 genera", "Common species")

# Define all possible predictors then subset to just the ones that show up in the input data
g_all_predictors_quantitative <- c (
      'Trunk diameter' =           'DBH_IN', 
      'Crown compactness' =        'CROWN_COMPACTNESS',
      'Max Height' =               'HEIGHT_MAX',
      'Mean Height' =              'HEIGHT_MEAN',
      'Crown area' =               'CROWN_AREA',
      'Building age' =             'BLDG_AGE',
      'Housing density' =          'HU_DENS',
      'Relative border' =          'RELBORDER_TREE',
      'NDVI max' =                 'NDVI_MAX',
      'NDVI mean' =                'NDVI_MEAN',
      'NDVI std'=                  'NDVI_STD'
      #      'Patch density' =            'PD',
      #      'Largest patch index' =      'LPI',
      #      'Landscape shape index' =    'LSI',
      #      'Distance to water' =        'DIST_WATER',
      #      'Distance to road (minor)' = 'DIST_MINOR',
      #      'Distance to road (major)' = 'DIST_MAJOR',
)
g_all_predictors_quantitative <- g_all_predictors_quantitative[order(names(g_all_predictors_quantitative))]

g_all_predictors_categorical <- c('Land use' = 'LU')
g_all_predictors <- c(g_all_predictors_quantitative, g_all_predictors_categorical)

#g_data_descriptors_local_file <<- paste(getwd(), '/', 'data_descriptors.rds', sep='') # Warning - getcwd() returns different results when call from "source"vs "runApp"
g_data_descriptors_local_file <<- 'D:/CRTI/r_projects/shinyapp/chicago_tree_ui/data_descriptors.rds'

g_data_descriptors_http_url <- 'https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/data_descriptors.rds'
g_data_descriptors <- list()
if (file.exists(g_data_descriptors_local_file))
{
      g_data_descriptors <- list(readRDS(g_data_descriptors_local_file))[[1]]
} else
{
      g_data_descriptors <- list(readRDS(gzcon(url(g_data_descriptors_http_url))))[[1]]
}
#g_default_data_descriptor <- g_data_descriptors[[1]]

g_label_font <- element_text(family="sans", color='black', size=16)
g_data_font <- element_text(family="sans", face="bold", color='black', size=12)
g_hr <- tags$hr(style="height:1px;border:none;color:#333;background-color:#333;")


read_data <- function (fn)
{
      # Read in the data
      ctree <- read.delim(fn, as.is=TRUE, sep=',') 
      # Remove extraneous columns
      extra <- setdiff(names(ctree), c('GENUSSPECI',g_all_predictors))
      ctree <- ctree[, !(names(ctree) %in% extra)]
      # Add in missing columns, initialized with zeros
      missing <- setdiff(c('GENUSSPECI',g_all_predictors),names(ctree))
      missing_df <- data.frame(matrix(nrow=nrow(ctree), ncol=length(missing),0))
      names(missing_df) <- missing
      ctree <- cbind(ctree, missing_df)
      # Arrange predictor columns in alphabetical order (no special reason for doing this)
      ctree <- ctree[, c('GENUSSPECI',sort(g_all_predictors))]
      # Convert model categories to factors that match the UI names
      ctree[['LU']] <- as.factor(g_land_use$V2[as.numeric(as.character(ctree$LU))])
      
      return (ctree)
}

# read_dataset <- function (dataset_name)
# {
#       if (is.null(g_data_descriptors[[dataset_name]]$ctree))
#       {
#             dataset_local_file  <- paste(getwd(), '/', dataset_name, '.csv', sep='')
#             dataset_http_url <- URLencode(paste('https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/', dataset_name, '.csv'))
#             g_data_descriptors[[dataset_name]]$ctree <<- if (file.exists(dataset_local_file)) read_data(dataset_local_file) else read_data(dataset_http_url)
#       }
#       return (g_data_descriptors[[dataset_name]]$ctree)
# }

read_dataset <- function (dataset_name)
{
      if (is.null(g_data_descriptors[[dataset_name]]$ctree))
      {
            fn <- paste(getwd(), '/', dataset_name, '.csv', sep='')
            if (!file.exists(fn))
            {
                  fn <- URLencode(paste('https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/', dataset_name, '.csv'))
            }
            g_data_descriptors[[dataset_name]]$ctree <<-  read_data(fn) 
      }
      return (g_data_descriptors[[dataset_name]]$ctree)
}


# Functions to allow prediction page to generated programatically
pred_row <- function (pred, data_descriptor_name) {return (list(cb(pred), sl(pred, data_descriptor_name)))}
cb <- function(pred) {checkboxInput (inputId = paste('predict_on_', pred, sep=''), label = strong(names(g_all_predictors_quantitative[g_all_predictors_quantitative==pred])), width = '600px', value = FALSE)} 
sl <- function(pred, data_descriptor_name) {conditionalPanel (condition = paste('input.predict_on_', pred, '==true', sep=''), sliderInput (inputId = paste('predict_', pred, sep=''), label = '', min = -999, max = -999, value = -999), g_hr)}



options(shiny.maxRequestSize=50*1024^2)

# Define UI
ui <- navbarPage("CRTI Tree Data", theme = shinytheme("cyborg"), selected = p(icon('eye-open', lib = "glyphicon"),'Model'),
                tags$head(tags$style(HTML("
                                          .shiny-notification {height:100px; width:600px; position:fixed; opacity:1.0; top:calc(50% - 50px);; left:calc(50% - 300px);;}
                                          .table.shiny-table > tbody > tr > td {padding-top: 0px; padding-bottom: 0px; line-height: 1;}
                                          .checkbox {margin-bottom: 0px;}
                                          .radio {margin-bottom: 0px;}
                                          "))),
                useShinyjs(),
                tabPanel(
                      title=p(icon('eye-open', lib = "glyphicon"),'Model'),
#                shinythemes::themeSelector(),
                            fluidRow
                            ( 
                                  column(3,selectInput(inputId = "data_descriptor_name", label = strong("Dataset"),  choices = names(g_data_descriptors), selected = names(g_data_descriptors)[1])),
                                  column(2,selectInput(inputId = "filter_species_set", label = strong("Species Set"),  choices = g_species_sets, selected = g_species_sets[1])),
                                  column(2,selectInput(inputId = "filter_species_set_others", label = strong("Include others"),  choices = c('Yes','No'), selected = c('Yes')))
                            ),
                            g_hr,
                            tabsetPanel
                            (
                                  selected = p(icon("stats", lib = "glyphicon"),'Species ocurrence probability'),
                      

                                  tabPanel
                                  (
                                        id="model",
                                        title=p(icon("stats", lib = "glyphicon"),'Species ocurrence probability'), 
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=8,
                                                    wellPanel
                                                    (  
                                                          width=8, style = "max-width: 800px", 
                                                          plotOutput(outputId = "probability_plot", height = '300px', dblclick = "plot_dblclick",  brush = brushOpts(id = "plot_brush", resetOnNew = TRUE))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=2,
                                                    wellPanel
                                                    (  
                                                          checkboxInput (inputId = "plot_stack", label = strong("Stack"), value=FALSE),
                                                          checkboxInput (inputId = "plot_observations", label = strong("Observations"), value=FALSE),
                                                          checkboxInput (inputId = "show_statistics", label = strong("Show statistics"), value=FALSE),
                                                          checkboxInput (inputId = "show_p_values", label = strong("Show p-values"), value=FALSE)
                                                    )
                                              )
                                        ),
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=12,
                                                    (
                                                          conditionalPanel
                                                          ( 
                                                                condition = 'input.show_statistics == true', 
                                                                wellPanel
                                                                (  
                                                                      title='Statistics',
                                                                      width=12, style = "overflow-y:scroll; max-height: 350px",
                                                                      tableOutput(outputId = "outText")
                                                                )
                                                          )
                                                    )
                                              )
                                        ),
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=12,
                                                    (
                                                          conditionalPanel
                                                          ( 
                                                                condition = 'input.show_p_values == true', 
                                                                wellPanel
                                                                (  
                                                                      title='p-Values',
                                                                      width=12, style = "overflow-y:scroll; max-height: 350px",
                                                                      tableOutput(outputId = "out_p_values")
                                                                )
                                                          )
                                                    )
                                              )
                                        ),
                                        
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    (  
                                                          radioButtons (inputId = "plot_predictor", label = strong("Plot Predictor"), choices = g_all_predictors_quantitative, selected = g_all_predictors_quantitative[1])
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    (  
#                                                          checkboxGroupInput (inputId = "land_use", label = strong("Land Use"), choices = g_default_data_descriptor$lu_cats, selected = g_default_data_descriptor$lu_cats),
                                                          checkboxGroupInput (inputId = "land_use", label = strong("Land Use"), choices = NULL, selected = NULL),
                                                          actionButton("land_use_none", label = strong("None")),
                                                          actionButton("land_use_all", label = strong("All"))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    ( 
#                                                          checkboxGroupInput (inputId = "species", label = strong("Species"), choices = g_default_data_descriptor$top_ten_species, selected = g_default_data_descriptor$top_ten_species[1]),
                                                          checkboxGroupInput (inputId = "species", label = strong("Species"), choices = NULL, selected = NULL),
                                                          actionButton("species_none", label = strong("None")),
                                                          actionButton("species_all", label = strong("All"))
                                                    )
                                              )
                                        )
                                        
                                  ),
                                  tabPanel
                                  (
                                        id="predict",
                                        title=p(icon("dashboard", lib = "glyphicon"),'Predict tree species'), 
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              (
                                                    width=3,
                                                    lapply (g_all_predictors_quantitative, pred_row, data_descriptor_name=NULL),
#                                                    lapply (g_all_predictors_quantitative, pred_row, data_descriptor_name=g_default_data_descriptor$name),
#                                                    selectInput("predict_LU", label = strong('Land Use'),  choices = g_default_data_descriptor$lu_cats, selected = "Street tree")
                                                    selectInput("predict_LU", label = strong('Land Use'),  choices = NULL, selected = NULL)
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=7,
                                              wellPanel
                                              (  
                                                    width=8, 
                                                    plotOutput(outputId = "out_piechart", height = "300px")
                                              ),
                                              wellPanel
                                              (  
                                                    width=5,
                                                    style="td line-height:0px; td padding-top: 0px, td padding-bottom: 0px; #shiny-html-output td color: #4d3a7d;",
                                                    tableOutput(outputId = "out_prediction")
                                              )
                                        )
                                  )
                            )
                  ),
                  tabPanel
                  (
                        title=p(icon('info-sign', lib = "glyphicon"),'Describe'),
                        fluidRow
                        ( 
                              column(3,selectInput(inputId = "ui_data_descriptor_name", label = strong("Dataset"),  choices = names(g_data_descriptors), selected = names(g_data_descriptors)[1]))
                        ),
                        g_hr,
                                    fluidRow (
                                          column (
                                                width=3,
                                                wellPanel (
                                                      sliderInput (inputId = "ui_abundance_level", label = strong("Abundance level"),  min=1, max=MAX_ABUNDANCE_LEVEL, value=4),
                                                      checkboxGroupInput (inputId = "ui_species", label = strong("Species"),  choices = "Acer platanoides", selected = "Acer platanoides"),
                                                      actionButton("ui_species_none", label = strong("None")),
                                                      actionButton("ui_species_all", label = strong("All"))
                                                )
                                          ),
                                          column (
                                                width=3,
                                                wellPanel (
                                                      selectInput (inputId = "ui_var", label = strong("Measurement"),  choices=g_all_predictors_quantitative, selected=names(g_all_predictors_quantitative)[1])
                                                ),
                                                wellPanel (
                                                      sliderInput (inputId = "ui_bins", label = strong("Quantiles"),  min=1, max=10, value=4)
                                                ),
                                                wellPanel (
                                                      checkboxGroupInput (inputId = "ui_land_use", label = strong("Land use"),  choices = NULL, selected = NULL),
#                                                      checkboxGroupInput (inputId = "ui_land_use", label = strong("Land use"),  choices = g_default_data_descriptor$main_lu_cats, selected = g_default_data_descriptor$main_lu_cats),
                                                      actionButton("ui_land_use_none", label = strong("None")),
                                                      actionButton("ui_land_use_all", label = strong("All"))
                                                )
                                          ),
                                          column (
                                                width=6,
                                                wellPanel (
                                                      style = "overflow-y:scroll; min-height: 300px; max-height: 1000px",
                                                      uiOutput(outputId = "ui_chart", width = '600px', height = "300px")
                                                ),
                                                fluidRow (
                                                      column (
                                                            width = 1,
                                                            actionButton("ui_flip_chart", label = strong("Flip"))
                                                      ) 
                                                      
                                                )
                                          )
                        )
                  ),



                 tabPanel
                 (
                       id="admin",
                       title=p(icon("cog", lib = "glyphicon"),'Admin'),

                       fluidRow (
                             column (
                                   width=9,
                                   wellPanel
                                   (  
                                         title='Datasets', width=3, 
                                         tableOutput(outputId = "admin_datasets")
                                   )
                             )
                       ),
                       g_hr,
                       
                       fluidRow (
                             column (
                                   width=3,
                                   selectInput (inputId = "admin_action_name", label = strong("Action"),  choices=c('Add dataset','Rebuild model', 'Delete dataset', 'Save configuration'), selected='Add dataset')
                             ),
                             column 
                             (
                                   width=6,
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Add dataset"', 
                                         (
                                               wellPanel
                                               (
                                                     textInput(inputId='admin_new_data_name', label='Name', value = "", width = '250px', placeholder = 'Specify the dataset display name'),
                                                     fileInput(inputId = 'admin_new_data_file', label='Data file', multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"), width = NULL, buttonLabel = "Browse", placeholder = "Specify cleaned .csv data file"),
                                                     g_hr,
                                                     actionButton(inputId = "admin_new_data_add", label = strong('Add'), icon("plus", lib = "glyphicon"))
                                                     
                                               )
                                         )
                                   ),
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Rebuild model"', 
                                         (
                                               wellPanel
                                               (
                                                     selectInput(inputId = "admin_update_name", label = 'Name',  choices = c('All', names(g_data_descriptors)), selected = if (length(g_data_descriptors) > 0) c('All',names(g_data_descriptors)[[1]]) else NULL, width = '200px'),
                                                     numericInput(inputId = "admin_update_iterations", label="Iterations", value=100, min = 1, max = 1000, width = '100px'),
                                                     checkboxInput (inputId = "admin_update_calculate_p_values", label = "Calculate p-values", value = FALSE),
                                                     g_hr,
                                                     actionButton(inputId="admin_update", label=strong("Rebuild"), icon("refresh", lib = "glyphicon"))
                                                )   
                                         )
                                   ),
                                   conditionalPanel
                                   ( 
                                         condition = 'input.admin_action_name == "Delete dataset"', 
                                         (
                                               wellPanel
                                               (
                                                     selectInput(inputId = "admin_delete_name", label = 'Name',  choices = names(g_data_descriptors), selected = if (length(g_data_descriptors) > 0) names(g_data_descriptors)[[1]] else NULL, width = '200px'),
                                                     g_hr,
                                                     actionButton(inputId="admin_delete", label=strong("Delete"), icon("trash", lib = "glyphicon"))
                                               )   
                                         )
                                   ),
                                   conditionalPanel 
                                   ( 
                                         condition = 'input.admin_action_name == "Save configuration"', 
                                         (
                                               wellPanel
                                               (
                                                     actionButton(inputId = "admin_save_configuration", label = strong('Save'), icon("save", lib = "glyphicon")) 
                                               )
                                         )
                                   )
                             )
                       )
                 )


)

# Define server function
server <- function(input, output, session) 
{
      


      ################################################################################################################
      # Reactive values
      ################################################################################################################

      r_values  <- reactiveValues(
                  species_names_all = NULL,                 # List of species name in the current "species set" (after filtering)  
                  x = NULL,                                 # Zoomable x coordinate
                  y = NULL,                                 # Zoomable y coordinate
                  run_predict_go = FALSE,                   # Flip the setting to trigger an update on the predictions
                  abundance_level = 0,
                  display_species_list = vector("character"),   # List of species to display
#                  selected_species_list = "Acer platanoides",  # Selected species (including those not dislayed)
                  selected_species_list = vector("character"),  # Selected species (including those not dislayed)
                  land_use_list = vector("character"),
                  selected_data_descriptor_name = NULL,          # Save state for selected data descriptor
                  flip_chart = TRUE,
#                  selected_land_use = g_default_data_descriptor$lu_cats,                       # Save state to be used when data_descriptor changes
                  selected_land_use = NA,                       # Save state to be used when data_descriptor changes
                  dataset_update = 0)
      

      
      
      get_model_hash_id <- function (data_descriptor_name,     # Data_descriptor name
                                   land_use,                 # Set of land use names
                                   species_set,              # Set of species (or genera) names
                                   model_predictors,         # Predictor variables (both quantitative and categorical)
                                   model_type,               # Model type (binomial or multinomial)
                                   species_set_others)       # Fold all other species into "other"?
      {
            return (digest::digest(paste(data_descriptor_name, land_use, species_set, model_predictors, model_type, species_set_others)))
      }
      

      
      ################################################################################################################
      # 
      # Model tab 
      # 
      ################################################################################################################  
      
      
      # Inputs:
      #     cf: coefficients matrix (including intercept) from multinomial model
      #     fullx: matrix - one row per prediction, one column per prediction variable
      #     ref_spp: reference species name for the species categorical prediction variable
      # Output:
      #     matrix of probabilites - one row for each requested prediction, one column per species (all rows add up to 1)
      do_predict_multinomial <- function (cf, fullx, ref_spp)
      {
            X <- as.matrix(cbind(1,fullx))
            cf <- t(cf)
            denom <- rep(1,nrow(X))
            for (i in seq(1,ncol(cf)))
            {
                  denom <- denom + exp(X %*% cf[,i])
            }
            result <- matrix(sapply(1:ncol(cf), function(i) exp(X %*% cf[,i]) / denom),ncol=ncol(cf))
            result <- cbind(1 - rowSums(result), result)
            colnames(result) <- c(ref_spp, colnames(cf))
            return (result)
      }
      
      
      # Inputs
      #     model: list of model-related objects
      #     m_preds_q_mx: matrix - quantitative predictor values matrix - one row per prediction, one column per quantitative prediction variable. 
      #     land_use: land use categories to include in the predictions
      # Output:
      #     matrix of predictions - one row per prediction, one column per species
      predict_multinomial <- function (model, m_preds_q_mx, land_use)
      {
            num_predictions <- nrow(m_preds_q_mx)
            prediction <- matrix(0, nrow=num_predictions, ncol=length(model$spps))
            colnames(prediction) <- model$spps
            pred_q_range <- g_data_descriptors[[input$data_descriptor_name]]$pred_q_range
            
            # Scale predictors from 0 to 1
            if (DO_SCALE)
            {
                  for (pred_name_q in colnames(m_preds_q_mx))
                  {
                        m_preds_q_mx[,pred_name_q] <- as.vector(scale(m_preds_q_mx[,pred_name_q], center=pred_q_range[pred_name_q,'min'], scale=pred_q_range[pred_name_q,'diff']))
                  }
                  m_preds_q_mx[!is.finite(m_preds_q_mx)]=0  #Handle the edge case where scale returns NAN or infinite because there is no variation
            }
            
            if (length(model$pred_c_spec) == 0 )
            {
                  prediction  <- do_predict_multinomial (cf, m_preds_q_mx, model$ref_spp)
            }
            else if (length(model$pred_c_spec) == 1 )
            {
                  for (pred_c_spec in model$pred_c_specs)
                  {
                        prediction_input <- cbind(m_preds_q_mx, matrix(NA, nrow=num_predictions, ncol=length(pred_c_spec$cat_cf_names)))
                        colnames (prediction_input) <- c(colnames(m_preds_q_mx), pred_c_spec$cat_cf_names)
                        
                        for (lu_cat in land_use)
                        {
                              prediction_input[,pred_c_spec$cat_cf_names] <- 0
                              if (lu_cat != pred_c_spec$cat_name_ref)
                              {
                                    prediction_input[,paste(pred_c_spec$cat_pred_name, lu_cat, sep='')] <- 1
                              }
                              p <- do_predict_multinomial (model$cf, prediction_input, model$ref_spp)
                              prediction <- prediction + p * pred_c_spec$cat_weights[lu_cat]
                        }   
                  }
            }
            else 
            {
                  return (NULL) # Can't handle multiple categorical predictors yets
            }
            
            # VERIFICATION - run the same prediction through the multinom model object
            all_pred_df <- data.frame(m_preds_q_mx[,g_all_predictors_quantitative, drop=FALSE])
            result_df2 <- data.frame (x=numeric(), y=numeric(), species=character())
            prediction2 <- matrix(0, nrow=num_predictions, ncol=length(model$spps))
            colnames(prediction2) <- model$spps
            for (lu_cat in land_use)
            {
                  all_pred_df$LU <- lu_cat
                  prediction2 <- prediction2 + predict(model$model_reduced, newdata=all_pred_df, type="probs", se=TRUE) * pred_c_spec$cat_weights[lu_cat]
            }
            if ((sum(abs(prediction - prediction2) > .000001)) > 0) 
            {
                  print ("Warning. Prediction mismatch")
            }
            
            return (prediction)
      }
      
      
      get_model <- reactive({
            r_values$dataset_update  #HACK
            if (is.null(g_data_descriptors[[input$data_descriptor_name]]$ctree))
            {
                  return (NULL)
            }

            filter_data <- filter_data()
            model_hash_id <- get_model_hash_id(input$data_descriptor_name, levels(filter_data$LU), input$filter_species_set, g_all_predictors, input$filter_model_type, input$filter_species_set_others)
            
            if (nrow(filter_data) == 0)
            {
                  return (NULL)
            }
            
            for (data_descriptor in g_data_descriptors)
            {
                  if (!is.null(data_descriptor$models[[model_hash_id]]) )
                  {
                        return (data_descriptor$models[[model_hash_id]])
                  }
            }
            return (NULL)
      })
      
      
      get_data_r <- reactive({
            ctree <- g_data_descriptors[[r_values$selected_data_descriptor_name]]$ctree
            if (is.null(ctree))
            {
                  withProgress (message='Reading dataset', value=0, max=1, {
                        incProgress (amount=.5, detail=r_values$selected_data_descriptor_name)
                        ctree <- read_dataset(r_values$selected_data_descriptor_name)
                  })
            }
            r_values$dataset_update <- r_values$dataset_update + 1
            return (ctree)
            
      })
      
      

      ################################################################################################################
      # 
      # Model -> Species occurrence tab
      # 
      ################################################################################################################
      
      
      get_regression_coords = function(filter_model_type, ctree, model, m_preds, p_pred, spps, land_use)
      {
            num_predictions = 101
            
            # Create a matrix with the means of all the quantitative predictors
            m_preds_q_means <- colMeans(ctree[g_all_predictors_quantitative], na.rm=TRUE)
            m_preds_q_mx <- t(matrix(rep(m_preds_q_means,num_predictions),nrow=length(g_all_predictors_quantitative)))
            colnames(m_preds_q_mx) <- g_all_predictors_quantitative
            m_preds_q_mx[,p_pred] <- seq(min(ctree[,p_pred],na.rm=TRUE),max(ctree[,p_pred],na.rm=TRUE),len=num_predictions)
            # Run the prediction and convert to long format
            result_mx <- predict_multinomial(model, m_preds_q_mx, land_use)
            result_df <- melt(as.data.frame(cbind(x=m_preds_q_mx[,p_pred], result_mx[,spps[spps %in% colnames(result_mx)],drop=FALSE])),id.vars='x')
            colnames(result_df) <- (c('x','species','y'))
            return (result_df[c('x','y','species')])
      }
      
      
      get_occurrence_coords <- function (ctree, spps, predictor)
      {
            num_bins = 20
            occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character()) 
            
            for (spp in spps)
            {
                  ctree[,'occur'] <- ifelse(ctree$GENUSSPECI==spp, 1,0)
                  bins <- unique(quantile(ctree[,predictor], prob=seq(0,1,len=num_bins),na.rm=TRUE))
                  num_bins <- length(bins)
                  if (num_bins > 1)
                  {
                        bins[num_bins] <- bins[num_bins] + 1                  ## Necessary so the highest x falls in the last bin
                        x_binned <- cut(ctree[,predictor], breaks=bins, right=FALSE)
                        
                        mean_x <- tapply(ctree[,predictor], x_binned, mean)
                        mean_y <- tapply(ctree$occur, x_binned, mean)
                        occurrence_coords <- rbind (occurrence_coords, data.frame(x=mean_x, y=mean_y, species=rep(spp, length(mean_x))))
                  }
                  else
                  {
                        occurrence_coords <- rbind (occurrence_coords, data.frame(x=0, y=0, spp))  # Edge case where there is no variation
                  }
            }
            return (occurrence_coords)
            
      }
      

      # Observe the filter tab's species list
      observeEvent (r_values$species_names_all, {
            species_intersect <- input$species[input$species %in% r_values$species_names_all]
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = (if (length(species_intersect)>0) species_intersect else r_values$species_names_all[1]))
      })
      
      
      # Observe a change to the data_descriptor name
      observeEvent (input$data_descriptor_name, {
            # TEST
#            read_dataset(input$data_descriptor_name)
#            r_values$dataset_update <- r_values$dataset_update + 1
            
            r_values$selected_data_descriptor_name <- input$data_descriptor_name
            get_data_r()
            
            model <- get_model()
            land_use_intersect <- if(length(r_values$selected_land_use)==1 && is.na(r_values$selected_land_use)) model$land_use else intersect(model$land_use, r_values$selected_land_use)
            new_land_use <- if (length(land_use_intersect)>0) land_use_intersect else model$land_use[1]
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = new_land_use)   
            r_values$selected_land_use <- new_land_use
      })

      # Observe a change to the selected land uses
      observeEvent (input$land_use, {
            r_values$selected_land_use <-input$land_use
      })
      
      # Observe the action button to select no land uses
      observeEvent (input$land_use_none, {
            model <- get_model()
            r_values$selected_land_use <- NULL
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = NULL)   
      })
      
      # Observe the action button to select all land uses
      observeEvent (input$land_use_all, {
            model <- get_model()
            r_values$selected_land_use <- model$land_use
            updateCheckboxGroupInput (session, "land_use", choices = model$land_use, selected = model$land_use)   
      })
      
      
      # Observe the action button to select no species
      observeEvent (input$species_none, {
            model <- get_model()
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = NULL)   
      })
      
      # Observe the action button to select all species
      observeEvent (input$species_all, {
            model <- get_model()
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = r_values$species_names_all)   
      })
      
      
      
      # Observe double clicks on the plot.
      # check if there's a brush on the plot. If so, zoom to the brush bounds; if not, reset the zoom.
      observeEvent(input$plot_dblclick, {
            brush <- input$plot_brush
            if (!is.null(brush)) {
                  r_values$x <- c(brush$xmin, brush$xmax)
                  r_values$y <- c(brush$ymin, brush$ymax)
                  
            } else {
                  r_values$x <- NULL
                  r_values$y <- NULL
            }
      })
   

      # Plot the probabilities
      output$probability_plot <- renderPlot({ 
            
            x=0
            model <- get_model()
            filter_data <- filter_data()

            # Make sure all paramters are set
            if (is.null(g_all_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% g_all_predictors) ||is.null(r_values$selected_land_use) || (sum(input$species %in% model$spps)==0) )
            {
                  return (NULL)     
            }
            regression_coords <- get_regression_coords(input$filter_model_type, filter_data, model, g_all_predictors, input$plot_predictor, input$species, r_values$selected_land_use)
            occurrence_coords <- get_occurrence_coords (filter_data, input$species, input$plot_predictor)
            
            p <- ggplot () +
                  scale_x_continuous(name=names(g_all_predictors_quantitative[g_all_predictors_quantitative==input$plot_predictor])) +
                  scale_y_continuous(limits=c(0,1.1), name="Probability of occurrence") +
                  theme(title = g_label_font, axis.title = g_label_font, axis.text = g_data_font, legend.title = g_label_font, legend.text = g_data_font) +
                  coord_cartesian(xlim = r_values$x, ylim = r_values$y, expand = FALSE)
            if (input$plot_stack == TRUE)
            {
                  if (input$plot_observations == FALSE)
                  {
                        p <- p + 
                              geom_area(data=regression_coords, position='stack', aes(x=x, y=y, group=species, fill=species)) +
                              scale_fill_discrete(name="Species")
                  }
                  else
                  {
                        p <- p + 
                              geom_line(data=regression_coords, position='stack', aes(x=x, y=y, group=species, colour=species)) +
                              scale_colour_discrete(name="Species") 
                  }
            }
            else
            {
                  p <- p + 
                        geom_line(data=regression_coords, aes(x=x, y=y, group=species, colour=species)) +
                        scale_colour_discrete(name="Species") 
            }
            if (input$plot_observations == TRUE)
            {
                  p <- p + geom_point(data=occurrence_coords, show.legend=FALSE, aes(x=x, y=y, group=species, colour=species))
            }
            return (p)
      })
      
      output$outText <- renderTable({ 
            
            model <- get_model()

            # Make sure all paramters are set
            if (is.null(model) || is.null(g_all_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% g_all_predictors))
            {
                  return (NULL)     
            }

            stats <- data.frame("Multinomial", model$sample_size, signif(model$aic,4), signif(model$r2,4))
            colnames(stats) <- c('Type', 'Samples', 'AIC', ' R**2')
            return (stats)
            
      })
      
  
      output$out_p_values <- renderTable({ 
            model <- get_model()
            if (is.null(model))
            {
                  return (NULL)     
            }            
            df <- as.data.frame(t(model$p_values))
            df <- df[,which(colnames(df) %in% input$species),drop=FALSE]
            if (ncol(df) > 0 && nrow(df) > 0)
            {
                  return (df)
            }
            else
            {
                  return (NULL)
            }
      }, rownames=TRUE, digits=3)
      
      
      
      ################################################################################################################
      # 
      # Model -> Predict tab
      # 
      ################################################################################################################
      
      
      # Observe updates to the checkboxes. When turned off, reset associated slider value to the mean
      lapply (X=g_all_predictors_quantitative, FUN=function (i)
      {
            observeEvent (input[[paste('predict_on_', i, sep='')]], {
                  check_box_id <- paste('predict_on_', i, sep='')
                  slider_id <- paste('predict_', i, sep='')
                  if (input[[check_box_id]] == FALSE)
                  {
                        new_val <- signif(mean(filter_data()[[i]],na.rm=TRUE),3)
                        updateSliderInput(session, slider_id, value = new_val)
                  }
            }, ignoreInit = TRUE)      
      })
      
      # Observe updates to the prediction sliders. 
      lapply (X=g_all_predictors_quantitative, FUN=function (i)
      {
            observeEvent (input[[paste('predict_', i, sep='')]], {
                  # Trigger the prediction update
                  r_values$run_predict_go <- !r_values$run_predict_go
            }, ignoreInit = TRUE)      
      })
      
      # Observe a change to the data_descriptor name - update the slide bar min/maxs and the land use selections
      observeEvent (input$data_descriptor_name, {
#            read_dataset(input$data_descriptor_name)
#            r_values$dataset_update <- r_values$dataset_update + 1
            
            r_values$selected_data_descriptor_name <- input$data_descriptor_name
            get_data_r()
            
            
            for (pred in g_all_predictors_quantitative)
            {
                  updateSliderInput(session, paste('predict_', pred, sep=''), step=(.1*(10^floor(log10(g_data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'diff'])))), min = floor(g_data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'min']), max = ceiling(g_data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'max']), value = if (input[[paste('predict_', pred, sep='')]]==-999) signif(g_data_descriptors[[input$data_descriptor_name]]$pred_q_range[pred,'mean'],3) else input[[paste('predict_', pred, sep='')]] )
            }
            updateSelectInput(session, 'predict_LU',  choices = g_data_descriptors[[input$data_descriptor_name]]$lu_cats, selected=if (input$predict_LU %in% g_data_descriptors[[input$data_descriptor_name]]$lu_cats) input$predict_LU else g_data_descriptors[[input$data_descriptor_name]]$lu_cats[1])
      })
      
      # This is triggered when the "run_predict_go" switch is flipped
      predict_go <- eventReactive(r_values$run_predict_go, {
            df <- data.frame(Predictor = character(), Value=numeric(), stringsAsFactors = FALSE)
            for (p in g_all_predictors_quantitative)
            {
                  if ( (input[[paste('predict_on_', p, sep='')]] == TRUE) && (p %in% g_all_predictors))
                  {
                        df <-rbind(df, data.frame(Predictor=p, Value=input[[paste('predict_',p,sep='')]], stringsAsFactors = FALSE))
                  }
                  else
                  {
                        df <-rbind(df, data.frame(Predictor=p, Value=mean(filter_data()[[p]],na.rm=TRUE), stringsAsFactors = FALSE))
                  }
            }
            colnames(df) <- c("Predictor", "Value")
            return (df)
      })
      
      
      get_species_prediction <- reactive ({
            model <- get_model()
            active_widgets <- predict_go()$Predictor %in% g_all_predictors
            model_input <- t(matrix(data=predict_go()$Value[active_widgets],dimnames=list(predict_go()$Predictor[active_widgets])))
            p <- predict_multinomial(model, model_input, input$predict_LU)
            lu_weight <- ifelse('LU' %in% g_all_predictors, model$pred_c_specs[['LU']]$cat_weights[[input$predict_LU]], 1)
            p <- data.frame(Probability=t(p/lu_weight))
            p$Species <- rownames(p)
            return (p)
      })
      
      output$out_prediction <- renderTable({ 
            if (!input$predict_LU %in% g_data_descriptors[[input$data_descriptor_name]]$lu_cats)
            {
                  return (NULL)
            }
            prediction <- get_species_prediction()
            prediction <- prediction[order(prediction$Probability, decreasing = TRUE),]
            return (rbind(prediction, data.frame(Species='Total', Probability=sum(prediction$Probability))))
      }, spacing='xs')

      output$out_piechart <- renderPlot({
            if (!input$predict_LU %in% g_data_descriptors[[input$data_descriptor_name]]$lu_cats)
            {
                  return (NULL)
            }
            prediction <- get_species_prediction()
             
             blank_theme <- theme_minimal()+
                   theme(
                         axis.title = g_label_font, axis.text = g_data_font, legend.title = g_label_font, legend.text = g_data_font,
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         panel.border = element_blank(),
                         panel.grid=element_blank(),
                         axis.ticks = element_blank()
                   )
             
             p <- ggplot (prediction) +
                   aes(x='', y=Probability, fill=Species) +
                   geom_bar(width = 1, stat = "identity") +
                   coord_polar("y", start=0) + 
                   blank_theme + 
                   theme(axis.text.x=element_blank()) 

             return (p)
       })

       
       
       
       ################################################################################################################
       # 
       # Describe species by frequency by landuse tab
       # 
       ################################################################################################################
       
       get_species <- function(ctree, lu_cats, abundance_level)
       {
             spps <- vector('character')
             top_spps <- g_data_descriptors[[input$ui_data_descriptor_name]]$top_spps
             for (lu_cat in lu_cats)
             {
                   spps <- unique(c(spps, na.omit(rownames(top_spps[top_spps[,lu_cat]<=abundance_level,,drop=FALSE]))))
             }
             return (sort(spps))
             
       }
       
       assign_quantiles <- function(full=g_data_descriptors[[input$ui_data_descriptor_name]]$ctree, var='HEIGHT_MEAN', num_bins=5)
       {
             # Calculate the break points - evenly spread acress the range
             val_range <- range(full[[var]], na.rm=TRUE)
             min <- as.integer(floor(val_range[1]))
             max <- as.integer(ceiling(val_range[2]))
             
             if (min == max)
             {
                   full$cat <- min   # Edge case where there is no variation
             }
             else if (num_bins == 1)
             {
                   full$cat[!is.na(full[[var]])] <- paste(min, '-' , max,sep='')
                   full$cat[is.na(full[[var]])] <- "Missing"
             }
             else
             {
                   # Bin the specified variable
                   full$cat <- cut2(full[[var]], g=num_bins, digits=3)
                   # Pretty up the level names
                   levels(full$cat) <- gsub(',','-',gsub('\\[||\\]||\\(||\\)','',levels(full$cat)))
             }
             return(full)
       }
       
       # Observe the abundance level slider
       observeEvent(input$ui_abundance_level, {
             r_values$abundance_level <- input$ui_abundance_level
             update_species_list()
       })
       
       # Observe the land use checkbox list
       observeEvent(input$ui_land_use, {
             r_values$land_use_list <- input$ui_land_use
             update_species_list()
       },ignoreNULL=FALSE)
       
       
       
       
       # Observe the button to clear the selected land use
       observeEvent(input$ui_land_use_none, {
             updateCheckboxGroupInput(session, "ui_land_use", choices = g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = NULL)
       })
       
       # Observe the button to select all land uses
       observeEvent(input$ui_land_use_all, {
             updateCheckboxGroupInput(session, "ui_land_use", choices = g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats)
       })
       
       # Observe the species checkbox list
       observeEvent(input$ui_species, {
             selected <- input$ui_species
             not_selected <- setdiff(r_values$display_species_list, selected)
             r_values$selected_species_list <- setdiff(unique(c(r_values$selected_species_list, selected)), not_selected)
       },ignoreNULL=FALSE)
       
       # Observe the button to clear the selected species
       observeEvent(input$ui_species_none, {
             r_values$selected_species_list <- NULL
             update_species_list()
       })

       # Observe the button to select all species
       observeEvent(input$ui_species_all, {
             r_values$selected_species_list <- r_values$display_species_list
             update_species_list()
       })
       
       # Observe the button to flip the chart
       observeEvent(input$ui_flip_chart, {
             r_values$flip_chart <- !r_values$flip_chart
       })
       
       update_species_list <- reactive({
             #            ctree <- current_data_descriptor$ctree
             ctree <- g_data_descriptors[[input$ui_data_descriptor_name]]$ctree
             #            r_values$display_species_list <- get_species(ctree, r_values$land_use_list, r_values$abundance_level)
             r_values$display_species_list <- get_species(ctree, Reduce (intersect,list(r_values$land_use_list, g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats )), r_values$abundance_level)
             updateCheckboxGroupInput(session, "ui_species", choices = r_values$display_species_list, selected = r_values$selected_species_list)
       })
       
       # Observe a change in the data_descriptor name
       observeEvent(input$ui_data_descriptor_name, {
             # TEST
#             read_dataset(input$ui_data_descriptor_name)
#             r_values$dataset_update <- r_values$dataset_update + 1
             
             
             r_values$selected_data_descriptor_name <- input$ui_data_descriptor_name
             get_data_r()
             
             
             update_species_list()
             updateCheckboxGroupInput(session, "ui_land_use", choices = g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats, selected = g_data_descriptors[[input$ui_data_descriptor_name]]$main_lu_cats)
       })
       
       # Plot the chart (note that this is wrapped by renderUI to allow the height to vary)
       output$contents <- renderPlot({ 
             var_descs <- g_all_predictors_quantitative
             # Keep only the trees in the requested set of land uses
             #            ctree <- current_data_descriptor$ctree
             ctree <- g_data_descriptors[[input$ui_data_descriptor_name]]$ctree
             ctree <- ctree[ctree$LU %in% r_values$land_use_list,]
             if(nrow(ctree)==0)
             {
                   return (NULL)
             }
             ctree$LU <- factor(ctree$LU)
             # Categorize the species by the requested variable
             ctree <- assign_quantiles (ctree, input$ui_var, input$ui_bins)
             
             # Create a data frame to collect the data
             df_cols <- c("Species", "LandUse", levels(ctree$cat))
             fits <- setNames(data.frame(matrix(ncol=length(df_cols), nrow = 0)), df_cols)
             for(sp in input$ui_species)
             {
                   ctree[,'occur'] = ifelse(ctree[,'GENUSSPECI']==sp, 1,0)
                   fit <- tapply(ctree$occur, list(ctree$LU, ctree$cat), mean, na.rm=TRUE)
                   fit <- cbind(Species=sp, LandUse=rownames(fit), as.data.frame(fit))
                   fits <- rbind(fits,fit) 
             }
             if (nrow(fits) == 0)
             {
                   return (NULL)
             }
             # Get the range for the y axis
             fits <- replace(fits,is.na(fits),0)
             yrange <- range(as.numeric(as.matrix(fits[3:ncol(fits)])),na.rm=TRUE)
             
             # Melt the dataframe to prepare it for plotting. This results in 4 columns: 1) Species, 2) LandUse, 3)Category, 4) fit value
             fits <- melt(fits, c(id="Species","LandUse"), variable.name="Category")
             
             if (r_values$flip_chart)
             {
                   # Plot the results
                   g <- ggplot(fits,aes(LandUse,value, fill=as.factor(Category))) +
                         geom_bar(position="dodge", stat="identity") +
                         facet_wrap(~Species, ncol=1) +
                         xlab('Land Use') +
                         ylab('Relative frequency') +
                         scale_fill_discrete(name=names(var_descs)[which(var_descs==input$ui_var)]) +
                         theme(axis.text.x=element_text(angle = -30, hjust = 0))
                   # Add vertical separator lines if more than one land use category
                   if (length(levels(fits$LandUse)) > 1)
                   {
                         g <- g + geom_vline(xintercept = seq(1.5,length(unique(fits$LandUse))-0.5,1))
                   }
             }
             else
             {
                   g <- ggplot(fits,aes(Category,value, fill=as.factor(LandUse))) +
                         geom_bar(position="dodge", stat="identity") +
                         facet_wrap(~Species, ncol=1) +
                         xlab(names(var_descs)[which(var_descs==input$ui_var)]) +
                         ylab('Relative frequency') +
                         scale_fill_discrete(name="Land use") +
                         theme(axis.text.x=element_text(angle = -30, hjust = 0))
                   # Add vertical separator lines if more than one measurement category
                   if (length(levels(fits$Category)) > 1)
                   {
                         g <- g + geom_vline(xintercept = seq(1.5,length(unique(fits$Category))-0.5,1))
                   }
             }
             return (g)
       })
       
       
       # Update the widget height to match the number of facets, then call the function that performs the plot
       output$ui_chart <- renderUI({
             height <- ifelse (length(input$ui_species)==0, 0, 200+(length(input$ui_species)-1)*100)
             plotOutput("contents", height = height, width = "100%")
       })
       
       
       
       ################################################################################################################
       # 
       # Admin tab
       # 
       ################################################################################################################
       
       # Build all the models for a single data_descriptor
       build_model_set <- function (data_descriptor_specs)
       {
             library(doParallel)
             library(foreach)
             cl<-makeCluster(min(detectCores(), length(data_descriptor_specs)))
             registerDoParallel(cl)

             # Make a local copy of global variables (required for parallel processing)
             DO_SCALE <- DO_SCALE
             all_predictors_quantitative <- g_all_predictors_quantitative


#            for (i in 1:length(data_descriptor_specs))
             model_set <- foreach (i=1:length(data_descriptor_specs), .packages='nnet') %dopar%
             {
                   spec <- data_descriptor_specs[[i]]

                   # Reduce categorical variables to the set that actually exist in the data
                   land_use <- levels(spec$data$LU)

                   pred_c_specs = list()
                   if ('LU' %in% spec$model_predictors)
                   {
                         cat_pred_name='LU'
                         cat_name_ref=land_use[1]
                         cat_names_non_ref=land_use[2:length(land_use)]
                         cat_cf_names=paste('LU',cat_names_non_ref,sep='')
                         cat_weights <- table(spec$data[[cat_pred_name]])/nrow(spec$data)
                         pred_c_specs[['LU']]  = list(cat_pred_name=cat_pred_name, cat_name_ref=cat_name_ref, cat_names_non_ref=cat_names_non_ref, cat_cf_names=cat_cf_names, cat_weights=cat_weights)
                   }

                   if (DO_SCALE)
                   {
                         for (pred_name_q in  all_predictors_quantitative)
                         {
                               spec$data[[pred_name_q]] <- as.vector(scale(spec$data[[pred_name_q]], center=spec$pred_q_range[pred_name_q,'min'], scale=spec$pred_q_range[pred_name_q,'diff']))
                               spec$data[[pred_name_q]] <- ifelse(is.nan(spec$data[[pred_name_q]]),0,spec$data[[pred_name_q]]) # Covers edge case where all numbers are the same and scale returns NaNs
                         }
                   }

                   predictors <- paste(spec$model_predictors,collapse='+')
                   model <- multinom(as.formula(paste('GENUSSPECI ~ ', predictors)), data=spec$data, maxit=spec$iterations)
                   model <- list(model_reduced=model,
                                 cf=coef(model, drop=FALSE),  # Warning - this returns a vector instead of a matrix if only 2 species exist
                                 aic=model$AIC,
                                 spps=model$lev,
                                 ref_spp=model$lev[1],
                                 sample_size=nrow(spec$data),
                                 land_use=land_use,
                                 pred_names_q=all_predictors_quantitative,
                                 pred_c_specs=pred_c_specs,
                                 model_hash_id=spec$model_hash_id)

                   model$p_values <- matrix()
                   if (spec$calculate_p_values)
                   {
                         z <- summary(model$model_reduced)$coefficients/summary(model$model_reduced)$standard.errors # from https://stats.stackexchange.com/questions/63222/getting-p-values-for-multinom-in-r-nnet-package
                         p_values <- (1 - pnorm(abs(z), 0, 1)) * 2
                         model$p_values <- p_values
                   }

                   r2 <- (1-(model$model_reduced$deviance/(update(model$model_reduced, . ~ 1,trace=F)$deviance)))
                   model$r2 <- r2

                   model$model_reduced$fitted.values <- NULL
                   model$model_reduced$residuals <- NULL
                   model$model_reduced$weights <- NULL
                   model$model_reduced$family <- NULL
                   model$model_reduced$na.action <- NULL
                   attr(model$model_reduced$terms,".Environment") =c()
                   attr(model$model_reduced$formula,".Environment") =c()
                   model
             }
             stopCluster(cl)
             return (model_set)
       }
        
       # Subset the data
       filter_data <- reactive({
             x=0
             data <- filter_data_x (g_data_descriptors[[input$data_descriptor_name]]$ctree,input$filter_species_set, input$filter_species_set_others)
             r_values$species_names_all <- levels(data$GENUSSPECI)
             return (data)
       })
       
#-----------------------------------------------------------------------------------------------------------
       get_top_spps <- function (ctree, lu_cats, limit)
       {
             top_spps_names = NULL
             top_spps_lu = list()
             # Get the top x species for each land use, plus combine them into a single list
             for (lu_cat in lu_cats)
             {
                   t <- sort(table(factor(ctree$GENUSSPECI[ctree$LU==lu_cat])), decreasing = TRUE)[1:limit,drop=FALSE]
                   for (i in seq(1:nrow(t))) {t[i]<-i}
                   top_spps_lu[[lu_cat]] <- t
                   top_spps_names <- unique(c(top_spps_names, names(t)))
             }
             
             top_spps <- matrix(NA, nrow=length(top_spps_names), ncol=length(lu_cats))
             rownames(top_spps) <- top_spps_names
             colnames(top_spps) <- lu_cats
             
             # Sum up each abundant species across all of the land uses
             for (lu_cat in lu_cats)
             {
                   for (spp in names(top_spps_lu[[lu_cat]]))
                   {
                         top_spps[spp,lu_cat] = top_spps_lu[[lu_cat]][[spp]] + ifelse(is.na(top_spps[spp,lu_cat]), 0, top_spps[spp,lu_cat])
                   }
             }
             return (top_spps)
       }
       
       # Observe a request to add a new dataset
       observeEvent (input$admin_new_data_add, {
             if (input$admin_new_data_name == "") {
                   showNotification('Dataset name is blank', duration = NULL, closeButton = TRUE, type = 'default')
                   return (NULL)
             } 
             if (!is.null(g_data_descriptors[[input$admin_new_data_name]])) 
             {
                   showNotification(paste('Dataset', input$admin_new_data_name, 'already exists', sep=' '), duration = NULL, closeButton = TRUE, type = 'default')
                   return (NULL)
             }
             
             withProgress (message=paste("Adding new dataset", input$admin_new_data_file$name, sep=' '),  value=0, max=3, {
                   
                   data_descriptor = list()
                   incProgress(1, detail="Reading data")
                   data_descriptor$ctree <- read_data (input$admin_new_data_file$datapath) 
                  
                   incProgress(1, detail="Creating objects")
                   data_descriptor$name <- input$admin_new_data_name
                   data_descriptor$file_name <- paste(getwd(), '/', input$admin_new_data_name, '.csv', sep='')
                   data_descriptor$file_name_original <- input$admin_new_data_file$name
                   data_descriptor$hash <- digest::digest(data_descriptor$ctree)
                   data_descriptor$records <- nrow(data_descriptor$ctree)
                   data_descriptor$top_ten_species <- sort(names(head(sort(table(factor(data_descriptor$ctree[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))
                   data_descriptor$lu_cats <- levels(data_descriptor$ctree$LU)
                   data_descriptor$main_lu_cats <- names(which(table(data_descriptor$ctree$LU)>400))
                   data_descriptor$top_spps <- get_top_spps (data_descriptor$ctree, data_descriptor$main_lu_cats, MAX_ABUNDANCE_LEVEL)
                   data_descriptor$models <- NULL

                   data_descriptor$pred_q_range <- matrix(NA, nrow=length(g_all_predictors_quantitative), ncol=4)
                   dimnames(data_descriptor$pred_q_range) <- list(g_all_predictors_quantitative, c('min', 'max', 'diff', 'mean'))
                   for (predictor_quantitative in g_all_predictors_quantitative)
                   {
                         data_descriptor$pred_q_range[predictor_quantitative, 'min'] <- min(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                         data_descriptor$pred_q_range[predictor_quantitative, 'max'] <- max(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                         data_descriptor$pred_q_range[predictor_quantitative, 'diff'] <- data_descriptor$pred_q_range[predictor_quantitative, 'max'] - data_descriptor$pred_q_range[predictor_quantitative, 'min']
                         data_descriptor$pred_q_range[predictor_quantitative, 'mean'] <- mean(data_descriptor$ctree[[predictor_quantitative]], na.rm=TRUE)
                   }
                   # Add the new data descriptor to the global list
                   g_data_descriptors[[input$admin_new_data_name]] <<- data_descriptor
                  
                   updateSelectInput(session, inputId='admin_update_name', choices = c('All', names(g_data_descriptors)), selected = 'All')
                   updateSelectInput(session, inputId='admin_delete_name', choices = names(g_data_descriptors), selected = names(g_data_descriptors)[1])
                   
                   file.copy (input$admin_new_data_file$datapath, data_descriptor$file_name, overwrite=TRUE )
                  
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
             r_values$dataset_update <- r_values$dataset_update + 1
       })
       
       # Observe a request to delete a dataset
       observeEvent (input$admin_delete, {

             withProgress (message=paste("Adding new dataset", input$admin_delete_name, sep=' '),  value=0, max=1, {
                   incProgress(1, detail="Processing delete request")
                   # Delete the local file (if it exists)
                   if (file.exists(g_data_descriptors[[input$admin_delete_name]]$file_name))
                   {
                         file.remove (g_data_descriptors[[input$admin_delete_name]]$file_name)
                   }
                   # Remove data descriptor from the global list
                   g_data_descriptors[[input$admin_delete_name]] <<- NULL
                   updateSelectInput(session, inputId='admin_update_name', choices = c('All', names(g_data_descriptors)), selected = 'All')
                   updateSelectInput(session, inputId='admin_delete_name', choices = names(g_data_descriptors), selected = if(length(g_data_descriptors)>0) names(g_data_descriptors)[1] else NULL)                   
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
             r_values$dataset_update <- r_values$dataset_update + 1
       })
       
       
       filter_data_x <- function (ctree, filter_species_set, filter_species_set_others)
       {
             
             if (filter_species_set == 'Top 10 species')
             {
                   species_names_all <- sort(names(head(sort(table(factor(ctree[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))
             }
             else if (filter_species_set ==  'Top 10 genera')
             {
                   x=0
                   # Coerce all species names to genus only, then select the top 10
                   ctree[['GENUSSPECI']] <-  sapply(strsplit(ctree$GENUSSPECI," "),"[",1)
                   species_names_all <- sort(names(head(sort(table(factor( ctree[['GENUSSPECI']]  )), decreasing = TRUE), TOP_TEN)))
             }
             else 
             {
                   # Default to common species
                   species_names_all <- g_common_species
             }
             if (filter_species_set_others == 'Yes')
             {
                   # Coerce all non-common species names to "Other"
                   ctree[['GENUSSPECI']] <- ifelse ((match(ctree[['GENUSSPECI']], species_names_all, nomatch = 0) > 0), ctree[['GENUSSPECI']], "Other")
                   species_names_all <- c(species_names_all, "Other")
             }
             ctree <- subset(ctree, GENUSSPECI %in% species_names_all)
             ctree$LU <- factor(ctree$LU)
             ctree$GENUSSPECI <- factor(ctree$GENUSSPECI)
             return (ctree)
       }
       
       
       # Observe the rebuild models button - PARALLEL
       observeEvent (input$admin_update, {
             
             dataset_names <- if (input$admin_update_name == 'All') names(g_data_descriptors) else input$admin_update_name
             
             models <<- list()
             # Collect all of the parameters up front
             withProgress (message='Building multinomial models', value=.1, max=length(dataset_names)+.1, {
                   for (dataset_name in dataset_names)
                   {
                         incProgress (amount=0, detail=dataset_name)
#                        # Refresh the data
                         g_data_descriptors[[r_values$selected_data_descriptor_name]]$ctree <<- NULL
                         r_values$selected_data_descriptor_name <- dataset_name
                         ctree <-get_data_r()
                         
                         data_specs <- list()
                         for (species_set in g_species_sets)
                         {
                               for (others in c('Yes', 'No'))
                               {
                                     spec <- list()
#                                     spec$data <- filter_data_x (g_data_descriptors[[dataset_name]]$ctree, species_set, others)
                                     
                                     # Filter data for model input
                                     spec$data <- ctree
                                     if (species_set == 'Top 10 species')
                                     {
                                           species_names_all <- sort(names(head(sort(table(factor(spec$data[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))
                                     }
                                     else if (species_set ==  'Top 10 genera')
                                     {
                                           # Coerce all species names to genus only, then select the top 10
                                           spec$data[['GENUSSPECI']] <-  sapply(strsplit(spec$data$GENUSSPECI," "),"[",1)
                                           species_names_all <- sort(names(head(sort(table(factor( spec$data[['GENUSSPECI']]  )), decreasing = TRUE), TOP_TEN)))
                                     }
                                     else 
                                     {
                                           # Default to common species
                                           species_names_all <- g_common_species
                                     }
                                     if (others == 'Yes')
                                     {
                                           # Coerce all non-common species names to "Other"
                                           spec$data[['GENUSSPECI']] <- ifelse ((match(spec$data[['GENUSSPECI']], species_names_all, nomatch = 0) > 0), spec$data[['GENUSSPECI']], "Other")
                                           species_names_all <- c(species_names_all, "Other")
                                     }
                                     spec$data <- subset(spec$data, GENUSSPECI %in% species_names_all)
                                     spec$data$LU <- factor(spec$data$LU)
                                     spec$data$GENUSSPECI <- factor(spec$data$GENUSSPECI)
                                     
                                     spec$model_predictors <- g_all_predictors
                                     spec$pred_q_range <- g_data_descriptors[[dataset_name]]$pred_q_range
                                     spec$calculate_p_values <- input$admin_update_calculate_p_values
                                     spec$iterations <- input$admin_update_iterations
                                     spec$model_hash_id <- get_model_hash_id(dataset_name, levels(spec$data$LU), species_set, g_all_predictors, input$filter_model_type, others) 
                                     data_specs <- append(data_specs, list(spec))
                               }
                         }
                         for (model in build_model_set(data_specs))
                         {
                               models[[model$model_hash_id]] <<- model
                         }
                         g_data_descriptors[[dataset_name]]$models <<- models
                         incProgress (amount=1)
                   }
             })
             r_values$dataset_update <- r_values$dataset_update + 1
       })
       
       
       
       # Observe a request to save the current configuration to disk
       observeEvent (input$admin_save_configuration, {
             withProgress (message=paste("Saving datasets and models to disk", g_data_descriptors_local_file, sep=' '), value=0, max=1, {
                   # Remove tree data to reduce size of saved object. The tree data will be merged back in on demand
                   for (name in names(g_data_descriptors))
                   {
                         g_data_descriptors[[name]]$ctree <- NULL
                   }
                   incProgress(.2, detail=g_data_descriptors_local_file)
                   saveRDS (g_data_descriptors, file=g_data_descriptors_local_file)
                   incProgress(1, detail="Success")
                   Sys.sleep(2)
             })
             r_values$dataset_update <- r_values$dataset_update + 1
       })
       
       # Output a summary table of dataset/model status
       output$admin_datasets <- renderTable(digits=0, { 
             r_values$dataset_update
             
             if(length(g_data_descriptors) == 0) 
             {
                   return (NULL)
             }

             model_status <- character()
             data_load_status <- character()
             records <- numeric()
             for (data_descriptor in g_data_descriptors)
             {
                   if (is.null(data_descriptor$models))
                   {
                         model_status <- c(model_status, "Rebuld required")
                   }
                   else
                   {
                         model_status <- c(model_status, 'OK')
                   }
                   data_load_status <- c(data_load_status, if (is.null(nrow(data_descriptor$ctree))) "Pending" else "OK")
                   records <- c(records, data_descriptor$records)
             }
             df <- data.frame(names(g_data_descriptors), records, data_load_status, model_status)
             colnames(df) <- c('Name', 'Records', 'Data Load', 'Model Status')
             return (df)
             
       })
}

# Create Shiny object
shinyApp(ui = ui, server = server)


