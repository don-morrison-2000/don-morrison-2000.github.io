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


# Set this to 1 for fast multinomial testing (100 for production)
test <- 0
MAX_ITER = ifelse (test==0, 100, 1)
TOP_TEN = ifelse (test==0, 10, 2)

ctree_local_file <- 'D:/CRTI/data/cleaned/dupage_county_accepted_V4.csv'
ctree_http_url <- 'https://don-morrison-2000.github.io/data/dupage_county_accepted_V4.csv'

# Read in the tree data (either from local disk or the http server)
if (file.exists(ctree_local_file))
{
      ctree <- read.delim(ctree_local_file, as.is=TRUE, sep=',') 
} else 
{
      ctree <- read.delim(ctree_http_url, as.is=TRUE, sep=',')
}

land_use <- read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)
common_species <- sort(unique(read.delim('https://don-morrison-2000.github.io/data/common_species.csv', as.is=TRUE, header=FALSE)$V1))
top_ten_species <- sort(names(head(sort(table(factor(ctree[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))

# Convert model categories to factors that match the UI names
ctree[['LU']] <- as.factor(land_use$V2[as.numeric(as.character(ctree$LU))])

species_sets <- c("Top 10 species", "Top 10 genera", "Common species")

# Define all possible predictors then subset to just the ones that show up in the input data
all_predictors_quantitative <- c (
      'Trunk diameter' =           'DBH_IN', 
      'Crown compactness' =        'CROWN_COMPACTNESS',
      'Max Height' =               'HEIGHT_MAX',
      'Mean Height' =              'HEIGHT_MEAN',
      'Crown area' =               'CROWN_AREA',
      'Building age' =             'BLDG_AGE',
      'Housing density' =          'HU_DENS',
      'Relative border' =          'RELBORDER_TREE',
      'Patch density' =            'PD',
      'Largest patch index' =      'LPI',
      'Landscape shape index' =    'LSI',
      'Distance to water' =        'DIST_WATER',
      'Distance to road (minor)' = 'DIST_MINOR',
      'Distance to road (major)' = 'DIST_MAJOR'
) 
all_predictors_quantitative <- all_predictors_quantitative[all_predictors_quantitative %in% colnames(ctree)]
all_predictors_quantitative <- all_predictors_quantitative[order(names(all_predictors_quantitative))]

all_predictors_categorical <- c('Land use' = 'LU')
all_predictors <- c(all_predictors_quantitative, all_predictors_categorical)


# Model cache
models_local_file <- paste(getwd(), '/', 'models.rds', sep='')
models_http_url <- 'https://don-morrison-2000.github.io/source/shiny/chicago_tree_ui/models.rds'
if (file.exists(models_local_file))
{
      g_models <- list(readRDS(models_local_file))[[1]]
} else
{
      g_models <- list(readRDS(gzcon(url(models_http_url))))[[1]]
}

# Functions to allow prediction page to generated programatically
pred_row <- function (pred) {return (list(cb(pred), sl(pred)))}
hr <- tags$hr(style="height:1px;border:none;color:#333;background-color:#333;")
cb <- function(pred) {checkboxInput (inputId = paste('predict_on_', pred, sep=''), label = strong(names(all_predictors_quantitative[all_predictors_quantitative==pred])), width = '600px', value = FALSE)} 
sl <- function(pred) {conditionalPanel (condition = paste('input.predict_on_', pred, '==true', sep=''), sliderInput (inputId = paste('predict_', pred, sep=''), label = '', step=(.1*(10^floor(log10(diff(range(ctree[[pred]])))))), min = floor(min(ctree[[pred]],na.rm=TRUE)), max = ceiling(max(ctree[[pred]],na.rm=TRUE)), value = signif(mean(ctree[[pred]],na.rm=TRUE),3)), hr)}


label_font <- element_text(family="sans", color='black', size=16)
data_font <- element_text(family="sans", face="bold", color='black', size=12)

# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
                tags$head(tags$style(HTML(".shiny-notification {height:100px; width:600px; position:fixed; opacity:1.0; top:calc(50% - 50px);; left:calc(50% - 300px);;}"))),
                useShinyjs(),
                titlePanel("DuPage County Tree Data"),
#                shinythemes::themeSelector(),
                fluidRow 
                (
                      column 
                      ( 
                            width=12,
                            tabsetPanel
                            (
                                  selected = 'Model',
                                  tabPanel
                                  (
                                        title='Data',
                                        
                                        
                                        
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    ( 
                                                          radioButtons (inputId = "filter_species_set", label = strong("Species Set"), choices = species_sets, selected = species_sets[1]),
                                                          checkboxInput (inputId = "filter_species_set_others", label = "Include others", value = TRUE),
                                                          hr,
                                                          checkboxInput (inputId = "filter_advanced", label = "Advanced filters", value = FALSE)
                                                    )
                                              )
                                        ),
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=3,
                                                    conditionalPanel
                                                    ( 
                                                          condition = 'input.filter_advanced == true', 
                                                          wellPanel
                                                          ( 
                                                                checkboxGroupInput (inputId = "filter_land_use", label = strong("Land Use"), choices = levels(ctree$LU), selected = levels(ctree$LU))
                                                          )
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    conditionalPanel
                                                    ( 
                                                          condition = 'input.filter_advanced == true', 
                                                          wellPanel
                                                          (  
                                                                checkboxGroupInput (inputId = "filter_model_predictors", label = strong("Model Predictors"), choices = all_predictors, selected = all_predictors)
                                                          )
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=2,
                                                    conditionalPanel
                                                    ( 
                                                          condition = 'input.filter_advanced == true', 
                                                          wellPanel
                                                          ( 
                                                                actionButton(inputId = "save_models", label = strong('Save models'))
                                                          )
                                                    )
                                              )
                                        )
                                        
                                  ),
                                  

                                  tabPanel
                                  (
                                        title='Model',
                                        fluidRow 
                                        (
                                              column 
                                              ( 
                                                    width=8,
                                                    wellPanel
                                                    (  
                                                          width=8, style = "overflow-y:scroll; min-height: 350px; max-height: 350px",
                                                          plotOutput(outputId = "probability_plot", width = '600px', height = "300px", dblclick = "plot_dblclick",  brush = brushOpts(id = "plot_brush", resetOnNew = TRUE))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=2,
                                                    wellPanel
                                                    (  
                                                          checkboxInput (inputId = "plot_stack", label = strong("Stack"), value=FALSE),
                                                          checkboxInput (inputId = "plot_observations", label = strong("Observations"), value=FALSE),
                                                          checkboxInput (inputId = "show_statistics", label = strong("Show statistics"), value=FALSE)
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
                                                    width=3,
                                                    wellPanel
                                                    (  
                                                          radioButtons (inputId = "plot_predictor", label = strong("Plot Predictor"), choices = all_predictors_quantitative, selected = all_predictors_quantitative[1])
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    (  
                                                          checkboxGroupInput (inputId = "land_use", label = strong("Land Use"), choices = levels(ctree$LU), selected = levels(ctree$LU))
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    ( 
                                                          checkboxGroupInput (inputId = "species", label = strong("Species"), choices = top_ten_species, selected = top_ten_species[1])
                                                    )
                                              )
                                        )
                                        
                                  ),
                                  tabPanel
                                  (
                                        title='Predict',
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              (
                                                    width=3,
                                                    lapply (all_predictors_quantitative, function (i) {pred_row(i)}),
                                                    #selectInput("predict_LU", label = strong('Land Use'),  choices = levels(ctree$LU), selected = levels(ctree$LU)[1])
                                                    selectInput("predict_LU", label = strong('Land Use'),  choices = levels(ctree$LU), selected = "Street tree")
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
                                                    tableOutput(outputId = "out_prediction")
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
                  run_predict_go = FALSE)                   # Flip the settng to trigger an update on the predictions
      
      
      
      
  
      ################################################################################################################
      # 
      # Common non-reactive functions
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
            all_pred_df <- data.frame(m_preds_q_mx[,model$pred_names_q, drop=FALSE])
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
      
      
      
      get_regression_coords = function(filter_model_type, ctree, model, m_preds, p_pred, spps, land_use)
      {
            num_predictions = 101

            # Create a matrix with the means of all the quantitative predictors
            m_preds_q_means <- colMeans(ctree[model$pred_names_q], na.rm=TRUE)
            m_preds_q_mx <- t(matrix(rep(m_preds_q_means,num_predictions),nrow=length(model$pred_names_q)))
            colnames(m_preds_q_mx) <- model$pred_names_q
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
                  bins[length(bins)] <- bins[length(bins)] + 1                  ## Necessary so the highest x falls in the last bin
                  x_binned <- cut(ctree[,predictor], breaks=bins, right=FALSE)
                  
                  mean_x <- tapply(ctree[,predictor], x_binned, mean)
                  mean_y <- tapply(ctree$occur, x_binned, mean)
                  occurrence_coords <- rbind (occurrence_coords, data.frame(x=mean_x, y=mean_y, species=rep(spp, length(mean_x))))
            }
            return (occurrence_coords)
            
      }
      
      
      
      ################################################################################################################
      # 
      # Common reactive functions
      # 
      ################################################################################################################  
      
      get_filter_hash <- reactive ({
            return (digest::digest(paste(input$filter_land_use, input$filter_species_set, input$filter_model_predictors, input$filter_model_type, input$filter_species_set_others)))
      })
      
      # Returns the model required to run predictions for any species in the current species set. 
      get_model <- reactive({

            # Make sure all paramters are set
            if (is.null(input$filter_model_predictors)|| is.null(input$filter_land_use) || length(input$filter_land_use)==0)
            {
                  return (NULL)
            }

            filter_data <- filter_data()
            filter_hash <- get_filter_hash()
            predictors <- paste(input$filter_model_predictors,collapse='+')

            if (nrow(ctree) == 0)
            {
                  return (NULL)
            }

            model = NULL
            if (!is.null(g_models[[filter_hash]]) )
            {
                  return (g_models[[filter_hash]])
            }
            else
            {
                  pred_c_specs = list()
                  if ('LU' %in% input$filter_model_predictors)
                  {
                        cat_pred_name='LU'
                        cat_name_ref=input$filter_land_use[1]
                        cat_names_non_ref=input$filter_land_use[2:length(input$filter_land_use)]
                        cat_cf_names=paste('LU',cat_names_non_ref,sep='')
                        cat_weights <- table(filter_data[[cat_pred_name]])/nrow(filter_data)
                        pred_c_specs[['LU']]  = list(cat_pred_name=cat_pred_name, cat_name_ref=cat_name_ref, cat_names_non_ref=cat_names_non_ref, cat_cf_names=cat_cf_names, cat_weights=cat_weights)
                  }
                  
                  
                  withProgress (message="Generating multinomial model", value=0, {
                        incProgress(.2, detail="Wait - this can take minutes")
                        model <- multinom(paste('GENUSSPECI ~ ', predictors), data=filter_data, maxit=MAX_ITER)
                        model <- list(model_reduced=model, 
                                      cf=coef(model), 
                                      aic=model$AIC, 
                                      r2=(1-(model$deviance/(update(model, . ~ 1,trace=F)$deviance))), 
                                      spps=model$lev,
                                      ref_spp=model$lev[1],
                                      sample_size=nrow(filter_data),
                                      filter_land_use=input$filter_land_use,
                                      pred_names_q=input$filter_model_predictors[input$filter_model_predictors %in% all_predictors_quantitative],
                                      pred_c_specs=pred_c_specs)
                        model$model_reduced$fitted.values <- NULL
                        model$model_reduced$residuals <- NULL
                        model$model_reduced$weights <- NULL
                        model$model_reduced$family <- NULL
                        model$model_reduced$na.action <- NULL
                        attr(model$model_reduced$terms,".Environment") =c()
                        attr(model$model_reduced$formula,".Environment") =c()
                        g_models[[filter_hash]] <<- model
                        incProgress(.9)
                  })
            }
            return (model)
      })
      


      
      ################################################################################################################
      # 
      # Filter tab
      # 
      ################################################################################################################

      # Observe the species set  UI
      observeEvent (input$filter_species_set, {
            # Update the model species selection list choices 
            filter_data()
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all)
      })
      
      
      # Observe the land_use filter UI
      observeEvent (input$filter_land_use, {
            # Update the model land_use list with the selected filter values
            updateCheckboxGroupInput (session, "land_use", choices = input$filter_land_use, selected = input$filter_land_use[input$filter_land_use %in% input$land_use])
      })
      
      
      
      # Observe the model predictors UI
      observeEvent (input$filter_model_predictors, {
            # Set the plot predictor choice to refect the ordered list of model predictors and update the UI
            p_choices <- all_predictors_quantitative[all_predictors_quantitative %in% input$filter_model_predictors]
            p_selected <- if (input$plot_predictor %in% p_choices) input$plot_predictor else p_choices[1]
            updateRadioButtons(session, "plot_predictor", choices=as.list(p_choices), selected=p_selected)
      })
      

      # Subset the data 
      filter_data <- reactive({
            data <- ctree
            if (input$filter_species_set == 'Top 10 species')
            {
                  species_names_all <- sort(names(head(sort(table(factor(data[['GENUSSPECI']])), decreasing = TRUE), TOP_TEN)))
            }
            else if (input$filter_species_set ==  'Top 10 genera')
            {
                  # Coerce all species names to genus only, then select the top 10
                  data[['GENUSSPECI']] <-  sapply(strsplit(data$GENUSSPECI," "),"[",1)
                  species_names_all <- sort(names(head(sort(table(factor( data[['GENUSSPECI']]  )), decreasing = TRUE), 10)))
            }
            else 
            {
                  # Default to common species
                  species_names_all <- common_species
            }
            if (input$filter_species_set_others == TRUE)
            {
                 # Coerce all non-common species names to "Other"
                 data[['GENUSSPECI']] <- ifelse ((match(data[['GENUSSPECI']], species_names_all, nomatch = 0) > 0), data[['GENUSSPECI']], "Other")
                 species_names_all <- c(species_names_all, "Other")
            }
            r_values$species_names_all <- species_names_all
            data <- subset(data, LU %in% input$filter_land_use & GENUSSPECI %in% r_values$species_names_all)
            data$LU <- factor(data$LU)
            data$GENUSSPECI <- factor(data$GENUSSPECI)
            return (data)
      })
      
      
      # Save cached models to local workspace
      observeEvent (input$save_models, {
            saveRDS (g_models, file=models_local_file)
      })
      
      
      
      
      ################################################################################################################
      # 
      # Model tab
      # 
      ################################################################################################################
      

      # Observe the filter tab's species list
      observeEvent (r_values$species_names_all, {
            species_intersect <- input$species[input$species %in% r_values$species_names_all]
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = (if (length(species_intersect)>0) species_intersect else r_values$species_names_all[1]))
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
            
            model <- get_model()
            filter_data <- filter_data()

            # Make sure all paramters are set
            if (is.null(input$filter_model_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% input$filter_model_predictors) ||is.null(input$land_use) || (sum(input$species %in% model$spps)==0) )
            {
                  return (NULL)     
            }
            regression_coords <- get_regression_coords(input$filter_model_type, filter_data, model, input$filter_model_predictors, input$plot_predictor, input$species, input$land_use)
            occurrence_coords <- get_occurrence_coords (filter_data, input$species, input$plot_predictor)
            
            p <- ggplot () +
                  scale_x_continuous(name=names(all_predictors_quantitative[all_predictors_quantitative==input$plot_predictor])) +
                  scale_y_continuous(limits=c(0,1.1), name="Probability of occurence") +
                  theme(title = label_font, axis.title = label_font, axis.text = data_font, legend.title = label_font, legend.text = data_font) +
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
            if (is.null(input$filter_model_predictors) || is.null(input$plot_predictor) ||is.null(input$species) || !(input$plot_predictor %in% input$filter_model_predictors))
            {
                  return (NULL)     
            }

            stats <- data.frame("Multinomial", model$sample_size, signif(model$aic,4), signif(model$r2,4))
            colnames(stats) <- c('Type', 'Samples', 'AIC', ' R**2')
            return (stats)
            
      })
      
  
      
      
      
      
      ################################################################################################################
      # 
      # Predict tab
      # 
      ################################################################################################################
      
      # Hide predictors that are not in the model
      observeEvent (input$filter_model_predictors, {
            for (pred in all_predictors_quantitative)
            {
                  checkbox_id <- paste('predict_on_', pred, sep='')
                  slider_id <- paste('predict_', pred, sep='')
                  if (!is.null(input[[checkbox_id]]))
                  {
                        if ((pred %in% input$filter_model_predictors))
                        {
                              shinyjs::show(checkbox_id)
                              shinyjs::show(slider_id)
                        }
                        else
                        {
                              shinyjs::hide(checkbox_id)
                              shinyjs::hide(slider_id)
                        }
                  }
            }
            for (pred in all_predictors_categorical)
            {
                  select_input_id <- paste('predict_', pred, sep='')
                  if (!is.null(input[[select_input_id]]))
                  {
                        if ((pred %in% input$filter_model_predictors))
                        {
                              shinyjs::show(select_input_id)
                        }
                        else
                        {
                              shinyjs::hide(select_input_id)
                        }
                  }
            }
      })
      
      # Observe updates to the checkboxes. When turned off, reset associated slider value to the mean
      lapply (X=all_predictors_quantitative, FUN=function (i)
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
      lapply (X=all_predictors_quantitative, FUN=function (i)
      {
            observeEvent (input[[paste('predict_', i, sep='')]], {
                  # Trigger the prediction update
                  r_values$run_predict_go <- !r_values$run_predict_go
            }, ignoreInit = TRUE)      
      })
      
      # This is triggered when the "run_predict_go" switch is flipped
      predict_go <- eventReactive(r_values$run_predict_go, {
            df <- data.frame(Predictor = character(), Value=numeric(), stringsAsFactors = FALSE)
            for (p in all_predictors_quantitative)
            {
                  if ( (input[[paste('predict_on_', p, sep='')]] == TRUE) && (p %in% input$filter_model_predictors))
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
            model_input <- t(matrix(data=predict_go()$Value,dimnames=list(predict_go()$Predictor)))
            lu_weight <- ifelse('LU' %in% input$filter_model_predictors, model$pred_c_specs[['LU']]$cat_weights[[input$predict_LU]], 1)
            p <- predict_multinomial(model, model_input, input$predict_LU)
            p <- data.frame(Probability=t(p/lu_weight))
            p$Species <- rownames(p)
            return (p)
      })
      
      output$out_prediction <- renderTable({ 
            prediction <- get_species_prediction()
            prediction <- prediction[order(prediction$Probability, decreasing = TRUE),]
            return (rbind(prediction, data.frame(Species='Total', Probability=sum(prediction$Probability))))
      }, spacing='xs')

       output$out_piechart <- renderPlot({
             prediction <- get_species_prediction()
             
             blank_theme <- theme_minimal()+
                   theme(
                         axis.title = label_font, axis.text = data_font, legend.title = label_font, legend.text = data_font,
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


}

# Create Shiny object
shinyApp(ui = ui, server = server)


