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
common_species <- unique(read.delim('https://don-morrison-2000.github.io/data/common_species.csv', as.is=TRUE, header=FALSE)$V1)

# Convert model categories to factors that match the UI names
ctree[['LU']] <- as.factor(land_use$V2[as.numeric(as.character(ctree$LU))])

species_sets <- c("Top 10 species", "Top 10 genera", "Common species")

# Define all possible predictors then subset to just the ones that show up in the input data
all_predictors <- c (
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
all_predictors <- all_predictors[all_predictors %in% colnames(ctree)]
all_predictors <- all_predictors[order(names(all_predictors))]

# Model cache
g_models <- list()

# Set this to 1 for fast multinomial testing (100 for production)
MAX_ITER = 100


# Functions to allow prediction page to generated programatically
pred_row <- function (pred) {return (list(cb(pred), sl(pred)))}
hr <- tags$hr(style="height:1px;border:none;color:#333;background-color:#333;")
cb <- function(pred) {checkboxInput (inputId = paste('predict_on_', pred, sep=''), label = strong(names(all_predictors[all_predictors==pred])), width = '600px', value = FALSE)} 
sl <- function(pred) {conditionalPanel (condition = paste('input.predict_on_', pred, '==true', sep=''), sliderInput (inputId = paste('predict_', pred, sep=''), label = '', step=(.1*(10^floor(log10(diff(range(ctree[[pred]])))))), min = floor(min(ctree[[pred]],na.rm=TRUE)), max = ceiling(max(ctree[[pred]],na.rm=TRUE)), value = signif(mean(ctree[[pred]],na.rm=TRUE),3)), hr)}



# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
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
                                        title='Filter',
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              ( 
                                                    radioButtons (inputId = "filter_species_set", label = strong("Species Set"), choices = species_sets, selected = species_sets[1]),
                                                    checkboxInput (inputId = "filter_species_set_others", label = "Include others", value = TRUE)
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              ( 
                                                    checkboxGroupInput (inputId = "filter_land_use", label = strong("Land Use"), choices = levels(ctree$LU), selected = levels(ctree$LU))
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=3,
                                              wellPanel
                                              (  
                                                    checkboxGroupInput (inputId = "filter_model_predictors", label = strong("Model Predictors"), choices = all_predictors, selected = all_predictors)
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=2,
                                              wellPanel
                                              ( 
                                                    radioButtons (inputId = "filter_model_type", label = strong('Model type'), choices = c("Binomial","Multinomial"), selected = "Binomial")
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
                                                          radioButtons (inputId = "plot_predictor", label = strong("Plot Predictor"), choices = all_predictors, selected = all_predictors[1])
                                                    )
                                              ),
                                              column 
                                              ( 
                                                    width=3,
                                                    wellPanel
                                                    ( 
                                                          checkboxGroupInput (inputId = "species", label = strong("Species"), choices = common_species, selected = common_species[1])
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
                                                    lapply (all_predictors, function (i) {pred_row(i)})
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=7,
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
                  m_preds = NULL,                           # Ordered list of selected model predictors
                  p_pred = NULL,                            # Single model predictor selected to be plotted
                  species_list_selected = NULL,             # List of species names selected to be plotted
                  species_names_all = NULL,                 # List of species name in the current "species set" (after filtering)  
                  x = NULL,                                 # Zoomable x coordinate
                  y = NULL,                                 # Zoomable y coordinate
                  run_predict_go = FALSE)                   # Flip the settng to trigger an update on the predictions
      
      
      
      
  
      ################################################################################################################
      # 
      # Common non-reactive functions
      # 
      ################################################################################################################  

      get_binomial_regression_coords = function(ctree, models, predictor, spps)
      {
            result_df <- data.frame (x=numeric(), y=numeric(), species=character())
            
            num_predictions = 101
            
            # Create a data frame with the means of all the predictors
            all_pred_names <- rownames(models[[1]]$cf)[-1] 
            all_pred_means <- colMeans(ctree[all_pred_names], na.rm=TRUE)
            all_pred_df <- data.frame(t(matrix(rep(all_pred_means,num_predictions),nrow=length(all_pred_names))))      
            colnames(all_pred_df) <- all_pred_names
            
            # Update the values in the input predictor column
            all_pred_df[predictor] <- seq(min(ctree[,predictor],na.rm=TRUE),max(ctree[,predictor],na.rm=TRUE),len=num_predictions)

            for (spp in spps)
            {
                  model <- models[[spp]]
                  if (!is.null(model))
                  {
                        prediction <- predict(model$model_reduced, all_pred_df, type="response")
                        result_df <- rbind (result_df, data.frame(x=all_pred_df[[predictor]], y=prediction, species=rep(model$species,num_predictions)))
                  }
            }
            return (result_df)
      }
      
      
      # Return a data frame where each row represents a predicted outcode
      get_multinomial_regression_coords = function(ctree, spps, model, predictor)
      {
            num_predictions = 101
            
            # Create a data frame with the means of the non-predictors
            non_pred_columns <- setdiff(all_predictors,predictor)
            non_pred_means <- colMeans(ctree[non_pred_columns], na.rm=TRUE)
            non_pred_matrix <- t(matrix(rep(non_pred_means,num_predictions),nrow=length(non_pred_columns)))
            non_pred_df <- data.frame(non_pred_matrix)      
            colnames(non_pred_df) <- non_pred_columns
            
            # Create a data frame with a set of values in the predictor variable range
            range <- range(ctree[[predictor]],na.rm=TRUE)
            min <- range[1]
            max <- range[2]
            pred_df <- data.frame(c(0:(num_predictions-1))*((max-min)/(num_predictions-1))+min)
            colnames(pred_df) <- predictor
            
            # The input to the model prediction is a cbind of the predictor and non-predictor data frames
            model_input_df <- cbind(pred_df, non_pred_df)
            
            # Get the prediction results and cbind it to the predictor values (for graphing purposes)
            result_df <- cbind(pred_df, predict(model, newdata = model_input_df, type = "probs", se = TRUE))
            colnames(result_df) <- c(predictor, model$lev)
            result_df <- result_df[c(predictor,spps)]
            result_df <- melt(result_df, id.vars=predictor, value.name="probability")
            colnames(result_df) <- (c('x','species','y'))
            
            return (result_df[c('x','y','species')])
      }
      
      get_regression_coords = function(filter_model_type, ctree, models, predictor, spps)
      {
            if (filter_model_type == 'Binomial')
            {
                  return (get_binomial_regression_coords(ctree, models, predictor, spps))
            }
            else if (filter_model_type == 'Multinomial')
            {
                  return (get_multinomial_regression_coords(ctree, spps, models$model_reduced, predictor))
            }
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
            return (digest::digest(paste(input$filter_land_use, input$filter_species_set, r_values$m_preds, input$filter_model_type, input$filter_species_set_others)))
      })
      
      # Returns the model(s) required to run predictions for any species in the current species set. If the current mode is "binomial" then 
      # a list of models is returned (one for each species). If the current mode is "multinomial", then a single model is returned
      get_models <- reactive({ 
            # Make sure all paramters are set
            if (is.null(r_values$m_preds)|| is.null(input$filter_land_use) || length(input$filter_land_use)==0)
            {
                  return (NULL)   
            }
            
            filter_data <- filter_data()
            filter_hash <- get_filter_hash()
            predictors <- paste(r_values$m_preds,collapse='+')
            
            if (nrow(ctree) == 0)
            {
                  return (NULL)
            }
            
            if (input$filter_model_type == 'Binomial')
            {
                  models <- list()
                  withProgress (message="Generating binomial model", value=0, {
                        n <- length(r_values$species_names_all)
                        for (species in r_values$species_names_all)
                        {
                              model_id <- paste(species, filter_hash)
                              if (!is.null(g_models[[model_id]]) )
                              {
                                    models[[species]] <- g_models[[model_id]]
                              }
                              else
                              {
                                    incProgress(1/n, detail = species)
                                    # Build a binomial model for this species
                                    filter_data[,'occur'] <- ifelse (filter_data[,'GENUSSPECI']==species, 1,0)
                                    model <- glm(formula=as.formula(paste('occur ~ ', predictors)),family=binomial(link='logit'),data=filter_data)
                                    cf <- summary(model)$coefficients
                                    model$data <- NULL
                                    model$y <- NULL
                                    model$linear.predictors <- NULL
                                    model$weights <- NULL
                                    model$fitted.values <- NULL
                                    model$model <- NULL
                                    model$prior.weights <- NULL
                                    model$residuals <- NULL
                                    model$effects <- NULL
                                    model$qr$qr <- NULL
                                    models[[species]] <- list(model_reduced=model, cf=cf, aic=model$aic, species=species, sample_size=sum(filter_data$occur),r2=(1-(model$deviance/model$null.deviance)))
                                    g_models[[model_id]] <<- models[[species]]
                              }
                        }
                  })
                  return (models)
            }
            else if (input$filter_model_type == 'Multinomial')
            {
                  model = NULL
                  if (!is.null(g_models[[filter_hash]]) )
                  {
                        return (g_models[[filter_hash]])
                  }
                  else
                  {
                        withProgress (message="Generating multinomial model", value=0, {
                              incProgress(.2, detail="Wait - this can take minutes")
                              model <- multinom(paste('GENUSSPECI ~ ', predictors), data = filter_data, maxit=MAX_ITER) # MAX IT IS FOR TEST ONLY TO SPEED IT UP
                              incProgress(.8)
                              model <- list(model_reduced=model, aic=model$AIC, r2=(1-(model$deviance/(update(model, . ~ 1,trace=F)$deviance))))
                              model$model_reduced$fitted.values = NULL
                              model$model_reduced$residuals = NULL
                              model$model_reduced$weights = NULL
                              g_models[[filter_hash]] <<- model 
                              incProgress(.9)
                        })
                  }
                  return (model)
            }
            else 
                  return (NULL)
      })
      
      
      
      ################################################################################################################
      # 
      # Filter tab
      # 
      ################################################################################################################
      
 
      # Observe the model predictors UI
      observeEvent (input$filter_model_predictors, {
            m_preds <- character(0)
            if (!is.null(input$filter_model_predictors))
            {
                  # Construct a list of selected model predictors - in order that they were selected
                  # First list in order the previously existing model predictors (except leave out those that have been deslected)
                  for (x in r_values$m_preds) {
                        if ((x %in% input$filter_model_predictors)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
                  # Now add in the newly selected predictors. Since this is inside an observeEvent, we would expect only 
                  # one new predictor. If there are more than one, then there is no way to determine the order - oh well....
                  for (x in input$filter_model_predictors) {
                        if (!(x %in% r_values$m_preds)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
            }
            # Set the plot predictor choice to refect the ordered list of model predictors and update the UI
            p_choices <- m_preds
            p_selected <- if (input$plot_predictor %in% p_choices) input$plot_predictor else p_choices[1]
            updateRadioButtons(session, "plot_predictor", choices=as.list(m_preds), selected=p_selected)
            # Update the reactive value to propagate the new model predictors
            r_values$m_preds <- m_preds
      })
      

      # Subset the data 
      filter_data <- reactive({
            data <- ctree
            if (input$filter_species_set == 'Top 10 species')
            {
                  species_names_all <- sort(names(head(sort(table(factor(data[['GENUSSPECI']])), decreasing = TRUE), 10)))
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
            return (subset(data, LU %in% input$filter_land_use & GENUSSPECI %in% r_values$species_names_all))
      })
      
      
      
      
      
      
      
      ################################################################################################################
      # 
      # Model tab
      # 
      ################################################################################################################
      
      # Observe the plot predictors UI
      observeEvent (input$plot_predictors, {
            # Update the reactive value to match the UI
            r_values$p_preds <- input$plot_predictor
      })
      
      
      # Observe the species selection UI
      observeEvent (input$species, {
            # Update the reactive value to match the UI
            r_values$species_list_selected <- input$species
      }, ignoreNULL=FALSE)

      
      # Observe the model predictor UI
      observeEvent (input$filter_model_predictors, {
            m_preds <- character(0)
            if (!is.null(input$filter_model_predictors))
            {
                  # Construct a list of selected model predictors - in order that they were selected
                  # First list in order the previously existing model predictors (except leave out those that have been deslected)
                  for (x in r_values$m_preds) {
                        if ((x %in% input$filter_model_predictors)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
                  # Now add in the newly selected predictors. Since this is inside an observeEvent, we would expect only 
                  # one new predictor. If there are more than one, then there is no way to determine the order - oh well....
                  for (x in input$filter_model_predictors) {
                        if (!(x %in% m_preds)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
            }
            # Update the plot predictor UI to refect the ordered list of model predictors
            updateRadioButtons(session, "plot_predictor", choices=as.list(m_preds), selected=(if (input$plot_predictor %in% m_preds) input$plot_predictor else m_preds[1]))
            # Update the reactive value to propagate the new model predictors
            r_values$m_preds <- m_preds
      }, ignoreNULL=FALSE)
      

      # Observe the plot predictor UI
      observeEvent (input$plot_predictor, {
            r_values$p_pred <- input$plot_predictor
      })

      
      # Observe the filter tab's species list
      observeEvent (r_values$species_names_all, {
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names_all, selected = input$species[input$species %in% r_values$species_names_all])
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
            
            # Make sure all paramters are set
            if (is.null(r_values$m_preds) || is.null(r_values$p_pred) ||is.null(r_values$species_list_selected) || !(r_values$p_pred %in% r_values$m_preds))
            {
                  return (NULL)     
            }
            models <- get_models()
            regression_coords <- get_regression_coords(input$filter_model_type, filter_data(), models, r_values$p_pred, r_values$species_list_selected)
            occurrence_coords <- get_occurrence_coords (filter_data(), r_values$species_list_selected, r_values$p_pred)
            
            label_font <- element_text(family="sans", color='black', size=16)
            data_font <- element_text(family="sans", face="bold", color='black', size=12)
            p <- ggplot () +
                  scale_x_continuous(name=names(all_predictors[all_predictors==r_values$p_pred])) +
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
            
            # Make sure all paramters are set
            if (is.null(r_values$m_preds) || is.null(r_values$p_pred) ||is.null(r_values$species_list_selected) || !(r_values$p_pred %in% r_values$m_preds))
            {
                  return (NULL)     
            }

            models <- get_models()
            if (input$filter_model_type == 'Binomial')
            {
                  col_names <- c('Model', 'Species', 'Samples', 'Predictor','Estimate', 'Std. Error', 'z value', 'Pr(>|z|)', 'AIC', 'R**2')
                  stats <- data.frame(model=character(), species=character(), sample_size=character(), predictor=character(), estimate=numeric(), std_error=numeric(), z_value=numeric(), pr=numeric(), aic=numeric(), r2=numeric)
                  colnames(stats) <- col_names
                  
                  for (model in models)
                  {
                        if (r_values$p_pred %in% rownames(model$cf) && model$species %in% r_values$species_list_selected)
                        {
                              # Extract the model statistics summary
                              m <- model$cf[r_values$p_pred,]
                              # Populate the statistics table row
                              r = array(0,dim=c(1,length(col_names)))
                              r[1,1] <- "Binomial"
                              r[1,2] <- model$species
                              r[1,3] <- model$sample_size
                              r[1,4] <- r_values$p_pred
                              r[1,5] <- signif(m[1],4)
                              r[1,6] <- signif(m[2],4)
                              r[1,7] <- signif(m[3],4)
                              r[1,8] <- signif(m[4],4)
                              r[1,9] <- signif(model$aic,4)
                              r[1,10] <- signif(model$r2,4)
                              colnames(r) <- col_names
                              # Add the row to the table
                              stats <- rbind (stats, as.data.frame(r))
                        }
                  }
                  return (stats)
            }
            else if (input$filter_model_type == 'Multinomial')
            {
                  stats <- data.frame("Multinomial", signif(models$aic,4), signif(models$r2,4))
                  colnames(stats) <- c('Model', 'AIC', ' R**2')
                  return (stats)
            }
      })
      
  
      
      
      
      
      ################################################################################################################
      # 
      # Predict tab
      # 
      ################################################################################################################
      
      # Hide non-selected predictors and set the values to the mean
      observeEvent (input$filter_model_predictors, {
            for (pred in all_predictors)
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
            # Trigger the prediction update
            r_values$run_predict_go <- !r_values$run_predict_go
      })
      
      # Observe updates to the prediction sliders. If the Go button is displayed, eat it. Otherwise send the updates 
      # to the output function that will update the prediction table
      lapply (X=all_predictors, FUN=function (i)
      {
            observeEvent (input[[paste('predict_', i, sep='')]], {
                  # Trigger the prediction update
                  r_values$run_predict_go <- !r_values$run_predict_go
            }, ignoreInit = TRUE)      
      })
      
      # This is triggered when the "run_predict_go" switch is flipped
      predict_go <- eventReactive(r_values$run_predict_go, {
            df <- data.frame(Predictor = character(), Value=numeric(), stringsAsFactors = FALSE)
            for (p in all_predictors)
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
  
      
      output$out_prediction <- renderTable({ 
            predictor_names <- predict_go()$Predictor
            predictor_values <- predict_go()$Value
            data <- filter_data()
            

            if (length(predictor_names) > 0)
            {
                  model_input <- as.data.frame(t(predictor_values))
                  colnames(model_input) <- predictor_names
                  
                  # Create the models (one per species) for the prediction
                  df <- data.frame(predictor = character(), prediction = numeric(), aic = numeric(), stringsAsFactors = FALSE)
                  if (input$filter_model_type == 'Binomial')
                  {
                        models <- get_models()
                        for (model in models)
                        {
                              prediction <- predict(model$model_reduced, model_input, type="response")
                              df <- rbind (df, data.frame(model$species, prediction, as.integer(model$model_reduced$aic)))
                        }
                  }
                  else if (input$filter_model_type == 'Multinomial')
                  {
                        model <- get_models()
                        df <- rbind(df, data.frame(predict(model$model_reduced, newdata = model_input, type = "probs", se = TRUE)))
                        df <- cbind(model$model_reduced$lev, df,rep(as.integer(model$aic,nrow(df))))
                  }
                  colnames(df) <- c("Species", "Probability", "AIC")
                  df <- df[order(df$Probability, decreasing = TRUE),]
                  return (rbind(df, data.frame(Species='Total', Probability=sum(df$Probability), AIC='')))
            }
            else
            {
                  return (NULL)
            }
      })
      
}

# Create Shiny object
shinyApp(ui = ui, server = server)

