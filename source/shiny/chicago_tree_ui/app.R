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
# To run from the web
# library(shiny)
# shiny::runGitHub('don-morrison-2000.github.io','don-morrison-2000', subdir='source/shiny/chicago_tree_ui/')

# Load packages
library(shiny)
library(shinythemes)
library(ggplot2)

# This file has the graphing functions
chicago_tree_app_local_file <- 'D:/CRTI/r_projects/shinyapp/chicagotree_dam.r'
chicago_tree_app_url <- 'https://don-morrison-2000.github.io/source/chicagotree_dam.r'

ctree_local_file <- 'D:/CRTI/data/cleaned/dupage_county_accepted_V4.csv'
ctree_http_url <- 'https://don-morrison-2000.github.io/data/dupage_county_accepted_V4.csv'

# Source Richard's modeling proram (either from local disk or the http server)
if (file.exists(chicago_tree_app_local_file))
{
      source(chicago_tree_app_local_file)
} else 
{
      source (chicago_tree_app_url)
}

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
g_model_full <- NULL


# Functions to allow prediction page to generated programatically
pred_row <- function (pred) {return (list(cb(pred), sl(pred)))}
hr <- tags$hr(style="height:1px;border:none;color:#333;background-color:#333;")
cb <- function(pred) {checkboxInput (inputId = paste('predict_on_', pred, sep=''), label = strong(names(all_predictors[all_predictors==pred])), width = '600px', value = FALSE)} 
sl <- function(pred) {conditionalPanel (condition = paste('input.predict_on_', pred, '==true', sep=''), sliderInput (inputId = paste('predict_', pred, sep=''), label = '', step=(.1*(10^floor(log10(diff(range(ctree[[pred]])))))), min = floor(min(ctree[[pred]],na.rm=TRUE)), max = ceiling(max(ctree[[pred]],na.rm=TRUE)), value = signif(mean(ctree[[pred]],na.rm=TRUE),3)), hr)}


# Models are maintined in a list with this format [[<<predictor formula]][[<land use desc>]][[<species name]]
build_model <- function (ctree, predictors, species, land_use)
{
      formula <- paste('occur ~ ', paste(predictors,collapse='+'))
      land_use_desc <- paste(sort(land_use), collapse="+")
      # Check if the model already exists in the cache
      if (!is.null(g_models[[formula]][[land_use_desc]][[species]]) )
      {
            return (g_models[[formula]][[land_use_desc]][[species]])
      }
      else
      {
            if (nrow(ctree) > 0)
            {
                  ctree[,'occur'] <- 0
                  ctree[ctree[,'GENUSSPECI']==species,'occur'] <- 1
                  model <- glm(formula=as.formula(formula),family=binomial(link='logit'),data=ctree)
                  model <- list(cf=summary(model)$coefficients,aic=summary(model)$aic, species=species, land_use=land_use_desc, sample_size=sum(ctree$occur),r2=(1-(model$deviance/model$null.deviance)))
                  # Cache the model in the global space - note the super-assign operator '<<-'
                  g_models[[formula]][[land_use_desc]][[species]] <<- model        
                  return(model)
            }
            else
            {
                  return (NULL)
            }
      }
}


# Define UI
ui <- fluidPage(theme = shinytheme("lumen"),
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
                                              width=4,
                                              wellPanel
                                              ( 
                                                    checkboxInput (inputId = "filter_species_set_on", label = strong("Species set"), value = FALSE),
                                                    conditionalPanel
                                                    ( 
                                                          condition = 'input.filter_species_set_on == true', 
                                                          radioButtons (inputId = "filter_species_set", label = '', choices = species_sets, selected = species_sets[1]),
                                                          checkboxInput (inputId = "filter_species_set_others", label = "Include others", value = TRUE)
                                                    )
                                              )
                                        ),
                                        column 
                                        ( 
                                              width=4,
                                              wellPanel
                                              ( 
                                                    checkboxInput (inputId = "filter_land_use_on", label = strong("Land use"), value = FALSE),
                                                    conditionalPanel
                                                    ( 
                                                          condition = 'input.filter_land_use_on == true', 
                                                          checkboxGroupInput (inputId = "filter_land_use", label = '', choices = levels(ctree$LU), selected = levels(ctree$LU))
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
                                                          checkboxGroupInput (inputId = "model_predictors", label = strong("Model Predictors"), choices = all_predictors, selected = all_predictors[1])
                                                    )
                                              ),
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
                                              conditionalPanel
                                              ( 
                                                    condition = 'output.show_predict_go == true', 
                                                    wellPanel
                                                    (
                                                          width=2,
                                                          actionButton ("predict_go", "Build models") 
                                                    )
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
                  filter_land_use = levels(ctree$LU),       # Land use settings included in data filter
                  m_preds = NULL,                           # Ordered list of selected model predictors
                  p_pred = NULL,                            # Single model predictor selected to be plotted
                  species_list = NULL,                      # List of species selected to be plotted 
                  x = NULL,                                 # Zoomable x coordinate
                  y = NULL,                                 # Zoomable y coordinate
                  show_predict_go = NULL,                   # Show/hide the Go button on the predict tab
                  run_predict_go = FALSE,                   # Flip the settng to trigger an update on the predictions
                  filter_species_set = NULL,
                  filter_species_set_others = TRUE,
                  species_names = NULL)
      
      
      
      
      
      
      ################################################################################################################
      # 
      # Filter tab
      # 
      ################################################################################################################
      
      # Observe the land use filter checkbox UI
      observeEvent (input$filter_land_use_on, {
            # Update the reactive value to match the UI
            if (input$filter_land_use_on == TRUE)
            {
                  # The filter is turned on - restore the filter values
                  r_values$filter_land_use <- input$filter_land_use
            }
            else
            {
                  # The filter is turned off - include all LUs
                  r_values$filter_land_use <- levels(ctree[['LU']])
            }
      })
      
      
      # Observe the land use filter selection UI
      observeEvent (input$filter_land_use, {
            if (is.null(input$filter_land_use))
            {
                  # All values were deselected - do not allow this
                  updateCheckboxGroupInput(session, "filter_land_use", choices = levels(ctree[['LU']]), selected = c(r_values$filter_land_use))
            }
            else
            {
                  r_values$filter_land_use <- input$filter_land_use
            }
      }, ignoreNULL=FALSE)
      
      
      # Observe the land use filter checkbox UI
      observeEvent (input$filter_land_use_on, {
            # Update the reactive value to match the UI
            if (input$filter_land_use_on == TRUE)
            {
                  # The filter is turned on - restore the filter values
                  r_values$filter_land_use <- input$filter_land_use
            }
            else
            {
                  # The filter is turned off - include all LUs
                  r_values$filter_land_use <- levels(ctree[['LU']])
            }
      })
      
      
      # Observe the species set filter selection UI
      observeEvent (input$filter_species_set_on, {
            # Update the reactive value to match the UI
            if (input$filter_species_set_on == TRUE)
            {
                  # The filter is turned on - restore the filter values
                  r_values$filter_species_set <- input$filter_species_set
                  r_values$filter_species_set_others <- input$filter_species_set_others
            }
            else
            {
                  # The filter is turned off - include all LUs
                  r_values$filter_species_set <- species_sets[1]
                  r_values$filter_species_set_others <- TRUE
            }
      })
      observeEvent (input$filter_species_set, {
            # Save the name of the species set
            r_values$filter_species_set <- input$filter_species_set
      })
      observeEvent (input$filter_species_set_others, {
            # Save the setting for keeping "others"
            r_values$filter_species_set_others <- input$filter_species_set_others
      })
      
      
      # Subset the data 
      filter_data <- reactive({
            data <- ctree
            if (r_values$filter_species_set == 'Top 10 species')
            {
                  species_names <- sort(names(head(sort(table(factor(data[['GENUSSPECI']])), decreasing = TRUE), 10)))
            }
            else if (r_values$filter_species_set ==  'Top 10 genera')
            {
                  # Coerce all species names to genus only, then select the top 10
                  data[['GENUSSPECI']] <-  sapply(strsplit(data$GENUSSPECI," "),"[",1)
                  species_names <- sort(names(head(sort(table(factor( data[['GENUSSPECI']]  )), decreasing = TRUE), 10)))
            }
            else 
            {
                  # Default to common species
                  species_names <- common_species
            }
            if (r_values$filter_species_set_others == TRUE)
            {
                 # Coerce all non-common species names to "Other"
                 data[['GENUSSPECI']] <- ifelse ((match(data[['GENUSSPECI']], species_names, nomatch = 0) > 0), data[['GENUSSPECI']], "Other")
                 species_names <- c(species_names, "Other")
            }
            r_values$species_names <- species_names
            return (subset(data, LU %in% r_values$filter_land_use & GENUSSPECI %in% r_values$species_names))
      })
      
      
      
      
      
      
      
      ################################################################################################################
      # 
      # Model tab
      # 
      ################################################################################################################
      
      
      # Observe the model predictors UI
      observeEvent (input$model_predictors, {
            m_preds <- character(0)
            if (!is.null(input$model_predictors))
            {
                  # Construct a list of selected model predictors - in order that they were selected
                  # First list in order the previously existing model predictors (except leave out those that have been deslected)
                  for (x in r_values$m_preds) {
                        if ((x %in% input$model_predictors)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
                  # Now add in the newly selected predictors. Since this is inside an observeEvent, we would expect only 
                  # one new predictor. If there are more than one, then there is no way to determine the order - oh well....
                  for (x in input$model_predictors) {
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
      
      # Observe the plot predictors UI
      observeEvent (input$plot_predictors, {
            # Update the reactive value to match the UI
            r_values$p_preds <- input$plot_predictor
      })
      
      
      # Observe the species selection UI
      observeEvent (input$species, {
            # Update the reactive value to match the UI
            r_values$species_list <- input$species
      }, ignoreNULL=FALSE)

      
      # Observe the model predictor UI
      observeEvent (input$model_predictors, {
            m_preds <- character(0)
            if (!is.null(input$model_predictors))
            {
                  # Construct a list of selected model predictors - in order that they were selected
                  # First list in order the previously existing model predictors (except leave out those that have been deslected)
                  for (x in r_values$m_preds) {
                        if ((x %in% input$model_predictors)) 
                        {
                              m_preds <- c(m_preds, all_predictors[all_predictors==x])
                        }
                  }
                  # Now add in the newly selected predictors. Since this is inside an observeEvent, we would expect only 
                  # one new predictor. If there are more than one, then there is no way to determine the order - oh well....
                  for (x in input$model_predictors) {
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
      observeEvent (r_values$species_names, {
            #a <- input$species[input$species %in% r_values$species_names]
            #b <- a
            updateCheckboxGroupInput (session, "species", choices = r_values$species_names, selected = input$species[input$species %in% r_values$species_names])
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
      

      # Build the models for the coordinates that need to be plotted
      get_models <- reactive({ 
            # Make sure all paramters are set
            if (is.null(r_values$m_preds)|| is.null(r_values$species_list) || is.null(r_values$filter_land_use) || length(r_values$filter_land_use)==0)
            {
                  return (NULL)   
            }
            
            withProgress (message="Generating model", value=0, {
                  n <- length(r_values$species_list)
                  models <- list()
                  for (species in r_values$species_list)
                  {
                        incProgress(1/n, detail = species)
                        models[[species]] <- build_model (filter_data(), r_values$m_preds, species, r_values$filter_land_use)
                  }
            })
            return (models)
      })
      

      # Plot the probabilities
      output$probability_plot <- renderPlot({ 
            
            # Make sure all paramters are set
            if (is.null(r_values$m_preds) || is.null(r_values$p_pred) ||is.null(r_values$species_list) || !(r_values$p_pred %in% r_values$m_preds))
            {
                  return (NULL)     
            }
            
            regression_coords <- data.frame(x=numeric(), y=numeric(), species=character(), plot_predictor=character())
            occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character(), plot_predictor=character())    
            
            models <- get_models()
            for (species in r_values$species_list)
            {
                  if (!is.null(models[[species]]))
                  {
                        a <- graphOneResult(full=filter_data(),mod=models, sp=species,predictor=r_values$p_pred, retSpecs=TRUE)
                        regression_coords <- rbind (regression_coords, data.frame(a[[1]], species=rep(species, nrow(a[[1]])), plot_predictor=rep(r_values$p_pred,nrow(a[[1]]))))
                        occurrence_coords <- rbind (occurrence_coords, data.frame(a[[2]], species=rep(species, nrow(a[[2]])), plot_predictor=rep(r_values$p_pred,nrow(a[[2]]))))
                  }
            }
            
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
            if (is.null(r_values$m_preds) || is.null(r_values$p_pred) ||is.null(r_values$species_list) || !(r_values$p_pred %in% r_values$m_preds))
            {
                  return (NULL)     
            }
            
            col_names <- c('Species', 'Samples', 'Predictor','Estimate', 'Std. Error', 'z value', 'Pr(>|z|)', 'AIC', 'R**2')
            stats <- data.frame(species=character(), sample_size=character(), predictor=character(), estimate=numeric(), std_error=numeric(), z_value=numeric(), pr=numeric(), aic=numeric(), r2=numeric)
            colnames(stats) <- col_names
            
            models <- get_models()
            for (model in models)
            {
                  if (r_values$p_pred %in% rownames(model$cf))
                  {
                        # Extract the model statistics summary
                        m <- model$cf[r_values$p_pred,]
                        # Populate the statistics table row
                        r = array(0,dim=c(1,length(col_names)))
                        r[1,1] <- model$species
                        r[1,2] <- model$sample_size
                        r[1,3] <- r_values$p_pred
                        r[1,4] <- signif(m[1],4)
                        r[1,5] <- signif(m[2],4)
                        r[1,6] <- signif(m[3],4)
                        r[1,7] <- signif(m[4],4)
                        r[1,8] <- signif(model$aic,4)
                        r[1,9] <- signif(model$r2,4)
                        colnames(r) <- col_names
                        # Add the row to the table
                        stats <- rbind (stats, as.data.frame(r))
                  }
            }
            return (stats)
      })
      
  
      
      
      
      
      ################################################################################################################
      # 
      # Predict tab
      # 
      ################################################################################################################
      
      
      # Show the Go button when a predictor checkbox changes state
      lapply (X=all_predictors, FUN=function (i)
      {
            observeEvent (input[[paste('predict_on_', i, sep='')]], {
                  r_values$show_predict_go <- TRUE
            }, ignoreInit = TRUE)      
      })
      # Hide the Go button after it is clicked
      observeEvent (input$predict_go, {
            r_values$show_predict_go <- FALSE
      })
      
      
      # Observe the GO button
      observeEvent (input$predict_go, {
            # Trigger the prediction update
            r_values$run_predict_go <- !r_values$run_predict_go
      })
      
      # Observe updates to the prediction sliders. If the Go button is displayed, eat it. Otherwise send the updates 
      # to the output function that will update the prediction table
      lapply (X=all_predictors, FUN=function (i)
      {
            observeEvent (input[[paste('predict_', i, sep='')]], {
                  if (r_values$show_predict_go == FALSE)
                  {
                        # Trigger the prediction update
                        r_values$run_predict_go <- !r_values$run_predict_go
                  }
            }, ignoreInit = TRUE)      
      })
      
      # This is triggered when the "run_predict_go" switch is flipped
      predict_go <- eventReactive(r_values$run_predict_go, {
            df <- data.frame(predictor = character(), value=numeric(), stringsAsFactors = FALSE)
            for (p in all_predictors)
            {
                  if (input[[paste('predict_on_', p, sep='')]] == TRUE)
                  {
                        df <-rbind(df, data.frame(p, input[[paste('predict_',p,sep='')]], stringsAsFactors = FALSE))
                  }
            }
            colnames(df) <- c("Predictor", "Value")
            return (df)
      })
      
      
      # Send the hide/show go button value to the UI 
      output$show_predict_go <- reactive({ 
            return (r_values$show_predict_go)
      })
      # This seems to be required to force the update of output$show_predict_go to be send to the UI and control the conditional panel 
      outputOptions(output, 'show_predict_go', suspendWhenHidden=FALSE)      
      
      
      output$out_prediction <- renderTable({ 
            x = 5
            x = 7
            predictor_names <- predict_go()$Predictor
            predictor_values <- predict_go()$Value
            data <- filter_data()
            
            if (length(predictor_names) > 0)
            {
                  model_input <- as.data.frame(t(predictor_values))
                  colnames(model_input) <- predictor_names
                  
                  # Create the models (one per species) for the prediction
                  df <- data.frame(predictor = character(), prediction = numeric(), aic = numeric(), stringsAsFactors = FALSE)
                  withProgress (message="Generating model", value=0, {
                        n <- length(r_values$species_names)
                        for (species in unique(r_values$species_names)) 
                        {
                              incProgress(1/n, detail = species)
                              
                              model_id <- digest::digest(paste(paste(predictor_names, collapse='+'), r_values$filter_species_set, as.character(r_values$filter_species_set_others), paste(sort(r_values$filter_land_use),collapse='+'), species, collapse='+'))
                              if ( length(g_model_full[[species]]$model_id) > 0 && g_model_full[[species]]$model_id == model_id)
                              {
                                    model <- g_model_full[[species]]$model
                              }
                              else 
                              {
                                    formula <- paste('occur ~ ', paste(predictor_names,collapse='+'))
                                    data[,'occur'] <- ifelse (data[,'GENUSSPECI']==species, 1,0)
                                    model <- glm(formula=as.formula(formula),family=binomial(link='logit'),data=data)
                                    g_model_full[[species]]$model_id <<- model_id
                                    g_model_full[[species]]$model <<- model
                              }
                              prediction <- predict(model,model_input, type="response")
                              df <- rbind (df, data.frame(species, prediction, as.integer(model$aic)))
                        }
                  })
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

