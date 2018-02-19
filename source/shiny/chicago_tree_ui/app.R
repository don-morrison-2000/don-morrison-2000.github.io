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

# Load packages
library(shiny)
library(shinythemes)
library(ggplot2)

chicago_tree_app_local_file <- 'D:/CRTI/r_projects/chicago_tree_dam/chicagotree_dam.r'
chicago_tree_app_url <- 'https://don-morrison-2000.github.io/source/chicagotree_dam.r'

ctree_local_file <- 'D:/CRTI/data/cleaned/dupage_county_accepted_V1.csv'
ctree_http_url <- 'https://don-morrison-2000.github.io/data/dupage_county_accepted_V1.csv'

# Source Richeard's modeling proram (either from local disk or the http server)
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
      ctree <- readChicagoTree(path='', infile=ctree_local_file)
} else
{
      ctree <- readChicagoTree(path='', infile=ctree_http_url)
}

# These come from Richard's modeling program
land_use <- landUse
species <- c(commonSpecies$V1, "Other")
#species <- c("Fraxinus pennsylvanica", "Acer saccharinum", "Gleditsia triacanthos", "Tilia americana", "Other")

# Coerce all non-common species names to "Other"
ctree[['GENUSSPECI']] <- ifelse ((match(ctree[['GENUSSPECI']], species, nomatch = 0) > 0), ctree[['GENUSSPECI']], "Other")
# Convert model categories to factors that match the UI names
ctree[['STREETTREE']] <- as.factor(ifelse (ctree[['STREETTREE']]=='Y', "Street", ifelse (ctree[['STREETTREE']]=='N', "Non-street", "Unknown")))
ctree[['LU']] <- as.factor(landUse$V2[as.numeric(as.character(ctree$LU))+1])


species_set <- "Common species"


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
#all_land_use <- unique(land_use$V2)


# Model cache
g_models <- list()


# Models are maintined in a list with this format [[<<predictor formula]][[<species name]][[<land use desc>]]
build_model <- function (ctree, predictors, species, land_use, street_tree)
{
      formula <- paste('occur ~ ', paste(predictors,collapse='+'))
      land_use_desc <- paste(sort(land_use), collapse="+")
      street_tree_desc <- paste(sort(street_tree), collapse="+")
      # Check if the model already exists in the cache
      if (!is.null(g_models[[formula]][[land_use_desc]][[street_tree_desc]][[species]]) )
      {
            return (g_models[[formula]][[land_use_desc]][[street_tree_desc]][[species]])
      }
      else
      {
#            if (!is.null(land_use))
#            {
#                  ctree <- subset(ctree, LU %in% land_use)
#            }
            if (nrow(ctree) > 0)
            {
                  ctree[,'occur'] <- 0
                  ctree[ctree[,'GENUSSPECI']==species,'occur'] <- 1
                  model <- glm(formula=as.formula(formula),family=binomial(link='logit'),data=ctree)
                  model <- list(cf=summary(model)$coefficients,aic=summary(model)$aic, species=species, land_use=land_use_desc, sample_size=sum(ctree$occur))
                  # Cache the model in the global space - note the super-assign operator '<<-'
                  g_models[[formula]][[land_use_desc]][[street_tree_desc]][[species]] <<- model        
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
                
                # Row that shows the probability plot and related statistics
                fluidRow 
                (
                      column 
                      ( 
                            width=12,
                            tabsetPanel
                            (
                                  tabPanel
                                  (
                                        title='Plot',
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
                                                    checkboxInput (inputId = "plot_observations", label = strong("Observations"), value=FALSE)
                                              )
                                        )
                                  ),
                                  tabPanel
                                  (
                                        title='Statistics',
                                        wellPanel
                                        (  
                                              width=12, style = "overflow-y:scroll; min-height: 350px; max-height: 350px",
                                              tableOutput(outputId = "outText")
                                        )
                                  )
                            )
                      )
                ),
                
                
                # Row with the input parameter settings
                fluidRow 
                (
                      column 
                      ( 
                            width=3,
                            wellPanel
                            ( 
                                  tags$div(class="header", tags$p(), tags$strong("Data Filters")), 
                                  tags$hr(style="height:1px;border:none;color:#333;background-color:#333;"),
                                  checkboxInput (inputId = "filter_street_on", label = strong("Street Tree"), value = FALSE),
                                  conditionalPanel
                                  ( 
                                        condition = 'input.filter_street_on == true', 
                                        checkboxGroupInput (inputId = "filter_street", label = '', choices = levels(ctree$STREETTREE), selected = levels(ctree$STREETTREE))
                                  ),
                                  tags$hr(style="height:1px;border:none;color:#333;background-color:#333;"),
                                  checkboxInput (inputId = "filter_land_use_on", label = strong("Land Use"), value = FALSE),
                                  conditionalPanel
                                  ( 
                                        condition = 'input.filter_land_use_on == true', 
                                        checkboxGroupInput (inputId = "filter_land_use", label = '', choices = levels(ctree$LU), selected = levels(ctree$LU))
                                  )
                            )
                      ),
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
#                      column 
#                      ( 
#                            width=2,
#                            wellPanel
#                            (  
#                                  p(strong("Land Use")),
#                                  checkboxInput (inputId = "all_land_use", label = strong("All"), value = TRUE),
#                                  conditionalPanel
#                                  ( 
#                                        condition = 'input.all_land_use == false', 
#                                        checkboxGroupInput (inputId = "land_use", label ='' , choices = unique(land_use$V2), selected = unique(land_use$V2[1]))
#                                  )
#                            )
#                      ),
                      column 
                      ( 
                            width=3,
                            wellPanel
                            ( 
                                  checkboxGroupInput (inputId = "species", label = strong("Species"), choices = unique(species), selected = unique(species)[1])
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
                  filter_street = levels(ctree$STREETTREE), # Street tree setting included in data filter
                  filter_land_use = levels(ctree$LU),       # Land use settings included in data filter
                  m_preds = NULL,                           # Ordered list of selected model predictors
                  p_pred = NULL,                            # Single model predictor selected to be plotted
                  species_list = NULL,                      # List of species selected to be plotted 
                  x = NULL,                                 # Zoomable x coordinate
                  y = NULL)                                 # Zoomable y coordinate

      
      ################################################################################################################
      # Observers
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
      
      # Observe the street tree filter checkbox UI
      observeEvent (input$filter_street_on, {
            # Update the reactive value to match the UI
            if (input$filter_street_on == TRUE)
            {
                  # The filter is turned on - restore the filter values
                  r_values$filter_street <- input$filter_street
            }
            else
            {
                  # The filter is turned off - include all
                  r_values$filter_street <- levels(ctree$STREETTREE)
            }
      })
      
      # Observe the street tree filter selection UI
      observeEvent (input$filter_street, {
            if (is.null(input$filter_street))
            {
                  # All values were deselected - do not allow this
                  updateCheckboxGroupInput(session, "filter_street", choices = levels(ctree$STREETTREE), selected = c(r_values$filter_street))
            }
            else
            {
                  r_values$filter_street <- input$filter_street
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
      
      
      
      ################################################################################################################
      # Reactive functions
      ################################################################################################################

      # Subset the data 
      filter_data <- reactive({ 
            return (subset(ctree, LU %in% r_values$filter_land_use & STREETTREE %in% r_values$filter_street))
      })
      
      # Build the models for the coordinates that need to be plotted
      get_models <- reactive({ 
            # Make sure all paramters are set
            if (is.null(r_values$m_preds)|| is.null(r_values$species_list) || is.null(r_values$filter_land_use) || length(r_values$filter_land_use)==0)
            {
                  return (NULL)   
            }
            
            models <- list()
            for (species in r_values$species_list)
            {
                  models[[species]] <- build_model (filter_data(), r_values$m_preds, species, r_values$filter_land_use, r_values$filter_street)
            }
            return (models)
      })
      
      
      ################################################################################################################
      # Inputs
      ################################################################################################################
      
      
      
      # Plot the probabilities
      output$probability_plot <- renderPlot({ 
            
            # Make sure all paramters are set
            if (is.null(r_values$m_preds) || is.null(r_values$p_pred) ||is.null(r_values$species_list) || !(r_values$p_pred %in% r_values$m_preds))
            {
                  return (NULL)     
            }
            
            regression_coords <- data.frame(x=numeric(), y=numeric(), species_set==character(), species=character(), plot_predictor=character())
            occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character(), species=character(), plot_predictor=character())    
            
            models <- get_models()
            for (species in r_values$species_list)
            {
                  if (!is.null(models[[species]]))
                  {
                        a <- graphOneResult(full=filter_data(),mod=models, sp=species,predictor=r_values$p_pred, retSpecs=TRUE)
                        regression_coords <- rbind (regression_coords, data.frame(a[[1]], species_set=rep(species_set, nrow(a[[1]])), species=rep(species, nrow(a[[1]])), plot_predictor=rep(r_values$p_pred,nrow(a[[1]]))))
                        occurrence_coords <- rbind (occurrence_coords, data.frame(a[[2]], species_set=rep(species_set, nrow(a[[2]])), species=rep(species, nrow(a[[2]])), plot_predictor=rep(r_values$p_pred,nrow(a[[2]]))))
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
            
            col_names <- c('Species', 'Samples', 'Predictor','Estimate', 'Std. Error', 'z value', 'Pr(>|z|)', 'AIC')
            stats <- data.frame(species=character(), sample_size=character(), predictor=character(), estimate=numeric(), std_error=numeric(), z_value=numeric(), pr=numeric(), aic=numeric())
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
                        colnames(r) <- col_names
                        # Add the row to the table
                        stats <- rbind (stats, as.data.frame(r))
                  }
            }
            return (stats)
      })
      
      
}

# Create Shiny object
shinyApp(ui = ui, server = server)

