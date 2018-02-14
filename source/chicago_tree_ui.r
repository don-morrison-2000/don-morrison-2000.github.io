
# source('D:/CRTI/r_projects/chicago_tree_dam/chicago_tree_ui.r')


# runApp('chicago_tree_ui.r')

# Load packages
library(shiny)
library(shinythemes)
library(ggplot2)


# Get data and helper functions from Richard's program
#source('D:/CRTI/r_projects/chicago_tree_dam/chicagotree_dam.r')
source("https://don-morrison-2000.github.io/source/chicagotree_dam.r")
ctree <- readChicagoTree()
land_use <- landUse
species <- c(commonSpecies$V1, "Other")

# Coerce all non-common species names to "Other"
ctree[['GENUSSPECI']] <- ifelse ((match(ctree[['GENUSSPECI']], commonSpecies[[1]], nomatch = 0) > 0), ctree[['GENUSSPECI']], "Other")

species_set = "Common species"
all_predictors = c('BLDG_AGE','HU_DENS', 'DIST_WATER', 'DIST_MINOR', 'DIST_MAJOR', 'PD', 'LPI', 'LSI', 'CROWN_AREA', 'CROWN_COMPACTNESS', 'HEIGHT_MAX', 'HEIGHT_MEAN', 'RELBORDER_TREE', 'DBH_IN')
all_land_use = c(0:11)

# Cache values
g_models=list()

# Models are maintined in a list with this format [[<<predictor formula]][[<species name]][[<land use desc>]]
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
        if (!is.null(land_use))
        {
            ctree <- subset(ctree, LU %in% land_use)
        }
        if (nrow(ctree > 0))
        {
            ctree[,'occur'] <- 0
            ctree[ctree[,'GENUSSPECI']==species,'occur'] <- 1
            model=glm(formula=as.formula(formula),family=binomial(link='logit'),data=ctree)
            model <- list(cf=summary(model)$coefficients,aic=summary(model)$aic, species=species, land_use=land_use_desc, sample_size=sum(ctree$occur))
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

      # Row that shows the probability plot
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
                        wellPanel
                        (  
                            width=8, style = "overflow-y:scroll; min-height: 350px; max-height: 350px",
                            plotOutput(outputId = "probability_plot", width = '600px', height = "300px")
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

      
      # Row with the parameter settings
      fluidRow 
      (
          column 
          ( 
              3,
              wellPanel
              (  
                  p(strong("Model Predictors")),
                  checkboxInput (inputId = "all_model_predictors", label = strong("All"), value = TRUE),
                  conditionalPanel
                  ( 
                      condition = 'input.all_model_predictors == false', 
                      checkboxGroupInput (inputId = "model_predictors", label = '', choices = all_predictors, selected = all_predictors[1])
                  )
              )
          ),
          column 
          ( 
              3,
              wellPanel
              (  
                  radioButtons (inputId = "plot_predictor", label = strong("Plot Predictor"), choices = all_predictors, selected = all_predictors[1])
              )
          ),
          column 
          ( 
              3,
              wellPanel
              (  
                  p(strong("Land Use")),
                  checkboxInput (inputId = "all_land_use", label = strong("All"), value = TRUE),
                  conditionalPanel
                  ( 
                      condition = 'input.all_land_use == false', 
                      checkboxGroupInput (inputId = "land_use", label ='' , choices = unique(land_use$V2), selected = unique(land_use$V2[1]))
                  )
              )
          ),
          column 
          ( 
              3,
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

      # Update the plot predictors list when the model predictors is updated
      observeEvent ( c(input$all_model_predictors,input$model_predictors), {
        
          m_preds = if (input$all_model_predictors == TRUE) all_predictors else input$model_predictors
          if (!is.null(m_preds))
          {
              p_choices = m_preds
              p_selected = if (input$plot_predictor %in% m_preds) input$plot_predictor else m_preds[1]
          }
          else
          {
              p_choices = character(0)
              p_selected = character(0)
          }

          updateRadioButtons(session, "plot_predictor", choices=p_choices, selected=p_selected)
        
    })
    
    # Figure out the intended model predictors
    resolve_m_preds <- reactive({ 
        m_preds <- if (input$all_model_predictors == TRUE) all_predictors else input$model_predictors
        return (m_preds)
    })
 
    # Map land use description to its numeric index
    resolve_land_use_desc <- reactive({ 
        if (input$all_land_use == TRUE)
        {
            return (all_land_use)
        }
        else
        {
            return (as.numeric(landUse$V1[match(input$land_use,landUse$V2)]) - 1)
        }
    })
    
    # Build the models for the coordinates that need to be plotted
    get_models <- reactive({ 
        m_preds <- resolve_m_preds()
        land_use <- resolve_land_use_desc()
        species_list <- input$species
        
        # Make sure all paramters are set
        if (is.null(m_preds)|| is.null(input$species) || is.null(land_use) || length(land_use)==0)
        {
            return (NULL)   
        }
        
        models <- list()

        for (species in species_list)
        {
            models[[species]] <- build_model (ctree, m_preds, species, land_use)
        }
        return (models)
    })
    
    
    
    # Plot the probabilities
    output$probability_plot <- renderPlot({ 
        m_preds <- resolve_m_preds()
        p_pred <- input$plot_predictor
        land_use <- resolve_land_use_desc()
        species_list <- input$species
        
        # Make sure all paramters are set
        if (is.null(m_preds)|| is.null(p_pred)||is.null(input$species) || is.null(land_use) || length(land_use)==0 || !(p_pred %in% m_preds))
        {
            return (NULL)     
        }
        regression_coords <- data.frame(x=numeric(), y=numeric(), species_set==character(), species=character(), model_predictors=character(), plot_predictor=character())
        occurrence_coords <- data.frame(x=numeric(), y=numeric(), species=character(), species=character(), model_predictors=character(), plot_predictor=character())    
 
        models <- get_models()
        for (species in species_list)
        {
            if (!is.null(models[[species]]))
            {
                a <- graphOneResult(full=ctree,mod=models, sp=species,predictor=p_pred, retSpecs=TRUE)
                regression_coords <- rbind (regression_coords, data.frame(a[[1]], species_set=rep(species_set, nrow(a[[1]])), species=rep(species, nrow(a[[1]])), model_predictors=rep(m_preds,nrow(a[[1]])), plot_predictor=rep(p_pred,nrow(a[[1]]))))
                occurrence_coords <- rbind (occurrence_coords, data.frame(a[[2]], species_set=rep(species_set, nrow(a[[2]])), species=rep(species, nrow(a[[2]])), model_predictors=rep(m_preds,nrow(a[[2]])), plot_predictor=rep(p_pred,nrow(a[[2]]))))
            }
        }
        ggplot () +
            scale_x_continuous(name='') +
            scale_y_continuous(limits=c(0,1), name="Probability of occurence") +
            geom_line(data=regression_coords, aes(x=x, y=y, group=species, colour=species)) +
            geom_point(data=occurrence_coords, aes(x=x, y=y, group=species, colour=species))
    })
    
    output$outText <- renderTable({ 
        col_names <- c('Species', 'Samples', 'Predictor','Estimate', 'Std. Error', 'z value', 'Pr(>|z|)')
        
        stats <- data.frame(species=character(), sample_size=character(), predictor=character(), estimate=numeric(), std_error=numeric(), z_value=numeric(), pr=numeric())
        colnames(stats) <- col_names
        
        models <- get_models()
        for (model in models)
        {
            if (input$plot_predictor %in% rownames(model$cf))
            {
                # Extract the model statistics summary
                m <- model$cf[input$plot_predictor,]
                # Populate the statistics table row
                r = array(0,dim=c(1,length(col_names)))
                r[1,1] <- model$species
                r[1,2] <- model$sample_size
                r[1,3] <- input$plot_predictor
                r[1,4] <- signif(m[1],4)
                r[1,5] <- signif(m[2],4)
                r[1,6] <- signif(m[3],4)
                r[1,7] <- signif(m[4],4)
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