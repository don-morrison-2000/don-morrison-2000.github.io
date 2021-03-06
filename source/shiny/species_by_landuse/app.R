#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
# source('D:/CRTI/r_projects/shiny_reports/species_by_landuse/app.R')
# runApp('species_by_landuse')

# Load packages

library(shiny)
library(ggplot2)
library(reshape2)
library(stringr)
library(Hmisc)

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


# Read in the land use names (indexed by the land use ID)
land_use_names <- read.delim('https://don-morrison-2000.github.io/data/land_use.csv', as.is=TRUE, header=FALSE)$V2
# Create a column with the land use name
ctree$lu_cat <- land_use_names[ctree$LU]
# Get the land use categories that have more than 400 trees (we will ignore the rest)
main_lu_cats <- land_use_names[as.integer(names(which(table(ctree$LU)>400)))]


var_descs <- c (
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
var_descs <- var_descs[var_descs %in% colnames(ctree)]
var_descs <- var_descs[order(names(var_descs))]


get_top_spps <- function (ctree, lu_cats, limit)
{
      top_spps_names = NULL
      top_spps_lu = list()
      # Get the top x species for each land use, plus combine them into a single list
      for (lu_cat in lu_cats)
      {
            t <- sort(table(factor(ctree$GENUSSPECI[ctree$lu_cat==lu_cat])), decreasing = TRUE)[1:limit,drop=FALSE]
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
top_spps <- get_top_spps (ctree, main_lu_cats, 10)



get_species <- function(ctree, lu_cats, abundance_level)
{
      spps <- vector('character')
      for (lu_cat in lu_cats)
      {
            spps <- unique(c(spps, na.omit(rownames(top_spps[top_spps[,lu_cat]<=abundance_level,,drop=FALSE]))))
      }
      return (sort(spps))
      
}

assign_quantiles <- function(full=ctree, var='HEIGHT_MEAN', num_bins=5)
{
      # Calculate the break points - evenly spread acress the range
      val_range <- range(full[[var]], na.rm=TRUE)
      min <- as.integer(floor(val_range[1]))
      max <- as.integer(ceiling(val_range[2]))
      
      if (num_bins == 1)
      {
            full$cat[!is.na(full[[var]])] <- paste(min, '-' , max,sep='')
            full$cat[is.na(full[[var]])] <- "Missing"
      }
      else
      {
            # Bin the specified variable
            full$cat <- cut2(full[[var]], g=num_bins)
            # Pretty up the level names
            breaks <- as.character(signif(round(cut2(full[[var]], g=num_bins, onlycuts=TRUE),digits=3),digits=3))
            labels <- vector("character")
            for (i in seq(1,length(breaks)-1))
            {
                  labels <- c(labels, paste(breaks[i],"-",breaks[i+1]))
            }
            levels(full$cat) <- labels
      }
      return(full)
}



# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Species Frequency Comparison by Land Use"),
   fluidRow (
         column (
               width=2,
               wellPanel (
                     sliderInput (inputId = "ui_abundance_level", label = strong("Abundance level"),  min=1, max=10, value=4),
                     checkboxGroupInput (inputId = "ui_land_use", label = strong("Land use"),  choices = main_lu_cats, selected = main_lu_cats),
                     actionButton("ui_clear_land_use", label = strong("Clear"))
               )
         ),
         column (
               width=2,
               wellPanel (
                     checkboxGroupInput (inputId = "ui_species", label = strong("Species"),  choices = character(0), selected = character(0)),
                     actionButton("ui_clear_species", label = strong("Clear"))
               )
         ),
         column (
               width=2,
               wellPanel (
                     selectInput (inputId = "ui_var", label = strong("Measurement"),  choices=var_descs, selected=names(var_descs)[1])
               ),
               wellPanel (
                     sliderInput (inputId = "ui_bins", label = strong("Quantiles"),  min=1, max=10, value=4)
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
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
      
      ################################################################################################################
      # Reactive values
      ################################################################################################################
      r_values <- reactiveValues(
            abundance_level = 0,
            display_species_list = vector("character"),   # List of species to display
            selected_species_list = vector("character"),  # Selected species (including those not dislayed)
            land_use_list = vector("character"),
            flip_chart = TRUE
      )
      
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
      observeEvent(input$ui_clear_land_use, {
            updateCheckboxGroupInput(session, "ui_land_use", choices = main_lu_cats, selected = NULL)
      })

      # Observe the species checkbox list
      observeEvent(input$ui_species, {
            selected <- input$ui_species
            not_selected <- setdiff(r_values$display_species_list, selected)
            r_values$selected_species_list <- setdiff(unique(c(r_values$selected_species_list, selected)), not_selected)
      },ignoreNULL=FALSE)
      
      # Observe the button to clear the selected species
      observeEvent(input$ui_clear_species, {
            r_values$selected_species_list <- NULL
            update_species_list()
      })
 
      # Observe the button to flip the chart
      observeEvent(input$ui_flip_chart, {
            r_values$flip_chart <- !r_values$flip_chart
      })
      
      update_species_list <- reactive({
            r_values$display_species_list <- get_species(ctree, r_values$land_use_list, r_values$abundance_level)
            updateCheckboxGroupInput(session, "ui_species", choices = r_values$display_species_list, selected = r_values$selected_species_list)
      })
            
      
   
      # Plot the chart (note that this is wrapped by renderUI to allow the height to vary)
      output$contents <- renderPlot({ 
            
            # Keep only the trees in the requested set of land uses
            ctree <- ctree[ctree$lu_cat %in% r_values$land_use_list,]
            if(nrow(ctree)==0)
            {
                  return (NULL)
            }
            # Categorize the species by the requested variable
            ctree <- assign_quantiles (ctree, input$ui_var, input$ui_bins)
            
            # Create a data frame to collect the data
            df_cols <- c("Species", "LandUse", levels(ctree$cat))
            fits <- setNames(data.frame(matrix(ncol=length(df_cols), nrow = 0)), df_cols)
            for(sp in input$ui_species)
            {
                  ctree[,'occur'] = ifelse(ctree[,'GENUSSPECI']==sp, 1,0)
                  fit <- tapply(ctree$occur, list(ctree$lu_cat, ctree$cat), mean, na.rm=TRUE)
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
                  # Add vertical separator lines if more than one land use category
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
}

# Run the application 
shinyApp(ui = ui, server = server)

