# ------------------------------------------------------------------------------
# Book:         Test Book
# ------------------------------------------------------------------------------
# Quantlet:     MMSTAThistogram_simple
# ------------------------------------------------------------------------------
# Description:  It produces an interactive interface to show the histogram. In 
#               the most basic version only a histogram of the TOTALAREA from
#               the USCRIME data set is shown. The user can interactively choose
#               the number of bins. The user can choose the variables and switch
#               to the other data set including CARS and DECATHLON.
# ------------------------------------------------------------------------------
# Datafiles:    CARS.rds, DECATHLON.rds, USCRIME.rds
# ------------------------------------------------------------------------------
# Inputs:       MMSTAThelper_function
#               Options: interactive user choice
# ------------------------------------------------------------------------------
# output:       Interactive shiny application
# ------------------------------------------------------------------------------
# Example:      The given application example of MMSTAThistogram_simple shows a
#               histogram of the USCRIME data set with TOTALAREA as selected 
#               variable. One can see the bins (in gray) in the histogram. The
#               number of bins used in this example is equal to 30.
# ------------------------------------------------------------------------------
# See also:     COPdaxnormhist, COPdaxreturnhist, BCS_Hist2, BCS_Hist1,
#               MMSTATtime_series_1, MMSTATlinreg, MMSTATconfmean, 
#               MMSTATconfi_sigma, MMSTATassociation, MMSTAThelper_function
# ------------------------------------------------------------------------------
# Keywords:     Parameters, data visualization, empirical, parameter, 
#               parametric, visualization, variable selection, frequency,
#               histogram, uscrime
# ------------------------------------------------------------------------------
# Author:       Yafei Xu
# ------------------------------------------------------------------------------ 



# please use "Esc" key to jump out the run of Shiny app
# clear history and close windows
# rm(list = ls(all = TRUE))
graphics.off()


# please set working directory setwd('C:/...') 

# setwd('~/...')    # linux/mac os
# setwd('/Users/...') # windows
source("MMSTAThelper_function.r")


##############################################################################
############################### SUBROUTINES ##################################
### server ###################################################################
##############################################################################

mmstat$vartype = 'numvars'

mmstat.ui.elem("bins", 'sliderInput',
               label = gettext("Number of bins:"),
               min   = 1, 
               max   = 50, 
               value = 30)
mmstat.ui.elem("obs", 'checkboxInput',
               label = gettext("Show observations"), value = FALSE)
mmstat.ui.elem("dataset", "dataSet",     
               choices = mmstat.getDataNames("USCRIME", "CARS", "DECATHLON"))
mmstat.ui.elem("variable", "variable1",   
               vartype = "numeric")
mmstat.ui.elem("cex", "fontSize")

server = shinyServer(function(input, output, session) {
  
  output$binsUI     = renderUI({ mmstat.ui.call('bins') })
  output$obsUI      = renderUI({ mmstat.ui.call('obs') })
  output$datasetUI  = renderUI({ mmstat.ui.call('dataset') })
  output$cexUI      = renderUI({ mmstat.ui.call('cex')  }) 
  output$variableUI = renderUI({ inp  = mmstat.getValues(NULL, dataset = input$dataset)
                                 mmstat.ui.call('variable',
                                                choices = mmstat.getVarNames(inp$dataset, 'numeric'))
  })   
  
  getVar = reactive({
    mmstat.log(paste('getVar'))
    var             = mmstat.getVar(isolate(input$dataset), input$variable)
    dec             = mmstat.dec(c(var$mean, var$median))
    var$decimal     = dec$decimal
    var[['pos']]    = 2*(var$mean<var$median)
    var
  })
  
  output$distPlot = renderPlot({
    var    = getVar()
    input  = mmstat.getValues(NULL, bins = input$bins, cex = input$cex, obs = input$obs)
    bins   = seq(var$min, var$max, length.out = as.numeric(input$bins) + 1)
    hist(var$values,
         breaks   = bins,
         col      = "grey", 
         xlab     = var$xlab,
         main     = gettext("Histogram"), 
         sub      = var$sub,
         ylab     = gettext("Absolute frequency"),
         cex.axis = input$cex,
         cex.lab  = input$cex,
         cex.main = 1.2*input$cex,
         cex.sub  = input$cex,
         axes     = F)
    usr = par("usr")
    mmstat.axis(1, usr[1:2], cex.axis = input$cex)
    mmstat.axis(2, usr[3:4], cex.axis = input$cex)    
    if (input$obs) rug(var$values, lwd = 1)
    box()
  })
  
  output$logText = renderText({
    mmstat.getLog(session)
  })
})


##############################################################################
############################### SUBROUTINES ##################################
### ui #######################################################################
##############################################################################

ui = shinyUI(fluidPage(
  div(class = "navbar navbar-static-top",
      div(class = "navbar-inner", 
          fluidRow(column(6, div(class = "brand pull-left", 
                                 gettext("Simple Histogram"))),
                   column(2, checkboxInput("showbins", 
                                           gettext("Histogram parameter"), 
                                           TRUE)),
                   column(2, checkboxInput("showdata", 
                                           gettext("Data choice"), 
                                           FALSE)),
                   column(2, checkboxInput("showoptions", 
                                           gettext("Options"), 
                                           FALSE))))),
    
  sidebarLayout(
    sidebarPanel(
      conditionalPanel(
        condition = 'input.showbins',
        uiOutput("binsUI"),
        br(),
        uiOutput("obsUI")
      ),
      conditionalPanel(
        condition = 'input.showdata',
        hr(),
        uiOutput("datasetUI"),
        uiOutput("variableUI")
      ),
      conditionalPanel(
        condition = 'input.showoptions',
        hr(),
        uiOutput("cexUI")
      )
    ),
    mainPanel(plotOutput("distPlot"))),

  htmlOutput("logText")  
))
##############################################################################
############################### SUBROUTINES ##################################
### shinyApp #################################################################
##############################################################################

shinyApp(ui = ui, server = server)

#
