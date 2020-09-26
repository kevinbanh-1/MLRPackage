# Load libraries needed
library(shiny)
library(rgl)
library(plotly)
#library(ggplot2)
library(plot3D)
library(ggplot2)
library(dplyr)
library(purrr)
library(rootSolve)
library(readr)
library(mathjaxr)
source("Rcode.r")



spruce.file <- "./data/SPRUCE.csv"
spruce.df <- read_csv(file = spruce.file,locale=locale())
d = spruce.df$BHDiameter



# Define UI for application that draws a histogram
ui <- fluidPage(


   # Application title
   titlePanel("Spruce Data Set: Piecewise Regression"),

   # Sidebar with a slider input for number of bins

   sidebarLayout(
      sidebarPanel(
         sliderInput("xk",
                     "Choose knot 1:",
                     min = min(d),
                     max = max(d),
                     value = 11.41873,
                     step=0.01),

         sliderInput("xk2",
                     "Choose knot 2: ",
                     min = min(d),
                     max = max(d),
                     value = 18.88361,
                     step=0.01),

         sliderInput("intervalroot",
                     "choose L and U for root interval:",
                     min = min(d),
                     max = max(d),
                     value = c(15,17.55),
                     step=0.01),
         sliderInput("filterR2",
                     "choose filter level",
                     min = 0.00,
                     max = 1.00,
                     value = 0.00,
                     step=.01),
         sliderInput("hlevel",
                     "choose convergence level",
                     min=0.00,
                     max=1.00,
                     value=0.00,
                     step=.01)

      ),

      # Show a plot of the generated distribution
      mainPanel(
         uiOutput("formula"),
         plotOutput("regressPlot"),
         plotlyOutput("plot"),
         plotOutput("R2"),
         tableOutput("root"),
         # table of data
         tableOutput("multroot"),
         tableOutput("tab"),
         plotOutput("allroots"),

      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   # For Rgl


   output$formula <- renderUI({
      withMathJax(
         helpText(
            '$$y = \\beta_0 + \\beta_1x + \\beta_2(x-x_k)*I*(x > x_k) + \\beta_3(x-x_{k2})*I*(x>x_{k2})$$'
            )
      )
   })
   output$tab <- renderTable(spruce.df)

   output$regressPlot <- renderPlot({
     plot(spruce.df,main="Piecewise regression",pch=21,bg="black")

     # Edited, add xk2
      sp2.df=within(spruce.df, X<-(BHDiameter-input$xk)*(BHDiameter>input$xk) + (BHDiameter-input$xk2)*(BHDiameter>input$xk))
      lmp = lm(Height ~ BHDiameter + X, data = sp2.df)
      tmp=summary(lmp) # tmp holds the summary info


      # Edited, add xk2
      # Save for later, once we calculate the 2 knots, use coef(xk,xk2) then put it into the curve
      curve(myf2(x,xk=input$xk,xk2=input$xk2,coef=tmp$coefficients[,"Estimate"] ),
            add=TRUE,
            lwd=2,
            col="Orange")


      # Edited, add xk2
     points(input$xk,myf(input$xk,input$xk2,coef=tmp$coefficients[,"Estimate"] ),col="black",pch=21,bg="green",cex=2)



     points(uroot()$root,myf(uroot()$root,uroot()$root,coef=tmp$coefficients[,"Estimate"]),col="black",pch=21,bg="purple",cex=2)




      text(input$xk,16,
           paste("R sq.=",round(tmp$r.squared,4) ))


   })

   uroot = reactive({
     intv = input$intervalroot
     uniroot(f=rsqdash, interval=intv, h=0.001,data=spruce.df, extendInt = "yes" )
   })

   urootall = reactive({
     intv = input$intervalroot
     uniroot.all(f=rsqdash, interval=intv, h=0.001,data=spruce.df )
   })



      # Data for widget

      knots <- seq(min(d),max(d),length=300)
      grid <- expand.grid(knots,knots)
      class(grid)

      z = map2_dbl(grid$Var1,grid$Var2, ~rsq2(.x,.y,data=spruce.df))

      df <- data.frame(
         x=grid$Var1,
         y=grid$Var2,
         z=z
      )

      #filterR2 <- reactive({
      #   filterR2 <- input$filterR2

      #})

      diff <- df %>% filter(z >= .78)
      output$plot <- renderPlotly({
         p <- with(df,
               plot_ly(x=~diff$x,
                       y=~diff$y,
                        z=~diff$z,
                       type="mesh3d"),
              vertexcolor= rbg(z,1-z,z^2))

      p <- p %>% layout(
         title = expression(R^2, "values of knots 1 and 2"),
         scene = list(
            xaxis=list(title = "knot1"),
            yaxis=list(title= "knot2"),
            zaxis=list(title= "R^2")
         )
      )

      p


   })


      #output$multroot <- renderTable({
      #   msq.eqn = multirsq(input$xk,input$xk2,h=.01,data=spruce.df)
      #   mr = multiroot(f=msq.eqn,start=c(1,1),maxiter = 100)
      #   as.data.frame(mr)
      #})






}

# Run the application
shinyApp(ui = ui, server = server)

