# Rshiny ideas from on https://gallery.shinyapps.io/multi_regression/

library(shiny)
library(nlme)

fig.width <- 700
fig.height <- 450

ui <-shinyUI(pageWithSidebar(
    
    headerPanel("Three level nested variance components analysis"),
    
    
    sidebarPanel(
        
        div(p("Frequentist modelling and a plot of the data.")),
        
        div(
            
            selectInput("model",
                        strong(""),
                        choices=c("Linear Mixed Model" )),
            
            div(p("Select the true population parameters and a 3 level nested data set is simulated (no missing data). A plot of the raw data is generated. 
                  A model is fit to estimate the variance components. You also have the choices of selecting a new sample.")),
            
            
            br(),
            actionButton("resample", "Simulate a new sample"),
            br(),
            br(),
            actionButton(inputId='ab1', label="R code here", 
                         icon = icon("th"), 
                         onclick ="window.open('https://raw.githubusercontent.com/eamonn2014/Three-level-nested-variance-components-analysis/master/three-level-nested-variance-components-analysis/app.R', '_blank')"),
            
            br(),
            br(),
            #p(strong("Generate true population parameters:")),
            sliderInput("top",
                        "Number of levels in top component",
                        min=2, max=100, step=1, value=9, ticks=FALSE),
            sliderInput("middle",
                        "Number of levels nested within top component",
                        min=2, max=100, step=1, value=11, ticks=FALSE),
            sliderInput("lower",
                        "Number of levels nested within middle component",
                        min=2, max=100, step=1, value=6, ticks=FALSE),
            sliderInput("replicates",
                        "Number of replicates nested within lower level",
                        min=2, max=100, step=1, value=8, ticks=FALSE),
            sliderInput("intercept",
                        "True intercept",
                        min=0, max=1000, step=.5, value=100, ticks=FALSE),
            sliderInput("a",
                        "True top level SD",
                        min=1, max=100, step=.5, value=20, ticks=FALSE),
            sliderInput("b",
                        "True middle level SD",
                        min=1, max=100, step=.5, value=2, ticks=FALSE),
            sliderInput("c",
                        "True lower level SD",
                        min=1, max=100, step=.5, value=2, ticks=FALSE),
            sliderInput("d",
                        "True error",
                        min=1, max=100, step=.5, value=2, ticks=FALSE)
        )
    ),
    
    mainPanel(
        
        div(plotOutput("reg.plot", width=fig.width, height=fig.height)),
        p(strong("Linear Mixed Model output:")),
        div(class="span7", verbatimTextOutput("reg.summary"))
    )
))


server <- shinyServer(function(input, output) {
    
    # --------------------------------------------------------------------------
    # This is where a new sample is instigated only random noise is required to be generated
    random.sample <- reactive({
        # Dummy line to trigger off button-press
        foo <- input$resample
        
        N <- input$top*input$middle*input$lower*input$replicates
        A <- input$top
        B <- input$top*input$middle
        C <- input$top*input$middle*input$lower
        
        noise1 <- rnorm(A, input$intercept, input$a)  # between
        noise2 <- rnorm(B, 0,               input$b)  # within
        noise3 <- rnorm(C, 0,               input$c)  # between
        noise4 <- rnorm(N, 0,               input$d)  # within
        
        return(list(noise1=noise1, noise2=noise2, noise3=noise3, noise4=noise4, A=A, B=B, C=C, D=D, N=N))
    })
    
    # --------------------------------------------------------------------------
    # Set up the dataset based on the inputs 
    make.regression <- reactive({
        
        sample <- random.sample()
        
        N <-  sample$N
        A <-  sample$A
        B <-  sample$B
        C <-  sample$C
        w <-  sample$noise1
        x <-  sample$noise2
        y <-  sample$noise3
        z <-  sample$noise4
        
        to <-  rep (1:A, each = input$middle*input$lower*input$replicates )
        day <- rep (1:B, each = input$lower*input$replicates ) 
        run <- rep (1:C, each = input$replicates ) 
        means <- w[to] + x[day] + y[run]
        Response <- rnorm(  N, means,  input$d)
        
        Data <- data.frame( top=as.factor(to),
                            day=as.factor(day), 
                            run=as.factor(run),
                            Response = Response)
        
        df <- as.data.frame(Data)
        return(list(df=df , to=to, day=day, run=run))   
        
    })  
    
    
    # --------------------------------------------------------------------------
    # Fit the specified regression model
    fit.regression <- reactive({
        
        p1 <- function(x) {formatC(x, format="f", digits=1)}
        # Get the current model structure
        data <- make.regression()
        
        df <- data$df
        
        # Conditionally fit the model
        if (input$model == "Linear Mixed Model") {
            
            fit.res <-  
                tryCatch(intervals(lme(Response ~ 1, random = ~1 |  top/day/run , data=df, method="REML")), 
                         error=function(e) e)
            
            ###http://stackoverflow.com/questions/8093914
            ###/skip-to-next-value-of-loop-upon-error-in-r-trycatch
            
            if (!inherits(fit.res, "error")) {
                
                modelint <- fit.res
                
                emu      <-p1(modelint[['fixed']][2][[1]]    )  
                etop     <-p1(modelint[['reStruct']][['top']][2][[1]])
                eday     <-p1(modelint[['reStruct']][['day']][2][[1]])
                erun     <-p1(modelint[['reStruct']][['run']][2][[1]])
                esigma   <-p1(modelint[['sigma']][2][[1]])
                
                
            } else  {
                
                fit.res <- NULL
                
            }  
        }
        
        
        # Get the model summary
        if (is.null(fit.res)) {
            fit.summary <- NULL
        } else {
            fit.summary <-  (fit.res)
        }
        
        return(list(emu=emu, etop=etop, eday=eday, erun=erun, esigma=esigma, fit.res=fit.res, fit.summary=fit.summary))
        
    })     
    
    #---------------------------------------------------------------------------
    # Plot a scatter of the data  
    
    
    
    output$reg.plot <- renderPlot({         
        
        # Get the current regression data
        data1 <- make.regression()
        
        level1. <- data1$to 
        level2. <- data1$day  
        level3. <- data1$run  
        
        
        d1 <- data1$df
        
        fit <- fit.regression()
        
        emu <- fit$emu
        etop <- fit$etop
        eday <- fit$eday
        erun <- fit$erun
        esigma <- fit$esigma
        
        require(reshape)
        d1$grp<-paste(level1. ,level2., level3., sep=".")
        d1 <- rename(d1, c(Response="count"))
        d1 <- rename(d1, c(grp="spray"))
        d1 <- d1[order(d1$spray),]
        
        limit<-dim(table(d1$spray))
        
        attach(d1)
        sprayTypes <- unique(spray)
        
        y <- as.numeric(as.character(d1[,4])) 
        
        
        plot(y, as.factor(d1[,4]) ,  ylim=c(min(y),max(y)) , xlim=c(1, limit) , xaxt="n",
             main=paste("Plot of the data. Truth: intercept ",input$intercept,"(",emu,"), top level sd=",
                        input$a,"(",etop,")", ",\n middle level sd=",
                        input$b ,"(",eday,"), lowest level sd=",
                        input$c, "(",erun,") & random error sd=", 
                        input$d,"(",esigma,")"),
             xlab=paste("Coloured by",input$top,"top factor levels x",input$middle,"x",input$lower,"x",input$replicates,"reps (LMM estimates in brackets)"), 
             ylab="Response", frame.plot=F , col="white")  
        
        
        axis(1,  at=1:limit,labels=F  )
        
        
        ##make colours
        chk<-as.character(d1$spray)
        x<-as.data.frame(table(chk))
        freq<-x[,2]
        value<-max(dput(1:dim(x)[1]))
        IR<-value+1
        clr<-rainbow(IR)
        #clr<-rep(rainbow(l1*l2), each=l2)  #new
        wR<-(2*IR-1:IR)/(2*IR)
        ##colour made
        for (i in 1 : length(sprayTypes)){
            y <- count[spray == sprayTypes[i]]
            n <- sum(spray == sprayTypes[i])
            points(jitter(rep(i, n), amount = .1), y, pch = 16, cex = .5,
                   
                   col=clr[i]
            )
            
            lines(i + c(.12, .28), rep(mean(y), 2), lwd = 1, col="black")
            lines(rep(i + .2, 2),
                  mean(y) + c(-1.96, 1.96) * sd(y) / sqrt(n),  lwd = 1, col="black"
                  
            )
        }
        
        detach(d1)
        
        
    })
    
    
    
    #---------------------------------------------------------------------------
    # Show the summary for the 
    output$reg.summary <- renderPrint({
        
        summary <- fit.regression()$fit.summary
        
        if (!is.null(summary)) {
            
            return(fit.regression()$fit.summary)
            
        }
        
    })
    
    
    
})



# Run the application 
shinyApp(ui = ui, server = server)