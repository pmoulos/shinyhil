# server.R

# Load required libraries
library(shiny)
library(GenomicAlignments)
#library(HilbertVis)

# Load additional functions
source("helpers.R")

# Set some global shiny options
# Increase max upload size so as to accomodate BAM files
options(shiny.maxRequestSize=10000*1024^2)

shinyServer(
    function(input,output,session) {
        
        # Intitiate list of observers
        session$observers <- list()
        # Record data sources
        session$bam <- initDataSource("bam")
        session$link <- initDataSource("link")
        # Record data source changes
        session$bamHitCount <- session$linkHitCount <- 0
        # Record displayed messages
        session$messages <- NULL
        
        # Set up response to clear uploaded file buttons
        output$bam_1 <- renderUI({
            input$clear_bam_1
            clearDataSource(session,"bam",1)
            fileInput(
                inputId="bam_1",label=""
            )
        })
        output$bam_2 <- renderUI({
            input$clear_bam_2
            fileInput(
                inputId="bam_2",label=""
            )
        })
        output$bam_3 <- renderUI({
            input$clear_bam_3
            fileInput(
                inputId="bam_3",label=""
            )
        })
        
        # Setup clear link button listeners (observers in shiny terms)
        session$observers$link_1 <- observe({
            input$clear_link_1
            updateTextInput(session,"link_1",value="")
        })
        session$observers$link_2 <- observe({
            input$clear_link_2
            updateTextInput(session,"link_2",value="")
        })
        session$observers$link_3 <- observe({
            input$clear_link_3
            updateTextInput(session,"link_3",value="")
        })
        
        # Respond to change in organism and repopulate chromosomes
        output$setChrs <- renderUI({
            selectInput(
                inputId="chromosomes",
                label="",
                choices=getValidChromosomes(input$organism)
            )
        })
        
        # TODO: Write a response to color selection, must convert to R colors
        
        # Initialize the image area, co-ordinates and messages divs
        initImage(output)
        initCoords(output)
        session$messages <- updateMessages(output,session,paste(
            "What a nice shiny day on the hill! Let's analyze some data!"))
        
        # Check if we have data to read in from BAM files
        session$observers$source_input <- observe({
            input$go_bam
            input$go_link
            bam <- input$go_bam
            link <- input$go_link

            if (bam > session$bamHitCount) {
                session$bamHitCount <- session$bamHitCount + 1
                isolate({
                    bams.present <- logical(3)
                    bam.paths <- bam.names <- rep(NA,3)
                    if (is.null(input$bam_1$datapath)
                        && is.null(input$bam_2$datapath)
                        && is.null(input$bam_3$datapath)) {
                        if (bam==1)
                            session$messages <- updateMessages(output,
                                session,"No input BAM files were provided",
                                clear=TRUE)
                        else
                            session$messages <- updateMessages(output,
                                session,"No input BAM files were provided")
                    }
                    else {
                        if (!is.null(input$bam_1$datapath)) {
                            bams.present[1] <- TRUE
                            bam.paths[1] <- input$bam_1$datapath
                            bam.names[1] <- input$bam_1$name
                        }
                        if (!is.null(input$bam_2$datapath)) {
                            bams.present[2] <- TRUE
                            bam.paths[2] <- input$bam_2$datapath
                            bam.names[2] <- input$bam_2$name
                        }
                        if (!is.null(input$bam_3$datapath)) {
                            bams.present[3] <- TRUE
                            bam.paths[3] <- input$bam_3$datapath
                            bam.names[3] <- input$bam_3$name
                        }
                        bam.paths <- bam.paths[which(bams.present)]
                        bam.names <- bam.names[which(bams.present)]
                        names(bam.paths) <- bam.names
                        
                        reads <- coverage <- vector("list",
                            length(bam.paths))
                        names(reads) <- names(coverage) <- bam.names
                        for (n in names(reads)) {
                            session$messages <- updateMessages(output,
                                session,paste("Reading total reads for ",n,
                                    "...",sep=""))
                            reads[[n]] <- as(readGAlignments(file=bam.paths[n]),
                                "GRanges")
                            session$messages <- updateMessages(output,
                                session,paste("Calculating coverage for ",n,
                                    "...",sep=""))
                            coverage[[n]] <- coverage(reads[[n]])
                        }
                            
                        session$messages <- updateMessages(output,session,
                            "Done!")
                    }
                        
                })
            }

            if (link > session$linkHitCount) {
                session$linkHitCount <- session$linkHitCount + 1
                isolate(
                    if (input$link_1=="" && input$link_2==""
                        && input$link_3=="") {
                        if (link==1)
                           session$messages <- updateMessages(output,session,
                                "No links to indexed BAM files were provided")
                        else
                            session$messages <- updateMessages(output,session,
                                "No links to indexed BAM files were provided")
                    }
                    else
                        session$messages <- updateMessages(output,session,
                            "Something was provided")
                )
            }
        })
        
        # When the client ends the session, suspend the observers
        destroyObservers(session)
    }
)

clearDataSource <- function(session) {
    session$bam[[1]]$present <- FALSE
    session$path[[1]]$path <- NA
    session$bam[[1]]$present <- FALSE
}

updateMessages <- function(output,session,msg,clear=FALSE) {
    if (clear)
        session$messages <- msg
    else
        session$messages <- c(session$messages,msg)
    output$messages <- renderUI({
        tags$div(
            #isolate(
                HTML(paste(session$messages,collapse="<br/>"))
            #)
        )
    })
    return(session$messages)
}

initImage <- function(output) {
    output$hilbert <- renderPlot({
        plot(1:100,1:100,xaxt="n",yaxt="n",pch=20,col="white",
            xlab="",ylab="")
        text(50,50,"The image will be displayed here",cex=5,font=2)
        
        output$coords <- renderText({
            "Chromosomal coordinates will be displayed here"
        })
    })
}

initCoords <- function(output) {
    output$browser <- renderUI({
        tags$div(
            "Associated UCSC Genome Browser session",
            tags$br(),
            tags$a(
                href="http://genome.ucsc.edu",
                target="_blank",
                "http://genome.ucsc.edu"
            )
        )
    })
}

initDataSource <- function(srctype) {
    switch(srctype,
        bam = {
            return(list(
                present <- logical(3),
                path <- rep(NA,3),
                name <- rep(NA,3)
            ))
        },
        link = {}
    )
}

destroyObservers <- function(session) {
    session$onSessionEnded(function() {
        for (obs in names(session$observers))
            session$observers[[obs]]$suspend()
    })
}
