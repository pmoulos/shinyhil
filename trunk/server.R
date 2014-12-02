# server.R

# Load required libraries
library(shiny)
#library(GenomicAlignments)
#library(HilbertVis)

library(datasets)

# Set some global shiny options
# Increase max upload size so as to accomodate BAM files
options(shiny.maxRequestSize=10000*1024^2)

shinyServer(
    function(input,output,session) {
        
        # Intitiate list of observers
        observers <- list()
        # Record data source changes
        session$bamHitCount <- session$linkHitCount <- 0
        # Record displayed messages
        session$messages <- NULL
        
        # Set up response to clear uploaded file buttons
        output$bam_1 <- renderUI({
            input$clear_bam_1
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
        observers$link_1 <- observe({
            input$clear_link_1
            updateTextInput(session,"link_1",value="")
        })
        observers$link_2 <- observe({
            input$clear_link_2
            updateTextInput(session,"link_2",value="")
        })
        observers$link_3 <- observe({
            input$clear_link_3
            updateTextInput(session,"link_3",value="")
        })
        
        # Respond to change in organism and repopulate chromosomes
        output$setChrs <- renderUI({
            selectInput(
                inputId="chromosomes",
                label="",
                choices=get.valid.chrs(input$organism)
            )
        })
        
        # TODO: Write a response to color selection, must convert to R colors
        
        # Check if we have data to read in from BAM files
        observers$source_input <- observe({
            input$go_bam
            input$go_link
            bam <- input$go_bam
            link <- input$go_link
            if (bam==0 && link==0) { 
                session$messages <- c(session$messages,
                    paste("What a nice shiny day on the hill! ",
                        "Let's analyze some data!"))
            }
            else {
                if (bam > session$bamHitCount) {
                    session$bamHitCount <- session$bamHitCount + 1
                    isolate({
                        bams.present <- logical(3)
                        bam.paths <- bam.names <- rep(NA,3)
                        if (is.null(input$bam_1$datapath)
                            && is.null(input$bam_2$datapath)
                            && is.null(input$bam_3$datapath)) {
                            if (bam==1)
                                session$messages <-
                                    "No input BAM files were provided"
                            else
                                session$messages <- c(session$messages,
                                    "No input BAM files were provided")
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
                            
                            reads <- coverage <- vector("list",
                                length(bam.paths))
                            names(reads) <- names(coverage) <- bam.names
                            for (n in names(reads)) {
                                paste("Reading total reads for ",n,"...")
                                reads[[n]] <- as(readGAlignments(file=input[n]),"GRanges")
                                disp("Calculating coverage for ",n,"...")
                                coverage[[n]] <- coverage(reads[[n]])
                            }
                                
                            session$messages <- c(session$messages,
                                "Something was provided")
                        }
                            
                    })
                }
                if (link > session$linkHitCount) {
                    session$linkHitCount <- session$linkHitCount + 1
                    isolate(
                        if (input$link_1=="" && input$link_2==""
                            && input$link_3=="") {
                            if (link==1)
                                session$messages <- 
                                    "No links to indexed BAM files were provided"
                            else
                                session$messages <- c(session$messages,
                                    "No links to indexed BAM files were provided")
                        }
                        else
                            session$messages <- c(session$messages,
                                "Something was provided")
                    )
                }
            }
            
            output$messages <- renderUI({
                tags$div(
                    isolate(
                        HTML(paste(session$messages,collapse="<br/>"))
                    )
                )
            })
        })
        
        # Fill in the spot we created for a plot
        output$hilbert <- renderPlot({
            # Initialize the visualization environment
            plot(1:100,1:100,xaxt="n",yaxt="n",pch=20,col="white",
                xlab="",ylab="")
            text(50,50,"The image will be displayed here",cex=5,font=2)
            
            output$coords <- renderText({
                "Chromosomal coordinates will be displayed here"
            })
        })
        
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
        
        # When the client ends the session, suspend the observers
        session$onSessionEnded(function() {
            for (obs in names(observers))
                observers[[obs]]$suspend()
        })
    }
)

get.valid.chrs <- function(org) {
    switch(org,
        hg18 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        hg19 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        hg38 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr3",
                "chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        mm9 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX","chrY"
            ))
        },
        mm10 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX","chrY"
            ))
        },
        rn5 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr3","chr4","chr5","chr6",
                "chr7","chr8","chr9","chrX"
            ))
        },
        dm3 = {
            return(c(
                "chr2L","chr2LHet","chr2R","chr2RHet","chr3L","chr3LHet",
                "chr3R","chr3RHet","chr4","chrU","chrUextra","chrX","chrXHet",
                "chrYHet"
            ))
        },
        danRer7 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr2","chr20","chr21","chr22","chr23",
                "chr24","chr25","chr3","chr4","chr5","chr6","chr7","chr8","chr9"
            ))
        },
        panTro4 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr19","chr20","chr21","chr22","chr2A","chr2B",
                "chr3","chr4","chr5","chr6","chr7","chr8","chr9","chrX","chrY"
            ))
        },
        susScr3 = {
            return(c(
                "chr1","chr10","chr11","chr12","chr13","chr14","chr15","chr16",
                "chr17","chr18","chr2","chr3","chr4","chr5","chr6","chr7",
                "chr8","chr9","chrX","chrY"
            ))
        },
        tair10 = {
            return(c(
                "chr1","chr2","chr3","chr4","chr5"
            ))
        }
    )
}
