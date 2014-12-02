# ui.R

shinyUI(
    fluidPage(
        tags$head(
            tags$script(
                type="text/javascript",
                src="jscolor/jscolor.js"
            ),
            tags$style(
                paste("#messages{color: #e50000;",
                    "background-color: #f5bca9;",
                    "font-family: \"Courier New\", Courier, monospace;",
                    "font-size: 0.9em;",
                    "margin-top: 5px;",
                    "margin-bottom: 10px;",
                    "border-style: solid;",
                    "border-width: 1px;",
                    "border-color: #e68701;",
                    "padding: 10px;",
                    "border-radius: 5px;",
                    "height:300px;",
                    "overflow: auto;}")
            ),
            tags$style(
                paste("#browser{background-color: #ffcc82;",
                    "margin-top: 5px;",
                    "margin-bottom: 10px;",
                    "border-style: solid;",
                    "border-width: 1px;",
                    "border-color: #f78181;",
                    "padding: 10px;",
                    "border-radius: 5px;}")
            )
        ),
        titlePanel("ShinyHil"),
        fluidRow(
            column(3,wellPanel(
                h4("Input source"),
                tabsetPanel(
                    id="input_type",
                    tabPanel(
                        title="BAM",
                        fluidRow(
                            column(1,h6("1st")),
                            column(7,htmlOutput("bam_1")),
                            column(2,actionButton(
                                inputId="clear_bam_1",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(1,h6("2nd")),
                            column(7,htmlOutput("bam_2")),
                            column(2,actionButton(
                                inputId="clear_bam_2",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(1,h6("3rd")),
                            column(7,htmlOutput("bam_3")),
                            column(2,actionButton(
                               inputId="clear_bam_3",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(10,""),
                            column(2,actionButton(
                                inputId="go_bam",label="Go!"
                            ))
                        )
                    ),
                    tabPanel(
                        title="Link",
                        fluidRow(
                            column(1,h6("1st")),
                            column(7,textInput(
                                inputId="link_1",label=""
                            )),
                            column(2,actionButton(
                                inputId="clear_link_1",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(1,h6("2nd")),
                            column(7,textInput(
                                inputId="link_2",label=""
                            )),
                            column(2,actionButton(
                                inputId="clear_link_2",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(1,h6("3rd")),
                            column(7,textInput(
                                inputId="link_3",label=""
                            )),
                            column(2,actionButton(
                                inputId="clear_link_3",label="Clear"
                            ))
                        ),
                        fluidRow(
                            column(10,""),
                            column(2,actionButton(
                                inputId="go_link",label="Go!"
                            ))
                        )
                    )
                )
            ),
            wellPanel(
                h4("Genome source"),
                fluidRow(
                    column(3,h6("Organism")),
                    column(9,selectInput(
                        inputId="organism",label="",
                        choices=c(
                            "Human hg18"="hg18",
                            "Human hg19"="hg19",
                            "Mouse mm9"="mm9",
                            "Mouse mm10"="mm10",
                            "Rat rn5"="rn5",
                            "Fruitfly dm3"="dm3",
                            "Zebrafish danRer7"="danRer7",
                            "Chimpanzee panTro4"="panTro4",
                            "Pig susScr3"="susScr3"
                        )
                    ))
                ),
                fluidRow(
                    column(3,h6("Chromosome")),
                    column(9,htmlOutput("setChrs"))
                )
            ),
            wellPanel(
                h4("Color controls"),
                hr(),
                h5("Source colors"),
                fluidRow(
                    column(3,h6("1st source")),
                    column(9,tags$input(
                        id="color_1",class="color",value="#ff0000"
                    ))
                ),
                fluidRow(
                    column(3,h6("2nd source")),
                    column(9,tags$input(
                        id="color_2",class="color",value="#0000ff"
                    ))
                ),
                fluidRow(
                    column(3,h6("3rd source")),
                    column(9,tags$input(
                        id="color_3",class="color"
                    ))
                ),
                hr(),
                h5("Saturation"),
                fluidRow(
                    column(3,h6("1st source")),
                    column(9,sliderInput(
                        inputId="slider_1",label="",
                        min=0,max=100,value=50
                    ))
                ),
                fluidRow(
                    column(3,h6("2nd source")),
                    column(9,sliderInput(
                        inputId="slider_2",label="",
                        min=0,max=100,value=50
                    ))
                ),
                fluidRow(
                    column(3,h6("3rd source")),
                    column(9,sliderInput(
                        inputId="slider_3",label="",
                        min=0,max=100,value=50
                    ))
                )
            )
        ),
        column(8,
            plotOutput("hilbert"),
            verbatimTextOutput("coords"),
            htmlOutput("browser"),
            htmlOutput("messages")
        )
    )
))

