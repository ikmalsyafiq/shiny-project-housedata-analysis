library(rsconnect)
library(shiny)
library(shinythemes)
library(DT)
library(tidyverse)
library(pastecs)
library(ggplot2)
library(pastecs)
library(Hmisc)
library(corrplot)
library(GGally)
library(scales)
library(mice)
library(ggthemes)
library(data.table)
library(testthat)
library(gridExtra)
library(egg)
library(psych)
library(e1071)
library(randomForest)
library(RColorBrewer)



ui <- navbarPage(title=div(img(alt = 'Dream Home', src="https://img.techpowerup.org/200625/toplogo-removebg-preview-1.png", height=30)),
                 theme = shinytheme("flatly"),
                 #Tab 1: Intro Page
                 tabPanel('Main', 
                          titlePanel('Dream Homes'),
                          
                          tags$div(
                            class='jumbotron',
                            style='background-color: white;',
                            
                            fluidRow(
                              
                              column(6,
                                     tags$img(
                                       src='https://img.techpowerup.org/200625/bottom-logo.jpg' ,
                                       width='80%',
                                       style='margin-left:10px',
                                       
                                     )
                              ),
                              column(6,
                                     
                                     tags$p(style='font-size: 17px; height: 15px; margin-top: 20px; margin-left: 0px',
                                            'Looking to Settle Down in the Beautiful State of Iowa? ',
                                            'Explore our App to Better Understand the Housing Market'
                                     ),
                                     
                                     tags$p(style='font-size: 28px; height: 20px; margin-top: 70px; margin-left: 35px',
                                            'IOWA: The Hawkeye State '
                                     ),
                                     tags$p(style='font-size: 13px; height: 10px; margin-top: 10px; margin-left: 30px',
                                            '--Our liberties we prize and our rights we will maintain--'
                                     ),
                                     
                                     tags$img(
                                       src='https://upload.wikimedia.org/wikipedia/commons/a/aa/Flag_of_Iowa.svg' ,
                                       width='20%',
                                       style='margin-left:30px; margin-top: 23px;'
                                     ),
                                     tags$img(
                                       src='https://upload.wikimedia.org/wikipedia/commons/thumb/5/5a/Iowa-StateSeal.svg/2000px-Iowa-StateSeal.svg.png' ,
                                       width='20%',
                                       style='margin-left:30px; margin-top: 20px;',
                                       
                                     ),
                                     tags$img(
                                       src='https://upload.wikimedia.org/wikipedia/en/a/a4/Flag_of_the_United_States.svg' ,
                                       width='20%',
                                       style='margin-left:30px; margin-top: 33px;',
                                       
                                     ))
                              
                              
                            ) 
                          )
                 ),
                 
                 tabPanel("Guidelines",
                          bootstrapPage(
                            sidebarLayout(
                              sidebarPanel(
                                titlePanel(title="What Do You Get by Using This App?"),
                                tags$ul(
                                  tags$li("Information on the properties previously sold in Iowa"), 
                                  tags$li("Price estimates of properties in Iowa based on your preferences "), 
                                  tags$li("Insights on the features that contribute to sale price")
                                ),
                                
                              ),
                              
                              mainPanel(
                                tags$h1("How to Use This App?"),
                                tags$h4('Open the Price Checker Tab by clicking it above'),
                                tags$ol(
                                  tags$li("Select Desired Attributes from the sidepanel located on left side of screen"), 
                                  tags$li("The selected features will be summarized in the main panel"),
                                  tags$li("The estimated price for the house matching the users selection will be shown in bold letters"),
                                  tags$li("The pie chart provides a summary of house prices of all properties sold in Iowa"),
                                ),
                                tags$h4('Open the Explore Data Tab by clicking it above'),
                                tags$ol(                        
                                  tags$li("This tab contains three sections, refer to the Description above each section to know more details about them"),
                                  tags$li("Click on the Data Correlation Section to understand how all features correlate with each other"),
                                  tags$li("From the sidepanel on the left, Select the features whose correlation with each other you want to understand deeper (minimum 2)"),
                                  tags$li("Click on the Individual Correlation Section to understand how the selected features correlate with one another"),
                                  tags$li("Feel free to select and unselect as many features as you want to gain better understanding on it "),
                                  tags$li("Click on the Importance Section to see which attribute has the largest importance towards determining sale price "),
                                  tags$li("Feel free to select and unselect as many features as you want to gain better understanding on it "),
                                ),
                                tags$h4('Open the Explore Dataset Description Tab by clicking it above'),
                                tags$ol(                        
                                  tags$li("The various features selected in the sidepanel on the left, will display tables and charts on the main panel"),
                                  tags$li("If you wish to see the summary for any variable, select the variable at the 'Summarize the Variable' part of the sidepanel"),
                                  tags$li("If you wish to see a histogram for any variable, select the variable at the 'Plot Value Count of the Variable' part of the sidepanel"),
                                  tags$li("If you wish to see a scatterplot between 2 variables, select the variables at the 'Scatterplot of Variable x'(for the x-axis) and 'Scatterplot of Variable y'(for the y-axis) part of the sidepanel"),
                                  tags$li("If you wish to see the list of total features in the dataset, it is shown in the 'List of Features in the Dataset' part of the sidepanel"),
                                )
                              )
                            )
                          )), 
                 #Johan Part
                 tabPanel('Price Checker',
                          titlePanel("Price Estimator for Iowa Houses"),
                          sidebarLayout(
                            sidebarPanel(
                              # Input: Lot Area ----
                              selectInput("BldgType", "Building Type",
                                          choices = c("Single Family Detached" = "1Fam",
                                                      "Two Family Conversion" = "2fmCon",
                                                      "Duplex" = "Duplex",
                                                      "Town House End Unit" = "TwnhsE",
                                                      "Town House Inside Unit" = "Twnhs")),
                              selectInput("HouseStyle", "House Style",
                                          choices = c("1 Story" = "1Story",
                                                      "1.5 Story Finished" = "1.5Fin",
                                                      "1.5 Story Unfinished" = "1.5Unf",
                                                      "2 Story" = "2Story",
                                                      "2.5 Story Finished" = "2.5Fin",
                                                      "2.5 Story Unfinished" = "2.5Unf",
                                                      "Split Level" = "SLvl",
                                                      "Split Foyer" = "SFoyer")),
                              # Input: Stuff  ----
                              selectInput("GarageType", "Garage Type",
                                          choices = c("Attached" = "Attchd",
                                                      "Detached" = "Detchd",
                                                      "Built in" = "BuiltIn",
                                                      "Basement" = "Basement",
                                                      "Car Port" = "CarPort",
                                                      "No Garage" = "None")),
                              radioButtons("PavedDrive", "Paved Driveway",
                                           choices = c("Yes" = "Y",
                                                       "No" = "N")),
                              radioButtons("CentralAir", "Central Aircond",
                                           choices = c("Yes" = "Y",
                                                       "No" = "N")),
                              radioButtons("1stFlrSF", "1st Floor Square Feet",
                                           choices = c("> 1000 sf" = "Y",
                                                       "< 1000 sf" = "N")),
                              # End Johan
                              # Horizontal line ----
                              tags$hr()
                            ),
                            mainPanel(
                              helpText("Select your desired house features on the left"),
                              helpText("Let's estimate how much it might cost with your features"),
                              htmlOutput("user_selected"),
                              htmlOutput("est_price"),
                              helpText("Here's a breakdown of the % of house prices in Iowa"),
                              plotOutput("priceBinPie")
                            )
                          )
                 ),
                 
                 #Dilraj Part
                 tabPanel('Explore Data',
                          titlePanel(title = "Insights on House Prices"),
                          sidebarLayout(
                            sidebarPanel(
                              textOutput("tab2desc"),
                              br(),
                              uiOutput('corrcheckbox'),
                            ),
                            
                            mainPanel(
                              tabsetPanel(
                                id = "corr",
                                
                                tabPanel('Data Correlation',
                                         column(12, h2(textOutput("tab2p1head"))),
                                         br(),
                                         column(12, textOutput("tab2p1desc")),
                                         column(12, plotOutput("relationplot", height = 500)),
                                         tags$b(style='font-size: 15px; height:7px; ',
                                                'Explanation of Results '
                                         ),
                                         tags$ul(style='font-size: 15px; height:7px; ',
                                            tags$li('Correlation is a standardized covariance measure that is used to quantify the strength and direction of the relationship between two different variables.'),
                                            tags$li('The strength of a correlation is determined by its numerical value and it indicates how strong the relationship is between the two variables.'),
                                            tags$li('The direction of the correlation is determined by whether the correlation is positive or negative.'),
                                            tags$li('Positive correlation: Both variables move in the same direction. In other words, as one variable increases, the other variable also increases. As one variable decreases, the other variable also decreases.'),
                                            tags$li('Negative correlation: The variables move in opposite directions. As one variable increases, the other variable decreases. As one variable decreases, the other variable increases.'),
                                            tags$li('No correlation: There is no apparent relationship between the two variables.'),
                                            tags$li('To summarize, the more positive the correlation coefficient between variables, the stronger their relationship with each other'),
                                         ),
                                         
                                ),   
                                tabPanel('Individual Correlation',
                                         column(12, h2(textOutput("tab2p2head"))),
                                         column(12, textOutput("tab2p2desc")),
                                         column(12, plotOutput("intrelation", height = 500)),
                                         tags$b(style='font-size: 15px; height:7px; ',
                                                'Explanation of Results '
                                         ),
                                         tags$ul(style='font-size: 15px; height:7px; ',
                                                 tags$li('This section produces a scatter plot matrix for selected variables'),
                                                 tags$li('A scatter plot matrix is a grid of mini-plots used to visualize bivariate relationships between combinations of variables.'),
                                                 tags$li('Each scatter plot in the matrix visualizes the relationship between a pair of variables, allowing many relationships to be explored in one chart.'),
                                                 tags$li('The bottom left side of the matrix shows the scatterplots between variables'),
                                                 tags$li('The top right side of the matrix shows the correlation coefficient between selected variables'),
                                                 tags$li('The central diagonal of the matrix shows histogram of the selected variable along with the density plots'),
                                                 
                                         ),
                                         
                                ), 
                                tabPanel('Importance',
                                         column(12, h2(textOutput("tab2p3head"))),
                                         column(12, textOutput("tab2p3desc")),
                                         column(12, plotOutput("varimp", height = 500)),                                 
                                         tags$b(style='font-size: 15px; height:7px; ',
                                                'Explanation of Results '
                                         ),
                                         tags$ul(style='font-size: 15px; height:7px; ',
                                                 tags$li('This section identifies which variables have the highest importance towards determining Sale Price. '),
                                                 tags$li('Varibles with higher importance have higher predictive power which means their values have a significant impact on the Sale Price'),
                                                 tags$li('This model predicts the importance by calculating Mean Decrease Gini (IncNodePurity on the x-axis)'),
                                                 tags$li('The higher the value of mean decrease accuracy or mean decrease gini score, the higher the importance of the variable to our desired feature (Sale Price).'),
                                                 tags$li('Further details about Mean Decrease Gini can be obtained from https://bambielli.com/til/2017-10-29-gini-impurity/'),

                                                 
                                         ),
                                )                                 
                                
                              ))
                            
                            
                          )
                          
                 ),
                 
                 tabPanel('Dataset Description',
                          titlePanel(title = "Description of Dataset Features"),
                          helpText("This tab describes the Iowa Housing Dataset that this app uses"),
                          helpText("Select the variables from the dropdowns that you would like to see be described"),
                          sidebarLayout(
                            sidebarPanel(
                              # Select variables to display ----
                              #select input to describe
                              selectInput(label = "Summarize the variable", "descvar", "Choose a variable to display",""),
                              #select input for plot
                              selectInput(label = "Plot value count of the variable", "housingcolumnplot", "Choose a variable to display",""),
                              #select column for scatterplot
                              selectInput(label = "Scatterplot of variable x", "housingcolumnscatx", "Choose a variable to display",""),
                              selectInput(label = "Scatterplot of variable y", "housingcolumnscaty", "Choose a variable to display",""),
                              htmlOutput("checkbox")
                              
                            ),
                            mainPanel(
                              #Data frame output ----
                              tableOutput('contents'),
                              #Describe project output
                              textOutput("describe_title"),
                              #Describe column output
                              verbatimTextOutput("describecolumn", placeholder = FALSE),
                              #plot output
                              textOutput("plot_title"),
                              plotOutput("plot"),
                              #scatterplot output
                              textOutput("scat_title"),
                              plotOutput("scat"),
                              #data analysis output
                              textOutput("analysis")
                            )
                          ))
)



shinyApp(ui = ui, server = server)
