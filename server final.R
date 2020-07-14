# get_est_price function
get_est_price <- function(item, hd_df)
{
    test_attr_df2 <- ""
    result_df <- hd_df
    last_price_df <- ""
    result = "test_attr_df2 <- hd_df"
    if (length(item) == 1)
    {
        if (is.character(item[[names(item)[1]]]))
        {
            str_to_run <- paste(" %>% filter(", names(item)[1], "==", '"', item[[names(item)[1]]], '"', ")", sep="")
            result <- paste(result, str_to_run)
            print(result)
            eval(parse(text=result))
        }
        str_to_run <- paste("%>% filter(", names(item)[1], "==", item[[names(item)[1]]], ")", sep="")
        result <- paste(result, str_to_run)
        print(result)
        eval(parse(text=result))
    }
    else if (length(item) > 1)
    {
        for(nam in names(item))
        {
            if (is.character(nam))
            {
                result_df <- result_df[result_df[[nam]] == item[[nam]], ]
                if (nrow(result_df) > 0)
                {
                    last_price_df <- result_df
                }
            }
            else if (nam == "1stFlrSF")
            {
                if (item[[nam]] == "Y")
                {
                    result_df <- result_df[result_df[[nam]] >= 1000, ]
                }
                else if (item[[nam]] == "N")
                {
                    result_df <- result_df[result_df[[nam]] <= 999, ]
                }
            }
        }
    }
    if (nrow(result_df) > 0)
    {
        result_df <- na.omit(result_df)
        est_price <- format(round(mean(result_df$SalePrice), 2), nsmall = 2)
        return(est_price)
    }
    else if (nrow(result_df) == 0)
    {
        last_price_df <- na.omit(last_price_df)
        est_price <- format(round(mean(last_price_df$SalePrice), 2), nsmall = 2)
        return(est_price)
    }
}

house_data_df <- read.csv("reduced home data final.csv", stringsAsFactors = FALSE)
house_data_df$GarageType <- replace_na(house_data_df$GarageType, "None")

house_data_dil <- house_data_df %>%select(1, 3:13,15,17:19,21:26)
house_data_dil <- house_data_dil %>%
    rename(
        Zoning= MSZoning,
        BuildingType = BldgType,
        OverallQuality= OverallQual,
        OverallCondition = OverallCond,
        CentralAirConditioning = CentralAir,
        FirstFloorSize= X1stFlrSF,
        SecondFloorSize = X2ndFlrSF,
        YearSold = YrSold
    )

cat_var <- names(house_data_dil)[which(sapply(house_data_dil, is.character))]
num_var <- names(house_data_dil)[which(sapply(house_data_dil, is.numeric))]

house_data_dil_char <- house_data_dil[cat_var]
house_data_dil_num <- house_data_dil[num_var]

server <- function(input, output, session) {
    # Input file ----
    df<-reactive({
        inFile<-house_data_dil
        #print(inFile)
        if(is.null(inFile))
            return(NULL)
        dt_frame = house_data_dil
        updateSelectInput(session, "housingcolumnscatx", choices = names(dt_frame))
        updateSelectInput(session, "housingcolumnscaty", choices = names(dt_frame))
        updateSelectInput(session, "housingcolumnplot", choices =  names(dt_frame))
        updateSelectInput(session, "descvar", choices = names(dt_frame))
        return(dt_frame)
        
    })
    
    # Dynamically generate UI input when data is uploaded ----
    output$checkbox <- renderUI({
        checkboxGroupInput(inputId = "select_var", 
                           label = "List of features in the dataset", 
                           choices = names(df()))
    })
    
    #Render plot ----
    output$plot <- renderPlot({
        barplot(table(house_data_dil[,input$housingcolumnplot]))
        
    })
    output$scat <- renderPlot({
        dataset = house_data_dil
        ggplot(dataset, aes(as.factor(dataset[,input$housingcolumnscatx]),  dataset[,input$housingcolumnscaty])) + 
            geom_point() + 
            #labs(y = dataset[,input$housingcolumnscaty], x = input$housingcolumnscatx);
            labs(y = input$housingcolumnscaty, x = input$housingcolumnscatx);
        
    })
    
    output$describecolumn <- renderPrint({
        dataset <- house_data_dil
        summary <- summary(house_data_dil[,input$descvar])
        decribeColumn <- Hmisc::describe(summary)
        print(decribeColumn)
    })
    # TODO
    # Render text ----
    
    # Start Johan
    bldgChoice <- c("Single Family Detached" = "1Fam",
                    "Two Family Conversion" = "2fmCon",
                    "Duplex" = "Duplex",
                    "Town House End Unit" = "TwnhsE",
                    "Town House Inside Unit" = "Twnhs")
    houseChoice <- c("1 Story" = "1Story",
                     "1.5 Story Finished" = "1.5Fin",
                     "1.5 Story Unfinished" = "1.5Unf",
                     "2 Story" = "2Story",
                     "2.5 Story Finished" = "2.5Fin",
                     "2.5 Story Unfinished" = "2.5Unf",
                     "Split Level" = "SLvl",
                     "Split Foyer" = "SFoyer")
    garageChoice <- c("Attached" = "Attchd",
                      "Detached" = "Detchd",
                      "Built in" = "BuiltIn",
                      "Basement" = "Basement",
                      "Car Port" = "CarPort",
                      "No Garage" = "None")
    yesNoChoice <- c("Yes" = "Y",
                     "No" = "N")
    sfChoice <- c("> 1000 sf" = "Y",
                  "< 1000 sf" = "N")
    # Render est price
    output$est_price <- renderUI({
        radio_list <- list("BldgType" = input$BldgType, "HouseStyle"= input$HouseStyle,
                           "GarageType" = input$GarageType, "PavedDrive" = input$PavedDrive,
                           "CentralAir" = input$CentralAir, "1stFlrSF" = input$`1stFlrSF`)
        wo <- get_est_price(radio_list, house_data_df)
        HTML("<b>",paste("The estimated price of the house is $", wo, sep=""),"</b>")
    })
    output$user_selected <- renderUI({
        bldg_str <- paste("Building Type:", names(bldgChoice)[bldgChoice == input$BldgType])
        house_str <- paste("House Style:", names(houseChoice)[houseChoice == input$HouseStyle])
        garage_str <- paste("Garage:", names(garageChoice)[garageChoice == input$GarageType])
        paved_str <- paste("Paved Drive:", names(yesNoChoice)[yesNoChoice == input$PavedDrive])
        central_str <- paste("Central Aircond:", names(yesNoChoice)[yesNoChoice == input$CentralAir])
        flr_str <- paste("First floor Square Feet:", names(sfChoice)[sfChoice == input$`1stFlrSF`])
        HTML(paste(bldg_str, house_str, garage_str, paved_str, central_str, flr_str, sep = '<br/>'))
    })
    output$priceBinPie <- renderPlot({
        housePricesBin <- cut(house_data_df$SalePrice, breaks=c(0,100000,150000,200000,250000,300000, 1000000),
                                labels=c("<100K","100K-150K","150K-200K", "200K-250K", "250K-300K", ">300K"))
        priceBinCount <- as.data.frame(table(housePricesBin))
        pct <- round(priceBinCount$Freq/sum(priceBinCount$Freq)*100)
        lbls <- paste(priceBinCount$housePricesBin, ", ", pct, sep="") # add percents to labels
        lbls <- paste(lbls,"%",sep="") # ad % to labels 
        pie(pct, labels = lbls, main="% of House Prices", col = brewer.pal(n=6, name="RdBu"))
        legend("topleft", legend = lbls, cex=0.8, fill = brewer.pal(n=6, name="RdBu"))
    })
    # End Johan
    
    output$describe_title <- renderText({
        
        paste("Here is a summary of the feature", input$descvar)
    })
    
    output$plot_title <- renderText({
        paste("Here is a plot count of values in", input$housingcolumnplot)
    })
    
    output$scat_title <- renderText({
        paste("Scatterplot of", input$housingcolumnscatx,
              "against", input$housingcolumnscaty)
    })
    # Start Dilraj
    
    #Individual Correlation Plot 
    output$corrcheckbox <- renderUI({
        checkboxGroupInput(inputId = "corr_select_var", 
                           label = "Select variables to Plot Individual Correlation (Section 2)", 
                           choices = names(house_data_dil_num),
                           selected = names(house_data_dil_num))
    })
    
    
    output$intrelation <- renderPlot({
        pairs.panels(house_data_dil_num %>% select(input$corr_select_var), 
                     method = "pearson", 
                     hist.col = "#00AFBB",
                     density = TRUE,  
                     ellipses = F
        )
        
    })
    
    #Correlation Plot
    output$relationplot <- renderPlot({
        correlations <- cor(na.omit(house_data_dil_num[,-1]))
        row_indic <- apply(correlations, 1, function(x) sum(x > 0.3 | x < -0.3) > 1)
        correlations<- correlations[row_indic ,row_indic ]
        corrplot(correlations, method="square")
        
    })
    
    #Variable Importance  
    output$varimp <- renderPlot({
        
        house_data_dil$GarageType1 <- as.character(house_data_dil$GarageType)
        house_data_dil$GarageType1[which(is.na(house_data_dil$GarageType))] <- "None"
        house_data_dil$GarageType <- as.factor(house_data_dil$GarageType1)
        house_data_dil <- subset(house_data_dil,select = -GarageType1)
        
        house_data_dil$Electrical1 <- as.character(house_data_dil$Electrical)
        house_data_dil$Electrical1[which(is.na(house_data_dil$Electrical))] <- "None"
        house_data_dil$Electrical <- as.factor(house_data_dil$Electrical1)
        house_data_dil <- subset(house_data_dil,select = -Electrical1)
        
        house_data_dil$Zoning<- factor(house_data_dil$Zoning)
        house_data_dil$LotShape <-factor(house_data_dil$LotShape)
        house_data_dil$Utilities<-factor(house_data_dil$Utilities)
        house_data_dil$BuildingType <-factor(house_data_dil$BuildingType)
        house_data_dil$HouseStyle<-factor(house_data_dil$HouseStyle)
        house_data_dil$Heating<-factor(house_data_dil$Heating)
        house_data_dil$CentralAirConditioning<-factor(house_data_dil$CentralAirConditioning)
        house_data_dil$Functional<-factor(house_data_dil$Functional)
        house_data_dil$PavedDrive<-factor(house_data_dil$PavedDrive)
        house_data_dil$SaleType<-factor(house_data_dil$SaleType)
        house_data_dil$SaleCondition<-factor(house_data_dil$SaleCondition)
        
        
        Column_classes <- sapply(names(house_data_dil),function(x){class(house_data_dil[[x]])})
        numeric_columns <-names(Column_classes[Column_classes != "factor"])
        
        skew <- sapply(numeric_columns,function(x){skewness(house_data_dil[[x]],na.rm = T)})
        
        
        for(x in names(skew)) 
        {
            house_data_dil[[x]] <- log(house_data_dil[[x]] + 1)
        }
        
        
        
        smp_size <- floor(0.75 * nrow(house_data_dil))
        
        set.seed(123)
        house_data_dil_ind <- sample(seq_len(nrow(house_data_dil)), size = smp_size)
        
        
        house_data_dil_new <- house_data_dil[house_data_dil_ind,]
        validate <- house_data_dil[-house_data_dil_ind, ]
        
        nrow(house_data_dil_new)
        nrow(validate)
        str(validate)
        
        
        house_data_dil_model <- randomForest(SalePrice~.,
                                             data = house_data_dil_new)
        
        house_data_dil_imp    <- importance(house_data_dil_model)
        varImpPlot(house_data_dil_model, main= '15 Most Important Attributes', bg= 'red', n.var=15)
        
        
    })
    
    #Text For Description
    output$tab2p1head <- renderText({
        
        'View Correlations between All Features' 
    })  
    
    output$tab2p1desc <- renderText({
        
        'This section investigates the correlations between numeric features of the Housing Data. It is often useful to understand how each attribute correlate to one another. The graph below highlights the most correlated variables in a data table'
    }) 
    
    
    output$tab2p2head <- renderText({
        
        'View Correlations between Individual Features' 
    })  
    
    output$tab2p2desc <- renderText({
        
        'This sections investigates correlations between individual features. The side panel provides a list of available features, please select the features you want to investigate (minimum 2)' 
    }) 
    
    
    output$tab2p3head <- renderText({
        
        'Importance of Features towards Sale Price ' 
    })  
    
    output$tab2p3desc <- renderText({
        'This section investigates the importance of each variable towards the Sale Price. The graph below shows the Top 15 Features that have the highest importance towards the Sale Price. The Features are sorted from top to bottom in the order of most important to least important. ' 
    }) 
    
    output$tab2desc <- renderText({
        
        'This Tab is to gain insights about the price of houses. Kindly view the 3 sections in this tab to further explore about housing prices' 
    }) 
    #End Dilraj
}
