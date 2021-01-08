# 1. Import useful libraries

if (!require(rsconnect)) install.packages('rsconnect')
if (!require(haven)) install.packages('haven')
library(haven)
if (!require(sas7bdat)) install.packages('sas7bdat')
library(sas7bdat)
if (!require(dplyr)) install.packages('dplyr')
library(dplyr)
if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)
if (!require(anytime)) install.packages('anytime')
library(anytime)
if (!require(shiny)) install.packages('shiny')
library(shiny)
if (!require(shinythemes)) install.packages('shinythemes')
library(shinythemes)
if (!require(scales)) install.packages('scales')
library(scales)
if (!require(maps)) install.packages('maps')
library(maps)
if (!require(viridis)) install.packages('viridis')
library(viridis)


# 2. Import datasets

# 2.1 create the dataframe Internet from the dataset AnalyticDataInternetGambling

internet <- read_sas("AnalyticDataInternetGambling.sas7bdat")

# 2.2 create the dataframe Demographics from the dataset RawDataIDemographics

demographics <- read_sas("RawDataIDemographics.sas7bdat")

# 2.3 create the dataframe Pokerchip from the dataset RawDataIIIPokerChipConversions

pokerchip <- read_sas("RawDataIIIPokerChipConversions.sas7bdat")


#remove the time part of the variable TransDateTime to keep only the date
pokerchip$TransDateTime <- as.Date(pokerchip$TransDateTime)


#rename the variable TransDateTime into TransDate
colnames(pokerchip)[2] <- "TransDate"


#create a new variable in the dataframe pokerchip which extracts the month from
#the variable TransDate
pokerchip$TransMonth <- as.integer(format(pokerchip$TransDate, "%m"))


# 2.3.1 Create pivot table to have unique UserID's
#The new variables will be:
#nr_trans which counts the number of transactions per user, tot_amount which sums the amount of transactions per user,
#min_date which is the minimum of the Transaction Date and max_date, the maximum

pokerchip1 <- pokerchip %>%
  group_by(UserID, TransType) %>% 
  summarize(nr_trans = n(),
            tot_amount = round(sum(TransAmount),2),
            min_date = min(TransDate),
            max_date = max(TransDate))


# 2.3.2 Make pivot table wider from the pivot longer pokerchip1

pokerchip_wider <- pivot_wider(
  pokerchip1,
  id_cols = UserID,
  names_from = TransType,
  values_from = c(nr_trans, tot_amount, min_date, max_date)
)


# 2.4 create the dataframe UserAggregation from the dataset RawDataIIUserDailyAggregation

UserAggregation <- read_sas("RawDataIIUserDailyAggregation.sas7bdat")

#replace null values in each variable of the dataframe by 0

UserAggregation$Stakes[UserAggregation$Stakes < 0] = 0

UserAggregation$Winnings[UserAggregation$Winnings < 0] = 0

UserAggregation$Bets[UserAggregation$Bets < 0] = 0


# 2.4.1 Rename date column into AggDate

names(UserAggregation)[2] <- "AggDate"

#transform the values of AggDate into date format
UserAggregation$AggDate <- anydate(UserAggregation$AggDate)


# 2.4.2 Create pivot table to have unique UserID's
#The new variables will be:
#nr_trans which counts the number of transactions per user, tot_stakes which sums the stakes per user,
#tot_winnings which sums the winnings of each user and tot_bets that sums the bets of each user

UserAggregation1 <- UserAggregation %>%
  group_by(UserID, ProductID) %>% 
  summarize(nr_trans = n(),
            tot_stakes = round(sum(Stakes),2),
            tot_winnings = round(sum(Winnings),2),
            tot_bets = round(sum(Bets),2))


# 2.4.3 Make pivot table wider from the pivot longer UserAggregation1

UserAggregation_wider <- pivot_wider(
  UserAggregation1,
  id_cols = UserID,
  names_from = ProductID,
  values_from = c(nr_trans, tot_stakes, tot_winnings, tot_bets)
)


# 2.4.4 Fill NA's of the table UserAggregation_wider by 0

UserAggregation_wider[is.na(UserAggregation_wider)] <- 0


# 3. start building our datamart by merging the dataframes obtained

# 3.1 Join internet and  demographics dataframes to get the dataframe internet_demographic

internet_demographics <- merge(internet, demographics, by.x="USERID", by.y="UserID")


# 3.1.1 Drop duplicate columns resulting of the merge

internet_demographics1 <- select(internet_demographics, -c(Country, Language, RegDate, AGE, Gender))

# 3.2 Join internet_demographics and poker original dataframes to get the dataframe internet_demographic_poker

# 3.2.1 merge With the pokerchip dataframe that has NON-unique UserID's:

internet_demographic_poker <- merge(internet_demographics1, pokerchip, by.x="USERID", by.y="UserID")

# 3.2.2 merge With pokerchip_wider that has unique UserID's:

internet_demographic_poker1 <- merge(internet_demographics1, pokerchip_wider, by.x="USERID", by.y="UserID", all=TRUE)

# 3.2.2.1 Replace null values of aggregated variables by 0

internet_demographic_poker2 <- internet_demographic_poker1 %>%
  mutate(nr_trans_24 = coalesce(nr_trans_24, 0),
         nr_trans_124 = coalesce(nr_trans_124, 0),
         tot_amount_24 = coalesce(tot_amount_24, 0),
         tot_amount_124 = coalesce(tot_amount_124, 0))


# 3.3 We get out FINAL DATAMART with unique UserID

datamart <- merge(internet_demographic_poker2, UserAggregation_wider, by.x="USERID", by.y="UserID", all=TRUE)


# 3.3.1 cleaning data in our datamart

#import a files containing countries' name and merge with our datamart to get countries' names instead of codes
countries <- read_csv("Countries.csv")
datamart <- merge(datamart, countries, by.x = "COUNTRY", by.Y = "COUNTRY", all = FALSE)


#import a files containing languages' name and merge with our datamart to get languages' names instead of codes
languages <- read_csv("Languages.csv")
datamart <- merge(datamart, languages, by.x = "LANGUAGE", by.Y = "LANGUAGE", all = FALSE)

#replace values of the variable GENDER 
datamart$GENDER[datamart$GENDER == 0] <- "Female"
datamart$GENDER[datamart$GENDER == 1] <- "Male"

#drop non useful columns after data cleansing
datamart <- datamart[-c(1,2)]

datamart_original <- datamart


#rename the variables of our final datamart
colnames(datamart) <- c("UserID", "RegistrationDate", "Gender", "Fixed_Odds_Stake_Tot", "Fixed_Odds_Winnings_Tot",
                        "Fixed_Odds_Bets_Tot", "First_Active_Date_FO", "Last_Active_Date_FO",
                        "Total_Days_Active_Fixed", 
                        "Live_Action_Stake_Tot", "Live_Action_Winnings_Tot",
                        "Live_Action_Bets_Tot", "First_Active_Date_LA", "Last_Active_Date_LA",
                        "Total_Days_Active_LA", "First Active Date (Sports Book)", 
                        "First Pay Date", "First Activity Date", "First Sports Book date", "First Casino Play date",
                        "First Game play date", "First Poker play date", "BettingApplication", "Number of Buying Transactions",
                        "Number of Selling Transactions", "Total Amount (Buying)", "Total Amount (Selling)", 
                        "First Buying Transaction", "First Selling Transaction", "Last Buying Transaction", "Last Selling Transaction",
                        'Sports Book Fixed Odd (Count)', 'Sports Book Live Action (Count)', 'Casino Chartwell (Count)', 
                        'Games VS (Count)', 'Games BWIN (Count)', 'Casino BossMedia (Count)', 'Supertoto (Count)', 
                        'Sports Book Fixed Amount (Total Stakes)', 'Sports Book Live Action (Total Stakes)', 
                        'Casino Chartwell (Total Stakes)', 'Games VS (Total Stakes)', 'Games BWIN (Total Stakes)', 
                        'Casino BossMedia (Total Stakes)', 'Supertoto (Total Stakes)', 
                        'Sports Book Fixed Odds (Total Winnings)', 'Sports Book Live Action (Total Winnings)', 
                        'Casino Chartwell (Total Winnings)', 'Games VS (Total Winnings)', 'Games BWIN (Total Winnings)', 
                        'Casino BossMedia (Total Winnings)', 'Supertoto (Total Winnings)', 
                        'Sports Book Fixed Odds (Total Bets)', 'Sports Book Live Action (Total Bets)', 
                        'Casino Chartwell (Total Bets)', 'Games VS (Total Bets)', 'Games BWIN (Total Bets)', 
                        'Casino BossMedia (Total Bets)', 'Supertoto (Total Bets)', 'Country', 'Language')

#replace values of the variable BettingApplication
datamart$BettingApplication[datamart$BettingApplication == 1] <- "BETANDWIN.COM "
datamart$BettingApplication[datamart$BettingApplication == 2] <- "TRIPLE-A-CASINO.COM "
datamart$BettingApplication[datamart$BettingApplication == 3] <- "BETANDWIN.DE "
datamart$BettingApplication[datamart$BettingApplication == 4] <- "WAP.BETANDWIN.COM "
datamart$BettingApplication[datamart$BettingApplication == 5] <- "SMS MOBILE BETTING APPLICATION "
datamart$BettingApplication[datamart$BettingApplication == 6] <- "CHARITY "
datamart$BettingApplication[datamart$BettingApplication == 7] <- "DOLCEVITACASINO.COM "
datamart$BettingApplication[datamart$BettingApplication == 8] <- "BALLS OF FIRE "
datamart$BettingApplication[datamart$BettingApplication == 9] <- "BETEUROPE.COM "
datamart$BettingApplication[datamart$BettingApplication == 10] <- "BAHSEGIR.COM "
datamart$BettingApplication[datamart$BettingApplication == 11] <- "CASINO.BETEUROPE.COM "
datamart$BettingApplication[datamart$BettingApplication == 12] <- "WWW.CASINOTURK.COM "
datamart$BettingApplication[datamart$BettingApplication == 13] <- "WWW.SANALCASINO.COM "
datamart$BettingApplication[datamart$BettingApplication == 14] <- "BETOTO.COM "
datamart$BettingApplication[datamart$BettingApplication == 15] <- "PLAYIT.COM "
datamart$BettingApplication[datamart$BettingApplication == 16] <- "CASINO.PLAYIT.COM "
datamart$BettingApplication[datamart$BettingApplication == 17] <- "THECROUPIER.COM "
datamart$BettingApplication[datamart$BettingApplication == 18] <- "SMS.BETANDWIN.COM "
datamart$BettingApplication[datamart$BettingApplication == 19] <- "WAP.BETANDWIN.DE "
datamart$BettingApplication[datamart$BettingApplication == 21] <- "BOF.PLAYIT.COM "
datamart$BettingApplication[datamart$BettingApplication == 22] <- "BOF.BETEUROPE.COM "
datamart$BettingApplication[datamart$BettingApplication == 23] <- "BETANDWIN POKER "
datamart$BettingApplication[datamart$BettingApplication == 24] <- "BETANDWIN CASINO "
datamart$BettingApplication[datamart$BettingApplication == 26] <- "SMS.BETANDWIN.DE "
datamart$BettingApplication[datamart$BettingApplication == 27] <- "LOTTERY.BETOTO.COM "
datamart$BettingApplication[datamart$BettingApplication == 28] <- "BETWORK.COM "
datamart$BettingApplication[datamart$BettingApplication == 29] <- "WAP.PLAYIT.COM "
datamart$BettingApplication[datamart$BettingApplication == 30] <- "PLAYIT POKER "
datamart$BettingApplication[datamart$BettingApplication == 31] <- "BETEUROPE POKER "
datamart$BettingApplication[datamart$BettingApplication == 32] <- "BETOTO POKER "
datamart$BettingApplication[datamart$BettingApplication == 33] <- "BETANDWIN GAMES "
datamart$BettingApplication[datamart$BettingApplication == 36] <- "BETOTO CASINO "
datamart$BettingApplication[datamart$BettingApplication == 38] <- "BETEUROPE GAMES "
datamart$BettingApplication[datamart$BettingApplication == 42] <- "PLAYIT GAMES "

#adjust our datamart's rows and columns to the limit we want to display
options(repr.matrix.max.cols=150, repr.matrix.max.rows=200)

#check and remove NAs
sum(is.na(datamart$Gender))
sum(!is.na(datamart$Gender))
nrow(datamart)
datamart <- datamart[!is.na(datamart$Gender),]

#make our datamart longer
datamart_long <- pivot_longer(datamart[, c("UserID", "RegistrationDate", "Gender", "Fixed_Odds_Stake_Tot", "Fixed_Odds_Winnings_Tot",
                                        "Fixed_Odds_Bets_Tot", 
                                        "Live_Action_Stake_Tot", "Live_Action_Winnings_Tot",
                                        "Live_Action_Bets_Tot")], 
                              -c("UserID", "RegistrationDate", "Gender"), names_to = "variable", values_to = "value")

#replace NAs by 0 
datamart_long$value <- replace_na(datamart_long$value, 0)

#drop some non useful variables
datamart1 <- datamart
datamart1 <- datamart1[-which(is.na(datamart1[,c(4,5,6,7,8,9)])), ] 
datamart1 <- datamart1[,-c(7,8,13,14,16,18,19,20,21,22,28,29,30,31)]

#replace NAs by 0
datamart1[is.na(datamart1)] <- 0

#Give the date format to the variable
datamart1[,12] <-  as.Date(datamart1[,12], "%Y%m%d")

#create new variable 
datamart1$TimeToDeposit <- datamart1[,12] - datamart1[,2]

#replace string element of the variable
datamart1$TimeToDeposit <- gsub("C([0-9]+)_.*", "\\1", datamart1$TimeToDeposit)

#drop some non useful variables
datamart1 <- datamart1[,-c(2,12)]
datamart1 <- datamart1[,-c(6,10)]

datamart <- datamart1


#Datamart contains :
#  1. Demographic data :- ID, Gender, Application Used, Country, Language, Time taken for first deposit
#2. Total Stakes, Winnings and Bets for the two types, Fixed Odds and Live Actions
#3. Number and Amount worth of poker chips bought or sold
#4. Total Count, Stakes, Bets and Winnings for each Product


# 4. Create 4 visualizations which respectively: 
  
#1. Compare Fixed Odds and Live Actions for Demographic Data
#2. Compare number of poker players for Demographic Data
#3. Compare Products 1-8 for Demographic Data
#4. Create Map to see how different countries are involved in Population, Stakes, Bets, Winnings
 

# Create the different visualizations that will be included in the interactive dashboard


pokerdata <- datamart[datamart[,9]>0 & datamart[,10]>0 ,c(1,2,9,10,11,12,13,42,43)]
colnames(pokerdata) <- c("UserID", "Gender", "BettingApplication", "NbrTransBuy", "NbrTransSell", "AmountBuy", "AmountSell", "Country", "Language")

genderpoker = pokerdata %>% group_by(Gender) %>% summarize(count = n(), NbrTransactions = sum(NbrTransBuy + NbrTransSell), AmountTotal = sum(AmountBuy) + sum(AmountSell))
genderpoker_long <- pivot_longer(genderpoker, -('Gender'), names_to = "variable", values_to = "value")

applicationpoker = pokerdata %>% group_by(BettingApplication) %>% summarize(count = n(), NbrTransactions = sum(NbrTransBuy + NbrTransSell), AmountTotal = sum(AmountBuy) + sum(AmountSell)) %>%
  arrange(desc(count)) %>% top_n(10)
applicationpoker_long <- pivot_longer(applicationpoker, -('BettingApplication'), names_to = "variable", values_to = "value")

countrypoker = pokerdata %>% group_by(Country) %>% summarize(count = n(), NbrTransactions = sum(NbrTransBuy + NbrTransSell), AmountTotal = sum(AmountBuy) + sum(AmountSell)) %>%
  arrange(desc(count))
countrypoker_long <- pivot_longer(countrypoker, -('Country'), names_to = "variable", values_to = "value")



world_map <- map_data("world") %>% rename(Country = region)
ggplot(world_map, aes(x = long, y = lat, group = group)) +
  geom_polygon(fill="lightgray", colour = "white")


country_map <- left_join(world_map, countrypoker_long, by = "Country")
country_map$order <- NULL
country_map$subregion <- NULL
country_map[is.na(country_map)] <- 0.1


ggplot(country_map, aes(long, lat, group = group))+
  geom_polygon(aes(fill = count), color = "white")+
  scale_fill_viridis_c(option = "C")


demographics_pokerchip <- merge(pokerchip, demographics, by.x="UserID", by.y="UserID")
demographics_pokerchip$Gender[demographics_pokerchip$Gender == 0] <- "Female"
demographics_pokerchip$Gender[demographics_pokerchip$Gender == 1] <- "Male"

demographics_pokerchip_long <- pivot_longer(demographics_pokerchip %>% 
  select(TransType, TransAmount, TransMonth, Gender), 
  -c('TransType', 'TransMonth', 'Gender'), 
  names_to = "variable", values_to = "value")


# BUILD A SHINY APP

ui <- fluidPage(theme = shinythemes::shinytheme("superhero"),
  titlePanel("Online Gambling Interactive Dashboard"),
  tabsetPanel(
    tabPanel("Gender", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Create a visualisation that 
                          shows customers' online gambling behavior per Gender."),
                 
                 selectInput("Variable", 
                             label = "Choose a variable",
                             choices = sort(unique(genderpoker_long$variable)))
               ),
               mainPanel(
                 plotOutput('plot_genderpoker')
               )
             )
    ),
    tabPanel("Applications", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Create a visualisation that 
                          shows the usage of online applications."),
                 
                 selectInput("Application", 
                             label = "Choose an application",
                             choices = sort(unique(applicationpoker_long$variable)))
               ),
               mainPanel(
                 plotOutput('plot_applicationpoker')
               )
             )
    ),
    tabPanel("Top Countries", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("See the best performing countries."),
                 
                 selectInput("Var", 
                             label = "Choose a variable",
                             choices = sort(unique(countrypoker_long$variable))),
                 sliderInput("Top", label = "Select how many countries to be displayed", 
                             value = 10, min = 1, max = 20)
               ),
               mainPanel(
                 plotOutput('plot_countrypoker')
               )
             )
    ),
    tabPanel("World Map", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Visualize customers' behavior by country."),
                 
                 selectInput("Vars", 
                             label = "Choose a variable",
                             choices = sort(unique(countrypoker_long$variable))),
                 #sliderInput("Top", label = "Select how many countries to be displayed", 
                  #           value = 10, min = 1, max = 20)
               ),
               mainPanel(
                 plotOutput('plot_map')
               )
             )
    ),
    tabPanel("FO vs. LA", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Fixed Odds versus Live Action behavior per gender."),
                 
                 selectInput("Gender", 
                             label = "Choose a gender",
                             choices = sort(unique(datamart_long$Gender)))
               ),
               mainPanel(
                 plotOutput('plot_point1')
               )
             )
    ),
    tabPanel("Poker Players", fluid = TRUE,
             sidebarLayout(
               sidebarPanel(
                 helpText("Create a visualisation that 
                          shows trends in poker clients per gender and transaction type in 2005."),
                 
                 selectInput("TransType", 
                             label = "Choose a transaction type",
                             choices = sort(unique(demographics_pokerchip$TransType))),
                 
                 selectInput("Gender2", 
                             label = "Choose a gender",
                             choices = sort(unique(demographics_pokerchip$Gender)))
                 ),
               mainPanel(
                 plotOutput('plot_point2')
               )
             )
    )
  )
)


# Define the server logic
server <- function(input, output, session) {
  
  output$plot_genderpoker <- renderPlot({
    
    ggplot(genderpoker_long %>%
             filter(variable == input$Variable)%>%
             group_by(Gender) %>%
             summarize(tot_value = sum(value)), 
           aes(Gender,tot_value,fill=as.factor(Gender))) +
      geom_bar(position="dodge",stat="identity") +
      geom_text(aes(x = Gender, y = tot_value, label = round(tot_value, 2), vjust = -1))+
      theme_minimal() +
      labs (y= "Total", x = "Gender") +
      theme(
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank()
    )
  }, height = 600) 
  
  output$plot_applicationpoker <- renderPlot({
    
    ggplot(applicationpoker_long %>%
             filter(variable == input$Application)%>%
             group_by(BettingApplication) %>%
             summarize(tot_value = sum(value)), 
           aes(BettingApplication,tot_value,fill=as.factor(BettingApplication))) +
      geom_bar(position="dodge",stat="identity") +
      geom_text(aes(x = BettingApplication, y = tot_value, label = round(tot_value, 2), vjust = -1))+
      theme_minimal() +
      labs (y= "Total", x = "Betting Application") +
      theme(
        axis.title.x = element_text(size = 18),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank()
      )
  }, height = 600) 
  
  output$plot_countrypoker <- renderPlot({
    
    ggplot(countrypoker_long %>%
             filter(variable == input$Var)%>%
             group_by(Country) %>%
             summarize(tot_value = sum(value)) %>%
             arrange(desc(input$Var)) %>% 
             top_n(input$Top), 
           aes(factor(Country,levels = Country[order(tot_value, decreasing = TRUE)]),
               tot_value,fill=factor(Country, levels = Country[order(tot_value, decreasing = TRUE)]))) +
      geom_bar(position="dodge",stat="identity") +
      geom_text(aes(x = Country, y = tot_value, label = round(tot_value, 2), vjust = -1))+
      theme_minimal() +
      labs (y= "Total", x = "Country") +
      theme(
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 14),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank()
      )
  }, height = 600) #, height = 800, width = 1200
  
  # ggplot(country_map, aes(long, lat, group = group))+
  #   geom_polygon(aes(fill = count), color = "white")+
  #   scale_fill_viridis_c(option = "C")
  
  output$plot_map <- renderPlot({
    
    ggplot(country_map %>%
             filter(variable == input$Vars) %>%
             group_by(Country, input$Vars) %>%
             summarize(value = mean(value), long=long, lat=lat, group = group), 
           aes(long, lat, group = group)) +
      geom_polygon(aes(fill = value), color = "white") +
      scale_fill_viridis_c(option = "E")
  }, height = 600, width = 900)
   
  # round(seq(0, max(country_map_long$value[country_map_long$variable == 'count']), 
  #     max(country_map_long$value[country_map_long$variable == 'count'])/7))
  
  output$plot_point1 <- renderPlot({
    
    ggplot(datamart_long %>%
             filter(Gender == input$Gender)%>%
             group_by(variable) %>%
             summarize(tot_value = sum(value)), 
           aes(variable,tot_value,fill=as.factor(variable))) +
      geom_bar(position="dodge",stat="identity") +
      geom_text(aes(x = variable, y = tot_value, label = round(tot_value, 2), vjust = -1))+
      theme_minimal() +
      labs (y= "Total Amount", x = "Categories") +
      theme(
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank()
      )
  }, height = 600) 
  
  output$plot_point2 <- renderPlot({
    
    ggplot(demographics_pokerchip_long %>% 
             filter(TransType == input$TransType) %>%
             filter(Gender == input$Gender2) %>%
             group_by(TransMonth) %>%
             summarize(tot_amt = sum(value)), 
           aes(x = TransMonth, y = tot_amt)) +
      geom_line() + #stat="identity", fill="steelblue"
      geom_point() +
      geom_text(aes(x = TransMonth, y = tot_amt, label = round(tot_amt, 2), vjust = -1))+
      theme_minimal() +
      labs (y= "Total Amount", x = "Months") +
      theme(
        axis.title.x = element_text(size = 18),
        axis.text.x = element_text(size = 13),
        axis.title.y = element_text(size = 18),
        axis.text.y = element_text(size = 14),
        legend.text = element_text(size = 14),
        legend.title = element_blank()
      )
  }) 
  
}

# Run the application
shinyApp(ui = ui, server = server, options = list(host = ("0.0.0.0"),port = 6969))



 
rsconnect::setAccountInfo(name='group7-bat-opensource-online-gambling-group-assignment', 
                         token='3B930CF0116745D821A73C2EB1E99E17', 
                         secret='gynhMAl77xKgC/GszAhpgXi3IJIUT3/sQpgy6QLd')

library(rsconnect)
rsconnect::deployApp("C:/Group assignment/Group7-online-gambling-open-source/rsconnect/shinyapps.io")
#C:/Users/arosato/Desktop/COURSE MATERIALS/X_8_BUSINESS ANALYTICS TOOLS - OPEN SOURCE (R)/OpenSourceProgramming/Group assignment/Group7-online-gambling-open-source/rsconnect/shinyapps.io/group7-bat-opensource-online-gambling-group-assignment/app.R