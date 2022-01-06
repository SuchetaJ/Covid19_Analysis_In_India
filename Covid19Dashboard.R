#Covid 19 Analysis in India
#Name: Sucheta Jhunjhunwala
#Roll Number: MDS202151

#dashboard link: https://suchetajjw.shinyapps.io/Covid19Dashboard/



library(shiny)
library(shinydashboard)
library(lubridate)
library(tidyverse)
library(dplyr)
library(reshape2)
library(gganimate)
theme_set(theme_bw())
library(gifski)
library(plotly)
library(png)

covid_data = data.frame(read.csv('covid_19_india.csv',stringsAsFactors = F))
covid_data = subset(covid_data,select=-c(Sno,ConfirmedIndianNational,ConfirmedForeignNational,Time))
covid_data$Date = as.Date(covid_data$Date,format = c("%d-%m-%y"))
#We can see there are some repetitions of states with the same name. We shall fix this by renaming them.
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Maharashtra***']='Maharashtra'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Madhya Pradesh***']='Madhya Pradesh'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Bihar****']='Bihar'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Daman & Diu']='Dadra
and Nagar Haveli and Daman and Diu'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Dadra and Nagar Haveli']=
    'Dadra and Nagar Haveli and Daman and Diu'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Himanchal Pradesh']=
    'Himachal Pradesh'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Karanataka']='Karnataka'
covid_data['State.UnionTerritory'][covid_data['State.UnionTerritory']=='Telengana']='Telangana'
#Some values in the column State.UnionTerritory are not w.r.t a state. We can remove the data
#corresponding to them.
covid_data = covid_data[covid_data['State.UnionTerritory']!='Cases being reassigned to states',]
covid_data = covid_data[covid_data['State.UnionTerritory']!='Unassigned',]

testing_data = data.frame(read.csv('StatewiseTestingDetails.csv',stringsAsFactors = F))

#Some of the values in the data is NA so we shall remove these
testing_data[is.na(testing_data)]=0
#Convert the string type 'Date; variable to Date type variable.
testing_data$Date = as.Date(testing_data$Date,format = c("%Y-%m-%d"))

testing_state =testing_data%>%group_by(State,Date)%>%summarise(Positive=sum(Positive), 
                                            TotalSamples=sum(TotalSamples),Negative=sum(Negative))

covid_state = covid_data%>%group_by(State.UnionTerritory)
#Let us now look at the monthly data of each state. We shall create a new column
#corresponding to month.
covid_state[['Month']]=month(covid_state$Date)
covid_state_month = covid_state%>%group_by(State.UnionTerritory,Month)%>%
    summarise(Cured = sum(Cured),Deaths = sum(Deaths), Confirmed = sum(Confirmed))

#Comparison between number of tests,positive and negative results state-wise.
#Let us group the data state-wise
testing_state = testing_data%>%group_by(State,Date)%>%summarise(Positive=sum(Positive), 
                                                  TotalSamples=sum(TotalSamples),Negative=sum(Negative))
testing_state[['Month']]=month(testing_state$Date)
testing_state_month = testing_state%>%group_by(State,Month)%>%summarise(Positive=
                                                   sum(Positive), TotalSamples=sum(TotalSamples),Negative=sum(Negative))



covid_state = covid_data%>%group_by(State.UnionTerritory)
#Find the total number of covid cases, deaths and recoveries in each state
covid_state_sum = covid_state%>%summarise(Cured = sum(Cured),Deaths = sum(Deaths), Confirmed
                                          = sum(Confirmed))



covid_date = covid_data%>%group_by(Date)
covid_date_sum = covid_date%>%summarise(Cured = sum(Cured),Deaths = sum(Deaths), Confirmed =
                                            sum(Confirmed))
jan_date = seq.Date(from =as.Date("01-01-2020", "%d-%m-%y"),
                    to=as.Date("31-01-2020", "%d-%m-%Y"), by="day")
feb_date = seq.Date(from =as.Date("01-02-2020", "%d-%m-%y"),
                    to=as.Date("29-02-2020", "%d-%m-%Y"), by="day")
mar_date = seq.Date(from =as.Date("01-03-2020", "%d-%m-%y"),
                    to=as.Date("31-03-2020", "%d-%m-%Y"), by="day")
apr_date = seq.Date(from =as.Date("01-04-2020", "%d-%m-%y"),
                    to=as.Date("30-04-2020", "%d-%m-%Y"), by="day")
may_date = seq.Date(from =as.Date("01-05-2020", "%d-%m-%y"),
                    to=as.Date("31-05-2020", "%d-%m-%Y"), by="day")
jun_date = seq.Date(from =as.Date("01-06-2020", "%d-%m-%y"),
                    to=as.Date("30-06-2020", "%d-%m-%Y"), by="day")
jul_date = seq.Date(from =as.Date("01-07-2020", "%d-%m-%y"),
                    to=as.Date("31-07-2020", "%d-%m-%Y"), by="day")
aug_date = seq.Date(from =as.Date("01-08-2020", "%d-%m-%y"),
                    to=as.Date("31-08-2020", "%d-%m-%Y"), by="day")
sep_date = seq.Date(from =as.Date("01-09-2020", "%d-%m-%y"),
                    to=as.Date("30-09-2020", "%d-%m-%Y"), by="day")
oct_date = seq.Date(from =as.Date("01-10-2020", "%d-%m-%y"),
                    to=as.Date("31-10-2020", "%d-%m-%Y"), by="day")
nov_date = seq.Date(from =as.Date("01-11-2020", "%d-%m-%y"),
                    to=as.Date("30-11-2020", "%d-%m-%Y"), by="day")
dec_date = seq.Date(from =as.Date("01-12-2020", "%d-%m-%y"),
                    to=as.Date("31-12-2020", "%d-%m-%Y"), by="day")



#We shall look at the top 6 states with the highest number of Covid cases
top = sort(covid_state_sum$Confirmed)[31:36]
highest_cases = covid_state_sum[covid_state_sum$Confirmed %in% top,]
#We shall look at the top 6 states with the highest number of deaths
top = sort(covid_state_sum$Deaths)[31:36]
highest_deaths = covid_state_sum[covid_state_sum$Deaths %in% top,]
#We shall look at the top 6 states with the highest number of recoveries
top = sort(covid_state_sum$Cured)[31:36]
highest_cured = covid_state_sum[covid_state_sum$Cured %in% top,]


# Define UI for application that draws a histogram
ui = dashboardPage(
    dashboardHeader(title ='Covid 19 Analysis in India',titleWidth = 300),
    dashboardSidebar(
        sidebarMenu(
            menuItem('About',tabName ='about'),
            menuItem('Covid Data',tabName ='stats'),
            menuItem('Testing Data',tabName = 'tests'))),
    dashboardBody(tabItems(
        tabItem(tabName ='about',h1("Analysis of Covid 19 in india statewise.",h3('The coronavirus disease (COVID-19) pandemic, which originated in the city of Wuhan,
China, has quickly spread to various countries, with many cases having been reported worldwide.
The first cases of COVID-19 in India were reported on 30 January 2020 in three towns of Kerala,
among three Indian medical students who had returned from Wuhan, the epicenter of the pandemic.
The number of Coronairus cases have been increasing rapidly in India since then. The aim of
the project is to analyse the trends in Covid-19 cases in India from January,2020 to January,2021.
The project has two datasets, covid_19_india.csv and StatewiseTestingDetails.csv.')),
                mainPanel(
                    fluidRow(
                        splitLayout(cellWidths = c("45%", "45%",'45%'),
                    plotOutput('pie1'),
                    plotOutput('pie2'),
                    plotOutput('pie3')
                )))),
        tabItem(tabName = 'stats',
            fluidPage(
                tabsetPanel(
                    tabPanel('Daily',h3("Daily data of each State or Union Territory"),
                             h4("We shall look at the data of each chosen State or Union Territory 
                                corresponding to the chosen date."),fluid=TRUE,
                            sidebarLayout(
                                sidebarPanel(
                                    div(style = "font-size:16px; margin: 14px; padding: 14px 28px",
                                    selectInput('state1',label ='Choose the state or Union Territory', 
                                                choices = sort(unique(covid_data$State.UnionTerritory))
                                                    ),
                                    uiOutput("secondSelection")
                                    )),
                                mainPanel(
                                    plotOutput('bar')
                                         )
                                     )
                                 ),
                        tabPanel('Monthly',h3("Monthly data of each State or Union Territory"),
                                 h4('We shall look at two plots, a barplot corresponding to each State or
                                    Union Territory for each month and a gif depicting the trends each month.'),fluid = TRUE,
                                 sidebarLayout(
                                     sidebarPanel(
                                         selectInput('state2',label ='Choose the state or Union Territory', 
                                                     choices = sort(unique(covid_data$State.UnionTerritory))
                                         ),
                                         selectInput('month',label='Choose the Month',
                                                     choices = unique(covid_state_month$Month))
                                 ),
                                 mainPanel(
                                     plotOutput('barMonth'),
                                     imageOutput('anim')
                                     #plotlyOutput(outputId = 'anim')
                                 )
                                 ))
                       
                    ))
            ),
        tabItem(tabName='tests',
                fluidPage(
                    tabsetPanel(
                        tabPanel('Overall',h3("Overall Testing Results of each State or Union Territory."),
                                 h4("Testing had begun in India from 2020-04-01 and the tests conducted 
                                    either had a positive or a negative result."),fluid=TRUE,
                                 sidebarLayout(
                                     sidebarPanel(
                                         selectInput('type',label='Choose the type of sample: ',choices = c('Positive','Negative','Total Samples'))
                                         
                                     ),
                                     mainPanel(
                                         plotOutput('barSam')
                                     )
                                 )),
                        tabPanel('Monthly',h4("Monthly testing data of each State or Union Territory"),fluid = TRUE,
                                 sidebarLayout(
                                     sidebarPanel(
                                         selectInput('stateT',label ='Choose the State or Union Territory', 
                                                     choices = sort(unique(testing_data$State))
                                         ),
                                         selectInput('monthT',label='Choose the Month',
                                                     choices = unique(testing_state_month$Month))
                                     ),
                                     mainPanel(
                                         plotOutput('barmonthT')
                                     )
                                 ))
                    )
                ))
        
        ))
)

# Define server logic required to draw a histogram
server = function(input, output,session){
    output$secondSelection = renderUI({
        dateInput('date','Date: ',min = covid_data[covid_data$State.UnionTerritory==input$state1,][1,1],
                  max ="2020-12-31",value=covid_data[covid_data$State.UnionTerritory==input$state1,][1,1])
        
    })
    
    output$pie1= renderPlot({pie(as.vector(highest_cases$Confirmed)/100,label=highest_cases$State.UnionTerritory,
        main='Covid Cases in top 6 States')})
    output$pie2 = renderPlot({pie(as.vector(highest_deaths$Deaths),label=highest_deaths$State.UnionTerritory,
                      main='Deaths in top 6 States')})
    output$pie3 = renderPlot({pie(as.vector(highest_cured$Deaths),label=highest_cured$State.UnionTerritory,
                      main='Recoveries in top 6 States')})
    con_case = reactive({
        data = covid_data[covid_data$Date == input$date & covid_data$State.UnionTerritory==input$state1,]
        data[1,]$Confirmed
    })
    cure_case =reactive({
        data = covid_data[covid_data$Date == input$date & covid_data$State.UnionTerritory==input$state1,]
        data[1,]$Cured
    })
    death_case = reactive({
        data = covid_data[covid_data$Date == input$date & covid_data$State.UnionTerritory==input$state1,]
        data[1,]$Deaths
    })
    
    output$bar=renderPlot({
        cases = c(con_case(),cure_case(),death_case())
        labels = c('Confirmed','Cured','Deaths')
        df = data.frame(cases,labels)
        ggplot(df,aes(x=labels,y=cases))+geom_col(aes(color=labels,fill=labels))+xlab(input$state1)+
            ylab('Count')+ggtitle(paste("Cases corresponding to the date:",input$date))+
            theme(legend.title = element_blank())})
    
    animData = reactive({
        if(input$month==1){
            data_anim = covid_date_sum[covid_date_sum$Date %in% jan_date,]
        }
        if(input$month == 2){
            data_anim= covid_date_sum[covid_date_sum$Date %in% feb_date,]
        }
        if(input$month == 3){
            data_anim=covid_date_sum[covid_date_sum$Date %in% mar_date,]
        }
        if(input$month == 4){
            data_anim=covid_date_sum[covid_date_sum$Date %in% apr_date,]
        }
        if(input$month == 5){
            data_anim = covid_date_sum[covid_date_sum$Date %in% may_date,]
        }
        if(input$month == 6){
            data_anim= covid_date_sum[covid_date_sum$Date %in% jun_date,]
        }
        if(input$month == 7){
            data_anim=covid_date_sum[covid_date_sum$Date %in% jul_date,]
        }
        if(input$month == 8){
            data_anim=covid_date_sum[covid_date_sum$Datee %in% aug_date,]
        }
        if(input$month == 9){
            data_anim = covid_date_sum[covid_date_sum$Date %in% sep_date,]
        }
        if(input$month == 10){
            data_anim= covid_date_sum[covid_date_sum$Date %in% oct_date,]
        }
        if(input$month == 11){
            data_anim=covid_date_sum[covid_date_sum$Date %in% nov_date,]
        }
        if(input$month == 12){
            data_anim=covid_date_sum[covid_date_sum$Date %in% dec_date,]
        }
        data_anim
        
    })  
    lab_mon = reactive({months(as.Date(input$Date,origin='2019-12-31'))})
        
        #Plot the number of deaths, recoveries and cases per day in January,2020
        output$anim = renderImage({ 
            
            p=ggplot(animData(),aes(Date))+geom_line(aes(y=Confirmed,colour='Confirmed'))+
            geom_line(aes(y=Cured,colour='Cured'))+geom_line(aes(y=Deaths,colour='Deaths'))+
            ylab('Number')+ggtitle('Cases, Deaths and Recoveries monthly Animation ')+transition_reveal(Date)
            p
            anim_save("outfile.gif", animate(p, renderer = gifski_renderer()))
            
            list(src = "outfile.gif", contentType = "image/gif")
        })

    
    con_month = reactive({
        data2 = covid_state_month[covid_state_month$Month==input$month & 
                                      covid_state_month$State.UnionTerritory==input$state2,]
        data2$Confirmed
    })
    cur_month = reactive({
        data2 = covid_state_month[covid_state_month$Month==input$month & 
                                      covid_state_month$State.UnionTerritory==input$state2,]
        data2$Cured
    })
    dea_month = reactive({
        data2 = covid_state_month[covid_state_month$Month==input$month & 
                                      covid_state_month$State.UnionTerritory==input$state2,]
        data2$Deaths
    })
    
    output$barMonth = renderPlot({
        monthly = c(con_month(),cur_month(),dea_month())
        labels2 = c('Confirmed','Cured','Deaths')
        df2 = data.frame(monthly,labels2)
        ggplot(df2,aes(x=labels2,y=monthly))+geom_col(aes(color=labels2,fill=labels2))+xlab(input$state2)+
            ylab("Count")+ggtitle(paste("Cases Corresponding to the Month:",input$month))+
            theme(legend.title = element_blank())
        })
    
    
    #Comparison between number of tests,positive and negative results state-wise.
    #Let us group the data state-wise

    output$barSam = renderPlot({
    if (input$type == 'Positive'){
            ggplot(testing_state, aes(x=State, y=Positive))+
            geom_col(fill="lightblue")+coord_flip()+xlab("State/Union Territory")
    }
    else if (input$type == 'Negative'){
        ggplot(testing_state, aes(x=State, y=Negative))+
        geom_col(fill="lightgreen")+coord_flip()+xlab("State/Union Territory")
    }
    else if (input$type == 'Total Samples'){
        ggplot(testing_state, aes(x=State, y=TotalSamples))+
        geom_col(fill="lightpink")+coord_flip()+xlab("State/Union Territory")
    }
})
    
    pos_month = reactive({
        dataT = testing_state_month[testing_state_month$Month==input$monthT & 
                                        testing_state_month$State==input$stateT,]
        dataT$Positive
    })
    neg_month = reactive({
        dataT = testing_state_month[testing_state_month$Month==input$monthT & 
                                      testing_state_month$State==input$stateT,]
        dataT$Negative
    })
    tot_month = reactive({
        dataT = testing_state_month[testing_state_month$Month==input$monthT & 
                                        testing_state_month$State==input$stateT,]
        dataT$TotalSamples
    })
    
    output$barmonthT = renderPlot({
        monthlyT = c(pos_month(),neg_month(),tot_month())
        labelsT = c('Positive','Negtive','Total Samples')
        dfT = data.frame(monthlyT,labelsT)
        ggplot(dfT,aes(x=labelsT,y=monthlyT))+geom_col(aes(color=labelsT,fill=labelsT))+xlab(input$stateT)+
            ylab("Count")+ggtitle(paste('Testing Data for Month',input$monthT))+
            theme(legend.title = element_blank())
    })
    

}
# Run the application 
shinyApp(ui = ui, server = server)

