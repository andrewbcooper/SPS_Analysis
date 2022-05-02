library(shiny)
library(shinyWidgets)
library(ggplot2)
library(dplyr)
library(Cairo)
library(lubridate)
library(stringr)
library(stringi)

load('ForApp.RData')

PrettyPercent <- function(x) {paste0(round(100*x),"%")}

myimpactmetric = c(
  "CumPer",
  "TimeShift"
)

names(myimpactmetric) = c(
  "% of Households Leaving to Work",
  "Change in Start Time relative to Current Start Time"
)

mychoices = c(
  "FRPL" ,                                            
  "American.Indian/.Alaskan.Native",                           
  "Asian",                                              
  "Black/.African.American",                                      
  "Hispanic/.Latino.of.any.race(s)",                              
  "Native.Hawaiian/.Other.Pacific.Islander",                      
  "Two.or.More.Races",                                            
  "White",             
  "English.Language.Learners",                                    
  "Highly.Capable",                                               
  "Homeless",                                                     
  "Low-Income",                                                   
  "Migrant",                                                      
  "Military.Parent",                                              
  "Mobile",                                                       
  "Section.504",                                                  
  "Students.with.Disabilities")                                   

names(mychoices) <- c(
  "% Free or Reduced Paid Lunch" ,                                            
  "% American Indian/Alaskan Native",                           
  "% Asian",                                              
  "% Black/African American",                                      
  "% Hispanic/Latino of any race(s)",                              
  "% Native Hawaiian/ Other Pacific Islander",                      
  "% Two or More Races",                                            
  "% White",             
  "% English Language Learners",                                    
  "% Highly Capable",                                               
  "% Homeless",                                                     
  "% Low-Income",                                                   
  "% Migrant",                                                      
  "% Military Parent",                                              
  "% Mobile",                                                       
  "% Section 504",                                                  
  "% Students with Disabilities")


# Define server logic 
server <- function(input, output,session) {
  
  session <- sessionInfo()
  version <- paste0(session$R.version$major,".",session$R.version$minor)
  
  
  
  SummaryData <- reactive({MyData %>% filter(Name %in% input$school_name) %>% ungroup() %>%
    group_by(LeaveDT) %>%
    summarize(
      Households = sum(estimate),
      TotalHouseholds = sum(Total),
      Percent = sum(estimate)/sum(Total),
      median_hhi = sum(estimate*median_hhi)/sum(Total),
      Total.Students.Agg=sum(Total.Students),
      Female=sum(Female),
      Gender.X=sum(Gender.X),
      Male=sum(Male),
      `American.Indian/.Alaskan.Native`=sum(`American.Indian/.Alaskan.Native`),
      Asian=sum(Asian),
      `Black/.African.American`=sum(`Black/.African.American`),
      `Hispanic/.Latino.of.any.race(s)`=sum(`Hispanic/.Latino.of.any.race(s)`),
      `Native.Hawaiian/.Other.Pacific.Islander`=sum(`Native.Hawaiian/.Other.Pacific.Islander`),
      Two.or.More.Races=sum(Two.or.More.Races),
      White=sum(White),
      English.Language.Learners=sum(English.Language.Learners),
      Foster.Care=sum(Foster.Care),
      Highly.Capable=sum(Highly.Capable),
      Homeless=sum(Homeless),
      `Low-Income`=sum(`Low-Income`),
      Migrant=sum(Migrant),
      Military.Parent=sum(Military.Parent),
      Mobile=sum(Mobile),
      Section.504=sum(Section.504),
      Students.with.Disabilities=sum(Students.with.Disabilities),
      `Non-English.Language.Learners`=sum(`Non-English.Language.Learners`),
      `Non-Foster.Care`=sum(`Non-Foster.Care`),
      `Non-Highly.Capable`=sum(`Non-Highly.Capable`),
      `Non-Homeless`=sum(`Non-Homeless`),
      `Non-Low.Income`=sum(`Non-Low.Income`),
      Non.Migrant=sum(Non.Migrant),
      Non.Military.Parent=sum(Non.Military.Parent),
      Non.Mobile=sum(Non.Mobile),
      Non.Section.504=sum(Non.Section.504),
      Students.without.Disabilities=sum(Students.without.Disabilities),
      `3.Year.Average.for.2020-21.Eligibility` = sum(Total.Students*`3.Year.Average.for.2020-21.Eligibility`)/sum(Total.Students),
      `Preliminary.High.Poverty.Eligibility.for.SY.2020-21` = sum(Total.Students*ifelse(`Preliminary.High.Poverty.Eligibility.for.SY.2020-21`=='Yes',1,0))/sum(Total.Students),
      Title1 = sum(Total.Students*ifelse(Title1 == "No",0,1))/sum(Total.Students),
      ProposedStart.agg = as.POSIXct("2022-04-26 00:00:00", tz="GMT") + dhours(sum(Total.Students*(hour(ProposedStart) + minute(ProposedStart)/60))/sum(Total.Students)),
      CurrentStart.agg = as.POSIXct("2022-04-26 00:00:00", tz="GMT") + dhours(sum(Total.Students*(hour(CurrentStartTime) + minute(CurrentStartTime)/60))/sum(Total.Students)),
      TimeShift.agg = sum(Total.Students*TimeShift)/sum(Total.Students)
      
      
      
    ) %>%
    arrange(LeaveDT) %>%
    mutate(CumPer = cumsum(Percent),
           FRPL = `3.Year.Average.for.2020-21.Eligibility`
           ) %>%
    mutate(Equity_var = case_when(
      input$equity_var == "FRPL" ~ get(input$equity_var),
      TRUE ~ get(input$equity_var)/Total.Students.Agg ),
      Impact_var = case_when(
        input$impact_metric == "CumPer" ~ CumPer,
        TRUE~ TimeShift.agg
      )
      )
  })
  
  EquityData <- reactive({Equity %>% 
      mutate(Equity_var = case_when(
        input$equity_var == "FRPL" ~ get(input$equity_var),
        TRUE ~ get(input$equity_var)/Total.Students ),
        Impact_var = get(input$impact_metric))
        })
  
  EquityLabel = reactive({names(mychoices[which(mychoices==input$equity_var)])})
  
  EquityImpact_label = reactive({
    ifelse(input$impact_metric == "CumPer","% of Households in Census Tract\nleaving at least 30 minutes before proposed school start time",
           "Magnitude of Time Shift (minutes)\nNegative values = proposed is earlier than current")
    })
  EquityImpact_scale = reactive({
  ifelse(input$impact_metric == "CumPer",scales::percent_format(),scales::number_format())
})
  EquityLine = reactive({
    ifelse(input$impact_metric == "TimeShift",1,0)
  })
  output$myGraph <- renderPlot({
    SummaryData() %>% ggplot(aes(x=LeaveDT,y=CumPer)) +
      geom_path(size = 1.5) +
      geom_point(size=3) + 
      
      geom_vline(aes(xintercept=as.POSIXct("2022-04-26 07:30:00", tz="GMT")),color='darkgrey',linetype='dashed',size=1)+
      geom_vline(aes(xintercept=as.POSIXct("2022-04-26 08:30:00", tz="GMT")),color='darkgrey',linetype='dashed',size=1)+
      geom_vline(aes(xintercept=as.POSIXct("2022-04-26 09:30:00", tz="GMT")),color='darkgrey',linetype='dashed',size=1)+
      
      geom_vline(aes(xintercept=ProposedStart.agg[1]),color='red',linetype='dashed',size=1) +
      geom_vline(aes(xintercept=CurrentStart.agg[1]),color='blue',linetype='solid',size=1) +
      
      annotate("text",label="Proposed\nStart\nTime",x=SummaryData()$ProposedStart.agg[1]+minutes(5),y=.15,hjust="left",color='red',size=5) + 
      annotate("text",label="Current\nStart\nTime",x=SummaryData()$CurrentStart.agg[1]+minutes(5),y=.5,hjust="left",color='blue',size=5) + 
      
      scale_y_continuous(name=paste0("% of Households in Census Tract\nLeaving to Work Before X"),labels=scales::percent_format()) +
      scale_x_datetime(name="",date_labels = "%l:%M", date_breaks = '2 hour', limits = c(as.POSIXct("2022-04-26 04:00:00", tz="GMT"),as.POSIXct("2022-04-26 12:00:00", tz="GMT"))) +
      ggtitle(str_wrap(paste(
        stri_replace_all_regex(str=input$school_name,pattern=c("School","International"),replacement = c("",""),vectorize=FALSE),
        collapse = ","),60)) +
      theme_bw() + theme(text = element_text(size = 20))
  })
  
  
  output$equityGraph <- renderPlot({
    EquityData() %>% 
      ggplot(aes(x=Equity_var,y=Impact_var,color=HighPoverty)) + geom_point(size=4) + 
      geom_point(data = subset(EquityData(),Equity$Name %in% input$school_name),aes(x=Equity_var,y=Impact_var,fill=HighPoverty),color="black",size=8,shape=24) +
      facet_grid(~Type) +
      scale_x_continuous(name=EquityLabel(),labels=scales::percent_format()) +
      geom_hline(aes(yintercept=0), color="darkgrey",linetype="dashed",alpha=EquityLine())+
      scale_y_continuous(name = EquityImpact_label(),labels=EquityImpact_scale()) +
      
      scale_color_manual(name = "Preliminary High\nPoverty Eligibility\nfor SY 2020-21",values=c('No' = 'black',"Yes"='red')) +
      scale_fill_manual(name = "Preliminary High\nPoverty Eligibility\nfor SY 2020-21",values=c('No' = 'black',"Yes"='red'),guide="none") +
      
      theme_bw() + theme(text = element_text(size = 15)) 
  })
  
  
  # For the Main page click
  output$click_info <- renderTable({
    silly <- nearPoints(SummaryData(),input$plot_click,threshold = 20) %>% 
      mutate(NameAgg = paste0(
        stri_replace_all_regex(str=input$school_name,pattern=c(" School"," International"),replacement = c("",""),vectorize=FALSE),
        collapse = ","),
        ProposedSTF = format(ProposedStart.agg,'%I:%M'),
        CurrentSTF = format(CurrentStart.agg,'%I:%M'),
        LeaveDTF = format(LeaveDT,'%I:%M'),
        CumPerF = PrettyPercent(CumPer),
        TimeShiftF = round(TimeShift.agg)
        ) %>% select(NameAgg,CurrentSTF,ProposedSTF,TimeShiftF,LeaveDTF,CumPerF)
     names(silly) <- c("School","Current Start Time","Proposed Start Time","Magnitude of Shift (minutes)","Selected Time","Percent of Households Leaving before Selected Time")
    silly
  })
  
  
  # For the Equity Clicks and brush
  output$click_info2 <- renderTable({
    
    # This is for the clicking and brushing
    Equity2 <- EquityData() %>% mutate(
      CurrentStartF = format(CurrentStartTime,'%I:%M'),
      ProposedStartF = format(ProposedStart, '%I:%M'),
      TimeShiftF = round(TimeShift),
      XAxisFormat = PrettyPercent(Equity_var),
      CumPerF = PrettyPercent(CumPer)
    )
    
    # This is just for the aggregate group not for click or brush
    Summary2 <- SummaryData()[max(which(SummaryData()$LeaveDT <=  SummaryData()$ProposedStart.agg - minutes(30))),] %>% 
      mutate(
      NameAgg = paste0(
      stri_replace_all_regex(str=input$school_name,pattern=c(" School"," International"),replacement = c("",""),vectorize=FALSE),
      collapse = ","),
      CurrentStartF = format(CurrentStart.agg,'%I:%M'),
      ProposedStartF = format(ProposedStart.agg, '%I:%M'),
      TimeShiftF = round(TimeShift.agg),
      XAxisFormat = PrettyPercent(Equity_var),
      CumPerF = PrettyPercent(CumPer),
      HighPoverty = case_when(
        `Preliminary.High.Poverty.Eligibility.for.SY.2020-21` == 0 ~ "No",
        `Preliminary.High.Poverty.Eligibility.for.SY.2020-21` == 1 ~ "Yes",
        TRUE~PrettyPercent(`Preliminary.High.Poverty.Eligibility.for.SY.2020-21`))
    )
    
    silly0 <- Summary2 %>% select(Name = NameAgg,CurrentStartF,ProposedStartF,XAxisFormat,TimeShiftF,CumPerF,HighPoverty)
    silly1 <- nearPoints(Equity2,input$plot_click2) %>% select(Name,CurrentStartF,ProposedStartF,XAxisFormat,TimeShiftF,CumPerF,HighPoverty)
    silly2 <- brushedPoints(Equity2,input$plot_brush) %>% select(Name,CurrentStartF,ProposedStartF,XAxisFormat,TimeShiftF,CumPerF,HighPoverty)
    silly <- rbind(silly0,silly1,silly2)
    
    names(silly) <- c("School","Current Start Time","Proposed Start Time",EquityLabel(),"Magnitude of Shift (minutes)","% of Households Leaving at least 30 minutes before Proposed School Start Time","Preliminary High Poverty Eligibility")
    
    silly
  })
  
  
  
  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel('SPS Start Times Exploration Tool'),
  sidebarLayout(
    sidebarPanel(
      
      h5("This tool compares the proposed SPS start times to data from US Census Bureau's 2016-2020 American Community Survey (ACS) on the percent of households departing for work by certain times
         and to student demographic data from the Washington Office of Superientendent of Public Instruction's (OSPI). The impact of the propsed start times are measured in two ways: 
         (1) The % of households departing for work prior to the propsed start time; and (2) the magnatidue of the change from the current start time to the proposed start time, measured in minutes.
         Negative values for the magnitude of change mean the proposed start time is earlier than the current start time.  Positive values mean the proposed start time is later than the current start time."),
      
      h4("Full Data Tab:"),
      
      h6("The table in the upper-left shows the percentage of households leaving for work by 7am, 8am, and 9am, grouped by the type of school"),
      h6("The table in the upper-right shows the percentage of households leaving for work by 7am, 8am, and 9am, grouped by the type of school and the proposed start time for those schools. 
         The final column shows the number of schools included in each group.  
         Highlighted cells represent the percent of households that would have to leave for work 30-minutes prior to the proposed school start time."
         ),
      
      h6('The plot shows the full range of data available in the ACS for each school along with the current start time (in blue), proposed start time (in red), and the alternative start times (in grey).
         When more than 1 school is selected, the Current and Proposed Start Times are the average current or proposed start times of the selected schools, weighted by the number of students in those schools.
         The specific values for each point along the line can be observed by clicking on that point.'),
      
      h4("Equity Analysis Tab:"),
      
      h6("This plot shows how schools compare to one another based on the ACS and OSPI data. Use the drop-down, below, to select how you want to measure the impact of the proposed change. 
      Use the other drop-down, to select the specific equity variable / metric upon which to base the comparisons. Variable / metric names are taken directly from the OSPI's Report Card Enrollment data cited below. 
      The color of the dots represents OSPI's preliminary determination whether the school is eligible for 'High Poverty LAP Funding' for the 2020-21 school year.
      The school(s) selected in the drop-down will appear as triangles in the plot"),
      
      h6("Users can click on single points or click-and-drag multiple points to get more information about the specific schools.
         The weighted-average values for the schools selected in the drop-down are automatically displayed."),
      
      br(),
      pickerInput("school_name", "Select a school",
                  AllSchools,
                  multiple=TRUE,selected=AllSchools[1],
                  options = pickerOptions(
                    title = 'Click to see options'
                  )
      ),
      
      pickerInput("impact_metric", "How to measure impact",
                  myimpactmetric,
                  multiple=FALSE,selected=myimpactmetric[1],
                  options = pickerOptions(
                    title = 'Click to see options'
                  )
      ),
      
      pickerInput("equity_var", "Select an Equity Measure for Equity Analysis",
                  mychoices,
                  multiple=FALSE,selected='FRPL',
                  options = pickerOptions(
                    title = 'Click to see options'
                  )
      ),

      hr(),
      HTML("For non-option schools, the ACS data is linked to the Census Block Groups that lie within school's attendance area boundaries.  
      For option schools, the ACS data is linked to those Census Block Groups within a 2-mile drive of the school. In both cases, ACS data may not represent all families with students attending that school.  
         More information on the ACS can be found at <a href='https://www.census.gov/programs-surveys/acs'>the ACS web page</a>.  
           School start times were taken from the <a href='https://www.seattleschools.org/resources/bell-schedules/bell-time-changes/'>SPS Bell Time Changes web page</a>.
           OSPI data on FRPL can be found through their <a href='https://www.k12.wa.us/data-reporting/data-portal'>Data Portal</a> or the data can be downloaded directly 
           via <a href='https://www.k12.wa.us/sites/default/files/public/safs/misc/budprep20/PovertyPercentageMar31.xlsx'>Poverty Percentages for the 2020–21 School Year - By District for LAP and By School for Additional High Poverty LAP Funding</a>.
           Other student demographic data was obtained through <a href='https://data.wa.gov/education/Report-Card-Enrollment-2021-22-School-Year/ymi4-syjv?fbclid=IwAR3WbEewtYX9BEyY8u6IeAdx697zN7ZnHjSQAcmnR6sH4--LmOZg2CpSb6o'>
           The Report Card Enrollment 2021-22 School Year</a>. School Attendance Boundaries for the 2021-22 School Year were obtained from <a href='https://www.seattleschools.org/departments/enrollment-planning/enrollment-data/maps/'>Seattle Public Schools<a/>."),
      br(),
      br(),
      HTML("Please direct questions, comments, and suggestions to <a href='mailto:acooper@alumni.washington.edu'>Andy Cooper</a>"),
      HTML("Data and code can be found at <a href='https://github.com/andrewbcooper/SPS_Analysis'>Andy's GitHub Page</a>.")
      
      
    
   
  ),
    mainPanel(
    tabsetPanel(type="tabs",
            tabPanel("Full Data",

      fluidRow(
        column(width=5,Summary1Table),
        column(width=7, Summary2Table)),
      fluidRow(
       plotOutput("myGraph",click="plot_click"),
       h4("Click a point in the graph, above, for more information"),
       tableOutput("click_info"))),
      
        tabPanel("Equity Analysis",
                 plotOutput("equityGraph",height='600px',click="plot_click2",brush="plot_brush"),
                 h4("Click a point or click-and-drag to select multiple points for more information"),
                 tableOutput("click_info2")
                 )
    
    ))

))

shinyApp(ui = ui, server = server)
