
library(shiny)


ui <- fluidPage(
    fluidRow(
        column(3,
               style = "background-color: lightgreen",
               h4("Please Input Your Basic Health Information"),
               wellPanel(
                   div(style="display: inline-block; vertical-align:top;  width:200px;",
                       strong("Current Age:"),
                       textInput("age",NULL, width = 100),
                       helpText("*minimum age is 50"))
                   
               ),
               
               wellPanel(
                   div(style="display: inline-block; vertical-align:top;  width:110px;",
                       strong("Height (cm):"),
                       textInput("height",NULL, width = 90)),
                   
                   div(style="display: inline-block; vertical-align:top;  width:110px;",
                       strong("Weight (kg):"),
                       textInput("weight",NULL, width = 90)),
                   
                   br(),
                   
                   div(style="display: inline-block; vertical-align:top;  width:120px;",
                       strong("Systolic Blood Pressure (mmHg):"),
                       textInput("bp_s",NULL, width = 90)),
                   
                   div(style="display: inline-block; vertical-align:top;  width:120px;",
                       strong("Diastolic Blood Pressure (mmHg):"),
                       textInput("bp_d",NULL, width = 90))
               ),
               
               wellPanel(
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       strong("Red Cell Distribution Width (%):"),
                       textInput("rcdw",NULL, width = 90)),
                   
                   br(),
                   
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       strong("Dietary Moisture (gm):"),
                       textInput("moisture",NULL, width = 90)),
                   
                   br(),
                   
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       strong("Waist Circumference (cm):"),
                       textInput("waist",NULL, width = 90)),
                   
                   br(),
                   
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       strong("Maximum Inflation Levels (mmHg):"),
                       textInput("inflation",NULL, width = 90))  
               )
               ),
        
        
        column(3,
               style = "background-color: lightblue",
               h4("Please Answer Following Questions"),
               wellPanel(
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       strong("How many close friends do you have?"),
                       textInput("closefriend",NULL, width = 90),
                       helpText("*close friends refer to relatives or non-relatives that you at ease with, can talk to about private matters and can call on for help")),
                   
                   br(),
                   
                   div(style="display: inline-block; vertical-align:top;  width:220px;",
                       radioButtons("absentjob","During the past 12 months, did you miss work at a job or business because of an illness or injury?",
                                    c("Yes"="1","No"="2"))),
                   radioButtons("congestive","Has a doctor or other health professional ever told you that you had congestive heart failure?",
                                c("Yes"="1","No"="2","Don't know"="9")),
                   radioButtons("bloodtransfusion","Have you ever received blood transfusion?",
                                c("Yes"="1","No"="2","Don't know"="9")),
                   radioButtons("weightcognition","How do you consider your weight?",
                                c("Overweight"="1","Underweight"="2","About the right weight"="3","Refuse to answer"="7","Don't know"="9")),
                   radioButtons("weighexpectation","Would you like to weigh:",
                                c("More"="1","Less"="2","Stay about the same"="3","Refuse to answer"="7","Don't know"="9")),
                   radioButtons("generalhealth","In general, would you say your health is:",
                                c("Excellent" = "1","Very Good"="2","Good"="3","Fair"="4","Poor"="5"))
               )),
        
        column(6,
               style = "background-color: lightpink",
               h4("Predict Your Probability of Death in 9-Year"),
               wellPanel(
                   # add a helper message
                   helpText("*Click to select a prediction model"),
                   
                   selectInput("model_select", "Prediction Model:",
                               c("Logistic Regression Model" = "logit",
                                 "Logistic Regression with Polynomial Term" = "logit_poly",
                                 "Best Subset Selection Model" = "best_sub",
                                 "LASSO Model" = "lasso",
                                 "Tree-base Adaptive Boosting" = "boosting")
                   ),
                   
                   submitButton("Apply Model",icon("refresh")),
                   
                   wellPanel(
                       helpText(textOutput("parametercheck"))
                   )
               ))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    # read the pre-processed nhanes data from github
    dat<-read.csv("https://raw.githubusercontent.com/AlexWang146/DataScience-Final/master/DS_final.csv")
    dat<-dat[,-1]
    
    # transform and entry the input value
    predict_dat<-data.frame(matrix(rep(NA,ncol(dat)),nrow=1))
    colnames(predict_dat)<-colnames(dat)
    predict_dat<-predict_dat[,-2]
   
   # input the age
   age<-reactive({
       as.numeric(input$age)
   })
   
   # input the red cell distribution width
   LBXRDW<-reactive({
       as.numeric(input$rcdw)
   })
   
   # input the bmi
   BMXBMI<-reactive({
       (as.numeric(input$weight)/(as.numeric(input$height)/100)^2) 
   })
   
   # input the diastolic pressure
   BPXDI<-reactive({
       as.numeric(input$bp_d)
   })
   
   # input the dietary moisture
   DR1TMOIS<-reactive({
       as.numeric(input$moisture)
   })
   
   # input the waist circumference
   BMXWAIST<-reactive({
       as.numeric(input$waist)
   })
   
   # input the systolic pressure
   BPXSY<-reactive({
       as.numeric(input$bp_s)
   })
   
   # input the maximum inflation levels
   BPXML1<-reactive({
       as.numeric(input$inflation)
   })
   
   # input the close friend number
   SSQ061<-reactive({
       as.numeric(input$closefriend)
   })
   
   # input the general health
   HSD010<-reactive({
       as.factor(input$generalhealth)
   })
   
   # input the miss work due to disease
   MCQ245A<-reactive({
       as.factor(input$absentjob)
   })
   
   # input the congestive heart disease
   MCQ160B<-reactive({
       as.factor(input$congestive)
   })
   
   # input the weight cognition
   WHQ030<-reactive({
       as.factor(input$weightcognition)
   })
   
   # input blood transfusion
   MCQ092<-reactive({
       as.factor(input$bloodtransfusion)
   })
   
   # input the weight expecatation
   WHQ040<-reactive({
       as.factor(input$weighexpectation)
   })
   
   output$parametercheck<-renderText({
       if (is.na(age()) | is.na(LBXRDW()) | is.na(BMXBMI()) | 
           is.na(BPXDI()) | is.na(DR1TMOIS()) | is.na(BMXWAIST())|
           is.na(BPXSY()) | is.na(BPXML1()) | is.na(SSQ061())){
           "*** Health Information Missing ***"
       } else if(!between(age(),50,200) | !between(LBXRDW(),0,100) |
                 !between(BMXBMI(),10,40) | !between(BPXDI(),30,150) | 
                 !between(DR1TMOIS(),0,20000) | !between(BMXWAIST(),20,300) |
                 !between(BPXSY(),50,250) | !between(BPXML1(),0,300) | !between(SSQ061(),0,150)){
              "*** Health Information Out of Range ***" 
           }
   })

   
}

# Run the application 
shinyApp(ui = ui, server = server)