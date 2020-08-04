#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(tidyverse)
library(RColorBrewer)
library(colorRamps)
library(shiny)
library(plotly)

quiz<-read.csv("quiz-categories.csv",stringsAsFactors = FALSE,header = TRUE)
midterm<-read.csv("midterm-results.csv",stringsAsFactors = FALSE,header = TRUE)


#transforming wide data into long data
quiz_reshape<-gather(quiz,categories,cover,-Question)

#removing necessary strings
quiz_reshape$Question<-gsub("Q","",quiz_reshape$Question)
quiz_reshape$Question<-gsub("_c","",quiz_reshape$Question)
quiz_reshape$Question<-as.numeric(quiz_reshape$Question)


quiz_reshape$Question<-factor(quiz_reshape$Question)

#filter for topic involved only, 0 values were removed
quiz_reshape1<-filter(quiz_reshape,cover!=0)



# Define the number of colors you want
nb.cols <- length(unique(quiz_reshape1$categories))
mycolors <- colorRampPalette(brewer.pal(9, "GnBu"))(nb.cols)


#item difficulty; using classical item theories
item_diff<-data.frame(colMeans(midterm[,c(3:32)]))
item_diff$id<-1:dim(item_diff)[1]
item_diff$id<-factor(item_diff$id)
names(item_diff)<-c("item_difficulty_rate","Question")

item_diff<-item_diff%>%
    mutate(item_difficulty=case_when(item_difficulty_rate<=0.33 ~"Difficult",
                                     item_difficulty_rate<=0.66 ~"Medium",
                                     item_difficulty_rate<=1 ~"Easy"
    ))


#How each of the student master the topics 
midterm_long<-midterm[,c(1,3:32)]
names(midterm_long)<-c('id',1:30)
midterm_long<-gather(midterm_long,Question,Outcome,-id)
midterm_long<-left_join(midterm_long,quiz_reshape1,by="Question")
midterm_long$Outcome<-factor(midterm_long$Outcome,labels = c("Wrong","Correct"))

test<-subset(midterm_long,id==1962)


#Total score , correct rate and percentile(for rendertable: results)
midterm$totalscore<-rowSums(midterm[,3:32]) 
midterm$correctrate<-rowMeans(midterm[,3:32])
midterm$percentile<-percent_rank(rowSums(midterm[,3:32]))

#How much time each student spends on the each question item
midterm_items<-midterm[,c(1,33:88)]
even<-seq(2,57,by=2)#select the even colume
midterm_items_time<-midterm_items[,even]
names(midterm_items_time)<-c(1:7,8.9,10,11,12.13,14:30)
midterm_items_time<-cbind(midterm_items[,1:2],midterm_items_time) 
midterm_items_time<-midterm_items_time[,c(1,3:30)]#change the name, to ensure all the colnames gathered are in same type

midterm_items_time_long<-gather(midterm_items_time,Question,Time,-id)
midterm_items_time_long$Question<-factor(midterm_items_time_long$Question,c(1:7,8.9,10,11,12.13,14:30))

#To highlight
#Since in the clickcount and timing part, several items were combined together;here we reshape the data and adjust the sequence of the column
correct<-midterm[,c(1,3:32)]
correct$'8.9'<-correct$Q8_c+correct$Q9_c
correct$`8.9`<-ifelse(correct$`8.9`==2,1,0)

correct$'12.13'<-correct$Q12_c+correct$Q13_c
correct$`12.13`<-ifelse(correct$`12.13`==2,1,0)

#adjust sequence
correct<-correct[,-c(9,10,13,14)]
correct<-correct[,c(1:8,28,9:10,29,11:27)]
names(correct)<-c('id',1:7,8.9,10,11,12.13,14:30)

#from wide to long
correct<-gather(correct,Question,Outcome,-id)
correct$Question<-factor(correct$Question,c(1:7,8.9,10,11,12.13,14:30))
correct$Outcome<-factor(correct$Outcome,labels = c("Wrong","Correct"))

#left_join base on two column, to add the correct feature to the graph
a<-left_join(midterm_items_time_long,correct,by=c("id","Question"))

#single example
test1<-subset(a,id==1170)


#How many times the student has clicked for each question item,clickcount
midterm_items_clickcount<-midterm_items[,-even]
names(midterm_items_clickcount)<-c('id',1:7,8.9,10,11,12.13,14:30)
midterm_items_clickcount<-gather(midterm_items_clickcount,Question,Click,-id)
midterm_items_clickcount$Question<-factor(midterm_items_clickcount$Question,c(1:7,8.9,10,11,12.13,14:30))

#left_join base on two column, to add the correct feature to the graph
a1<-left_join(midterm_items_clickcount,correct,by=c("id","Question"))
#single example
test2<-subset(a1,id==1170)  


#create a table where all the information are included for the interactive plot
relation<-left_join(a,a1,by=c("id","Question"))
relation<-relation[,-c(2,4)]
head(relation)

ggplot(relation, aes(x=Time,y=Click,col=Outcome.y,size=Click))+
    geom_point()+
    theme(rect=element_blank())



# Define UI for application that draws a histogram
ui <- fluidPage(
    h1("Midterm Results Dashboard"),
    sidebarLayout(
        sidebarPanel("Welcome! Congrats! It's been half of the semster, let's check how well you are doing so far.
               Don't worry if you find the result least satisfactory. It's only a diagonisc report with 
               possible measurement error. Take it as a stepping stone for your next progress. Good luck!",
                     
                     selectInput("studentid",label="Please select your id to see your quiz
                           results and corresponding data information in the 
                           the follwing tabs",choices=unique(midterm$id)),
                     tableOutput("studentouptput")
                     
        ),
        
        mainPanel(
            tabsetPanel(
                
                tabPanel("General",
                         h4("About the quiz question items"),
                         actionButton("topic_cover","Topic_Coverage"),
                         actionButton("topic_dist","Topic_Distribution"),
                         actionButton("item_diff","Item_difficulty"),
                         plotlyOutput("topic"),
                         plotlyOutput("topic2"),
                         plotlyOutput("itdiff")
                ),
                
                tabPanel("Individual Diagonistic",
                         h4("Individual's test monitoring"),
                         plotlyOutput("student1"),
                         plotlyOutput("student2"),
                         plotlyOutput("mastery")
                ),
                
                tabPanel("Time v.s Clickcount v.s Correct Rate",
                         h4("Exploring Relationship"),
                         selectInput("x","X-axis",choices = c("Time","Outcome.y","Click")),
                         selectInput("y","Y-axis",choices = c("Click","Outcome.y","Time")),
                         selectInput("col","Color",choices = c("Outcome.y","Time","Click")),
                         selectInput("size","Point Size",choices = c("Click","Outcome.y","Time")),
                         plotlyOutput("relation")
                )
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$studentouptput<-renderTable({
        filter(midterm[,c(1,89:91)],id==input$studentid)
    })
    
    #Activation by a button
    observeEvent(input$item_diff,
                 output$itdiff<-renderPlotly({
                     plot1<-ggplot(item_diff,aes(x=Question,y=item_difficulty_rate))+
                         geom_line(group=1)+
                         geom_point(aes(col=item_difficulty),alpha=0.8)+
                         theme(rect=element_blank(),
                               line=element_blank())+
                         labs(title = "Distribution of Question items with Different Difficulty")
                     
                     ggplotly(plot1)
                 }))
    
    observeEvent(input$topic_cover,
                 output$topic <- renderPlotly({
                     plot1<-ggplot(quiz_reshape1,aes(x=Question,fill=categories))+
                         geom_bar()+
                         coord_flip()+ylim(c(0,5))+
                         ylab("Topics and Number")+
                         xlab("Question Item")+
                         theme(rect=element_blank(),
                               line=element_blank())+
                         labs(title = "What is/are Tested in each Question")+
                         scale_fill_manual(values = mycolors)
                     
                     ggplotly(plot1)
                 }))
    
    observeEvent(input$topic_dist,
                 output$topic2 <- renderPlotly({
                     plot1<-ggplot(quiz_reshape1,aes(x=categories,fill=categories))+
                         geom_bar()+ylim(c(0,25))+
                         theme(rect=element_blank(),
                               line=element_blank(),
                               axis.text.x = element_text(angle=90))+
                         xlab("Topics")+
                         labs(title = "Distribution of Topics")+
                         theme(axis.text.x  = element_text(angle=45, hjust = 1))+scale_fill_manual(values = mycolors)
                     ggplotly(plot1)
                 }))
    
    output$student1<-renderPlotly({
        
        test1<-subset(a,id==input$studentid)
        
        p1<-ggplot(test1,aes(x=Question,y=Time))+
            geom_point(aes(col=Outcome))+geom_line(group = 1)+
            theme(rect=element_blank())+
            ylim(c(0,800))+
            labs(title = "Time You Spent on each Question")
        
        # grid.newpage()
        # grid.draw(rbind(ggplotGrob(ggplotly(p1)), ggplotGrob(ggplotly(p2)), size ="last") )  
        ggplotly(p1)
    })
    
    
    output$student2<-renderPlotly({
        
        test2<-subset(a1,id==input$studentid)  
        
        p2<-ggplot(test2,aes(x=Question,y=Click))+
            geom_col(aes(fill=Outcome))+
            theme(rect=element_blank())+
            labs(title = "Times You Click on each Question")
        
        ggplotly(p2)
    })
    
    
    
    
    output$mastery<-renderPlotly({
        test<-subset(midterm_long,id==input$studentid)
        plot1<-ggplot(test,aes(x=categories,fill=Outcome))+
            geom_bar(position = "fill",width = 0.6)+
            theme(rect=element_blank(),
                  line=element_blank(),
                  axis.text.x = element_text(angle=90))+
            xlab("Categories")+
            ylab("Proportion of Correct Answers")+
            labs(title = "Mastery of the Topics")+
            theme(axis.text.x  = element_text(angle=45, hjust = 1))
        ggplotly(plot1)
    })
    
    
    plot1<-reactive({ggplot(data=relation, aes_string(x=input$x,y=input$y,col=input$col,size=input$size))+
            geom_point(alpha=0.8)+
            theme(rect=element_blank())+
            labs(title="Exploring relationship among Clickcount, Time on each items and Test Outcome")
        
    })
    output$relation<-renderPlotly({
        ggplotly(plot1())
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
