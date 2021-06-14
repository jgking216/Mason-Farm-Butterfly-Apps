#load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(scales)

# Define a server for the Shiny app

#load the data, needs to be in the "myapp" folder along with the r script
server<-function(input,output){
  mydat <- read.csv("legrand.kingsolver.mf.dat.8.16.18.csv")
  
  mydat<-mydat[mydat$location == "mf",]
  mydat<-mydat[mydat$number > 0,]
  mydat$date.observed<-trimws(mydat$date.observed, which = c("both"))
  mydat$date.observed2<-paste(mydat$year,mydat$month,mydat$day)
  mydat$date.observed2<-gsub(" ", "-", mydat$date.observed2)
  mydat$date.observed3<-as.Date(mydat$date.observed2)
  mydat$date.observed<-mydat$date.observed3
  mydat$julian<-yday(mydat$date.observed3)
  mydat$year<-signif(mydat$year,4)
  mydat$date.observed2<-NULL
  mydat$date.observed3<-NULL
  mydat$year<-as.numeric(as.character(mydat$year))
  mydat$subfamily_sci<-NULL
  mydat$BAMONA<-NULL
  mydat$location<-NULL
  mydat$source<-NULL
  mydat$image<-NULL
  colnames(mydat)<-c("Family", "Subfamily", "Genus_species","Common_name","Date_observed","Number_observed","Observers","Year","Month","Day","Julian_day")
  
  # Reactive input for data to display
  subsetData <- reactive({
    if(input$show_all_families) newdat<-mydat else newdat<-mydat[mydat$Family == input$Familyname,]
    if(input$show_all_subfamilies) newdat<-newdat else newdat<-newdat[newdat$Subfamily == input$Subfamilyname,]
    if(input$show_all_species) newdat<-newdat else newdat<-newdat[newdat$Common_name == input$Commonname,]
    newdat
  })
  
  fam.dat<-reactive({
    if(input$show_all_families) a<-mydat else a<-mydat[mydat$Family == input$Familyname,]
  })
  
  subfam.dat<-reactive({
    if(input$show_all_subfamilies) b<-fam.dat() else b<-fam.dat()[fam.dat()$Subfamily== input$Subfamilyname,]
    #b <-fam.dat()[fam.dat()$Subfamily == input$Subfamilyname,]
  })
  
  output$table1 <- renderDataTable({
    validate(need(input$Commonname != "", "Please select a maximum of 5 species to display"))
    subsetData()
    })
  
  #creates a dynamic list of "family" values to call in the Subfamily SelectInput dropdown list in the UI
  familyvar<-reactive({
    sort(c(unique(as.character(mydat$Family))))})
  
  output$Familyname<-renderUI({
    selectizeInput("Familyname", "Family:", choices=as.character(familyvar()), multiple = TRUE, options = list(maxItems = 5))
  })
  
  #creates a dynamic list of "Subfamily" values to call in the Subfamily SelectInput dropdown list in the UI
  subfamilyvar<-reactive({
    sort(c(unique(as.character(fam.dat()$Subfamily))))})
  
  output$Subfamilyname<-renderUI({
    selectizeInput("Subfamilyname", "Subfamily:", choices=as.character(subfamilyvar()), multiple = TRUE, options = list(maxItems = 5))
  })
  
  #creates a dynamic list of "Common name" values to call in the Common name SelectInput dropdown list in the UI
  commonnamevar<-reactive({
    sort(c(unique(as.character(subfam.dat()$Common_name))))})
  
  output$Commonname<-renderUI({
    selectizeInput("Commonname", "Common name:", choices=as.character(commonnamevar()),multiple = TRUE, options = list(maxItems = 5))
  })
  
  sum1<-reactive({
    x<-aggregate(Number_observed~Common_name+Month, data=subsetData(), sum, na.rm=TRUE)
    return(x)
    #x$Month<-as.factor(x$Month)
    x$Number_observed<-as.numeric(x()$Number_observed)
    x$Common_name<-as.factor(x()$Common_name)
    })

  #Number of butterflies by julian date plot
  plot1<-reactive({
    validate(need(input$Commonname != "", "Please select a maximum of 5 species to display"))
    
      ggplot(subsetData(), aes(Julian_day, as.numeric(Number_observed)))+
      geom_point(aes(shape=factor(Common_name), colour=Year, size=5))+
      geom_line(aes(linetype=factor(Common_name), size=1))+
      scale_colour_gradient(low="blue",high="red",limits=c(1987,2018),guide="legend")+
      scale_shape_discrete()+
      theme_classic()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,angle=90),legend.title=element_text(size=15), legend.text=element_text(size=15), title=element_text(size=25))+
      theme(legend.position="bottom", legend.direction="vertical")+
      xlim(c(1,365))+
      scale_x_continuous(name="Month", breaks=c(1,32,60,91,121,152,182,213,244,274,305,335), limits=c(1,365),labels=c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
      guides(shape = guide_legend(nrow=20,byrow = TRUE,title="",override.aes = list(size=5)),colour = guide_legend(nrow=20,byrow = TRUE,title="",override.aes = list(size=5)),size=FALSE, linetype=FALSE)+
      ylab("Abundance")+
      xlab("Julian date (ie. Jan. 1 = 1 to Dec. 31 = 365)")})
  
  output$plot1<-renderPlot(plot1(), height = 800, width = 500)
  
  #Number of butterflies aggregated by month
  plot2<-reactive({
    validate(need(input$Commonname != "", "Please select a maximum of 5 species to display"))
    
    ggplot(sum1(), aes(x=Month, y=Number_observed, group=Common_name))+
      geom_point(aes(colour=factor(Common_name), shape=factor(Common_name), size=5))+
      geom_line(aes(colour=factor(Common_name),linetype=factor(Common_name), size=1))+
      theme_classic()+theme(axis.text.x=element_text(size=15),axis.text.y=element_text(size=15),axis.title.x=element_text(size=15),axis.title.y=element_text(size=15,angle=90),legend.title=element_text(size=15), legend.text=element_text(size=15), title=element_text(size=25))+
      theme(legend.position="bottom")+
      ylab("Abundance")+
      xlab("Month")+
      scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12), limits=c(1,12),labels=c("Jan","Feb","Mar","Apr","May","June","July","Aug","Sept","Oct","Nov","Dec"))+
      guides(colour=guide_legend(title="",ncol=3), shape=guide_legend(title="",ncol=3,override.aes = list(size=5)),size=FALSE,linetype=FALSE)

  })
  
  output$plot2 <-renderPlot(
    plot2(), height = 500, width = 500)
  
}

ui<-fluidPage(
  titlePanel(title=div(img(src='mfbp.logo.png'), br(),br(),
                       "Our Observations from the MFBP")),
  sidebarLayout(
    sidebarPanel(
      h3("How to use this app:"),
      p("Please make selections in all 3 boxes below before data display."),
      
      h3("Please select parameters:"),
      
      uiOutput("Familyname"),
      checkboxInput("show_all_families", "Show all Families", FALSE),
      uiOutput("Subfamilyname"),
      checkboxInput("show_all_subfamilies", "Show all Subfamilies", FALSE),
      uiOutput("Commonname"),
      h5("Note: Only a maximum of 5 species will display."),
      checkboxInput("show_all_species", "Show all Species", FALSE)
      
    ),
    
    # Create tabs
    mainPanel(
      tabsetPanel(
        tabPanel("Data", dataTableOutput("table1")),
        tabPanel("Abundance by date plot", plotOutput("plot1")),
        tabPanel("Total by month", plotOutput("plot2"))
      )
    )
  )
)

#app call
shinyApp(ui, server)