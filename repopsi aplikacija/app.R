library(shiny)
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(gsheet)
library(writexl)
library(httr)



# Global

#Downloading the data and cleaning



repopsi <- gsheet2tbl("https://docs.google.com/spreadsheets/d/1A6O1d0S7iBYsKVbdBP9AWdYXMWvrN_lCh_oMldCmNsA/edit#gid=0")

repopsi <- repopsi[-c(1:8),]

my.names <- repopsi[1,]

colnames(repopsi) <- my.names

repopsi <- repopsi[-1,]

repopsi <- repopsi[,-1]

colnames(repopsi)<-gsub(".$","",colnames(repopsi))



#Creating hyperlinked columns for the table. For that is used repopsi1.This will apply to columns with links and with contact
#email

repopsi1 <-repopsi

repopsi1$`Source of the original instrument` <- ifelse(is.na(repopsi1$`Source of the original instrument`), 
                                                       
                NA, paste0("<a href='", repopsi1$`Source of the original instrument`, "' target='_blank'>",
                           
               repopsi1$`Source of the original instrument`, " </a>"))


repopsi1$`Source of the translation/adaptation` <- ifelse(is.na(repopsi1$`Source of the translation/adaptation`), 
              
                NA, paste0("<a href='", repopsi1$`Source of the translation/adaptation` , "' target='_blank'>",
                            
                repopsi1$`Source of the translation/adaptation`,"</a>"))


repopsi1$`Link to instrument in the Repository` <- ifelse(is.na(repopsi1$`Link to instrument in the Repository` ), 
              
                NA, paste0("<a href='", repopsi1$`Link to instrument in the Repository` , "' target='_blank'>",
                                                                                                            
                repopsi1$`Link to instrument in the Repository`, "</a>"))

repopsi1$`Link to instrument outside of the Repository` <- ifelse(is.na(repopsi1$`Link to instrument outside of the Repository` ),
              
                NA, paste0("<a href='", repopsi1$`Link to instrument outside of the Repository` , "' target='_blank'>",
                                                                                                                                    
                 repopsi1$`Link to instrument outside of the Repository` ,"</a>"))

repopsi1$`Contact email address` <- ifelse(is.na(repopsi1$`Contact email address`  ),
                                                                 
                                  NA, paste0("<a href='mailto:",repopsi1$`Contact email address`  , "' target='_blank'>",
                                                                            
                         repopsi1$`Contact email address`  ,"</a>"))

#Adding ID row in order to allow for filtering of original table based on row ID

repopsi1$ID <- seq.int(nrow(repopsi1))

# Merging various columns to be shown in the table


repopsi1<- repopsi1%>%
  
  unite(`Instrument name and version`, `Title in English`, Abbreviation, Version, sep = " | ",remove = FALSE,na.rm = TRUE)%>%
  
  select(-c(`Title in English`,Abbreviation,Version))%>%
  
  unite(`Contact person`, `Contact person`, `Contact email address`, sep = " | ",remove = FALSE,na.rm = TRUE) %>%
  
  select(-`Contact email address`) %>%
  
  unite(`Where to find the instrument?`, `Instrument availability`, `Link to instrument in the Repository`, `Link to instrument outside of the Repository`, sep = " | ",remove = FALSE,na.rm = TRUE)%>%
  
  select(-c(`Instrument availability`, `Link to instrument in the Repository`, `Link to instrument outside of the Repository`))

#Changing order of columns

repopsi1<- repopsi1%>%
  
  relocate(`Where to find the instrument?`, .after = `Instrument name and version`) %>%
  
  relocate(`Citation of the original instrument`, .after = `Where to find the instrument?`) %>%
  
  relocate(`Citation of the translation/adaptation`, .after = `Citation of the original instrument`) %>%
  
  relocate(Keywords, .after = `Citation of the translation/adaptation`) %>%
  
  relocate(`Contact person`, .after = Keywords)


  #repopsi duplicate for download 

repopsi2 <-repopsi

repopsi2$ID  <-seq.int(nrow(repopsi2))


#URL for CSV download of all data

URL <- "https://osf.io/download/mxrc2/"

#UI part

                             
ui <-  navbarPage( theme = shinytheme("cerulean"),
                   
  #Js necessary for global search
  
                   tags$script(src = "myscript.js"),
                   
                   tags$script(src = "https://cdn.datatables.net/1.13.3/js/jquery.dataTables.min.js"),
                   
  #Change of color of the navbar and other elements
  
  tags$head ( tags$style(HTML(
    "
    .dataTables_filter{
        float: left !important;
        text-align: left !important;
    }
  
.dataTables_length{
float: right !important;
        text-align: right !important;
}

 div.dt-buttons{
position: absolute !important;
    left: 300px;
        height: 100px;
         display: block;
}
    
   .bttn-primary {
    background-image: linear-gradient( #20242c,  #20242c) !important;
   }
   .h3, .h5, h3, h5, h6 {
   color: #20242c !important;
   }
   .navbar-nav {
   display: none !important;
   }
   .navbar-default {
      background-image: linear-gradient( #1d7bd3,  #1d7bd3) !important;
    } "))),
  

    #Title panel
    
    
  title = strong( a("REPOPSI: Repository of Psychological Instruments in Serbian", href="https://www.repopsi.f.bg.ac.rs/sr/",style= "color:white;")),

                  #Arranging side panel
                  
                  sidebarPanel(
                    
                    a(img(src="REPOPSI.svg", height="50%", width="50%",
                          
                          style="display: block; margin-left: auto; margin-right: auto;"),href = "https://www.repopsi.f.bg.ac.rs/sr/"),
                    
                    h5(strong("Enter search term(s):")),
                   
                   tags$input(id="global_filter", class="global_filter form-control shiny-bound-input", type="text"),
                    
                    h5("Use this application to easily browse and access the instruments available in REPOPSI.", align = "justify"), 
                    
                    h5("Enter a search term (e.g. personality) or multiple search terms (e.g. personality self report) in English to get a table of instruments matching those terms. 
                    If using author names, include all variations (e.g. Knežević / Knezevic) in separate searches.", align = "justify"),
                    
                    h5("New to REPOPSI?", a("Visit our website to learn more.",href = "https://www.repopsi.f.bg.ac.rs/sr/")),
         
                  h5("Download filtered instrument records:", align = "justify"),
                  
                  # adding download buttons for all data and for filtered data 
              
                  
                  downloadBttn (outputId = "downloadfilterxlsx", label = "Get filtered data.XLSX", style = "simple", size = "sm",color = "primary"), 
                  
                  h5("Download all REPOPSI instrument records:", align = "justify"),
      
                  downloadBttn ("downloadalldatacsv", "Get all data.CSV", style = "simple", size = "sm",color = "primary"),
                  
                  downloadBttn ("downloadalldataxlsx", "Get all data.XLSX", style = "simple", size = "sm",color = "primary")),
                
                  
                #Arranging main panel
                
  mainPanel( h5("Please note that you may use the instruments found in REPOPSI without permission for non-commercial clinical, 
                
                research, and educational purposes provided you cite instrument authors and translators. 
                
                ",a("Learn more about the CC BY-NC-SA 4.0 licence.", 
                                                                                                  
                 href= "https://creativecommons.org/licenses/by-nc-sa/4.0/ "), align = "justify"),
             
             h5("If you would like for your instrument or an instrument translation/adaptation to be stored in REPOPSI",
                
               a( "you may fill in this online contribution form.",href="https://www.repopsi.f.bg.ac.rs/en/how-to-contribute-to-repopsi/")),
             
       
  DT:: dataTableOutput("tablerepopsi"),
  
  br(),
  
  br(),
  
  
  img(src="EUlogo.jpg", width="70px", style="display: block; margin-right: auto;"), 
  
  h6("This web application was created as part of the project which has received funding from the European Union’s
  
     Horizon 2020 research and innovation programme under grant agreement No 101017536.", align = "justify"),
  
  h6("This web application has received support from the ", a("EOSC Future",hef="https://eoscfuture.eu/"),
  
  " through the ", a( "RDA",href = "https://www.rd-alliance.org/"), 
  
 a(" Open Call mechanism",href="https://eoscfuture-grants.eu/provider/research-data-alliance"),
 
 "based on evaluations of external, independent experts.", align = "justify"),
 
 h6("The project “TRUST-ification and FAIR-ification of an 
 
  Open Repository for Research Instruments in Psychology: REPOPSI’s Adoption of RDA Outputs” is
    
    led by the ",a( "University of Belgrade Faculty of Philosophy",href="https://www.f.bg.ac.rs/en2"), 
    
    " and ", a("LIRA – Laboratory for Research of Individual Differences.",href="https://lira.f.bg.ac.rs/en/"), align = "justify")))


#Server side of the app

server <- function (input, output, session) {
  

 
  #Display of table
  
  output$tablerepopsi <- DT::renderDataTable({
   

   
    DT::datatable( repopsi1, rownames = FALSE, escape = FALSE,  filter = list(position = "top"),  selection="none", 
                   
                   options=list(autoWidth = FALSE, dom='riltp',pageLength = 10, lengthMenu = c(10, 20, 50),
                                                                              
                                  scrollX = TRUE, 
                  
                                columnDefs = list(list(visible=FALSE, targets=c(-1:-15))))) 

  })  
  

  

  #Downloading filtered rows but from original table based on global search
   
  downloadtablereactive <- reactive({
   
    
    downloadtablefilter<-  repopsi2%>%
      
      filter(ID %in%input$tablerepopsi_rows_all)%>%
     select(-ID)
    
  })
 
  #Filtered data excel
  
  output$downloadfilterxlsx <- downloadHandler(
    
    
    
    filename = function(){paste("Filtered data REPOPSI-", Sys.Date(), ".xlsx")},
    
    content = function(file) {
      
      write_xlsx(downloadtablereactive(), path = file)})
  
 

  #Download all data csv
  
  output$downloadalldatacsv <- 
    
   
    downloadHandler(
      filename = function() {
        paste("All data REPOPSI.csv",sep = "")
      },
      content = function(file) {
        GET(URL, write_disk(file))
      },
      contentType = "csv"
    )
  
 #Download all data excel

  output$downloadalldataxlsx <- downloadHandler (
    
    filename = function(){paste("All data REPOPSI.xlsx",sep = "")},
    
    content = function(file) {
      
      write_xlsx(repopsi, file)
    }
  )  
  
  
}



shinyApp(ui,server)




