# install.packages("shinythemes")
library(shinythemes)
library(shiny)
library(DT)
library("shinycssloaders")
options(spinner.color="#CD1076", spinner.type = 1, shiny.maxRequestSize=30*1024^2)
ui <- fluidPage(theme = shinytheme("cyborg"),
tags$head(tags$style(HTML("a {color: #CD1076; font-weight: bold}"))),
    navbarPage(windowTitle=("GeoStoryTelling [GST]"), title=p("GeoStoryTelling [GST]", style="font-size:20px; font-weight:bold; color: #CD1076; background-color: white;
	padding-left: 7.5px; 
    padding-right: 7.5px; 
	padding-top: 5px; 
    padding-bottom: 5px; 
	line-height: 1.1;
	border-radius: 25px"),#; color:orangered
				tabPanel("I. Load Data and Select Features",
				 sidebarPanel(
				 selectInput("select", h4("A. Load your data or the example", style ="font-weight:bold"), 
                       choices = list("Your data will be displayed after loading is completed"=2, "Minimal example with six cases" = 1), selected = 2),
									  
tags$style(".btn-file {background-color:#CD1076; border-color: #2e6da4; } .btn:hover {
color: #ffffff;
background-color: #CD1076;
border-color: #2e6da4;
}
.progress-bar {background-color: #CD1076; }",
"
                    .dataTables_wrapper .dataTables_length, .dataTables_wrapper .dataTables_filter, .dataTables_wrapper .dataTables_info, .dataTables_wrapper .dataTables_processing, .dataTables_wrapper .dataTables_paginate {
                    color: #ffffff;
                    }

                    thead {
                    color: #ffffff;
                    }

                    select {
                    color: #CD1076;
                    }
					
                    input {
                    color: #CD1076;
                    }

                     tbody {
                    color: #ffffff;
                    }
					
					body {
  color: lightgrey;
  font-family: Arial, sans-serif !important;
}


                   "),
		   
	
tags$style(HTML("b, strong{color:#CD1076;
} a:hover{color:#2e6da4;}
		")),
												h4("C. Select column names", style ="font-weight:bold"), #; color:black
												textInput('map_title', 'Map Title, "<br>" without quotes adds a second line', value = "Type a short title or delete this text"),
												selectInput('ego', 'Name of Place or Entity', ""),
												selectInput('alter', 'Address or Location', ""),
												selectInput('role', 'Hyperlink to further information', ""),
												selectInput('woman', 'Image (local file supported), music, or video hyperlinks', ""),
												selectInput('shape', 'Longitude', ""),
												selectInput('shapel', 'Latitude', ""),
												selectInput('storytelling', 'Geo-Story to Share', ""),
												# selectInput('quotes', 'Column for *code* content or information description', ""),
							tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: #CD1076}")),
							tags$style(HTML(".js-irs-1 .irs-single, .js-irs-1 .irs-bar-edge, .js-irs-1 .irs-bar {background: #CD1076}")),
												# sliderInput("obs", "No. of groups for role variable (default 25):", min = 1, max = 50, value = 25),
												# sliderInput("trans", "Color transparency (1 is solid):", min = 0, max = 1, value = .8),
												h5("If ready, go to step II above to execute GST", style ="font-size:20px; font-weight:bold; color: #CD1076; background-color: white;
	padding-left: 7.5px; 
    padding-right: 7.5px; 
	padding-top: 5px; 
    padding-bottom: 5px; 
	line-height: 1.1;
	border-radius: 25px")
												),
												mainPanel(
												h3("Please follow the column order of example", style ="font-weight:bold"),
												h4("B. Upload your data here or load example(s) as indicated in step A", style ="font-weight:bold"),#; color:black"
                                                
												uiOutput("toy_data"),#PHUDCFILY
							h4("MINIMAL information required:", style ="font-weight:bold"), #; color:black
    htmlOutput("mintext"),
	h4("OPTIONAL information based on actors' or units' locations, images, or hyperlinks. This information is used to strengthen your storytelling", style ="font-weight:bold"),
	htmlOutput("opttext"),
	DT::dataTableOutput("table")#, DT::dataTableOutput("tables") Changed it to only there
												),
                                              ),
											  
	tabPanel(p(icon("project-diagram"), "   II. Execute GST   ", style ="font-weight:bold; background-color: #CD1076; font-size: 20px; 
	padding-left: 5px; 
    padding-right: 5px; 
	padding-top: 3px; 
    padding-bottom: 3px; 
	line-height: 1.1;
	border-radius: 25px;"), 
            
			 htmlOutput("opttextphu", style = "font-size: 20px"), withSpinner(uiOutput("mymap"))),
		tabPanel(p("III. About GeoStoryTelling", style ="font-weight:bold"), # ; color:black p("active tab is in gold" , style ="font-weight:bold")
		tags$h4("Program Description"),
				tags$h5(HTML("Despite the widespread availability of artificial intelligence <b>(AI)</b>, machine learning <b>(ML)</b>, and data science and visualization <b>(DSV)</b> tools, up until now <b>access</b> to the benefits of these analytic tools has been <b>conditioned</b> to computer programming and software coding proficiency. Indirectly then, these benefits are currently <b>overconcentrated in a single demographic</b>. <br><br>
				
		<b>Democratizing Data Science (DDS)</b>, a new data analytics movement, frees these benefits by lifting computer programing restrictions and offering <b>open software</b> access to rigorous, state-of-the-art analytic tools for mixed method research. <br><br>
		
		<b>DDS</b> then, seeks to <b>expand access</b> to the <b>analytic and knowledge-based</b> benefits of <b>AI, ML,</b> and <b>DSV</b> with <b>zero</b> coding requirements.<br> <br>
		
		<b>GeoStoryTelling</b> constitutes one of the first products released as part of the mission of <b>DDS</b>.")),###PHUDCFILY <br><br>
		
		tags$hr(),
		# # # PHUDCFILY
		tags$h4("Questions and ideas for software applications?"),
		
		tags$h5("Reach me (Manuel S. Gonzalez Canche) at ",a("msgc@upenn.edu", href="mailto:msgc@upenn.edu"), "and", a("@manu_canche, Twitter", href="https://twitter.com/manu_canche", target="_blank")),
		tags$h3("More info at", a("Democratizing Data Science, GitHub", href="https://github.com/democratizing-data-science", target="_blank"), "and quick updates", a("@DataScienceOpen, Twitter", href="https://twitter.com/DataScienceOpen", target="_blank")),
		
		img(src = "DDS_white_phudcfily.png", style = "float: left; width: 400px; margin-right: 5px; margin-top: 1px")
                        )
)
)

server = function(input, output, session){
library(DT)#PHUDCFILY
library(gtools)
ex1<- read.csv(paste(getwd(), "/www/cultural_map_PHUDCFILY.csv", sep=""))
ex1$Video.Image.Link.or.Location <- ifelse(ex1$Video.Image.Link.or.Location == "D:\\Dropbox (Penn GSE)\\virtual\\PC Applications PHUDCFILY\\Geo-StoryTelling_PHUDCFILY\\www\\berlinphudcfily.jpg", paste(getwd(), "\\www\\berlinphudcfily.jpg",sep=""), paste0(ex1$Video.Image.Link.or.Location))
print(ex1)


output$toy_data <- renderUI({
if (input$select==1) {
               actionButton(inputId = "first", label = "Click to load GeoStory data example shown below", icon("paper-plane"), 
    style="color: #fff; background-color: #CD1076; border-color: #2e6da4")#PHUDCFILY
} else {
fileInput("file1", label=NULL,
													buttonLabel = "Click to browse for your CSV file or simply drag it here with your mouse",
									                placeholder = NULL,
                                                            multiple = FALSE,
                                                            accept = c("text/csv",
                                                                       "text/comma-separated-values,text/plain",
                                                                       ".csv"))
} 
  })

a1 <- reactiveValues(a = NULL)
observeEvent(input$file1, {

     a1$a <- read.csv(input$file1$datapath)#PHUDCFILY
	 print(a1$a)
    })
observeEvent(input$first, {
     a1$a <- ex1[,1:ncol(ex1)]
	     })	
#PHUDCFILY
output$mintext <- renderText({
if (input$select==1) {
	 print(a1$a)
               paste("Columns", "<b>'lon'</b>", "and", "<b>'lat'</b>", "represent the locations of interest")
			   
			   
               
} else { #MODIFY PHUDCFILY
print(a1$a)
               paste("Your data should have <b>at least</b> two columns", "<b>longitude</b>", "and", "<b>latitude</b>", "<br>These coordinates will be used to identify points of interest")

}
})

output$opttext <- renderText({
if (input$select==1) {
print(a1$a)
               paste("In *this example* <b>'lon'</b>", "and <b>'lat'</b> are the locations,<br> 
			   <b>'Name.or.Cultural.Center'</b> Is the name to be plotted as a label.<br>
			   <b>'Location'</b>", "May be the address or point of interest.<br>
			   <b>'Link.to.Webpage'</b>", "Is a link to a website that can be accessed from the map.<br>
			   <b>'Video.Image.Link.or.location'</b>", "Link to local image file or a link to a", a('YouTube', href='https://www.youtube.com'),"video. We recommend a <b>width frame or 300 and height of 169</b>.<br>
			   <b>'Map title'</b> allows typing a descriptor for the map,<br><br>")
               
} else { #MODIFY PHUDCFILY
print(a1$a)
               paste(
			   "To use GST you need to upload a database with <b>'latitudes'</b> and <b>'longitudes'</b> as depicted in the example provided.<br>
			   <b>Section C</b> may be populated with <b>'Name'</b>", "that indicates participants' or units' names and, <b>hovered</b> will automatically display this info as a <b>label</b>.<br>
			   <b>'Location'</b>", "May be the address or point of interest.<br>
			   <b>'Link.to.Webpage'</b>", "Is a link to a website that can be accessed from the map.<br>
			   <b>'Video.Image.Link.or.location'</b>", "Link to local image file or a link to a", a('YouTube', href='https://www.youtube.com'),"video. We recommend a <b>width frame or 300 and height of 169</b>.<br>
			   <b>'Map title'</b> allows typing a descriptor for the map,<br><br>")
}
})

output$table <- DT::renderDataTable({
if (input$select==1) {
datatable(ex1, options = list(
    pageLength = 6))
} else {
datatable(a1$a, options = list(
    pageLength = 6))
}
})
#PHUDCFILY					  

outVar0 = reactive({
      mydata = a1$a
      c("Column with name of center/unit",names(mydata))
    })
    observe({
      updateSelectInput(session, "ego",
      choices = outVar0()
    )})
	
outVar1 = reactive({
      mydata = a1$a
      c("Column with physical address",names(mydata))
    })	
    observe({
      updateSelectInput(session, "alter",
      choices = outVar1()
    )})

outVar2 = reactive({
      mydata = a1$a
      c("Column with Web address",names(mydata))
    })
	
    observe({
      updateSelectInput(session, "role",
      choices = outVar2()
    )})
	
outVar3 = reactive({
      mydata = a1$a
      c("Column with link to image", names(mydata))
    })
	
    observe({
      updateSelectInput(session, "woman",
      choices = outVar3()
    )})
	
outVar4 = reactive({
      mydata = a1$a
      c("Column with longitude", names(mydata))
    })
	
    observe({
      updateSelectInput(session, "shape",
      choices = outVar4()
    )})

outVar5 = reactive({
      mydata = a1$a
      c("Column with latitude", names(mydata))
    })
	
    observe({
      updateSelectInput(session, "shapel",
      choices = outVar5()
    )})	

outVar6 = reactive({
      mydata = a1$a
      c("Column with geostory", names(mydata))
    })
	
    observe({
      updateSelectInput(session, "storytelling",
      choices = outVar6()
    )})	
	


print(getwd())
	library(leaflet)
	library(htmlwidgets)
	library(htmltools)
    output$mymap <- renderUI({
	if (is.data.frame(a1$a)==TRUE) {
namei<-"information"
a <- a1$a
		colnames(a)[colnames(a) == input$ego] <- "ego"
		colnames(a)[colnames(a) == input$alter] <- "alter"
		colnames(a)[colnames(a) == input$role] <- "role"
		colnames(a)[colnames(a) == input$woman] <- "woman"
		colnames(a)[colnames(a) == input$shape] <- "shape"
		colnames(a)[colnames(a) == input$shapel] <- "shapel"
		colnames(a)[colnames(a) == input$storytelling] <- "storytelling"

a$shape<-as.numeric(as.character(a$shape))
a$shapel<-as.numeric(as.character(a$shapel))


print(str(a))

 if ("woman" %in% colnames(a)==TRUE) { 
a$videophudcfily <- substr(a$woman, 1, 5)#ifelse("woman" %in% colnames(a)==TRUE, substr(a$woman, 1, 5), NULL)
a$woman <- ifelse ((a$videophudcfily == "<ifra" | a$videophudcfily == "https"), paste0(a$woman), paste("file://", gsub("\\\\", "/", a$woman), sep=""))
 } else {
 a$videophudcfily <- "not a media link"
 }

print(a$videophudcfily)
conteudo <- paste(sep = "<br/>",
               ifelse(a$videophudcfily=="<ifra", paste0(a$woman), paste0("<img src='", a$woman, "' width = 300>")), #paste0("'",a$woman,"'")
               paste0("<b>Name: </b>", a$ego),
               paste0("<b>Address/Location: </b>", a$alter),
               paste0("<a href='", a$role,"'>Hyperlink for more ", namei, " </a>"),
               paste0("<b>GeoStory: </b>", a$storytelling))

 if ("ego" %in% colnames(a)==TRUE) { 
a$ego <- paste0(a$ego)
 } else {
a$ego <- paste("ID", 1:nrow(a))
 }


print(conteudo)


tag.map.mtitle <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 70%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
	padding-top: 10px; 
    padding-bottom: 10px; 
    background: rgba(205,16,118,0.9);
    font-weight: normal;
    font-size: 16px;
	line-height: 1.1;
	border-radius: 25px;
	color: white;
  }
"))

tfphu <- ifelse(input$map_title=="", "", paste("<b>GST Title: </b>", input$map_title, sep=""))
	if (tfphu!="") { 
mtitle <- tags$div(
  tag.map.mtitle, HTML(tfphu)#HTML(paste("<b>Title: </b>", input$map_title, sep=""))
) 
} else {
mtitle <- NA
}

###PHUDCFILY
library(leaflet.extras) # extending the leaflet.js #not intalled yet? PHUDCFILY
library(Polychrome)#not intalled yet? P
Glasbey = glasbey.colors(length(unique(a$ego))+1)[-1]
pal <- colorFactor(
  palette = Glasbey,
  domain = a$ego
)
map <- leaflet() %>%
		addProviderTiles("CartoDB") %>% ###Stamen.TonerLite PHUDCFILY
setView(mean(a$shape), mean(a$shapel), zoom=11) %>%
addCircleMarkers(data = a, lng = ~shape, lat = ~shapel, popup=conteudo, label = ~ego, radius = 10, # size of the dots
                   fillOpacity = .7, # alpha of the dots
                   stroke = FALSE, color = pal(a$ego),
				   clusterOptions = markerClusterOptions())%>% 
addControl(mtitle, position = "topright", className="map-title")%>% 
  leaflet::addLegend(position = "bottomright",
            values = paste(a$ego, rownames(a)), # data frame column for legend
            opacity = .7, # alpha of the legend
            pal = pal, # palette declared earlier
            title = "Legend Key") %>%  # legend title
  leaflet.extras::addResetMapButton()

print(map)
saveWidget(widget = map, file = paste(getwd(), "/www/index.html", sep=""), selfcontained = FALSE, libdir = "lib")
utils::browseURL(paste(getwd(), "/www/index.html", sep=""))

}else{
}
     })#closes output$mymap PHUDCFILY

output$opttextphu <- renderText({
if (is.data.frame(a1$a)==TRUE) {
              paste("<br><b>GeoStorytelling</b> ", "was programmed to produce an interactive map to be launched automatically.<br><br>
			 If the HTML file does not launch, you can copy and paste this link in your favorite browser:<br><br><b>",
       print(paste(getwd(),"/www/index.html",sep="")),
#             a('Click to launch the info map',target='_blank',href='index.html'),
             "</b><br><br> In addition you can locate the <b>'index.html'</b> and the folder called <b>'lib'</b> in the following directory: <br><br><b>", print(paste(getwd(),"/www",sep="")),
			 "</b><br><br> You will need both the 'index.html' file and the folder 'lib' to distribute your info map<br><br> 
			 If an error message that reads <b>'replacement has 0 rows, data has ", nrow(a1$a), "'</b> appears below, it means you have not defined latitude and longitude columns, if so ignore the 'index.html' file and define lat and lon columns. Otherwise, your map should launch automatically", sep="")
			 # paste()
} else { #MODIFY PHUDCFILY
               paste(
			   "<b>To start, upload a dataset</b> in *.csv format with at least <b>latitude and longitude</b> columns.</b><br><br>
			   After uploading the dataset select the columns that correspond to attributes you want to plot<br><br>
			   <b>Considerations:</b> <br>
			   <b>1.</b> You cannot select the same column twice<br>
			   <b>2.</b> Currently only YouTube videos are enabled<br>
			   <b>3.</b> If the videos have copyright music, they won't be played in your map, <br>
			   <b>4.</b> Your images may be selected from the internet by clicking on an image and <b>copy location</b>, <br>
			   <b>5.</b> You can select local images from your computer.")

}
})

 } #closes server PHUDCFILY

shinyApp(ui, server)
