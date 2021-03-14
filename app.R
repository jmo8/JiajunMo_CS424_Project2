library(shiny)
library(shinydashboard)
library(ggplot2)
library(lubridate)
library(DT)
library(jpeg)
library(grid)
library(leaflet)
library(scales)
library(readxl)

egrid2018 <- read_excel("egrid2018PLNT.xlsx")
egrid2018IL <- subset(egrid2018, PSTATABB=="IL")
nonrenew2018IL <- subset(egrid2018IL, PLGENATN!=0)
renew2018IL <- subset(egrid2018IL, PLGENATR!=0)
mapdata <- subset(egrid2018, PSTATABB=="IL")


egrid2000 <- read_excel("egrid2000PLNT.xlsx")
egrid2000IL <- subset(egrid2000, PSTATABB == "IL")
egrid2010 <- read_excel("egrid2010PLNT.xlsx")
egrid2010IL <- subset(egrid2010, PSTATABB == "IL")

defaultbox <- c("COAL", "OIL", "GAS", "NUCLEAR","HYDRO", "BIOMASS", "WIND", "SOLAR", "GEOTHERMAL", "OTHF")

cmpmap1 <- subset(egrid2010IL, PLFUELCT %in% defaultbox)
cmpmap2 <- egrid2018IL

ui <- navbarPage(
	
	  title = "CS 424 Project2",
		

	  tabPanel(title = "Map", 
		  tags$style(
			  '.navbar { background-color: #a2d2ff;}',
		  ),
		  mainPanel(
			  div(class="outer",
			  tags$style(type = "text/css", ".outer { position: fixed; border-width:5px; border-style:double; width: 1280px; height: 720px;left:300px}"),
			  leafletOutput("map", width = "100%", height = "100%"),
			  
			  ),
			  absolutePanel(top = 0, right = 0,left = 30, bottom = 0,
			    div(class = "boxstyle1",
					tags$style(type = "text/css", " .boxstyle1{text-indent: 10px;border-width:5px; border-style:double;font-weight: bolder;background: #afeeee; width:200px;height: 50px;}"),
					checkboxInput("legend", "Show legend", TRUE)
				)
			  ),
			  absolutePanel(top = 70, right = 0,left = 30, bottom = 0,
				  div(class = "boxstyle2",
						tags$style(type = "text/css", " .boxstyle2{text-indent: 10px;border-width:5px; border-style:double;font-weight: normal;background: #edf2fb; width:200px;}"),
						checkboxInput("all", "All", TRUE),
						checkboxInput("ren","Renewable",TRUE),
						checkboxInput("non","Non-renewable",TRUE),
						#checkboxGroupInput("renewable", "RENEWABLE", c("hydro","biomass","wind","solar","geothermal")),
						#checkboxGroupInput("non-renewable", "NON-RENEWABLE", c("coal","oil","gas","nuclear","other")),
						checkboxInput("coal", "COAL", TRUE),
						checkboxInput("oil", "OIL", TRUE),
						checkboxInput("gas", "GAS", TRUE),
						checkboxInput("nuclear", "NUCLEAR", TRUE),
						checkboxInput("hydro", "HYDRO", TRUE),
						checkboxInput("biomass", "BIOMASS", TRUE),
						checkboxInput("wind", "WIND", TRUE),
						checkboxInput("solar", "SOLAR", TRUE),
						checkboxInput("geothermal", "GEOTHERMAL", TRUE),
						checkboxInput("other", "OTHER", TRUE)
				  )
			  ),
			  absolutePanel(top = 100, right = 0,left = 300, bottom = 0,
						actionButton("reset", "Reset Map")
			  )
		   )
	  ),

	  tabPanel(title = "Compare", 
		mainPanel(
			 
			div(class="cmpm1",
			  tags$style(type = "text/css", ".cmpm1 { position: fixed; border-width:5px; border-style:double; width: 852px; height: 480px;left:0px}"),
			  textOutput("cmptext1"),
			  leafletOutput("cmp1", width = "100%", height = "95%"),
			 ),
			div(class="cmpm2",
			  tags$style(type = "text/css", ".cmpm2 { position: fixed; border-width:5px; border-style:double; width: 852px; height: 480px;left:1000px}"),
			  textOutput("cmptext2"),
			  leafletOutput("cmp2", width = "100%", height = "95%"),
		     )
		)






	  ),

	  tabPanel(title = "About Page",
		
		mainPanel(
			div(
				class="textstyle1",
				tags$style(type = "text/css", ".textstyle1 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text1")
			),
			div(
				class="textstyle2",
				tags$style(type = "text/css", ".textstyle2 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text2")
			),
			div(
				class="textstyle3",
				tags$style(type = "text/css", ".textstyle3 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 100px;}"),
				textOutput("text3")
			),
			div(
				class="textstyle4",
				tags$style(type = "text/css", ".textstyle4 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text4")
			),
			div(
				class="textstyle5",
				tags$style(type = "text/css", ".textstyle5 {
					position: relative;
					margin: 10px;
					border-width:5px;  
					border-style:double;
					background: #afeeee;
					background-clip: content-box;
					font-weight: bold;
					font-size: large;
					width: 1080px;
					height: 50px;}"),
				textOutput("text5")
			)
		)
	  )
	
 )



 server <- function(input,output,session){
	
	observeEvent(input$reset, {
		updateCheckboxInput(session,"all", value = TRUE)
		updateCheckboxInput(session,"ren",value = TRUE)
		updateCheckboxInput(session,"non",value = TRUE)
		updateCheckboxInput(session,"hydro",value = TRUE)
		updateCheckboxInput(session,"biomass",value = TRUE)
		updateCheckboxInput(session,"wind",value = TRUE)
		updateCheckboxInput(session,"solar",value = TRUE)
		updateCheckboxInput(session,"geothermal",value = TRUE)
		updateCheckboxInput(session,"coal",value = TRUE)
		updateCheckboxInput(session,"oil",value = TRUE)
		updateCheckboxInput(session,"gas",value = TRUE)
		updateCheckboxInput(session,"nuclear",value = TRUE)
		updateCheckboxInput(session,"other",value = TRUE)

	})

	
	observe({
		if(input$ren && input$non){
			updateCheckboxInput(session,"all",value = TRUE)
		}
	})

    observe({
			if(input$all){
				updateCheckboxInput(session,"ren",value = TRUE)
				updateCheckboxInput(session,"non",value = TRUE)
			}
    })
	
	observe({
		if(input$ren){
			updateCheckboxInput(session,"hydro",value = TRUE)
			updateCheckboxInput(session,"biomass",value = TRUE)
			updateCheckboxInput(session,"wind",value = TRUE)
			updateCheckboxInput(session,"solar",value = TRUE)
			updateCheckboxInput(session,"geothermal",value = TRUE)

		}
		else{
			updateCheckboxInput(session,"all",value = FALSE)
			updateCheckboxInput(session,"hydro",value = FALSE)
			updateCheckboxInput(session,"biomass",value = FALSE)
			updateCheckboxInput(session,"wind",value = FALSE)
			updateCheckboxInput(session,"solar",value = FALSE)
			updateCheckboxInput(session,"geothermal",value = FALSE)
		}
	})

	observe({
		if(input$non){
			updateCheckboxInput(session,"coal",value = TRUE)
			updateCheckboxInput(session,"oil",value = TRUE)
			updateCheckboxInput(session,"gas",value = TRUE)
			updateCheckboxInput(session,"nuclear",value = TRUE)
			updateCheckboxInput(session,"other",value = TRUE)
		}
		else{
			updateCheckboxInput(session,"all",value = FALSE)
			updateCheckboxInput(session,"coal",value = FALSE)
			updateCheckboxInput(session,"oil",value = FALSE)
			updateCheckboxInput(session,"gas",value = FALSE)
			updateCheckboxInput(session,"nuclear",value = FALSE)
			updateCheckboxInput(session,"other",value = FALSE)
		}
	})

	observe({
		check <- c(0,0,0,0,0,0,0,0,0,0)
		totalcolor <- c('red', 'black', '#F72585', '#386641', '#7209B7','#f95738','#fee440','#00bbf9','#00f5d4','#6f1d1b')
		if(input$coal){
			check[1] <- "COAL"
		}
		if(input$oil){
			check[2] <- "OIL"
		}
		if(input$gas){
			check[3] <- "GAS"
		}
		if(input$nuclear){
			check[4] <- "NUCLEAR"
		}
		if(input$hydro){
			check[5] <- "HYDRO"
		}
		if(input$biomass){
			check[6] <- "BIOMASS"
		}
		if(input$wind){
			check[7] <- "WIND"
		}
		if(input$solar){
			check[8] <- "SOLAR"
		}
		if(input$geothermal){
			check[9] <- "GEOTHERMAL"
		}
		if(input$other){
			check[10] <- "OTHF"
		}
		
		check <- subset(check, check!=0)
		

		if(!is.numeric(check)){
			mapdata <- subset(egrid2018IL, PLFUELCT %in% check)
			totalcolor <- totalcolor[1:length(check)]
			color2018IL <- colorFactor(palette = totalcolor, domain = mapdata$PLFUELCT )
			if (input$legend) {
			output$map <- renderLeaflet({
				leaflet() %>% addTiles() %>% addCircleMarkers(weight=1,lng=mapdata$LON, lat=mapdata$LAT, popup=mapdata$PNAME,color=color2018IL(mapdata$PLFUELCT),fillOpacity = 0.7,) %>%
				addLegend(
						position = "bottomleft",
						colors = totalcolor,
						labels = check,
						opacity = 1,
						title = "Type"
				)
			})
			}
			else{
				output$map <- renderLeaflet({
					leaflet() %>% addTiles() %>% addCircleMarkers(weight=1,lng=mapdata$LON, lat=mapdata$LAT, popup=mapdata$PNAME,color=color2018IL(mapdata$PLFUELCT),fillOpacity = 0.7,) 
				})
			}
		}
		else if(is.numeric(check)){
			output$map <- renderLeaflet({
					leaflet() %>% addTiles() %>% clearMarkers
			})
		}
	})

	colorcmp1 <- colorFactor(palette = c('red', 'black', '#F72585', '#386641', '#7209B7','#f95738','#fee440','#00bbf9','#00f5d4','#6f1d1b'), domain = cmpmap1$PLFUELCT )

	output$cmp1 <- renderLeaflet({
		leaflet() %>% addTiles() %>% addCircleMarkers(weight=1,lng=cmpmap1$LON, lat=cmpmap1$LAT, popup=cmpmap1$PNAME,color=colorcmp1(cmpmap1$PLFUELCT),fillOpacity = 0.7) %>%
		addLegend(
				position = "bottomleft",
				colors = c('red', 'black', '#F72585', '#386641', '#7209B7','#f95738','#fee440','#00bbf9','#00f5d4','#6f1d1b'),
				labels = c("COAL", "OIL", "GAS", "NUCLEAR","HYDRO", "BIOMASS", "WIND", "SOLAR", "GEOTHERMAL", "OTHF"),
				opacity = 1,
				title = "Type"
		)
	})

	colorcmp2 <- colorFactor(palette = c('red', 'black', '#F72585', '#386641', '#7209B7','#f95738','#fee440','#00bbf9','#00f5d4','#6f1d1b'), domain = cmpmap2$PLFUELCT )

	output$cmp2 <- renderLeaflet({
		leaflet() %>% addTiles() %>% addCircleMarkers(weight=1,lng=mapdata$LON, lat=cmpmap2$LAT, popup=cmpmap2$PNAME,color=colorcmp2(cmpmap2$PLFUELCT),fillOpacity = 0.7) %>%
		addLegend(
				position = "bottomleft",
				colors = c('red', 'black', '#F72585', '#386641', '#7209B7','#f95738','#fee440','#00bbf9','#00f5d4','#6f1d1b'),
				labels = c("COAL", "OIL", "GAS", "NUCLEAR","HYDRO", "BIOMASS", "WIND", "SOLAR", "GEOTHERMAL", "OTHF"),
				opacity = 1,
				title = "Type"
		)
	})

	output$text1 <- renderText({
		"Data source: https://www.epa.gov/egrid/download-data" 
	})

	output$text2 <- renderText({
		"File Name:eGRID2018v2 Data File (XLSX) & eGRID2000_plant.xls & eGRID2010_Data.xls" 
	})

	output$text3 <- renderText({
		"Changed: Take out the PLNT page from eGRID2018v2 Data File (XLSX) & eGRID2010_Data.xls;copy to a new xlsx file and delete the first/second row to get simplified column name. Take out the EGRDPLNT00 page from eGRID2000_plant.xls; copy to a new xlsx file and delete the description(the black bold text) in the first row to get simplified column name. " 
	})

	output$text4 <- renderText({
		"Author: Jiajun Mo " 
	})

	output$text5 <- renderText({
		"Date: 2021/3/12" 
	})

	output$cmptext1 <- renderText({
		"Map 2010" 
	})
	output$cmptext2 <- renderText({
		"Map 2018" 
	})

	
	

 
 }

 shinyApp(ui, server)

