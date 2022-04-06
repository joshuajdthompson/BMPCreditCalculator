################################################################################
#                   Author: Joshua Thompson
#   O__  ----       Email:  pwthom19@aacounty.org
#  c/ /'_ ---
# (*) \(*) --
# ======================== Script  Information =================================
# PURPOSE: Shiny App for NGOs
#
# PROJECT INFORMATION:
#   Name: Shiny App for NGOs
#
# HISTORY:----
#   Date		        Remarks
#	-----------	   ---------------------------------------------------------------
#	 08/26/2021    Created script                                   JThompson (JT)
#  09/24/2021    Finished script after a hiatus                   JT
#  02/22/2022    Added CSS loader, simplified geom,               JT  
#                and used R data for loading   
#===============================  Environment Setup  ===========================
#==========================================================================================

library(shiny)
library(inlmisc)
library(stringr)
library(wesanderson)
library(sf)
library(leaflet)
library(shinyjs)
library(shinyBS)
library(shinycssloaders)
library(htmlwidgets)

ui = fluidPage(
  tags$head(HTML("<title>BWPR Credit Calculator Tool</title> <link rel='icon' type='image/gif/png' href='logo.png'>")),
  titlePanel(fluidRow(
    column(9,p("BWPR's credit calculator was developed to assist NGOs and others to estimate potential impervious restoration credit and TMDL reductions from their project.  Please select the relevant tab to enter data and estimate the credit and reductions.
    ",style="font-size:18px;font-style:normal; font-weight: 400")),column(3, tags$a(img(src='bwpr_logo_aarivers.jpg', align = "right",height = 279*0.4, width = 558*0.4, style="padding: 0px"),href="https://www.aarivers.org")),
    br(),
    column(12,p("Note: this tool is provided 'as is' without warranty of any kind, either expressed, implied, or statutory. The user assumes the entire risk as to quality and performance of the data from this tool.    
                 ",style="font-size:11.5px;font-style:italic")),br(), 
    
    column(3,a(actionButton(inputId = "email1", label = "  Contact",icon = icon("envelope", lib = "font-awesome")),href="mailto:pwthom19@aacounty.org"))
  )),
  br(),
  tabsetPanel(id = "tabs",
              
              #===================================================
              # Structural BMPs
              #===================================================
              
              tabPanel("Structural BMP", value = "1", fluid = TRUE,icon=icon("tint"), 
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.sbmp",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Bioretention (FBIO)"  
                                                                          ,"Bio-Swale (MSWB)"
                                                                          ,"Dry Swale (ODSW)"
                                                                          ,"Dry Well (MIDW)"
                                                                          ,"ED Shallow Wetland (WEDW)"
                                                                          ,"Enhanced Filter (MENF)"
                                                                          ,"Grass Swale (MSWG)"
                                                                          ,"Infiltration Basin (IBAS)"
                                                                          ,"Infiltration Berm (MIBR)"
                                                                          ,"Infiltration Trench (ITRN)"
                                                                          ,"Landscape Infiltration (MILS)"
                                                                          ,"Micro-Bioretention (MMBR)"
                                                                          ,"Micro-Pool Extended Detention (PMED)" 
                                                                          ,"Multiple Pond (PMPS)" 
                                                                          ,"Organic Filter (FORG)" 
                                                                          ,"Perimeter Filter (FPER)" 
                                                                          ,"Pocket Filter (FPKT)" 
                                                                          ,"Pocket Pond (PPKT)" 
                                                                          ,"Pocket Wetland (WPKT)" 
                                                                          ,"Pond/Wetland System (WPWS)" 
                                                                          ,"Rain Garden (MRNG)" 
                                                                          ,"Rainwater Harvesting (MRWH)" 
                                                                          ,"Shallow Wetland (WSHW)" 
                                                                          ,"Submerged Gravel Wetland (MSGW)" 
                                                                          ,"Surface Sand Filter (FSND)" 
                                                                          ,"Underground Filter (FUND)" 
                                                                          ,"Wet Extended Detention Pond (PWED)" 
                                                                          ,"Wet Pond (PWET)" 
                                                                          ,"Wet Swale 1 (OWSW)" 
                                                                          ,"Wet Swale 2 (MSWW)"),
                                                           selected = NULL, 
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )),
                                            bsPopover(id = "q1sbmp", title = "Select BMP Type",
                                                      content = paste0("Please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            fluidRow(column(6,numericInput("da.sbmp",
                                                                           label = h5(strong("Drainage area (ac):"),
                                                                                      tags$style(type = "text/css", "#q2sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                                      bsButton("q2sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                           value = NULL, width = '100%'
                                            ),
                                            bsPopover(id = "q2sbmp", title = "Enter BMP drainage area",
                                                      content = paste0("If no access to GIS please use  ", 
                                                                       a("the StreamStats app.", 
                                                                         href = "https://streamstats.usgs.gov/ss",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            )),
                                            column(6,numericInput("ia.sbmp",
                                                                  label = h5(strong("Impervious area (ac):"),
                                                                             tags$style(type = "text/css", "#q3sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                             bsButton("q3sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                  value = NULL, width = '100%'
                                            ),
                                            bsPopover(id = "q3sbmp", title = "Enter impervious area in the BMP drainage area",
                                                      content = paste0("If no access to GIS please use  ", 
                                                                       a("the StreamStats app.", 
                                                                         href = "https://streamstats.usgs.gov/ss",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ))),br(),
                                            fluidRow(column(6,numericInput("wqv.sbmp",
                                                                           label = h5(strong("WQv (ac/ft):"),
                                                                                      tags$style(type = "text/css", "#q7sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                                      bsButton("q7sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                           value = NULL, width = '100%'
                                            ),
                                            bsPopover(id = "q7sbmp", title = "Water Quality Volume",
                                                      content = paste0("Please refer to the ", 
                                                                       a("Maryland Stormwater Design Manual for Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/water/stormwatermanagementprogram/pages/stormwater_design.aspx",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            )),
                                            column(6,selectizeInput("c3.sbmp",
                                                                    label = h5(strong("C3 practice eligible for GSI?"),
                                                                               tags$style(type = "text/css", "#q5sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                               bsButton("q5sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                    choices = list("Yes"
                                                                                   ,"No"),
                                                                    selected = NULL, 
                                                                    options = list(
                                                                      placeholder = 'Please select an option below',
                                                                      onInitialize = I('function() { this.setValue(""); }')
                                                                    )),
                                                   bsPopover(id = "q5sbmp", title = "Is this a chapter 3 BMP that provides enhanced features?",
                                                             content = paste0("If unknown select no, otherwise please refer to ", 
                                                                              a("MDE 2020 Guidance.", 
                                                                                href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                                target="_blank")
                                                             ),
                                                             placement = "right", 
                                                             trigger = "focus", 
                                                             options = list(container = "body")
                                                   ))),
                                            
                                            selectizeInput("lrs.sbmp",
                                                           label = h5(strong("Land river segment:"),
                                                                      tags$style(type = "text/css", "#q6sbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q6sbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("N24003WL0_4390_0000"
                                                                          ,"N24003WL0_4391_0000"
                                                                          ,"N24003WL0_4392_0000"
                                                                          ,"N24003WL0_4393_0000"
                                                                          ,"N24003WL0_4394_0000"
                                                                          ,"N24003WL0_4420_0000"
                                                                          ,"N24003WL0_4421_0000"
                                                                          ,"N24003WL0_4422_0000"
                                                                          ,"N24003WL0_4423_0000"
                                                                          ,"N24003WL0_4424_0000"
                                                                          ,"N24003WL0_4425_0000"
                                                                          ,"N24003WL0_4600_0000"
                                                                          ,"N24003WL0_4601_0000"
                                                                          ,"N24003WL0_4602_0000"
                                                                          ,"N24003WL0_4603_0000"
                                                                          ,"N24003WL0_4770_0000"
                                                                          ,"N24003WL0_4771_0000"
                                                                          ,"N24003WL0_4772_0000"
                                                                          ,"N24003WM0_3961_0000"
                                                                          ,"N24003WM0_3962_0000"
                                                                          ,"N24003WM0_3963_0000"
                                                                          ,"N24003WM0_3966_0000"
                                                                          ,"N24003WM3_4060_0001"
                                                                          ,"N24003XL3_4710_0000"
                                                                          ,"N24003XL3_4711_0000"
                                                                          ,"N24003XL3_4712_0000"
                                                                          ,"N24003XL3_4713_0000"
                                                                          ,"N24003XL3_4950_0000"
                                                                          ,"N24003XU2_4270_4650"
                                                                          ,"N24003XU2_4480_4650"
                                                                          ,"N24003XU3_4650_0001"),
                                                           selected = NULL,
                                                           options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )
                                            )
                         ) #wellPanel
                         ), #column
                         
                         bsPopover(id = "q6sbmp", title = "Select Land River Segment",
                                   content = paste0("If unknown, refer to Land River Segment Map and search for the area where your BMP is located using Latitude and Longitude search tool."
                                   ),
                                   placement = "right", 
                                   trigger = "focus", 
                                   options = list(container = "body")
                         ),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.sbmp"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.sbmp"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.sbmp"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.sbmp")
                                            
                         ))
                       )
              ),
              
              #===================================================
              # Stream Restorations
              #===================================================
              
              tabPanel("Stream Restoration", value = "2", icon=icon("water"),
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("protocol.stre",
                                                           label = h5(strong("Use stream protocols?"),
                                                                      tags$style(type = "text/css", "#q1stre {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1stre", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Yes"  
                                                                          ,"No"),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )),
                                            bsPopover(id = "q1stre", title = "Stream Protocol Reductions",
                                                      content = paste0("If no, enter length only. Credit will use the planning rate and will be based on length. If yes, enter protocol data but do not apply delivery factors to these numbers. Further information on stream restoration protocols can be found by ", 
                                                                       a("reviewing the expert panel guidance.", 
                                                                         href = "https://chesapeakestormwater.net/bmp-resources/urban-stream-restoration/",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            fluidRow(column(6,numericInput("length.stre",
                                                                           label = h5(strong("Length of restoration (ft):")),
                                                                           value = NULL, width = '100%'
                                            ),
                                            numericInput("p1tn.stre",
                                                         label = h5(strong("Protocol 1 TN reduction (lbs):")), 
                                                         value = NULL, width = '100%'
                                            ),
                                            numericInput("p1tp.stre",
                                                         label = h5(strong("Protocol 1 TP reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ),
                                            numericInput("p1tss.stre",
                                                         label = h5(strong("Protocol 1 TSS reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            )),
                                            column(6,numericInput("p2tn.stre",
                                                                  label = h5(strong("Protocol 2 TN reduction (lbs):")),
                                                                  value = NULL, width = '100%'
                                            ),
                                            numericInput("p3tn.stre",
                                                         label = h5(strong("Protocol 3 TN reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ),
                                            numericInput("p3tp.stre",
                                                         label = h5(strong("Protocol 3 TN reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ),
                                            numericInput("p3tss.stre",
                                                         label = h5(strong("Protocol 3 TSS reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ))
                                            ))),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.stre"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.stre"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.stre"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.stre")
                                            
                         ))
                       )
              ),
              
              #===================================================
              # Outfall Stabilization/SPSCs
              #===================================================
              
              tabPanel("Outfall Stabilization or SPSC", value = "3", icon=icon("dot-circle"), 
                       
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.outspsc",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1outspsc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1outspsc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Outfall (OUT)"  
                                                                          ,"Regenerative Step Pool Stormwater Conveyance System (SPSC)"),
                                                           selected = NULL),
                                            bsPopover(id = "q1outspsc", title = "Select BMP Type",
                                                      content = paste0("If the BMP is a regenerative step pool conveyance system, it can be credited by entering the BMP drainage area, impervious area, and water quality volume (Protocol 4). Additionally, a regenerative step pool conveyance system can also be credited using Protocol 5. Protocol 4 and 5 are additive. If the BMP is an outfall stabilization, do not enter BMP drainage area, impervious area, or a water quality volume (Protocol 4). Outfall stabilizations should only be credited using Protocol 5. When entering Protocol 5 data, do no apply delivery factors. Further information on Protocol 5 can be found by ",  
                                                                       a("reviewing the expert panel guidance.", 
                                                                         href = "https://chesapeakestormwater.net/wp-content/uploads/dlm_uploads/2019/10/FINAL-APPROVED-OUTFALL-RESTORATION-MEMO-101519.pdf",
                                                                         target="_blank"), " For further information, please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            fluidRow(column(6,numericInput("da.outspsc",
                                                                           label = h5(strong("BMP drainage area (ac):"),
                                                                                      tags$style(type = "text/css", "#q2outspsc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                                      bsButton("q2outspsc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                           value = NULL, width = '100%'
                                            ),
                                            bsPopover(id = "q2outspsc", title = "Enter BMP drainage area",
                                                      content = paste0("If no access to GIS please use  ", 
                                                                       a("the StreamStats app.", 
                                                                         href = "https://streamstats.usgs.gov/ss",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            numericInput("ia.outspsc",
                                                         label = h5(strong("BMP impervious area (ac):"),
                                                                    tags$style(type = "text/css", "#q3outspsc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                    bsButton("q3outspsc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                         value = NULL, width = '100%'
                                            ),
                                            bsPopover(id = "q3outspsc", title = "Enter impervious area in the BMP drainage area",
                                                      content = paste0("If no access to GIS please use  ", 
                                                                       a("the StreamStats app.", 
                                                                         href = "https://streamstats.usgs.gov/ss",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            numericInput("wqv.outspsc",
                                                         label = h5(strong("Water Quality Volume, WQv (ac/ft):")),
                                                         value = NULL, width = '100%'
                                            )),
                                            column(6,numericInput("p5tn.outspsc",
                                                                  label = h5(strong("Protocol 5 TN reduction (lbs):")),
                                                                  value = NULL, width = '100%'
                                            ),
                                            numericInput("p5tp.outspsc",
                                                         label = h5(strong("Protocol 5 TP reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ),
                                            numericInput("p5tss.outspsc",
                                                         label = h5(strong("Protocol 5 TSS reduction (lbs):")),
                                                         value = NULL, width = '100%'
                                            ))),
                                            selectizeInput("lrs.outspsc",
                                                           label = h5(strong("Land river segment:"),
                                                                      tags$style(type = "text/css", "#q4outspsc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q4outspsc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("N24003WL0_4390_0000"
                                                                          ,"N24003WL0_4391_0000"
                                                                          ,"N24003WL0_4392_0000"
                                                                          ,"N24003WL0_4393_0000"
                                                                          ,"N24003WL0_4394_0000"
                                                                          ,"N24003WL0_4420_0000"
                                                                          ,"N24003WL0_4421_0000"
                                                                          ,"N24003WL0_4422_0000"
                                                                          ,"N24003WL0_4423_0000"
                                                                          ,"N24003WL0_4424_0000"
                                                                          ,"N24003WL0_4425_0000"
                                                                          ,"N24003WL0_4600_0000"
                                                                          ,"N24003WL0_4601_0000"
                                                                          ,"N24003WL0_4602_0000"
                                                                          ,"N24003WL0_4603_0000"
                                                                          ,"N24003WL0_4770_0000"
                                                                          ,"N24003WL0_4771_0000"
                                                                          ,"N24003WL0_4772_0000"
                                                                          ,"N24003WM0_3961_0000"
                                                                          ,"N24003WM0_3962_0000"
                                                                          ,"N24003WM0_3963_0000"
                                                                          ,"N24003WM0_3966_0000"
                                                                          ,"N24003WM3_4060_0001"
                                                                          ,"N24003XL3_4710_0000"
                                                                          ,"N24003XL3_4711_0000"
                                                                          ,"N24003XL3_4712_0000"
                                                                          ,"N24003XL3_4713_0000"
                                                                          ,"N24003XL3_4950_0000"
                                                                          ,"N24003XU2_4270_4650"
                                                                          ,"N24003XU2_4480_4650"
                                                                          ,"N24003XU3_4650_0001"),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           ) 
                                            ),
                                            bsPopover(id = "q4outspsc", title = "Select Land River Segment",
                                                      content = paste0("If unknown, refer to Land River Segment Map and search for the area where your BMP is located using Latitude and Longitude search tool."
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            )
                         )),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.outspsc"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.outspsc"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.outspsc"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.outspsc")
                                            
                         ))
                       )
                       
                       
                       
                       
              ),
              
              #===================================================
              # Shoreline Restorations
              #===================================================
              
              tabPanel("Shoreline Restoration", value = "4", icon=icon("ship"), 
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            fluidRow(
                                              column(6,selectizeInput("protocol.shst",
                                                                      label = h5(strong("Use shoreline protocols?"),
                                                                                 tags$style(type = "text/css", "#q1shst {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                                 bsButton("q1shst", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                      choices = list("Yes"  
                                                                                     ,"No"),
                                                                      selected = NULL,options = list(
                                                                        placeholder = 'Please select an option below',
                                                                        onInitialize = I('function() { this.setValue(""); }')
                                                                      )),
                                                     bsPopover(id = "q1shst", title = "Shoreline Protocol Reductions",
                                                               content = paste0("If no, enter length only. Credit will use the planning rate and will be based on length. If yes, enter data below. Further information on shoreline restoration protocols can be found by ", 
                                                                                a("reviewing the expert panel guidance.", 
                                                                                  href = "https://chesapeakestormwater.net/wp-content/uploads/dlm_uploads/2018/05/SHORT_Final_Shoreline-Management-Protocol_11-24-19_FINAL.pdf",
                                                                                  target="_blank")
                                                               ),
                                                               placement = "right", 
                                                               trigger = "focus", 
                                                               options = list(container = "body")
                                                     ),
                                                     numericInput("length.shst",
                                                                  label = h5(strong("Length of restored shoreline (ft):")),
                                                                  value = NULL, width = '100%'
                                                     ),
                                                     numericInput("bankheight.shst",
                                                                  label = h5(strong("Average bank height (ft):")),
                                                                  value = NULL, width = '100%'
                                                     ),
                                                     numericInput("laterosion.shst",
                                                                  label = h5(strong("Lateral erosion rate (ft/yr):"),
                                                                             tags$style(type = "text/css", "#q2shst {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                             bsButton("q2shst", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                  value = NULL, width = '100%'),
                                                     bsPopover(id = "q2shst", title = "Shoreline Erosion Rate",
                                                               content = paste0("If unknown, refer to the Shoreline Erosion Transect Map. Clicking on a transect will display the annual erosion rate. The average should be computed for the entire length of the proposed restoration. Positive numbers are aggradation, negative numbers are erosion. Enter the lateral erosion rate as a positive number." 
                                                               ),
                                                               placement = "right", 
                                                               trigger = "focus", 
                                                               options = list(container = "body")
                                                     )),
                                              column(6,numericInput("soilbulkden.shst",
                                                                    label = h5(strong("Soil bulk density (lbs/ft3):"),
                                                                               tags$style(type = "text/css", "#q3shst {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                               bsButton("q3shst", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                                    value = 93.6, width = '100%'
                                              ),
                                              bsPopover(id = "q3shst", title = "Soil Bulk Density",
                                                        content = paste0("Unless measured, use the default of 93.6 lbs/ft3." 
                                                        ),
                                                        placement = "right", 
                                                        trigger = "focus", 
                                                        options = list(container = "body")
                                              ),
                                              numericInput("srf.shst",
                                                           label = h5(strong("Sand reduction factor:"),
                                                                      tags$style(type = "text/css", "#q4shst {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q4shst", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           value = 0.551, width = '100%'
                                              ),
                                              bsPopover(id = "q4shst", title = "Sand Reduction Factor",
                                                        content = paste0("Unless measured, use Maryland default of 0.551." 
                                                        ),
                                                        placement = "right", 
                                                        trigger = "focus", 
                                                        options = list(container = "body")
                                              ),
                                              numericInput("bir.shst",
                                                           label = h5(strong("Bank instability reduction factor:"),
                                                                      tags$style(type = "text/css", "#q5shst {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q5shst", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           value = 1, width = '100%'
                                              ),
                                              bsPopover(id = "q5shst", title = "Bank Instability Reduction Factor",
                                                        content = paste0("Use the default value of 1.00. If slope stability has not been addressed, enter 0.5." 
                                                        ),
                                                        placement = "right", 
                                                        trigger = "focus", 
                                                        options = list(container = "body")
                                              ),
                                              numericInput("plantacres.shst",
                                                           label = h5(strong("Area of tidal wetlands planted (ac):")),
                                                           value = NULL, width = '100%'
                                              )),
                                            ))),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.shst"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.shst"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.shst"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.shst")
                                            
                         ))
                       )
              ),
              
              #===================================================
              # Land Cover Conversion
              #===================================================
              
              tabPanel("Land Cover Conversion", value = "5",icon=icon("tree"),
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.lcc",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1lcc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1lcc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Conservation Landscaping (CLTM)"  
                                                                          ,"Forest Conservation (FCO)"
                                                                          ,"Impervious Surface Reduction [Impervious to Pervious] (IMPP)"
                                                                          ,"Impervious Surface to Forest [Impervious to Pervious with Forest Planting] (IMPF)"
                                                                          ,"Forestation on Pervious Urban [Forest Planting] (FPU)"
                                                                          ,"Riparian Conservation Landscaping (RCL)"
                                                                          ,"Riparian Forest Planting (RFP)"
                                                                          ,"Street Trees (STCI)"
                                                                          ,"Urban Tree Canopy [Pervious Turf to Tree Canopy over Turf] (UTC)" 
                                                                          
                                                           ),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           )),
                                            bsPopover(id = "q1lcc", title = "Select BMP Type",
                                                      content = paste0("Some land cover conversion BMPs have minimum requirements for the size of the planting area, tree survival rates, and tree size. Please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            
                                            numericInput("acres.lcc",
                                                         label = "Area of land converted (ac):",
                                                         value = NULL, width = '100%'
                                            )
                         )),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.lcc"),
                                            span(textOutput("eia.lcc.warning"), style="color:red"),
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.lcc"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.lcc"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.lcc")
                                            
                         ))
                       )
              ),
              
              
              #===================================================
              # Soil Compaction
              #===================================================
              
              tabPanel("Soil Compaction", value = "6",icon=icon("mountain"),
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.sc",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1sc {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1sc", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Urban Soil Restoration (Compacted Pervious Surfaces)"
                                                                          ,"Urban Soil Restoration (Removed Impervious Surfaces)"
                                                           ),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           ),width='80%'),
                                            bsPopover(id = "q1sc", title = "Select BMP Type",
                                                      content = paste0("Please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            numericInput("acres.sc",
                                                         label = "Area of soil restored (ac):",
                                                         value = NULL, width = '40%'
                                            ),
                                            selectizeInput("depth.sc",
                                                           label = h5(strong("Depth of excavation or tilling depth (in):")),
                                                           choices = list("15 Inches"
                                                                          ,"20 Inches"
                                                           ),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           ), width = '60%' 
                                            ),
                         )),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.sc"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.sc"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.sc"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.sc")
                                            
                         ))
                       )
              ),
              
              #===================================================
              # Alternative Surfaces 
              #===================================================
              
              tabPanel("Alternative Surfaces", value = "7",icon=icon("road"),
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.as",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1as {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1as", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Green Roof, Extensive (AGRE)"
                                                                          ,"Green Roof, Intensive (AGRI)"
                                                                          ,"Permeable Pavement (APMP)"
                                                                          ,"Reinforced Turf (ARTF)"
                                                           ),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           ),width='70%'),
                                            bsPopover(id = "q1as", title = "Select BMP Type",
                                                      content = paste0("Please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            numericInput("acresremoved.as",
                                                         label = "Area of impervious surface replaced (ac):",
                                                         value = NULL, width = '40%'
                                            ),
                         )),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.as"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.as"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.as"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.as")
                                            
                         ))
                       )
              ),
              
              #===================================================
              # Non-Structural BMPs
              #===================================================
              
              tabPanel("Non-Structural BMP", value = "7",icon=icon("tint-slash"),
                       
                       fluidRow(
                         br(),
                         column(7,wellPanel(style = "background-color: #cdf7d4;",
                                            h4(strong("Input values:")),
                                            
                                            selectizeInput("bmptype.nsbmp",
                                                           label = h5(strong("BMP type:"),
                                                                      tags$style(type = "text/css", "#q1nsbmp {vertical-align: bottom; width: 16px !important; height: 14.4px !important; font-size: 5.4px !important; line-height: 2px !important; padding: 0px !important;}"),
                                                                      bsButton("q1nsbmp", label = "", icon = icon("question"), style = "primary", size = "extra-small")),
                                                           choices = list("Rooftop Disconnect (NDRR)"
                                                                          ,"Non-Rooftop Disconnect (NDNR)"
                                                                          ,"Sheetflow to Conservation Area (NSCA)"
                                                           ),
                                                           selected = NULL,options = list(
                                                             placeholder = 'Please select an option below',
                                                             onInitialize = I('function() { this.setValue(""); }')
                                                           ),width = '70%'),
                                            bsPopover(id = "q1nsbmp", title = "Select BMP Type",
                                                      content = paste0("Please refer to ", 
                                                                       a("MDE 2020 Guidance.", 
                                                                         href = "https://mde.maryland.gov/programs/Water/StormwaterManagementProgram/Documents/2020%20MS4%20Accounting%20Guidance.pdf",
                                                                         target="_blank")
                                                      ),
                                                      placement = "right", 
                                                      trigger = "focus", 
                                                      options = list(container = "body")
                                            ),
                                            numericInput("imperviousacres.nsbmp",
                                                         label = "Impervious area (ac):",
                                                         value = NULL, width = '40%'
                                            ),
                                            numericInput("disconnection.nsbmp",
                                                         label = "Impervious runoff disconnected (%):",
                                                         value = NULL, width = '40%'
                                            ),
                                            
                         )),
                         
                         column(5,wellPanel(style = "background: #e6f7ff",
                                            h4(strong("Impervious and TMDL credit:")),
                                            
                                            h5(strong("Impervious surface restored (ac):")),
                                            textOutput("eia.nsbmp"),
                                            
                                            br(),h5(strong("Total nitrogen reduction (lbs):")),
                                            textOutput("tnred.nsbmp"),
                                            
                                            br(),h5(strong("Total phosphorus reduction (lbs):")),
                                            textOutput("tpred.nsbmp"),
                                            
                                            br(),h5(strong("Total suspended sediment reduction (lbs):")),
                                            textOutput("tssred.nsbmp")
                                            
                         ))
                       )
              ),              
              
              #===================================================
              # Land River Segment Map
              #===================================================
              
              tabPanel("Land River Segment Map", value = "8",icon=icon("map-marked"),
                       
                       fluidPage(br(),column(12,wellPanel(style = "background: #e6f7ff",
                                                          h4(strong("Click on the land river segment in the location of the proposed BMP. Land River Segment ID will show in popup when clicking on the appropriate segment.")),
                       ),
                       ),
                       br(),
                       leafletOutput("mymap",height = 520, width = "100%")%>% withSpinner(color="#cdf7d4")
                       
                       )
              ),
              
              #===================================================
              # Shoreline Erosion Map
              #===================================================
              
              tabPanel("Shoreline Erosion Map", value = "8",icon=icon("map-marked"),
                       
                       fluidPage(br(),column(12,wellPanel(style = "background: #e6f7ff",
                                                          h4(strong("Click on transect to see annual erosion rate. Rendering may be slow due to the size of this dataset.")))),
                                 br(),
                                 leafletOutput("mymap2",height = 550, width = "100%")%>% withSpinner(color="#cdf7d4")
                                 
                       )
              )
              
  )
  
)
