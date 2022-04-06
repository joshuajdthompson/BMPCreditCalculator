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
#  02/28/2022    Added result validation messages and minimaps    JT 
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
library(rmapshaper)
library(shinycssloaders)
library(shinythemes)
library(Cairo)
library(tidyverse)
library(htmlwidgets)

#aacolrs <- st_read("RiverSegmentsAACo.shp") %>%
#  st_transform(4326)
#aacolrs <- ms_simplify(aacolrs)

landriversegs <- read.csv("eot_lndrvrsg.csv",header=T) 

#aashoretransects <- st_read("AnneArundelShorelineTransects.shp") %>%
#  st_transform(4326)
#aashoretransects <- ms_simplify(aashoretransects)

#save(aacolrs, file = "aacolrs.RData")
#save(aashoretransects, file = "aashoretransects.RData")
load("aacolrs.RData") # saving geom to RData means it's significanly faster. 
load("aashoretransects.RData")


shinyServer(function(input, output) {
  
  values <- reactiveValues()
  observe({
    
    #===================================================
    # Structural BMP
    #===================================================
    
    # define RR and ST
    RR <- c("MILS","MIBR","MIDW","MMBR","FBIO","MSGW","MRNG","MENF","MSWG","MSWW","ODSW","OWSW","MSWB","AGRE","AGRI","APRP","ARTF","MRWH","SPSC","NDRR","NDNR","NSCA")
    ST <- c("PWED","PWET","PMPS","PPKT","PMED","WSHW","WEDW","WPWS","WPKT","IBAS","ITRN","FSND","FUND","FPER","FORG","FPKT")
    
    # strip out description to just get code
    values$BMPPracticeType.sbmp <- ifelse(str_extract(string = input$bmptype.sbmp,pattern = "(?<=\\().*(?=\\))") %in% RR,"RR",
                                          ifelse(str_extract(string = input$bmptype.sbmp,pattern = "(?<=\\().*(?=\\))") %in% ST,"ST",NA))
    
    values$rv.sbmp <- 0.05+0.009*((input$ia.sbmp/input$da.sbmp)*100) 
    values$wqt.sbmp <- (input$wqv.sbmp*12)/(values$rv.sbmp*input$da.sbmp)
    
    values$eia.sbmp<- ifelse(values$BMPPracticeType.sbmp == "RR" | input$c3.sbmp == "Yes",ifelse(values$wqt.sbmp>1&values$wqt.sbmp<3,(input$ia.sbmp*1)+(input$ia.sbmp*((values$wqt.sbmp-1)*0.25)),
                                                                                                 ifelse(values$wqt.sbmp>=3,(input$ia.sbmp*1)+(input$ia.sbmp*((3-1)*0.25)),
                                                                                                        ifelse(values$wqt.sbmp<=1,input$ia.sbmp*values$wqt.sbmp,NA)))*1.35,
                             ifelse(values$wqt.sbmp>1&values$wqt.sbmp<3,(input$ia.sbmp*1)+(input$ia.sbmp*((values$wqt.sbmp-1)*0.25)),
                                    ifelse(values$wqt.sbmp>=3,(input$ia.sbmp*1)+(input$ia.sbmp*((3-1)*0.25)),
                                           ifelse(values$wqt.sbmp<=1,input$ia.sbmp*values$wqt.sbmp,NA))))
    
    #print(values$eia.sbmp)
    #print(input$c3.sbmp)
    
    values$tneot.sbmp <- landriversegs$TN_EOT[landriversegs$LndRvrSg == input$lrs.sbmp]
    values$tpeot.sbmp <- landriversegs$TP_EOT[landriversegs$LndRvrSg == input$lrs.sbmp] 
    values$tsseot.sbmp <- landriversegs$TSS_EOT[landriversegs$LndRvrSg == input$lrs.sbmp]
    
    #print(values$tneot.sbmp)
    #print(values$tpeot.sbmp)
    #print(values$tsseot.sbmp)
    
    
    values$tnred.sbmp <- ifelse(values$wqt.sbmp >3, ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*13.43)+(20.39*input$ia.sbmp))*(((1.5319*(3^5)) - (15.414*(3^4)) + (62.165*(3^3)) - (128.07*(3^2)) + (139.73*3))/100))*values$tneot.sbmp,
                                                           ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*13.43)+(20.39*input$ia.sbmp))*(((0.9232*(3^5)) - (9.1883*(3^4)) + (36.618*(3^3)) - (74.852*(3^2)) + (81.522*3))/100))*values$tneot.sbmp),
                                ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*13.43)+(20.39*input$ia.sbmp))*(((1.5319*(values$wqt.sbmp ^5)) - (15.414*(values$wqt.sbmp ^4)) + (62.165*(values$wqt.sbmp ^3)) - (128.07*(values$wqt.sbmp ^2)) + (139.73*values$wqt.sbmp ))/100))*values$tneot.sbmp,
                                       ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*13.43)+(20.39*input$ia.sbmp))*(((0.9232*(values$wqt.sbmp ^5)) - (9.1883*(values$wqt.sbmp ^4)) + (36.618*(values$wqt.sbmp ^3)) - (74.852*(values$wqt.sbmp ^2)) + (81.522*values$wqt.sbmp ))/100))*values$tneot.sbmp))	   
    
    #print(values$tnred.sbmp)
    
    values$tpred.sbmp <- ifelse(values$wqt.sbmp >3, ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*2.10)+(2.55*input$ia.sbmp))*(((1.8636*(3^5)) - (18.531*(3^4)) + (73.724*(3^3)) - (150.33*(3^2)) + (163.29*3))/100))*values$tpeot.sbmp,
                                                           ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*2.10)+(2.55*input$ia.sbmp))*(((1.4472*(3^5)) - (14.443*(3^4)) + (57.677*(3^3)) - (117.91*(3^2)) + (128.25*3))/100))*values$tpeot.sbmp),
                                ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*2.10)+(2.55*input$ia.sbmp))*(((1.8636*(values$wqt.sbmp ^5)) - (18.531*(values$wqt.sbmp ^4)) + (73.724*(values$wqt.sbmp ^3)) - (150.33*(values$wqt.sbmp ^2)) + (163.29*values$wqt.sbmp ))/100))*values$tpeot.sbmp,
                                       ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*2.10)+(2.55*input$ia.sbmp))*(((1.4472*(values$wqt.sbmp ^5)) - (14.443*(values$wqt.sbmp ^4)) + (57.677*(values$wqt.sbmp ^3)) - (117.91*(values$wqt.sbmp ^2)) + (128.25*values$wqt.sbmp ))/100))*values$tpeot.sbmp))
    
    #print(values$tpred.sbmp)
    
    values$tssred.sbmp <- ifelse(values$wqt.sbmp >3, ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*3552)+(8793*input$ia.sbmp))*(((1.938*(3^5)) - (19.377*(3^4)) + (77.698*(3^3)) - (159.63*(3^2)) + (174.45*3))/100))*values$tsseot.sbmp,
                                                            ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*3552)+(8793*input$ia.sbmp))*(((1.8636*(3^5)) - (18.531*(3^4)) + (73.724*(3^3)) - (150.33*(3^2)) + (163.29*3))/100))*values$tsseot.sbmp),
                                 ifelse(values$BMPPracticeType.sbmp=="RR",ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*3552)+(8793*input$ia.sbmp))*(((1.938*(values$wqt.sbmp ^5)) - (19.377*(values$wqt.sbmp ^4)) + (77.698*(values$wqt.sbmp ^3)) - (159.63*(values$wqt.sbmp ^2)) + (174.45*values$wqt.sbmp ))/100))*values$tsseot.sbmp,
                                        ifelse(is.na(input$ia.sbmp),NA,((( input$da.sbmp-input$ia.sbmp)*3552)+(8793*input$ia.sbmp))*(((1.8636*(values$wqt.sbmp ^5)) - (18.531*(values$wqt.sbmp ^4)) + (73.724*(values$wqt.sbmp ^3)) - (150.33*(values$wqt.sbmp ^2)) + (163.29*values$wqt.sbmp ))/100))*values$tsseot.sbmp))
    
    #print(values$tssred.sbmp)
    
    #===================================================
    # Stream Restorations
    #===================================================
    
    values$eia.stre<- ifelse(input$protocol.stre == "No",ifelse(is.na(input$length.stre),0,input$length.stre*0.02),  
                             ifelse(all(is.na(c(input$p1tn.stre, input$p2tn.stre, input$p3tn.stre,input$p1tp.stre,input$p3tp.stre,input$p1tss.stre, input$p3tss.stre))),NA,
                                    (((sum(input$p1tn.stre,input$p2tn.stre, input$p3tn.stre,na.rm=T))/18.08)+((sum(input$p1tp.stre,input$p3tp.stre,na.rm=T))/2.23)+((sum(input$p1tss.stre,input$p3tss.stre,na.rm=T))/8046))/3))
    
    
    
    
    values$tnred.stre<- ifelse(input$protocol.stre == "No",ifelse(is.na(input$length.stre),0,input$length.stre*0.075),
                               ifelse(all(is.na(c(input$p1tn.stre, input$p2tn.stre, input$p3tn.stre))),NA,
                                      sum(input$p1tn.stre, input$p2tn.stre, input$p3tn.stre,na.rm=T)))
    
    values$tpred.stre<- ifelse(input$protocol.stre == "No",ifelse(is.na(input$length.stre),0,input$length.stre*0.068),
                               ifelse(all(is.na(c(input$p1tp.stre,input$p3tp.stre))),NA,
                                      sum(input$p1tp.stre,input$p3tp.stre,na.rm=T)))
    
    values$tssred.stre<- ifelse(input$protocol.stre == "No",ifelse(is.na(input$length.stre),0,input$length.stre*248),
                                ifelse(all(is.na(c(input$p1tss.stre, input$p3tss.stre))),NA,
                                       sum(input$p1tss.stre, input$p3tss.stre,na.rm=T)))
    
    #===================================================
    # Outfalls and SPSCs
    #===================================================
    

    if(str_extract(string = input$bmptype.outspsc,pattern = "(?<=\\().*(?=\\))") == "SPSC"){
      values$rv.outspsc <- 0.05+0.009*((input$ia.outspsc/input$da.outspsc)*100) 
      values$wqt.outspsc <- (input$wqv.outspsc*12)/(values$rv.outspsc*input$da.outspsc)  
      values$eia.spsc_up <- ifelse(values$wqt.outspsc>1&values$wqt.outspsc<3,(input$ia.outspsc*1)+(input$ia.outspsc*((values$wqt.outspsc-1)*0.25)),
                                   ifelse(values$wqt.outspsc>=3,(input$ia.outspsc*1)+(input$ia.outspsc*((3-1)*0.25)),
                                          ifelse(values$wqt.outspsc<=1,input$ia.outspsc*values$wqt.outspsc,NA)))
      values$tneot.spsc_up <- landriversegs$TN_EOT[landriversegs$LndRvrSg == input$lrs.outspsc]
      values$tpeot.spsc_up <- landriversegs$TP_EOT[landriversegs$LndRvrSg == input$lrs.outspsc] 
      values$tsseot.spsc_up <- landriversegs$TSS_EOT[landriversegs$LndRvrSg == input$lrs.outspsc]
      values$tnred.spsc_up <- ifelse(values$wqt.outspsc >3, ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*13.43)+(20.39*input$ia.outspsc))*(((1.5319*(3^5)) - (15.414*(3^4)) + (62.165*(3^3)) - (128.07*(3^2)) + (139.73*3))/100))*values$tneot.spsc_up,
                                     ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*13.43)+(20.39*input$ia.outspsc))*(((1.5319*(values$wqt.outspsc ^5)) - (15.414*(values$wqt.outspsc ^4)) + (62.165*(values$wqt.outspsc ^3)) - (128.07*(values$wqt.outspsc ^2)) + (139.73*values$wqt.outspsc ))/100))*values$tneot.spsc_up)
      values$tpred.spsc_up <- ifelse(values$wqt.outspsc >3, ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*2.10)+(2.55*input$ia.outspsc))*(((1.8636*(3^5)) - (18.531*(3^4)) + (73.724*(3^3)) - (150.33*(3^2)) + (163.29*3))/100))*values$tpeot.spsc_up,
                                     ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*2.10)+(2.55*input$ia.outspsc))*(((1.8636*(values$wqt.outspsc ^5)) - (18.531*(values$wqt.outspsc ^4)) + (73.724*(values$wqt.outspsc ^3)) - (150.33*(values$wqt.outspsc ^2)) + (163.29*values$wqt.outspsc ))/100))*values$tpeot.spsc_up)
      values$tssred.spsc_up <- ifelse(values$wqt.outspsc >3, ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*3552)+(8793*input$ia.outspsc))*(((1.938*(3^5)) - (19.377*(3^4)) + (77.698*(3^3)) - (159.63*(3^2)) + (174.45*3))/100))*values$tsseot.spsc_up,
                                      ifelse(is.na(input$ia.outspsc),NA,((( input$da.outspsc-input$ia.outspsc)*3552)+(8793*input$ia.outspsc))*(((1.938*(values$wqt.outspsc ^5)) - (19.377*(values$wqt.outspsc ^4)) + (77.698*(values$wqt.outspsc ^3)) - (159.63*(values$wqt.outspsc ^2)) + (174.45*values$wqt.outspsc ))/100))*values$tsseot.spsc_up)
      if(all(is.na(c(input$p5tn.outspsc,input$p5tp.outspsc,input$p5tss.outspsc)))=="TRUE"){
        values$eia.outspsc <- values$eia.spsc_up
        values$tnred.outspsc  <- values$tnred.spsc_up 
        values$tpred.outspsc <- values$tpred.spsc_up
        values$tssred.outspsc <- values$tssred.spsc_up 
      } else if(all(is.na(c(input$p5tn.outspsc,input$p5tp.outspsc,input$p5tss.outspsc)))=="FALSE"){
        values$eia.spsc_str<- ((ifelse(is.na(input$p5tn.outspsc),0,input$p5tn.outspsc)/18.08)+(ifelse(is.na(input$p5tp.outspsc),0,input$p5tp.outspsc)/2.23)+(ifelse(is.na(input$p5tss.outspsc),0,input$p5tss.outspsc)/8046))/3
        values$eia.outspsc <- sum(values$eia.spsc_up,values$eia.spsc_str,na.rm=T)
        values$tnred.outspsc  <- sum(values$tnred.spsc_up,input$p5tn.outspsc,na.rm=T) 
        values$tpred.outspsc <- sum(values$tpred.spsc_up,input$p5tp.outspsc,na.rm=T)
        values$tssred.outspsc <- sum(values$tssred.spsc_up,input$p5tss.outspsc,na.rm=T) 
      }} else if(str_extract(string = input$bmptype.outspsc,pattern = "(?<=\\().*(?=\\))") == "OUT"){
        values$eia.outspsc <- ((ifelse(is.na(input$p5tn.outspsc),NA,input$p5tn.outspsc)/18.08)+(ifelse(is.na(input$p5tp.outspsc),0,input$p5tp.outspsc)/2.23)+(ifelse(is.na(input$p5tss.outspsc),0,input$p5tss.outspsc)/8046))/3
        values$tnred.outspsc  <- input$p5tn.outspsc
        values$tpred.outspsc <- input$p5tp.outspsc
        values$tssred.outspsc <- input$p5tss.outspsc
      } 

    #===================================================
    # Shoreline Restorations
    #===================================================
    
    if(input$protocol.shst=="No"){
      values$eia.shst <- ifelse(is.na(input$length.shst),NA,input$length.shst*0.04)
      values$tnred.shst <- ifelse(is.na(input$length.shst),NA,input$length.shst*0.173)
      values$tpred.shst <- ifelse(is.na(input$length.shst),NA,input$length.shst*0.122)
      values$tssred.shst <- ifelse(is.na(input$length.shst),NA,input$length.shst*328)
    } else if (input$protocol.shst=="Yes"){
      values$p1tss.shst <- ifelse(is.na(input$bankheight.shst),NA,((input$length.shst*input$bankheight.shst*input$laterosion.shst*input$soilbulkden.shst)*input$srf.shst*input$bir.shst))
      values$p1tn.shst <- (values$p1tss.shst*0.00029)/input$srf.shst
      values$p1tp.shst <- (values$p1tss.shst*0.000205)/input$srf.shst
      values$p2tn.shst <- ifelse(is.na(input$plantacres.shst),NA,input$plantacres.shst*85)
      values$p3tp.shst <- ifelse(is.na(input$plantacres.shst),NA,input$plantacres.shst*5.289)
      values$p3tss.shst <- ifelse(is.na(input$plantacres.shst),NA,input$plantacres.shst*6959)
      values$p4tn.shst <- ifelse(is.na(input$plantacres.shst),NA,input$plantacres.shst*6.83)
      values$p4tp.shst <- ifelse(is.na(input$plantacres.shst),NA,input$plantacres.shst*0.3)
      
      values$eia.shst<- ifelse(all(is.na(c(values$p1tss.shst,values$p1tn.shst,values$p1tp.shst,values$p2tn.shst,values$p3tp.shst,values$p3tss.shst,values$p4tn.shst,values$p4tp.shst))),NA,
                               (((sum(values$p1tn.shst,values$p2tn.shst,values$p4tn.shst,na.rm=T))/18.08)+((sum(values$p1tp.shst,values$p3tp.shst,values$p4tp.shst,na.rm=T))/2.23)+((sum(values$p1tss.shst,values$p3tss.shst,na.rm=T))/8046))/3)
      
      values$tssred.shst <- ifelse(all(is.na(c(values$p1tss.shst,values$p3tss.shst))),NA,sum(values$p1tss.shst,values$p3tss.shst,na.rm=T))
      values$tnred.shst <- ifelse(all(is.na(c(values$p1tn.shst,values$p2tn.shst,values$p4tn.shst))),NA,sum(values$p1tn.shst,values$p2tn.shst,values$p4tn.shst,na.rm=T))
      values$tpred.shst <- ifelse(all(is.na(c(values$p1tp.shst,values$p3tp.shst,values$p4tp.shst))),NA,sum(values$p1tp.shst,values$p3tp.shst,values$p4tp.shst,na.rm=T))
      
    } else {
      values$eia.shst<- NA
      values$tssred.shst <- NA
      values$tnred.shst <- NA
      values$tpred.shst <- NA 
    }  
    #print(paste0("Protocol Selection: ",input$protocol.shst))
    #class(input$protocol.shst)
    #if(is.null(input$protocol.shst)){
    #  print("Null")
    #}
    #===================================================
    # land cover conversions 
    #===================================================
    
    values$eia.lcc <- ifelse(input$bmptype.lcc == "Conservation Landscaping (CLTM)",input$acres.lcc*0.37, 
                             ifelse(input$bmptype.lcc %in%c("Forestation on Pervious Urban [Forest Planting] (FPU)","Impervious Surface to Forest [Impervious to Pervious with Forest Planting] (IMPF)"),input$acres.lcc*1.10,
                                    ifelse(input$bmptype.lcc =="Impervious Surface Reduction [Impervious to Pervious] (IMPP)",input$acres.lcc*0.71,
                                           ifelse(input$bmptype.lcc =="Street Trees (STCI)",input$acres.lcc*0.40, 
                                                  ifelse(input$bmptype.lcc =="Urban Tree Canopy [Pervious Turf to Tree Canopy over Turf] (UTC)",input$acres.lcc*0.28,
                                                         ifelse(input$bmptype.lcc =="Riparian Conservation Landscaping (RCL)",input$acres.lcc*0.50,
                                                                ifelse(input$bmptype.lcc =="Riparian Forest Planting (RFP)",input$acres.lcc*1.50,
                                                                       ifelse(input$bmptype.lcc =="Forest Conservation (FCO)",input$acres.lcc*0.46,NA))))))))                                   
    
    values$tnred.lcc <- ifelse(input$bmptype.lcc == "Conservation Landscaping (CLTM)",input$acres.lcc*5.24, 
                               ifelse(input$bmptype.lcc %in%c("Forestation on Pervious Urban [Forest Planting] (FPU)","Impervious Surface to Forest [Impervious to Pervious with Forest Planting] (IMPF)"),input$acres.lcc*11.12,
                                      ifelse(input$bmptype.lcc =="Impervious Surface Reduction [Impervious to Pervious] (IMPP)",input$acres.lcc*6.96,
                                             ifelse(input$bmptype.lcc =="Street Trees (STCI)",input$acres.lcc*3.10, 
                                                    ifelse(input$bmptype.lcc =="Urban Tree Canopy [Pervious Turf to Tree Canopy over Turf] (UTC)",input$acres.lcc*3.20,
                                                           ifelse(input$bmptype.lcc =="Riparian Conservation Landscaping (RCL)",input$acres.lcc*6.75,
                                                                  ifelse(input$bmptype.lcc =="Riparian Forest Planting (RFP)",input$acres.lcc*14.34,
                                                                         ifelse(input$bmptype.lcc =="Forest Conservation (FCO)",input$acres.lcc*10.57,NA))))))))   
    
    values$tpred.lcc <- ifelse(input$bmptype.lcc == "Conservation Landscaping (CLTM)",input$acres.lcc*0.53, 
                               ifelse(input$bmptype.lcc %in%c("Forestation on Pervious Urban [Forest Planting] (FPU)","Impervious Surface to Forest [Impervious to Pervious with Forest Planting] (IMPF)"),input$acres.lcc*1.78,
                                      ifelse(input$bmptype.lcc =="Impervious Surface Reduction [Impervious to Pervious] (IMPP)",input$acres.lcc*0.45,
                                             ifelse(input$bmptype.lcc =="Street Trees (STCI)",input$acres.lcc*0.76, 
                                                    ifelse(input$bmptype.lcc =="Urban Tree Canopy [Pervious Turf to Tree Canopy over Turf] (UTC)",input$acres.lcc*0.50,
                                                           ifelse(input$bmptype.lcc =="Riparian Conservation Landscaping (RCL)",input$acres.lcc*0.74,
                                                                  ifelse(input$bmptype.lcc =="Riparian Forest Planting (RFP)",input$acres.lcc*2.50,
                                                                         ifelse(input$bmptype.lcc =="Forest Conservation (FCO)",input$acres.lcc*1.10,NA))))))))   
    
    values$tssred.lcc <- ifelse(input$bmptype.lcc == "Conservation Landscaping (CLTM)",input$acres.lcc*0.00, 
                                ifelse(input$bmptype.lcc %in%c("Forestation on Pervious Urban [Forest Planting] (FPU)","Impervious Surface to Forest [Impervious to Pervious with Forest Planting] (IMPF)"),input$acres.lcc*2805,
                                       ifelse(input$bmptype.lcc =="Impervious Surface Reduction [Impervious to Pervious] (IMPP)",input$acres.lcc*5241,
                                              ifelse(input$bmptype.lcc =="Street Trees (STCI)",input$acres.lcc*1404, 
                                                     ifelse(input$bmptype.lcc =="Urban Tree Canopy [Pervious Turf to Tree Canopy over Turf] (UTC)",input$acres.lcc*206,
                                                            ifelse(input$bmptype.lcc =="Riparian Conservation Landscaping (RCL)",input$acres.lcc*0.00,
                                                                   ifelse(input$bmptype.lcc =="Riparian Forest Planting (RFP)",input$acres.lcc*4411,
                                                                          ifelse(input$bmptype.lcc =="Forest Conservation (FCO)",input$acres.lcc*2465,NA))))))))   
    
    #===================================================
    #soil compaction
    #===================================================
    
    values$eia.sc <- ifelse(input$bmptype.sc == "Urban Soil Restoration (Compacted Pervious Surfaces)",
                            ifelse(input$depth.sc == "15 Inches", input$acres.sc*0.40, 
                                   ifelse(input$depth.sc == "20 Inches",input$acres.sc*0.80,NA)),
                            ifelse(input$bmptype.sc == "Urban Soil Restoration (Removed Impervious Surfaces)",
                                   ifelse(input$depth.sc == "15 Inches", input$acres.sc*0.91, 
                                          ifelse(input$depth.sc == "20 Inches",input$acres.sc*1,NA)),NA))
    
    values$tnred.sc <- ifelse(input$bmptype.sc == "Urban Soil Restoration (Compacted Pervious Surfaces)",
                              ifelse(input$depth.sc == "15 Inches", input$acres.sc*4.4, 
                                     ifelse(input$depth.sc == "20 Inches",input$acres.sc*8.9,NA)),
                              ifelse(input$bmptype.sc == "Urban Soil Restoration (Removed Impervious Surfaces)",
                                     ifelse(input$depth.sc == "15 Inches", input$acres.sc*13.7, 
                                            ifelse(input$depth.sc == "20 Inches",input$acres.sc*15.0,NA)),NA))
    
    values$tpred.sc <- ifelse(input$bmptype.sc == "Urban Soil Restoration (Compacted Pervious Surfaces)",
                              ifelse(input$depth.sc == "15 Inches", input$acres.sc*0.72, 
                                     ifelse(input$depth.sc == "20 Inches",input$acres.sc*1.44,NA)),
                              ifelse(input$bmptype.sc == "Urban Soil Restoration (Removed Impervious Surfaces)",
                                     ifelse(input$depth.sc == "15 Inches", input$acres.sc*0.70, 
                                            ifelse(input$depth.sc == "20 Inches",input$acres.sc*0.77,NA)),NA))
    
    values$tssred.sc <- ifelse(input$bmptype.sc == "Urban Soil Restoration (Compacted Pervious Surfaces)",
                               ifelse(input$depth.sc == "15 Inches", input$acres.sc*278, 
                                      ifelse(input$depth.sc == "20 Inches",input$acres.sc*557,NA)),
                               ifelse(input$bmptype.sc == "Urban Soil Restoration (Removed Impervious Surfaces)",
                                      ifelse(input$depth.sc == "15 Inches", input$acres.sc*1696, 
                                             ifelse(input$depth.sc == "20 Inches",input$acres.sc*1864,NA)),NA))
    
    #===================================================
    #alternative surfaces
    #===================================================
    
    values$eia.as <- input$acresremoved.as
    values$tnred.as <- NA
    values$tpred.as <- NA
    values$tssred.as <- NA
    
    #===================================================
    # not structural BMP
    #===================================================
    
    values$eia.nsbmp <-  input$imperviousacres.nsbmp*(input$disconnection.nsbmp/100)
    values$tnred.nsbmp <- NA
    values$tpred.nsbmp <- NA
    values$tssred.nsbmp <- NA
    
  })
  
  #===================================================
  #structural BMP
  #===================================================
  
  output$eia.sbmp <- renderText({
    validate(need(!is.na(values$eia.sbmp), "Enter values to calculate EIA"))
    round(values$eia.sbmp,1)})
  output$tnred.sbmp <- renderText({
    validate(need(!is.na(values$tnred.sbmp), "Enter values to calculate TN Reduction"))
    round(values$tnred.sbmp,1)})
  output$tpred.sbmp <- renderText({
    validate(need(!is.na(values$tpred.sbmp), "Enter values to calculate TP Reduction"))
    round(values$tpred.sbmp,1)})
  output$tssred.sbmp <- renderText({
    validate(need(!is.na(values$tssred.sbmp), "Enter values to calculate TSS Reduction"))
    round(values$tssred.sbmp,1)})
  
  #===================================================
  #stream restoration
  #===================================================
  
  output$eia.stre <- renderText({
    validate(need(!is.na(values$eia.stre), "Enter values to calculate EIA"))
    round(values$eia.stre,1)})
  output$tnred.stre <- renderText({
    validate(need(!is.na(values$tnred.stre), "Enter values to calculate TN Reduction"))
    round(values$tnred.stre,1)})
  output$tpred.stre <- renderText({
    validate(need(!is.na(values$tpred.stre), "Enter values to calculate TP Reduction"))
    round(values$tpred.stre,1)})
  output$tssred.stre <- renderText({
    validate(need(!is.na(values$tssred.stre), "Enter values to calculate TSS Reduction"))
    round(values$tssred.stre,1)})
  
  #===================================================
  # outfalls stabilization or SPSC
  #===================================================
  
  output$eia.outspsc <- renderText({
    validate(need(!is.na(values$eia.outspsc), "Enter values to calculate EIA"))
    round(values$eia.outspsc,1)})
  output$tnred.outspsc <- renderText({
    validate(need(!is.na(values$tnred.outspsc), "Enter values to calculate TN Reduction"))
    round(values$tnred.outspsc,1)})
  output$tpred.outspsc <- renderText({
    validate(need(!is.na(values$tpred.outspsc), "Enter values to calculate TP Reduction"))
    round(values$tpred.outspsc,1)})
  output$tssred.outspsc <- renderText({
    validate(need(!is.na(values$tssred.outspsc), "Enter values to calculate TSS Reduction"))
    round(values$tssred.outspsc,1)})
  
  #===================================================
  # shoreline stabilization
  #===================================================
  
  output$eia.shst <- renderText({
    validate(need(!is.na(values$eia.shst), "Enter values to calculate EIA"))
    round(values$eia.shst,1)})
  output$tnred.shst <- renderText({
    validate(need(!is.na(values$tnred.shst), "Enter values to calculate TN Reduction"))
    round(values$tnred.shst,1)})
  output$tpred.shst <- renderText({
    validate(need(!is.na(values$tpred.shst), "Enter values to calculate TP Reduction"))
    round(values$tpred.shst,1)})
  output$tssred.shst <- renderText({
    validate(need(!is.na(values$tssred.shst), "Enter values to calculate TSS Reduction"))
    round(values$tssred.shst,1)})
  
  #===================================================
  # land cover conversion
  #===================================================
  
  output$eia.lcc <- renderText({
    validate(need(!is.na(values$eia.lcc), "Enter values to calculate EIA"))
    round(values$eia.lcc,1)})
  output$eia.lcc.warning <- renderText({
    validate(need(values$eia.lcc>1.5, ""))
    "Are you sure? This value is very high :)"})
  output$tnred.lcc <- renderText({
    validate(need(!is.na(values$tnred.lcc), "Enter values to calculate TN Reduction"))
    round(values$tnred.lcc,1)})
  output$tpred.lcc <- renderText({
    validate(need(!is.na(values$tpred.lcc), "Enter values to calculate TP Reduction"))
    round(values$tpred.lcc,1)})
  output$tssred.lcc <- renderText({
    validate(need(!is.na(values$tssred.lcc), "Enter values to calculate TSS Reduction"))
    round(values$tssred.lcc,1)})
  
  #===================================================
  #soil compaction
  #===================================================
  
  output$eia.sc <- renderText({
    validate(need(!is.na(values$eia.sc), "Enter values to calculate EIA"))
    round(values$eia.sc,1)})
  output$tnred.sc <- renderText({
    validate(need(!is.na(values$tnred.sc), "Enter values to calculate TN Reduction"))
    round(values$tnred.sc,1)})
  output$tpred.sc <- renderText({
    validate(need(!is.na(values$tpred.sc), "Enter values to calculate TP Reduction"))
    round(values$tpred.sc,1)})
  output$tssred.sc <- renderText({
    validate(need(!is.na(values$tssred.sc), "Enter values to calculate TSS Reduction"))
    round(values$tssred.sc,1)})
  
  #===================================================
  # not structural BMP
  #===================================================
  
  output$eia.as <- renderText({
    validate(need(!is.na(values$eia.as), "Enter values to calculate EIA"))
    round(values$eia.as,1)})
  output$tnred.as <- renderText({
    validate(need(!is.na(values$tnred.as), "No TN Reduction associated with this practice"))
    round(values$tnred.as,1)})
  output$tpred.as <- renderText({
    validate(need(!is.na(values$tpred.as), "No TP Reduction associated with this practice"))
    round(values$tpred.as,1)})
  output$tssred.as <- renderText({
    validate(need(!is.na(values$tssred.as), "No TSS Reduction associated with this practice"))
    round(values$tssred.as,1)})
  
  #===================================================
  # alternative surfaces
  #===================================================
  
  output$eia.nsbmp <- renderText({
    validate(need(!is.na(values$eia.nsbmp), "Enter values to calculate EIA"))
    round(values$eia.nsbmp,1)})
  output$tnred.nsbmp <- renderText({
    validate(need(!is.na(values$tnred.nsbmp), "No TN Reduction associated with this practice"))
    round(values$tnred.nsbmp,1)})
  output$tpred.nsbmp <- renderText({
    validate(need(!is.na(values$tpred.nsbmp), "No TP Reduction associated with this practice"))
    round(values$tpred.nsbmp,1)})
  output$tssred.nsbmp <- renderText({
    validate(need(!is.na(values$tssred.nsbmp), "No TSS Reduction associated with this practice"))
    round(values$tssred.nsbmp,1)})
  
  
  #===================================================
  # Land River Segment Map
  #===================================================
  output$mymap <-renderLeaflet({
    leaflet() %>%
      setView(lng = -76.5, lat = 38.9, zoom = 13) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolygons(data = aacolrs, 
                  fill = TRUE,
                  color = "black", 
                  weight =1,
                  fillColor = wes_palette('Darjeeling1', type = c("continuous")),#"#b7f5b5", # color is function of the wbd AREA column
                  fillOpacity = 0.35,
                  popup = paste("Land River Segment: ", aacolrs$LandRiverS, "<br>",
                                "Total Nitrogen Delivery Factor: ", aacolrs$TotalNitro, "<br>",
                                "Total Phosphorus Delivery Factor: ", aacolrs$TotalPhosp, "<br>",
                                "Total Suspended Sediment Delivery Factor: ", aacolrs$SedimentDe, "<br>"))%>%
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-5) %>% 
      htmlwidgets::onRender(
        'function(el, x) {                        
                    var lat = document.getElementById("lat").value;
                    var lng = document.getElementById("lng").value;
                    this.panTo(new L.LatLng(lat, lng));
              }'
      )
  })
  
  #===================================================
  # Erosion Transect Map
  #===================================================
  
  output$mymap2 <-renderLeaflet({
    leaflet() %>%
      setView(lng = -76.5, lat = 38.9, zoom = 13) %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addPolylines(data = aashoretransects, 
                   fill = TRUE,
                   color = "red", 
                   weight =3.5,
                   fillColor = 'red',
                   fillOpacity = 0,
                   popup = paste("Annual Erosion Rate (ft/yr): ", round(aashoretransects$RecentRate,2)))%>%
      addMiniMap(
        tiles = providers$Stamen.TonerLite,
        position = 'topright', 
        width = 200, height = 200,
        toggleDisplay = FALSE,
        aimingRectOptions = list(color = "red", weight = 1, clickable = FALSE),
        zoomLevelOffset=-5)
  })
})