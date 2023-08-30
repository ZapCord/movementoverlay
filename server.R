## Tony Johnson
## Last Modified: 8/29/2023
## UCHealth Motion Capture Lab

## Max upload size for files is 3GB
options(shiny.maxRequestSize=3000*1024^2)
shinyOptions(cache = cachem::cache_mem(max_size = 3000e6))

## libraries
library(dplyr)
library(av)
library(tidyr)
library(stringr)
library(data.table)
library(ggplot2)
library(jpeg)

## functions
## vectorize assign, get and exists for convenience
assign_hash <- Vectorize(assign, vectorize.args = c("x", "value"))
get_hash <- Vectorize(get, vectorize.args = "x")
exists_hash <- Vectorize(exists, vectorize.args = "x")

## server
server<-function(input,output,session){
  session$cache <- cachem::cache_mem(max_size = 1000e6)

  observeEvent(input$refresh,{shinyjs::js$refresh()})
  hash = new.env(hash = TRUE, parent = emptyenv())
  output$fileinputpanel<-renderPrint({ df<-rbind(input$existing_video_input,
                                                 input$existing_data_input)
                                      df
                        })

  startinginput<-observe({
    req(input$existing_video_input)

    ## find current working directory and see if temp video storage exists
    ## if it does not exist, create it. If it does exist, delete it and recreate.
    workingdir<-getwd()
    print(workingdir)
    if(dir.exists(file.path(paste(workingdir,"/temp_video",sep="")))){
      unlink("temp_video", recursive = TRUE)
      dir.create("temp_video")
    } else{
      dir.create("temp_video")
    }
    av_video_images(toString(input$existing_video_input[4]), destdir = paste(workingdir,"/temp_video",sep=""), format = "jpg")
    images<-list.files(path="temp_video")

    ## update the slider for time based on the number of frames in the video
    updateSliderInput(session,"slider_time",max=length(images))
    

  })

  # Show the image from the video reactive based on slider time.
  img<-reactive({
      req(input$existing_video_input,input$dimension)
      images<-list.files(path="temp_video")
      ## render the image based on where the slider time is.
      pull_frame<-images[input$slider_time]
      filename <- normalizePath(file.path(paste(getwd(),"/temp_video",sep=""),
                                          pull_frame))
      image_dim <- dim(readJPEG(filename))
      image_ratio <- image_dim[2]/image_dim[1]
      ## Return a list containing the file name and alt text
      list(src = filename,
           width = round(input$dimension[1]*0.5),
           height = round(input$dimension[1]*0.5/image_ratio),
           alt = paste("Image number", pull_frame))

  }) %>% bindCache(input$slider_time, input$dimension)
  
  output$image<-renderImage({
    req(input$slider_time)
    #unlist(keys()[as.character(input$slider_time)])
    img()
  }, deleteFile = FALSE) 

  ## grab the file path for the uploaded csv
  filename<-reactive({
    req(input$existing_data_input)
    input$existing_data_input[4]
    })

  ## read the uploaded csv at the file path.
  exist_data<-reactive({
    req(filename())
    fread(toString(filename()))
  })
  
  ## grab the file path for the uploaded csv
  timingfilename<-reactive({
    req(input$existing_timings_input)
    input$existing_timings_input[4]
  })
  
  ## read the uploaded csv at the file path.
  exist_timing<-reactive({
    req(timingfilename())
    fread(toString(timingfilename()))
  })
  
  change_names<-observe({
    req(is.null(exist_data())==FALSE)
    ## Find the available graphs from the csv
    column_names<-colnames(exist_data())
    select_names<-str_split(column_names,"[LR]")
    change_names<-c()
    for(i in 1:length(select_names)){
      change_names<-c(change_names,select_names[[i]][2])
    }

    ## update the drop down to reflect the available graphs
    change_names<-unique(change_names)
    updateSelectInput(session,"graph_name", choices = change_names[seq(1,length(change_names)-3)])
  })

  ## create a data table where a time vector in frames,
  ## along with the left and right kinematic/kinetic data is stored
  ## for graphing
  df<-reactive({
    
    req(input$graph_name!="plac")
    data <- exist_data()
    leftname<-paste0("L",input$graph_name, collapse="")
    rightname<-paste0("R",input$graph_name, collapse="")
    timevec <- seq_len(nrow(data %>% select(leftname)))
    
    data<-cbind(timevec, data %>% select(leftname, rightname))
    colnames(data) <- c("Time", leftname, rightname)
    
    longData<-as.data.table(melt(data, id=c("Time"),na.rm=FALSE),na.rm=FALSE)
    setnames(longData,names(longData)[2:3],c("Variable","Value"))

  }) %>% bindCache(input$graph_name)

  ## render the data table created above
  output$kinematics_csv<-renderDataTable({
    df()
  })
  
  ## graph kinematics/kinetics 
  ## without the vertical line that says where the frame is
  gg<-reactive({
      #req(input$existing_video_input)
      data<-df()
      
      
      if(input$graph_name=="PelvisAnglesX"){
        ylabel = expression("Post"~'('*degree*')'~"Ant")
        title = "Pelvic Tilt"
      } else if(input$graph_name=="PelvisAnglesY"){
        ylabel = expression("Down"~'('*degree*')'~"Up")
        title = "Pelvic Obliquity"
      } else if(input$graph_name=="PelvisAnglesZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Pelvic Rotation"
      } else if(input$graph_name=="KneeAnglesX"){
        ylabel = expression("Ext"~'('*degree*')'~"Flex")
        title = "Knee Flexion/Extension"
      } else if(input$graph_name=="KneeAnglesY"){
        ylabel = expression("Val"~'('*degree*')'~"Var")
        title = "Knee Varus/Valgus"
      } else if(input$graph_name=="KneeAnglesZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Knee/Tibia Rotation"
      } else if(input$graph_name=="HipAnglesX"){
        ylabel = expression("Ext"~'('*degree*')'~"Flex")
        title = "Hip Flexion/Extension"
      } else if(input$graph_name=="HipAnglesY"){
        ylabel = expression("Abd"~'('*degree*')'~"Add")
        title = "Hip Abduction/Adduction"
      } else if(input$graph_name=="HipAnglesZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Hip/Femoral Rotation"
      }else if(input$graph_name=="AnkleAnglesX"){
        ylabel = expression("Plan"~'('*degree*')'~"Dors")
        title = "Ankle Plantarflexion/Dorsiflexion"
      } else if(input$graph_name=="AnkleAnglesY"){
        ylabel = expression("Abd"~'('*degree*')'~"Add")
        title = "Ankle Abduction/Adduction"
      } else if(input$graph_name=="AnkleAnglesZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Ankle Rotation"
      } else if(input$graph_name=="FootProgressionAnglesZ"){
        ylabel = expression("Ext"~'('*degree*')'~"Int")
        title = "Foot Progression"
      } else if(input$graph_name=="HipMomentX"){
        ylabel = expression("Ext (Nm/kg) Flex")
        title = "Hip Flexion/Extension Moment"
      } else if(input$graph_name=="HipMomentY"){
        ylabel = expression("Abd (Nm/kg) Add")
        title = "Hip Abduction/Adduction Moment"
      } else if(input$graph_name=="HipMomentZ"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Hip Rotation Moment"
      } else if(input$graph_name=="KneeMomentX"){
        ylabel = expression("Ext (Nm/kg) Flex")
        title = "Knee Flexion/Extension Moment"
      } else if(input$graph_name=="KneeMomentY"){
        ylabel = expression("Valg (Nm/kg) Var")
        title = "Knee Varus/Valgus Moment"
      } else if(input$graph_name=="KneeMomentZ"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Knee Rotation Moment"
      } else if(input$graph_name=="AnkleMomentX"){
        ylabel = expression("Plan (Nm/kg) Dors")
        title = "Ankle Dorsiflexion/Plantarflexion Moment"
      } else if(input$graph_name=="AnkleMomentY"){
        ylabel = expression("Abd (Nm/kg) Add")
        title = "Ankle Abduction/Adduction Moment"
      } else if(input$graph_name=="AnkleMomentZ"){
        ylabel = expression("Ext (Nm/kg) Int")
        title = "Foot Rotation Moment"
      } else if(input$graph_name=="AnklePower"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Ankle Power"
      } else if(input$graph_name=="KneePower"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Knee Power"
      } else if(input$graph_name=="HipPower"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Total Hip Power"
      } else if(input$graph_name=="AnklePowerZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Ankle Power"
      } else if(input$graph_name=="KneePowerZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Knee Power"
      } else if(input$graph_name=="HipPowerZ"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Rotational Hip Power"
      } else if(input$graph_name=="AnklePowerX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Dorsiflexion/Plantarflexion Ankle Power"
      } else if(input$graph_name=="KneePowerX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Flexion/Extension Knee Power"
      } else if(input$graph_name=="HipPowerX"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Flexion/Extension Hip Power"
      } else if(input$graph_name=="AnklePowerY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Abduction/Adduction Ankle Power"
      } else if(input$graph_name=="KneePowerY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Varus/Valgus Knee Power"
      } else if(input$graph_name=="HipPowerY"){
        ylabel = expression("Abs (W/kg) Gen")
        title = "Abduction/Adduction Hip Power"
      } else{
        ylabel = "Variable"
        title = "Variable vs Time"
      }


      ggplot(data=df(), aes(x=Time, y=Value, group=Variable, color=Variable)) +
        geom_line()+
        scale_color_manual(values=c('#DC143C','#14C108'))+
        geom_hline(yintercept=0, color="black")+
        labs(x = "Time (Frames)", y = ylabel, title = title)+ theme_bw()
  })
  
  ## find the start and end from the csv file
  startend<-reactive({
    req(exist_timing(),input$ratio)
    timing <- exist_timing()
    start <- timing$TrialStart
    end <- timing$TrialEnd
    c(start,end)
    
  })
  
  ## find the model frame based on ratio and slider time
  frame<-reactive({
    req(input$slider_time,input$ratio)
    input$slider_time*input$ratio
  })
  
  ## plot the kinematics with the vertical line that says where someone is
  ggg<-reactive({
    req(gg(), frame(), startend())
    startendvec<-startend()
    g<-gg() + geom_vline(xintercept=frame())
    g
  })
  
  output$plot<-renderPlot({
    ggg()
  }) %>% bindCache(input$graph_name,frame())
  
  ## show the info for max and min
  output$info <- renderPrint({
    req(input$graph_name!="plac")
    if(!is.null(input$plot_brush)){
      newdf<-brushedPoints(df()
               , input$plot_brush, xvar = "Time", yvar = "Value")
      newdf<-cbind(c("Max","Min"),rbind(newdf[which.max(newdf$Value)],newdf[which.min(newdf$Value)]))
      names(newdf)<-c("Type","Time","Variable","Value")
      newdf
    } else{
      nearPoints(df()
               , input$plot_click, xvar = "Time", yvar = "Value", threshold = 10, maxpoints = 1)
    }
    
  })



}
