library(shiny)
library(digest) # digest() Create hash function digests for R objects
library(shinyjs)
library(dplyr)
library(DT)

fieldsMandatory <- c("name", "observernum","idlevel","loopcompletion")

labelMandatory <- function(label) {
  tagList(
    label,
    span("*", class = "mandatory_star")
  )
}

appCSS <-
  ".mandatory_star { color: red; }
#error { color: red; }"

fieldsAll <- c("name", "observernum", "submittername","date1", "temp","sitecon","timestart","timeend","idlevel","loopcompletion","eastern_tiger_swallowtail", "spicebush_swallowtail",
               "pipevine_swallowtail", "black_swallowtail", "zebra_swallowtail", "unknown_swallowtail","cloudless_sulphur", "clouded_sulphur", "orange_sulphur", "sleepy_orange",
               "little_yellow", "unknown_sulphur", "cabbage_white", "falcate_orangetip", "checkered_white","unknown_white","harvester","unknown_harvester","eastern_tailed_blue", "summer_azure",
               "spring_azure","unknown_blue","grey_hairstreak", "red_banded_hairstreak", "juniper_hairstreak", "henrys_elfin","banded_hairstreak","great_purple_hairstreak",
               "white_m_hairstreak", "unknown_hairstreak","hackberry_emperor", "tawny_emperor","unknown_emperor","monarch","unknown_milkweed","great_spangeld_fritillary", "variegated_fritillary",
               "gulf_fritillary", "unknown_heliconian","unknown_fritillary","american_snout","unknown_snout",
               "red_spotted_purple","viceroy","unknown_admiral","pearl_crescent","common_buckeye", "red_admiral","question_mark","eastern_comma","silvery_checkerspot","mourning_cloak","american_lady",
               "painted_lady","unknown_brushfoot","carolian_satyr","gemmed_satyr","little_wood_satyr","common_wood_nymph","northern_pearly_eye","appalachian_brown","unknown_satyr","sachem", "clouded_skipper",
               "zabulon_skipper","crossline_skipper","little_glassywing","dun_skipper","least_skipper","southern_broken_dash","fiery_skipper","northern_broken_dash","ocola_skipper",
               "swarthy_skipper","common_roadside_skipper","delaware_skipper","duster_skipper","tawny_edged_skipper","dion_skipper","unknown_grass_skipper","silver_spotted_skipper",
               "juvenals_duskywing","horaces_duskywing","hoary_edge","northern_cloudywing","southern_cloudywing","sleepy_duskywing","long_tailed_skipper","wild_indigo_duskywing",
               "zarucco_duskywing","unknown_spread_wing_skipper","name_other_sp_1","number_other_sp_1","name_other_sp_2","number_other_sp_1")

responsesDir <- file.path("responses")
epochTime <- function() {
  as.integer(Sys.time())
}

humanTime <- function() format(Sys.time(), "%Y%m%d-%H%M%OS")

ui <- fluidPage(
  titlePanel(title=div(img(src='mfbp.logo.png'), br(),br(),
                                       "Form for recording butterfly observations at Mason Farm")),
  sidebarLayout(
    sidebarPanel("", uiOutput("info")),
    mainPanel(
      tabsetPanel(
        tabPanel("How to Use", uiOutput("howto")),
        tabPanel("Papilionidae", uiOutput("papilionidae")),
        tabPanel("Pieridae", uiOutput("pieridae")),
        tabPanel("Lycaenidae", uiOutput("lycaenidae")),
        tabPanel("Nymphalidae",uiOutput("nymphalidae")),
        tabPanel("Hesperiidae", uiOutput("hesperiidae")),
        tabPanel("Unknown", uiOutput("unknowns")),
        tabPanel("Other", uiOutput("writeins")))
    )
  ),
  
  shinyjs::hidden(
    div(
      id = "thankyou_msg",
      h3("Thanks, your response was submitted successfully!"),
      actionLink("submit_another", "Submit another response")
    )
  ),
  
  shinyjs::hidden(
    span(id = "submit_msg", "Submitting..."),
    div(id = "error",
        div(br(), tags$b("Error: "), span(id = "error_msg"))
    )
  ),
  
  shinyjs::useShinyjs(),
  
  shinyjs::inlineCSS(appCSS)
  
)

server <- function(input, output, session) {
  output$info<-renderUI({
    div(
      id = "form",
      textInput("name", labelMandatory("Observer name(s)"), ""),
      sliderInput("observernum", labelMandatory("Number of Observers"), 1, 20, 1, ticks = FALSE),
      textInput("submittername", "If you are entering these observations for someone else, what is your name?",""),
      dateInput("date1", "Date observed:", value = Sys.Date()-10, format = "yyyy-mm-dd"),
      textInput("temp", "Temperature (F) (optional)", ""),
      textInput("sitecon", "Site conditions (optional)", ""),
      textInput("timestart", "Start time (ex. 09:00AM) (optional)", ""),
      textInput("timeend", "End time (ex. 12:00PM) (optional)", ""),
      selectInput("idlevel", labelMandatory("How would you rate your experience level in identifying butterflies?"),
                  c("",  "Beginner (know a few common species)", "Proficient (know many common species)", "Expert (know many common and rare species)")),
      selectInput("loopcompletion", labelMandatory("Over how much of the loop did you record butterflies?"),
                  c("", "1/4", "1/2", "3/4", "Entire loop")),
      br(),
      actionButton("submit", "Submit", class = "btn-primary"))
  })
  
  output$howto<-renderUI({
    div(
      h2("How to use this form:"),
      p("If you filled out a checklist during your walk at Mason Farm Biological Reserve, input your observations using this form. Please record your name, the number of observers on your walk, your experience level identifying butterflies, and the amount of the survey loop you completed using the side panel."),
      br(),
      p("If you spotted an", span("unidentifiable butterfly", style="color:orange"), "please submit that observation on the", span("Unknown", style="color:blue"), "tab. If you spotted a ", span("species not on our checklist", style="color:orange"), "please submit that observation on the", span("Other", style="color:blue"), "tab."),
      br(),
      p("After you have reported all the butterflies you observed, press the", span("Submit", style="color:blue"), "button on the side panel."),
      br(),
      p("Thank you for participating!")
    )
  })
  
  output$papilionidae<-renderUI({
    div(
      h2("Swallowtails (Number seen)"),
      textInput("eastern_tiger_swallowtail", "Eastern Tiger Swallowtail", ""),
      textInput("spicebush_swallowtail", "Spicebush Swallowtail", ""),
      textInput("pipevine_swallowtail", "Pipevine Swallowtail", ""),
      textInput("blacks_wallowtail", "Black Swallowtail", ""),
      textInput("zebra_swallowtail", "Zebra Swallowtail", ""))
  })
  
  output$pieridae<-renderUI({
    div(
      h2("Sulphurs (Number seen)"),
      textInput("cloudless_sulphur", "Cloudless Sulphur", ""),
      textInput("clouded_sulphur", "Clouded Sulphur", ""),
      textInput("orange_sulphur", "Orange Sulphur", ""),
      textInput("sleepy_orange", "Sleepy Orange", ""),
      textInput("little_yellow", "Little Yellow", ""),
      textInput("unknown_sulphur","Unknown Sulphur", ""),
      h2("Whites (Number seen)"),
      textInput("cabbage_white", "Cabbage White", ""),
      textInput("falcate_orangetip", "Falcate Orangetip", ""),
      textInput("checkered_white", "Checkered White"))
  })
  
  output$lycaenidae<-renderUI({
    div(
      h2("Harvesters and Woolly Legs (Number seen)"),
      textInput("harvester", "Harvester", ""),
      h2("Blues (Number seen)"),
      textInput("eastern_tailed_blue", "Eastern Tailed-Blue", ""),
      textInput("summer_azure", "Summer Azure", ""),
      textInput("spring_azure", "Spring Azure", ""),
      h2("Hairstreaks (Number seen)"),
      textInput("grey_hairstreak", "Grey Hairstreak", ""),
      textInput("red_banded_hairstreak", "Red-banded Hairstreak", ""),
      textInput("juniper_hairstreak", "Juniper Hairstreak", ""),
      textInput("henrys_elfin", "Henry's Elfin", ""),
      textInput("banded_hairstreak", "Banded Hairstreak", ""),
      textInput("great_purple_hairstreak", "Great Purple Hairstreak", ""),
      textInput("white_m_hairstreak", "White-M Hairstreak", ""))
  })
  
  output$nymphalidae<-renderUI({
    div(
      h2("Emperors (Number seen)"),
      textInput("hackberry_emperor", "Hackberry Emperor", ""),
      textInput("tawny_emperor", "Tawny Emperor", ""),
      h2("Milkweed Butterflies (Number seen)"),
      textInput("monarch", "Monarch", ""),
      h2("Heliconians and Fritillaries (Number seen)"),
      textInput("great_spangled_fritillary", "Great-spangled Fritillary", ""),
      textInput("variegated_fritillary", "Variegated Fritillary", ""),
      textInput("gulf_fritillary", "Gulf Fritillary", ""),
      h2("Snout Butterflies (Number seen)"),
      textInput("american_snout", "American Snout", ""),
      h2("Admirals (Number seen"),
      textInput("red_spotted_purple", "Red-spotted Purple", ""),
      textInput("viceroy", "Viceroy", ""),
      h2("True Brush-foots (Number seen)"),
      textInput("pearl_crescent", "Pearl Crescent", ""),
      textInput("common_buckeye", "Common Buckeye", ""),
      textInput("red_admiral", "Red Admiral", ""),
      textInput("question_mark", "Question Mark", ""),
      textInput("eastern_comma", "Eastern Comma", ""),
      textInput("silvery_checkerspot", "Silvery Checkerspot", ""),
      textInput("mourning_cloak", "Mourning Cloak", ""),
      textInput("american_lady", "American Lady", ""),
      textInput("painted_lady", "Painted Lady", ""),
      h2("Satyrs (Number seen)"),
      textInput("carolina_satyr", "Carolina Satyr", ""),
      textInput("gemmed_satyr", "Gemmed Satyr", ""),
      textInput("little_wood_satyr", "Little Wood-Satyr", ""),
      textInput("common_wood_nymph", "Common Wood-nymph", ""),
      textInput("northern_pearly_eye", "Northern Pearly-eye", ""),
      textInput("appalachian_brown", "Appalachian Brown",""))
  })
  
  output$hesperiidae<-renderUI({
    div(
      h2("Grass Skippers (Number seen)"),
      textInput("sachem", "Sachem", ""),
      textInput("clouded_skipper", "Clouded Skipper", ""),
      textInput("zabulon_skipper", "Zabulon Skipper", ""),
      textInput("crossline_skipper", "Crossline Skipper", ""),
      textInput("little_glasswing", "Little Glassywing", ""),
      textInput("dun_skipper","Dun Skipper", ""),
      textInput("least_skipper", "Least Skipper", ""),
      textInput("southern_broken_dash", "Southern Broken-Dash", ""),
      textInput("fiery_skipper", "Fiery Skipper", ""),
      textInput("northern_broken_dash", "Northern Broken-Dash", ""),
      textInput("ocola_skipper", "Ocola Skipper", ""),
      textInput("swarthy_skipper", "Swarthy Skipper", ""),
      textInput("common_roadside_skipper", "Common Roadside-Skipper", ""),
      textInput("delaware_skipper", "Delaware Skipper", ""),
      textInput("dusted_skipper", "Dusted Skipper", ""),
      textInput("tawny_edged_skipper", "Tawny-edged Skipper", ""),
      textInput("dion_skipper", "Dion Skipper", ""),
      h2("Spread-wing Skippers (Number seen)"),
      textInput("silver_spotted_skipper", "Silver-spotted Skipper", ""),
      textInput("juvenals_duskywing", "Juvenal's Duskywing", ""),
      textInput("horaces_duskywing", "Horace's Duskywing", ""),
      textInput("hoary_edge", "Hoary Edge", ""),
      textInput("northern_cloudywing", "Northern Cloudywing", ""),
      textInput("southern_cloudywing", "Sourthern Cloudywing", ""),
      textInput("common_checkered_skipper", "Common Checkered-Skipper", ""),
      textInput("sleepy_duskywing", "Sleepy Duskywing", ""),
      textInput("long_tailed_skipper", "Long-tailed Skipper", ""),
      textInput("wild_indigo_duskywing", "Wild Indigo Duskywing", ""),
      textInput("zarucco_duskywing", "Zarucco Duskywing", ""))
  })
  
  output$unknowns<-renderUI({
    div(
      h2("Unknowns (Number Seen)"),
      h3("Papilionidae"),
      h4("Swallowtails"),
      textInput("unknown_swallowtail", "Unknown Swallowtail", ""),
      br(),
      h3("Pieridae"),
      textInput("unknown_pieridae", "Unknown Pieridae"),
      h4("Sulphurs"),
      textInput("unknown_sulphur","Unknown Sulphur sp.",""),
      h4("Whites"),
      textInput("unknown_white","Unknown White sp.",""),
      br(),
      h3("Lycaenidae"),
      textInput("unknown_lycaenidae", "Unknown Lycaenidae"),
      h4("Harvesters"),
      textInput("unknown_harvester", "Unknown Harvester sp.",""),
      h4("Blues"),
      textInput("unknown_blue", "Unknown Blue",""),
      h4("Hairstreaks"),
      textInput("unknown_hairstreak", "Unknown Hairstreak sp.", ""),
      br(),
      h3("Nymphalidae"),
      textInput("unknown_nymphalidae", "Unknown Nymphalidae",""),
      h4("Emperors"),
      textInput("unknown_emperor", "Unknown Emperor sp.",""),
      h4("Milkweed Butterflies"),
      textInput("unknown_milkweed", "Unknown Milkweed Butterfly sp.", ""),
      h4("Heliconians and Fritillaries"),
      textInput("unknown_heliconian", "Unknown Heliconian sp.", ""),
      textInput("unknown_fritillary", "Unknown Fritillary sp.", ""),
      h4("Snouts"),
      textInput("unknown_snout", "Unknown Snout sp.", ""),
      h4("Admirals"),
      textInput("unknown_admiral", "Unknown Admiral sp.", ""),
      h4("Brush-foots"),
      textInput("unknown_brushfoot", "Unknown True Brush-foot sp.", ""),
      h4("Satyrs"),
      textInput("unknown_satyr","Unknown Satyr sp.",""),
      br(),
      h3("Hesperiidae"),
      textInput("unknown_he,speriidae", "Unknown Hesperiidae sp.",""),
      h4("Grass Skippers"),
      textInput("unknown_grass_skipper", "Unknown Grass skipper sp.", ""),
      h4("Spread-wing Skippers"),
      textInput("unknown_spread_wing_skipper", "Unknown Spread-wing Skipper sp.", "")
    )
  })
  
  output$writeins<-renderUI({
    div(
      h2("Write-ins"),
      textInput("name_other_sp_1", "Name of Other Sp. #1", ""),
      textInput("number_other_sp_1", "Number seen of Other Sp. #1", ""),
      textInput("name_other_sp_2", "Name of Other Sp. #2", ""),
      textInput("number_other_sp_2", "Number seen of Other Sp. #2", ""))
  })
  
  observe({
    # check if all mandatory fields have a value
    mandatoryFilled <-
      vapply(fieldsMandatory,
             function(x) {
               !is.null(input[[x]]) && input[[x]] != ""
             },
             logical(1))
    mandatoryFilled <- all(mandatoryFilled)
    
    # enable/disable the submit button
    shinyjs::toggleState(id = "submit", condition = mandatoryFilled)
    
    
  })
  
  formData <- reactive({
    data <- sapply(fieldsAll, function(x) input[[x]])
    data <- c(data, timestamp = epochTime())
    data <- t(data)
    data
  })
  
  saveData <- function(data) {
    fileName <- sprintf("%s_%s.csv",
                        humanTime(),
                        digest::digest(data))
    
    write.csv(x = data, file = file.path(responsesDir, fileName),
              row.names = FALSE, quote = TRUE)
  }
  
  # action to take when submit button is pressed
  observeEvent(input$submit, {
    shinyjs::disable("submit")
    shinyjs::show("submit_msg")
    shinyjs::hide("error")
    
    tryCatch({
      saveData(formData())
      shinyjs::reset("form")
      shinyjs::hide("form")
      shinyjs::show("thankyou_msg")
    },
    error = function(err) {
      shinyjs::html("error_msg", err$message)
      shinyjs::show(id = "error", anim = TRUE, animType = "fade")
    },
    finally = {
      shinyjs::enable("submit")
      shinyjs::hide("submit_msg")
    })
  })
  
  observeEvent(input$submit_another, {
    shinyjs::show("form")
    shinyjs::hide("thankyou_msg")
  }) 
}

shinyApp(ui,server)
