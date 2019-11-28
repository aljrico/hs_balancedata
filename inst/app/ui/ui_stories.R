ui_stories <- function(){
  list(
    uiOutput("ui_pick_economy_document_stories"),
    checkboxInput("checkboxInput.stories.prod", "Update Prod Data", value = FALSE),
    checkboxInput("checkboxInput.stories.localisation", "Update Quest Localisation Data", value = FALSE)
  )
}