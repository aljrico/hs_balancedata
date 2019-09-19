#' @export
my_button <- function(inputId, label = NULL, icon = NULL, style = "unite",
                      color = "default", size = "md", block = FALSE,
                      no_outline = TRUE){
  
  value <- shiny::restoreInput(id = inputId, default = NULL)
  
  attach_dep <- function(tag){
    
    dep <- htmltools::htmlDependency(
      name = 'hs.balancedata',
      version = '0.1.3',
      src = c(href = "hs.balancedata"),
      stylesheet = "button-styles.css"
    )
    
    htmltools::attachDependencies(tag, dep, append = TRUE)
  }
  
  tag_button <- htmltools::tags$button(
    id = inputId, type = "button", class = "action-button bttn", `data-val` = value,
    class = 'btn left',
    class = 'slant-left',
    class = 'right title'
  )
  
  attach_dep(tag_button)
  
}