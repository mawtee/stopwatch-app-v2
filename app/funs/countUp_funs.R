countUp <- function(title, value, color = "black"){
  list(title = title, value = value, color = color)
}

countUpOutput <- function(id){
  el <- shiny::tags$div(
    id = id, class = "boxxy",
    h1(id = sprintf("%s-boxxy-value", id), class = "boxxy-value"),
    p(id = sprintf("%s-boxxy-title", id), class = "boxxy-title")
  )
  
  path <- normalizePath("js-assets/countUp")
  
  deps <- list(
    htmltools::htmlDependency(
      name = "boxxy",
      version = "1.0.0",
      src = c(file = path),
      script = c("countup.js", "countup_binding.js"),
      stylesheet = "styles.css"
    )
  )
  
  htmltools::attachDependencies(el, deps)
}

renderCountUp <- function(expr, env = parent.frame(), 
                        quoted = FALSE) {
  # Convert the expression + environment into a function
  func <- shiny::exprToFunction(expr, env, quoted)
  
  function(){
    func()
  }
}
