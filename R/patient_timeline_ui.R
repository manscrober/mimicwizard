patientTimelineUI <- function(id) {
  ns <- NS(id)
  div(
    style = "display:flex; flex-direction:column;width:100%;padding:20px;",
    uiOutput(ns("select_patient")),
    uiOutput(ns("patient_summary")),
    div(
      class = "ui segment",
      div(class = "ui header", "Complete Patient Timeline - All Data"),
      uiOutput(ns("timeline_content"))
    )
  )
}
