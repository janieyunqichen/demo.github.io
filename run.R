
# -------------------------------------------------------------------------
# to produce HTML and .pdf
quarto::quarto_render("quarto documents")

# -------------------------------------------------------------------------
# to produce HTML
# quarto::quarto_render("quarto documents", output_format = "html")

# to produce PDF
# quarto::quarto_render("quarto documents", output_format = "pdf")

# ```{r}
# #| label: fig-yes-returns
# #| fig-cap: Number of YES survey returns by setting


# -------------------------------------------------------------------------
# Publish to RStudio Connect
# -------------------------------------------------------------------------

# Internal Draft
app_name <- "unmet_need_draft" 
app_id <- 109
hpa.tools::quarto_pub(
  dir = "report_main",
  name = app_name,
  appId = app_id
)

# Production version
app_name = "unmet_need" 
app_id <- 110
hpa.tools::quarto_pub(
  dir = "report_main",
  name = app_name,
  appId = app_id
)


# -------------------------------------------------------------------------
# Run the following if publishing this App for the first time
# -------------------------------------------------------------------------
# Internal draft
# app_name <-  "unmet_need_draft"
# hpa.tools::quarto_pub(
#   dir = "report_main",
#   name = app_name,
#   server = "rsc.healthpolicy.com.au",
#   account = hpa.tools::rsc_account()
# )
# # Run this line and hard code the result to the app_id variable so it will work for collaborators
# rsconnect::applications() |> dplyr::filter(name==app_name) |> dplyr::pull(id)

# Production version
# app_name <-  "unmet_need" 
# hpa.tools::quarto_pub(
#   dir = "report_main",
#   name = app_name,
#   server = "rsc.healthpolicy.com.au",
#   account = hpa.tools::rsc_account(),
# )
# # Run this line and hard code the result to the app_id variable so it will work for collaborators
# rsconnect::applications() |> dplyr::filter(name==app_name) |> dplyr::pull(id)
