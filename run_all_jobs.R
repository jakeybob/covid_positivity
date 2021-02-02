# run get_data script as background job. Check if still running, and when done run the other 
# scripts as background jobs (as they depend on get_data.R running first)

get_data_job_id <- rstudioapi::jobRunScript("get_data.R")
while(.rs.invokeRpc("get_jobs")[[get_data_job_id]]$completed == 0){
  Sys.sleep(10) # check status every ten seconds
  }

# run jobs in parallel
# generate_plots_job_id <- rstudioapi::jobRunScript("generate_plots.R")
# generate_maps_job_id <- rstudioapi::jobRunScript("generate_maps.R")
# generate_phase_anim_job_id <- rstudioapi::jobRunScript("generate_phase_anim.R")
# 
# while(.rs.invokeRpc("get_jobs")[[generate_plots_job_id]]$completed == 0 |
#       .rs.invokeRpc("get_jobs")[[generate_maps_job_id]]$completed == 0 |
#       .rs.invokeRpc("get_jobs")[[generate_phase_anim_job_id]]$completed == 0
#       ){
#   Sys.sleep(10) # check status every ten seconds
# }

# run jobs sequentially
generate_plots_job_id <- rstudioapi::jobRunScript("generate_plots.R")
while(.rs.invokeRpc("get_jobs")[[generate_plots_job_id]]$completed == 0){
  Sys.sleep(10) # check status every ten seconds
}

generate_maps_job_id <- rstudioapi::jobRunScript("generate_maps.R")
while(.rs.invokeRpc("get_jobs")[[generate_maps_job_id]]$completed == 0){
  Sys.sleep(10) # check status every ten seconds
}

generate_phase_anim_job_id <- rstudioapi::jobRunScript("generate_phase_anim.R")
while(.rs.invokeRpc("get_jobs")[[generate_phase_anim_job_id]]$completed == 0){
  Sys.sleep(10) # check status every ten seconds
}

rm(list = ls())
gc()

# commit and push (note won't add any new files to repo)
system(paste0("git commit -am  \"", Sys.Date(), " updates", "\""))
system("git push origin master")
system("git gc")
