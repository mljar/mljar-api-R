submit_prdict_job <- function(project_hid, dataset_hid, result_hid){
  
  data <- list(project_hid = projecthid,
               fname = fname)
  'predict_params' : data.frame(project_id =  project_hid,
                                project_hardware = 'cloud',
                                algorithms_ids = list(result_hid),
                                'dataset_id': dataset_hid,
                                'cv_models':1})
}