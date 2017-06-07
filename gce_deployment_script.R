## Deploy shiny app
## Taken from https://cloudyr.github.io/googleComputeEngineR/articles/shiny-app.html
## We'll need the DEV version of the library
devtools::install_github("cloudyr/googleComputeEngineR")
library(googleComputeEngineR)

## Auth, set project and zone pre-set
Sys.setenv("GCE_AUTH_FILE" = "/home/manos/Downloads/auth.json")
gce_global_project('YOUR-PROJECT-NAME')
gce_global_zone(zone = "YOUR-PROJECT-REGION")

gce_auth()

## Create an instance
vm <- gce_vm("myapp", 
             template = "shiny",
             predefined_type = "n1-standard-2")

## Add SSH keys
vm <- gce_ssh_setup(vm,
                    username = "manosp", 
                    key.pub = "/home/manos/.ssh/gcloud.pub",
                    key.private = "/home/manos/.ssh/gcloud")

## Let's build a docker image
build_folder <- "~/Documents/Github/CampaignPlanner"
gce_shiny_addapp(instance = vm, 
                 app_image = "gceshinycampaign", 
                 dockerfolder = build_folder)

vm2 <- gce_vm("production-shiny-app", 
              template = "shiny",
              predefined_type = "n1-standard-1")

## Again add SSH keys to the second GCE VM
vm2 <- gce_ssh_setup(vm2,
                    username = "manosp", 
                    key.pub = "/home/manos/.ssh/gcloud.pub",
                    key.private = "/home/manos/.ssh/gcloud")

# Deploy the docker image
gce_shiny_addapp(vm2, 
                 app_image = "gceshinycampaign")

## Tidy up
# Clean up the VMs to avoid unnecessary costs:
# Delete build VM
gce_vm_delete(vm)

# stop and start production shiny app as needed
# gce_vm_stop(vm2)
gce_shiny_listapps(vm2)
