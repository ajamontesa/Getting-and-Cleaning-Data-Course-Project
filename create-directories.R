library(tidyverse)

for(f in c("Scripts", "Data", "Output")) {
    if(!dir.exists(f)) {
        dir.create(f)
    }
}

