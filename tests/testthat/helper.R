library(dittodb)
library(httptest)
httptest::.mockPaths("mock_reqs/")
Sys.setenv("AWS_DEFAULT_REGION" = "us-east-2")
