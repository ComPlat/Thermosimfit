#!/bin/bash	
docker build -t tsf .
docker tag tsf konradkraemer/tsf:latest
docker push konradkraemer/tsf:latest

# pull in 193.196.36.224
# docker run -p 3838:3838 konradkraemer/tsf:latest  

# 193.196.36.224:3838