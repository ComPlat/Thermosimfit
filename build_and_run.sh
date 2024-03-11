#!/bin/bash
sudo systemctl stop shiny-server
var=`docker container ls  | grep 'tsf' | awk '{print $1}'`
docker stop $var
docker build -t tsf .
docker run --net=host --rm -p 3838:3838 tsf 

