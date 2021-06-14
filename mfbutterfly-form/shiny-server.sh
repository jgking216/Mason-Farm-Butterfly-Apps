#!/bin/bash
## Set up an alternate passwd file using libnss_wrapper since the Shiny server
## requires a named user to work and OpenShift docker images get a random uid
## with no username attached to it. The envsubst command from gettext package
## is used to replace environment variables in the template.
export USER_ID=$(id -u)
export GROUP_ID=$(id -g)
envsubst < /tmp/passwd.template > /tmp/passwd
## Run the existing shiny script, output is captured to stdout
exec shiny-server