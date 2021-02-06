#!/bin/bash

cp /opt/workspace/apps/sarkari-portal/server/target/universal/server-0.1.0-SNAPSHOT.zip /home/pawan/workspace/
cd /home/pawan/workspace
unzip -a server-0.1.0-SNAPSHOT.zip
chown -R pawan:pawan /home/pawan/workspace
