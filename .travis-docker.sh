#!/bin/bash
set -eux
apt-get install -y apt-transport-https ca-certificates curl gnupg-agent software-properties-common
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | apt-key add -
add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
apt-cache policy docker-ce
apt-get upgrade docker-ce
cat /etc/docker/daemon.json > /tmp/daemon.json
sed 's/{/{"experimental": true,/' < /tmp/daemon.json > /etc/docker/daemon.json
cat /etc/docker/daemon.json
sudo systemctl restart docker
