#!/bin/bash

sshport=22
srcdev="pi@raspi6"
srcpath="/home/pi/RPi_Lazarus/*"
destpath="/home/$USER/Seafile/RPi_Lazarus"

# synchronisieren des html-Verzeichnisses
rsync -e "ssh -p ${sshport}" -rtlv --delete ${srcdev}:${srcpath} ${destpath}

sleep 2
