#!/bin/bash

sshport=22
destdev="pi@raspi6"
destpath="/home/pi/RPi_Lazarus"
srcpath="/home/$USER/Seafile/RPi_Lazarus/*"

rsync -e "ssh -p ${sshport}" -rtlv ${srcpath} ${destdev}:${destpath}

sleep 2

# zur Sicherheit nach jedem Aufruf die Berechtigung aufheben
chmod -x $0