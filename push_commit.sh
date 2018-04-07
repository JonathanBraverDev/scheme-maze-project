#!/usr/bin/bash

git status
git add .
git commit -am "$1"
git push origin master
