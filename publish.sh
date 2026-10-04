#!/bin/bash
git pull

quarto render

git add .

git commit -m "Updated Files"

git push