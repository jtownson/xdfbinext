#!/bin/bash
sort -u src/main/resources/BMW-a2l-object-descriptions-en.csv > sorted.csv
mv sorted.csv src/main/resources/BMW-a2l-object-descriptions-en.csv

