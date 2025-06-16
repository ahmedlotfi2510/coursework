#!/bin/bash

# reformat total counts in googlebooks-eng-all-totalcounts-20120701.txt to a valid csv
#   use tr, awk, or sed to convert tabs to newlines
#   write results to total_counts.csv


awk 'BEGIN { OFS = "\n" } { $1=$1; print }' googlebooks-eng-all-totalcounts-20120701.txt > total_counts.csv


