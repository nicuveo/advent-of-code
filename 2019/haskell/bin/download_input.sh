#! /usr/bin/env bash

set -e

if [ $# -ne 2 ]; then
    echo "usage: download_input year day"
    exit 1
fi

base=$(dirname $0)
year=$1
day=$2
file=$(printf "%02d.in" $day)

source $base/../dat/cookie.dat

cd $base/../../../

curl "https://adventofcode.com/$year/day/$day/input" -H "Cookie: session=$cookie" > $year/input/$file 2> /dev/null
echo "Downloaded $year/input/$file"
