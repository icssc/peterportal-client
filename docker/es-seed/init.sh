#!/bin/bash

sleep 20

COURSES=$(curl es:9200/courses/_count) # check if course index exists
PROFESSORS=$(curl es:9200/professors/_count) # check if professor index exists
MISSING='index_not_found_exception' # string to look for if doesnt exist

# index data if doesnt exist
if [[ "$COURSES" == *"$MISSING"* ]]
then
    echo "Courses index not found!"
    curl -L https://drive.google.com/uc?id=1eostVGdUhqZcd02RgqVV2EiazUeQPufQ > courses.json
    curl -H 'Content-Type: application/x-ndjson' -XPOST 'es:9200/courses/_bulk?pretty' --data-binary @courses.json
else
    echo "Courses index already exists."
fi

# index data if doesnt exist
if [[ "$PROFESSORS" == *"$MISSING"* ]]
then
    echo "Professors index not found!"
    curl -L https://drive.google.com/uc?id=1tN5OzrhaZiwgElkOrT9URMog8YVLRbbO > professors.json
    curl -H 'Content-Type: application/x-ndjson' -XPOST 'es:9200/professors/_bulk?pretty' --data-binary @professors.json
else
    echo "Professors index already exists."
fi