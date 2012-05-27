#!/bin/bash
while getopts ":d:e:p:" opt; do
    case $opt in
	d)
	    D="$OPTARG"
	    ;;
	e)
	    E="$OPTARG"
	    ;;
	p)
	    P="$OPTARG"
	    ;;
	:)
	    echo "Option - $OPTARG requires an argument."
	    exit 1
	    ;;
    esac
done
curl --data data=$D --data event=$E --data path=$P http://localhost:8080/event/push