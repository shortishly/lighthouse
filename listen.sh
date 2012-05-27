#!/bin/sh
while getopts ":e:p:" opt; do
    case $opt in
	e)
	    LAST_EVENT_ID="$OPTARG"
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

if [ -z "$LAST_EVENT_ID" -a "${LAST_EVENT_ID+xxx}" = "xxx" ]; then
    curl http://localhost:8080/es/$P;
else
    curl --header "Last-Event-ID: ${LAST_EVENT_ID}" http://localhost:8080/es/$P;
fi