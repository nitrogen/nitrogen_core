#!/bin/sh

if [ $# -ne 1 ]; then
echo "Usage: $0 <handler name>"
    exit 1
fi

HANDLER="$1"


# Check if source directory exists
if [ -d "$HANDLER" ]; then
    echo "Error: Handler $HANDLER already exists"
    exit 1
fi


mkdir "$HANDLER"

cp prototype/example_handler.erl.prototype "$HANDLER/${HANDLER}_handler.erl"
cp prototype/default_example_handler.erl.prototype "$HANDLER/default_${HANDLER}_handler.erl"

find $HANDLER -type f -exec sed -i "s/EXAMPLE/$HANDLER/g" {} +

echo "Done! New handler established in $HANDLER/"
