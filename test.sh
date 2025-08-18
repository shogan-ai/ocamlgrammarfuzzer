#!/bin/sh
dune exec bin/main.exe -- \
--avoid METAOCAML_ESCAPE \
--avoid METAOCAML_BRACKET_OPEN \
--avoid METAOCAML_BRACKET_CLOSE \
--weight '1000 list(attribute):' \
--weight '1000 list(post_item_attribute):' \
--weight '0.01 floating_attribute' "$@"
