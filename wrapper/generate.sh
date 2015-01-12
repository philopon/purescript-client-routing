#!/bin/bash

OUT=src/Network/Routing/Client/Foreign.purs

cat <<EOC > $OUT
module Network.Routing.Client.Foreign (Director(), director) where

foreign import data Director :: *

foreign import director """
var director = (function(exports){
EOC

cat bower_components/director/build/director.js >> $OUT

cat <<EOC >> $OUT

return exports.Router;
}({}));""" :: Director
EOC
