#!/bin/sh

ROOT=$(pwd)

OPTIMIZE='magick mogrify -type PaletteAlpha -sample 400%'

cd public/static/img/Items
$OPTIMIZE '*.png'
cd "$ROOT"

cd public/static/img/Crops
for dir in */
do
    cd "$dir"
    $OPTIMIZE '*.png'
    cd ../
done
