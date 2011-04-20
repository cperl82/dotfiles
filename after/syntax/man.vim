syn region manTopLevelFold start='^\(NAME\|LIBRARY\|SYNOPSIS\|COPYRIGHT\|\%^\)\@![^[:space:]]' end='^$\n\([^[:space:]]\)\@=' fold contains=ALL
set fdm=syntax
