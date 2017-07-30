#!/bin/bash

source activate dashboard

R -e "rmarkdown::render(\"dashboard.Rmd\")"

# Add a auto-refresh meta tag to the html

sed -i -e 's/<head>/<head>\n<meta http-equiv=\"refresh\" content=\"60\">/g' dashboard.html


