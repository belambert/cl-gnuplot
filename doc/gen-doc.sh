#!/bin/sh

todos.sh
fixme.sh

echo "Generating Lisp documentation..."
mkdir -p ./lisp
lispdoc.sh ../src ./lisp/ gnuplot
cd lisp
wkhtmltopdf index.html [a-hm-v]*.html ../gnuplot-doc.pdf
cd -

# echo "Generating Python documentation..."
# mkdir -p ./python
# cd ./python
# pydoc -w ../../python/
# cd ..

