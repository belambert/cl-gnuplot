gnuplot
=======

Here, I've defined a number of Lisp functions that I've used to convert
basic data structures in Lisp into gnuplot source files.  This makes it
easier to plot data from Lisp.

Some of the supported graph types are:

 * :heatmap
 * :surface
 * :2dgraph
 * :line-graph
 * :scatter
 * :histogram
 * :thumbnail

The main function to call is this:

```lisp
(defun plot-graph (points graph-type &rest rest &key filename (title "Auto-generated graph")
		   x-label x2-label y-label y2-label cblabel z-label;; axis labels
		   x-range y-range x2-range y2-range ;; axis ranges
		   data-axes ;; which axes to plot each point on.
		   thumbnail ;; as a thumbnail?
		   legend-labels (key-vertical "top") (key-horizontal "left") ;; legend labels and location
		   (legend t)
		   (type '(:png)) ;; '(:png :svg))
		   (width 1280) (height 960) ;; type and size
		   (debug t) (verbose t)  ;; how much info to print and/or save.
		   #+darwin (encoding "utf8")
		   #-darwin (encoding "default")
		   regression
		   manual-options)
```
