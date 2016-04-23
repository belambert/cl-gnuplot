;;; Copyright Ben. Lambert
;;; ben@benjaminlambert.com

(declaim (optimize (debug 3)))
(in-package :gnuplot)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  Interface with OS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *environment-vars* nil
  "A list of environment variables that I copied from bash, to be used for gnuplot.")

;; Assign this separately b/c o/w it messed up the lispdoc
(setf *environment-vars*  '("GDFONTPATH=/Users/bel/Library/Fonts:/Library/Fonts:/System/Library/Fonts:/Library/Fonts/Microsoft:/home/belamber/Fonts:/home/belamber/Fonts/Microsoft" 
			    "GNUPLOT_DEFAULT_GDFONT=Lucida Sans Unicode"
			    "PATH=/home/belamber/software/bin:/opt/local/bin:/opt/local/sbin:/usr/local/bin:/sw/bin:/usr/X11R6/bin:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:/usr/X11/bin:/bin:/Users/bel/Applications/bin:/afs/cs.cmu.edu/user/belamber/software/bin"))

(defun run-gnuplot (input-stream &key (verbose t))
  "Run the acutal 'gnuplot' executable -- this shouldn't be called by users."
  (let ((output-stream (if verbose *standard-output* nil)))
    #+darwin      ;; It needs the absolute path for some reason... on Mac.
    (sb-ext:run-program "/opt/local/bin/gnuplot" '() :input input-stream :output output-stream :error output-stream :wait t :search t :environment *environment-vars*)
    #-darwin
    (sb-ext:run-program "gnuplot" '() :input input-stream :output output-stream :error output-stream :wait t :search t :environment *environment-vars*)))

(defun gnuplot-plot (string &key filename verbose debug)
  "Calls this function above after turning the given string into an input stream.  If 'debug' is true, then
   it also save the string to a file."
  (with-input-from-string (stream string)
    (run-gnuplot stream :verbose verbose))
  (when debug
    (with-open-file (f filename :direction :output :if-exists :supersede)
      (princ string f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Helper functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun print-gnuplot-grid-to-file (points file)
  "Print the gievn points to a gnuplot data file."
  (with-open-file (f file :direction :output :if-exists :supersede)
    (print-gnuplot-grid-to-stream points f)))

(defun print-gnuplot-grid-to-stream (points stream &key (sort t) (n 1))
  "A helper function that prints a 'grid' of n-d points to the stream."
  (setf points (copy-seq points))
  (when sort
    (setf points (sort points '< :key 'first)))
  (dotimes (i n)
    (loop for p in points do
	 (format stream "~{~f ~}~%" p))
    (when (> n 1)
      (format stream "EOF~%"))))

(defun thumbnail (stream &key type (width 32) (height 20))
  "Print the gnuplot commands needed to make a thumbnail to the given stream."
  ;; This is to compensate for the cropping that we do...
  (incf width 40)
  (incf height 27)
  (format stream "unset xtics~%")
  (format stream "unset ytics~%")
  (format stream "unset key~%")
  (format stream "unset title~%")
  (format stream "unset xlabel~%")
  (format stream "unset ylabel~%")
  (format stream "set style data dots~%")
  (format stream "set terminal ~A size ~d,~d" (string-downcase (string type)) width height)
  (if (or (eq type :x11) (eq type :aqua))
      (format stream "~%")
      (format stream " crop~%")))
    
(defun heatmap (stream &key label thumbnail type)
  "Add the commands to turn the graph into a heat map."
  (format stream "unset key~%")
  (format stream "set style data image~%")
  (format stream "set palette rgbformula 7,5,15~%")  ;;  black, blue, red, yellow.
  (when label (format stream "set cblabel \"~A\"~%" label))
  (format stream "set view map~%")
  (when thumbnail (thumbnail stream :type type))
  (format stream "splot '-' matrix with image~%"))

(defun surface (stream &key z-label thumbnail type)
  "Print a 2d list structure as a 3D surface."
  (when z-label (format stream "set zlabel \"~A\"~%" z-label))
  (format stream "set style data linespoints~%")
  (format stream "set hidden3d~%")
  (format stream "set contour base~%")
  (when thumbnail (thumbnail stream :type type))
  (format stream "splot '-' w linesp~%"))

(defun 2d-graph (stream &key thumbnail type)
  "Prints a line graph...  probably don't need this..  use linegraph instead."
  (format stream "set style data linespoints~%")
  (when thumbnail (thumbnail stream :type type))
  (format stream "plot '-' w linesp~%"))

(defun line-graph (stream line-count legend-labels data-axes &key thumbnail type)
  "We have to repeat the data, but rather than a 'true' EOF marker, we just need an 'e', 'E', 'EOF', or something similar."
  (format stream "set style data linespoints~%")
  (when thumbnail (thumbnail stream :type type))
  (format stream "plot ")
  (loop for data-column from 1 below (1+ line-count)
     for label in legend-labels
     for axes in data-axes do
       (format stream "'-' using 1:~d with linespoints axes ~A title \"~A\" " (1+ data-column) axes label)
       (if (= data-column line-count)
	   (format stream "~%")
	   (format stream ", "))))

(defun scatter-plot (stream &key regression points thumbnail type)
  "General function to plot a scatter plot with gnuplot."
  (format stream "set nokey~%")
  (when regression
    (format stream "set fit logfile \"~A\"~%" regression)
    (format stream "f1(x) = a1*x + b1~%")
    (format stream "fit f1(x) '-' using 1:2 via a1,b1~%")
    (print-gnuplot-grid-to-stream points stream)
    (format stream "EOF~%"))
  (format stream "set style data points pointtype 7~%")
  (format stream "set style data points~%")
  (format stream "set pointtype 7~%")
  (format stream "set pointsize 1~%")
  (when thumbnail (thumbnail stream :type type))
  (format stream "plot '-' using 1:2 lc rgb \"blue\"")
  (when regression
    (format stream ", f1(x) title 'Linear regression' linecolor rgb 'red'")) ;; OR: ", f2(x) title 'Quadratic' linecolor rgb 'black'"
  (format stream "~%"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; User interface functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
  "General function to plot graphs with gnuplot."
  ;; If we're given a list of graph types, re-call this same function with the same args for
  ;; each values of the those 'types'.  After that, return.
  (when (listp type)
    (dolist (this-type type)
      (apply 'plot-graph points graph-type :type this-type rest))
    (return-from plot-graph (values)))
  (unless filename
    (setf filename (format nil "./~A" (string-downcase (string graph-type)))))
  (when regression
    (setf verbose nil))
  (when regression
    (setf regression (concatenate 'string filename ".fit.log")))
  ;; Check if the points all have the same number of dimensions
  (assert (bl:all-equal (mapcar 'length points)))
  ;; The column count is 1- the number of points because the first number in each list is the values used for the x-axis!!!
  (let ((column-count (1- (length (first points)))))
    ;; If axes are specified, make sure they all are, o/w put everything on x1y1
    (if data-axes
	(assert (= (length data-axes) column-count))
	(setf data-axes (make-sequence 'list column-count :initial-element "x1y1")))
    ;; Either make sure we have enough legend labels, or set them to something generic
    (if legend-labels
	(assert (or (not legend-labels) (= (length legend-labels) column-count)))
	(setf legend-labels (mapcar (lambda (x) (format nil "Line ~d" x)) (bl:interval 1 column-count))))
  ;; Start constructing the string that we'll use as the file...
  (with-output-to-string (s)
    ;; first do the terminal and type
    (format s "set terminal ~A enhanced size ~D,~D~%" (string-downcase (string type)) width height)
    (format s "set encoding ~A~%" encoding)
    (when (find type '(:png :svg :gif :jpg :ps :pdf))
      (format s "set output \"~A.~A\"~%" filename (string-downcase (string type))))
    ;; Blindly include the string specified under manual-options.
    (when manual-options
      (write-line manual-options s)
      (fresh-line s))
    ;; Set the title and some other things
    (format s "set title \"~A\"~%" title)
    (format s "set datafile missing 'NIL'~%")  ;; this means to ignore data that's "NIL"
    (format s "set key ~A ~A~%" key-vertical key-horizontal)
    (unless legend
      (format s "unset key~%"))
    ;; Name the axes
    (when x-label (format s "set xlabel \"~A\"~%" x-label))
    (when y-label (format s "set ylabel \"~A\"~%" y-label))
    (when x2-label (format s "set x2label \"~A\"~%" x2-label))
    (when y2-label (format s "set y2label \"~A\"~%" y2-label))
    (when y2-label
      (format s "set format y2 \"%.1f\"~%")
      (format s "set ytics nomirror~%")
      (format s "set y2tic~%"))
    ;; Do some scaling that will be overridden if specified...
    (format s "set autoscale x~%")
    (format s "set autoscale y~%")
    (format s "set auto x~%")
    (format s "set auto y~%")
    (unless (eq type :dumb)
      (format s "set grid~%"))
    ;; Setup the ranges if specified
    (when x-range (format s "set xrange [~A:~A]~%" (first x-range) (second x-range)))
    (when y-range (format s "set yrange [~A:~A]~%" (first y-range) (second y-range)))
    (when x2-range (format s "set x2range [~A:~A]~%" (first x2-range) (second x2-range)))
    (when y2-range (format s "set y2range [~A:~A]~%" (first y2-range) (second y2-range)))
    ;; If thumbnail specified, unset all the things we had set...
    (when thumbnail
      (thumbnail s :type type))
    ;; The 'plot' part of the graphing depends on the graph type
    (cond ((eq graph-type :heatmap)
	   (heatmap s :thumbnail thumbnail :type type :label cblabel))
	  ((eq graph-type :surface)
	   (surface s :thumbnail thumbnail :type type :z-label z-label))
	  ((eq graph-type :2dgraph)
	   (2d-graph s :thumbnail thumbnail :type type))
	  ((or (eq graph-type :line-graph) (eq graph-type :line))
	   (line-graph s column-count legend-labels data-axes :thumbnail thumbnail :type type))
	  ((eq graph-type :scatter)
	   (scatter-plot s :regression regression :points points :thumbnail thumbnail :type type))
	  ((eq graph-type :histogram)
	   (histogram points s :thumbnail thumbnail :type type))
	  (t (format t "UNKNOWN GRAPH TYPE!!! ~A" graph-type)))
    ;; Save the data
    (ensure-directories-exist filename)
    ;; we really need "column-count" versions of the data in this file... but not in "this" file, in the *main* file...
    (if (eq graph-type :heatmap)
	(print-gnuplot-grid-to-stream points s :n 1 :sort nil)
	(print-gnuplot-grid-to-stream points s :n column-count))
    ;; Run gnuplot
    (gnuplot-plot (get-output-stream-string s) :filename (bl:concat filename ".gp") :verbose verbose :debug debug)
    (values))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plotting histogram -- not working ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun histogram (values stream &key (bin-width nil) (bin-count nil) min-val max-val thumbnail type)
  "A generic function for plotting histograms."
  ;; Remove any points outside the specified range
  (when min-val (setf values (remove-if (lambda (x) (< x min-val)) values)))
  (when max-val (setf values (remove-if (lambda (x) (> x max-val)) values)))
  (let* ((begin (reduce 'min values))
	 (end (reduce 'max values))
	 (total-width (- end begin))
	 (value-table (make-hash-table))
	 (xtics '())
	 (max-count 0))
    (assert (or bin-count bin-width))
    (if bin-count
	(setf bin-width (/ total-width bin-count))
	(setf bin-count (/ (- end begin) bin-width)))
    ;; Collect all the points into bins 
    (loop for bin from 0 to bin-count
       for bottom = (+ (* bin bin-width) begin)
       for top = (+ bottom bin-width) do
	 (dolist (val values)
	   (when (and (>= val bottom) (< val top))
	     (push val (gethash bin value-table))))
	 (push (list (format nil "\"~,2f to ~,2f\"" bottom top) bin) xtics))
   ;; Find the maximum
   (loop for key being the hash-keys of value-table
      for point-count = (length (gethash key value-table)) do
	(when (> point-count max-count) (setf max-count point-count)))
   (format stream "set xtics(~{ ~{~A ~}~^, ~})" xtics)
   (format stream "set nokey~%")
   (format stream "set xtics nomirror rotate by -90~%")
   (format stream "set style data histograms~%")
   (format stream "set style histogram clustered gap 1~%") ;; change this to change the spacing b/tw the bars..
   (format stream "set style fill solid 1.0 border -1~%")
   (format stream "set yrange [0:~d]" (+ max-count 1))
   (format stream "set xrange [-1:~d]" (+ bin-count 1))
   (when thumbnail (thumbnail stream :type type))
   (format stream "plot '-' using 2 lc rgb \"blue\"")
   ;; Verify that this is correct...? It may not be.
   (loop for key being the hash-keys of value-table do
   	(format stream "~A ~A~%" key (gethash key value-table)))))

