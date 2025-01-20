;;; NA Skyline Generator

;;; Copyright 2025 Jeffrey T. Read. For license, see LICENSE.

;;; This is the NullAwesome Skyline Generator. I used it to generate
;;; the Onyxopolis skyline for level backgrounds for my side-scrolling
;;; game, NullAwesome.

;;; Define a bit-mapped image type. No big deal; it has just a width
;;; and a height and a u32vector of pixels each of which is an integer
;;; in RGB order.

;;; It is written for Gambit Scheme v4.9.5 or later:
;;; https://gambitscheme.org

(define-record-type image
  (really-make-image width height pixels)
  image?
  (width image-width)
  (height image-height)
  (pixels image-pixels))

(define (make-image width height)
  (really-make-image width height (make-u32vector (* width height))))

;;; Combine red, green, and blue values into a single pixel value.

(define (rgb r g b)
  (bitwise-ior
   (arithmetic-shift
    (bitwise-and r 255)
    16)
   (arithmetic-shift
    (bitwise-and g 255)
    8)
   (bitwise-and b 255)))

;;; Fill a rectangle in an image with the specified color.

(define (fill-rectangle! image x y w h color)
  (let* ((img-w (image-width image))
	 (img-h (image-height image))
	 (pixels (image-pixels image))
	 (l (max 0 x))
	 (t (max 0 y))
	 (r (min img-w (+ x w)))
	 (b (min img-h (+ y h))))
    (do ((j t (+ j 1)))
	((>= j b))
      (do ((i l (+ i 1)))
	  ((>= i r))
	(u32vector-set! pixels (+ (* j img-w) i) color)))))

;;; Draw a building with random lights. The building is itself a black
;;; rectangle, with the horizontal offset from the left edge of the
;;; image given by the x parameter, and the width and height (from
;;; bottom of image) given by w and h.

;;; Lights are computed as follows: Pick a side margin randomly from
;;; [0,w/6), a top margin randomly from [0,w/10) and a bottom margin
;;; randomly from [0,w/5). (This makes the lights appear more toward
;;; the tops of the buildings.) Then, imagine a rectangle inside the
;;; building's rectangle, but offset by `margin-sides` at the sides,
;;; `margin-top` at the top, and `margin-bottom` at the bottom. Divide
;;; this rectangle into a grid `lights-per-row` (randomly chosen from
;;; [6,12)) cells across by `lights-rows` (randomly chosen from
;;; [15,25)) cells down. Randomly choose a size for each light where
;;; the width is at least 2 but less than 1/6 the light cell width;
;;; and the height is at least 2 but less than 1/3 the light cell
;;; height. Draw the lights as yellow rectangles, centering them
;;; horizontally in their light cells. There is a 15% chance that any
;;; given light will not be drawn.

(define (make-building! image x w h)
  (fill-rectangle! image x (- (image-height image) h) w h (rgb 0 0 0))
  (let* ((margin-sides (random-integer (quotient w 6)))
	 (margin-top (random-integer (quotient h 10)))
	 (margin-bottom (+ margin-top (random-integer (quotient h 5))))
	 (lights-width (- w margin-sides margin-sides))
	 (lights-height (- h margin-top margin-bottom))
	 (lights-per-row (+ 6 (random-integer 6)))
	 (lights-rows (+ 15 (random-integer 10)))
	 (light-spacing-h (quotient lights-width lights-per-row))
	 (light-spacing-v (quotient lights-height lights-rows))
	 (light-width (+ (max 2 (quotient light-spacing-h 8)) (random-integer (quotient (* light-spacing-h 6) 8))))
	 (light-height (+ (max 2 (quotient light-spacing-v 8)) (random-integer (quotient (* light-spacing-v 3) 8)))))
    (do ((j 0 (+ j light-spacing-v)))
	((>= j lights-height))
      (do ((i 0 (+ i light-spacing-h)))
	  ((>= i lights-width))
	(if (< (random-integer 100) 85)
	    (fill-rectangle! image (+ x margin-sides (quotient light-spacing-h 2) i) (+ (- (image-height image) h) margin-top j) light-width light-height (rgb 255 255 64)))))))
	 
;;; Make a solid dark-blue sky.

(define (make-sky! image)
  (fill-rectangle! image 0 0 (image-width image) (image-height image) (rgb 15 15 47)))

;;; Draw the complete skyline by painting the sky, then moving left to
;;; right, adding randomly sized buildings with `make-building!`
;;; above. Building widths are between 40 and 100 pixels, in
;;; increments of 6 pixels; heights are between 100 and 300 pixels in
;;; increments of 20 pixels.

(define (make-skyline! image)
  (make-sky! image)
  (let loop ((startx 0)
	     (width (+ 40 (* 6 (random-integer 10)))))
    (if (< (+ startx width) (image-width image))
	(begin
	  (if (> (+ startx width)
		 (- (image-width image) 20))
	      (set! width (- (image-width image) startx)))
	  (make-building! image startx width (+ 100 (* 20 (random-integer 10))))
	  (loop (+ startx width) (+ 40 (* 6 (random-integer 10))))))))

;;; Write the image as a PNM file (ASCII "P3" type, 24 bits per
;;; pixel).

(define (output-image image)
  (display "P3")
  (newline)
  (display (image-width image))
  (display " ")
  (display (image-height image))
  (newline)
  (display 255)
  (newline)
  (let* ((pix (image-pixels image))
	 (len (u32vector-length pix)))
    (do ((i 0 (+ i 1)))
	((>= i len))
      (let* ((p (u32vector-ref pix i)))
	(display (bitwise-and (arithmetic-shift p -16) 255))
	(display " ")
	(display (bitwise-and (arithmetic-shift p -8) 255))
	(display " ")
	(display (bitwise-and p 255))
	(display " "))))
  (newline))

;;; Create an image, render a skyline to it, then output it to stdout.

(let* ((img (make-image 1440 320)))
  (make-skyline! img)
  (output-image img))
