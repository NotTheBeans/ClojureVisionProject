;Name:Joseph Hall
(ns assignment3.core
  (:gen-class)
  (:import [java.awt.image  BufferedImage])
  (:import [javax.imageio  ImageIO])
  (:import [java.io File]))

(defn read-image
  "Reads in a image and puts it into a BufferedImage object."
  [filename]
  (let [file (File. filename)]
    (ImageIO/read file)))

(defn save-image
  "Save a given image to a file."
  [image extension filename]
  (let [file (File. filename)]
    (ImageIO/write image extension file)))

(defn move-num
  "Moves a value in a kirch filter to the next position"
  [k_filter value x y]
  (if (= y 0)
    ;if it's the first column
    ;if it's the end value, move it down a column, else move to the right
    (if (= x 2)
      (assoc k_filter 1 (assoc (nth k_filter 1) x value))
      (assoc k_filter y (assoc (nth k_filter y) (+ x 1) value)))
    (if (= y 1)
      ;if it's the second row
      ;if it's the middle value, do nothing
      (if (= x 1)
        (vec k_filter)
        ;if it's the left most value, move it up a column
        ;else if it's the right most value, move it down a column
        (if (= x 0)
          (assoc k_filter 0 (assoc (nth k_filter 0) x value))
          (assoc k_filter 2 (assoc (nth k_filter 2) x value))))
      ;if it's the third column
      ;if it's the left most value, move it up a row
      ;else, move the value to the left
      (if (= x 0)
        (assoc k_filter (- y 1) (assoc (nth k_filter (- y 1)) x value))
        (assoc k_filter y (assoc (nth k_filter y) (- x 1) value))))))

(defn next-filter
  "Gets the next Kirsch Filter based on the given Kirsch Filter."
  [k_filter]
  (loop [counter 0
         n_filter k_filter
         y 0
         x 0]
    ;if there's still values to move, move them
    ;else, return the next filter
    (if (> 9 counter)
      (recur (inc counter)
             (move-num n_filter (nth (nth k_filter y) x) x y)
             ;if it's at the end of the row, go down a column
             ;else, stay in the same column
             (if (= x 2)
               (inc y)
               (int y))
             ;if it's at the end of the row, go back to the start of the row
             ;else, move to the next position in the row
             (if (< x 2)
               (inc x)
               (- x 2)))
      (vec n_filter))))

(defn set-grey
  "Sets the grey value at a given co-ordinate."
  [image x y grey]
  (.setRGB image x y (+ (+ (bit-shift-left grey 16) (bit-shift-left grey 8)) grey)))

(defn get-kirch
  "Get's the ith kirch filter."
  [i]
  (loop [k-filter [[-1 0 1] [-2 0 2] [-1 0 1]]
         counter 0]
    ;if it's the correct filter, return it
    ;else, get the next filter
    (if (= i counter)
      (vec k-filter)
      (recur (next-filter k-filter)
             (inc counter)))))

(defn get-pixels
  "Gets the pixels that need the kirch filter to be applied to."
  [image x y]
  (loop [yp (- y 1)
         pixels []]
    ;if it's gotten all of the pixels, return the vector of pixels
    ;else, get the pixels in the current row
    (if (= yp (+ y 2))
      (vec pixels)
      (recur (inc yp)
             (loop [xp (- x 1)
                    x-pixels []]
               ;if it's gotten all of the values in a row, add them to the vector of pixels
               ;else, get the current pixels 8-bit greyscale value
               (if (= xp (+ x 2))
                 (conj pixels x-pixels)
                 (recur (inc xp)
                        (conj x-pixels (bit-and (.getRGB image xp yp) 0xFF)))))))))

(defn get-all-pixels
  "Takes in a given an image and all of the grey pixel values."
  [image]
  (loop [y 0
         pixel-values []]
    (if (< y (.getHeight image))
      (recur (inc y)
      (loop [x 0
             x-values pixel-values]
        (if (< x (.getWidth image))
          (recur (inc x) (conj x-values (bit-and (.getRGB image x y) 0xFF)))
          (vec x-values))))
      (vec pixel-values))))

(defn clamp
  "Ensures the the given value is between 0 and 255. If the value is negative, it'll retrun 0, if it's greater than 255, it'll return 255."
  [value]
  (max 0 (min 255 value)))

(defn get-kirch-value
  "Get's the value of a pixel after the ith kirch filter has been applied to it."
  [image i x y]
  (let [ ;get all of the relevant pixels
         image-pixels (get-pixels image x y)
         ;get the relevant kirch filter
         k-filter (get-kirch i)]
    (clamp (+ 127 (reduce + (map * (flatten image-pixels) (flatten k-filter)))))))

(defn kirch
  "Applies the ith kirch filter to an image."
  [image i]
  (let [;create a new grey scale image, with a border one pixel smaller
         test-image (BufferedImage. (- (.getWidth image) 2)(- (.getHeight image) 2) BufferedImage/TYPE_BYTE_GRAY)]
    (dotimes [x (- (.getWidth image) 2)]
     (dotimes [y (- (.getHeight image) 2)]
      ;get the kirch value for the given pixel, then apply it to the new image
      (let [grey (get-kirch-value image i (+ 1 x) (+ 1 y))]
        (set-grey test-image x y grey))))
   test-image))

(defn get-maxes
  "Gets all of the filtered values for a given position and put them into a vector."
  [image x y]
  (let [mags []]
  (vec (flatten (pmap #(get-kirch-value image % x y) (range 0 8))))))

(def get-maxes-memoize
  (memoize get-maxes))

(defn edge-magnitude-hist-memoize
  "Gets the edge magnitude histogram of a given image."
  [filename]
  (let [;get the image from the filename
         image (read-image filename)]
    (loop [y 1
           height (- (.getHeight image) 1)
           hist [0 0 0 0 0 0 0 0]]
      ;if there's still pixels to get
      (if (> height y)
        (recur (inc y)
               height
               ;get the histogram for the given y position, add it to the current histogram
               (vec (map + hist (apply pmap + (pmap
                                                ;increments the vector at a given position
                                                #(update [0 0 0 0 0 0 0 0]
                                                         ;add to the bin based on the value range of the bin that the max filtered value fits into
                                                         (quot (apply max (get-maxes-memoize image % y)) 32)
                                                         inc)
                                                ;do the function for all of the x values
                                                (range 1 (- (.getWidth image) 1)))))))
        ;return the completed, normalised, histogram, once all pixels have been gone through
        (vec (map #(/ % (double (reduce + hist))) hist))))))

(defn edge-direction-hist-memoize
  "Gets the edge direction histogram of a given image."
  [filename]
  (let [;get the image from the filename
         image (read-image filename)]
    (loop [y 1
           height (- (.getHeight image) 1)
           hist [0 0 0 0 0 0 0 0]]
      ;if there's still pixels to get
      (if (> height y)
        (recur (inc y)
               height
               ;get the histogram for the given y position, add it to the current histogram
                (vec (map + hist (apply pmap + (pmap
                                                ;increments the vector at a given position
                                                 #(update [0 0 0 0 0 0 0 0]
                                                          ;add to the bin based on the filer that gave the max value
                                                          (first (apply max-key second(map-indexed vector (get-maxes-memoize image % y))))
                                                          inc)
                                                ;do the function for all of the x values
                                                 (range 1 (- (.getWidth image) 1)))))))
        ;return the completed, normalised, histogram, once all pixels have been gone through
        (vec (map #(/ % (double (reduce + hist))) hist))))))

(def edge-direction-hist
  ;memoizes the edge direction histogram function
  (memoize edge-direction-hist-memoize))

(def edge-magnitude-hist
  ;memoizes the edge magnitude histogram function
  (memoize edge-magnitude-hist-memoize))

(def get-all-pixels-memoize
  (memoize get-all-pixels))

(defn intensity-hist-memoize
  "Gets the intensity histogram for the given image."
  [filename]
    (let [image (read-image filename)
           hist [0 0 0 0 0 0 0 0]
           pixels (get-all-pixels-memoize image)
          totals (apply pmap + (pmap #(update hist (quot % 32) inc) pixels))]
      (vec (map #(/ % (double (reduce + totals))) totals))))

(def intensity-hist
  (memoize intensity-hist-memoize))

(defn image-descriptor
  "Returns the image descriptor of a given image."
  [filename]
  (let [magnitude (edge-magnitude-hist filename)
        direction (edge-direction-hist filename)
        intensity (intensity-hist filename)]
    (map #(/ % 3.0) (concat  magnitude direction intensity))))

(defn image-similarity
  "Returns the combined minimum values of the two histograms, derived from the two provided images."
  [file1 file2]
  (let [des1 (image-descriptor file1)
        des2 (image-descriptor file2)]
    (reduce + (pmap #(min %1 %2) des1 des2))))

(defn -main
  "For testing all images of a set against all other images in a set."
  [& args]
  (for [i1 [1 2 3 4 5 6 7 8]
         i2 [1 2 3 4 5 6 7 8]]
    (image-similarity (str "vehicle_images/" (first args) i1 ".jpg") (str "vehicle_images/" (second args) i2 ".jpg"))
    ))
