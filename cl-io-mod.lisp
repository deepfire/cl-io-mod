(defpackage cl-io-mod
  (:use :common-lisp :bintype :iterate)
  (:export #:mod-v3))

(in-package :cl-io-mod)

(defbintype sample
  (:fields
   (value name              (zero-terminated-string 22))
   (value length            (unsigned-byte 16))
   (value finetune          (unsigned-byte 8))
   (value volume            (unsigned-byte 8))
   (value repeat-offset     (unsigned-byte 16))
   (value repeat-length     (unsigned-byte 16))))

(defbintype division
  (:fields
   (value a                 (unsigned-byte 8) :ignore t)
   (value b                 (unsigned-byte 8) :ignore t)
   (value c                 (unsigned-byte 8) :ignore t)
   (value d                 (unsigned-byte 8) :ignore t)
   (value sample            (pure sample (path-value *self* '(:typed-parent mod-mk) 'samples (logior (ash 4 (ldb (byte 4 4) (path-value *self* 'a)))
                                                                                                     (ldb (byte 4 4) (path-value *self* 'c))))))
   (value period            (pure (unsigned-byte 32) (logior (ash 8 (ldb (byte 4 0) (path-value *self* 'a)))
                                                             (path-value *self* 'b))))
   (match effect            (pure (unsigned-byte 8) (ldb (byte 4 0) (path-value *self* 'c)))
                            ((#x0 :arpeggio) (#x1 :slide-up) (#x2 :slide-down) (#x3 :slide-to)
                             (#x4 :vibrato) (#x5 :slide-to-note+volume-slide) (#x6 :vibrato+volume-slide) (#x7 :tremolo)
                             (#x9 :set-sample-offset) (#xa :volume-slide) (#xb :position-jump) (#xc :set-volume)
                             (#xd :pattern-break) (#xe :other) (#xf :set-speed)))
   (value effect-x          (pure (unsigned-byte 8) (ldb (byte 4 4) (path-value *self* 'd))))
   (value effect-y          (pure (unsigned-byte 8) (ldb (byte 4 0) (path-value *self* 'd))))))

(defbintype pattern
  (:fields
   (value divisions         (sequence 64 :element-type division))))

(defconstant +pattern-table-size+ 128)
(defconstant +sample-table-size+ 31)

(defbintype mod-mk
  (:documentation "Noisetracker/Soundtracker/Protracker Module Format Revision 3")
  (:total-length :length)
  (:fields
   (value title             (zero-terminated-string 20))
   (value samples           (sequence +sample-table-size+ :element-type sample))
   (value song-position     (unsigned-byte 8))
   (value magic1            (unsigned-byte 8))
   (value pattern-table     (sequence +pattern-table-size+ :element-type (unsigned-byte 8)))
   (match magic2            (zero-terminated-string 4) (("M.K.") ("M!K!")))
   (value patterns          (sequence (1+ (iter (for i below +pattern-table-size+)
                                                (maximize (path-value *self* 'pattern-table i))))
                                      :element-type pattern))
   (value sample-data       (irregular-sequence +sample-table-size+ :element-type (current-offset 32)
                                                                    :stride-fn (lambda (i)
                                                                                 (* 2 (path-value *self* 'samples i 'length))))
                            :out-of-stream-offset (- *total-length*
                                                     (* 2 (iter (for i below +sample-table-size+) 
                                                                (sum (path-value *self* 'samples i 'length))))))))

;; (let ((test-file-name "/mnt/etherstorm/audio/modules_orig/starcon2/pkunk.mod"))
;;   (format t "testing ~S:~%~S~%"
;;           test-file-name
;;           (with-open-file (str test-file-name :element-type '(unsigned-byte 8))
;;             (let ((seq (captured-stream:make-captured-stream str :allocate-full t)))
;;               (parse (bintype 'mod-mk) seq :big-endian)))))
