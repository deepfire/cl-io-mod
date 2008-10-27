;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: MOD; Base: 10 -*-
;;;
;;;  (c) copyright 2007-2008 by
;;;           Samium Gromoff (_deepfire@feelingofgreen.ru)
;;;
;;; This library is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU Library General Public
;;; License as published by the Free Software Foundation; either
;;; version 2 of the License, or (at your option) any later version.
;;;
;;; This library is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; Library General Public License for more details.
;;;
;;; You should have received a copy of the GNU Library General Public
;;; License along with this library; if not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA  02111-1307  USA.

(in-package :cl-io-mod)

(setf *break-on-signals* t)
(setf *break-on-signals* nil)

(defbintype sample ()
  (:fields
   (value name              (zero-terminated-string 22))
   (value length            (unsigned-byte 16))
   (value finetune          (unsigned-byte 8))
   (value volume            (unsigned-byte 8))
   (value repeat-offset     (unsigned-byte 16))
   (value repeat-length     (unsigned-byte 16))))

(defbintype division-channel ()
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

(defbintype division (channels)
  (:fields
   (value channels          (sequence channels :element-type division-channel))))

(defbintype pattern (channels)
  (:fields
   (value divisions         (sequence 64 :element-type (division channels) :stride (ash (* 4 channels) 3)))))

(defconstant +pattern-table-size+ 128)
(defconstant +sample-table-size+ 31)

(defbintype mod-mk ()
  (:documentation "Noisetracker/Soundtracker/Protracker Module Format Revision 3")
  (:total-length :length)
  (:fields
   (value title             (zero-terminated-string 20))
   (value samples           (sequence +sample-table-size+ :element-type (sample)))
   (value song-position     (unsigned-byte 8))
   (value magic1            (unsigned-byte 8))
   (value pattern-table     (sequence +pattern-table-size+ :element-type (unsigned-byte 8)))
   (match channel-count     (zero-terminated-string 4) (("M.K." 4) ("FLT4" 4) ("6CHN" 6) ("8CHN" 8)))
   (value patterns          (sequence (1+ (iter (for i below +pattern-table-size+)
                                                (maximize (path-value *self* 'pattern-table i))))
                                      :element-type (pattern (path-value *self* 'channel-count))
                                      :stride (ash (* 64 4 (path-value *self* 'channel-count)) 3)))
   (value sample-data       (sequence +sample-table-size+ :element-type (current-offset 32)
                                                          :stride-fn (lambda (i)
                                                                       (ash (* 2 (path-value *self* 'samples i 'length)) 3)))
                            :out-of-stream-offset (ash (- (length *sequence*)
                                                          (* 2 (iter (for i below +sample-table-size+) 
                                                                     (sum (path-value *self* 'samples i 'length)))))
                                                       3))))

;; (let ((test-file-name "/mnt/core/audio/modules_orig/starcon2/pkunk.mod"))
;;   (format t "testing ~S:~%~S~%"
;;           test-file-name
;;           (let ((seq (pergamum:file-as-vector test-file-name)))
;;             (parse 'mod-mk seq :endianness :big-endian))))
