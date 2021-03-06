(in-package :shuffletron)

(defvar *library* nil)
(defvar *filtered-library* nil "The library, excluding songs tagged 'ignore'")
(defvar *local-path->song* (make-hash-table :test 'equal))
(define-symbol-macro *library-base* (pref "library-base"))

(defstruct song
  (full-path nil :type pathname)
  local-path
  tags
  smashed
  properties
  matchprops
  id3
  id3-p)

(defmethod song-full-path-as-string ((song song))
  (namestring (song-full-path song)))

(defmethod song-local-path-as-string ((song song))
  (namestring (song-local-path song)))

(defun init-library ()
  (setf *library* (make-array 0 :fill-pointer 0 :adjustable t)))

(defun music-file-type (filename)
  "Should return the string corresponding to the filetype."
  (awhen (member (pathname-type filename)
                 '("mp3" "ogg" "flac") :test #'string=)
    (intern (string-upcase (car it)) :keyword)))

(defvar *library-progress* 0)

(defun smash-string (string)
  (substitute #\Space #\_ (string-downcase string)))

(defun carriage-return () (format t "~C" (code-char 13)))

(defun add-song-file (full-filename relative-filename)
  (let ((song (make-song :full-path full-filename
                         :local-path relative-filename
                         :smashed (smash-string
                                   (pathname-name relative-filename))
                         :tags nil)))
    (vector-push-extend song *library*)
    (setf (gethash (song-local-path song) *local-path->song*) song)))

(defun library-scan (path)
  "Add all songs in the library path."
  (let ((*library-progress* 0))
    (clrhash *local-path->song*)
    (when (probe-file path)
      (walk-directory path
            (lambda (filename)
              "Take a file name if its of music file-type then
increase the library progress and add song to library."
              (when (music-file-type filename)
                (incf *library-progress*)
                (when (zerop (mod *library-progress* 10))
                  (carriage-return)
                  (format t "Scanning. ~:D files.." *library-progress*)
                  (force-output))
                (add-song-file filename (enough-namestring filename path)))))
      t)))

(defun songs-needing-id3-scan () (count-if-not #'song-id3-p *library*))

(defun save-metadata-cache ()
  (setf (pref "id3-cache")
        (map 'vector (lambda (song) (list (song-local-path song)
                                          (song-id3-p song)
                                          (song-id3 song)))
             *library*))
  (values))

(defun load-metadata-cache ()
  (loop for (name id3-p id3) across (pref "id3-cache" #())
        as song = (gethash name *local-path->song*)
        when (and song id3-p)
        do (setf (song-id3-p song) t
                 (song-id3 song) id3)))

(defun get-song-metadata (absolute-path)
  (ignore-errors                        ; I'm a bad person.
   (case (music-file-type absolute-path)
     (:mp3  (mpg123:get-tags-from-file absolute-path :no-utf8 t))
     ;; FIXME: Audit OGG/FLAC paths for unicode insanity.
     (:ogg  (vorbisfile:get-vorbis-tags-from-file absolute-path))
     (:flac (flac:get-flac-tags-from-file absolute-path)))))

(defun scan-file-metadata (&key verbose adjective)
  (format t "~&Scanning file metadata (~:D files).~%" (songs-needing-id3-scan))
  (when verbose (fresh-line))
  (loop with pending = (and verbose (songs-needing-id3-scan))
        with n = 1
        for song across *library*
        unless (song-id3-p song) do
        (when verbose
          (carriage-return)
          (format t "Reading ~Atags: ~:D of ~:D" (or adjective "") n pending)
          (force-output))
        (setf (song-id3 song) (get-song-metadata (song-full-path-as-string song))
              (song-matchprops song) nil
              (song-id3-p song) t)
        (incf n)
        finally
        (when (and pending (not (zerop pending))) (terpri)))
  (save-metadata-cache))

(defun compute-filtered-library ()
  (setf *filtered-library* (remove-if (lambda (song) (find "ignore" (song-tags song) :test #'string=)) *library*)))
