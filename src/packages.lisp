(defpackage :shuffletron
  (:use :common-lisp :mixalot)
  (:import-from :uiop/filesystem :directory-files
                                 :subdirectories
                                 :truenamize)
  (:import-from :uiop/pathname :ensure-absolute-pathname
                               :subpathp
                               :merge-pathnames*)
  (:import-from :cl-fad :walk-directory)
  (:import-from :anaphora :it
                          :awhen)
  (:export #:run #:*shuffletron-version*
           #:emptyp
           #:walk #:rel #:dfn
           #:*profile* #:pref #:prefpath
           #:*library* #:*filtered-library* #:*library-base*
           #:song #:song-full-path #:song-local-path #:song-tags
           #:song-properties #:song-id3 #:song-id3-p
           #:song-start-time
           #:songs-matching-tags #:songs-matching-tag
           #:tag-songs #:tag-song #:untag-songs #:untag-song
           #:decode-as-filename #:encode-as-filename
           #:*selection* #:selection-history*
           #:querying-library-p #:set-selection
           #:reset-query #:refine-query #:query
           #:with-stream-control #:with-playqueue #:when-playing
           #:*mixer* #:*current-stream* #:*playqueue*
           #:song-of #:stopped
           #:*loop-mode* #:*wakeup-time*
           #:end-stream #:finish-stream
           #:play-song #:play-songs #:add-songs #:play-next-song #:skip-song
           #:play-command #:stop-command
           #:toggle-pause #:unpause
           #:current-song-playing
           #:playqueue-and-current
           #:queue-remove-songs #:queue-remove-indices
           #:parse-item-list #:parse-tag-list
           #:tag-current-song #:untag-current-song
           #:kill-tag #:tag-count-pairs
           #:parse-ranges #:expand-ranges #:extract-ranges
           #:sgr #:spacing
           #:time->string #:utime->string #:parse-relative-time
           #:parse-alarm-args
           #:parse-and-execute))
