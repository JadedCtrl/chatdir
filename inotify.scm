;; Copyright 2023, Jaidyn Levesque <jadedctrl@posteo.at>
;; All rights reserved. Distributed under the terms of the MIT license.
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.
;;

(import scheme
		(chicken file) (chicken io) (chicken pathname)
		srfi-1
        (prefix inotify inotify:))


;; ——————————————————————————————————————————————————
;; Skeleton of a daemon
;; ——————————————————————————————————————————————————

;; The FS-backed input loop, to be run in a seperate thread (so as to not block).
;; This handles channel leaving/joining, and the sending of messages.
;; It should be called after input-loop-init.
;; Call-backs that should be provided in the callbacks-alist are:
;;    (join-channel channel)    ;; Should call this library's channel-joined function.
;;    (leave-channel channel)
;;    (send-message channel message-content)
(define (input-loop root-dir callbacks-alist)
  (map (lambda (event)
         (handle-file-event root-dir callbacks-alist event))
       (inotify:next-events!))
  (input-loop root-dir callbacks-alist))


;; Initialization for the input loop
(define (input-loop-init root-dir callbacks-alist)
  (let ([join-callback (alist-ref 'join-channel callbacks-alist)])
    (inotify:init!)
    ;; Start watching the chatdir (for new channel joins, etc)
    (inotify:add-watch!
     root-dir '(onlydir moved-to moved-from delete delete-self create))

    ;; Auto-join channels with all pre-existing channel directories
    (map (lambda (path)
           (let* ([channel-dirname (pathname-file (pathname-directory (string-append path "/")))]
				  [in-path (subpath root-dir channel-dirname ".in")]
                  [join-callback (alist-ref 'join-channel callbacks-alist)])
			 (print channel-dirname " - " in-path)
             (if join-callback
                 (apply join-callback (list channel-dirname)))

             (inotify:add-watch! (create-directory in-path #t)
								 '(moved-to close-write))
             (print "Joined and watching: " in-path)))
         (filter directory-exists? (directory-rel root-dir)))))


;; Should be called from the join-channel callback, to
;; communicate that the channel was successfully joined.
(define (channel-joined root-dir channel)
  (let* ([in-path (subpath root-dir channel ".in")])
    (inotify:add-watch! (create-directory in-path #t)
						'(moved-to close-write))
    (print "Began watching input " in-path ".")))


;; Handle a single inotify file event, as part of the input loop
(define (handle-file-event root-dir callbacks-alist event)
  (if (not (member 'ignored (inotify:event-flags event)))
      (let* ([flags (inotify:event-flags event)]
             [wd-path (inotify:wd->path (inotify:event-wd event))]
             [main-dir? (string=? wd-path root-dir)])
        (if main-dir?
            (handle-main-dir-event root-dir callbacks-alist event)
            (handle-channel-dir-event root-dir callbacks-alist event)))))


;; Handles all inotify-watched file events from the top-level IRC-directory.
;; Mainly, checking for newly-joined or left channels.
(define (handle-main-dir-event root-dir callbacks-alist event)
  (let ([flags (inotify:event-flags event)]
        [leave-callback (alist-ref 'leave-channel callbacks-alist)]
        [join-callback (alist-ref 'join-channel callbacks-alist)])
    (cond
     ;; If a channel dir's been moved or removed, stop watching (ofc)
     ;; … Also quit that room! Heck them!
     [(or (member 'moved-from flags)
          (member 'delete flags)
          (member 'delete-self flags))
      (let* ([channel (inotify:event-name event)]
			 [channel-inpath (subpath root-dir channel ".in")]
             [channel-wd (path->wd channel-inpath)])
        (print "Remove watch for " channel-inpath "…")
        (if (and channel-wd (member channel-wd (inotify:wd-list)))
            (attempt-remove-watch! channel-wd))
        (if leave-callback
            (apply leave-callback (list channel))))]

     ;; If a dir's been created for a channel, maybe-join, then watch input!
     [(or (member 'create flags)
          (member 'moved-to flags))
      (let* ([channel (inotify:event-name event)])
        (print "Attempting to join channel " channel "…")
        (if join-callback
            (apply join-callback (list channel))))])))


;; Handles an inotify event that pertains to a channel's .in/ directory
(define (handle-channel-dir-event root-dir callbacks-alist event)
  (let* ([event-dir (pathname-directory (inotify:event->pathname event))]
         [dirname (pathname-file event-dir)]
         [channel (pathname-file (pathname-directory event-dir))]
         [send-message-callback (alist-ref 'send-message callbacks-alist)])
    (cond
     ;; If input is given to an `.in` dir… well, send that darn message!
     ;; What're you wating for?
     [(and (string=? dirname ".in")
           send-message-callback)
      (map (lambda (message)
             (apply send-message-callback (list channel message)))
           (with-input-from-file (inotify:event->pathname event)
             read-lines))
      (delete-file* (inotify:event->pathname event))])))



;; ——————————————————————————————————————————————————
;; Utility
;; ——————————————————————————————————————————————————

;; Returns an inotify watch-descriptor according the given path
(define (path->wd path)
  (car
   (filter (lambda (wd)
             (string=? (normalize-pathname (inotify:wd->path wd))
                       (normalize-pathname path)))
           (inotify:wd-list))))


;; Attempt to remove an inotify watch; if it's already been removed, no sweat
;; (This happens sometimes when inotify automatically deletes a watch)
(define (attempt-remove-watch! watch)
  (handle-exceptions exn
      #t
    (inotify:remove-watch! watch)))


;; Wrapper around `directory` that lists a dir's files as a relative path
(define (directory-rel #!optional (path "./"))
  (let ([relative-parent (normalize-pathname (string-append path "/"))])
    (map (lambda (leaf)
           (string-append relative-parent leaf))
         (directory path))))


;; Return a file path with the given parameters as elements of the path
;; E.g., "/etc/", "/systemd/user" "mom" => "/etc/systemd/user/mom"
(define (subpath . children)
  (normalize-pathname
   (reduce-right (lambda (a b)
                   (string-append a "/" b))
                 "" children)))

;; Repeat after me:
;; 🎵 Symbolic links cannot have extended attributes, and that is a war-crime. 🎶
;; 🎵 Directories cannot have extended attributes, and that is a war-crime. 🎶

