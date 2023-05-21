;; Copyright 2023, Jaidyn Levesque <jadedctrl@posteo.at>
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
        (chicken file) (chicken file posix) (chicken pathname) (chicken io)
        (chicken random) (chicken string)
        srfi-1 srfi-13  srfi-19
        (prefix inotify inotify:)
        (prefix xattr xattr:))


;; ——————————————————————————————————————————————————
;; Channel management
;; ——————————————————————————————————————————————————

;; Lists all currently-joined channels.
(define (channels root)
  (directory root))

;; Creates a channel's file hierarchy; safe to run, even if the channel
;; has already been created.
(define (channel-add! root channel)
  (let* ([path (subpath root channel)])
    (create-directory (subpath path ".in") #t)
    (create-directory (subpath path ".users" "online") #t)
    (create-directory (subpath path ".users" "offline") #t)
    (create-directory (subpath path ".users" "all") #t)
    (channel-cleanup! root channel)))


;; Tidies up a channel directory: Removes `online` and `offline` user links.
(define (channel-cleanup! root channel)
  (let ([users-dir (subpath root channel ".users")])
    (map
     (lambda (state-dir)
       (if (not (substring-index state-dir "/all"))
           (map
            (lambda (link)
              (let ([link-path (subpath users-dir state-dir link)])
                (if (symbolic-link? link-path)
                    (delete-file link-path))))
            (directory (subpath users-dir state-dir)))))
     (directory users-dir))))


;; Sets a channel's metadata value; that is, sets the contents of the file
;; /$channel/.meta/$key to $value.
(define (channel-metadata-set! root channel key value #!optional (xattr-alist '()))
  (directory-file-set! (subpath root channel ".meta")
                       key value
                       xattr-alist))


;; Return a specific bit of metadata of a channel, as a string
(define (channel-metadata-get root channel key)
  (directory-file-get (subpath root channel ".meta") key))


;; Return a cons-list of a channel's metadata, with the file-content followed by
;; an alist of the extended attributes
(define (channel-metadata-get* root channel key)
  (directory-file-get* (subpath root channel ".meta") key))


;; Return a list of all metadata key (files in /$channel/.meta/).
(define (channel-metadata root channel)
  (directory (subpath root channel ".meta")))



;; ——————————————————————————————————————————————————
;; User management
;; ——————————————————————————————————————————————————

;; Create a user's server-wide global-user directory.
;; Quite simple, compared to channel-user-add!
(define (user-add! root username)
  (create-directory (subpath root ".users" username "local") #t))


;; Sets a file in the user's directory to given value.
;; Sets /.users/$user/$key to $value.
(define (user-file-set! root username key value #!optional (xattr-alist '()))
  (directory-file-set! (subpath root ".users" username)
                       key value xattr-alist))


;; Returns the contents of a file in the user's global directory,
;; /.users/$user/$key.
(define (user-file-get root username key)
  (directory-file-get (subpath root ".users" username) key))


;; Enables a user's state (online/offline/etc), for all channels they are in.
(define (user-enable-state! root username state)
  (map
   (lambda (channel)
     (channel-user-enable-state! root channel username state))
   (directory (subpath root ".users" username "local"))))


;; Disables a user's state (online/offline/etc), for all channels they are in.
(define (user-disable-state! root username state)
  (map
   (lambda (channel)
     (channel-user-disable-state! root channel username state))
   (directory (subpath root ".users" username "local"))))


;; Ensures the enabled-state is enabled, and it's opposite (disabled-state) is not,
;; for all channels the given user is in.
(define (user-toggle-states! root username enabled-state disabled-state)
  (map
   (lambda (channel)
     (channel-user-toggle-states! root channel username
                                  enabled-state disabled-state))
   (directory (subpath root ".users" username "local"))))


;; Return a list of all users of a channel of given state.
;; (Lists files in /$channel/.users/$state/).
(define (channel-users root channel #!optional (state "online"))
  (directory (subpath root channel ".users" state)))


;; Add a user to a channel, creating their channel-user directory.
;; There are three types of channel users:
;; * Channel-only: We have no meaningful way of ever linking this user to a
;;                 server-wide identity.
;;                 (global? #f) (global-pairity #f)
;; * Serverwide-1: The user has a server-wide identity, and data like
;;                 nicknames/profile-pictures can NOT be changed on a per-channel
;;                 basis. channel-user is link to global-user.
;;                 (global #t) (global-pairity #t)
;; * Serverwide-2: The user has a server-wide identity, but their
;;                 nickname/profile-picture/etc can vary by the channel.
;;                 (global #t) (global-pairity #f)
(define (channel-user-add! root channel username
                           #!optional (global? #t) (global-pairity? #t) (global-name #f))
  (let* ([g-name (if global-name global-name username)]
         [user-path (subpath root channel ".users" "all" username)]
         [g-user-path (subpath root ".users" g-name)])
    (cond [(or (file-exists? user-path) (directory-exists? user-path))
           #f]
          ;; If global, we gotta do some symlink dancing.
          [global?
           (user-add! root g-name)
           (if global-pairity?
               (create-symbolic-link (subpath "../../../.users" g-name) user-path)
               (create-directory user-path #t))
           (create-symbolic-link (subpath "../../../../.users" g-name)
                                 (subpath user-path "global"))
           (create-symbolic-link (subpath "../../../" channel ".users" "all" username)
                                 (subpath g-user-path "local" channel))]
          ;; This is a channel-only user, don't bother with symlink fanciness.
          [#t
           (create-directory user-path #t)])))


;; Sets a file in the channel-user's directory to given value.
;; Sets /$channel/.users/all/$user/$key to $value.
(define (channel-user-file-set! root channel username key value #!optional (xattr-alist '()))
  (directory-file-set! (subpath root channel ".users" "all" username)
                       key value xattr-alist))


;; Returns the contents of a file in the user's channel directory,
;; /$channel/.users/all/$user/$key.
(define (channel-user-file-get root channel username key)
  (directory-file-get (subpath root channel ".users" "all" username) key))


;; Disables a channel-user's online/offline/etc state.
;; That is, removes a symlink from a /$channel/.users/* directory.
(define (channel-user-disable-state! root channel username state)
  (let ([state-link (subpath root channel ".users" state username)])
    (if (or (file-exists? state-link)
            (symbolic-link? state-link))
        (delete-file state-link))))


;; Enables a channel-user's state (online/offline/etc).
;; That is, makes a symlink to a /$channel/.users/* directory.
(define (channel-user-enable-state! root channel username state)
  (let* ([state-path
          (create-directory (subpath root channel ".users" state) #t)]
         [user-path (subpath ".." "all" username)]
         [state-link (subpath state-path username)])
    (if (not (or (file-exists? state-link)
                 (symbolic-link? state-link)))
        (create-symbolic-link user-path
                              state-link))))


;; Ensures the enabled-state is enabled, and it's opposite (disabled-state) is not
(define (channel-user-toggle-states! root channel username enabled-state disabled-state)
  (channel-user-disable-state! root channel username disabled-state)
  (channel-user-enable-state! root channel username enabled-state))



;; ——————————————————————————————————————————————————
;; Message management
;; ——————————————————————————————————————————————————

;; Create a message file for the given channel, contents, sender, etc.
(define (channel-message-add! root channel contents
                              #!optional (sender #f) (date (current-date))
                              (additional-xattrs '()))
  (let* ([attrs-sans-sender (append
                             `((user.chat.date . ,(date->string date "~1T~2"))
                               (user.chat.channel . ,channel))
                             additional-xattrs)]
         [attrs (if sender
                    (append attrs-sans-sender `((user.chat.sender . ,sender)))
                    attrs-sans-sender)])
    (directory-file-set! (subpath root channel)
                         (channel-message-file-leaf root channel date)
                         contents attrs)))


;; List all messages of the given channel.
(define (channel-messages root channel)
  (directory (subpath root channel)))


;; List all messages that have the given xattr set to the given value.
(define (channel-messages-by-xattr root channel xattr value)
  (filter
   (lambda (message-leaf)
     (string=? (xattr:get-xattr (subpath root channel message-leaf)
                                xattr)
               value))
   (channel-messages root channel)))


;; List all messages from the given sender.
(define (channel-messages-by-sender root channel sender)
  (channel-messages-by-xattr root channel "user.chat.sender" sender))


;; List all messages sent at exactly the given date.
(define (channel-messages-by-date root channel date)
  (channel-messages-by-xattr root channel "user.chat.date"
                             (date->string date "~1T~2")))


;; List all messages sent around the given date, ±deviation seconds.
(define (channel-messages-by-date* root channel date deviation)
  (channel-messages-by-date-range root channel
                                  (seconds->date (- (date->seconds date) deviation))
                                  (seconds->date (+ (date->seconds date) deviation))))


;; List all messages sent within the given date range.
(define (channel-messages-by-date-range root channel min-date max-date)
  (filter
   (lambda (message-leaf)
     (let* ([message-path (subpath root channel message-leaf)]
            [message-date (string->date (xattr:get-xattr message-path "user.chat.date")
                                        "~Y-~m-~dT~H:~M:~S~z")])
       (and (date<=? min-date message-date)
            (date<=? message-date max-date))))
   (channel-messages root channel)))


;; Finds an appropriate (non-colliding, non-in-use) name for a message file,
;; based on its date.
(define (channel-message-file-leaf root channel date)
  (directory-unique-file (subpath root channel)
                         (date->string date "[~m-~d] ~H:~M:~S")))



;; ——————————————————————————————————————————————————
;; Skeleton of a daemon
;; ——————————————————————————————————————————————————

;; Initialization for the input loop
(define (input-loop-init root-dir callbacks-alist)
  (let ([join-callback (alist-ref 'join-channel callbacks-alist)])
    (inotify:init!)
    ;; Start watching the chatdir (for new channel joins, etc)
    (inotify:add-watch!
     root-dir '(onlydir moved-to moved-from delete delete-self create))

    ;; Auto-join channels with all pre-existing channel directories
    (map (lambda (path)
           (let ([channel-dirname (pathname-file (pathname-directory (string-append path "/")))]
                 [join-callback (alist-ref 'join-channel callbacks-alist)])
             (if join-callback
                 (apply join-callback (list channel-dirname)))

             (inotify:add-watch! in-path '(moved-to close-write))
             (print "Joined and watching: " in-path)))
         (filter directory-exists? (directory-rel irc-dir)))))


;; Handles all inotify-watched file events from the top-level IRC-directory.
;; Mainly, checking for newly-joined or left channels.
(define (handle-main-dir-event callbacks-alist event)
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
             [channel-inpath
              (string-append (inotify:wd->path
                              (inotify:event-wd event))
                             channel "/.in")]
             [channel-wd (path->wd channel-inpath)])
        (print "Remove watch for " channel-inpath "…")
        (if (and channel-wd (member channel-wd (wd-list)))
            (attempt-remove-watch! channel-wd))
        (if leave-callback
            (apply leave-callback (list channel))))]

     ;; If a dir's been created for a channel, maybe-join, then watch input!
     [(or (member 'create flags)
          (member 'moved-to flags))
      (let* ([channel (inotify:event->pathname event)])
        (print "Attempting to join channel " dirname "…")
        (if join-callback
            (apply join-callback (list path))))])))


(define (channel-joined root-dir channel)
  (let* ([in-path (normalize-pathname (string-append root-dir "/" channel "/.in"))])
    (inotify:add-watch! in-path '(moved-to close-write))
    (print "Began watching input " in-path ".")))


;; Handles an inotify event that pertains to a channel's .in/ directory
(define (handle-channel-dir-event callbacks-alist event)
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
             (apply send-message (list channel message)))
           (with-input-from-file (inotify:event->pathname event)
             read-lines))
      (delete-file* (inotify:event->pathname event))])))


;; Handle a single inotify file event, as part of the input loop
(define (handle-file-event root-dir callbacks-alist event)
  (if (not (member 'ignored (event-flags event)))
      (let* ([flags (inotify:event-flags event)]
             [wd-path (inotify:wd->path (inotify:event-wd event))]
             [main-dir? (string=? wd-path root-dir)])
        (if main-dir?
            (handle-main-dir-event root-dir callbacks-alist event)
            (handle-channel-dir-event root-dir callbacks-alist event)))))


;; The FS-backed input loop, to be run in a seperate thread (so as to not block)
;; This handles channel leaving/joining, and sending messages
;; Call-backs that should be provided:
;;    (channel-joined channel)
;;    (new-message channel text)
(define (input-loop root-dir callbacks-alist)
  (map (lambda (event)
         (handle-file-event root-dir callbacks-alist event))
       (inotify:next-events!))

  (input-loop root-dir callbacks-alist))



;; ——————————————————————————————————————————————————
;; inotify utility
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

;; Repeat after me:
;; 🎵 Symbolic links cannot have extended attributes, and that is a war-crime. 🎶
;; 🎵 Directories cannot have extended attributes, and that is a war-crime. 🎶



;; ——————————————————————————————————————————————————
;; Directory as key/value store
;; ——————————————————————————————————————————————————

;; Set the contents of a directory's file `key` to `value`, setting any
;; extended attributes passed as xattr-alist.
(define (directory-file-set! directory key value #!optional (xattr-alist '()))
  (let ([path (subpath directory key)])
    ;; Write the contents (value)
    (cond [(string? value)
           (write-string-to-file path value)]
          [(input-port? value)
           (write-port-to-file path value)]
          [(list? value)
           (write-byte-list-to-file path value)])

    ;; Write the xattrs (if applicable)
    (map (lambda (xattr-cons)
           (xattr:set-xattr path (symbol->string (car xattr-cons))
                            (cdr xattr-cons)))
         xattr-alist)))

;; Get the contents of the given file as astring.
(define (directory-file-get directory key)
  (read-file-to-string (subpath directory key)))


;; Get the contents of the given file as a string, including the all
;; extended attributes as an alist.
;; (contents (xattr . value) (xattr .value) …)
(define (directory-file-get* directory key)
  (let ([path (subpath directory key)])
    (cons (directory-file-get directory key)
          (map (lambda (xattr)
                 (cons (string->symbol xattr)
                       (xattr:get-xattr path xattr)))
               (xattr:list-xattrs path)))))


;; Given a directory and a filename, return a unique filename by appending
;; a number to the end of the name, as necessary.
(define (directory-unique-file directory name #!optional (suffix ""))
  (let* ([leaf
          (string-append name suffix)]
         [path
          (subpath directory leaf)])
    (if (file-exists? path)
        (directory-unique-file
         directory
         leaf
         (number->string (+ (or (string->number suffix) 0)
                            .1)))
        leaf)))



;; ——————————————————————————————————————————————————
;; Misc. utility
;; ——————————————————————————————————————————————————

;; Return a file path with the given parameters as elements of the path
;; E.g., "/etc/", "/systemd/user" "mom" => "/etc/systemd/user/mom"
(define (subpath . children)
  (normalize-pathname
   (reduce-right (lambda (a b)
                   (string-append a "/" b))
                 "" children)))

;; Wrapper around `directory` that lists a dir's files as a relative path
(define (directory-rel #!optional (path "./"))
  (let ([relative-parent (normalize-pathname (string-append path "/"))])
    (map (lambda (leaf)
           (string-append relative-parent leaf))
         (directory path))))


;; Title says all, I'd hope.
(define (write-string-to-file file value)
  (call-with-output-file file
    (lambda (out-port)
      (write-string value #f out-port))))


;; Again, self-evident. Right?
(define (write-port-to-file path in-port)
  (call-with-output-file path
    (lambda (out-port)
      (copy-port in-port out-port read-byte write-byte))))


;; Still obvious, no?
(define (write-byte-list-to-file path byte-list)
  (call-with-output-file path
    (lambda (out-port)
      (map (lambda (byte)
             (write-char byte out-port))
           byte-list))))


;; And we're still on the same page, I'd hope?
(define (read-file-to-string file)
  (call-with-input-file file
    (lambda (in-port)
      (read-string #f in-port))))

