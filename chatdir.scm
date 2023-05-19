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
		(chicken file) (chicken file posix) (chicken io) (chicken keyword) (chicken random)
		(chicken pathname)
		(chicken pretty-print) (chicken process-context)
		(chicken process-context posix) (chicken string)
		srfi-1 srfi-13 srfi-18 srfi-19 srfi-69 srfi-180
		inotify
		xattr
		getopt-long)


;; Wrapper around `directory` that lists a dir's files as a relative path
(define (directory-rel #!optional (path "./"))
  (let ([relative-parent (normalize-pathname (string-append path "/"))])
	(map (lambda (leaf)
		   (string-append relative-parent leaf))
		 (directory path))))


;; Returns an inotify watch-descriptor according the given path
(define (path->wd path)
  (car
   (filter (lambda (wd)
			 (string=? (normalize-pathname (wd->path wd))
					   (normalize-pathname path)))
		   (wd-list))))


;; Attempt to remove an inotify watch; if it's already been removed, no sweat
;; (This happens sometimes when inotify automatically deletes a watch)
(define (attempt-remove-watch! watch)
  (handle-exceptions exn
	  #t
	(remove-watch! watch)))


;; Returns the path of a room's directory
(define (channel-directory-path conn channel)
  (let ([dir (hash-table-ref conn 'directory)])
	(if (and (string? dir) (string? channel))
		(string-append dir "/" channel "/"))))


;; Returns the .users/ path of a channel
(define (channel-users-directory-path conn channel)
  (string-append (channel-directory-path conn channel)
				 ".users/"))


;; Main directory path of the given user
(define (channel-user-directory-path conn channel hostmask #!optional (state "all"))
  (string-append (channel-users-directory-path conn channel)
				 state "/" (irc:hostmask-nick hostmask)))


;; Main directory path of the given user
(define (user-directory-path conn channel hostmask)
  (string-append (channel-users-directory-path conn channel)
				 "all/" hostmask))


;; Tidies up a channel directory; removes `online` and `offline` user links, etc.
(define (cleanup-channel conn channel)
  (let ([users-dir (channel-users-directory-path conn channel)])
	(map
	 (lambda (state-dir)
	   (if (not (substring-index state-dir "/all"))
		   (map
			(lambda (link)
			  (let ([link-path (string-append users-dir state-dir "/" link)])
				(if (symbolic-link? link-path)
					(delete-file link-path))))
			(directory (string-append users-dir state-dir)))))
	   (directory users-dir))))


;; Creates a channel's file hierarchy, if need be
(define (make-channel conn channel)
  (let* ([path (channel-directory-path conn channel)]
		 [subpath (lambda (leaf) (string-append path leaf))])
	(create-directory (subpath ".in") #t)
	(create-directory (subpath ".users/online") #t)
	(create-directory (subpath ".users/offline") #t)
	(create-directory (subpath ".users/all") #t)
	(cleanup-channel conn channel)))


;; Creates a user's info files in the given channel, if need bee
(define (make-user conn channel hostmask)
  (create-directory (user-directory-path conn channel hostmask) #t))


;; Disables a user-state (that is, removes a symlink from a .users directory
(define (user-disable-state conn channel hostmask state)
  (let ([state-link
		 (create-directory (channel-user-directory-path conn channel hostmask state) #t)])
	(if (or (file-exists? state-link)
			(symbolic-link? state-link))
		(delete-file state-link))))


;; Enables a user-state (that is, makes a symlink to a .users directory
(define (user-enable-state conn channel hostmask state)
  (let ([state-link
		 (create-directory (channel-user-directory-path conn channel hostmask state) #t)])
	(if (not (or (file-exists? state-link)
				 (symbolic-link? state-link)))
		(create-symbolic-link (string-append "../all/" hostmask)
							  state-link))))


;; Ensures the enabled-state is enabled, and it's opposite (disabled-state) is not
(define (user-toggle-state conn channel hostmask enabled-state disabled-state)
  (user-disable-state conn channel hostmask disabled-state)
  (user-enable-state conn channel hostmask enabled-state))


;; Sets a channel's .topic file
(define (set-channel-topic conn channel topic #!optional (username #f) (date #f))
  (let ([topic-path (string-append (channel-directory-path conn channel)
								   ".topic")])
	(if (string? topic)
		(call-with-output-file
			topic-path
		  (lambda (out-port)
			(write-string topic #f out-port))))
	(if username
		(set-xattr topic-path "user.chat.sender" (irc:hostmask-nick username)))))


;; Send message to an IRC channel
(define (send-message connection channel message)
  (irc:write-cmd connection "PRIVMSG" channel message)
  (make-message-file connection channel
					 (hash-table-ref connection 'nick)
					 message))


;; Hook function for irc:loop; handles all IRC commands
(define (on-command conn cmd params #!optional sender)
  (cond
   [(and (string=? cmd "PRIVMSG")
		 (string? sender)
		 (irc:hostmask? sender))
	(let ([target (if (irc:user-is-self? conn (car params))
					  (irc:hostmask-nick sender)
					  (car params))])
	  (make-message-file conn target (irc:hostmask-nick sender) (last params)))]

   [(or (string=? cmd "NOTICE")
		(and (string=? cmd "PRIVMSG")
			 (or (string-null? sender) (not (irc:hostmask? sender)))))
	(make-message-file conn ".server" "server" (last params))]

   [(and (string=? cmd "JOIN") (irc:user-is-self? conn sender))
	(make-channel conn (last params))]

   [(string=? cmd "JOIN")
	(make-user conn (last params) sender)]

;;   [(string=? cmd "NICK")
;;	(chatd-json-write conn
;;	 (compose-event-alist conn "user-info" #:user (last params)))])
))


;; Hook function for irc:loop; handles all IRC errors and replies
(define (on-reply conn reply params #!optional sender)
  (cond
	  ;; If topic set, output to a channel's .topic file
	  [(and (eq? reply RPL_TOPIC)
			(irc:channel? (second params)))
	   (set-channel-topic conn (second params) (last params))]

	  [(and (eq? reply RPL_TOPICWHOTIME)
			(irc:channel? (second params)))
	   (set-channel-topic conn (second params) #f (third params) (last params))]

	  ;; We've got to add users, when they join the room!
	  [(or (and (irc:capability? conn 'userhost-in-names)
				(eq? reply RPL_ENDOFNAMES))
		   (eq? reply RPL_ENDOFWHO))
	   (map (lambda (nick)
			  (let ([hostmask (irc:user-get conn nick 'hostmask)]
					[channel (second params)])
				(make-user conn channel hostmask)
				(user-toggle-state conn channel hostmask "online" "offline")))
			(irc:channel-users conn (second params)))]

	  [#t
	   (make-message-file conn ".server" "server" (last params))]))


(define (write-string-to-file file value)
  (call-with-output-file file
	(lambda (out-port)
	  (write-string value #f out-port))))


(define (write-port-to-file path in-port)
  (call-with-output-file path
	(lambda (out-port)
	  (copy-port in-port out-port read-byte write-byte))))


(define (write-byte-list-to-file path byte-list)
  (call-with-output-file path
	(lambda (out-port)
	  (map (lambda (byte)
			 (write-char byte out-port))
		   byte-list))))


(define (read-file-to-string file)
  (call-with-input-file file
	(lambda (in-port)
	  (read-string #f in-port))))


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
		   (set-xattr path (symbol->string (car xattr-cons))
					  (cdr xattr-cons)))
		 xattr-alist)))



(define (directory-file-get directory key)
  (read-file-to-string (subpath directory key)))


;; Get the contents of the given file as a string, including the 
(define (directory-file-get* directory key)
  (let ([path (subpath directory key)])
	(cons (directory-file-get directory key)
		(map (lambda (xattr)
			   (cons (string->symbol xattr)
					 (get-xattr path xattr)))
			 (list-xattrs path)))))


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


;; Return a file path with the given parameters as elements of the path
;; E.g., "/etc/", "/systemd/user" "mom" => "/etc/systemd/user/mom"
(define (subpath . children)
  (normalize-pathname
   (reduce-right (lambda (a b)
				   (string-append a "/" b))
				 "" children)))


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


;; Finds an appropriate (non-colliding, non-in-use) name for a message file,
;; based on its date.
(define (message-file-leaf root channel date)
  (directory-unique-file (subpath root channel)
						(date->string date "[~m-~d] ~H:~M:~S")))


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
						 (message-file-leaf root channel date)
						 contents attrs)))


;; Initialization for the input loop
(define (input-loop-init root-dir callbacks-alist)
  (let ([join-callback (alist-ref 'join-channel callbacks-alist)])
	(init!)
  ;; Start watching the chatdir (for new channel joins, etc)
	(add-watch! root-dir
				'(onlydir moved-to moved-from delete delete-self create))

	;; Auto-join channels with all pre-existing channel directories
	(map (lambda (path)
		   (let ([channel-dirname (pathname-file (pathname-directory (string-append path "/")))]
				 [join-callback (alist-ref 'join-channel callbacks-alist)])
			 (if join-callback
				 (apply join-callback (list channel-dirname)))

		   (add-watch! in-path '(moved-to close-write))
		   (print "Joined and watching: " in-path)))
	   (filter directory-exists? (directory-rel irc-dir)))))


;; Handles all inotify-watched file events from the top-level IRC-directory.
;; Mainly, checking for newly-joined or left channels.
(define (handle-main-dir-event callbacks-alist event)
  (let ([flags (event-flags event)]
		[leave-callback (alist-ref 'leave-channel callbacks-alist)]
		[join-callback (alist-ref 'join-channel callbacks-alist)])
	(cond
	 ;; If a channel dir's been moved or removed, stop watching (ofc)
	 ;; … Also quit that room! Heck them!
	 [(or (member 'moved-from flags)
		  (member 'delete flags)
		  (member 'delete-self flags))
	  (let* ([channel (event-name event)]
			 [channel-inpath (string-append (wd->path (event-wd event)) channel "/.in")]
			 [channel-wd (path->wd channel-inpath)])
		(print "Remove watch for " channel-inpath "…")
		(if (and channel-wd (member channel-wd (wd-list)))
			(attempt-remove-watch! channel-wd))
		(if leave-callback
			(apply leave-callback (list channel))))]

	 ;; If a dir's been created for a channel, maybe-join, then watch input!
	 [(or (member 'create flags)
		  (member 'moved-to flags))
	  (let* ([channel (event->pathname event)])
		(print "Attempting to join channel " dirname "…")
		(if join-callback
			(apply join-callback (list path))))])))


(define (channel-joined root-dir channel)
  (let* ([in-path (normalize-pathname (string-append root-dir "/" channel "/.in"))])
	(add-watch! in-path '(moved-to close-write))
	(print "Began watching input " in-path ".")))


;; Handles an inotify event that pertains to a channel's .in/ directory
(define (handle-channel-dir-event callbacks-alist event)
  (let* ([event-dir (pathname-directory (event->pathname event))]
		 [dirname (pathname-file event-dir)]
		 [channel (pathname-file (pathname-directory event-dir))]
		 [send-message-callback (alist-ref 'send-message callbacks-alsit)])
	(cond
	 ;; If input is given to an `.in` dir… well, send that darn message!
	 ;; What're you wating for?
	 [(and (string=? dirname ".in")
		   send-message-callback)
	  (map (lambda (message)
			 (apply send-message (list channel message)))
		   (with-input-from-file (event->pathname event)
			 read-lines))
	  (delete-file* (event->pathname event))])))


;; Handle a single inotify file event, as part of the input loop
(define (handle-file-event root-dir callbacks-alist event)
  (if (not (member 'ignored (event-flags event)))
	  (let* ([flags (event-flags event)]
			 [wd-path (wd->path (event-wd event))]
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
	   (next-events!))

  (input-loop root-dir callbacks-alist))

