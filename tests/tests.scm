(import (chicken sort) srfi-78)
(load "../chatdir.scm")


;; ——————————————————————————————————————————————————
;; Helper functions for making tests
;; ——————————————————————————————————————————————————
;; Appends six random chars
(define (randomize-string str)
  (string-append str " "
				 (random-bytes "ABC123" 6)))



;; ——————————————————————————————————————————————————
;; Set up testing environment
;; ——————————————————————————————————————————————————
(define *dir* "test chatdir")
(create-directory "test chatdir/dining room/.meta" #t)



;; ——————————————————————————————————————————————————
;; General-pupose functions
;; ——————————————————————————————————————————————————
(check (subpath "/etc" "systemd/" "user" "momma")
	   =>
	   "/etc/systemd/user/momma")



;; ——————————————————————————————————————————————————
;; Room metadata sets/gets
;; ——————————————————————————————————————————————————
(define *room* "dining room")
(define *room-path* "test chatdir/dining room")
(define *room-meta-path* "test chatdir/dining room/.meta")
(define *room-topic* "Here we can discuss everything relating to food.\nCooking, dining, etc!\nThe only limit is your palette!\n")
(define *room-topic-xattrs* (list (cons 'user.chat.user "admin-tan")
								  (cons 'user.chat.date (number->string (pseudo-random-integer 9999)))))

(directory-file-set! *room-meta-path* "topic" *room-topic* *room-topic-xattrs*)
(check (directory-file-get *room-meta-path* "topic")
	   =>
	   *room-topic*)
(check (directory-file-get* *room-meta-path* "topic")
	   =>
	   (cons *room-topic* *room-topic-xattrs*))


(define *room-topic-2* (randomize-string *room-topic*))
(define *room-topic-xattrs-2* (alist-update 'user.chat.user "admin-mom" *room-topic-xattrs*))

(channel-metadata-set! *dir* *room* "topic"
					   *room-topic-2*
					   *room-topic-xattrs-2*)
(check (channel-metadata-get *dir* *room* "topic")
	   =>
	   *room-topic-2*)
(check (channel-metadata-get* *dir* *room* "topic")
	   =>
	   (cons *room-topic-2* *room-topic-xattrs-2*))



;; ——————————————————————————————————————————————————
;; Message creating/reading
;; ——————————————————————————————————————————————————
(define *msg-sender* "maya")
(define *msg-text* "eee… hiya, papaya!")
(define *msg-date* (current-date))
(define *msg-xattr* '(user.bovo . "muuuu"))
(define *msg-name* (message-file-leaf *dir* *room* *msg-date*))
(channel-message-add! *dir* *room* *msg-text* *msg-sender* *msg-date* `(,*msg-xattr*))

(define *msg-sender-2* "bildinto")
(define *msg-text-2* "he? ĉu vi bonsanas?")
(define *msg-date-2* *msg-date*)
(define *msg-name-2* (message-file-leaf *dir* *room* *msg-date-2*))
(channel-message-add! *dir* *room* *msg-text-2* *msg-sender-2* *msg-date-2*)

(define *msg-sender-3* *msg-sender*)
(define *msg-text-3* "feliĉan novjaron! =w= :D ^_^")
(define *msg-date-3* (string->date "2023-01-01 00:01:00" "~Y-~m-~d ~H:~M:~S"))
(define *msg-name-3* (message-file-leaf *dir* *room* *msg-date-3*))
(channel-message-add! *dir* *room* *msg-text-3* *msg-sender-3* *msg-date-3*)

(define *msg-sender-4* *msg-sender-2*)
(define *msg-text-4* "certainly! :D")
(define *msg-date-4* (string->date "2023-01-02 21:43:09" "~Y-~m-~d ~H:~M:~S"))
(define *msg-name-4* (message-file-leaf *dir* *room* *msg-date-4*))
(channel-message-add! *dir* *room* *msg-text-4* *msg-sender-4* *msg-date-4*)


(check (directory-file-get* (subpath *dir* *room*) *msg-name*)
	   =>
	   (list *msg-text*
			 (cons 'user.chat.date (date->string *msg-date* "~1T~2"))
			 *msg-xattr*
			 (cons 'user.chat.sender *msg-sender*)
			 (cons 'user.chat.channel *room*)))


(check (directory-file-get* (subpath *dir* *room*) *msg-name-2*)
	   =>
	   (list *msg-text-2*
			 (cons 'user.chat.date (date->string *msg-date-2* "~1T~2"))
			 (cons 'user.chat.sender *msg-sender-2*)
			 (cons 'user.chat.channel *room*)))


(check (list (find (lambda (a) (string=? *msg-name* a))
				   (channel-messages *dir* *room*))
			 (find (lambda (a) (string=? *msg-name-2* a))
				   (channel-messages *dir* *room*)))
	   =>
	   (list *msg-name* *msg-name-2*))


(check (list (<= 2 (length (channel-messages-by-sender *dir* *room* "maya")))
			 (find (lambda (a) (string=? *msg-name-3* a))
				   (channel-messages-by-sender *dir* *room* "maya")))
	   =>
	   (list #t *msg-name-3*))


(check (find (lambda (a) (string=? *msg-name-3* a))
			 (channel-messages-by-date *dir* *room* *msg-date-3*))
	   =>
	   *msg-name-3*)


(check (let ([messages
			  (channel-messages-by-date-range *dir* *room* *msg-date-3* *msg-date-4*)])
		 (list (find (lambda (a) (string=? *msg-name-3* a)) messages)
			   (find (lambda (a) (string=? *msg-name-4* a)) messages)))
	   =>
	   (list *msg-name-3* *msg-name-4*))



;; ——————————————————————————————————————————————————
;; Channel creation/management
;; ——————————————————————————————————————————————————
(define *new-room* "living room")
(define *new-room-path* (subpath *dir* *new-room*))
(define *new-room-users* (subpath *new-room-path* ".users"))
(define *new-room-all* (subpath *new-room-users* "all"))
(if (directory-exists? *new-room-path*)
	(delete-directory (subpath *dir* *new-room*) #t))
(channel-add! *dir* *new-room*)
(check (and (directory-exists? *new-room-path*)
			(directory-exists? *new-room-all*))
	   =>
	   *new-room-all*)


(define *new-room-online* (subpath *new-room-path* ".users" "online"))
(create-symbolic-link "./" (subpath *new-room-online* "birdo"))
(create-symbolic-link "./" (subpath *new-room-online* "mondo"))
(check (sort (directory *new-room-online*) string<)
	   =>
	   '("birdo" "mondo"))


(channel-cleanup! *dir* *new-room*)
(check (directory *new-room-online*)
	   =>
	   '())



;; ——————————————————————————————————————————————————
;; User management
;; ——————————————————————————————————————————————————
(define *users-dir* (subpath *dir* ".users"))
(if (directory-exists? *users-dir*)
	(delete-directory *users-dir* #t))
(if (directory-exists? *new-room-users*)
	(delete-directory *new-room-users* #t))

;; Create a global user-directory.
(user-add! *dir* "birdo")
(check (string? (directory-exists? (subpath *dir* ".users" "birdo")))
	   =>
	   #t)


;; Check a room-only account; it has no global directory.
(channel-user-add! *dir* *new-room* "mondo" #f #f)
(check (and (not (directory-exists? (subpath *users-dir* "mondo")))
			(not (symbolic-link? (subpath *new-room-all* "mondo")))
			(string? (directory-exists? (subpath *new-room-all* "mondo"))))
	   =>
	   #t)


;; Check a room user-directory, that matches up one-to-one with a
;; global user-directory. Pairity: That is, the channel user
;; directory is just a link from the global user directory.
;; /.users/birdo → /living room/.users/birdo
(channel-user-add! *dir* *new-room* "birdo" #t #t)
(channel-user-file-set! *dir* *new-room* "birdo" "nick" "rose")
(check (read-symbolic-link (subpath *new-room-all* "birdo"))
	   =>
	   "../../../.users/birdo")
(check (read-symbolic-link (subpath *new-room-all* "birdo" "global"))
	   =>
	   "../../../../.users/birdo")
(check (read-symbolic-link (subpath *users-dir* "birdo" "local" *new-room*))
	   =>
	   (subpath "../../../" *new-room* ".users" "all" "birdo"))
(check (user-file-get *dir* "birdo" "nick")
	   =>
	   "rose")


;; Check a room user-directory with corresponding global user-directory,
;; but without the above link/pairity.
(channel-user-add! *dir* *room* "mawa" #t #f)
(channel-user-add! *dir* *new-room* "mawa" #t #f)
(channel-user-file-set! *dir* *new-room* "mawa" "nick" "mawarth")
(user-file-set! *dir* "mawa" "nick" "magma")
(check (and (not (symbolic-link? (subpath *new-room-all* "mawa")))
			(symbolic-link? (subpath *new-room-all* "mawa" "global"))
			(directory-exists? (subpath *new-room-all* "mawa"))
			(string? (directory-exists? (subpath *users-dir* "mawa")))
			)
	   =>
	   #t)
(check (user-file-get *dir* "mawa" "nick")
	   =>
	   "magma")
(check (channel-user-file-get *dir* *new-room* "mawa" "nick")
	   =>
	   "mawarth")


;; Make sure user-states (online/offline) work!
(channel-user-enable-state! *dir* *new-room* "mawa" "online")
(check (read-symbolic-link (subpath *new-room-users* "online" "mawa"))
	   =>
	   "../all/mawa")

(channel-user-toggle-states! *dir* *new-room* "mawa" "offline" "online")
(check (list (symbolic-link? (subpath *new-room-users* "online" "mawa"))
			 (read-symbolic-link (subpath *new-room-users* "offline" "mawa")))
	   =>
	   '(#f "../all/mawa"))

(channel-user-disable-state! *dir* *new-room* "mawa" "offline")
(check (symbolic-link? (subpath *new-room-users* "offline" "mawa"))
	   =>
	   #f)

(user-enable-state! *dir* "mawa" "online")
(check (list (symbolic-link? (subpath *new-room-users* "online" "mawa"))
			 (symbolic-link? (subpath *dir* *room* ".users" "online" "mawa")))
	   =>
	   '(#t #t))


;; ——————————————————————————————————————————————————
(check-report)
