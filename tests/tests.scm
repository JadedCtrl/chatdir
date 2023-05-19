(import (chicken sort) srfi-78)
(load "../chatdir.scm")

(define *dir* "test chatdir")

;; ——————————————————————————————————————————————————
;; Helper functions for making tests
;; ——————————————————————————————————————————————————
;; Appends six random chars
(define (randomize-string str)
  (string-append str " "
				 (random-bytes "ABC123" 6)))



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
(check (directory-file-get* (subpath *dir* *room*) *msg-name*)
	   =>
	   (list *msg-text*
			 (cons 'user.chat.date (date->string *msg-date* "~1T~2"))
			 *msg-xattr*
			 (cons 'user.chat.sender *msg-sender*)
			 (cons 'user.chat.channel *room*)))


(define *msg-sender-2* "bildinto")
(define *msg-text-2* "he? ĉu vi bonsanas?")
(define *msg-date-2* *msg-date*)
(define *msg-name-2* (message-file-leaf *dir* *room* *msg-date-2*))

(channel-message-add! *dir* *room* *msg-text-2* *msg-sender-2* *msg-date-2*)
(check (directory-file-get* (subpath *dir* *room*) *msg-name-2*)
	   =>
	   (list *msg-text-2*
			 (cons 'user.chat.date (date->string *msg-date-2* "~1T~2"))
			 (cons 'user.chat.sender *msg-sender-2*)
			 (cons 'user.chat.channel *room*)))



;; ——————————————————————————————————————————————————
;; Channel creation/management
;; ——————————————————————————————————————————————————
(define *new-room* "living room")
(define *new-room-path* (subpath *dir* *new-room*))
(define *new-room-all* (subpath *new-room-path* ".users" "all"))
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
(check-report)
