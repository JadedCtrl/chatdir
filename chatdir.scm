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

(module chatdir
(channels channel-add! channel-cleanup!
 channel-metadata-set! channel-metadata-get channel-metadata-get* channel-metadata
 user-add! user-file-set! user-file-get
 user-enable-state! user-disable-state! user-toggle-states!
 channel-users channel-user-add! channel-user-file-set! channel-user-file-get
 channel-user-disable-state! channel-user-enable-state! channel-user-toggle-states!
 channel-message-add! channel-messages channel-message-get
 channel-messages-by-xattr channel-messages-by-sender
 channel-messages-by-date channel-messages-by-date* channel-messages-by-date-range
 )


(import scheme
        (chicken file) (chicken file posix) (chicken pathname) (chicken port)
        (chicken io) (chicken random) (chicken string)
        srfi-1 srfi-13  srfi-19
        (prefix xattr xattr:))


;; ——————————————————————————————————————————————————
;; Channel management
;; ——————————————————————————————————————————————————

;; Lists all currently-joined channels.
(define (channels root)
  (append (directory root) '(".server")))


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
         (user-global-path (subpath user-path "global"))
         [g-user-path (subpath root ".users" g-name)]
         [g-local-path (subpath g-user-path "local" channel)])
    (cond [(or (file-exists? user-path) (directory-exists? user-path)
               (symbolic-link? user-path))
           #f]

          ;; If global, we gotta do some symlink dancing.
          [global?
           (user-add! root g-name)
           (if global-pairity?
               (create-symbolic-link (subpath "../../../.users" g-name) user-path)
               (create-directory user-path #t))
           (if (not (symbolic-link? user-global-path))
               (create-symbolic-link (subpath "../../../../.users" g-name)
                                     user-global-path))
           (if (not (symbolic-link? g-local-path))
               (create-symbolic-link (subpath "../../../" channel ".users" "all" username)
                                     g-local-path))]

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
  (filter
   (lambda (file)
     (let ([path (subpath root channel file)])
       (and (file-exists? path)
            (not (directory-exists? path)))))
   (directory (subpath root channel))))


;; Return a message's whole data.
(define (channel-message-get root channel message)
  (directory-file-get* (subpath root channel) message))


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
;; Directory as key/value store
;; ——————————————————————————————————————————————————

;; Set the contents of a directory's file `key` to `value`, setting any
;; extended attributes passed as xattr-alist.
(define (directory-file-set! directory key value #!optional (xattr-alist '()))
  (let ([path (subpath (create-directory directory #t)
                       key)])
    ;; Write the contents (value)
    (cond [(string? value)
           (write-string-to-file path value)]
          [(input-port? value)
           (write-port-to-file path value)]
          [(list? value)
           (write-byte-list-to-file path value)]
          ;; If no data sent (e.g., value is #f), at least make the file!
          [(not (file-exists? path))
           (write-string-to-file path "")])

    ;; Write the xattrs (if applicable)
    (map (lambda (xattr-cons)
           (xattr:set-xattr path (symbol->string (car xattr-cons))
                            (cdr xattr-cons)))
         xattr-alist)))

;; Get the contents of the given file as astring.
(define (directory-file-get directory key)
  (let ([path (subpath directory key)])
    (if (and (file-exists? path)
             (not (directory-exists? path)))
        (read-file-to-string (subpath directory key))
        #f)))


;; Get the contents of the given file as a string, including the all
;; extended attributes as an alist.
;; (contents (xattr . value) (xattr .value) …)
(define (directory-file-get* directory key)
  (let ([path (subpath directory key)]
        [contents (directory-file-get directory key)])
    (if contents
        (cons contents
              (map (lambda (xattr)
                     (cons (string->symbol xattr)
                           (xattr:get-xattr path xattr)))
                   (xattr:list-xattrs path))))))


;; Given a directory and a filename, return a unique filename by appending
;; a number to the end of the name, as necessary.
(define (directory-unique-file directory name #!optional (suffix ""))
  (let* ([leaf
          (string-append name (if (not (string-null? suffix)) "." "")
                         suffix)]
         [path
          (subpath directory leaf)])
    (if (file-exists? path)
        (directory-unique-file
         directory
         name
         (string-pad
           (number->string (+ (or (and (string? suffix)
                                       (string->number suffix))
                                  0)
                              1))
           4 #\0))
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

) ;; chatdir module
