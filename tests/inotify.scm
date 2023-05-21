(load "../chatdir-inotify.scm")

(define (join-callback channel)
  (print "Joined " channel "! ^_^")
  (channel-joined "test chatdir" channel))

(define (leave-callback channel)
  (print "We've left " channel " </3>"))

(define (send-message-callback channel message)
  (print "Sent message to " channel ": " message))


(define *callbacks*
  (list (cons 'join-channel join-callback)
		(cons 'leave-channel leave-callback)
		(cons 'send-message send-message-callback)))


(input-loop-init "test chatdir" *callbacks*)
(input-loop "test chatdir" *callbacks*)
