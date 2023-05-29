#!/bin/sh
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# Name: proc_notify
# Desc: Bad script that checks over and over and over if a program is running,
#       by greping `ps ax` w the passed arg. If the program isn't running,
#       send an URGENT notification, telling the user.
# Reqs: notify-send, shell
# Date: 2023
# Lisc: CC0; jadedctrl <jadedctrl@posteo.at>
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

watch_loop() {
	local program="$1"
	local us="$2"
	local last_id="$3"
	if test -z "$last_id"; then
		last_id=13375
	fi

	if test -z "$(ps ax | grep "$program" | grep -ve "grep" -e "$us")"; then
		notify-send --replace-id="$last_id" \
					--print-id \
					--urgency=critical \
					"$program not running!"
	fi
	sleep 30
}


PROGRAM="$1"
LAST_ID=13375

if test -z "$PROGRAM"; then
	echo "usage: $(basename "$0") REGEX"
	echo ""
	echo "Monitors output of `ps ax` for the expect program, defined by regex."
	echo "If said program isn't found, then an URGENT notification will be sent."
	exit 2
fi

while test "life" = "life"; do
	id="$(watch_loop "$PROGRAM" "$0" "$LAST_ID")"
	last_id="$id"
	echo "$last_id"
done
