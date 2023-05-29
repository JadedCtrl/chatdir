#!/bin/sh
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# Name: chat_file_notify
# Desc: Watches a directory for any newly-created files, and then sends a
#       notification!
# Reqs: inotifywait, notify-send, shell
# Date: 2022
# Lisc: CC0; jadedctrl <jadedctrl@posteo.at>
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

notification_title() {
	local file="$1"
	local sender="$(attr -qg chat.sender "$file")"
	local channel="$(basename "$(dirname "$file")")"

	echo "$sender [$channel]"
}


notification_message() {
	local file="$1"
	local message="$(head -1 "$file")"

	echo "$message"
}


monitor_dirs() {
	inotifywait --format "%w%f" \
				--event MOVED_TO,CREATE \
				$@
}


TARGET="$1"

if test -z "$IRC_ICON"; then
	# Set this variable, or you'll get my preferred icon. Suffer! c:<
	IRC_ICON="$XDG_DATA_HOME/icons/retrosmart-icon-theme/scalable/chat.svg"
fi


if test -z "$TARGET"; then
	echo "usage: $(basename "$0") CHAT_DIRECTORY"
	echo ""
	echo "Monitors a specific channel's directory for any new messages, sending"
	echo "a notification when one is receieved."
	exit 2
fi


while test 1 -eq 1; do
	FILE="$(monitor_dirs $@)"
	notify-send \
		--app-name="$(basename "$0")" \
		--icon "$IRC_ICON" \
		"$(notification_title "$FILE")" \
		"$(notification_message "$FILE")"

done
