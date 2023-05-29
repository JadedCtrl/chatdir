#!/bin/sh
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――
# Name: chatdir-in
# Desc: A simple wrapper for sending messages in a chatdir, w autocomplete
#       and all that sweet stuff.
# Reqs: rwlrap, shell
# Date: 2023-02-14
# Lisc: CC0; jadedctrl <jadedctrl@posteo.at>
#―――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――――

# Create the auto-complete file used by rlwrap, containing users anc channel-names
prep_auto_complete() {
	local file="$1"

	ls "$CHATDIR" \
	   > "$file"

	if test -n "$CHANNEL"; then
		ls "$CHATDIR/$CHANNEL/.users/online/" \
		   >> "$file"
	fi
}


# Take in user-input with the oh-so-pretty rlwrap
# seriously, <3<3,3 rlwrap I love you marry me
# have my babies rlwrap! have my babies!
# we'll live in a secluded cottage, with nary a
# bad piece of code. it'll be a very sugary life,
# my dearest rlwrap. just you and I in the world,
# no-one else
get_input() {
	prep_auto_complete "$AUTO_COMPLETE_FILE"

	rlwrap --one-shot \
		   --substitute-prompt='» ' \
		   --break-chars "$(printf '\n')" \
		   --extra-char-after-completion='' \
		   --file "$AUTO_COMPLETE_FILE" \
		   cat
}


# Find out if the user is selecting a channel, or sending a message; act appropriately.
parse_input() {
	local input="$1"

	if test -d "$CHATDIR/$input" -a -d "$CHATDIR/$input/.in/"; then
		set_channel "$input"
	elif test -n "$input"; then
		send_message "$input"
	fi
}


# Send a message to the selected channel, and echo it
send_message() {
	local message="$1"

	if test -z "$CHANNEL"; then
		echo "No channel set; type in its name verbatim, first."
	else
		echo "$message" \
			 > "$CHATDIR/$CHANNEL/.in/input"
		echo "$CHATDIR/$CHANNEL ← $message"
	fi
}


# Set the selected channel, and brag about it
set_channel() {
	local channel="$1"

	if test -d "$CHATDIR/$channel" -a -d "$CHATDIR/$channel/.in/"; then
		CHANNEL="$channel"
		echo "— $CHANNEL —"
	fi
}


help() {
	echo "usage: $(basename "$0") CHATD_DIR"
	echo ""
	echo "Chatdir daemons send plain-text files placed in .in/ directories as messages."
	echo "This script is a wrapper around that: Just type in a message and hit ENTER"
	echo "to send it to the currently selected channel. To select a channel, type in"
	echo "a line solely containing the verbatim channel name."
	echo "TAB-completion is supported, for both channel-names and for online users."
	echo "rlwrap is used for input."
	exit 2
}


if test ! -d "$1"; then
	help
fi

echo "~~ $(basename $0) ~~"
echo "No channel selected."
echo "Select a channel by typing in its name verbatim!"

CHATDIR="$1"
CHANNEL=""
AUTO_COMPLETE_FILE="$(mktemp --suffix=.chatd-in)"


INPUT=""
while INPUT="$(get_input)"; do
	parse_input "$INPUT"
done


rm "$AUTO_COMPLETE_FILE"
