#compdef pass

# Copyright (C) 2012:
#    Johan Venant <jvenant@invicem.pro>
#    Brian Mattern <rephorm@rephorm.com>
#    Jason A. Donenfeld <Jason@zx2c4.com>.
# All Rights Reserved.
# This file is licensed under the GPLv2+. Please see COPYING for more information.

_pass () {
	local cmd
	if (( CURRENT > 2)); then
		cmd=${words[2]}
		# Set the context for the subcommand.
		curcontext="${curcontext%:*:*}:pass-$cmd"
		# Narrow the range of words we are looking at to exclude `pass'
		(( CURRENT-- ))
		shift words
		# Run the completion for the subcommand
		case "${cmd}" in
			init)
				_arguments : \
					"-r[re-encrypt existing passwords]" \
					"--reencrypt[re-encrypt existing passwords]"
				_pass_complete_keys
				;;
			ls|list|edit)
				_pass_complete_entries_with_subdirs
				;;
			insert)
				_arguments : \
					"-e[echo password to console]" \
					"--echo[echo password to console]" \
					"-m[multiline]" \
					"--multiline[multiline]"
				_pass_complete_entries_with_subdirs
				;;
			generate)
				_arguments : \
					"-n[don't include symbols in password]" \
					"--no-symbols[don't include symbols in password]" \
					"-c[copy password to the clipboard]" \
					"--clip[copy password to the clipboard]"
				_pass_complete_entries_with_subdirs
				;;
			rm)
				_arguments : \
					"-f[force deletion]" \
					"--force[force deletion]" \
					"-r[recursively delete]" \
					"--recursive[recursively delete]"
					_pass_complete_entries_with_subdirs
				;;
			git)
				local -a subcommands
				subcommands=(
					"init:Initialize git repository"
					"push:Push to remote repository"
					"pull:Pull from remote repository"
					"config:Show git config"
					"log:Show git log"
					"reflog:Show git reflog"
				)
				_describe -t commands 'pass git' subcommands
				;;
			show|*)
				_pass_cmd_show
				;;
		esac
	else
		local -a subcommands
		subcommands=(
			"init:Initialize new password storage"
			"ls:List passwords"
			"show:Decrypt and print a password"
			"insert:Insert a new password"
			"generate:Generate a new password using pwgen"
			"edit:Edit a password with \$EDITOR"
			"rm:Remove the password"
			"git:Call git on the password store"
			"version:Output version information"
			"help:Output help message"
		)
		_describe -t commands 'pass' subcommands
		_arguments : \
			"--version[Output version information]" \
			"--help[Output help message]"
		_pass_cmd_show
	fi
}

_pass_cmd_show () {
	_arguments : \
		"-c[put it on the clipboard]" \
		"--clip[put it on the clipboard]"
	_pass_complete_entries
}
_pass_complete_entries_helper () {
	local IFS=$'\n'
	local prefix="${PASSWORD_STORE_DIR:-$HOME/.password-store}"
	_values -C 'passwords' $(find "$prefix" \( -name .git -o -name .gpg-id \) -prune -o $@ -print | sed -e "s#${prefix}.##" -e 's#\.gpg##' | sort)
}

_pass_complete_entries_with_subdirs () {
	_pass_complete_entries_helper
}

_pass_complete_entries () {
	_pass_complete_entries_helper -type f
}

_pass_complete_keys () {
	local IFS=$'\n'
	# Extract names and email addresses from gpg --list-keys
	_values 'gpg keys' $(gpg2 --list-secret-keys --with-colons | cut -d : -f 10 | sort -u | sed '/^$/d')
}
