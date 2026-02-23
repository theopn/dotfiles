# CAOS is a cringe name I gave to my productivity system
# (Capture Act Organize Sustain).
# I mainly use nvim-org-mode for task management ("Capture", "Act", and "Organize" parts),
# and various shell scripts + Vimwiki for journaling and habit tracking ("Sustain" part)


# personal journal entry for the day
# - If I just pass the subshell without escaping (i.e., `+/$(date)`),
#   then Fish will evaluate in the shell startup and the fixed date.
# - I do not want to overcomplicate the command so I keep the first one as-is
#   (since year changing does not happen very often),
#   but I do not want to reopen the shell every 24 hours
abbr -a dw vim \"$CACHE_DIR/dw-$(date +%Y)/index.md\" +/\$\(date +%Y-%m-%d\)

# EOD checklist ("Roll call")
abbr -a call 'test -f $CACHE_DIR/habits-(date +%Y)/(date +%Y-%m-%d)-roll-call.md; \
  or cat $CACHE_DIR/habits-(date +%Y)/assets/call-template.md | string replace "YYYY-MM-DD" (date +%Y-%m-%d) >> $CACHE_DIR/habits-(date +%Y)/(date +%Y-%m-%d)-roll-call.md; \
  vim $CACHE_DIR/habits-(date +%Y)/(date +%Y-%m-%d)-roll-call.md
'

