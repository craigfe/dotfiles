#!/bin/bash
set -euo pipefail

ERRORS=()

# find all executables and run `shellcheck`
for f in $(git ls-files | sort -u); do
	  if file "$f" | grep -q shell; then
		    {
			      shellcheck "$f" && echo "[OK]: successfully linted $f"
		    } || {
			      # add to errors
			      ERRORS+=("$f")
		    }
	  fi
done

if [ ${#ERRORS[@]} -eq 0 ]; then
	  echo "No errors, hooray"
else
	  echo "These files failed shellcheck: ${ERRORS[*]}"
	  exit 1
fi
