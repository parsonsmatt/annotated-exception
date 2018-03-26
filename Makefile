ghcid:
	ghcid \
	    --command "stack ghci located-exception:lib" \
	    --restart package.yaml

ghcid-test:
	ghcid \
	    --command "stack ghci located-exception:lib" \
	    --restart package.yaml \
	    --test "main"

.PHONY: ghcid ghcid-test
