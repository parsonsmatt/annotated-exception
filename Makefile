ghcid:
	ghcid \
	    --command "stack ghci annotated-exception:lib" \
	    --restart package.yaml

ghcid-test:
	ghcid \
	    --command "stack ghci annotated-exception:lib annotated-exception:test" \
	    --restart package.yaml \
	    --test "main"

.PHONY: ghcid ghcid-test
