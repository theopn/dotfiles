.PHONY: update
update:
	home-manager switch --flake .#theopn

.PHONY: clean
clean:
	nix-collect-garbage -d
