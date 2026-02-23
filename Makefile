.PHONY: switch
switch:
	home-manager switch --flake .#theopn

.PHONY: clean
clean:
	nix-collect-garbage -d

.PHONY: update
update:
	nix flake update
	home-manager switch --flake .#theopn
