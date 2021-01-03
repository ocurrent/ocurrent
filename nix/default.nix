{ lib, pkgs, stdenv, opam2nix, ocluster }:
let
	# TODO update to use `make`
	args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_10) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
	opam-selection = opam2nix.build args;
	override = { pkgs, selection}: {
		current_ocluster = super: super.overrideAttrs (o: {
			src = ocluster;
		});
	};
	resolve = opam2nix.resolve args [
		"current.opam"
		"current_ocluster"
		"current_ansi.opam"
		"current_docker.opam"
		"current_examples.opam"
		"current_git.opam"
		"current_github.opam"
		"current_web.opam"
	];
	# current_ocluster depends on current, but we want to use the local version
	buildInputs = (lib.filter
		(drv: !(lib.hasPrefix "current" drv.name))
		(opam2nix.buildInputs args))
		++ [ opam-selection.current_ocluster ]	;
in
{
	selection = opam-selection;
	inherit buildInputs;
	inherit (opam-selection) current;
	inherit resolve;
	inherit pkgs;
}
