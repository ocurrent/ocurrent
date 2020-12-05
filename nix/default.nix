{ pkgs, stdenv, opam2nix }:
let
	# TODO update to use `make`
	args = {
		inherit (pkgs.ocaml-ng.ocamlPackages_4_10) ocaml;
		selection = ./opam-selection.nix;
		src = ../.;
	};
	opam-selection = opam2nix.build args;
	resolve = opam2nix.resolve args [
		"current.opam"
		"current_ansi.opam"
		"current_docker.opam"
		"current_examples.opam"
		"current_git.opam"
		"current_github.opam"
		"current_web.opam"
	];
in
{
	selection = opam-selection;
	buildInputs = opam2nix.buildInputs args;
	inherit (opam-selection) current;
	inherit resolve;
	inherit pkgs;
}
