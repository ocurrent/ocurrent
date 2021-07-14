with import ./default.nix {};
with pkgs;
mkShell {
	buildInputs = buildInputs ++ [ graphviz ];
}
