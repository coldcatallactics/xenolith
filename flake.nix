{
  description = "Xenolith: an opinionated Autolith distribution";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

      # Autolith asserts that its pinned SBCL matches the interpreter in its
      # own nixpkgs, so that input keeps its lock instead of following ours.
      autolith.url = "github:lambda-symbolics/autolith";
  };

  outputs = { self, nixpkgs, autolith }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      forAllSystems = function:
        nixpkgs.lib.genAttrs systems (system: function {
          inherit system;
          pkgs = nixpkgs.legacyPackages.${system};
        });

      siteFor = pkgs: pkgs.runCommand "xenolith-site" { } ''
        mkdir -p "$out"
        cp -R ${./site}/. "$out"
        chmod -R u+w "$out"
      '';
    in
    {
      packages = forAllSystems ({ system, pkgs }:
        let
          site = siteFor pkgs;
          upstream = autolith.packages.${system}.default;
        in
        {
          inherit site;

          default = pkgs.writeShellApplication {
            name = "xenolith";
            text = ''
              export AUTOLITH_SITE_CONFIG_ROOT="${site}"
              exec ${upstream}/bin/autolith "$@"
            '';
            meta = {
              description = "Autolith with the Xenolith site configuration";
              mainProgram = "xenolith";
              license = pkgs.lib.licenses.isc;
              homepage = "https://github.com/coldcatallactics/xenolith";
            };
          };
        });

      apps = forAllSystems ({ system, ... }: {
        default = {
          type = "app";
          program = "${self.packages.${system}.default}/bin/xenolith";
          meta.description = "Run Xenolith";
        };
      });

      homeManagerModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.xenolith;
        in
        {
          options.programs.xenolith.enable =
            lib.mkEnableOption "the Xenolith Autolith distribution";

          config = lib.mkIf cfg.enable {
            home.packages =
              [ self.packages.${pkgs.stdenv.hostPlatform.system}.default ];
          };
        };

      nixosModules.default = { config, lib, pkgs, ... }:
        let
          cfg = config.programs.xenolith;
        in
        {
          options.programs.xenolith.enable =
            lib.mkEnableOption "the Xenolith Autolith distribution";

          config = lib.mkIf cfg.enable {
            environment.systemPackages =
              [ self.packages.${pkgs.stdenv.hostPlatform.system}.default ];
          };
        };
    };
}
