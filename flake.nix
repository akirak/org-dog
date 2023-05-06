{
  description = "org-dog";

  inputs = {
    gnu-elpa = {
      url = "git+https://git.savannah.gnu.org/git/emacs/elpa.git?ref=main";
      flake = false;
    };
    melpa = {
      # Temporarily use my personal branch
      url = "github:akirak/melpa/akirak";
      flake = false;
    };

    nomake = {
      url = "github:emacs-twist/nomake";
      inputs.gnu-elpa.follows = "gnu-elpa";
      inputs.melpa.follows = "melpa";
    };
  };

  nixConfig = {
    extra-substituters = [
      "https://emacs-ci.cachix.org"
    ];
    extra-trusted-public-keys = [
      "emacs-ci.cachix.org-1:B5FVOrxhXXrOL0S+tQ7USrhjMT5iOPH+QN9q0NItom4="
    ];
  };

  outputs =
    { self
    , nomake
    , ...
    } @ inputs:
    nomake.lib.mkFlake {
      src = ./.;
      localPackages = [
        "org-dog"
        "org-dog-embark"
        "org-dog-facade"
        "consult-org-dog"
        "octopus"
      ];

      scripts.test = {
        extraPackages = [
          "buttercup"
        ];
        text = ''
          emacs -batch -l buttercup -L . -f buttercup-run-discover
        '';
      };
    };
}
