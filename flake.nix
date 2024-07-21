{
  outputs = {...}: {
    elisp-rice = {
      packages = [
        "org-dog"
        "org-dog-facade"
        "org-dog-embark"
        "consult-org-dog"
        "octopus"
        # This package is not usable yet
        # "org-dog-export"
      ];
      tests = {
        buttercup.enable = true;
      };
    };
  };
}
