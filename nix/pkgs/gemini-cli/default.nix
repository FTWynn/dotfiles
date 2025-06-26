{ lib, buildNpmPackage, fetchFromGitHub }:

buildNpmPackage rec {
  pname = "gemini-cli";
  version = "early-access-2025-06-24"; # A descriptive version for this commit

  src = fetchFromGitHub {
    owner = "google-gemini";
    repo = pname;
    # This now points to the exact commit hash you found
    rev = "52afcb3a1233237b07aa86b1678f4c4eded70800";
    # The hash must be updated to match the new 'rev'
    hash = "sha256-4O5iMvXgQjvyGuA8cZ0Kk7gLq/dYJ1b+Q+hJ44WjHkU=";
  };

  npmDepsHash = lib.fakeHash; # This part remains the same for now

  meta = with lib; {
    description = "A command-line interface for Google's Gemini models";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = licenses.asl20; # Apache 2.0 License
    maintainers = [ ];
  };
}