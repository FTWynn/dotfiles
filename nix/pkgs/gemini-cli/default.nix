{ lib, buildNpmPackage, fetchFromGitHub, git }:

buildNpmPackage rec {
  pname = "gemini-cli";
  version = "early-access-2025-06-24"; # A descriptive version for this commit

  src = fetchFromGitHub {
    owner = "google-gemini";
    repo = "gemini-cli";
    rev = "52afcb3a1233237b07aa86b1678f4c4eded70800";
    hash = "sha256-KNnfo5hntQjvc377A39+QBemeJjMVDRnNuGY/93n3zc=";
  };

  npmDepsHash = "sha256-/IAEcbER5cr6/9BFZYuV2j1jgA75eeFxaLXdh1T3bMA="; # This part remains the same for now

  # We need the 'git' command itself
  nativeBuildInputs = [ git ];

  # This hook runs right before the 'npm run build' command.
  # It creates a new git repository in the build directory, adds all the
  # files, and makes a commit. This is enough to satisfy the build script.
  preBuild = ''
    git init
    git config user.email "nix-builder@example.com"
    git config user.name "Nix Builder"
    git add .
    git commit -m "Nix build commit"
  '';

  # The default installer fails on this monorepo. We override it to
  # manually install the final executable, which we know exists after
  # the successful buildPhase.
  installPhase = ''
    runHook preInstall
    
    # The build process places the final executable in the "dist"
    # directory of the "cli" package.
    install -D packages/cli/dist/index.js $out/bin/gemini

    runHook postInstall
  '';

  meta = with lib; {
    description = "A command-line interface for Google's Gemini models";
    homepage = "https://github.com/google-gemini/gemini-cli";
    license = licenses.asl20; # Apache 2.0 License
    maintainers = [ ];
  };
}