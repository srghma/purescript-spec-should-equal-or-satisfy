let upstream = ./upstream.dhall

in  (     upstream
      //  https://raw.githubusercontent.com/srghma/my-purescript-package-sets/b329910/packages.dhall sha256:773dba33bac1ce8395e89554cc6ee5ea86112eceb54e7236171625d193c1e000
            upstream.(https://raw.githubusercontent.com/srghma/my-purescript-package-sets/b329910/upstreamTypeChunk.dhall sha256:8a4543a6ab82a4873958510d701ebeaa6dc1729634f70871299b35e535b6a3cd)
    )
  with metadata.version = "v0.14.0-rc3"
  with spec.repo = "https://github.com/instateam/purescript-spec.git"
  with spec.version = "master"
  with strings.repo = "https://github.com/instateam/purescript-strings.git"
  with strings.version = "unix-parenthesis"
  with tuples.repo = "https://github.com/srghma/purescript-tuples.git"
  with tuples.version = "master"
  with unfoldable.repo =
      "https://github.com/purescript/purescript-unfoldable.git"
  with unfoldable.version = "master"
  with foldable-traversable.version = "master"
  with prelude.version = "master"
