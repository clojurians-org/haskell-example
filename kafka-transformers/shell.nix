with import <nixpkgs> { };

haskell.lib.buildStackProject {
  name = "kafka-transformer" ;
  buildInputs = [ zlib pkgconfig  rdkafka ] ;
}

