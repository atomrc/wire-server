# WARNING: GENERATED FILE, DO NOT EDIT.
# This file is generated by running hack/bin/generate-local-nix-packages.sh and
# must be regenerated whenever local packages are added or removed, or
# dependencies are added or removed.
{ mkDerivation
, aeson
, attoparsec
, attoparsec-iso8601
, base
, base16-bytestring
, base64-bytestring
, binary
, bytestring
, bytestring-conversion
, cassandra-util
, cereal
, containers
, cryptohash-md5
, cryptohash-sha1
, cryptonite
, currency-codes
, data-default
, generic-random
, gitignoreSource
, hashable
, http-api-data
, imports
, iproute
, iso3166-country-codes
, iso639
, lens
, lens-datetime
, lib
, mime
, optparse-applicative
, pem
, protobuf
, QuickCheck
, quickcheck-instances
, random
, schema-profunctor
, scientific
, servant-server
, string-conversions
, swagger2
, tagged
, tasty
, tasty-hunit
, tasty-quickcheck
, text
, time
, time-locale-compat
, tinylog
, unix
, unordered-containers
, uri-bytestring
, uuid
, vector
, yaml
}:
mkDerivation {
  pname = "types-common";
  version = "0.16.0";
  src = gitignoreSource ./.;
  libraryHaskellDepends = [
    aeson
    attoparsec
    attoparsec-iso8601
    base
    base16-bytestring
    base64-bytestring
    binary
    bytestring
    bytestring-conversion
    cassandra-util
    containers
    cryptohash-md5
    cryptohash-sha1
    cryptonite
    currency-codes
    data-default
    generic-random
    hashable
    http-api-data
    imports
    iproute
    iso3166-country-codes
    iso639
    lens
    lens-datetime
    mime
    optparse-applicative
    pem
    protobuf
    QuickCheck
    quickcheck-instances
    random
    schema-profunctor
    scientific
    servant-server
    string-conversions
    swagger2
    tagged
    tasty
    tasty-hunit
    text
    time
    time-locale-compat
    tinylog
    unix
    unordered-containers
    uri-bytestring
    uuid
    vector
    yaml
  ];
  testHaskellDepends = [
    aeson
    base
    base16-bytestring
    base64-bytestring
    bytestring
    bytestring-conversion
    cereal
    imports
    protobuf
    QuickCheck
    string-conversions
    tasty
    tasty-hunit
    tasty-quickcheck
    text
    time
    unordered-containers
    uuid
  ];
  description = "Shared type definitions";
  license = lib.licenses.agpl3Only;
}
