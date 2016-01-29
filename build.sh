#!/bin/bash -xe

# Build client
rm -rf hive-client/hive
cp -r hive hive-client/hive
stack build --stack-yaml=hive-client/stack.yaml

# Copy all.js to server/static/all.js
rm -f hive-server/static/all.js
cp $(stack path --stack-yaml=hive-client/stack.yaml --local-install-root)/bin/hive-client.jsexe/all.js hive-server/static/all.js

# Build server
stack build --stack-yaml=hive-server/stack.yaml

rm -rf hive-client/hive
