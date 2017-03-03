#!/bin/bash
unset USE_LXC
export SIGNER=
export VERSION=1.0.0.0
export BTCPATH=/fast/bitcoin/bu
export REL_DIR=/me/bitcoin/releases/${VERSION}

mkdir ${REL_DIR}
./bin/gbuild --url bitcoin=${BTCPATH} --commit bitcoin=release ../bu/contrib/gitian-descriptors/gitian-linux.yml
mv build/out ${REL_DIR}/linux
mv var/build.log build_linux.log
./bin/gbuild --url bitcoin=${BTCPATH} --commit bitcoin=release ../bu/contrib/gitian-descriptors/gitian-osx.yml
mv build/out ${REL_DIR}/osx
mv var/build.log build_osx.log
./bin/gbuild --url bitcoin=${BTCPATH} --commit bitcoin=release ../bu/contrib/gitian-descriptors/gitian-win.yml
mv build/out ${REL_DIR}/win
mv var/build.log build_win.log
./bin/gbuild --url bitcoin=${BTCPATH} --commit bitcoin=release ../bu/contrib/gitian-descriptors/gitian-arm.yml
mv build/out ${REL_DIR}/arm
mv var/build.log build_arm.log
