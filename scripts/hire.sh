#!/usr/bin/env bash

cat /dev/null > /tmp/hire.tmp
hire --start-from $1 --output-file /tmp/hire.tmp
cat /tmp/hire.tmp
