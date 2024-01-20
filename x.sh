#!/bin/sh

set -e

RED="\033[0;31m"
GREEN="\033[0;32m"
BLUE="\033[0;34m"
CLEAR="\033[0m"

function decorate
{
  echo -e "$@${CLEAR}"
}

function red
{
  decorate "${RED}$@"
}
function green
{
  decorate "${GREEN}$@"
}
function blue
{
  decorate "${BLUE}$@"
}
cmd=$1

shift

case $cmd in
esac
