#!/usr/bin/env bash

set -o errexit

for f in stack*.yaml ; do
  echo testing $f
  export STACK_YAML=$f
  stack setup
  stack test
  echo -----------------------------------------------
done
