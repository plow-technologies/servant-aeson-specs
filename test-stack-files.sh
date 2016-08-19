#!/usr/bin/env bash

set -o errexit

for f in stack*.yaml ; do
  export STACK_YAML=$f
  stack setup
  stack test
done
