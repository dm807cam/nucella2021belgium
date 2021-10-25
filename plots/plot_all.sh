#!/bin/sh

# Simple bashs cript to run all plot scripts sequentially.

for ii in * .R;
do R < "$ii" --no-save;
done
