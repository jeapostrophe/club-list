#!/bin/bash

rsync -h --progress -a --delete -L --exclude compiled --exclude .git plt-etc:local/club-list/sites/wics/ ./sites/wics/
rsync -h --progress -a --delete -L --exclude compiled --exclude .git . plt-etc:local/club-list/
