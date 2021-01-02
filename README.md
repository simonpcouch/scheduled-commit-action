# Scheduling R Scripts

This repository is an example accompanying my recent blog post [Running R Scripts on a Schedule](https://blog.simonpcouch.com/blog/r-github-actions-commit/). The [schedule-commit workflow file](.github/workflows/schedule-commit.yaml) sets up R, runs an R script that runs `rnorm(10)` and saves it to a file, and commits it once an hour. Read the linked blog post for more details and explanation!

UPDATE, 2 January 2021—I've disabled Actions in this repository's settings to save on compute time, but the code and repository structure is exactly as it was when the Action was running regularly!
