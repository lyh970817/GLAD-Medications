# Project Context
This project uses `ProjectTemplate`.
- Dependency management and sourcing of libraries/helpers in `lib/` are automated by the framework.
- Munging scripts in `munge/` are executed within this context.
- To verify changes, use `library(ProjectTemplate); load.project()`.
- If you run into dependency issues, do NOT install R packages directly in `shell.nix`. Only add the necessary system dependencies for `install.packages()` to work correctly. Then reload the shell and retry installation.
