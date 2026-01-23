# Epic Template

This repository contains a sample COBOL program and a development environment for building and running COBOL code using GNU COBOL, Docker, and Visual Studio Code.

## Getting Started

### Prerequisites

- [Docker](https://www.docker.com/) (for devcontainer usage)
- [Visual Studio Code](https://code.visualstudio.com/) with the [Dev Containers extension](https://marketplace.visualstudio.com/items?itemName=ms-vscode-remote.remote-containers)
- Alternatively, a local installation of GNU COBOL

### Opening in VS Code Dev Container

1. Open this repository in VS Code.
2. When prompted, reopen in the dev container, or use the Command Palette:
   `Dev Containers: Reopen in Container`
3. The environment will automatically install GNU COBOL and required extensions.

### Compiling the COBOL Program

The main COBOL source file is [`src/main.cob`](src/main.cob).

To compile:

#### Using VS Code Tasks

- Press <kbd>Ctrl</kbd>+<kbd>Shift</kbd>+<kbd>B</kbd> and select **COBOL: Build active file**.
- The compiled executable will be placed in the `bin/` directory (e.g., `bin/main`).

#### Using the Command Line

From the project root, run:

```sh
mkdir -p bin
cobc -x -free -o bin/main src/main.cob
./bin/main
```

## Test Files
... TODO