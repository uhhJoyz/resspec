# Welcome

This is **resspec** (pronounced "ree-speck"), which is a single-page resume
tailoring application built on Typst using OCaml. For those who are unaware,
Typst is a modern type-setting language similar to LaTeX with scripting
capabilities similar to Python. **resspec**'s implementation is based on a
reduction ot the knapsack problem and uses an $O(n \cdot W)$ implementation of
the knapsack problem where $n$ is the number of resume entries you have and $W$
is the amount of vertical space in your Typst document. (Example shown below.)

Demo usage:
![Example GIF](./demo.gif "Demonstration GIF.")

Output resume:
![Example Resume](./example-res.png "Example Resume")

In the event that one of your sections on your shortened resume ends up being
empty, we recommend modifying your tag assignments to better suit your desired
output.

## Before you resspec

You should quickly note that any sections titled "Skills" or "Education" will
always be included in full. This is subject to change and may require manual
specification in future updates. All data and text *after* the resume block
will be omitted.

## Regarding Performance

It should be noted that, due to the nature of Typst's query and the developer
decision to use the Typst CLI to implement this project, this application's
runtime is almost entirely determined by file operations and Typst compilation
and query times. As of the writing of this section, only $1.3\%$ of
programruntime is spent performing calculations or doing operations. This
information was gleaned using a mix of various timing tools and manual tooling
using `Sys.time`.

This could be resolved by using a server instance of Typst and communicating
through their JSON RPC infrastructure, however this application is not highly
latency sensitive, so this was not a high priority.

# Getting Started

To get started, you need to install [Typst (found
here)](https://github.com/typst/typst). There are many ways to do this, but if
you are on MacOS or Ubuntu, run one of the following commands:

```bash
# MacOS
brew install typst
# Ubuntu
sudo snap install typst
```

You also need to install [dune](https://github.com/ocaml/dune) and
[opam](https://github.com/ocaml/opam) and can then run `./setup.sh` to create
an opam switch and install the necessary dependencies. The commands for this
are listed below in case you would rather run them yourself.

```bash
# create an opam switch
opam switch create resspec 5.2.1
opam switch resspec
# install necessary packages
opam install minttea spices leaves
```

# Build

The application can be built from source using dune with the `dune build`
command.

# Installation / Usage

First, clone the repository:

```bash
git clone https://github.com/uhhJoyz/resspec.git
```

Now, you can find the example resume (for the Pokemon Diglett) in
`example/resume.typ`. If you would like to preview this resume, you need to
specify the root directory of your preview command as `<path-to>/resspec` to
let *Typst* find the `./styles.typ` file (needed by the `resspec application`).
In short, wherever you run the `resspec` binary, you must ensure that
`styles.typ` **is in the same directory** or it will fail to compile.

To compile, ensure that you have `dune` and `opam` installed and then run the
following:
```bash
./build.sh
```
--- OR ---
```bash
# compile
dune build
```

You can then find the binary in `./_build/default/bin/main.exe`. The `.exe`
extension is **not** indicative of a Windows executable.

You can copy it using:
```bash
cp ./_build/default/bin/main.exe ./resspec
```
and then run it in interactive mode with:
```bash
./resspec <path-to-your-resume> -i
```

# Credits

Lead Developer: William Bradford (uhhJoyz)

Put some resspec on our name (written with full irony).
