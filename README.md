# relnot

A small program to generate release notes files in the format:  
`[TEAM] MESSAGE (TICKET)`

[Download](https://github.com/gpunto/relnot/releases)

## Usage

```
relnot ((-i|--infer) | TICKET [MESSAGE])
    [-d|--dir DIRECTORY]
    [-o|--overwrite]
    [-g|--git-add]
```

- `TICKET`: the ticket number, e.g. CAP-1234, CX-1010
- `MESSAGE`: the release notes message
- `-i` or `--infer`: try to infer both ticket and message from the last commit  
It expects a commit message in the format: `[TICKET] MESSAGE`, e.g. `[CAP-1234] Add juicy stuff`
- `-d|--dir DIRECTORY`: the directory where to put the release notes file  
Defaults to: `./release_notes` 
- `[-o|--overwrite]`: force overwrite the target file if it already exists 
- `[-g|--git-add]`: run `git add` on the generated file

## Examples

**Explicitly specify ticket and message**  

The command:  
`relnot cap-1234 "This is an explicit message"`

Generates `./release_notes/cap-1234` with content:
```
[CaP] This is an explicit message (CAP-1234)
```

**Let infer do the work**  

Assuming the latest commit message is `[CX-1010] This is a commit message`, the command:  
`relnot -i`

Generates `./release_notes/CX-1010` with content:

```
[CX] This is a commit message (CX-1010)
```

## Building

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)  
E.g. through brew: `brew install haskell-stack`
2. Clone the repo  
`git clone git@github.com:gpunto/relnot.git`
3. Build and install with [Stack](https://docs.haskellstack.org/en/stable/build_command/)  
`cd relnot ; stack install`  
Pay attention to the output of the command, as the target folder may not be in your `PATH`