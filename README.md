# relnot

A small program to generate release notes files in the format:  
`[TEAM] MESSAGE (TICKET)`

[Download](https://github.com/gpunto/relnot/releases)

## Usage

```
relnot ((-i|--infer) [-b|--branch] | TICKET [MESSAGE]) 
       [-d|--dir DIRECTORY]
       [(-o|--overwrite) | (-n|--create-new)] 
       [-g|--git-add]
```

- `TICKET`: the ticket number, e.g. CAP-1234, CX-1010
- `MESSAGE`: the release notes message
- `-i|--infer`: try to infer both ticket and message from the last commit  
It expects a commit message in the format: `[TICKET] MESSAGE`, e.g. `[CAP-1234] Add juicy stuff`
- `-b|--branch`: try to infer using the branch name instead of the last commit  
It expects a branch name in the format `type/ticket/name` or `ticket/name`.
Only valid when used with `-i|--infer`
- `-d|--dir DIRECTORY`: the directory where to put the release notes file  
Defaults to: `./release_notes`
- `-o|--overwrite`: when the target file exists: force overwrite
- `-n|--create-new`: when the target file exists: create a new one alongside
- `-g|--git-add`: run `git add` on the generated file

## Examples

### Explicitly specify ticket and message

The command:  
`relnot cap-1234 "This is an explicit message"`

Generates `./release_notes/cap-1234` with content:
```
[CaP] This is an explicit message (CAP-1234)
```

### Let infer do the work

**Using the latest commit**

Assuming the latest commit message is `[CX-1010] This is a commit message`, the command:  
`relnot -i`

Generates `./release_notes/CX-1010` with content:

```
[CX] This is a commit message (CX-1010)
```

**Using the branch name**

Assuming the branch name is `cd-9999/beautiful-feature`, the command:  
`relnot -ib`

Generates `./release_notes/cd-9999` with content:

```
[CD]  (CD-9999)
```

## Building

1. Install [Stack](https://docs.haskellstack.org/en/stable/README/)  
E.g. through brew: `brew install haskell-stack`
2. Clone the repo  
`git clone git@github.com:gpunto/relnot.git`
3. Build and install with [Stack](https://docs.haskellstack.org/en/stable/build_command/)  
`cd relnot ; stack install`  
Pay attention to the output of the command, as the target folder may not be in your `PATH`