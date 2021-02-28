# notes

------

## 20201104 - [[linux]] - man page sections

Often a number is seen following a command, e.g. `ping (8)`. This corresponds
to the man page section it belongs to.

The following are copied directly from running `man man`:

```text
1   Executable programs or shell commands
2   System calls (functions provided by the kernel)
3   Library calls (functions within program libraries)
4   Special files (usually found in /dev)
5   File formats and conventions, e.g. /etc/passwd
6   Games
7   Miscellaneous (including macro packages and conventions), e.g. man(7), groff(7)
8   System administration commands (usually only for root)
9   Kernel routines [Non standard
```

------

## 20201029 - [[zsh]] - clear zsh history

Erase all zsh history, leaving the command "erase_history".

```bash
function erase_history { local HISTSIZE=0; }
erase_history
```

[source](https://unix.stackexchange.com/a/544815)

------

## 20201021 - [[docker]] - format container ls output

For a simpler output from `docker container ls`, use the `--format` flag.

For example:

```bash
docker container list --format "table {{.Names}}\t{{.Status}}\t{{.Image}}"

# add as alias
alias dlss='docker container list --format "table {{.Names}}\t{{.Status}}\t{{.Image}}"'
```

Available options:

- Command
- CreatedAt
- ID
- Image
- Labels
- LocalVolumes
- Mounts
- Names
- Networks
- Ports
- RunningFor
- Size
- Status

Run `docker container ls --format='{{json .}}'` to get the above options.

[source](https://docs.docker.com/config/formatting/)

------

## 20201010 - [[vim]] - using tabs

Use `:tabe[dit] {filename}` to create a new tab. 
Open current window into new tab `<C-w>T`, if there is more than one window.
Use `:tabc[lose] {filename}` to close the current tab and all its windows.

| Switch to tab | normal command | ex command       |
|---------------|----------------|------------------|
| next          | `gt`           | `:tabn[ext]`     |
| previous      | `gT`           | `:tabp[revious]` |

------

## 20200816 - [[linux]] [[networking]] - set static ip with netplan

Put the following file at `/etc/netplan`.

```yaml
# 01-static-ip.yaml
network:
  version: 2
  ethernets:
    eth0:
      addresses:
        - 192.168.1.186/24
      gateway4: 192.168.1.1
      nameservers:
        addresses: [192.168.1.199, 1.1.1.1]
```

Note that the IP address ends with `/24`.

To apply changes:

```bash
sudo netplan apply
```

------
