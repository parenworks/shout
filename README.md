# SHOUT

**Social Herald Over Unix Terminals**

A pure ANSI terminal user interface for [Multiposter](https://github.com/Shinmera/multiposter) — compose and broadcast posts to multiple social media platforms from your terminal.

```
╭─ Clients ──────────────╮╭─ Compose ──────────────────────────────────╮
│ ● bluesky (bluesky)    ││                                            │
│ ● mastodon (mastodon)  ││ Oye Belters 🛰️🔧 Morning Drift is         │
│ ○ Discord (not config) ││ online — soft light on the console...      │
│ ○ Git (not configured) ││                                            │
│ ○ WebDAV (not config)  ││                                            │
│ ○ Tumblr (not config)  ││                                            │
├─ Tags ─────────────────┤├─ Preview ───────────────────────────────────┤
│ ● #NowPlaying          ││ ████████████░░░░░░░░ 142/300 bluesky       │
│ ○ #AsteroidRadio       ││ ████████░░░░░░░░░░░░ 155/500 mastodon      │
│ + add tag...           ││                                            │
├─ Status ───────────────┤╰────────────────────────────────────────────╯
│ ✓ 2/2 clients ready    │
╰────────────────────────╯
 Tab: next │ S-Tab: prev │ F5: post │ C-s: post │ C-q: quit
```

## Features

- **Pure ANSI** — no ncurses dependency, just escape sequences
- **CLOS architecture** — fully extensible widget system
- **Live preview** — per-client character counts with colour-coded progress bars (Bluesky counts text only, Mastodon counts text + tags)
- **Client management** — toggle which platforms to post to; shows all known multiposter client types
- **Tag management** — persistent saved tags with per-post enable/disable
- **Context-sensitive help** — keybindings update based on focused panel
- **Posting progress** — animated spinner with success/failure per client
- **Auto-clear** — compose area and tags reset after successful post
- **UTF-8 support** — emoji and multi-byte characters in compose

## Requirements

- [SBCL](http://www.sbcl.org/) (Steel Bank Common Lisp)
- [Quicklisp](https://www.quicklisp.org/)
- [Multiposter](https://github.com/Shinmera/multiposter) source (with at least one configured client)
- A terminal with Unicode and 256-colour support

## Building & Installing

```bash
git clone https://github.com/parenworks/shout.git
cd shout

# Multiposter source must be available in one of:
#   ../multiposter/
#   ~/SourceCode/multiposter/
#   ~/common-lisp/multiposter/
#   ~/quicklisp/local-projects/multiposter/

make build
sudo make install   # installs to /usr/local/bin/shout
```

## Usage

```bash
shout              # launch the TUI
shout --help       # show usage and keybindings
shout --version    # show version
```

SHOUT reads your existing Multiposter configuration from `~/.config/multiposter/multiposter.lisp`. Tags are saved to `~/.config/shout/tags.lisp`.

## Keybindings

| Key | Action |
|-----|--------|
| `Tab` / `S-Tab` | Cycle focus between panels |
| `F5` / `Ctrl+S` | Post to selected clients |
| `Ctrl+Q` | Quit |
| `Space` | Toggle item (clients or tags) |
| `a` | Add new tag (in Tags panel) |
| `d` | Delete selected tag (in Tags panel) |
| `↑↓` | Navigate lists |
| `Ctrl+A` / `Ctrl+E` | Beginning / end of line (in Compose) |
| `Ctrl+K` | Kill to end of line (in Compose) |

## License

zlib
