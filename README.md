# SHOUT

**Social Herald Over Unix Terminals**

A pure ANSI terminal user interface for [Multiposter](https://github.com/Shinmera/multiposter) — compose and broadcast posts to multiple social media platforms from your terminal.

```
╭─ Clients ──────────────╮╭─ Compose ──────────────────────────────────╮
│ ● Bluesky              ││                                            │
│ ● Mastodon             ││ Oye Belters 🛰️🔧 Morning Drift is         │
│ ○ Discord              ││ online — soft light on the console...      │
│                        ││                                            │
├─ Tags ─────────────────┤│                                            │
│ #NowPlaying            │├─ Preview ───────────────────────────────────┤
│ #AsteroidRadio         ││ ████████████░░░░░░░░ 187/300 Bluesky       │
│ + add tag...           ││ ████████░░░░░░░░░░░░ 187/500 Mastodon      │
├─ Status ───────────────┤│                                            │
│ ✓ 2/2 clients ready    ││                                            │
╰────────────────────────╯╰────────────────────────────────────────────╯
╭─ Tab: next │ Space: toggle │ C-Enter: post │ C-q: quit ─────────────╯
```

## Features

- **Pure ANSI** — no ncurses dependency, just escape sequences
- **CLOS architecture** — fully extensible widget system
- **Live preview** — character counts per platform with colour-coded progress bars
- **Client management** — toggle which platforms to post to
- **Tag management** — add/remove hashtags with a dedicated panel
- **Context-sensitive help** — keybindings update based on focused panel
- **Posting progress** — animated spinner with success/failure per client

## Requirements

- SBCL (Steel Bank Common Lisp)
- [Multiposter](https://github.com/Shinmera/multiposter) with configured clients
- A terminal with Unicode and 256-colour support

## Building

```bash
sbcl --eval '(require :asdf)' \
     --eval '(load "~/quicklisp/setup.lisp")' \
     --eval '(push #P"/path/to/shout/" asdf:*central-registry*)' \
     --eval '(push #P"/path/to/multiposter/" asdf:*central-registry*)' \
     --eval '(asdf:make :shout)' \
     --quit
```

## Usage

```bash
./shout
```

SHOUT reads your existing Multiposter configuration from `~/.config/multiposter/multiposter.lisp`.

## Keybindings

| Key | Action |
|-----|--------|
| `Tab` | Cycle focus between panels |
| `Ctrl+Enter` | Post to selected clients |
| `Ctrl+Q` | Quit |
| `Space` | Toggle client (in Clients panel) |
| `a` | Add tag (in Tags panel) |
| `d` | Delete tag (in Tags panel) |
| `↑↓` | Navigate lists |

## License

zlib
