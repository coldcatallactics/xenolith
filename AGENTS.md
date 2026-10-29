# AGENTS.md

Working rules for agents in the Xenolith repository.

Xenolith is an Autolith distribution: a site configuration tree that
Autolith loads, not a fork.

## Layout

- `site/init.lisp` is the entry point Autolith loads first.
- `site/xenolith.lisp` holds the module manifest, loader, and reporting.
- `site/modules/` holds one file per module, ordinary Common Lisp in the
  `AUTOLITH` package.
- `flake.nix` packages the site tree and a `xenolith` launcher.
- Upstream Autolith is a flake input. The tracked source tree is never
  modified or vendored here.

## Rules

1. Never patch upstream in place. If Autolith lacks a seam, change Autolith
   upstream and depend on the result.
2. A module must be idempotent on reload: wrap a tracked definition through
   `xenolith--chain` and restore it through `xenolith--unchain`, so a reload
   replaces the wrapper instead of stacking another one.
3. A module must degrade, not abort. Record the condition, keep a switch that
   disables it, and let startup continue.
4. The semantic style set is closed: `*terminal-style-names*` and
   `deftype terminal-style` are fixed at build time. Do not invent style names
   at runtime; add them upstream instead.
5. Every module documents its disable path, its configuration, and how to
   verify it.
6. README is a front door, about 65 lines. Facts live once, in
   `docs/modules.org` or `docs/look.org`; everything else links.
7. Do not add a document without need. Three files are the current budget:
   `README.org`, `docs/modules.org`, `docs/look.org`.

## Checks

Run before claiming a change works:

```sh
AUTOLITH_SITE_CONFIG_ROOT=$PWD/site autolith models
```

- It must exit 0 and print no `Xenolith: module ... failed` line.
- `(xenolith-report)` must show every module as `loaded`.
- Theme changes must resolve every name in `*terminal-style-names*`.
- Composer and UI changes need a real terminal check. pty capture through
  `script` is unreliable in this environment; do not treat it as evidence
  beyond startup.

Record what was run and what it printed in the commit message or the module
documentation.

## Commits

- One focused change per commit, imperative subject.
- Stage only files belonging to the change.
- Do not commit credentials, tokens, or personal paths.
- Pushing is a separate, explicit step.
