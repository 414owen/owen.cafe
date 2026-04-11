# owen.cafe

Static site built with Hugo.

## Requirements

- `hugo`
- `pnpm`
- `chromium` for local Mermaid rendering only

## Build

Install JS tooling:

```bash
pnpm install
```

Build Mermaid SVGs, then build the site:

```bash
pnpm run build
```

If you only need to refresh generated Mermaid assets:

```bash
pnpm run build:mermaid
```

## Live Reload

Run both the Mermaid watcher and `hugo server`:

```bash
pnpm run dev
```

This does two things:

- watches Markdown content for Mermaid fences and regenerates static SVGs
- runs `hugo server`

## Mermaid

Mermaid diagrams are rendered ahead of time, not in the browser.

- `scripts/render-mermaid.mjs` scans content for fenced `mermaid` blocks
- each unique diagram is rendered to `static/generated/mermaid/<hash>.svg`
- SVGs are optimized with `svgo`
- Hugo emits those SVGs as figures during page render

Generated Mermaid assets live under [static/generated/mermaid](static/generated/mermaid) and should be checked into the repo.

## Lightbox

Images, figures, and Mermaid diagrams all use the same pure-CSS lightbox.

- the preview is a link to `#lightbox-...`
- the overlay has the matching `id`
- CSS uses `.lightbox:target` to show the overlay
- the backdrop and close button link to `#`, clearing the target

Shared lightbox markup lives in [layouts/partials/lightbox.html](layouts/partials/lightbox.html). The CSS is in [themes/owen.cafe/assets/css/main.css](themes/owen.cafe/assets/css/main.css).
