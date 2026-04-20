# owen.cafe

Static site built with Hugo.

## Requirements

- `hugo`

## Build

```bash
hugo
```

## Live Reload

```bash
hugo server
```

## Palette

- background: `#111111`
- panel/node fill: `#191724`
- primary accent: `#5577dd`
- edge/accent: `#cdbae7`
- green accent: `#3f8f6b`
- red accent: `#9a4f67`
- text: `#eeeeee`

## Lightbox

Images and figures use the same pure-CSS lightbox.

- the preview is a link to `#lightbox-...`
- the overlay has the matching `id`
- CSS uses `.lightbox:target` to show the overlay
- the backdrop and close button link to `#`, clearing the target

Shared lightbox markup lives in [layouts/partials/lightbox.html](layouts/partials/lightbox.html). The CSS is in [themes/owen.cafe/assets/css/main.css](themes/owen.cafe/assets/css/main.css).
