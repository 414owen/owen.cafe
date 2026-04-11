import { createHash } from 'node:crypto';
import { promises as fs } from 'node:fs';
import os from 'node:os';
import path from 'node:path';
import { fileURLToPath } from 'node:url';
import { spawn } from 'node:child_process';
import { optimize } from 'svgo';

const __dirname = path.dirname(fileURLToPath(import.meta.url));
const root = path.resolve(__dirname, '..');
const contentDir = path.join(root, 'content');
const outputDir = path.join(root, 'static', 'generated', 'mermaid');
const stylesDir = path.join(outputDir, 'styles');
const sharedStyleFileName = 'mermaid.css';
const mermaidConfigPath = path.join(__dirname, 'mermaid-config.json');
const puppeteerConfigPath = path.join(__dirname, 'puppeteer-config.json');
const mmdcPath = path.join(root, 'node_modules', '.bin', 'mmdc');
const hashPrefix = 'mermaid-static-v1:';

async function listFiles(dir) {
  const entries = await fs.readdir(dir, { withFileTypes: true });
  const files = await Promise.all(entries.map(async (entry) => {
    const fullPath = path.join(dir, entry.name);
    if (entry.isDirectory()) {
      return listFiles(fullPath);
    }
    if (entry.isFile() && /\.(md|markdown)$/i.test(entry.name)) {
      return [fullPath];
    }
    return [];
  }));
  return files.flat();
}

function mermaidHash(source) {
  return createHash('md5').update(`${hashPrefix}${source}`).digest('hex');
}

function extractMermaidBlocks(text) {
  const matches = [];
  const pattern = /^```mermaid[^\n]*\n([\s\S]*?)^```$/gm;
  let match;
  while ((match = pattern.exec(text)) !== null) {
    matches.push(match[1].replace(/\s+$/, ''));
  }
  return matches;
}

async function runMmdc(inputPath, outputPath) {
  const svgId = 'root';
  await new Promise((resolve, reject) => {
    const child = spawn(
      mmdcPath,
      [
        '-i', inputPath,
        '-o', outputPath,
        '-e', 'svg',
        '-c', mermaidConfigPath,
        '-p', puppeteerConfigPath,
        '-b', '#111',
        '-I', svgId
      ],
      { stdio: 'inherit' }
    );

    child.on('error', reject);
    child.on('exit', (code) => {
      if (code === 0) {
        resolve();
        return;
      }
      reject(new Error(`mmdc exited with code ${code}`));
    });
  });
}

async function optimizeSvg(outputPath) {
  const input = await fs.readFile(outputPath, 'utf8');
  const result = optimize(input, {
    path: outputPath,
    multipass: true,
    js2svg: {
      pretty: false
    },
    plugins: [
      {
        name: 'preset-default',
        params: {
          overrides: {
            cleanupNumericValues: {
              floatPrecision: 1
            },
            convertPathData: {
              floatPrecision: 1
            },
            convertTransform: {
              floatPrecision: 1
            }
          }
        }
      },
      {
        name: 'removeDimensions',
        active: true
      }
    ]
  });
  const cleaned = result.data
    .replace(/@keyframes edge-animation-frame\{[^}]*\}/g, '')
    .replace(/@keyframes dash\{[^}]*\}/g, '')
    .replace(/;max-width:[^";]+/g, '')
    .replace(/style=";+/g, 'style="')
    .replace(/style=""\s*/g, '');

  const styleMatch = cleaned.match(/<style>([\s\S]*?)<\/style>/);
  if (!styleMatch) {
    await fs.writeFile(outputPath, cleaned);
    return null;
  }

  const css = styleMatch[1];
  const styleFileName = sharedStyleFileName;
  const styleOutputPath = path.join(stylesDir, styleFileName);
  await fs.mkdir(stylesDir, { recursive: true });
  await fs.writeFile(styleOutputPath, css);

  const svgWithoutInlineStyle = cleaned.replace(/<style>[\s\S]*?<\/style>/, '');
  const svgWithExternalStyle = `<?xml-stylesheet href="/generated/mermaid/styles/${styleFileName}" type="text/css"?>\n${svgWithoutInlineStyle}`;
  await fs.writeFile(outputPath, svgWithExternalStyle);
  return styleFileName;
}

async function main() {
  const files = await listFiles(contentDir);
  const diagrams = new Map();

  for (const file of files) {
    const text = await fs.readFile(file, 'utf8');
    for (const block of extractMermaidBlocks(text)) {
      diagrams.set(mermaidHash(block), block);
    }
  }

  await fs.mkdir(outputDir, { recursive: true });
  await fs.mkdir(stylesDir, { recursive: true });

  const existing = await fs.readdir(outputDir).catch(() => []);
  const wanted = new Set([...diagrams.keys()].map((hash) => `${hash}.svg`));

  for (const name of existing) {
    if (name.endsWith('.svg') && !wanted.has(name)) {
      await fs.unlink(path.join(outputDir, name));
    }
  }

  for (const [hash, source] of diagrams) {
    const outputPath = path.join(outputDir, `${hash}.svg`);
    try {
      await fs.access(outputPath);
      continue;
    } catch {}

    const tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'owencafe-mermaid-'));
    const inputPath = path.join(tempDir, `${hash}.mmd`);
    await fs.writeFile(inputPath, source);
    try {
      await runMmdc(inputPath, outputPath);
      await optimizeSvg(outputPath);
    } finally {
      await fs.rm(tempDir, { recursive: true, force: true });
    }
  }

  const styleEntries = await fs.readdir(stylesDir).catch(() => []);
  for (const name of styleEntries) {
    if (name.endsWith('.css') && name !== sharedStyleFileName) {
      await fs.unlink(path.join(stylesDir, name));
    }
  }

  console.log(`Rendered ${diagrams.size} Mermaid diagram(s).`);
}

await main();
