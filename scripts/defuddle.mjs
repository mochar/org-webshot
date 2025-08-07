#!/usr/bin/env node

import { readFile } from 'fs/promises';
import { Defuddle } from 'defuddle/node';
import { JSDOM } from 'jsdom';

const baseURL = 'https://example.com/';

const inputPath = process.argv[2];
const outputFormat = process.argv[3];

const errorFmt = () => console.error('Usage: defuddle.mjs path/to/file.html md/html');

if (!inputPath || !(outputFormat === "md" || outputFormat === "html")) {
  errorFmt();
  process.exit(1);
}

try {
  const html = await readFile(inputPath, 'utf8');
  const result = await Defuddle(html, baseURL, { markdown: outputFormat == 'md' });

  if (result?.content) {
    console.log(result.content);
  } else {
    console.error('Could not extract readable content.');
    process.exit(1);
  }
} catch (err) {
  console.error(`Error: ${err.message}`);
  process.exit(1);
}
