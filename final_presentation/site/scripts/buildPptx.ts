/**
 * Assemble the recorded slide videos into a single PPTX where each
 * slide is a full-bleed autoplaying mp4.
 *
 * PptxGenJS handles the bulk of the .pptx structure (relationships,
 * slide XML, embedded media) but does not write the <p:timing> block
 * needed to make videos autoplay - by default PowerPoint treats embedded
 * videos as click-to-play. So we build with pptxgenjs, then crack the
 * resulting zip and inject an autoplay timing block per slide that
 * contains a video.
 */

import { mkdir, readFile, readdir, writeFile } from 'node:fs/promises';
import { existsSync } from 'node:fs';
import { dirname, resolve } from 'node:path';
import { fileURLToPath } from 'node:url';
import JSZip from 'jszip';
import * as PptxGenJSNs from 'pptxgenjs';

// pptxgenjs ships CJS, and tsx's interop wraps the default export twice
// when pulling it through ESM (default.default is the actual class).
type PptxCtor = typeof import('pptxgenjs').default;
const PptxGenJS: PptxCtor = (() => {
  const ns = PptxGenJSNs as unknown as { default?: { default?: PptxCtor } & PptxCtor };
  if (typeof ns.default === 'function') return ns.default;
  if (ns.default && typeof ns.default.default === 'function') return ns.default.default;
  return PptxGenJSNs as unknown as PptxCtor;
})();

const __dirname = dirname(fileURLToPath(import.meta.url));
const SITE_DIR = resolve(__dirname, '..');
const VIDEO_DIR = resolve(SITE_DIR, 'out/videos');
const OUT_PPTX = resolve(SITE_DIR, 'out/incollege-deck.pptx');

const SLIDE_W = 13.333;
const SLIDE_H = 7.5;

async function buildBasePptx(videoFiles: readonly string[]): Promise<void> {
  const pptx = new PptxGenJS();
  pptx.layout = 'LAYOUT_WIDE';
  pptx.title = 'InCollege - Final Presentation';
  pptx.company = 'USF CEN 4020 - Spring 2026';

  for (const file of videoFiles) {
    const slide = pptx.addSlide();
    slide.background = { color: '050814' };
    slide.addMedia({
      type: 'video',
      path: resolve(VIDEO_DIR, file),
      x: 0,
      y: 0,
      w: SLIDE_W,
      h: SLIDE_H,
    });
  }

  await mkdir(dirname(OUT_PPTX), { recursive: true });
  await pptx.writeFile({ fileName: OUT_PPTX });
}

function autoplayTimingXml(spId: string): string {
  return [
    '<p:timing>',
    ' <p:tnLst>',
    '  <p:par>',
    '   <p:cTn id="1" dur="indefinite" restart="never" nodeType="tmRoot">',
    '    <p:childTnLst>',
    '     <p:seq concurrent="1" nextAc="seek">',
    '      <p:cTn id="2" dur="indefinite" nodeType="mainSeq">',
    '       <p:childTnLst>',
    '        <p:par>',
    '         <p:cTn id="3" fill="hold">',
    '          <p:stCondLst><p:cond delay="indefinite"/></p:stCondLst>',
    '          <p:childTnLst>',
    '           <p:par>',
    '            <p:cTn id="4" fill="hold">',
    '             <p:stCondLst><p:cond delay="0"/></p:stCondLst>',
    '             <p:childTnLst>',
    '              <p:par>',
    '               <p:cTn id="5" presetID="1" presetClass="mediacall" presetSubtype="0" fill="hold" grpId="0" nodeType="afterEffect">',
    '                <p:stCondLst><p:cond delay="0"/></p:stCondLst>',
    '                <p:childTnLst>',
    '                 <p:cmd type="call" cmd="playFrom(0.0)">',
    '                  <p:cBhvr>',
    '                   <p:cTn id="6" dur="indefinite" fill="hold"/>',
    `                   <p:tgtEl><p:spTgt spid="${spId}"/></p:tgtEl>`,
    '                  </p:cBhvr>',
    '                 </p:cmd>',
    '                </p:childTnLst>',
    '               </p:cTn>',
    '              </p:par>',
    '             </p:childTnLst>',
    '            </p:cTn>',
    '           </p:par>',
    '          </p:childTnLst>',
    '         </p:cTn>',
    '        </p:par>',
    '       </p:childTnLst>',
    '      </p:cTn>',
    '      <p:prevCondLst><p:cond evt="onPrev" delay="0"><p:tgtEl><p:sldTgt/></p:tgtEl></p:cond></p:prevCondLst>',
    '      <p:nextCondLst><p:cond evt="onNext" delay="0"><p:tgtEl><p:sldTgt/></p:tgtEl></p:cond></p:nextCondLst>',
    '     </p:seq>',
    '     <p:video>',
    '      <p:cMediaNode vol="80000">',
    '       <p:cTn id="7" fill="hold" display="0">',
    '        <p:stCondLst><p:cond delay="indefinite"/></p:stCondLst>',
    '       </p:cTn>',
    `       <p:tgtEl><p:spTgt spid="${spId}"/></p:tgtEl>`,
    '      </p:cMediaNode>',
    '     </p:video>',
    '    </p:childTnLst>',
    '   </p:cTn>',
    '  </p:par>',
    ' </p:tnLst>',
    '</p:timing>',
  ].join('');
}

function findVideoSpIds(slideXml: string): string[] {
  const ids: string[] = [];
  const picRegex = /<p:pic\b[\s\S]*?<\/p:pic>/g;
  let match: RegExpExecArray | null;
  while ((match = picRegex.exec(slideXml))) {
    const block = match[0];
    if (!block.includes('<a:videoFile')) continue;
    const idMatch = block.match(/<p:cNvPr\s+id="(\d+)"/);
    if (idMatch) ids.push(idMatch[1]);
  }
  return ids;
}

async function injectAutoplay(): Promise<void> {
  const buffer = await readFile(OUT_PPTX);
  const zip = await JSZip.loadAsync(buffer);

  const slideEntries = Object.keys(zip.files)
    .filter((name) => /^ppt\/slides\/slide\d+\.xml$/.test(name))
    .sort((a, b) => slideNum(a) - slideNum(b));

  let touched = 0;
  for (const name of slideEntries) {
    const file = zip.file(name);
    if (!file) continue;
    const xml = await file.async('text');
    const spIds = findVideoSpIds(xml);
    if (spIds.length === 0) continue;
    const timing = autoplayTimingXml(spIds[0]);
    const next = xml.includes('<p:timing')
      ? xml.replace(/<p:timing[\s\S]*?<\/p:timing>/, timing)
      : xml.replace('</p:sld>', `${timing}</p:sld>`);
    zip.file(name, next);
    touched += 1;
  }

  const out = await zip.generateAsync({
    type: 'nodebuffer',
    compression: 'DEFLATE',
    compressionOptions: { level: 6 },
    mimeType:
      'application/vnd.openxmlformats-officedocument.presentationml.presentation',
  });
  await writeFile(OUT_PPTX, out);
  console.log(`[autoplay] injected timing into ${touched}/${slideEntries.length} slides`);
}

function slideNum(name: string): number {
  const m = name.match(/slide(\d+)\.xml$/);
  return m ? Number.parseInt(m[1], 10) : 0;
}

async function main(): Promise<void> {
  if (!existsSync(VIDEO_DIR)) {
    console.error(`No videos at ${VIDEO_DIR}; run record + transcode first.`);
    process.exit(1);
  }
  const videos = (await readdir(VIDEO_DIR))
    .filter((f) => f.endsWith('.mp4') && /^\d{2}-/.test(f))
    .sort();
  if (videos.length === 0) {
    console.error(`No mp4 files found in ${VIDEO_DIR}.`);
    process.exit(1);
  }
  console.log(`[build] ${videos.length} slide videos -> ${OUT_PPTX}`);

  await buildBasePptx(videos);
  await injectAutoplay();
  console.log(`[done] wrote ${OUT_PPTX}`);
}

main().catch((err) => {
  console.error(err);
  process.exit(1);
});
