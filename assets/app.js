/* =====================================================================
   Du cacao au café – interactions du site
   Toutes les données viennent de assets/data/modele.json (produit par
   R/donnees_site.R) et de assets/data/resultats.json (tableaux du projet).
   ===================================================================== */
(() => {
'use strict';

// ---------- utilitaires ----------
const $ = (s, r = document) => r.querySelector(s);
const $$ = (s, r = document) => [...r.querySelectorAll(s)];
const NS = 'http://www.w3.org/2000/svg';
const fr = (x, d = 2) => (x < 0 ? '−' : '') + Math.abs(x).toFixed(d).replace('.', ',');
const pct = (x, d = 1) => (x > 0 ? '+' : '') + fr(x, d) + ' %';
const MOIS = ['janv.', 'févr.', 'mars', 'avr.', 'mai', 'juin', 'juil.', 'août', 'sept.', 'oct.', 'nov.', 'déc.'];
const moisFr = d => `${MOIS[d.getUTCMonth()]} ${d.getUTCFullYear()}`;
const css = v => getComputedStyle(document.documentElement).getPropertyValue(v).trim();
function el(tag, attrs = {}, parent) {
  const e = document.createElementNS(NS, tag);
  for (const k in attrs) e.setAttribute(k, attrs[k]);
  if (parent) parent.appendChild(e);
  return e;
}
const tip = $('#tip');
function showTip(html, ev) {
  tip.innerHTML = html; tip.style.opacity = 1;
  const w = tip.offsetWidth, h = tip.offsetHeight;
  let x = ev.clientX + 16, y = ev.clientY + 16;
  if (x + w > innerWidth - 8) x = ev.clientX - w - 16;
  if (y + h > innerHeight - 8) y = ev.clientY - h - 16;
  tip.style.left = x + 'px'; tip.style.top = y + 'px';
}
const hideTip = () => { tip.style.opacity = 0; };

// Adresse du dépôt GitHub, déduite de l'adresse du site (utilisateur.github.io/depot/)
const REPO = (() => {
  const h = location.hostname, p = location.pathname.split('/').filter(Boolean);
  if (h.endsWith('.github.io') && p.length) return `https://github.com/${h.split('.')[0]}/${p[0]}`;
  return 'https://github.com/klarawlbk/Projet-cacao-caf-';
})();
$('#repo-link').href = REPO;

// ---------- échelles ----------
function niceStep(span, n) {
  const raw = span / n, p = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / p;
  return (f < 1.5 ? 1 : f < 3 ? 2 : f < 7 ? 5 : 10) * p;
}
function linTicks(a, b, n = 5) {
  const s = niceStep(b - a, n), out = [];
  for (let v = Math.ceil(a / s) * s; v <= b + 1e-9; v += s) out.push(+v.toFixed(10));
  return out;
}
function logTicks(a, b) {
  const base = [1, 2, 5], out = [];
  for (let e = Math.floor(Math.log10(a)) - 1; e <= Math.ceil(Math.log10(b)); e++)
    for (const m of base) { const v = m * Math.pow(10, e); if (v >= a && v <= b) out.push(v); }
  return out;
}

// ---------- graphique en lignes générique (SVG) ----------
function lineChart(host, o) {
  const draw = () => {
    host.innerHTML = '';
    const W = host.clientWidth, H = host.clientHeight;
    if (!W || !H) return;
    const m = Object.assign({ l: 50, r: 16, t: 14, b: 26 }, o.margin || {});
    const svg = el('svg', { viewBox: `0 0 ${W} ${H}` }, host);
    const i0 = o.range ? o.range[0] : 0, i1 = o.range ? o.range[1] : o.x.length - 1;
    const vis = o.series.filter(s => !s.hidden);
    let lo = Infinity, hi = -Infinity;
    for (const s of vis) for (let i = i0; i <= i1; i++) { const v = s.values[i]; if (v == null || !isFinite(v)) continue; if (v < lo) lo = v; if (v > hi) hi = v; }
    if (o.yMin != null) lo = Math.min(lo, o.yMin);
    if (o.yMax != null) hi = Math.max(hi, o.yMax);
    if (o.hline != null) { lo = Math.min(lo, o.hline); hi = Math.max(hi, o.hline); }
    if (o.bars) for (let i = i0; i <= i1; i++) { const v = o.bars.values[i]; if (v == null) continue; if (v < lo) lo = v; if (v > hi) hi = v; }
    if (!isFinite(lo)) { lo = 0; hi = 1; }
    let ty, ticks;
    if (o.log) {
      lo *= 0.92; hi *= 1.08;
      const L0 = Math.log(lo), L1 = Math.log(hi);
      ty = v => H - m.b - (Math.log(v) - L0) / (L1 - L0) * (H - m.t - m.b);
      ticks = logTicks(lo, hi);
    } else {
      const pad = (hi - lo) * 0.06 || 1; lo -= o.yMin != null && lo >= o.yMin ? 0 : pad; hi += pad;
      ty = v => H - m.b - (v - lo) / (hi - lo) * (H - m.t - m.b);
      ticks = linTicks(lo, hi, o.yTicks || 5);
    }
    const n = i1 - i0;
    const tx = i => m.l + (i - i0) / Math.max(n, 1) * (W - m.l - m.r);
    // grille et axes
    const gA = el('g', { class: 'axis' }, svg);
    for (const t of ticks) {
      const y = ty(t);
      el('line', { x1: m.l, x2: W - m.r, y1: y, y2: y, class: 'gridl' }, gA);
      const tt = el('text', { x: m.l - 8, y: y + 4, 'text-anchor': 'end' }, gA);
      tt.textContent = o.yfmt ? o.yfmt(t) : fr(t, t % 1 ? 1 : 0);
    }
    if (o.x[0] instanceof Date) {
      const y0 = o.x[i0].getUTCFullYear(), y1 = o.x[i1].getUTCFullYear();
      const span = y1 - y0 + 1, step = [1, 2, 5, 10].find(s => span / s <= Math.max(2, (W - m.l - m.r) / 70)) || 10;
      for (let y = Math.ceil(y0 / step) * step; y <= y1; y += step) {
        const idx = o.x.findIndex(d => d.getUTCFullYear() === y && d.getUTCMonth() === 0);
        if (idx < i0 || idx > i1 || idx < 0) continue;
        const t = el('text', { x: tx(idx), y: H - 6, 'text-anchor': 'middle' }, gA); t.textContent = y;
      }
    } else {
      for (const v of o.xTicks || []) { if (v < i0 || v > i1) continue; const t = el('text', { x: tx(v), y: H - 6, 'text-anchor': 'middle' }, gA); t.textContent = o.xfmt ? o.xfmt(v) : v; }
    }
    // zones grisées
    for (const s of o.shades || []) {
      const a = Math.max(s.from, i0), b = Math.min(s.to, i1);
      if (a >= b) continue;
      el('rect', { x: tx(a), y: m.t, width: tx(b) - tx(a), height: H - m.t - m.b, class: 'shade' }, svg);
      if (s.label) { const right = tx(a) + 100 > W - m.r; const t = el('text', { x: right ? tx(b) - 6 : tx(a) + 6, y: H - m.b - 8, class: 'shadelbl', 'text-anchor': right ? 'end' : 'start' }, svg); t.textContent = W < 600 ? 'Crise' : s.label; }
    }
    if (o.hline != null) el('line', { x1: m.l, x2: W - m.r, y1: ty(o.hline), y2: ty(o.hline), stroke: css('--axis'), 'stroke-width': 1 }, svg);
    for (const r of o.refs || []) {
      el('line', { x1: m.l, x2: W - m.r, y1: ty(r.y), y2: ty(r.y), stroke: r.color, 'stroke-dasharray': '5 4', 'stroke-width': 1.3 }, svg);
      if (r.label) { const t = el('text', { x: r.left ? m.l + 6 : W - m.r - 4, y: ty(r.y) - 6, 'text-anchor': r.left ? 'start' : 'end', fill: r.color, 'font-size': 11, 'font-weight': 600 }, svg); t.textContent = r.label; }
    }
    // barres (écart)
    if (o.bars) {
      const bw = Math.max(1, (W - m.l - m.r) / Math.max(n, 1) * 0.9), z = ty(0);
      for (let i = i0; i <= i1; i++) {
        const v = o.bars.values[i]; if (v == null) continue;
        const y = ty(v);
        el('rect', { x: tx(i) - bw / 2, y: Math.min(y, z), width: bw, height: Math.abs(z - y), fill: v >= 0 ? o.bars.pos : o.bars.neg, opacity: .85 }, svg);
      }
    }
    // aires et lignes
    for (const s of vis) {
      let d = '';
      for (let i = i0; i <= i1; i++) { const v = s.values[i]; if (v == null || !isFinite(v)) { continue; } d += (d && s.values[i - 1] != null ? 'L' : 'M') + tx(i).toFixed(1) + ',' + ty(v).toFixed(1); }
      if (s.area) {
        const z = ty(o.log ? lo : Math.max(lo, Math.min(0, hi)));
        el('path', { d: d + `L${tx(i1)},${z}L${tx(i0)},${z}Z`, fill: s.color, opacity: .12 }, svg);
      }
      el('path', { d, fill: 'none', stroke: s.color, 'stroke-width': s.width || 2, 'stroke-dasharray': s.dash || '', 'stroke-linejoin': 'round', 'stroke-linecap': 'round' }, svg);
    }
    for (const mk of o.marks || []) {
      if (mk.i < i0 || mk.i > i1) continue;
      el('line', { x1: tx(mk.i), x2: tx(mk.i), y1: m.t, y2: H - m.b, stroke: mk.color, 'stroke-dasharray': '3 3' }, svg);
      el('circle', { cx: tx(mk.i), cy: ty(mk.y), r: 5, fill: mk.color, stroke: css('--surface'), 'stroke-width': 2 }, svg);
      if (mk.label) { const t = el('text', { x: tx(mk.i) + 8, y: ty(mk.y) + (mk.dy || -10), fill: mk.color, 'font-size': 12, 'font-weight': 700 }, svg); t.textContent = mk.label; }
    }
    // survol
    if (o.tip !== false) {
      const xl = el('line', { y1: m.t, y2: H - m.b, class: 'xline', visibility: 'hidden' }, svg);
      const dots = vis.map(s => el('circle', { r: 4, fill: s.color, stroke: css('--surface'), 'stroke-width': 2, visibility: 'hidden' }, svg));
      const ov = el('rect', { x: m.l, y: m.t, width: W - m.l - m.r, height: H - m.t - m.b, fill: 'transparent' }, svg);
      const move = ev => {
        const r = svg.getBoundingClientRect();
        const px = (ev.clientX - r.left) * (W / r.width);
        const i = Math.max(i0, Math.min(i1, Math.round(i0 + (px - m.l) / (W - m.l - m.r) * n)));
        xl.setAttribute('x1', tx(i)); xl.setAttribute('x2', tx(i)); xl.setAttribute('visibility', 'visible');
        vis.forEach((s, k) => { const v = s.values[i]; if (v == null) { dots[k].setAttribute('visibility', 'hidden'); return; } dots[k].setAttribute('cx', tx(i)); dots[k].setAttribute('cy', ty(v)); dots[k].setAttribute('visibility', 'visible'); });
        const title = o.x[i] instanceof Date ? moisFr(o.x[i]) : (o.xfmt ? o.xfmt(i) : i);
        const rows = vis.map(s => s.values[i] == null ? '' : `<div class="r"><span><i style="background:${s.color}"></i>${s.name}</span><b>${(s.fmt || o.tipfmt || (v => fr(v)))(s.values[i])}</b></div>`).join('');
        const extra = o.tipExtra ? o.tipExtra(i) : '';
        showTip(`<div class="t">${title}</div>${rows}${extra}`, ev);
        if (o.onHover) o.onHover(i);
      };
      ov.addEventListener('pointermove', move);
      ov.addEventListener('pointerdown', move);
      ov.addEventListener('pointerleave', () => { hideTip(); xl.setAttribute('visibility', 'hidden'); dots.forEach(d => d.setAttribute('visibility', 'hidden')); });
    }
  };
  draw();
  return { draw, set(p) { Object.assign(o, p); draw(); } };
}
const charts = [];
let rto; addEventListener('resize', () => { clearTimeout(rto); rto = setTimeout(() => charts.forEach(c => c.draw()), 120); });

// ---------- thème ----------
const root = document.documentElement;
try { const t = localStorage.getItem('theme'); if (t) root.dataset.theme = t; } catch (e) {}
$('#theme').onclick = () => {
  const dark = root.dataset.theme ? root.dataset.theme === 'dark' : matchMedia('(prefers-color-scheme: dark)').matches;
  root.dataset.theme = dark ? 'light' : 'dark';
  try { localStorage.setItem('theme', root.dataset.theme); } catch (e) {}
  charts.forEach(c => c.draw()); heroLines && heroLines();
};

// ---------- barre de progression et menu actif ----------
const prog = $('#progress');
addEventListener('scroll', () => { const h = document.documentElement; prog.style.width = (h.scrollTop / (h.scrollHeight - h.clientHeight) * 100) + '%'; }, { passive: true });
const navA = $$('#navlinks a');
if ('IntersectionObserver' in window) {
  const io = new IntersectionObserver(es => es.forEach(e => {
    if (e.isIntersecting) navA.forEach(a => a.classList.toggle('on', a.getAttribute('href') === '#' + e.target.id));
  }), { rootMargin: '-45% 0px -50% 0px' });
  $$('section[id]').forEach(s => io.observe(s));
  // apparition au défilement
  const rv = new IntersectionObserver(es => es.forEach(e => { if (e.isIntersecting) { e.target.classList.add('in'); rv.unobserve(e.target); } }), { threshold: .08 });
  $$('.sechead, section .card, .pipeline, .gallery, .viewer').forEach(n => { n.classList.add('reveal'); rv.observe(n); });
}

// ---------- compteurs ----------
function countUp() {
  $$('[data-count]').forEach(n => {
    const target = +n.dataset.count, dec = +n.dataset.dec, t0 = performance.now(), dur = 1400;
    const step = t => { const k = Math.min(1, (t - t0) / dur), e = 1 - Math.pow(1 - k, 3); n.textContent = fr(target * e, dec); if (k < 1) requestAnimationFrame(step); };
    requestAnimationFrame(step);
  });
}

let heroLines = null;

// =====================================================================
Promise.all([fetch('assets/data/modele.json').then(r => r.json()), fetch('assets/data/resultats.json').then(r => r.json())])
.then(([M, R]) => {
  const dates = M.dates.map(s => new Date(s + 'T00:00:00Z'));
  const N = dates.length;
  const P = M.prix;
  const idx = s => dates.findIndex(d => d.toISOString().slice(0, 7) === s);
  const CRISE = { from: idx('2024-01'), to: N - 1, label: 'Crise du cacao' };

  // ---------- hero : courbes animées en fond ----------
  heroLines = () => {
    const svg = $('#hero-lines'); svg.innerHTML = '';
    const W = svg.clientWidth || 1200, H = svg.clientHeight || 600;
    svg.setAttribute('viewBox', `0 0 ${W} ${H}`);
    [['cacao', '--cacao'], ['arabica', '--cafe']].forEach(([k, c], j) => {
      const v = P[k].map(x => Math.log(x / P[k][0]));
      const lo = -1, hi = 2.6;
      let d = '';
      v.forEach((y, i) => { d += (i ? 'L' : 'M') + (i / (N - 1) * W).toFixed(1) + ',' + (H - (y - lo) / (hi - lo) * H * .8 - H * .1).toFixed(1); });
      const p = el('path', { d, fill: 'none', stroke: css(c), 'stroke-width': 2.2, opacity: .55 }, svg);
      const L = p.getTotalLength(); p.style.strokeDasharray = L; p.style.strokeDashoffset = L;
      p.style.transition = `stroke-dashoffset ${2.6 + j * .4}s ease-out`;
      requestAnimationFrame(() => requestAnimationFrame(() => { p.style.strokeDashoffset = 0; }));
    });
  };
  heroLines(); countUp();
  addEventListener('resize', () => { clearTimeout(heroLines.t); heroLines.t = setTimeout(heroLines, 200); });

  // ---------- 01 explorateur ----------
  const SER = [
    { k: 'cacao', name: 'Cacao', c: '--cacao', u: '$/kg', on: true },
    { k: 'arabica', name: 'Café Arabica', c: '--cafe', u: '$/kg', on: true },
    { k: 'robusta', name: 'Café Robusta', c: '--robusta', u: '$/kg', on: true },
    { k: 'sucre', name: 'Sucre', c: '--sucre', u: '$/kg', on: false },
    { k: 'brent', name: 'Pétrole Brent', c: '--brent', u: '$/baril', on: false },
    { k: 'uree', name: 'Urée', c: '--uree', u: '$/t', on: false }];
  let mode = 'base', range = [0, N - 1];
  const chips = $('#chips');
  SER.forEach(s => {
    const b = document.createElement('button'); b.className = 'chip' + (s.on ? '' : ' off');
    b.innerHTML = `<i style="background:var(${s.c})"></i>${s.name}`;
    b.onclick = () => { s.on = !s.on; b.classList.toggle('off', !s.on); updExplorer(); };
    chips.appendChild(b);
  });
  const expSeries = () => SER.map(s => ({
    name: s.name, color: css(s.c), hidden: !s.on,
    values: mode === 'base' ? P[s.k].map(v => 100 * v / P[s.k][range[0]]) : P[s.k],
    fmt: mode === 'base' ? (v => fr(v, 0)) : (v => fr(v, 2) + ' ' + s.u)
  }));
  const exp = lineChart($('#ch-explorer'), { x: dates, series: expSeries(), log: true, range, shades: [CRISE], yfmt: v => v >= 1000 ? fr(v / 1000, 0) + ' k' : (v < 1 ? fr(v, 1) : fr(v, 0)) });
  charts.push(exp);
  function updExplorer() {
    exp.set({ series: expSeries(), range, log: true, hline: mode === 'base' ? 100 : null, tipExtra: null });
    drawBrush();
  }
  $$('#mode button').forEach(b => b.onclick = () => { $$('#mode button').forEach(x => x.classList.toggle('on', x === b)); mode = b.dataset.m; updExplorer(); });

  // frise de sélection (brush)
  const bh = $('#ch-brush');
  let drag = null;
  function drawBrush() {
    bh.innerHTML = '';
    const W = bh.clientWidth, H = bh.clientHeight, ml = 50, mr = 16;
    const svg = el('svg', { viewBox: `0 0 ${W} ${H}` }, bh);
    const tx = i => ml + i / (N - 1) * (W - ml - mr);
    ['cacao', 'arabica'].forEach((k, j) => {
      const v = P[k].map(Math.log), lo = Math.min(...v), hi = Math.max(...v);
      let d = ''; v.forEach((y, i) => d += (i ? 'L' : 'M') + tx(i).toFixed(1) + ',' + (H - 6 - (y - lo) / (hi - lo) * (H - 12)).toFixed(1));
      el('path', { d, fill: 'none', stroke: css(j ? '--cafe' : '--cacao'), 'stroke-width': 1.2, opacity: .7 }, svg);
    });
    el('rect', { x: ml, y: 0, width: W - ml - mr, height: H, fill: 'none', stroke: css('--border'), rx: 6 }, svg);
    el('rect', { x: tx(range[0]), y: 0, width: Math.max(2, tx(range[1]) - tx(range[0])), height: H, fill: css('--accent'), opacity: .16, stroke: css('--accent'), rx: 4 }, svg);
    const toI = ev => { const r = svg.getBoundingClientRect(); return Math.max(0, Math.min(N - 1, Math.round(((ev.clientX - r.left) * (W / r.width) - ml) / (W - ml - mr) * (N - 1)))); };
    svg.onpointerdown = ev => { svg.setPointerCapture(ev.pointerId); drag = toI(ev); };
    svg.onpointermove = ev => { if (drag == null) return; const i = toI(ev); if (Math.abs(i - drag) >= 6) { range = [Math.min(i, drag), Math.max(i, drag)]; exp.set({ series: expSeries(), range }); drawBrush(); } };
    svg.onpointerup = () => { drag = null; };
    svg.ondblclick = () => { range = [0, N - 1]; updExplorer(); };
  }
  drawBrush(); charts.push({ draw: drawBrush });
  $('#ch-explorer').addEventListener('dblclick', () => { range = [0, N - 1]; updExplorer(); });

  // corrélation glissante
  const dl = k => P[k].map((v, i) => i ? Math.log(v) - Math.log(P[k][i - 1]) : null);
  const dc = dl('cacao'), da = dl('arabica');
  const corr = (a, b) => { const n = a.length, ma = a.reduce((s, x) => s + x, 0) / n, mb = b.reduce((s, x) => s + x, 0) / n; let sab = 0, saa = 0, sbb = 0; for (let i = 0; i < n; i++) { sab += (a[i] - ma) * (b[i] - mb); saa += (a[i] - ma) ** 2; sbb += (b[i] - mb) ** 2; } return sab / Math.sqrt(saa * sbb); };
  const rolling = w => dates.map((_, t) => t < w ? null : corr(dc.slice(t - w + 1, t + 1), da.slice(t - w + 1, t + 1)));
  const roll = lineChart($('#ch-roll'), { x: dates, series: [{ name: 'Corrélation', color: css('--accent'), values: rolling(36), area: false, width: 2.2 }], hline: 0, yMin: -0.3, yMax: 0.8, refs: [{ y: 0.22, color: css('--cafe'), label: 'sur toute la période : 0,22', left: true }], shades: [{ from: CRISE.from, to: N - 1 }], tipfmt: v => fr(v, 2), margin: { l: 40 } });
  charts.push(roll);
  $('#win').oninput = e => { $('#winv').textContent = e.target.value; roll.set({ series: [{ name: 'Corrélation', color: css('--accent'), values: rolling(+e.target.value), width: 2.2 }] }); };

  // statistiques descriptives (tableaux B1 et B2 du mémoire)
  const ST = {
    niv: { cols: ['Moy.', 'É.-type', 'Méd.', 'Min', 'Max', 'Asym.', 'Kurt.', 'p JB'], rows: [
      ['Cacao', '--cacao', 2.741, 1.631, 2.39, 0.86, 10.75, 2.473, 9.940, 0.000],
      ['Arabica', '--cafe', 3.652, 1.748, 3.30, 1.20, 9.05, 1.018, 3.817, 0.000],
      ['Robusta', '--robusta', 1.988, 1.033, 1.89, 0.50, 5.81, 1.343, 5.294, 0.000],
      ['Sucre', '--sucre', 0.331, 0.121, 0.32, 0.11, 0.65, 0.278, 2.455, 0.019],
      ['Brent', '--brent', 67.61, 28.31, 66.50, 18.60, 133.90, 0.188, 2.126, 0.003],
      ['Urée', '--uree', 298.0, 159.8, 263.6, 77.4, 925.0, 1.493, 5.876, 0.000]],
      note: 'Unités : $/kg (Brent : $/baril ; urée : $/tonne). En niveau, aucun prix n\'est normal (p Jarque-Bera < 0,05) et le cacao est très asymétrique (2,47), tiré par la flambée de 2024-2025. Calculées sous R directement à partir de la base PostgreSQL.' },
    log: { cols: ['Moy.', 'É.-type', 'Méd.', 'Min', 'Max', 'Asym.', 'Kurt.', 'p JB'], rows: [
      ['log cacao', '--cacao', 0.885, 0.474, 0.871, -0.151, 2.375, 0.533, 4.182, 0.000],
      ['log Arabica', '--cafe', 1.184, 0.479, 1.194, 0.182, 2.203, -0.136, 2.595, 0.220],
      ['log Robusta', '--robusta', 0.557, 0.523, 0.637, -0.693, 1.760, -0.295, 2.956, 0.094],
      ['log sucre', '--sucre', -1.181, 0.399, -1.139, -2.207, -0.431, -0.479, 2.498, 0.000],
      ['log Brent', '--brent', 4.112, 0.476, 4.197, 2.923, 4.897, -0.568, 2.403, 0.000],
      ['log urée', '--uree', 5.565, 0.524, 5.574, 4.349, 6.830, -0.200, 3.035, 0.332]],
      note: 'Le logarithme réduit fortement l\'asymétrie (cacao : de 2,47 à 0,53) et fait des coefficients des élasticités.' } };
  function drawStats(k) {
    const s = ST[k];
    $('#stats').innerHTML = `<thead><tr><th>Série</th>${s.cols.map(c => `<th>${c}</th>`).join('')}</tr></thead><tbody>${s.rows.map(r => `<tr><td><span class="dot" style="background:var(${r[1]})"></span>${r[0]}</td>${r.slice(2).map((v, j) => `<td class="${j === 7 ? (v < 0.05 ? 'pbad' : 'pok') : ''}">${fr(v, j >= 5 ? 3 : Math.abs(v) >= 100 ? 1 : Math.abs(v) >= 10 ? 2 : (j >= 2 && k === 'niv') ? 2 : 3)}</td>`).join('')}</tr>`).join('')}</tbody>`;
    $('#statnote').textContent = s.note;
  }
  drawStats('niv');
  $$('#statmode button').forEach(b => b.onclick = () => { $$('#statmode button').forEach(x => x.classList.toggle('on', x === b)); drawStats(b.dataset.s); });

  // ---------- 02 démarche ----------
  const STEPS = [
    { t: 'Construire la base', s: 'etape1_donnees.R', d: 'Import du fichier Excel de la Banque mondiale, sélection des six séries, passage en logarithme, statistiques descriptives et export en CSV pour R et pour PostgreSQL.', r: '<b>321 mois</b> de janvier 2000 à septembre 2026, aucune valeur manquante.', mini: [['Source', 'Pink Sheet'], ['Séries', '6'], ['Observations', '321']] },
    { t: 'Tester la stationnarité', s: 'etape2_stationnarite.R', d: 'ADF et KPSS, aux hypothèses nulles opposées, en niveau et en différence, complétés par Phillips-Perron et Zivot-Andrews (rupture structurelle).', r: 'Toutes les séries sont <b>I(1)</b> : aucune n\'est I(2), le test des bornes est donc valide.', stat: true },
    { t: 'Choisir le sens', s: 'etape3_exploration_sens.R', d: 'Nous avions prévu d\'expliquer le cacao par le café. Avant d\'estimer quoi que ce soit, nous avons passé le test des bornes dans les deux sens. Cliquez sur la carte pour la retourner.', r: 'Nous <b>inversons la spécification</b> : le café devient la variable expliquée.', flip: true },
    { t: 'Construire l\'ARDL', s: 'etape4_modelisation_ardl.R', d: 'Analyse combinatoire des contrôles (2³ = 8 modèles), sélection des retards avec auto_ardl (AIC contre BIC), test des bornes en cas 3, cas 2 en contrôle.', r: 'Modèle retenu : <b>ARDL(2,1,1,0)</b> avec sucre et Brent ; le BIC est écarté car ses résidus restent autocorrélés.', mini: [['F (cas 3)', '5,88'], ['Borne I(1) à 1 %', '5,55'], ['p-valeur', '0,006']] },
    { t: 'Lire et valider', s: 'etape5_resultats.R', d: 'Multiplicateurs de long terme, modèle à correction d\'erreur, diagnostics avec les fonctions check_* du package performance, VIF, RESET, ARCH, CUSUM.', r: 'Élasticité <b>0,87</b>, ECT <b>−0,060</b>, demi-vie <b>11,3 mois</b> ; tous les diagnostics passent.', mini: [['Court terme', '0,155'], ['Long terme', '0,869'], ['ECT', '−0,060']] },
    { t: 'Éprouver la robustesse', s: 'etape6_robustesse.R', d: 'Huit variantes du modèle, causalité de Granger par Toda-Yamamoto, exogénéité faible pour savoir qui corrige l\'écart à l\'équilibre.', r: 'La cointégration tient dans les <b>9 spécifications</b> ; c\'est le café qui s\'ajuste, pas le cacao.', mini: [['Variantes OK', '9 / 9'], ['Élasticité', '0,69 à 1,16'], ['Ajustement du cacao', 'p = 0,58']] }];
  const sl = $('#steplist'), sv = $('#stepview');
  STEPS.forEach((s, i) => {
    const li = document.createElement('li');
    li.innerHTML = `<button><span class="n">${i + 1}</span><span><b>${s.t}</b><span>${s.s}</span></span></button>`;
    li.firstChild.onclick = () => showStep(i);
    sl.appendChild(li);
  });
  function showStep(i) {
    $$('#steplist button').forEach((b, k) => b.classList.toggle('on', k === i));
    const s = STEPS[i];
    let extra = '';
    if (s.mini) extra = `<div class="minirow">${s.mini.map(m => `<div class="mini-stat">${m[0]}<b>${m[1]}</b></div>`).join('')}</div>`;
    if (s.stat) extra = `<div class="scroll"><table class="tbl"><thead><tr><th>Série (log)</th><th>ADF niveau</th><th>ADF diff.</th><th>KPSS niveau</th><th>KPSS diff.</th></tr></thead><tbody>${R.stationnarite.map(r => `<tr><td>${r.serie.replace('log ', '')}</td><td class="pbad">${fr(r.adf_niv, 2)}</td><td class="pok">${fr(r.adf_dif, 2)}</td><td class="pbad">${fr(r.kpss_niv, 3)}</td><td class="pok">${fr(r.kpss_dif, 3)}</td></tr>`).join('')}</tbody></table></div><p class="note">Valeurs critiques à 5 % : ADF −3,42 (niveau) et −2,87 (différence) ; KPSS 0,146 (niveau) et 0,463 (différence). Rouge : non stationnaire, vert : stationnaire.</p>`;
    if (s.flip) extra = `<div class="flip" id="flip" title="Cliquer pour retourner"><div class="flip-in"><div class="face front"><span class="muted small">Spécification de départ</span><div class="eq">ln Cacao = f(ln Café, sucre, Brent)</div><div class="pv" style="color:var(--bad)">F = 1,86 · p = 0,59</div><span class="small">Aucune cointégration : F est même sous la borne I(0).</span></div><div class="face back"><span class="muted small">Spécification inversée</span><div class="eq">ln Café = f(ln Cacao, sucre, Brent)</div><div class="pv" style="color:var(--ok)">F = 5,88 · p = 0,006</div><span class="small">Cointégration nette, au-dessus de la borne I(1) à 1 %.</span></div></div></div>`;
    sv.innerHTML = `<div class="eyebrow">Étape ${i + 1} sur 6</div><h3>${s.t}</h3><p>${s.d}</p>${extra}<div class="res">→ ${s.r}</div><button class="btn sm" data-open="R/${s.s}">Voir le code de l'étape</button>`;
    sv.style.animation = 'none'; sv.offsetHeight; sv.style.animation = '';
    const f = $('#flip'); if (f) { f.onclick = () => f.classList.toggle('on'); setTimeout(() => f.classList.add('on'), 1600); }
    $('[data-open]', sv).onclick = e => openCode(e.target.dataset.open, true);
  }
  showStep(0);

  // ---------- 03 test des bornes ----------
  const FV = { cafe: { F: M.F, lab: 'ln Café = f(ln Cacao, sucre, Brent)' }, cacao: { F: 1.86, lab: 'ln Cacao = f(ln Café, sucre, Brent)' } };
  let al = 2, dir = 'cafe', gaugeX = null;
  function drawGauge() {
    const host = $('#gauge'); host.innerHTML = '';
    const W = host.clientWidth, H = host.clientHeight, ml = 20, mr = 20, max = 8;
    const svg = el('svg', { viewBox: `0 0 ${W} ${H}` }, host);
    const tx = v => ml + v / max * (W - ml - mr);
    const I0 = M.bornes.I0[al], I1 = M.bornes.I1[al], F = FV[dir].F, y = 70;
    const z = [[0, I0, '--bad', 'Pas de cointégration'], [I0, I1, '--muted', 'Zone d\'indécision'], [I1, max, '--ok', 'Cointégration']];
    z.forEach(([a, b, c, lab]) => {
      el('rect', { x: tx(a), y, width: tx(b) - tx(a), height: 22, fill: css(c), opacity: .22 }, svg);
      if (tx(b) - tx(a) > lab.length * 6.5) { const t = el('text', { x: (tx(a) + tx(b)) / 2, y: y + 40, 'text-anchor': 'middle', fill: css(c), 'font-size': 12, 'font-weight': 600 }, svg); t.textContent = lab; }
    });
    [[I0, 'I(0) = ' + fr(I0, 2), 'end'], [I1, 'I(1) = ' + fr(I1, 2), 'start']].forEach(([v, lab, anc]) => {
      el('line', { x1: tx(v), x2: tx(v), y1: y - 6, y2: y + 28, stroke: css('--text'), 'stroke-width': 1.5 }, svg);
      const t = el('text', { x: tx(v) + (anc === 'end' ? -4 : 4), y: y + 62, 'text-anchor': anc, fill: css('--muted'), 'font-size': 11, 'font-family': 'JetBrains Mono, monospace' }, svg); t.textContent = lab;
    });
    for (let v = 0; v <= max; v++) { const t = el('text', { x: tx(v), y: H - 4, 'text-anchor': 'middle', fill: css('--axis'), 'font-size': 10 }, svg); t.textContent = v; }
    const col = F > I1 ? css('--ok') : F < I0 ? css('--bad') : css('--muted');
    const g = el('g', {}, svg);
    el('line', { x1: 0, x2: 0, y1: y - 30, y2: y + 26, stroke: col, 'stroke-width': 3 }, g);
    el('circle', { cx: 0, cy: y - 34, r: 7, fill: col }, g);
    const t = el('text', { x: 0, y: y - 48, 'text-anchor': 'middle', fill: col, 'font-size': 14, 'font-weight': 700, 'font-family': 'JetBrains Mono' }, g); t.textContent = 'F = ' + fr(F, 2);
    const xEnd = tx(Math.min(F, max - .2)), xStart = gaugeX == null ? tx(0) : gaugeX;
    g.setAttribute('transform', `translate(${xEnd},0)`); gaugeX = xEnd;
    const t0 = performance.now();
    const anim = t => { const k = Math.min(1, (t - t0) / 900), e = 1 - Math.pow(1 - k, 3); g.setAttribute('transform', `translate(${xStart + (xEnd - xStart) * e},0)`); if (k < 1) requestAnimationFrame(anim); };
    requestAnimationFrame(anim);
    const lvl = ['10 %', '5 %', '1 %'][al];
    $('#gverdict').innerHTML = `<span class="mono small">${FV[dir].lab}</span><br>` + (F > I1 ? `F = <b>${fr(F, 2)}</b> dépasse la borne I(1) à ${lvl} (${fr(I1, 2)}) : <b style="color:var(--ok)">cointégration</b>. Il existe une relation de long terme.` : F < I0 ? `F = <b>${fr(F, 2)}</b> est sous la borne I(0) à ${lvl} (${fr(I0, 2)}) : <b style="color:var(--bad)">pas de cointégration</b> dans ce sens.` : `F = <b>${fr(F, 2)}</b> tombe entre les bornes à ${lvl} : on ne peut pas conclure.`);
  }
  drawGauge(); charts.push({ draw: drawGauge });
  $$('#alpha button').forEach(b => b.onclick = () => { $$('#alpha button').forEach(x => x.classList.toggle('on', x === b)); al = +b.dataset.a; drawGauge(); });
  $$('#sens button').forEach(b => b.onclick = () => { $$('#sens button').forEach(x => x.classList.toggle('on', x === b)); dir = b.dataset.d; drawGauge(); });

  // ---------- 04 équilibre ----------
  const obs = M.observe.map(Math.exp), eqv = M.equilibre.map(Math.exp), gp = M.ecart.map(v => (Math.exp(v) - 1) * 100);
  const eqRead = i => { const g = gp[i]; $('#eqread').innerHTML = `<b>${moisFr(dates[i])}</b> : l'Arabica vaut <b>${fr(obs[i], 2)} $/kg</b>, pour un équilibre de long terme de <b>${fr(eqv[i], 2)} $/kg</b>, soit un café <b style="color:${g >= 0 ? 'var(--cafe)' : 'var(--bad)'}">${fr(Math.abs(g), 0)} % ${g >= 0 ? 'au-dessus' : 'au-dessous'}</b> de son équilibre.`; };
  const eqc = lineChart($('#ch-eq'), { x: dates, series: [{ name: 'Observé', color: css('--cafe'), values: obs, width: 2.2, fmt: v => fr(v, 2) + ' $/kg' }, { name: 'Équilibre', color: css('--cacao'), values: eqv, dash: '6 4', width: 2, fmt: v => fr(v, 2) + ' $/kg' }], log: true, shades: [CRISE], yfmt: v => fr(v, v < 1 ? 1 : 0) + ' $', onHover: i => { eqRead(i); gapc.set({ marks: [{ i, y: gp[i], color: css('--accent') }] }); } });
  const gapc = lineChart($('#ch-gap'), { x: dates, series: [], bars: { values: gp, pos: css('--cafe'), neg: css('--bad') }, hline: 0, yfmt: v => fr(v, 0) + ' %', tip: false, margin: { t: 6 } });
  charts.push(eqc, gapc);
  eqRead(idx('2025-02'));

  // ---------- 05 simulateur ----------
  const mult = M.multiplicateurs, LR = M.long_terme[1];
  const H = mult.length - 1;
  const half = mult.findIndex(v => v >= LR / 2);
  const base = P.arabica[N - 1];
  let shock = 50;
  const resp = s => mult.map(m => (Math.exp(m * Math.log(1 + s / 100)) - 1) * 100);
  const lrResp = s => (Math.exp(LR * Math.log(1 + s / 100)) - 1) * 100;
  const sim = lineChart($('#ch-sim'), { x: mult.map((_, i) => i), xTicks: [0, 6, 12, 18, 24, 36, 48, 60], xfmt: i => i + ' m', series: [], margin: { l: 54 } });
  charts.push(sim);
  function updSim() {
    const r = resp(shock), lr = lrResp(shock), col = css('--cafe');
    $('#shockv').textContent = (shock > 0 ? '+' : '') + shock + ' %';
    sim.set({
      series: [{ name: 'Réponse du café', color: col, values: r, area: true, width: 2.6, fmt: v => pct(v) }],
      refs: [{ y: lr, color: css('--accent'), label: 'long terme : ' + pct(lr) }],
      hline: 0, yfmt: v => (v > 0 ? '+' : '') + fr(v, 0) + ' %',
      marks: [{ i: half, y: r[half], color: css('--accent'), label: `moitié de l'effet : ${half} mois`, dy: shock < 0 ? 22 : -12 }],
      tipExtra: i => `<div class="r"><span>Arabica</span><b>${fr(base * (1 + r[i] / 100), 2)} $/kg</b></div>`,
      xfmt: i => i === 0 ? 'mois du choc' : i + ' mois'
    });
    const ro = [['Le mois du choc', r[0]], ['Après 6 mois', r[6]], ['Après 12 mois', r[12]], ['Après 24 mois', r[24]]];
    $('#readouts').innerHTML = ro.map(([k, v]) => `<div class="ro"><div class="k">${k}</div><div class="v">${pct(v)}</div></div>`).join('') +
      `<div class="ro wide"><div class="k">Avec l'Arabica à ${fr(base, 2)} $/kg (septembre 2026), à long terme</div><div class="v">${fr(base * (1 + lr / 100), 2)} $/kg <span class="small muted">(${pct(lr)})</span></div></div>`;
    $$('#presets button').forEach(b => b.classList.toggle('on', +b.dataset.v === shock));
  }
  $('#shock').oninput = e => { shock = +e.target.value; updSim(); };
  $$('#presets button').forEach(b => b.onclick = () => { shock = +b.dataset.v; $('#shock').value = shock; updSim(); });
  updSim();

  // ---------- 06 robustesse ----------
  const lo = 0.5, hi = 1.3, pos = v => ((v - lo) / (hi - lo) * 100).toFixed(1) + '%';
  $('#forest').innerHTML = R.variantes.map((v, k) => `<div class="frow${k ? '' : ' main'}" title="${v.nom} · ${v.periode} · ARDL${v.ordre} · F = ${fr(v.F, 2)} · demi-vie ${fr(v.demi_vie, 1)} mois"><div class="lab">${v.nom.replace(' (Arabica)', '')}</div><div class="ftrack"><div class="bar"></div><div class="ref" style="left:${pos(0.869)}"></div><div class="pt" data-l="${pos(v.elast)}" style="left:${pos(0.869)}"></div></div><div class="v">${fr(v.elast, 2)}</div></div>`).join('') +
    `<div class="faxis"><div></div><div><span>0,5</span><span>0,7</span><span>0,9</span><span>1,1</span><span>1,3</span></div><div></div></div><p class="note">Point = élasticité de long terme ; trait orange = modèle principal (0,87). Survolez une ligne pour le détail. La cointégration est significative à 5 % dans toutes les variantes (p de 0,003 à 0,043).</p>`;
  if (!('IntersectionObserver' in window)) $$('#forest .pt').forEach(p => p.style.left = p.dataset.l);
  else { const fo = new IntersectionObserver(es => es.forEach(e => { if (e.isIntersecting) { $$('#forest .pt').forEach(p => p.style.left = p.dataset.l); fo.disconnect(); } }), { threshold: .3 });
  fo.observe($('#forest')); }
  $('#diags').innerHTML = R.diagnostics.map(d => `<div class="diag"><span class="ck">✓</span><span>${d.test.replace(' – ', ' · ')}</span><span class="p">p = ${fr(d.p, 3)}</span></div>`).join('');

  // ---------- 07 SQL dans le navigateur ----------
  initSQL(dates, P);
}).catch(err => {
  console.error(err);
  document.body.insertAdjacentHTML('afterbegin', '<div style="padding:12px;background:#b2453a;color:#fff;text-align:center">Les données du site ne se chargent pas en ouvrant le fichier directement : ouvrez la page publiée sur GitHub Pages.</div>');
});

// =====================================================================
// SQL : copie de la base dans le navigateur (sql.js = SQLite en WebAssembly)
// =====================================================================
const EX = [
  ['Aperçu', `-- Les 12 derniers mois de la table prix
SELECT * FROM prix ORDER BY date DESC LIMIT 12;`],
  ['Vérification', `-- Combien de mois, de quand à quand, valeurs manquantes ?
SELECT COUNT(*) AS nb_mois, MIN(date) AS debut, MAX(date) AS fin,
       SUM(cacao IS NULL OR arabica IS NULL OR robusta IS NULL
           OR sucre IS NULL OR brent IS NULL OR uree IS NULL) AS lignes_incompletes
FROM prix;`],
  ['Moyennes annuelles', `-- Prix moyens par année
SELECT substr(date, 1, 4) AS annee,
       ROUND(AVG(cacao), 2)   AS cacao_moyen,
       ROUND(AVG(arabica), 2) AS arabica_moyen
FROM prix
GROUP BY annee
ORDER BY annee;`],
  ['Top 5 du cacao', `-- Les cinq mois où le cacao a été le plus cher
SELECT date, cacao, arabica FROM prix ORDER BY cacao DESC LIMIT 5;`],
  ['Moyenne mobile', `-- Fonctions de fenêtrage : moyenne mobile 12 mois et variation sur un an
WITH indicateurs AS (
  SELECT date, cacao,
         AVG(cacao) OVER (ORDER BY date ROWS BETWEEN 11 PRECEDING AND CURRENT ROW) AS mm12,
         cacao / LAG(cacao, 12) OVER (ORDER BY date) - 1 AS var_1an
  FROM prix)
SELECT date, cacao, ROUND(mm12, 2) AS moyenne_mobile_12m,
       ROUND(100 * var_1an, 0) AS variation_1an_pct
FROM indicateurs
WHERE date >= '2023-01-01' AND CAST(substr(date, 6, 2) AS INT) IN (1, 4, 7, 10)
ORDER BY date;`],
  ['Volatilité par période', `-- Volatilité mensuelle et corrélation des variations, par période
WITH v AS (
  SELECT date,
         ln(cacao)   - ln(LAG(cacao)   OVER (ORDER BY date)) AS d_cacao,
         ln(arabica) - ln(LAG(arabica) OVER (ORDER BY date)) AS d_arabica
  FROM prix)
SELECT CASE WHEN date < '2010-01-01' THEN '1. 2000-2009'
            WHEN date < '2024-01-01' THEN '2. 2010-2023'
            ELSE '3. 2024-2026 (crise)' END AS periode,
       COUNT(*) AS mois,
       ROUND(100 * stddev_samp(d_cacao), 1)   AS volatilite_cacao_pct,
       ROUND(100 * stddev_samp(d_arabica), 1) AS volatilite_arabica_pct,
       ROUND(corr(d_cacao, d_arabica), 2)     AS corr_variations
FROM v WHERE d_cacao IS NOT NULL
GROUP BY periode ORDER BY periode;`],
  ['Régression naïve', `-- log café = a + b × log cacao (régression « naïve » en niveau)
SELECT ROUND(regr_slope(ln(arabica), ln(cacao)), 3)     AS pente,
       ROUND(regr_intercept(ln(arabica), ln(cacao)), 3) AS constante,
       ROUND(regr_r2(ln(arabica), ln(cacao)), 3)        AS r2,
       ROUND(corr(ln(cacao), ln(arabica)), 2)           AS corr_niveaux
FROM prix;`],
  ['Jointure', `-- Statistiques par variable, avec la table de description (JOIN)
WITH long AS (
  SELECT 'cacao' AS code, cacao AS valeur FROM prix UNION ALL
  SELECT 'arabica', arabica FROM prix UNION ALL
  SELECT 'robusta', robusta FROM prix UNION ALL
  SELECT 'sucre', sucre FROM prix UNION ALL
  SELECT 'brent', brent FROM prix UNION ALL
  SELECT 'uree', uree FROM prix)
SELECT va.role, va.libelle, va.unite, COUNT(*) AS n,
       ROUND(AVG(l.valeur), 2) AS moyenne,
       ROUND(100 * stddev_samp(l.valeur) / AVG(l.valeur), 1) AS cv_pct
FROM long l JOIN variables va ON va.code = l.code
GROUP BY va.role, va.libelle, va.unite ORDER BY va.role;`]];

function initSQL(dates, P) {
  const ta = $('#sqlin'), out = $('#sqlout'), st = $('#dbstate'), exb = $('#examples');
  let db = null;
  EX.forEach(([k, q], i) => {
    const b = document.createElement('button'); b.textContent = k;
    b.onclick = () => { $$('#examples button').forEach(x => x.classList.toggle('on', x === b)); ta.value = q; run(); };
    exb.appendChild(b);
  });
  ta.value = EX[5][1]; exb.children[5].classList.add('on');
  ta.addEventListener('keydown', e => {
    if ((e.ctrlKey || e.metaKey) && e.key === 'Enter') { e.preventDefault(); run(); }
    if (e.key === 'Tab') { e.preventDefault(); const s = ta.selectionStart; ta.setRangeText('  ', s, ta.selectionEnd, 'end'); }
  });
  $('#run').onclick = run;
  if (typeof initSqlJs !== 'function') { st.textContent = 'SQL indisponible'; return; }
  initSqlJs({ locateFile: f => 'assets/vendor/' + f }).then(SQL => {
    db = new SQL.Database();
    db.run(`CREATE TABLE prix (
      date TEXT PRIMARY KEY, cacao REAL NOT NULL CHECK (cacao > 0), arabica REAL NOT NULL CHECK (arabica > 0),
      robusta REAL NOT NULL CHECK (robusta > 0), sucre REAL NOT NULL CHECK (sucre > 0),
      brent REAL NOT NULL CHECK (brent > 0), uree REAL NOT NULL CHECK (uree > 0));
      CREATE TABLE variables (code TEXT PRIMARY KEY, libelle TEXT, unite TEXT, role TEXT);
      INSERT INTO variables VALUES ('arabica','Coffee, Arabica','$/kg','1. Expliquée'),('cacao','Cocoa','$/kg','2. Explicative principale'),
      ('sucre','Sugar, world','$/kg','3. Contrôle'),('brent','Crude oil, Brent','$/baril','3. Contrôle'),
      ('robusta','Coffee, Robusta','$/kg','4. Robustesse'),('uree','Urea','$/tonne','5. Sensibilité');`);
    const ins = db.prepare('INSERT INTO prix VALUES (?,?,?,?,?,?,?)');
    dates.forEach((d, i) => ins.run([d.toISOString().slice(0, 10), P.cacao[i], P.arabica[i], P.robusta[i], P.sucre[i], P.brent[i], P.uree[i]]));
    ins.free();
    // fonctions de PostgreSQL absentes de SQLite
    db.create_function('ln', x => x > 0 ? Math.log(x) : null);
    db.create_function('exp', x => x == null ? null : Math.exp(x));
    db.create_function('sqrt', x => x == null ? null : Math.sqrt(x));
    const acc = () => ({ n: 0, sx: 0, sy: 0, sxx: 0, syy: 0, sxy: 0 });
    const step2 = (s, y, x) => { if (y == null || x == null) return s; s.n++; s.sx += x; s.sy += y; s.sxx += x * x; s.syy += y * y; s.sxy += x * y; return s; };
    const sxx = s => s.sxx - s.sx * s.sx / s.n, syy = s => s.syy - s.sy * s.sy / s.n, sxy = s => s.sxy - s.sx * s.sy / s.n;
    db.create_aggregate('corr', { init: acc, step: step2, finalize: s => s.n > 1 ? sxy(s) / Math.sqrt(sxx(s) * syy(s)) : null });
    db.create_aggregate('regr_slope', { init: acc, step: step2, finalize: s => s.n > 1 ? sxy(s) / sxx(s) : null });
    db.create_aggregate('regr_intercept', { init: acc, step: step2, finalize: s => s.n > 1 ? (s.sy - sxy(s) / sxx(s) * s.sx) / s.n : null });
    db.create_aggregate('regr_r2', { init: acc, step: step2, finalize: s => s.n > 1 ? sxy(s) ** 2 / (sxx(s) * syy(s)) : null });
    db.create_aggregate('stddev_samp', { init: () => ({ n: 0, s: 0, q: 0 }), step: (a, x) => { if (x == null) return a; a.n++; a.s += x; a.q += x * x; return a; }, finalize: a => a.n > 1 ? Math.sqrt((a.q - a.s * a.s / a.n) / (a.n - 1)) : null });
    st.textContent = '● base chargée : 321 mois'; st.classList.add('ok');
    run();
  }).catch(e => { st.textContent = 'SQL indisponible'; console.error(e); });

  function run() {
    if (!db) { out.innerHTML = '<div class="msg">La base se charge…</div>'; return; }
    const t0 = performance.now();
    try {
      const res = db.exec(ta.value);
      const ms = (performance.now() - t0).toFixed(1);
      if (!res.length) { out.innerHTML = `<div class="msg">Requête exécutée (${ms} ms), aucun résultat à afficher.</div>`; return; }
      const r = res[res.length - 1], rows = r.values.slice(0, 400);
      const f = v => v == null ? '<span class="muted">NULL</span>' : typeof v === 'number' ? (Number.isInteger(v) ? v : (+v.toFixed(4)).toString().replace('.', ',')) : String(v).replace(/</g, '&lt;');
      out.innerHTML = `<table class="tbl"><thead><tr>${r.columns.map(c => `<th>${c}</th>`).join('')}</tr></thead><tbody>${rows.map(row => `<tr>${row.map(v => `<td>${f(v)}</td>`).join('')}</tr>`).join('')}</tbody></table><div class="msg">${r.values.length} ligne${r.values.length > 1 ? 's' : ''} · ${ms} ms${r.values.length > 400 ? ' · 400 premières affichées' : ''}</div>`;
    } catch (e) { out.innerHTML = `<div class="msg err">Erreur : ${String(e.message).replace(/</g, '&lt;')}</div>`; }
  }
}

// ---------- galerie de captures ----------
const GAL = [
  ['figures/figA1_terminal.png', 'A1 · Terminal Linux : création de la table prix et import du CSV (COPY 321)'],
  ['figures/figA2_terminal.png', 'A2 · Terminal : requêtes de vérification et prix moyens par année'],
  ['figures/figA3_sql.png', 'A3 · Analyses SQL : table variables et statistiques par série'],
  ['figures/figA4_sql.png', 'A4 · Fenêtrage, corrélations, volatilité par période, régression'],
  ['figures/figA5_R_sql.png', 'A5 · RStudio : connexion de R à PostgreSQL et exploration des tables'],
  ['figures/figA6_R_sql.png', 'A6 · Requêtes SQL envoyées depuis R, import de la table prix'],
  ['figures/figA7_R_sql.png', 'A7 · Statistiques descriptives en niveau calculées depuis la base'],
  ['figures/figA8_R_sql.png', 'A8 · ARDL estimé sur les données PostgreSQL : F = 5,88, élasticité 0,869'],
  ['figures/figA9_R_sql.png', 'A9 · R réécrit ses résultats dans deux nouvelles tables PostgreSQL']];
const gal = $('#gallery'), lb = $('#lightbox');
GAL.forEach(([src, cap]) => {
  const b = document.createElement('button');
  b.innerHTML = `<img src="${src}" alt="${cap}" loading="lazy"><span>${cap}</span>`;
  b.onclick = () => { $('img', lb).src = src; $('p', lb).textContent = cap; lb.hidden = false; };
  gal.appendChild(b);
});
lb.onclick = e => { if (e.target !== $('img', lb)) lb.hidden = true; };
addEventListener('keydown', e => { if (e.key === 'Escape') lb.hidden = true; });

// ---------- visionneuse de code ----------
const FILES = [
  ['R/projet_cacao_cafe.R', 'Script complet et reproductible : toute l\'analyse en un fichier (annexe F du mémoire).'],
  ['R/travail_R_base_SQL.R', 'R branché sur PostgreSQL : requêtes, statistiques en niveau, ARDL, résultats réécrits dans la base.'],
  ['R/etape1_donnees.R', 'Étape 1 · import du Pink Sheet, préparation, statistiques descriptives.'],
  ['R/etape2_stationnarite.R', 'Étape 2 · ADF, KPSS, Phillips-Perron, Zivot-Andrews.'],
  ['R/etape3_exploration_sens.R', 'Étape 3 · test des bornes dans les deux sens.'],
  ['R/etape4_modelisation_ardl.R', 'Étape 4 · combinaisons de contrôles, auto_ardl, AIC/BIC, test des bornes.'],
  ['R/etape5_resultats.R', 'Étape 5 · long terme, ECM, diagnostics check_*, CUSUM.'],
  ['R/etape6_robustesse.R', 'Étape 6 · robustesse, Toda-Yamamoto, exogénéité faible.'],
  ['R/donnees_site.R', 'Données de ce site : équilibre, multiplicateurs dynamiques, bornes du test.'],
  ['R/figures_rapport.R', 'Figures du mémoire.'],
  ['SQL/base_cacao_cafe_postgresql.sql', 'PostgreSQL · création de la table, import, vérifications.'],
  ['SQL/analyses_sql_postgresql.sql', 'PostgreSQL · jointure, vues, fenêtrage, CORR, REGR.']];
const tabs = $('#tabs'), code = $('#vcode'), cache = {};
let current = FILES[0][0];
async function openCode(path, scroll) {
  const f = FILES.find(x => x[0] === path); if (!f) return;
  current = path;
  $$('#tabs button').forEach(b => b.classList.toggle('on', b.dataset.f === path));
  $('#vdesc').textContent = f[1];
  $('#vlink').href = `${REPO}/blob/main/${path}`;
  if (scroll) $('#code').scrollIntoView();
  code.className = ''; code.textContent = 'Chargement…';
  try {
    if (!cache[path]) { const r = await fetch(path); if (!r.ok) throw 0; cache[path] = await r.text(); }
    code.textContent = cache[path];
    code.className = 'language-' + (path.endsWith('.sql') ? 'sql' : 'r');
    if (window.hljs) hljs.highlightElement(code);
  } catch (e) { code.textContent = 'Fichier indisponible ici : utilisez le lien GitHub.'; }
}
FILES.forEach(([p]) => { const b = document.createElement('button'); b.textContent = p.split('/')[1]; b.dataset.f = p; b.onclick = () => openCode(p); tabs.appendChild(b); });
$('#copy').onclick = async () => { try { await navigator.clipboard.writeText(cache[current] || ''); $('#copy').textContent = 'Copié ✓'; setTimeout(() => $('#copy').textContent = 'Copier', 1500); } catch (e) {} };
openCode(FILES[0][0]);
window.openCode = openCode;
})();
