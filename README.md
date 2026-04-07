# SVGOUT2

An AutoLISP command that exports selected AutoCAD entities to a single SVG file, using the current viewport's parallel projection. AutoCAD has no built-in SVG export.

> **⚠️ DISCLAIMER: This is a 100% AI-generated ("vibecoded") tool.**
> It was built entirely through AI pair-programming. The code has been tested only on **AutoCAD 2026**.
> It may not work on other versions. It will not work for many entity types.
> **Use at your own risk.** No warranty is provided — see [LICENSE](LICENSE).

---

## What It Does

AutoCAD has no native SVG export capability. **SVGOUT2** fills that gap — select any number of entities across any layers and produce a single SVG file that matches what your current viewport is showing.

- Exports selected entities to a single `.svg` file
- Uses the current viewport's 3D parallel projection (VIEWDIR, VIEWTWIST, VIEWSIZE, TARGET)
- Groups output by layer (`<g id="layername">`) with per-layer stroke colour
- Auto-fits the SVG `viewBox` to the bounding box of all projected geometry (+ 5% margin)
- Prompts for: decimal precision, stroke width, ignore object colour, auto-fill, output path

---

## Quick Start

1. **Download** `trial/svgout2.lsp` — that's the only file you need.
2. **Load it in AutoCAD:**
   ```
   (load "C:/path/to/svgout2.lsp")
   ```
3. **Run the command:**
   ```
   SVGOUT2
   ```
4. Select entities, answer the prompts, and an SVG file is written to your chosen path.

### Test Drawing Generator

The package includes `trial/make-test-entities.lsp` — a helper that creates one of every supported (and unsupported) entity type so you can verify SVGOUT2 works in your AutoCAD version.

```
(load "C:/path/to/make-test-entities.lsp")
MAKETEST
```

This creates three rows of labelled entities across three layers:
- **Row 1 (NATIVE, red):** LINE, LWPOLYLINE, CIRCLE, ARC, 3DFACE, SOLID, POLYLINE
- **Row 2 (NEW-V25, green):** ELLIPSE, POINT, SPLINE, XLINE, RAY, HELIX, WIPEOUT
- **Row 3 (UNSUPPORTED, blue):** TEXT, MTEXT, INSERT, HATCH, DIMENSION

Select all and run `SVGOUT2` to see what works on your version.

---

## Supported Entity Types

| Status | Entity Types |
|---|---|
| ✅ **Fully supported** | LINE, LWPOLYLINE, POLYLINE, CIRCLE, ARC, 3DFACE, SOLID |
| ✅ **Supported (v25+)** | ELLIPSE, POINT, WIPEOUT, SPLINE, XLINE, RAY, HELIX |
| ⚡ **Auto-converted via XEDGES** | 3DSOLID, BODY, REGION, SURFACE, PLANESURFACE |
| ❌ **Not supported** | TEXT, MTEXT, INSERT (blocks), HATCH, DIMENSION, LEADER, MLEADER, MESH, TABLE |

For ACIS-based entities (3DSOLID, REGION, etc.), the tool automatically runs `XEDGES` to extract wireframe edges, exports them, then undoes the extraction — your drawing is not modified.

---

## Limitations

> **This tool has significant limitations. Please read before using.**

- **AI-generated code** — this was built entirely by AI. Edge cases, untested AutoCAD versions, and unusual geometry may produce incorrect output or errors.
- **Tested on AutoCAD 2026 only** — other versions may lack required functions (e.g. `GETPROPERTYVALUE`, `vlax-curve-*`). No compatibility testing has been done.
- **Parallel projection only** — the `PERSPECTIVE` system variable must be `0`. Perspective views are not supported.
- **No hidden-line removal** — all visible edges are exported regardless of occlusion. Overlapping geometry will overlap in the SVG.
- **No shading or rendering** — wireframe output only. Filled faces (via auto-fill prompt) use flat colour, not shading.
- **No text or annotations** — TEXT, MTEXT, DIMENSION, LEADER, and TABLE entities are silently skipped with a warning.
- **No block support** — INSERT entities (block references) are not exploded or exported.
- **No hatch patterns** — HATCH entities are not supported.
- **SPLINE/HELIX sampling** — these are approximated by sampling points along the curve. Complex splines may lose detail.
- **SVG display size** — fixed at max 800px on the longest side; the `viewBox` ensures correct aspect ratio.

---

## Requirements

- **AutoCAD 2026** (the only tested version)
- **Parallel projection** (`PERSPECTIVE` = 0)
- May work on earlier AutoCAD versions that support `GETPROPERTYVALUE`, `vlax-curve-getPointAtDist`, and Visual LISP COM (`vl-load-com`) — but this is **untested and unsupported**.

---

## Prompts

When you run `SVGOUT2`, you'll be asked:

| Prompt | Default | Description |
|---|---|---|
| Decimal precision | `2` | Number of decimal places in SVG coordinates |
| Stroke width multiplier | `0` (hairline) | `0` = thinnest possible line; higher values = thicker |
| Ignore object colour? | `No` | `Yes` = all black; `No` = uses layer colours |
| Auto-fill closed shapes? | `No` | `Yes` = fills closed polygons with their stroke colour |
| Output file path | DWG folder | Full path for the `.svg` output file |

---

## How It Works

1. Reads the current viewport projection parameters (VIEWDIR, VIEWTWIST, VIEWSIZE, TARGET)
2. Builds a WCS→screen projection matrix from these parameters
3. For each selected entity, transforms OCS coordinates to WCS (using the Arbitrary Axis Algorithm where needed), then projects to 2D screen coordinates
4. Groups entities by layer and assigns per-layer stroke colours (ACI or true colour)
5. Computes a bounding box with 5% margin and writes the SVG with auto-fitted `viewBox`

For ACIS entities (3DSOLID, REGION, etc.), the tool sets an UNDO mark, runs XEDGES to extract edges, exports, then runs UNDO Back — your drawing is restored to its original state.

---

## Troubleshooting

- **Empty SVG output** — your selection likely contains only unsupported entity types (TEXT, INSERT, HATCH, etc.). Check the AutoCAD command line for warnings.
- **SVG looks rotated** — make sure `PERSPECTIVE` is `0`. Switch to a parallel projection view before exporting.
- **Invisible lines** — try increasing the stroke width multiplier from `0` to `1` or `2`.
- **Colours are all black** — answer `No` to "Ignore object colour?" and ensure your layers have assigned colours.
- **AutoCAD freezes during export** — this can happen with very large selections or complex ACIS geometry during XEDGES. Try selecting fewer entities.
- **Function not found errors** — your AutoCAD version may not support required functions. This tool is only tested on AutoCAD 2026.

---

## Project Structure

```
svgout/
├── trial/
│   ├── svgout2.lsp            ← The tool (this is all you need)
│   └── make-test-entities.lsp ← Test drawing generator (creates sample entities)
├── docs/                      ← Internal project documentation
├── README.md                  ← This file
└── LICENSE                    ← MIT License
```

---

## About

This tool was built to fill a gap: AutoCAD has no native SVG export. SVGOUT2 handles multiple entities, multiple layers, and respects the current 3D parallel projection.

**This is a vibecoded project.** The entire codebase — every line of AutoLISP — was generated through AI pair-programming using [Windsurf](https://windsurf.com). The human operator directed the work, tested in AutoCAD, and reported results back to the AI. No AutoLISP was written by hand.

It works for the author's purposes. It is shared in the hope it may be useful to others, but with no guarantees whatsoever.

---

## License

[MIT](LICENSE) — do whatever you want with it. See the license file for full terms.

**No warranty.** This software is provided "as is". The authors are not liable for any damage arising from its use.
