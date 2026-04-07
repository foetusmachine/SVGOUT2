;; make-test-entities.lsp — Create one of every entity type for SVGOUT2 testing
;; Run command: MAKETEST
;; Entities created (three rows, y=0/50/100):
;;   Row 0 — NATIVE (red):       LINE, LWPOLYLINE, POLYLINE, 3DFACE, SOLID, CIRCLE, ARC
;;   Row 1 — NEW-V25 (green):    ELLIPSE(full), ELLIPSE(partial), POINT, SPLINE, XLINE, RAY, HELIX, WIPEOUT
;;   Row 2 — UNSUPPORTED (blue): TEXT, MTEXT, INSERT, HATCH, DIMENSION

(defun C:MAKETEST ( / old-cmdecho old-osmode old-layer hatch-rect tmp-ename)
  (setq old-cmdecho (getvar "CMDECHO"))
  (setq old-osmode  (getvar "OSMODE"))
  (setq old-layer   (getvar "CLAYER"))
  (setvar "CMDECHO" 0)
  (setvar "OSMODE"  0)
  (vl-load-com)

  ;; ----------------------------------------------------------------
  ;; Create a layer via entmake — works without the LAYER command dialog
  ;; ----------------------------------------------------------------
  (defun mt-make-layer (name aci-colour / )
    (if (null (tblsearch "LAYER" name))
      (entmake (list '(0 . "LAYER")
                     '(100 . "AcDbSymbolTableRecord")
                     '(100 . "AcDbLayerTableRecord")
                     (cons 2  name)
                     '(70 . 0)
                     (cons 62 aci-colour)
                     '(6 . "Continuous")))
    )
  )

  ;; ----------------------------------------------------------------
  ;; Layers
  ;; ----------------------------------------------------------------
  (mt-make-layer "NATIVE"      1)   ; red
  (mt-make-layer "NEW-V25"     3)   ; green
  (mt-make-layer "UNSUPPORTED" 5)   ; blue

  ;; ================================================================
  ;; ROW 0 — NATIVE ENTITIES  (y centred around y=10)
  ;; ================================================================
  (setvar "CLAYER" "NATIVE")

  ;; LINE
  (entmake (list '(0 . "LINE")
                 '(8 . "NATIVE")
                 (cons 10 '(0.0 0.0 0.0))
                 (cons 11 '(18.0 14.0 0.0))))

  ;; LWPOLYLINE — closed rectangle
  (entmake (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(8 . "NATIVE")
                 '(100 . "AcDbPolyline")
                 '(90 . 4)
                 '(70 . 1)
                 (cons 10 '(25.0  2.0))
                 (cons 10 '(43.0  2.0))
                 (cons 10 '(43.0 18.0))
                 (cons 10 '(25.0 18.0))))

  ;; CIRCLE
  (entmake (list '(0 . "CIRCLE")
                 '(8 . "NATIVE")
                 (cons 10 '(60.0 10.0 0.0))
                 (cons 40 8.0)))

  ;; ARC  0→270 deg
  (entmake (list '(0 . "ARC")
                 '(8 . "NATIVE")
                 (cons 10 '(88.0 10.0 0.0))
                 (cons 40 8.0)
                 (cons 50   0.0)
                 (cons 51 270.0)))

  ;; 3DFACE
  (entmake (list '(0 . "3DFACE")
                 '(8 . "NATIVE")
                 (cons 10 '(105.0  2.0 0.0))
                 (cons 11 '(123.0  2.0 0.0))
                 (cons 12 '(123.0 18.0 0.0))
                 (cons 13 '(105.0 18.0 0.0))))

  ;; SOLID (filled quad — note: point order is Z-shaped in DXF)
  (entmake (list '(0 . "SOLID")
                 '(8 . "NATIVE")
                 (cons 10 '(130.0  2.0 0.0))
                 (cons 11 '(148.0  2.0 0.0))
                 (cons 12 '(130.0 18.0 0.0))
                 (cons 13 '(148.0 18.0 0.0))))

  ;; POLYLINE (3D closed triangle)
  (entmake (list '(0 . "POLYLINE")
                 '(8 . "NATIVE")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDb3dPolyline")
                 '(70 . 9)))
  (entmake (list '(0 . "VERTEX") '(8 . "NATIVE")
                 '(100 . "AcDbEntity") '(100 . "AcDb3dPolylineVertex")
                 (cons 10 '(155.0  2.0 0.0)) '(70 . 32)))
  (entmake (list '(0 . "VERTEX") '(8 . "NATIVE")
                 '(100 . "AcDbEntity") '(100 . "AcDb3dPolylineVertex")
                 (cons 10 '(173.0  2.0 0.0)) '(70 . 32)))
  (entmake (list '(0 . "VERTEX") '(8 . "NATIVE")
                 '(100 . "AcDbEntity") '(100 . "AcDb3dPolylineVertex")
                 (cons 10 '(164.0 18.0 0.0)) '(70 . 32)))
  (entmake '((0 . "SEQEND") (8 . "NATIVE")))

  ;; ================================================================
  ;; ROW 1 — NEW V25 ENTITIES  (y centred around y=60)
  ;; ================================================================
  (setvar "CLAYER" "NEW-V25")

  ;; ELLIPSE — full, centre (10,60), semi-major=12 along X, ratio=0.45
  (entmake (list '(0 . "ELLIPSE")
                 '(100 . "AcDbEntity")
                 '(8 . "NEW-V25")
                 '(100 . "AcDbEllipse")
                 (cons 10 '(10.0 60.0 0.0))
                 (cons 11 '(12.0  0.0 0.0))
                 (cons 40 0.45)
                 (cons 41 0.0)
                 (cons 42 (* 2.0 pi))
                 (cons 210 '(0.0 0.0 1.0))))

  ;; ELLIPSE — partial (0.4 → 4.5 rad), centre (38,60)
  (entmake (list '(0 . "ELLIPSE")
                 '(100 . "AcDbEntity")
                 '(8 . "NEW-V25")
                 '(100 . "AcDbEllipse")
                 (cons 10 '(38.0 60.0 0.0))
                 (cons 11 '(10.0  0.0 0.0))
                 (cons 40 0.6)
                 (cons 41 0.4)
                 (cons 42 4.5)
                 (cons 210 '(0.0 0.0 1.0))))

  ;; POINT
  (entmake (list '(0 . "POINT")
                 '(8 . "NEW-V25")
                 (cons 10 '(60.0 60.0 0.0))))

  ;; SPLINE — legacy fit-point form works on all AutoCAD versions:
  ;; SPLINE → pick points → "" to end → start tangent "" → end tangent ""
  (vl-cmdf "._SPLINE"
    '(75.0 52.0 0.0)
    '(83.0 68.0 0.0)
    '(91.0 53.0 0.0)
    '(99.0 67.0 0.0)
    ""    ; end fit point entry
    ""    ; start tangent (Enter = default)
    "")   ; end tangent (Enter = default)
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "NEW-V25")
  )

  ;; XLINE — horizontal through (115,55)
  (vl-cmdf "._XLINE" "H" '(115.0 55.0 0.0) "")
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "NEW-V25")
  )

  ;; RAY — from (115,65) pointing right
  (vl-cmdf "._RAY" '(115.0 65.0 0.0) '(135.0 65.0 0.0) "")
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "NEW-V25")
  )

  ;; HELIX — prompt sequence: base centre → base radius pt → top radius pt
  ;;   → [Axis endpoint/Turns/tWist/Height]: T → turns count
  ;;   → [Axis endpoint/Turns/tWist/Height]: H → height
  ;; The top radius point must be given BEFORE the T/H options.
  (vl-cmdf "._HELIX"
    '(150.0 55.0 0.0)   ; base centre
    '(158.0 55.0 0.0)   ; base radius point (r=8)
    '(154.0 55.0 0.0)   ; top radius point  (r=4)
    "T" "3"             ; Turns = 3
    "H" "20")           ; Height = 20
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "NEW-V25")
  )

  ;; WIPEOUT — vl-cmdf won't pass list coords to WIPEOUT; use "x,y" strings instead.
  (vl-cmdf "._WIPEOUT"
    "170,52"
    "198,52"
    "198,68"
    "170,68"
    "")   ; close
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "NEW-V25")
  )

  ;; ================================================================
  ;; ROW 2 — NOT-YET-SUPPORTED ENTITIES  (y centred around y=110)
  ;; ================================================================
  (setvar "CLAYER" "UNSUPPORTED")

  ;; TEXT
  (entmake (list '(0 . "TEXT")
                 '(8 . "UNSUPPORTED")
                 (cons 10 '(0.0 105.0 0.0))
                 (cons 40 7.0)
                 (cons  1 "TEXT entity")))

  ;; MTEXT — pure entmake. Group 10=insertion, 40=text height, 41=ref width, 1=content.
  (entmake (list '(0 . "MTEXT")
                 '(100 . "AcDbEntity")
                 '(8 . "UNSUPPORTED")
                 '(100 . "AcDbMText")
                 (cons 10 '(40.0 103.0 0.0))
                 (cons 40 7.0)    ; text height
                 (cons 41 50.0)   ; reference (box) width
                 (cons 71 1)      ; attachment point: top-left
                 (cons 72 1)      ; drawing direction: left to right
                 (cons  1 "MTEXT entity")))

  ;; BLOCK definition via entmake (pure DXF — no command needed)
  (if (null (tblsearch "BLOCK" "SVGTEST"))
    (progn
      (entmake '((0 . "BLOCK")
                 (8 . "0")
                 (100 . "AcDbEntity")
                 (100 . "AcDbBlockBegin")
                 (2 . "SVGTEST")
                 (70 . 0)
                 (10 0.0 0.0 0.0)))
      (entmake '((0 . "LINE") (8 . "0")
                 (10 0.0 0.0 0.0) (11 12.0 0.0 0.0)))
      (entmake '((0 . "LINE") (8 . "0")
                 (10 12.0 0.0 0.0) (11 6.0 10.0 0.0)))
      (entmake '((0 . "LINE") (8 . "0")
                 (10 6.0 10.0 0.0) (11 0.0 0.0 0.0)))
      (entmake '((0 . "ENDBLK") (8 . "0")))
    )
  )
  ;; INSERT
  (entmake (list '(0 . "INSERT")
                 '(8 . "UNSUPPORTED")
                 '(2 . "SVGTEST")
                 (cons 10 '(100.0 104.0 0.0))
                 (cons 41 1.0)
                 (cons 42 1.0)
                 (cons 50 0.0)))

  ;; HATCH — pure entmake. Encode a rectangular boundary path directly in DXF.
  ;; Group 91=loop count, 92=boundary type (1=outer), 93=edge count, 72=edge type(1=line),
  ;; 10=edge start, 11=edge end. No associated boundary object needed.
  (entmake (list
    '(0 . "HATCH")
    '(100 . "AcDbEntity")
    '(8 . "UNSUPPORTED")
    '(100 . "AcDbHatch")
    ;; Hatch elevation and normal
    (cons 10 '(0.0 0.0 0.0))       ; elevation point (ignored for flat hatch)
    (cons 210 '(0.0 0.0 1.0))      ; normal
    '(2 . "SOLID")                  ; pattern name
    '(70 . 1)                       ; solid fill flag
    '(71 . 0)                       ; associativity (0=non-associative)
    '(91 . 1)                       ; number of boundary paths (loops)
    ;; Loop 1 — outer boundary, 4 line edges forming a rectangle
    '(92 . 1)                       ; boundary type: 1=outer
    '(93 . 4)                       ; number of edges
    ;; Edge 1
    '(72 . 1)                       ; edge type: 1=line
    (cons 10 '(125.0 104.0))        ; start
    (cons 11 '(160.0 104.0))        ; end
    ;; Edge 2
    '(72 . 1)
    (cons 10 '(160.0 104.0))
    (cons 11 '(160.0 118.0))
    ;; Edge 3
    '(72 . 1)
    (cons 10 '(160.0 118.0))
    (cons 11 '(125.0 118.0))
    ;; Edge 4
    '(72 . 1)
    (cons 10 '(125.0 118.0))
    (cons 11 '(125.0 104.0))
    ;; Source boundary objects count (0 = none)
    '(97 . 0)
    ;; Hatch style and pattern type
    '(75 . 1)                       ; hatch style: 1=normal
    '(76 . 1)                       ; pattern type: 1=predefined
    ;; Seed points
    '(98 . 1)                       ; number of seed points
    (cons 10 '(142.5 111.0))        ; seed point inside boundary
  ))

  ;; DIMENSION (linear) — -DIMLINEAR is command-line safe
  (vl-cmdf "._DIMLINEAR"
    '(0.0 104.0 0.0)     ; first extension line origin
    '(35.0 104.0 0.0)    ; second extension line origin
    '(17.5 99.0 0.0))    ; dimension line location
  (if (entlast)
    (vla-put-layer (vlax-ename->vla-object (entlast)) "UNSUPPORTED")
  )

  ;; ================================================================
  ;; ROW 3 — 3D ENTITIES  (varying Z, tilted normals — exercises projection)
  ;; Base around y=160, x=0..200, Z ranges 0..40
  ;; ================================================================
  (mt-make-layer "3D-TEST" 4)   ; cyan
  (setvar "CLAYER" "3D-TEST")

  ;; LINE rising in Z — diagonal through 3D space
  (entmake (list '(0 . "LINE")
                 '(8 . "3D-TEST")
                 (cons 10 '(0.0  150.0  0.0))
                 (cons 11 '(20.0 170.0 30.0))))

  ;; 3DFACE — tilted quad, all four corners at different Z
  (entmake (list '(0 . "3DFACE")
                 '(8 . "3D-TEST")
                 (cons 10 '(25.0 150.0  0.0))
                 (cons 11 '(55.0 150.0  5.0))
                 (cons 12 '(25.0 170.0 25.0))
                 (cons 13 '(55.0 170.0 35.0))))

  ;; CIRCLE — standard normal, centre lifted 20 units in Z.
  ;; With top-down view this coincides with XY, but in an isometric view it separates.
  ;; Z elevation is stored in group 38; centre OCS x,y maps to WCS x,y.
  (entmake (list '(0 . "CIRCLE")
                 '(8 . "3D-TEST")
                 (cons 10 '(75.0 160.0 0.0))
                 (cons 38 20.0)              ; elevation: 20 units above XY
                 (cons 40 10.0)))

  ;; CIRCLE with tilted normal — exercises svgo-ocs-to-wcs.
  ;; OCS centre placed so WCS result lands near x=75,y=160 when normal=(0,1/√2,1/√2).
  ;; Under that normal the WCS origin maps to (0,0,0), so offset OCS coords accordingly.
  ;; Simplest: put the OCS centre at (75,160,0) — the transform will rotate but keep it visible.
  (entmake (list '(0 . "CIRCLE")
                 '(8 . "3D-TEST")
                 (cons 10 '(75.0 160.0 0.0))
                 (cons 38 0.0)
                 (cons 40 8.0)
                 (cons 210 (list 0.0 (/ 1.0 (sqrt 2.0)) (/ 1.0 (sqrt 2.0))))))

  ;; ARC — Z-elevated, standard normal
  (entmake (list '(0 . "ARC")
                 '(8 . "3D-TEST")
                 (cons 10 '(105.0 160.0 0.0))
                 (cons 38 15.0)              ; elevation
                 (cons 40 10.0)
                 (cons 50   0.0)
                 (cons 51 270.0)))

  ;; ELLIPSE tilted — normal 45° around X axis.
  ;; ELLIPSE stores centre in WCS (not OCS), so provide WCS coords directly.
  (entmake (list '(0 . "ELLIPSE")
                 '(100 . "AcDbEntity")
                 '(8 . "3D-TEST")
                 '(100 . "AcDbEllipse")
                 (cons 10 '(135.0 160.0 10.0))   ; WCS centre
                 (cons 11 '( 14.0   0.0  0.0))   ; major axis vector (WCS-relative)
                 (cons 40 0.5)
                 (cons 41 0.0)
                 (cons 42 (* 2.0 pi))
                 (cons 210 (list 0.0 (/ 1.0 (sqrt 2.0)) (/ 1.0 (sqrt 2.0))))))

  ;; LWPOLYLINE with elevation — flat poly lifted 20 units above XY
  (entmake (list '(0 . "LWPOLYLINE")
                 '(100 . "AcDbEntity")
                 '(8 . "3D-TEST")
                 '(100 . "AcDbPolyline")
                 '(90 . 5)
                 '(70 . 1)
                 (cons 38 20.0)           ; elevation
                 (cons 10 '(160.0 150.0))
                 (cons 10 '(180.0 150.0))
                 (cons 10 '(190.0 160.0))
                 (cons 10 '(180.0 170.0))
                 (cons 10 '(160.0 170.0))))

  ;; 3D POLYLINE — staircase rising in Z
  (entmake (list '(0 . "POLYLINE")
                 '(8 . "3D-TEST")
                 '(100 . "AcDbEntity")
                 '(100 . "AcDb3dPolyline")
                 '(70 . 8)))              ; open 3D polyline
  (foreach pt '((0.0 180.0 0.0) (10.0 180.0 8.0) (20.0 180.0 0.0)
                (30.0 180.0 16.0) (40.0 180.0 0.0) (50.0 180.0 24.0))
    (entmake (list '(0 . "VERTEX") '(8 . "3D-TEST")
                   '(100 . "AcDbEntity") '(100 . "AcDb3dPolylineVertex")
                   (cons 10 pt) '(70 . 32)))
  )
  (entmake '((0 . "SEQEND") (8 . "3D-TEST")))

  ;; ================================================================
  ;; Restore and zoom
  ;; ================================================================
  (setvar "CLAYER" old-layer)
  (setvar "OSMODE"  old-osmode)
  (setvar "CMDECHO" old-cmdecho)
  (vl-cmdf "._ZOOM" "Extents")
  (princ "\n[MAKETEST] Done.")
  (princ "\n  Row 0 (y~10):  NATIVE     — LINE LWPOLYLINE CIRCLE ARC 3DFACE SOLID POLYLINE")
  (princ "\n  Row 1 (y~60):  NEW-V25    — ELLIPSE(x2) POINT SPLINE XLINE RAY HELIX WIPEOUT")
  (princ "\n  Row 2 (y~110): UNSUPPORTED— TEXT MTEXT INSERT HATCH DIMENSION")
  (princ "\n  Row 3 (y~160): 3D-TEST    — LINE 3DFACE CIRCLE(tilt) ARC(tilt) ELLIPSE(tilt) LWPOLY(elev) 3DPOLY")
  (princ "\n  Select all and run SVGOUT2. Unsupported types will warn; all others should render.")
  (princ)
)

(princ "\n[MAKETEST] Loaded. Type MAKETEST to create test entities.")
(princ)
