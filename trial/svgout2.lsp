;; SVGOUT2 — Multi-object AutoCAD -> SVG exporter using current viewport projection

;;; ============================================================
;;; VECTOR MATH HELPERS
;;; ============================================================

(defun svgo-vec-sub (a b)
  ;; Subtract vector b from vector a
  (list (- (car a) (car b))
        (- (cadr a) (cadr b))
        (- (caddr a) (caddr b)))
)

(defun svgo-vec-dot (a b)
  ;; Dot product of two 3D vectors
  (+ (* (car a) (car b))
     (* (cadr a) (cadr b))
     (* (caddr a) (caddr b)))
)

(defun svgo-vec-cross (a b)
  ;; Cross product of two 3D vectors
  (list (- (* (cadr a) (caddr b)) (* (caddr a) (cadr b)))
        (- (* (caddr a) (car b))  (* (car a) (caddr b)))
        (- (* (car a) (cadr b))   (* (cadr a) (car b))))
)

(defun svgo-vec-length (v)
  ;; Euclidean length of a 3D vector
  (sqrt (+ (* (car v) (car v))
           (* (cadr v) (cadr v))
           (* (caddr v) (caddr v))))
)

(defun svgo-vec-normalise (v / len)
  ;; Return unit vector; returns nil if zero-length
  (setq len (svgo-vec-length v))
  (if (> len 1e-10)
    (list (/ (car v) len) (/ (cadr v) len) (/ (caddr v) len))
    nil
  )
)

;;; ============================================================
;;; OCS -> WCS TRANSFORM (Arbitrary Axis Algorithm)
;;; ============================================================

(defun svgo-arbitrary-axis (norm / wx wy wz ax ay)
  ;; Given entity extrusion normal (group 210), return (Xaxis Yaxis) in WCS.
  ;; Implements AutoCAD's Arbitrary Axis Algorithm exactly.
  (setq wx (car norm) wy (cadr norm) wz (caddr norm))
  (if (and (< (abs wx) 0.015625) (< (abs wy) 0.015625))
    ;; Close to Z axis — use world Y to derive X axis
    (setq ax (svgo-vec-normalise (svgo-vec-cross '(0.0 1.0 0.0) norm)))
    ;; Otherwise use world Z
    (setq ax (svgo-vec-normalise (svgo-vec-cross '(0.0 0.0 1.0) norm)))
  )
  (setq ay (svgo-vec-normalise (svgo-vec-cross norm ax)))
  (list ax ay)
)

(defun svgo-ocs-to-wcs (pt norm elev / axes ax ay)
  ;; Transform a 2D OCS point (x y) + elevation to WCS 3D point.
  ;; pt   = (x y) or (x y z) — only x,y used; z ignored (elevation handles Z)
  ;; norm = entity extrusion normal (group 210), default (0 0 1)
  ;; elev = elevation along normal (group 38)
  (if (null norm) (setq norm '(0.0 0.0 1.0)))
  (if (null elev) (setq elev 0.0))
  (setq axes (svgo-arbitrary-axis norm))
  (setq ax (car axes) ay (cadr axes))
  ;; WCS point = x*Xaxis + y*Yaxis + elev*Normal
  (list
    (+ (* (car pt)  (car ax))  (* (cadr pt) (car ay))  (* elev (car norm)))
    (+ (* (car pt)  (cadr ax)) (* (cadr pt) (cadr ay)) (* elev (cadr norm)))
    (+ (* (car pt)  (caddr ax))  (* (cadr pt) (caddr ay))  (* elev (caddr norm)))
  )
)

;;; ============================================================
;;; VIEW MATRIX — WCS -> 2D SCREEN (DCS) PROJECTION
;;; ============================================================

(defun svgo-build-view-matrix ( / vdir vsize target twist
                                   zc xc yc worldup)
  ;; Build orthonormal camera basis from current viewport sysvars.
  ;; Returns a list: (X_cam Y_cam target twist vsize)
  (setq vdir   (getvar "VIEWDIR"))
  (setq vsize  (getvar "VIEWSIZE"))
  (setq target (getvar "TARGET"))
  (setq twist  (getvar "VIEWTWIST"))
  (setq worldup '(0.0 0.0 1.0))

  (setq zc (svgo-vec-normalise vdir))
  (if (null zc)
    (progn
      (princ "\n[SVGOUT2] Error: VIEWDIR is a zero vector — cannot build view matrix.")
      (exit)
    )
  )

  ;; X_cam = normalise(WORLDUP x Z_cam)
  (setq xc (svgo-vec-normalise (svgo-vec-cross worldup zc)))
  (if (null xc)
    ;; VIEWDIR is straight down/up — use alternate world axis
    (setq xc (svgo-vec-normalise (svgo-vec-cross '(0.0 1.0 0.0) zc)))
  )
  (if (null xc)
    (progn
      (princ "\n[SVGOUT2] Error: Cannot compute camera X axis — degenerate view direction.")
      (exit)
    )
  )

  ;; Y_cam = Z_cam x X_cam
  (setq yc (svgo-vec-cross zc xc))

  (list xc yc target twist vsize)
)

(defun svgo-project-point (pt vmat svg-w svg-h / p xc yc tgt twist vsize sx sy rx ry cos-t sin-t)
  ;; Project a WCS 3D point to SVG 2D coordinates.
  ;; vmat = (X_cam Y_cam target twist vsize) from svgo-build-view-matrix
  ;; svg-w, svg-h = SVG canvas dimensions (for centre reference)
  (setq xc    (nth 0 vmat)
        yc    (nth 1 vmat)
        tgt   (nth 2 vmat)
        twist (nth 3 vmat)
        vsize (nth 4 vmat))

  ;; 1. Translate: P' = P - TARGET
  (setq p (svgo-vec-sub pt tgt))

  ;; 2. Project onto camera plane
  (setq sx (svgo-vec-dot p xc))
  (setq sy (svgo-vec-dot p yc))

  ;; 3. Apply VIEWTWIST rotation
  (setq cos-t (cos twist))
  (setq sin-t (sin twist))
  (setq rx (- (* sx cos-t) (* sy sin-t)))
  (setq ry (+ (* sx sin-t) (* sy cos-t)))

  ;; 4. Scale to SVG pixels: scale = svg-w / vsize
  ;;    Flip Y (SVG Y increases downward, AutoCAD Y up)
  (setq rx (* rx (/ (float svg-w) vsize)))
  (setq ry (* ry (/ (float svg-w) vsize)))

  ;; Return as (x y) in SVG space (origin at top-left of viewBox — caller handles offset)
  (list rx (- ry))
)

;;; ============================================================
;;; COLOUR HELPERS
;;; ============================================================

(defun svgo-int-to-hex2 (n / hi lo)
  ;; Convert integer 0-255 to two-digit hex string
  (setq hi (/ n 16)
        lo (rem n 16))
  (strcat
    (nth hi '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F"))
    (nth lo '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "A" "B" "C" "D" "E" "F")))
)

(defun svgo-rgb-to-hex (r g b)
  ;; Convert R G B integers (0-255) to "#RRGGBB" string
  (strcat "#" (svgo-int-to-hex2 r) (svgo-int-to-hex2 g) (svgo-int-to-hex2 b))
)

;; ACI (AutoCAD Color Index) to approximate RGB — standard 255-entry table (common values)
;; Full 255-entry mapping is impractical; we cover indices 1-9 and standard layer colours.
;; For true colour (ACI=256, truecolor property), we read the RGB directly.
(defun svgo-aci-to-rgb (aci)
  ;; Returns (r g b) list for common ACI values; defaults to black for unknowns.
  (cond
    ((= aci 1)  '(255 0   0))    ; Red
    ((= aci 2)  '(255 255 0))    ; Yellow
    ((= aci 3)  '(0   255 0))    ; Green
    ((= aci 4)  '(0   255 255))  ; Cyan
    ((= aci 5)  '(0   0   255))  ; Blue
    ((= aci 6)  '(255 0   255))  ; Magenta
    ((= aci 7)  '(255 255 255))  ; White
    ((= aci 8)  '(128 128 128))  ; Dark grey
    ((= aci 9)  '(192 192 192))  ; Light grey
    (t          '(0   0   0))    ; Default: black
  )
)

(defun svgo-layer-colour-hex (layer-name / ltbl tc clr rgb)
  ;; Resolve a layer's colour to "#RRGGBB" by reading the layer table entry.
  ;; Returns black if layer not found or colour unresolvable.
  (setq ltbl (tblsearch "LAYER" layer-name))
  (if (null ltbl)
    "#000000"
    (progn
      ;; Group 420 = true colour on layer (24-bit packed RGB)
      (setq tc (cdr (assoc 420 ltbl)))
      (if tc
        (svgo-rgb-to-hex (/ tc 65536) (/ (rem tc 65536) 256) (rem tc 256))
        (progn
          ;; Group 62 = ACI colour index (negative means layer is off — use abs)
          (setq clr (cdr (assoc 62 ltbl)))
          (if (or (null clr) (= clr 0))
            "#000000"
            (progn
              (setq clr (abs clr))
              (setq rgb (svgo-aci-to-rgb clr))
              (svgo-rgb-to-hex (car rgb) (cadr rgb) (caddr rgb))
            )
          )
        )
      )
    )
  )
)

(defun svgo-entity-colour-hex (ename ignore-colour / edata clr tc r g b rgb lyr-name)
  ;; Get entity stroke colour as "#RRGGBB" string (pure DXF — no VLA).
  ;; Group 420 = true colour packed as 24-bit integer (R*65536 + G*256 + B).
  ;; Group 62  = ACI colour index (0=ByBlock, 256=ByLayer, 1-255=explicit).
  ;; ByLayer (null or 256): resolve from layer table. ByBlock (0): black fallback.
  (if ignore-colour
    "#000000"
    (progn
      (setq edata (entget ename))
      ;; True colour (group 420) takes priority
      (setq tc (cdr (assoc 420 edata)))
      (if tc
        (progn
          (setq r (/ tc 65536))
          (setq g (/ (rem tc 65536) 256))
          (setq b (rem tc 256))
          (svgo-rgb-to-hex r g b)
        )
        ;; Fall back to ACI (group 62)
        (progn
          (setq clr (cdr (assoc 62 edata)))
          (cond
            ;; ByLayer (absent or 256) — resolve from layer table
            ((or (null clr) (= clr 256))
             (setq lyr-name (cdr (assoc 8 edata)))
             (svgo-layer-colour-hex lyr-name)
            )
            ;; ByBlock — no block context available, fall back to black
            ((= clr 0) "#000000")
            ;; Explicit ACI index
            (t
              (setq rgb (svgo-aci-to-rgb clr))
              (svgo-rgb-to-hex (car rgb) (cadr rgb) (caddr rgb))
            )
          )
        )
      )
    )
  )
)

;;; ============================================================
;;; POINT LIST -> SVG COORDINATE STRING
;;; ============================================================

(defun svgo-pts-to-str (pts prec / result pt)
  ;; Convert a list of (x y) pairs to "x1,y1 x2,y2 ..." string
  (setq result "")
  (foreach pt pts
    (setq result
      (strcat result
        (rtos (car pt) 2 prec) "," (rtos (cadr pt) 2 prec) " "))
  )
  ;; Trim trailing space
  (if (> (strlen result) 0)
    (substr result 1 (1- (strlen result)))
    result)
)

(defun svgo-pts-to-pathd (pts closed prec / result pt first-pt)
  ;; Convert a list of (x y) pairs to SVG path d attribute string
  (if (null pts)
    ""
    (progn
      (setq first-pt (car pts))
      (setq result (strcat "M " (rtos (car first-pt) 2 prec) "," (rtos (cadr first-pt) 2 prec)))
      (foreach pt (cdr pts)
        (setq result
          (strcat result " L " (rtos (car pt) 2 prec) "," (rtos (cadr pt) 2 prec)))
      )
      (if closed (setq result (strcat result " Z")))
      result
    )
  )
)

;;; ============================================================
;;; ENTITY -> SVG ELEMENTS
;;; ============================================================

(defun svgo-project-list (wcs-pts vmat svg-w svg-h)
  ;; Project a list of WCS 3D points to SVG 2D points
  (mapcar '(lambda (p) (svgo-project-point p vmat svg-w svg-h)) wcs-pts)
)

(defun svgo-circle-points (centre radius n-seg norm / result i angle axes ex ey)
  ;; Generate n-seg+1 3D points around a circle in the plane defined by norm.
  ;; The extra closing point (angle=2pi = angle=0) ensures the polygon closes properly
  ;; when projected — without it a 22.5-degree gap appears as a C-shape at oblique angles.
  ;; norm = entity extrusion normal (group 210); points lie in the OCS plane.
  ;; Uses svgo-arbitrary-axis so angle=0 aligns with the OCS X axis (matches AutoCAD).
  (if (null norm) (setq norm '(0.0 0.0 1.0)))
  (setq axes (svgo-arbitrary-axis norm))
  (setq ex (car axes))
  (setq ey (cadr axes))
  (setq result '())
  (setq i 0)
  (while (<= i n-seg)
    (setq angle (* (/ (* 2.0 pi) n-seg) i))
    (setq result
      (append result
        (list (list
          (+ (car centre)   (* radius (cos angle) (car ex))   (* radius (sin angle) (car ey)))
          (+ (cadr centre)  (* radius (cos angle) (cadr ex))  (* radius (sin angle) (cadr ey)))
          (+ (caddr centre) (* radius (cos angle) (caddr ex)) (* radius (sin angle) (caddr ey)))))))
    (setq i (1+ i))
  )
  result
)

(defun svgo-arc-points (centre radius start-ang end-ang n-seg norm / pts i angle step axes ex ey)
  ;; Generate n-seg+1 3D points along an arc in the plane defined by norm.
  ;; norm = entity extrusion normal (group 210).
  ;; Uses svgo-arbitrary-axis so angle=0 aligns with the OCS X axis (matches AutoCAD).
  ;; Sweep forward from start-ang to end-ang (CCW in OCS plane).
  ;; Handles wrap-around (end < start).
  (if (null norm) (setq norm '(0.0 0.0 1.0)))
  (setq axes (svgo-arbitrary-axis norm))
  (setq ex (car axes))
  (setq ey (cadr axes))
  (if (< end-ang start-ang)
    (setq end-ang (+ end-ang (* 2.0 pi)))
  )
  (setq step (/ (- end-ang start-ang) n-seg))
  (setq pts '())
  (setq i 0)
  (while (<= i n-seg)
    (setq angle (+ start-ang (* step i)))
    (setq pts
      (append pts
        (list (list
          (+ (car centre)   (* radius (cos angle) (car ex))   (* radius (sin angle) (car ey)))
          (+ (cadr centre)  (* radius (cos angle) (cadr ex))  (* radius (sin angle) (cadr ey)))
          (+ (caddr centre) (* radius (cos angle) (caddr ex)) (* radius (sin angle) (caddr ey)))))))
    (setq i (1+ i))
  )
  pts
)

(defun svgo-ellipse-points (centre major-vec ratio start-p end-p n-seg norm
                            / pts i param step a b ex ey)
  ;; Generate 3D sample points along an ellipse arc in its local XY plane.
  ;; centre    = WCS centre (3D point)
  ;; major-vec = WCS vector from centre to end of major axis (group 11, already WCS-relative)
  ;; ratio     = minor/major ratio (group 40)
  ;; start-p / end-p = parametric start/end (radians; 0..2pi for full ellipse)
  ;; n-seg     = number of segments
  ;; norm      = entity extrusion normal (group 210) — defines the ellipse plane
  (setq a (svgo-vec-length major-vec))     ; semi-major length
  (setq b (* a ratio))                     ; semi-minor length
  ;; Build orthonormal basis: Xellipse = unit(major-vec)
  ;; Yellipse = unit(norm x Xellipse) — uses actual entity normal so tilted ellipses project correctly
  (setq ex (svgo-vec-normalise major-vec))
  (if (null ex) (setq ex '(1.0 0.0 0.0)))
  (if (null norm) (setq norm '(0.0 0.0 1.0)))
  (setq ey (svgo-vec-normalise (svgo-vec-cross norm ex)))
  (if (null ey) (setq ey (svgo-vec-normalise (svgo-vec-cross '(0.0 1.0 0.0) ex))))
  ;; Handle wrap-around
  (if (< end-p start-p) (setq end-p (+ end-p (* 2.0 pi))))
  (setq step (/ (- end-p start-p) n-seg))
  (setq pts '())
  (setq i 0)
  (while (<= i n-seg)
    (setq param (+ start-p (* step i)))
    (setq pts
      (append pts
        (list
          (list
            (+ (car centre)
               (* a (cos param) (car ex))
               (* b (sin param) (car ey)))
            (+ (cadr centre)
               (* a (cos param) (cadr ex))
               (* b (sin param) (cadr ey)))
            (+ (caddr centre)
               (* a (cos param) (caddr ex))
               (* b (sin param) (caddr ey)))))))
    (setq i (1+ i))
  )
  pts
)

(defun svgo-sample-curve (ename n-seg / total-len step-dist ii wpt pts)
  ;; Sample n-seg+1 WCS 3D points along a vlax-curve entity (SPLINE, HELIX).
  ;; Returns point list or nil on failure.
  (setq total-len
    (vl-catch-all-apply 'vlax-curve-getDistAtParam
      (list ename
        (vl-catch-all-apply 'vlax-curve-getEndParam (list ename)))))
  (if (vl-catch-all-error-p total-len) (setq total-len nil))
  (if (and total-len (not (vl-catch-all-error-p total-len)) (> total-len 0.0))
    (progn
      (setq step-dist (/ total-len (float n-seg)))
      (setq pts '())
      (setq ii 0)
      (while (<= ii n-seg)
        (setq wpt
          (vl-catch-all-apply 'vlax-curve-getPointAtDist
            (list ename (* ii step-dist))))
        (if (and wpt (not (vl-catch-all-error-p wpt)))
          (setq pts (append pts (list wpt)))
        )
        (setq ii (1+ ii))
      )
      pts
    )
    nil
  )
)

(defun svgo-entity-to-svg (ename vmat svg-w svg-h prec stroke-w ignore-col auto-fill
                           / etype edata col-hex fill-val svg-str
                             pt1 pt2 pts proj-pts closed
                             cx cy r sa ea norm elev
                             vertex-e vdata p3 p4
                             ex ey major-vec ratio full-ellipse
                             marker-r dir-vec t-lo t-hi tx ty)
  ;; Dispatch on entity type and return an SVG element string (or nil if unsupported).
  (setq edata (entget ename))
  (setq etype (cdr (assoc 0 edata)))
  (setq col-hex (svgo-entity-colour-hex ename ignore-col))
  (setq fill-val (if auto-fill col-hex "none"))
  (setq svg-str nil)

  (cond

    ;; --- LINE ---
    ((= etype "LINE")
     (setq pt1 (cdr (assoc 10 edata)))
     (setq pt2 (cdr (assoc 11 edata)))
     (setq proj-pts (svgo-project-list (list pt1 pt2) vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <line"
         " x1=\"" (rtos (car  (car  proj-pts)) 2 prec) "\""
         " y1=\"" (rtos (cadr (car  proj-pts)) 2 prec) "\""
         " x2=\"" (rtos (car  (cadr proj-pts)) 2 prec) "\""
         " y2=\"" (rtos (cadr (cadr proj-pts)) 2 prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"none\"/>"))
    )

    ;; --- LWPOLYLINE ---
    ((= etype "LWPOLYLINE")
     (setq pts '())
     (setq norm (cdr (assoc 210 edata)))
     (if (null norm) (setq norm '(0.0 0.0 1.0)))
     (setq elev (cdr (assoc 38 edata)))
     (if (null elev) (setq elev 0.0))
     (foreach pair edata
       (if (= (car pair) 10)
         ;; group 10 is OCS 2D point — transform to WCS
         (setq pts (append pts (list (svgo-ocs-to-wcs (cdr pair) norm elev))))
       )
     )
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq closed (= (logand (cdr (assoc 70 edata)) 1) 1))
     (setq svg-str
       (strcat "    <" (if closed "polygon" "polyline")
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"" (if (and closed auto-fill) col-hex "none") "\"/>"))
    )

    ;; --- POLYLINE (3D polyline or old-style) ---
    ((= etype "POLYLINE")
     (setq pts '())
     (setq closed (= (logand (cdr (assoc 70 edata)) 1) 1))
     ;; Walk vertex entities
     (setq vertex-e (entnext ename))
     (while (and vertex-e
                 (not (equal (cdr (assoc 0 (entget vertex-e))) "SEQEND")))
       (setq vdata (entget vertex-e))
       (if (= (cdr (assoc 0 vdata)) "VERTEX")
         (setq pts (append pts (list (cdr (assoc 10 vdata)))))
       )
       (setq vertex-e (entnext vertex-e))
     )
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <" (if closed "polygon" "polyline")
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"" (if (and closed auto-fill) col-hex "none") "\"/>"))
    )

    ;; --- 3DFACE ---
    ((or (= etype "3DFACE") (= etype "SOLID"))
     (setq pt1 (cdr (assoc 10 edata)))
     (setq pt2 (cdr (assoc 11 edata)))
     (setq p3  (cdr (assoc 12 edata)))
     (setq p4  (cdr (assoc 13 edata)))
     ;; SOLID: DXF point order is 10=p1,11=p2,12=p3,13=p4 in Z-order (bowtie).
     ;; Reorder to p1,p2,p4,p3 for correct quad winding. 3DFACE uses natural order.
     ;; If p3 == p4 it is a triangle.
     (if (equal p3 p4 1e-6)
       (setq pts (list pt1 pt2 p3))
       (if (= etype "SOLID")
         (setq pts (list pt1 pt2 p4 p3))
         (setq pts (list pt1 pt2 p3 p4))
       )
     )
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <polygon"
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"" fill-val "\"/>"))
    )

    ;; --- CIRCLE ---
    ((= etype "CIRCLE")
     (setq norm (cdr (assoc 210 edata)))
     (if (null norm) (setq norm '(0.0 0.0 1.0)))
     (setq elev (cdr (assoc 38 edata)))
     (if (null elev) (setq elev (caddr (cdr (assoc 10 edata)))))
     (if (null elev) (setq elev 0.0))
     (setq cx (svgo-ocs-to-wcs (cdr (assoc 10 edata)) norm elev))
     (setq r  (cdr (assoc 40 edata)))
     (setq pts (svgo-circle-points cx r 32 norm))
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <polygon"
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"" fill-val "\"/>"))
    )

    ;; --- ARC ---
    ((= etype "ARC")
     (setq norm (cdr (assoc 210 edata)))
     (if (null norm) (setq norm '(0.0 0.0 1.0)))
     (setq elev (cdr (assoc 38 edata)))
     (if (null elev) (setq elev (caddr (cdr (assoc 10 edata)))))
     (if (null elev) (setq elev 0.0))
     (setq cx (svgo-ocs-to-wcs (cdr (assoc 10 edata)) norm elev))
     (setq r  (cdr (assoc 40 edata)))
     (setq sa (cdr (assoc 50 edata)))
     (setq ea (cdr (assoc 51 edata)))
     (setq pts (svgo-arc-points cx r sa ea 32 norm))
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <polyline"
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"none\"/>"))
    )

    ;; --- ELLIPSE ---
    ((= etype "ELLIPSE")
     (setq norm (cdr (assoc 210 edata)))
     (if (null norm) (setq norm '(0.0 0.0 1.0)))
     ;; Centre is in WCS for ELLIPSE (not OCS)
     (setq cx (cdr (assoc 10 edata)))
     ;; Major axis endpoint is relative to centre (WCS offset vector)
     (setq major-vec (cdr (assoc 11 edata)))
     (setq ratio (cdr (assoc 40 edata)))
     (setq sa (cdr (assoc 41 edata)))
     (setq ea (cdr (assoc 42 edata)))
     (if (null sa) (setq sa 0.0))
     (if (null ea) (setq ea (* 2.0 pi)))
     (setq full-ellipse (< (abs (- ea sa (* 2.0 pi))) 1e-4))
     (setq pts (svgo-ellipse-points cx major-vec ratio sa ea 32 norm))
     (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
     (setq svg-str
       (strcat "    <" (if full-ellipse "polygon" "polyline")
         " points=\"" (svgo-pts-to-str proj-pts prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"" (if (and full-ellipse auto-fill) col-hex "none") "\"/>"))
    )

    ;; --- POINT ---
    ((= etype "POINT")
     (setq pt1 (cdr (assoc 10 edata)))
     (setq proj-pts (svgo-project-list (list pt1) vmat svg-w svg-h))
     ;; Marker radius: 3x stroke width — stroke-w is already scaled to vbw so this is always visible
     (setq marker-r (* stroke-w 3.0))
     (setq svg-str
       (strcat "    <circle"
         " cx=\"" (rtos (car  (car proj-pts)) 2 prec) "\""
         " cy=\"" (rtos (cadr (car proj-pts)) 2 prec) "\""
         " r=\""  (rtos marker-r 2 2) "\""
         " fill=\"" col-hex "\""
         " stroke=\"none\"/>"))
    )

    ;; --- WIPEOUT ---
    ((= etype "WIPEOUT")
     ;; Boundary vertices stored as group-11 points (first is count, rest are corners)
     ;; Group 71 = vertex count; group 14 = each boundary point (OCS 2D)
     (setq norm (cdr (assoc 210 edata)))
     (if (null norm) (setq norm '(0.0 0.0 1.0)))
     (setq pts '())
     (foreach pair edata
       (if (= (car pair) 14)
         (setq pts (append pts (list (svgo-ocs-to-wcs (cdr pair) norm 0.0))))
       )
     )
     (if pts
       (progn
         (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
         (setq svg-str
           (strcat "    <polygon"
             " points=\"" (svgo-pts-to-str proj-pts prec) "\""
             " stroke=\"none\""
             " fill=\"#FFFFFF\"/>"))
       )
       (setq svg-str nil)
     )
    )

    ;; --- SPLINE ---
    ((= etype "SPLINE")
     (setq pts (svgo-sample-curve ename 64))
     (if pts
       (progn
         (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
         (setq svg-str
           (strcat "    <polyline"
             " points=\"" (svgo-pts-to-str proj-pts prec) "\""
             " stroke=\"" col-hex "\""
             " stroke-width=\"" (rtos stroke-w 2 2) "\""
             " fill=\"none\"/>")))
       (princ "\n[SVGOUT2] Warning: could not sample SPLINE — skipping.")
     )
    )

    ;; --- XLINE / RAY ---
    ((or (= etype "XLINE") (= etype "RAY"))
     ;; Base point(10) in WCS; direction vector(11) in WCS.
     ;; Clip to viewBox extents (derived from current bbox — extended generously).
     ;; We use a large parametric t-range and clip to SVG canvas bounds.
     (setq pt1     (cdr (assoc 10 edata)))
     (setq dir-vec (cdr (assoc 11 edata)))
     (setq pt1 (svgo-project-point pt1 vmat svg-w svg-h))
     ;; Project a second point to get screen-space direction
     (setq pt2 (svgo-project-point
       (list (+ (car  (cdr (assoc 10 edata))) (car  dir-vec))
             (+ (cadr (cdr (assoc 10 edata))) (cadr dir-vec))
             (+ (caddr (cdr (assoc 10 edata))) (caddr dir-vec)))
       vmat svg-w svg-h))
     ;; Screen-space direction
     (setq tx (- (car pt2) (car pt1)))
     (setq ty (- (cadr pt2) (cadr pt1)))
     ;; Use a large clip range in SVG screen units (~10× the canvas size)
     (setq t-lo (if (= etype "XLINE") -8000.0 0.0))
     (setq t-hi 8000.0)
     (setq svg-str
       (strcat "    <line"
         " x1=\"" (rtos (+ (car  pt1) (* t-lo tx)) 2 prec) "\""
         " y1=\"" (rtos (+ (cadr pt1) (* t-lo ty)) 2 prec) "\""
         " x2=\"" (rtos (+ (car  pt1) (* t-hi tx)) 2 prec) "\""
         " y2=\"" (rtos (+ (cadr pt1) (* t-hi ty)) 2 prec) "\""
         " stroke=\"" col-hex "\""
         " stroke-width=\"" (rtos stroke-w 2 2) "\""
         " fill=\"none\"/>"))
    )

    ;; --- HELIX ---
    ((= etype "HELIX")
     (setq pts (svgo-sample-curve ename 64))
     (if pts
       (progn
         (setq proj-pts (svgo-project-list pts vmat svg-w svg-h))
         (setq svg-str
           (strcat "    <polyline"
             " points=\"" (svgo-pts-to-str proj-pts prec) "\""
             " stroke=\"" col-hex "\""
             " stroke-width=\"" (rtos stroke-w 2 2) "\""
             " fill=\"none\"/>")))
       (princ "\n[SVGOUT2] Warning: could not sample HELIX — skipping.")
     )
    )

    ;; --- Unsupported entity type ---
    (t
     (princ (strcat "\n[SVGOUT2] Skipping unsupported entity type: " etype))
     (setq svg-str nil)
    )
  )

  svg-str
)

;;; ============================================================
;;; BOUNDING BOX HELPERS
;;; ============================================================

(defun svgo-bbox-init ()
  ;; Returns initial bbox: (minx miny maxx maxy) with extreme values
  (list 1e38 1e38 -1e38 -1e38)
)

(defun svgo-bbox-expand (bbox pts / minx miny maxx maxy pt)
  ;; Expand bbox to include all points in pts list ((x y) pairs)
  (setq minx (car bbox) miny (cadr bbox) maxx (caddr bbox) maxy (cadddr bbox))
  (foreach pt pts
    (if (< (car pt) minx) (setq minx (car pt)))
    (if (< (cadr pt) miny) (setq miny (cadr pt)))
    (if (> (car pt) maxx) (setq maxx (car pt)))
    (if (> (cadr pt) maxy) (setq maxy (cadr pt)))
  )
  (list minx miny maxx maxy)
)

;;; ============================================================
;;; LAYER GROUPING HELPERS
;;; ============================================================

(defun svgo-entity-layer (ename)
  ;; Return layer name string for entity
  (cdr (assoc 8 (entget ename)))
)

(defun svgo-collect-layers (ss / i ename layers layer)
  ;; Return sorted list of unique layer names from selection set
  (setq layers '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq layer (svgo-entity-layer ename))
    (if (not (member layer layers))
      (setq layers (append layers (list layer)))
    )
    (setq i (1+ i))
  )
  ;; Sort alphabetically (simple insertion sort on strings)
  (svgo-sort-strings layers)
)

(defun svgo-sort-strings (lst / remaining min-str _removed)
  ;; Recursive selection sort for string lists.
  ;; AutoLISP < operator works on strings (lexicographic).
  (if (null lst)
    '()
    (progn
      (setq min-str (car lst))
      (foreach s (cdr lst)
        (if (< s min-str) (setq min-str s))
      )
      ;; Remove only the FIRST occurrence of min-str
      (setq remaining '()
            _removed  nil)
      (foreach s lst
        (if (or _removed (not (equal s min-str)))
          (setq remaining (append remaining (list s)))
          (setq _removed T)
        )
      )
      (cons min-str (svgo-sort-strings remaining))
    )
  )
)

;;; ============================================================
;;; XEDGES DEDUPLICATION
;;; ============================================================

(defun svgo-pt-key (pt tol / x y z)
  ;; Round a 3D point to a grid of size tol — returns a string key
  (setq x (fix (+ 0.5 (/ (car   pt) tol)))
        y (fix (+ 0.5 (/ (cadr  pt) tol)))
        z (fix (+ 0.5 (/ (caddr pt) tol))))
  (strcat (itoa x) "," (itoa y) "," (itoa z))
)

(defun svgo-dedup-ss (src-ss tol / result seen k k2 ed et p1 p2 pc r j en)
  ;; Return a new ss with duplicate LINE/ARC/CIRCLE entities removed.
  ;; Two entities are duplicates if their key geometry matches (forward or reversed).
  ;; tol = coordinate snap tolerance (e.g. 0.001 drawing units).
  (setq result (ssadd))
  (setq seen '())
  (setq j 0)
  (while (< j (sslength src-ss))
    (setq en (ssname src-ss j))
    (setq ed (entget en))
    (setq et (cdr (assoc 0 ed)))
    (cond
      ((= et "LINE")
       (setq p1 (cdr (assoc 10 ed))
             p2 (cdr (assoc 11 ed)))
       (setq k  (strcat "L" (svgo-pt-key p1 tol) "-" (svgo-pt-key p2 tol)))
       (setq k2 (strcat "L" (svgo-pt-key p2 tol) "-" (svgo-pt-key p1 tol)))
       (if (and (not (member k seen)) (not (member k2 seen)))
         (progn (ssadd en result) (setq seen (cons k seen)))
       )
      )
      ((= et "ARC")
       (setq pc (cdr (assoc 10 ed))
             r  (cdr (assoc 40 ed)))
       (setq k (strcat "A" (svgo-pt-key pc tol)
                        "r" (itoa (fix (+ 0.5 (/ r tol))))
                        "s" (itoa (fix (* (cdr (assoc 50 ed)) 1000)))
                        "e" (itoa (fix (* (cdr (assoc 51 ed)) 1000)))))
       (if (not (member k seen))
         (progn (ssadd en result) (setq seen (cons k seen)))
       )
      )
      ((= et "CIRCLE")
       (setq pc (cdr (assoc 10 ed))
             r  (cdr (assoc 40 ed)))
       (setq k (strcat "C" (svgo-pt-key pc tol)
                        "r" (itoa (fix (+ 0.5 (/ r tol))))))
       (if (not (member k seen))
         (progn (ssadd en result) (setq seen (cons k seen)))
       )
      )
      (t
       ;; Non-deduplicable type — always include
       (ssadd en result)
      )
    )
    (setq j (1+ j))
  )
  result
)

;;; ============================================================
;;; MAIN COMMAND: C:SVGOUT2
;;; ============================================================

(defun C:SVGOUT2 ( / *error* old-cmdecho
                     ss prec stroke-w ignore-col auto-fill
                     outpath dwgdir
                     vmat svg-w svg-h
                     layers layer-name layer-map
                     bbox i ename edata etype
                     bb-pts p norm elev
                     elem
                     lyr-name lyr-elems
                     vertex-e vdata cx r sa ea
                     margin-x margin-y vbx vby vbw vbh
                     fh vb-prec svg-stroke-w
                     svg-aspect svg-px-w svg-px-h
                     xedges-run last-ent solid-enames new-ss new-full-ss j je jtype
                     svgo-native-types svgo-acis-types
                     major-vec ratio)

  ;; --- Error handler: restore AutoCAD state on any exit ---
  (defun *error* (msg)
    (if (not (member msg '("Function cancelled" "quit / exit abort")))
      (princ (strcat "\n[SVGOUT2] Error: " msg))
    )
    ;; Close file handle if still open (prevents file lock / corruption)
    (if (and (boundp 'fh) fh)
      (progn (close fh) (setq fh nil))
    )
    ;; Undo XEDGES edges — vla-SendCommand is the only option from *error*
    ;; (vl-cmdf / command / command-s all fail inside error handlers).
    ;; Wrap in vl-catch-all-apply so a failure here doesn't chain-crash.
    (if xedges-run
      (progn
        (if (vl-catch-all-error-p
              (vl-catch-all-apply 'vla-SendCommand
                (list (vla-get-ActiveDocument (vlax-get-acad-object))
                      "._UNDO Back\n")))
          (princ "\n[SVGOUT2] WARNING: Could not undo XEDGES — type UNDO Back manually to remove stray edges.")
          (princ "\n[SVGOUT2] XEDGES undone (error path).")
        )
      )
    )
    (setvar "CMDECHO" old-cmdecho)
    (princ)
  )

  (setq old-cmdecho (getvar "CMDECHO"))
  (setq xedges-run nil)
  (setvar "CMDECHO" 0)
  (vl-load-com)

  (princ "\n[SVGOUT2] Select objects to export:")
  (setq ss (ssget))
  (if (null ss)
    (progn
      (princ "\n[SVGOUT2] No objects selected. Exiting.")
      (exit)
    )
  )
  (princ (strcat "\n[SVGOUT2] " (itoa (sslength ss)) " object(s) selected."))

  ;; --- Auto-XEDGES: run on ACIS entity types that XEDGES can extract edges from ---
  ;; Only these specific ACIS types trigger XEDGES. Other unsupported types are
  ;; warned about but not processed — inverse logic caused XEDGES to fire on
  ;; TEXT, INSERT, HATCH, etc. which crashed or produced garbage.
  (setq svgo-native-types
    '("LINE" "LWPOLYLINE" "POLYLINE" "3DFACE" "SOLID" "CIRCLE" "ARC"
      "ELLIPSE" "POINT" "WIPEOUT" "SPLINE" "XLINE" "RAY" "HELIX"))
  (setq svgo-acis-types
    '("3DSOLID" "BODY" "REGION" "SURFACE" "PLANESURFACE"))
  (setq solid-enames '())
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq etype (cdr (assoc 0 (entget ename))))
    (if (member etype svgo-acis-types)
      (setq solid-enames (append solid-enames (list ename)))
    )
    (setq i (1+ i))
  )
  (if solid-enames
    (progn
      (princ (strcat "\n[SVGOUT2] Found " (itoa (length solid-enames))
                     " non-native entity(ies) — running XEDGES automatically."))
      ;; Mark undo state so we can roll back the new edge entities after export
      (vl-cmdf "._UNDO" "Mark")
      (setq xedges-run T)
      ;; Record the last entity before XEDGES so we can identify new ones
      (setq last-ent (entlast))
      ;; Run XEDGES on each solid
      (foreach sename solid-enames
        (vl-cmdf "._XEDGES" sename "")
      )
      ;; Collect top-level entities added after last-ent by walking the db forward.
      ;; Skip sub-entity types (VERTEX, SEQEND, ATTRIB, ATTDEF) — these are owned
      ;; sub-objects of complex entities and must not be added to a selection set directly.
      (setq new-ss (ssadd))
      (setq ename (if last-ent (entnext last-ent) (entnext nil)))
      (while ename
        (setq etype (cdr (assoc 0 (entget ename))))
        (if (not (member etype '("VERTEX" "SEQEND" "ATTRIB" "ATTDEF")))
          (ssadd ename new-ss)
        )
        (setq ename (entnext ename))
      )
      (if (> (sslength new-ss) 0)
        (progn
          (princ (strcat "\n[SVGOUT2] XEDGES created " (itoa (sslength new-ss)) " edge entity(ies)."))
          ;; Deduplicate — XEDGES on shared edges between regions produces duplicates
          (setq ss (svgo-dedup-ss new-ss 0.001))
          (princ (strcat "\n[SVGOUT2] After dedup: " (itoa (sslength ss)) " edge(s)."))
        )
        (progn
          ;; XEDGES produced nothing — remove non-native entities from ss so they
          ;; don't hit the unsupported-type warning in the SVG pass.
          (setq new-full-ss (ssadd))
          (setq j 0)
          (while (< j (sslength ss))
            (setq je (ssname ss j))
            (setq jtype (cdr (assoc 0 (entget je))))
            (if (member jtype svgo-native-types)
              (ssadd je new-full-ss)
            )
            (setq j (1+ j))
          )
          (setq ss new-full-ss)
          (princ "\n[SVGOUT2] Warning: XEDGES produced no new entities.")
        )
      )
    )
  )

  ;; --- User prompts ---
  (initget 6)  ; No zero, no negative (precision must be >= 1)
  (setq prec (getint "\nDecimal precision [2]: "))
  (if (null prec) (setq prec 2))
  (if (< prec 0) (setq prec 0))
  (if (> prec 8) (setq prec 8))

  (initget 4)
  (setq stroke-w (getreal "\nStroke width multiplier (0=hairline) [0]: "))
  (if (null stroke-w) (setq stroke-w 0.0))
  (if (< stroke-w 0.0) (setq stroke-w 1.0))

  (initget "Yes No")
  (setq ignore-col (getkword "\nIgnore object colour? (all black) [Yes/No] <No>: "))
  (setq ignore-col (equal ignore-col "Yes"))

  (initget "Yes No")
  (setq auto-fill (getkword "\nAuto-fill closed shapes? [Yes/No] <No>: "))
  (setq auto-fill (equal auto-fill "Yes"))

  ;; --- Output path ---
  (setq dwgdir (getvar "DWGPREFIX"))
  (setq outpath
    (getfiled "Save SVG As" dwgdir "svg" 1))
  (if (null outpath)
    (progn
      (princ "\n[SVGOUT2] No output path specified. Exiting.")
      (exit)
    )
  )
  ;; Ensure .svg extension
  (if (not (= (strcase (substr outpath (- (strlen outpath) 3))) ".SVG"))
    (setq outpath (strcat outpath ".svg"))
  )

  ;; --- Build view matrix ---
  (setq vmat (svgo-build-view-matrix))
  (setq svg-w 800)
  (setq svg-h 600)
  ;; svg-stroke-w is computed after bbox is known (see below — scaled from vbw)

  ;; --- Collect layers ---
  (setq layers (svgo-collect-layers ss))
  (princ (strcat "\n[SVGOUT2] Layers found: " (itoa (length layers))))

  ;; --- Pass 1: bbox only — project key points for every entity to compute viewBox ---
  (setq bbox (svgo-bbox-init))
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq edata (entget ename))
    (setq etype (cdr (assoc 0 edata)))
    (setq bb-pts '())
    (cond
      ((or (= etype "LINE") (= etype "3DFACE") (= etype "SOLID"))
       (foreach code '(10 11 12 13)
         (setq p (cdr (assoc code edata)))
         (if p (setq bb-pts (append bb-pts (list (svgo-project-point p vmat svg-w svg-h)))))
       ))
      ((= etype "LWPOLYLINE")
       (setq norm (cdr (assoc 210 edata)))
       (if (null norm) (setq norm '(0.0 0.0 1.0)))
       (setq elev (cdr (assoc 38 edata)))
       (if (null elev) (setq elev 0.0))
       (foreach pair edata
         (if (= (car pair) 10)
           (setq bb-pts
             (append bb-pts
               (list (svgo-project-point
                 (svgo-ocs-to-wcs (cdr pair) norm elev) vmat svg-w svg-h))))))
      )
      ((= etype "POLYLINE")
       (setq vertex-e (entnext ename))
       (while (and vertex-e
                   (not (equal (cdr (assoc 0 (entget vertex-e))) "SEQEND")))
         (setq vdata (entget vertex-e))
         (if (= (cdr (assoc 0 vdata)) "VERTEX")
           (setq bb-pts
             (append bb-pts
               (list (svgo-project-point (cdr (assoc 10 vdata)) vmat svg-w svg-h)))))
         (setq vertex-e (entnext vertex-e))
       ))
      ((= etype "CIRCLE")
       (setq norm (cdr (assoc 210 edata)))
       (if (null norm) (setq norm '(0.0 0.0 1.0)))
       (setq elev (cdr (assoc 38 edata)))
       (if (null elev) (setq elev (caddr (cdr (assoc 10 edata)))))
       (if (null elev) (setq elev 0.0))
       (setq cx (svgo-ocs-to-wcs (cdr (assoc 10 edata)) norm elev))
       (setq r  (cdr (assoc 40 edata)))
       (setq bb-pts
         (svgo-project-list (svgo-circle-points cx r 8 norm) vmat svg-w svg-h))
      )
      ((= etype "ARC")
       (setq norm (cdr (assoc 210 edata)))
       (if (null norm) (setq norm '(0.0 0.0 1.0)))
       (setq elev (cdr (assoc 38 edata)))
       (if (null elev) (setq elev (caddr (cdr (assoc 10 edata)))))
       (if (null elev) (setq elev 0.0))
       (setq cx (svgo-ocs-to-wcs (cdr (assoc 10 edata)) norm elev))
       (setq r  (cdr (assoc 40 edata)))
       (setq sa (cdr (assoc 50 edata)))
       (setq ea (cdr (assoc 51 edata)))
       (setq bb-pts
         (svgo-project-list (svgo-arc-points cx r sa ea 8 norm) vmat svg-w svg-h))
      )
      ((= etype "ELLIPSE")
       (setq norm (cdr (assoc 210 edata)))
       (if (null norm) (setq norm '(0.0 0.0 1.0)))
       (setq cx (cdr (assoc 10 edata)))
       (setq major-vec (cdr (assoc 11 edata)))
       (setq ratio (cdr (assoc 40 edata)))
       (setq sa (cdr (assoc 41 edata)))
       (setq ea (cdr (assoc 42 edata)))
       (if (null sa) (setq sa 0.0))
       (if (null ea) (setq ea (* 2.0 pi)))
       (setq bb-pts
         (svgo-project-list (svgo-ellipse-points cx major-vec ratio sa ea 8 norm) vmat svg-w svg-h))
      )
      ((= etype "POINT")
       (setq p (cdr (assoc 10 edata)))
       (setq bb-pts (list (svgo-project-point p vmat svg-w svg-h)))
      )
      ((= etype "WIPEOUT")
       (setq norm (cdr (assoc 210 edata)))
       (if (null norm) (setq norm '(0.0 0.0 1.0)))
       (foreach pair edata
         (if (= (car pair) 14)
           (setq bb-pts
             (append bb-pts
               (list (svgo-project-point
                 (svgo-ocs-to-wcs (cdr pair) norm 0.0) vmat svg-w svg-h))))))
      )
      ((or (= etype "SPLINE") (= etype "HELIX"))
       (setq bb-pts (svgo-sample-curve ename 8))
       (if bb-pts
         (setq bb-pts (svgo-project-list bb-pts vmat svg-w svg-h))
       )
      )
      ((or (= etype "XLINE") (= etype "RAY"))
       ;; Use only base point — endpoints are ±8000 units and would blow out the viewBox.
       (setq p (cdr (assoc 10 edata)))
       (setq bb-pts (list (svgo-project-point p vmat svg-w svg-h)))
      )
    )
    (if bb-pts
      (setq bbox (svgo-bbox-expand bbox bb-pts)))
    (setq i (1+ i))
  )

  ;; --- Warn if no geometry found ---
  (if (= (car bbox) 1e38)
    (progn
      (princ (strcat "\n[SVGOUT2] WARNING: No supported entities found in selection."))
      (princ "\n[SVGOUT2] Supported types: LINE, LWPOLYLINE, POLYLINE, 3DFACE, SOLID, CIRCLE, ARC, ELLIPSE, POINT, WIPEOUT, SPLINE, XLINE, RAY, HELIX")
      (princ "\n[SVGOUT2] Non-native types (3DSOLID, REGION, SURFACE, etc.) are processed via auto-XEDGES."))
  )

  ;; --- Compute viewBox with 5% margin ---
  (setq vbw (- (caddr bbox) (car bbox)))
  (setq vbh (- (cadddr bbox) (cadr bbox)))
  ;; Handle degenerate (single point) case
  (if (< vbw 1e-6) (setq vbw 10.0))
  (if (< vbh 1e-6) (setq vbh 10.0))
  (setq margin-x (* vbw 0.05))
  (setq margin-y (* vbh 0.05))
  (setq vbx (- (car bbox) margin-x))
  (setq vby (- (cadr bbox) margin-y))
  (setq vbw (+ vbw (* 2.0 margin-x)))
  (setq vbh (+ vbh (* 2.0 margin-y)))

  ;; --- Compute stroke width from geometry extent (not VIEWSIZE) ---
  ;; Anchored to vbw so stroke is always proportional to the drawing regardless of viewport zoom.
  ;; stroke-w=0 (hairline): 0.05% of viewBox width
  ;; stroke-w=1: 0.3% of viewBox width
  (if (= stroke-w 0.0)
    (setq svg-stroke-w (* vbw 0.0005))
    (setq svg-stroke-w (* vbw stroke-w 0.003))
  )

  ;; --- Pass 2: build SVG element strings now that stroke width is known ---
  (setq layer-map '())
  (foreach lyr layers
    (setq layer-map (append layer-map (list (list lyr '()))))
  )
  (setq i 0)
  (while (< i (sslength ss))
    (setq ename (ssname ss i))
    (setq layer-name (svgo-entity-layer ename))
    (setq elem
      (svgo-entity-to-svg ename vmat svg-w svg-h prec svg-stroke-w ignore-col auto-fill))
    (if elem
      (setq layer-map
        (mapcar
          '(lambda (entry)
             (if (equal (car entry) layer-name)
               (list (car entry) (append (cadr entry) (list elem)))
               entry))
          layer-map))
    )
    (setq i (1+ i))
  )

  ;; --- Write SVG file ---
  (setq fh (open outpath "w"))
  (if (null fh)
    (progn
      (princ (strcat "\n[SVGOUT2] Error: could not open output file: " outpath))
      (setvar "CMDECHO" old-cmdecho)
      (exit)
    )
  )

  (write-line "<?xml version=\"1.0\" encoding=\"utf-8\"?>" fh)
  (write-line "<!-- Generator: SVGOUT2 AutoLISP by DP project -->" fh)
  ;; Derive canvas dimensions from viewBox aspect ratio, max 800px on longest side
  (setq vb-prec (max prec 4))
  (setq svg-aspect (/ vbw vbh))
  (if (>= svg-aspect 1.0)
    (progn (setq svg-px-w 800) (setq svg-px-h (fix (+ 0.5 (/ 800.0 svg-aspect)))))
    (progn (setq svg-px-h 800) (setq svg-px-w (fix (+ 0.5 (* 800.0 svg-aspect)))))
  )
  (write-line
    (strcat "<svg version=\"1.1\" xmlns=\"http://www.w3.org/2000/svg\""
      " width=\"" (itoa svg-px-w) "px\""
      " height=\"" (itoa svg-px-h) "px\""
      " viewBox=\""
        (rtos vbx 2 vb-prec) " "
        (rtos vby 2 vb-prec) " "
        (rtos vbw 2 vb-prec) " "
        (rtos vbh 2 vb-prec)
      "\">"
      )
    fh)

  (foreach entry layer-map
    (setq lyr-name  (car entry))
    (setq lyr-elems (cadr entry))
    (if lyr-elems
      (progn
        (write-line (strcat "  <g id=\"" lyr-name "\">") fh)
        (foreach el lyr-elems
          (write-line el fh)
        )
        (write-line "  </g>" fh)
      )
    )
  )

  (write-line "</svg>" fh)
  (close fh)

  ;; --- Undo XEDGES additions — leave drawing in pre-export state ---
  (if xedges-run
    (progn
      (vl-cmdf "._UNDO" "Back")
      (princ "\n[SVGOUT2] XEDGES undone — drawing restored.")
    )
  )

  (setvar "CMDECHO" old-cmdecho)
  (princ (strcat "\n[SVGOUT2] SVG written to: " outpath))
  (princ)
)

(princ "\n[SVGOUT2] v42 Loaded. Type SVGOUT2 to run.")
(princ)
