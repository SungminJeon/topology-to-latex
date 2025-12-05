(* ::Package:: *)

(* ================================================================== *)
(* topology_to_latex_external.m *)
(* ✅✅✅ FIXED FINAL VERSION 3.0 - 2024-12-04 ✅✅✅ *)
(* External curves are shown in RED above each curve digit *)
(* Port order: left\[RightArrow]right, bottom\[RightArrow]top at each position *)
(* ✨ ENHANCED: Multiple instanton support with configurable positions *)
(* 🔧 BUGFIX: Externals are OUTERMOST! Order: base → top → extBottom → extTop *)
(* ================================================================== *)

Print[""];
Print["==============================================="];
Print["  ✅✅✅ VERSION 3.0 FINAL LOADED ✅✅✅"];
Print["  Externals OUTERMOST!"];
Print["  Order: base → top → extBottom → extTop"];
Print["  Date: 2024-12-04"];
Print["==============================================="];
Print[""];


(* ================================================================== *)
(* ===== INSTANTON POSITION CONFIGURATION ===== *)
(* ✨ EDIT HERE to change instanton placement rules *)
(* Positions: "left", "right", "top", "bottom" *)
(* ================================================================== *)

(* Single node (no interior links): instanton positions by count *)
(* Format: count -> {1st position, 2nd position, 3rd position, 4th position} *)
instantonPositionsSingleNode = <|
    1 -> {"left"},
    2 -> {"left", "top"},
    3 -> {"left", "top", "bottom"},
    4 -> {"left", "top", "bottom", "right"}
|>;

(* Left-end node (interior link on right): instanton positions by count *)
instantonPositionsLeftEnd = <|
    1 -> {"left"},
    2 -> {"left", "top"},
    3 -> {"left", "top", "bottom"},
    4 -> {"left", "top", "bottom", "right"}
|>;

(* Right-end node (interior link on left): instanton positions by count *)
instantonPositionsRightEnd = <|
    1 -> {"right"},
    2 -> {"right", "top"},
    3 -> {"right", "top", "bottom"},
    4 -> {"right", "top", "bottom", "left"}
|>;

(* Middle node (interior links on both sides): instanton positions by count *)
instantonPositionsMiddle = <|
    1 -> {"top"},
    2 -> {"top", "bottom"},
    3 -> {"top", "bottom", "left"},
    4 -> {"top", "bottom", "left", "right"}
|>;

(* ===== Gauge Algebra Mapping ===== *)

gaugeAlgebra[param_Integer] := Switch[param,
    4, "\\mathfrak{so}",
    6, "\\mathfrak{e}_6",
    7, "\\mathfrak{e}_7'",
    8, "\\mathfrak{e}_7",
    12, "\\mathfrak{e}_8",
    _, "\\mathfrak{g}_{" <> ToString[param] <> "}"
];

(* ===== Parse Line-Compact Format with External ===== *)

parseLine[line_String] := Module[{parts, kinds, bparams, sConn, iConn, eConn, sp, ip, ep},
    parts = StringSplit[line, "|"];
    parts = StringTrim /@ parts;
    
    If[Length[parts] < 6, Return[$Failed]];
    
    (* Parse kinds and bparams *)
    kinds = ToExpression /@ StringSplit[parts[[1]], ","];
    bparams = ToExpression /@ StringSplit[parts[[2]], ","];
    
    If[Length[kinds] != Length[bparams], Return[$Failed]];
    
    (* Parse S connections *)
    sConn = {};
    If[StringContainsQ[parts[[3]], "("],
        sConn = StringCases[parts[[3]], 
            "(" ~~ a:NumberString ~~ "," ~~ b:NumberString ~~ ")" :> 
            {ToExpression[a], ToExpression[b]}];
    ];
    
    (* Parse I connections *)
    iConn = {};
    If[StringContainsQ[parts[[4]], "("],
        iConn = StringCases[parts[[4]], 
            "(" ~~ a:NumberString ~~ "," ~~ b:NumberString ~~ ")" :> 
            {ToExpression[a], ToExpression[b]}];
    ];
    
    (* Parse sp *)
    sp = {};
    If[StringContainsQ[parts[[5]], "="],
        With[{spStr = StringTrim[StringReplace[parts[[5]], "sp=" -> ""]]},
            If[StringLength[spStr] > 0,
                sp = ToExpression /@ StringSplit[spStr, ","];
            ];
        ];
    ];
    
    (* Parse ip *)
    ip = {};
    If[StringContainsQ[parts[[6]], "="],
        With[{ipStr = StringTrim[StringReplace[parts[[6]], "ip=" -> ""]]},
            If[StringLength[ipStr] > 0,
                ip = ToExpression /@ StringSplit[ipStr, ","];
            ];
        ];
    ];
    
    (* Parse E connections: E=(parent_id, parent_type, port_idx, external_id) *)
    eConn = {};
    Do[
        If[StringContainsQ[parts[[i]], "E="],
            eConn = StringCases[parts[[i]], 
                "(" ~~ a:NumberString ~~ "," ~~ b:NumberString ~~ "," ~~ 
                    c:NumberString ~~ "," ~~ d:NumberString ~~ ")" :> 
                {ToExpression[a], ToExpression[b], ToExpression[c], ToExpression[d]}];
        ],
        {i, 7, Length[parts]}
    ];
    
    (* Parse ep *)
    ep = {};
    Do[
        If[StringContainsQ[parts[[i]], "ep="],
            With[{epStr = StringTrim[StringReplace[parts[[i]], "ep=" -> ""]]},
                If[StringLength[epStr] > 0,
                    ep = ToExpression /@ StringSplit[epStr, ","];
                ];
            ];
        ],
        {i, 7, Length[parts]}
    ];
    
    {kinds, bparams, sConn, iConn, sp, ip, eConn, ep}
];

(* ================================================================== *)
(* ===== CURVE SEQUENCE DEFINITIONS ===== *)
(* Format: { {base}, {base, top}, {base}, ... } *)
(* Each position: {bottom_curve} or {bottom_curve, top_curve} *)
(* Port order: left\[RightArrow]right, bottom\[RightArrow]top at each position *)
(* ================================================================== *)

(* ===== SideLink/Instanton Curve Sequences ===== *)
(* \[FivePointedStar] \:d558\:b4dc\:cf54\:b529: \:c67c\:cabd\[RightArrow]\:c624\:b978\:cabd, \:ac01 \:c704\:ce58\:c5d0\:c11c \:c544\:b798\[RightArrow]\:c704 \[FivePointedStar] *)

getSideLinkCurves[param_Integer] := Switch[param,
    (* === Instantons === *)
    1,    {{"1"}},
    882,  {{"2"}, {"1"}},
    883,  {{"2"}, {"2"}, {"1"}},
    884,  {{"2"}, {"2"}, {"2"}, {"1"}},
    885,  {{"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    886,  {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    887,  {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    8881, {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    889,  {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    8810, {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    8811, {{"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"2"}, {"1"}},
    
    (* === alkali 1-links with no -5 curve === *)
    91,  {{"3"}, {"2", "2"}, {"1"}},                    (* 3\overset{2}{2}1 *)
    92,  {{"2"}, {"3", "2"}, {"1"}},                    (* 2\overset{2}{3}1 *)
    93,  {{"3"}, {"2"}, {"2"}, {"1"}},                  (* 3221 *)
    94,  {{"2"}, {"3"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    95,  {{"2"}, {"2"}, {"3"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    96,  {{"3"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    97,  {{"3"}, {"2"}, {"1"}},
    98,  {{"2"}, {"3"}, {"2"}, {"1"}},
    99,  {{"2"}, {"3"}, {"1"}, {"3"}, {"2"}, {"1"}},
    910, {{"2"}, {"2"}, {"3"}, {"1"}, {"3"}, {"2"}, {"1"}},
    911, {{"3"}, {"1"}, {"3"}, {"2"}, {"1"}},
    912, {{"3"}, {"1"}},
    913, {{"2"}, {"3"}, {"1"}, {"3"}, {"1"}},
    914, {{"2"}, {"2"}, {"3"}, {"1"}, {"3"}, {"1"}},
    915, {{"3"}, {"1"}, {"3"}, {"1"}},
    916, {{"2"}, {"3"}, {"1"}},
    917, {{"2"}, {"2"}, {"3"}, {"1"}},
    
    (* === alkali 2-links with no -5 curve === *)
    991,  {{"2"}, {"3", "1"}, {"1"}},                   (* 2\overset{1}{3}1 *)
    9920, {{"1"}, {"2"}, {"3", "2"}, {"1"}},            (* 12\overset{2}{3}1 *)
    9902, {{"1"}, {"3", "2"}, {"2"}, {"1"}},            (* 1\overset{2}{3}21 *)
    993,  {{"2"}, {"3", "1"}, {"2"}, {"1"}},            (* 2\overset{1}{3}21 *)
    
    (* === alkali 3-links with one -5 curve === *)
    99910, {{"1"}, {"5", "1"}, {"1"}, {"3"}, {"1"}},    (* 1\overset{1}{5}131 *)
    99920, {{"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"1"}},
    99930, {{"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    994,   {{"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"1"}},
    995,   {{"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"1"}},
    996,   {{"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    997,   {{"2"}, {"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"1"}},
    998,   {{"2"}, {"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    999,   {{"2"}, {"2"}, {"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    9910,  {{"2"}, {"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"1"}},
    9911,  {{"2"}, {"2"}, {"3"}, {"1"}, {"5", "1"}, {"1"}, {"3"}, {"2"}, {"1"}},
    9912,  {{"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    9913,  {{"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    9914,  {{"1"}, {"5"}, {"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    
    (* === alkali 1-links with one -5 curve === *)
    918, {{"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    919, {{"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    920, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    921, {{"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    922, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    923, {{"2"}, {"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    924, {{"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    925, {{"5"}, {"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    926, {{"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    927, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    928, {{"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    929, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    930, {{"2"}, {"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    931, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    932, {{"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    933, {{"3"}, {"1"}, {"5"}, {"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    934, {{"5"}, {"1"}, {"3"}, {"1"}},
    935, {{"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    936, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    937, {{"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    938, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    939, {{"2"}, {"3"}, {"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    940, {{"5"}, {"1"}, {"2"}, {"3"}, {"1"}},
    941, {{"1"}, {"5"}, {"1"}, {"2"}, {"3"}, {"1"}},
    942, {{"5"}, {"1"}, {"2"}, {"2"}, {"3"}, {"1"}},
    943, {{"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    944, {{"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    945, {{"2"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    
    (* === alkali 2-links with two -5 curves === *)
    9915, {{"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    9916, {{"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    9917, {{"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    
    (* === alkali 1-links with two -5 curves === *)
    946, {{"5"}, {"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    947, {{"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    948, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    949, {{"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    950, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    951, {{"5"}, {"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    952, {{"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    953, {{"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    954, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    955, {{"5"}, {"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    956, {{"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    957, {{"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    
    11,  {{"1"}},
    22,  {{"1"}, {"3"}, {"1"}},
    23,  {{"1"}, {"3"}, {"2"}, {"1"}},
    32,  {{"1"}, {"2"}, {"3"}, {"1"}},
    33,  {{"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    24,  {{"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    42,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}},
    34,  {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    43,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    44,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    35,  {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    53,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    45,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    54,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    55,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    
    (* === Special 331 connection === *)
    331, {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    (* Default *)
    _, {{ToString[param]}}
];

(* ===== Interior (Link/Connection) Curve Sequences ===== *)
(* \[FivePointedStar] \:d558\:b4dc\:cf54\:b529: \:b178\:b4dc \:c0ac\:c774 \:c5f0\:acb0 \:cee4\:be0c \[FivePointedStar] *)

getInteriorCurves[param_Integer] := Switch[param,
    (* === Basic connections === *)
    11,  {{"1"}},
    22,  {{"1"}, {"3"}, {"1"}},
    23,  {{"1"}, {"3"}, {"2"}, {"1"}},
    32,  {{"1"}, {"2"}, {"3"}, {"1"}},
    33,  {{"1"}, {"2"}, {"3"}, {"2"}, {"1"}},
    24,  {{"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    42,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}},
    34,  {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    43,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    44,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    35,  {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    53,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    45,  {{"1"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    54,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"1"}},
    55,  {{"1"}, {"2"}, {"2"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"2"}, {"2"}, {"1"}},
    
    (* === Special 331 connection === *)
    331, {{"1"}, {"3"}, {"1"}, {"5"}, {"1"}, {"3"}, {"1"}},
    
    (* Default *)
    _, {{ToString[param]}}
];

(* ================================================================== *)
(* ===== RENDERING FUNCTIONS ===== *)
(* ================================================================== *)

(* Render a single position with optional top curve and external *)
(* pos: {base} or {base, top} *)
(* extBottom, extTop: external values or None *)
(* ✅ CORRECT ORDER: base → top → extBottom → extTop (externals OUTERMOST!) *)
renderPosition[pos_List, extBottom_, extTop_] := Module[{base, top, result},
    base = pos[[1]];
    top = If[Length[pos] >= 2, pos[[2]], None];
    
    (* Build: base → top → extBottom → extTop *)
    result = base;
    
    (* Step 1: Add top curve first if exists *)
    If[top =!= None,
        result = "\\overset{" <> top <> "}{" <> result <> "}"
    ];
    
    (* Step 2: Add extBottom (above top!) *)
    If[extBottom =!= None,
        result = "\\overset{\\textcolor{red}{" <> ToString[extBottom] <> "}}{" <> result <> "}"
    ];
    
    (* Step 3: Add extTop (topmost!) *)
    If[extTop =!= None,
        result = "\\overset{\\textcolor{red}{" <> ToString[extTop] <> "}}{" <> result <> "}"
    ];
    
    result
];

(* Apply externals to curve sequence *)
(* curves: {{base}, {base, top}, ...} *)
(* externals: Association[port_idx -> external_value] *)
(* direction: "left" or "right" - right\:c774\:ba74 position \:c21c\:c11c \:b4a4\:c9d1\:c74c *)
applyCurvesWithExternals[curves_List, externals_Association, direction_String:"left"] := Module[
    {result, portIdx, i, pos, extBottom, extTop, 
     portRanges, renderedPositions, renderOrder},
    
    (* 1. \:ba3c\:c800 \:ac01 position\:c758 \:d3ec\:d2b8 \:bc94\:c704 \:acc4\:c0b0 *)
    portRanges = {};
    portIdx = 0;
    For[i = 1, i <= Length[curves], i++,
        pos = curves[[i]];
        If[Length[pos] >= 2,
            AppendTo[portRanges, {portIdx, portIdx + 1}];  (* {bottom, top} *)
            portIdx += 2;
        ,
            AppendTo[portRanges, {portIdx}];  (* {bottom only} *)
            portIdx++;
        ];
    ];
    
    (* 2. \:ac01 position \:b80c\:b354\:b9c1 *)
    renderedPositions = {};
    For[i = 1, i <= Length[curves], i++,
        pos = curves[[i]];
        
        (* \:c774 position\:c758 \:d3ec\:d2b8 \:c778\:b371\:c2a4 *)
        extBottom = If[KeyExistsQ[externals, portRanges[[i, 1]]], 
                       externals[portRanges[[i, 1]]], None];
        extTop = None;
        If[Length[pos] >= 2 && Length[portRanges[[i]]] >= 2,
            extTop = If[KeyExistsQ[externals, portRanges[[i, 2]]], 
                        externals[portRanges[[i, 2]]], None];
        ];
        
        AppendTo[renderedPositions, renderPosition[pos, extBottom, extTop]];
    ];
    
    (* 3. \:bc29\:d5a5\:c5d0 \:b530\:b77c \:c21c\:c11c \:acb0\:c815 *)
    renderOrder = If[direction == "right", 
        Reverse[renderedPositions], 
        renderedPositions
    ];
    
    StringJoin[renderOrder]
];

(* Render interior connection with externals *)
(* External 없으면 축약 표현, 있으면 전개 (otimes 없이) *)
renderInterior[param_Integer, externals_Association] := Module[{curves, content, digits},
    
    (* External이 없으면 기존 축약 표현 사용 *)
    If[Length[externals] == 0,
        digits = IntegerDigits[param];
        If[Length[digits] == 3 && digits == {3, 3, 1},
            (* Special 331 case - 그냥 3,3으로 표시 *)
            Return[" \\overset{3,3}{\\bigcirc} "]
        ];
        If[Length[digits] >= 2,
            Return[" \\overset{" <> StringJoin[Riffle[ToString /@ digits, ","]] <> "}{\\otimes} "],
            Return[" \\overset{" <> ToString[param] <> "}{\\otimes} "]
        ]
    ];
    
    (* External이 있으면 전개 - otimes 없이 *)
    curves = getInteriorCurves[param];
    content = applyCurvesWithExternals[curves, externals, "left"];
    
    " " <> content <> " "
];

(* ================================================================== *)
(* ===== BUILD DECORATED NODE (✨ ENHANCED) ===== *)
(* Supports multiple instantons (up to 4) with configurable positions *)
(* ================================================================== *)

buildDecoratedNode[algebra_String, sidelinks_List, instantons_List, 
                   sideLinkExternals_Association, instantonExternals_Association,
                   nodeExts_List, position_String, sp_List, ip_List] := Module[
    {sortedS, result,
     leftParam, rightParam, topParam, bottomParam,
     leftExt, rightExt, topExt, bottomExt,
     leftCurves, rightCurves, topCurves, bottomCurves,
     nodeExtStr, instCount, instPositions, positionTable,
     instParams, instExts, pos, hasDecorations},
    
    (* Build node external string *)
    nodeExtStr = "";
    If[Length[nodeExts] > 0,
        nodeExtStr = StringJoin[
            Riffle[("\\textcolor{red}{" <> ToString[#] <> "}") & /@ nodeExts, ","]
        ];
    ];
    
    (* Check if there are any decorations *)
    hasDecorations = Length[sidelinks] > 0 || Length[instantons] > 0;
    
    (* If no decorations, return algebra (with nodeExt if exists) *)
    If[!hasDecorations,
        If[nodeExtStr != "",
            Return["\\underset{" <> nodeExtStr <> "}{" <> algebra <> "}"],
            Return[algebra]
        ]
    ];
    
    (* Sort sidelinks by param descending *)
    sortedS = Reverse[Sort[sidelinks]];
    
    (* Initialize all positions *)
    leftParam = 0; rightParam = 0; topParam = 0; bottomParam = 0;
    leftExt = <||>; rightExt = <||>; topExt = <||>; bottomExt = <||>;
    
    (* ✨ Select instanton position table based on node position *)
    positionTable = Which[
        position == "single", instantonPositionsSingleNode,
        position == "left", instantonPositionsLeftEnd,
        position == "right", instantonPositionsRightEnd,
        position == "middle", instantonPositionsMiddle,
        True, instantonPositionsSingleNode
    ];
    
    (* ✨ Get instanton positions for this count *)
    instCount = Length[instantons];
    instPositions = Lookup[positionTable, instCount, {}];
    
    (* ✨ Build instanton params and externals lists *)
    instParams = Table[ip[[instantons[[i]] + 1]], {i, instCount}];
    instExts = Table[Lookup[instantonExternals, instantons[[i]], <||>], {i, instCount}];
    
    (* ✨ Assign instantons to positions (negative param marks as instanton) *)
    Do[
        pos = instPositions[[i]];
        Which[
            pos == "left" && leftParam == 0,
                leftParam = -instParams[[i]]; leftExt = instExts[[i]],
            pos == "right" && rightParam == 0,
                rightParam = -instParams[[i]]; rightExt = instExts[[i]],
            pos == "top" && topParam == 0,
                topParam = -instParams[[i]]; topExt = instExts[[i]],
            pos == "bottom" && bottomParam == 0,
                bottomParam = -instParams[[i]]; bottomExt = instExts[[i]]
        ],
        {i, instCount}
    ];
    
    (* Assign sidelinks to remaining positions based on node position *)
    Which[
        position == "right",
            If[Length[sortedS] >= 1 && rightParam == 0, 
                rightParam = sortedS[[1]]; rightExt = Lookup[sideLinkExternals, sortedS[[1]], <||>]];
            If[Length[sortedS] >= 2 && topParam == 0,
                topParam = sortedS[[2]]; topExt = Lookup[sideLinkExternals, sortedS[[2]], <||>]];
            If[Length[sortedS] >= 2 && topParam != 0 && leftParam == 0,
                leftParam = sortedS[[2]]; leftExt = Lookup[sideLinkExternals, sortedS[[2]], <||>]];
            If[Length[sortedS] >= 3 && bottomParam == 0,
                bottomParam = sortedS[[3]]; bottomExt = Lookup[sideLinkExternals, sortedS[[3]], <||>]],
        True,  (* left, single, middle *)
            If[Length[sortedS] >= 1 && leftParam == 0,
                leftParam = sortedS[[1]]; leftExt = Lookup[sideLinkExternals, sortedS[[1]], <||>]];
            If[Length[sortedS] >= 2 && topParam == 0,
                topParam = sortedS[[2]]; topExt = Lookup[sideLinkExternals, sortedS[[2]], <||>]];
            If[Length[sortedS] >= 2 && topParam != 0 && rightParam == 0,
                rightParam = sortedS[[2]]; rightExt = Lookup[sideLinkExternals, sortedS[[2]], <||>]];
            If[Length[sortedS] >= 3 && bottomParam == 0,
                bottomParam = sortedS[[3]]; bottomExt = Lookup[sideLinkExternals, sortedS[[3]], <||>]]
    ];
    
    (* Helper to get curves - negative param means instanton *)
    getCurvesForParam[p_] := If[p < 0, getSideLinkCurves[-p], getSideLinkCurves[p]];
    
    (* Build result *)
    result = algebra;
    
    (* ✨ Add node external as underset - always at bottom under algebra *)
    (* This goes directly under the node, before any sidelinks *)
    If[nodeExtStr != "",
        result = "\\underset{" <> nodeExtStr <> "}{" <> result <> "}"
    ];
    
    (* Add top *)
    If[topParam != 0,
        topCurves = getCurvesForParam[topParam];
        result = "\\overset{" <> applyCurvesWithExternals[topCurves, topExt, "left"] <> "}{" <> result <> "}"
    ];
    
    (* Add bottom *)
    If[bottomParam != 0,
        bottomCurves = getCurvesForParam[bottomParam];
        result = "\\underset{" <> applyCurvesWithExternals[bottomCurves, bottomExt, "left"] <> "}{" <> result <> "}"
    ];
    
    (* Add left *)
    If[leftParam != 0,
        leftCurves = getCurvesForParam[leftParam];
        result = applyCurvesWithExternals[leftCurves, leftExt, "left"] <> " " <> result
    ];
    
    (* Add right - reversed direction *)
    If[rightParam != 0,
        rightCurves = getCurvesForParam[rightParam];
        result = result <> " " <> applyCurvesWithExternals[rightCurves, rightExt, "right"]
    ];
    
    result
];

(* ================================================================== *)
(* ===== CONVERT TOPOLOGY TO LATEX ===== *)
(* ================================================================== *)

topologyToLatex[line_String] := Module[
    {parsed, kinds, bparams, sConn, iConn, sp, ip, eConn, ep,
     nodes, links, nodeIndices, linkIndices,
     sidelinksPerNode, instantonsPerNode,
     sideLinkExternalsPerNode, instantonExternalsPerNode, interiorExternals, nodeExternals,
     latex, i, j, k, nodeAlgebra, decorated, linkExts},
    
    parsed = parseLine[line];
    If[parsed === $Failed, Return["% Parse error"]];
    
    {kinds, bparams, sConn, iConn, sp, ip, eConn, ep} = parsed;
    
    (* Separate nodes and links *)
    nodes = {};
    links = {};
    nodeIndices = {};
    linkIndices = {};
    
    For[i = 1, i <= Length[kinds], i++,
        Which[
            kinds[[i]] == 0, 
                (AppendTo[nodeIndices, i - 1 -> Length[nodes]];
                 AppendTo[nodes, bparams[[i]]]),
            kinds[[i]] == 1, 
                (AppendTo[linkIndices, i - 1 -> Length[links]];
                 AppendTo[links, bparams[[i]]])
        ]
    ];
    nodeIndices = Association[nodeIndices];
    linkIndices = Association[linkIndices];
    
    (* Build sidelinks per node: nodeIdx -> {param1, param2, ...} *)
    sidelinksPerNode = Table[{}, Length[nodes]];
    For[j = 1, j <= Length[sConn], j++,
        With[{blockIdx = sConn[[j, 1]], slIdx = sConn[[j, 2]]},
            If[KeyExistsQ[nodeIndices, blockIdx] && slIdx + 1 <= Length[sp],
                With[{nodeIdx = nodeIndices[blockIdx] + 1},
                    AppendTo[sidelinksPerNode[[nodeIdx]], sp[[slIdx + 1]]];
                ]
            ]
        ]
    ];
    
    (* Build instantons per node *)
    instantonsPerNode = Table[{}, Length[nodes]];
    For[j = 1, j <= Length[iConn], j++,
        With[{blockIdx = iConn[[j, 1]], instIdx = iConn[[j, 2]]},
            If[KeyExistsQ[nodeIndices, blockIdx] && instIdx + 1 <= Length[ip],
                With[{nodeIdx = nodeIndices[blockIdx] + 1},
                    AppendTo[instantonsPerNode[[nodeIdx]], instIdx];
                ]
            ]
        ]
    ];
    
    (* Build external connections *)
    (* sideLinkExternalsPerNode: nodeIdx -> <|param -> <|port -> extVal|>|> *)
    (* instantonExternalsPerNode: nodeIdx -> <|instIdx -> <|port -> extVal|>|> *)
    (* interiorExternals: linkIdx -> <|port -> extVal|> *)
    (* nodeExternals: nodeIdx -> {extVal1, extVal2, ...} - Block에 직접 붙는 External *)
    sideLinkExternalsPerNode = Table[<||>, Length[nodes]];
    instantonExternalsPerNode = Table[<||>, Length[nodes]];
    interiorExternals = Table[<||>, Length[links]];
    nodeExternals = Table[{}, Length[nodes]];
    
    For[j = 1, j <= Length[eConn], j++,
        With[{parentId = eConn[[j, 1]], parentType = eConn[[j, 2]], 
              portIdx = eConn[[j, 3]], extId = eConn[[j, 4]]},
            With[{extVal = If[extId + 1 <= Length[ep], ep[[extId + 1]], 0]},
                Which[
                    parentType == 0,  (* Block/Interior *)
                        If[KeyExistsQ[linkIndices, parentId],
                            (* It's an interior link *)
                            With[{linkIdx = linkIndices[parentId] + 1},
                                interiorExternals[[linkIdx]] = 
                                    Append[interiorExternals[[linkIdx]], portIdx -> extVal]
                            ],
                            (* It's a Block (node) - External 직접 붙음 *)
                            If[KeyExistsQ[nodeIndices, parentId],
                                With[{nodeIdx = nodeIndices[parentId] + 1},
                                    AppendTo[nodeExternals[[nodeIdx]], extVal]
                                ]
                            ]
                        ],
                    parentType == 1,  (* SideLink *)
                        Do[
                            If[sConn[[k, 2]] == parentId && parentId + 1 <= Length[sp],
                                With[{blockIdx = sConn[[k, 1]], slParam = sp[[parentId + 1]]},
                                    If[KeyExistsQ[nodeIndices, blockIdx],
                                        With[{nodeIdx = nodeIndices[blockIdx] + 1},
                                            With[{currentExt = Lookup[sideLinkExternalsPerNode[[nodeIdx]], slParam, <||>]},
                                                sideLinkExternalsPerNode[[nodeIdx]] = 
                                                    Append[
                                                        KeyDrop[sideLinkExternalsPerNode[[nodeIdx]], slParam],
                                                        slParam -> Append[currentExt, portIdx -> extVal]
                                                    ]
                                            ]
                                        ]
                                    ]
                                ]
                            ],
                            {k, Length[sConn]}
                        ],
                    parentType == 2,  (* Instanton *)
                        Do[
                            If[iConn[[k, 2]] == parentId && parentId + 1 <= Length[ip],
                                With[{blockIdx = iConn[[k, 1]]},
                                    If[KeyExistsQ[nodeIndices, blockIdx],
                                        With[{nodeIdx = nodeIndices[blockIdx] + 1},
                                            With[{currentExt = Lookup[instantonExternalsPerNode[[nodeIdx]], parentId, <||>]},
                                                instantonExternalsPerNode[[nodeIdx]] = 
                                                    Append[
                                                        KeyDrop[instantonExternalsPerNode[[nodeIdx]], parentId],
                                                        parentId -> Append[currentExt, portIdx -> extVal]
                                                    ]
                                            ]
                                        ]
                                    ]
                                ]
                            ],
                            {k, Length[iConn]}
                        ]
                ]
            ]
        ]
    ];
    
    (* Build LaTeX *)
    If[Length[nodes] == 0, Return["\\text{empty}"]];
    
    latex = "{";
    For[i = 1, i <= Length[nodes], i++,
        (* Add interior link before node (except first) *)
        If[i > 1 && i - 1 <= Length[links],
            linkExts = interiorExternals[[i - 1]];
            latex = latex <> renderInterior[links[[i - 1]], linkExts];
        ];
        
        nodeAlgebra = gaugeAlgebra[nodes[[i]]];
        
        With[{position = Which[
                i == 1, "left",
                i == Length[nodes], "right",
                True, "middle"
            ]},
            decorated = buildDecoratedNode[
                nodeAlgebra, 
                sidelinksPerNode[[i]], 
                instantonsPerNode[[i]],
                sideLinkExternalsPerNode[[i]],
                instantonExternalsPerNode[[i]],
                nodeExternals[[i]],
                position, sp, ip
            ];
        ];
        latex = latex <> decorated;
    ];
    latex = latex <> "}";
    
    latex
];

(* ================================================================== *)
(* ===== PROCESS FILE ===== *)
(* ================================================================== *)

extractNodeSignature[line_String] := Module[{parsed, kinds, bparams, nodes},
    parsed = parseLine[line];
    If[parsed === $Failed, Return["error"]];
    {kinds, bparams} = parsed[[1;;2]];
    nodes = {};
    Do[If[kinds[[i]] == 0, AppendTo[nodes, bparams[[i]]]], {i, Length[kinds]}];
    Sort[nodes]
];

processFile[inputFile_String, outputFile_String] := Module[
    {lines, latexLines, uniqueLatex, output, i, j, validLines, 
     signatures, grouped, sortedGroups},
    
    If[!FileExistsQ[inputFile],
        Print["Error: Input file not found: ", inputFile];
        Return[$Failed]
    ];
    
    lines = ReadList[inputFile, String];
    validLines = Select[lines, StringLength[#] > 0 &];
    
    Print["Processing ", Length[validLines], " topologies..."];
    
    latexLines = {};
    signatures = {};
    For[i = 1, i <= Length[validLines], i++,
        If[Mod[i, 100] == 0, Print["Processed ", i, " / ", Length[validLines]]];
        AppendTo[latexLines, topologyToLatex[validLines[[i]]]];
        AppendTo[signatures, extractNodeSignature[validLines[[i]]]];
    ];
    
    uniqueLatex = DeleteDuplicates[latexLines];
    Print["After removing exact duplicates: ", Length[uniqueLatex]];
    
    grouped = GatherBy[
        Transpose[{latexLines, signatures}], 
        Last
    ];
    
    sortedGroups = Reverse[SortBy[grouped, Length]];
    
    output = "% Generated LaTeX file with External curves (in RED)\n";
    output = output <> "% Use with amsmath and xcolor packages\n";
    output = output <> "% \\usepackage{xcolor}\n";
    output = output <> "% Total unique: " <> ToString[Length[uniqueLatex]] <> "\n";
    output = output <> "% Total groups: " <> ToString[Length[sortedGroups]] <> "\n\n";
    
    For[i = 1, i <= Length[sortedGroups], i++,
        With[{group = sortedGroups[[i]]},
            With[{sig = group[[1, 2]], items = DeleteDuplicates[group[[All, 1]]]},
                output = output <> "% === Group " <> ToString[i] <> ": nodes = " <> ToString[sig] <> " (" <> ToString[Length[items]] <> " items) ===\n";
                output = output <> "\\begin{align*}\n";
                For[j = 1, j <= Length[items], j++,
                    Which[
                        Mod[j, 3] == 1,
                            (If[j > 1, output = output <> " \\\\\n"];
                             output = output <> "&" <> items[[j]]),
                        True,
                            output = output <> ",  &&" <> items[[j]]
                    ]
                ];
                output = output <> "\n\\end{align*}\n\n";
            ]
        ]
    ];
    
    WriteString[outputFile, output];
    Close[outputFile];
    
    Print["Done! Output saved to: ", outputFile];
    Print["Total groups: ", Length[sortedGroups]];
];

(* ===== Usage ===== *)

Print["========================================"];
Print["  Topology to LaTeX Converter"];
Print["  (WITH EXTERNAL CURVES - RED)"];
Print["========================================"];
Print[];
Print["USAGE:"];
Print["  processFile[\"input.txt\", \"output.tex\"]"];
Print[];
Print["CURVE SEQUENCE FORMAT:"];
Print["  {{\"base\"}, {\"base\", \"top\"}, ...}"];
Print["  Port order: left->right, bottom->top"];
Print[];
Print["LaTeX PREAMBLE REQUIRED:"];
Print["  \\usepackage{amsmath}"];
Print["  \\usepackage{amssymb}"];
Print["  \\usepackage{xcolor}"];
Print["  \\allowdisplaybreaks"];
Print[];
Print["========================================"];
