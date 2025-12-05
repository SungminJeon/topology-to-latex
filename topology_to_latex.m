(* ::Package:: *)

(* topology_to_latex_YH.m *)
(* Converts topology line-compact format to LaTeX following Bases_YH notation *)

(* ===== Gauge Algebra Mapping (from Bases_YH.nb) ===== *)

gaugeAlgebra[param_Integer] := Switch[param,
    4, "\\mathfrak{so}",
    6, "\\mathfrak{e}_6",
    7, "\\mathfrak{e}_7'",  (* User's mapping: 7 -> e_7' *)
    8, "\\mathfrak{e}_7",   (* Bases_YH: 8 -> e_7 *)
    12, "\\mathfrak{e}_8",
    _, "\\mathfrak{g}_{" <> ToString[param] <> "}"
];

(* ===== Connection Type Mapping ===== *)
(* From Bases_YH.nb: links like {1,3,1} are converted based on adjacent nodes *)
(* But in our input, links are already encoded as integers like 11, 22, 33, 55 *)

connectionToLatex[param_Integer] := Module[{digits, is331},
    digits = IntegerDigits[param];
    
    (* Check for special 331 case *)
    is331 = (Length[digits] == 3 && digits == {3, 3, 1});
    
    If[is331,
        (* Special 331 case: use \bigcirc *)
        " \\overset{3,3}{\\bigcirc} ",
        (* Normal case: use \otimes *)
        If[Length[digits] >= 2,
            " \\overset{" <> StringJoin[Riffle[ToString /@ digits, ","]] <> "}{\\otimes} ",
            " \\overset{" <> ToString[param] <> "}{\\otimes} "
        ]
    ]
];

(* ===== Parse Line-Compact Format ===== *)

parseLine[line_String] := Module[{parts, kinds, bparams, sConn, iConn, sp, ip},
    (* Split by | *)
    parts = StringSplit[line, "|"];
    parts = StringTrim /@ parts;
    
    If[Length[parts] < 6, Return[$Failed]];
    
    (* Parse kinds and bparams *)
    kinds = ToExpression /@ StringSplit[parts[[1]], ","];
    bparams = ToExpression /@ StringSplit[parts[[2]], ","];
    
    If[Length[kinds] != Length[bparams], Return[$Failed]];
    
    (* Parse S connections: S=(block_id, sidelink_id);... *)
    sConn = {};
    If[StringContainsQ[parts[[3]], "("],
        sConn = StringCases[parts[[3]], 
            "(" ~~ a:NumberString ~~ "," ~~ b:NumberString ~~ ")" :> 
            {ToExpression[a], ToExpression[b]}];
    ];
    
    (* Parse I connections: I=(block_id, instanton_id);... *)
    iConn = {};
    If[StringContainsQ[parts[[4]], "("],
        iConn = StringCases[parts[[4]], 
            "(" ~~ a:NumberString ~~ "," ~~ b:NumberString ~~ ")" :> 
            {ToExpression[a], ToExpression[b]}];
    ];
    
    (* Parse sp (sidelink params) *)
    sp = {};
    If[StringContainsQ[parts[[5]], "="],
        With[{spStr = StringTrim[StringReplace[parts[[5]], "sp=" -> ""]]},
            If[StringLength[spStr] > 0,
                sp = ToExpression /@ StringSplit[spStr, ","];
            ];
        ];
    ];
    
    (* Parse ip (instanton params) *)
    ip = {};
    If[StringContainsQ[parts[[6]], "="],
        With[{ipStr = StringTrim[StringReplace[parts[[6]], "ip=" -> ""]]},
            If[StringLength[ipStr] > 0,
                ip = ToExpression /@ StringSplit[ipStr, ","];
            ];
        ];
    ];
    
    {kinds, bparams, sConn, iConn, sp, ip}
];

(* ===== Build Node with Decorations ===== *)

buildDecoratedNode[algebra_String, sidelinks_List, instantons_List, position_String] := Module[
    {sortedS, left, top, right, bottom, result, instStr},
    
    (* If no decorations, return plain algebra *)
    If[Length[sidelinks] == 0 && Length[instantons] == 0,
        Return[algebra]
    ];
    
    (* Sort sidelinks by param descending *)
    sortedS = Reverse[Sort[sidelinks]];
    
    (* Assign positions based on node position in chain *)
    (* position: "left" = \:c67c\:cabd \:b05d, "right" = \:c624\:b978\:cabd \:b05d, "middle" = \:c911\:ac04 *)
    left = ""; top = ""; right = ""; bottom = "";
    
    Which[
        position == "right",
            (* \:c624\:b978\:cabd \:b05d \:b178\:b4dc: \:c624\:b978\:cabd -> \:c704 -> \:c67c\:cabd \:c21c\:c11c\:b85c \:bd99\:c74c *)
            (* \:b2e8, Instanton\:c774 \:c788\:c73c\:ba74 top\:c740 \:be44\:c6cc\:b460 (instanton\:c774 \:c704\:c5d0 \:ac10) *)
            (If[Length[sortedS] >= 1, right = ToString[sortedS[[1]]]];
             If[Length[sortedS] >= 2 && Length[instantons] == 0, top = ToString[sortedS[[2]]]];
             If[Length[sortedS] >= 2 && Length[instantons] > 0, left = ToString[sortedS[[2]]]];
             If[Length[sortedS] >= 3 && Length[instantons] == 0, left = ToString[sortedS[[3]]]];
             If[Length[sortedS] >= 3 && Length[instantons] > 0, bottom = ToString[sortedS[[3]]]]),
        True,
            (* \:c67c\:cabd \:b05d \:b610\:b294 \:c911\:ac04 \:b178\:b4dc: \:c67c\:cabd -> \:c704 -> \:c624\:b978\:cabd \:c21c\:c11c\:b85c \:bd99\:c74c *)
            (* \:b2e8, Instanton\:c774 \:c788\:c73c\:ba74 top\:c740 \:be44\:c6cc\:b460 (instanton\:c774 \:c704\:c5d0 \:ac10) *)
            (If[Length[sortedS] >= 1, left = ToString[sortedS[[1]]]];
             If[Length[sortedS] >= 2 && Length[instantons] == 0, top = ToString[sortedS[[2]]]];
             If[Length[sortedS] >= 2 && Length[instantons] > 0, right = ToString[sortedS[[2]]]];
             If[Length[sortedS] >= 3 && Length[instantons] == 0, right = ToString[sortedS[[3]]]];
             If[Length[sortedS] >= 3 && Length[instantons] > 0, bottom = ToString[sortedS[[3]]]])
    ];
    
    (* Build result with overset/underset *)
    result = algebra;
    
    (* \:c704\:cabd\:c5d0 \:bd99\:b294 SideLink \:d45c\:ae30 *)
    (* \[FivePointedStar] \:ac01 parameter\:bcc4 \:d45c\:ae30\:bc95 \:d558\:b4dc\:cf54\:b529 \[FivePointedStar] *)
    formatTop[p_] := Switch[ToExpression[p],
        1,   "1 ",                          (*instantons*)    
        882,  "I^{\\oplus 2}",                         
        883, "I^{\\oplus 3} ",                
        884, "I^{\\oplus 4} ",                
        885, "I^{\\oplus 5} ",                     
        886, "I^{\\oplus 6} ",
        887, "I^{\\oplus 7} ",
        8881, "I^{\\oplus 8} ",
        889, "I^{\\oplus 9} ",
        8810, "I^{\\oplus 10} ",
        8811, "I^{\\oplus 11} ",
        991, "2\\overset{1}{3}1",	        (* alkali 2-links with no -5 curve*)
        9920, "12\\overset{2}{3}1",
        9902, "1\\overset{2}{3}21",
        993, "2\\overset{1}{3}21",
        91, "3\\overset{2}{2}1",              (* alkali 1-links with no -5 curve*)
        92, "2\\overset{2}{3}1",
        93, "3221",
        94, "2313221",
        95, "22313221",
        96, "313221",
        97, "321",
        98, "2321",
        99, "231321",
        910, "2231321",
        911, "31321",
        912, "31",
        913, "23131",
        914, "223131",
        915, "3131",
        916, "231",
        917, "2231",
        99910, "1\\overset{1}{5}131",           (*alkali 3-links with one -5 curve*)
        99920, "1\\overset{1}{5}1321",
        99930, "1\\overset{1}{5}13221",
        994, "31\\overset{1}{5}131",
        995, "31\\overset{1}{5}1321",
        996, "31\\overset{1}{5}13221",
        997, "231\\overset{1}{5}1321",
        998, "231\\overset{1}{5}13221",
        999, "2231\\overset{1}{5}13221",
        9910, "231\\overset{1}{5}131",
        9911, "2231\\overset{1}{5}1321",
        9912, "1513221",
        9913, "151321",
        9914, "1512321",                         (*alkali 1-links with one -5 curve*)        
        918, "513221",
        919, "321513221",
        920, "231513221",
        921, "2231513221",
        922, "31513221",
        923, "2321513221",
        924, "51321",
        925, "512321",
        926, "32151321",
        927, "23151321",
        928, "223151321",
        929, "3151321",
        930, "232151321",
        931, "231512321",
        932, "2231512321",
        933, "31512321",
        934, "5131",
        935, "3215131",
        936, "2315131",
        937, "22315131",
        938, "315131",
        939, "23215131",
        940, "51231",
        941, "151231",
        942, "512231",
        943, "215131",
        944, "2151321",
        945, "21513221",
        9915, "15131513221",              (*alkali 2-links with two -5 curves*)
        9916, "1513151321",
        9917, "151315131",
        946, "51231513221",               (*alkali 1-links with two -5 curves*)
        947, "5131513221",
        948, "2315131513221",
        949, "22315131513221",
        950, "315131513221",
        951, "5123151321",
        952, "513151321",
        953, "231513151321",
        954, "31513151321",
        955, "512315131",
        956, "51315131",
        957, "3151315131", 
        22, "\\overset{2,2}{\\otimes}",
        32, "\\overset{3,2}{\\otimes}",
        23, "\\overset{2,3}{\\otimes}",
        33, "\\overset{3,3}{\\otimes}",
        42, "\\overset{4,2}{\\otimes}",
        24, "\\overset{2,4}{\\otimes}",
        331, "\\overset{3,3}{\\bigcirc}", 
        43, "\\overset{4,3}{\\otimes}",
        34, "\\overset{3,4}{\\otimes}",
        44, "\\overset{4,4}{\\otimes}",
        53, "\\overset{5,3}{\\otimes}",
        35, "\\overset{3,5}{\\otimes}",
        54, "\\overset{5,4}{\\otimes}",
        45, "\\overset{4,5}{\\otimes}",
        55, "\\overset{5,5}{\\otimes}",            
        _,   p                              (* \:ae30\:bcf8\:ac12: \:adf8\:b0e5 \:c22b\:c790 *)
    ];
    
    (* Add top sidelink if exists *)
    If[top != "",
        result = "\\overset{" <> formatTop[top] <> "}{" <> result <> "}"
    ];
    
    (* Add left/right sidelinks *)
    (* \[FivePointedStar] \:ac01 parameter\:bcc4 \:d45c\:ae30\:bc95 \:d558\:b4dc\:cf54\:b529 \[FivePointedStar] *)
    
    (* \:c67c\:cabd\:c5d0 \:bd99\:b294 SideLink \:d45c\:ae30 *)
    formatLeft[p_] := Switch[ToExpression[p],
        1,   "1 ",                          (*instantons*)    
        882,  "I^{\\oplus 2}",                         
        883, "I^{\\oplus 3} ",                
        884, "I^{\\oplus 4} ",                
        885, "I^{\\oplus 5} ",                     
        886, "I^{\\oplus 6} ",
        887, "I^{\\oplus 7} ",
        8881, "I^{\\oplus 8} ",
        889, "I^{\\oplus 9} ",
        8810, "I^{\\oplus 10} ",
        8811, "I^{\\oplus 11} ",
        991, "2\\overset{1}{3}1",	        (* alkali 2-links with no -5 curve*)
        9920, "12\\overset{2}{3}1",
        9902, "1\\overset{2}{3}21",
        993, "2\\overset{1}{3}21",
        91, "3\\overset{2}{2}1",              (* alkali 1-links with no -5 curve*)
        92, "2\\overset{2}{3}1",
        93, "3221",
        94, "2313221",
        95, "22313221",
        96, "313221",
        97, "321",
        98, "2321",
        99, "231321",
        910, "2231321",
        911, "31321",
        912, "31",
        913, "23131",
        914, "223131",
        915, "3131",
        916, "231",
        917, "2231",
        99910, "1\\overset{1}{5}131",           (*alkali 3-links with one -5 curve*)
        99920, "1\\overset{1}{5}1321",
        99930, "1\\overset{1}{5}13221",
        994, "31\\overset{1}{5}131",
        995, "31\\overset{1}{5}1321",
        996, "31\\overset{1}{5}13221",
        997, "231\\overset{1}{5}1321",
        998, "231\\overset{1}{5}13221",
        999, "2231\\overset{1}{5}13221",
        9910, "231\\overset{1}{5}131",
        9911, "2231\\overset{1}{5}1321",
        9912, "1513221",
        9913, "151321",
        9914, "1512321",                         (*alkali 1-links with one -5 curve*)        
        918, "513221",
        919, "321513221",
        920, "231513221",
        921, "2231513221",
        922, "31513221",
        923, "2321513221",
        924, "51321",
        925, "512321",
        926, "32151321",
        927, "23151321",
        928, "223151321",
        929, "3151321",
        930, "232151321",
        931, "231512321",
        932, "2231512321",
        933, "31512321",
        934, "5131",
        935, "3215131",
        936, "2315131",
        937, "22315131",
        938, "315131",
        939, "23215131",
        940, "51231",
        941, "151231",
        942, "512231",
        943, "215131",
        944, "2151321",
        945, "21513221",
        9915, "15131513221",              (*alkali 2-links with two -5 curves*)
        9916, "1513151321",
        9917, "151315131",
        946, "51231513221",               (*alkali 1-links with two -5 curves*)
        947, "5131513221",
        948, "2315131513221",
        949, "22315131513221",
        950, "315131513221",
        951, "5123151321",
        952, "513151321",
        953, "231513151321",
        954, "31513151321",
        955, "512315131",
        956, "51315131",
        957, "3151315131", 
        22, "\\overset{2,2}{\\otimes}",
        32, "\\overset{3,2}{\\otimes}",
        23, "\\overset{2,3}{\\otimes}", 
        33, "\\overset{3,3}{\\otimes}",
        42, "\\overset{4,2}{\\otimes}",
        24, "\\overset{2,4}{\\otimes}",
        331, "\\overset{3,3}{\\bigcirc}", 
        43, "\\overset{4,3}{\\otimes}",
        34, "\\overset{3,4}{\\otimes}",
        44, "\\overset{4,4}{\\otimes}",
        53, "\\overset{5,3}{\\otimes}",
        35, "\\overset{3,5}{\\otimes}",
        54, "\\overset{5,4}{\\otimes}",
        45, "\\overset{4,5}{\\otimes}",
        55, "\\overset{5,5}{\\otimes}",     
        _,   p <> " "                           (* \:ae30\:bcf8\:ac12: \:adf8\:b0e5 \:c22b\:c790 *)
    ];
    
    (* \:c624\:b978\:cabd\:c5d0 \:bd99\:b294 SideLink \:d45c\:ae30 *)
    formatRight[p_] := Switch[ToExpression[p],
        1,   "1 ",                          (*instantons*)    
        882,  "I^{\\oplus 2}",                         
        883, "I^{\\oplus 3} ",                
        884, "I^{\\oplus 4} ",                
        885, "I^{\\oplus 5} ",                     
        886, "I^{\\oplus 6} ",
        887, "I^{\\oplus 7} ",
        8881, "I^{\\oplus 8} ",
        889, "I^{\\oplus 9} ",
        8810, "I^{\\oplus 10} ",
        8811, "I^{\\oplus 11} ",
        991, "1\\overset{1}{3}2",	        (* alkali 2-links with no -5 curve*)
        9920, "1\\overset{2}{3}21",
        9902, "12\\overset{2}{3}1",
        993, "12\\overset{1}{3}2",
        91, "1\\overset{2}{2}3",              (* alkali 1-links with no -5 curve*)
        92, "1\\overset{2}{3}2",
        93, "1223",
        94, "1223132",
        95, "12231322",
        96, "122313",
        97, "123",
        98, "1232",
        99, "123132",
        910, "1231322",
        911, "12313",
        912, "13",
        913, "13132",
        914, "131322",
        915, "1313",
        916, "132",
        917, "1322",
        99910, "131\\overset{1}{5}1",           (*alkali 3-links with one -5 curve*)
        99920, "1231\\overset{1}{5}1",
        99930, "12231\\overset{1}{5}1",
        994, "131\\overset{1}{5}13",
        995, "1231\\overset{1}{5}13",
        996, "12231\\overset{1}{5}13",
        997, "1231\\overset{1}{5}132",
        998, "12231\\overset{1}{5}132",
        999, "12231\\overset{1}{5}1322",
        9910, "131\\overset{1}{5}132",
        9911, "1231\\overset{1}{5}1322",
        9912, "1223151",
        9913, "123151",
        9914, "1232151",                         (*alkali 1-links with one -5 curve*)        
        918, "122315",
        919, "122315123",
        920, "122315132",
        921, "1223151322",
        922, "12231513",
        923, "1223151232",
        924, "12315",
        925, "123215",
        926, "12315123",
        927, "12315132",
        928, "123151322",
        929, "1231513",
        930, "123151232",
        931, "123215132",
        932, "1232151322",
        933, "12321513",
        934, "1315",
        935, "1315123",
        936, "1315132",
        937, "13151322",
        938, "131513",
        939, "13151232",
        940, "13215",
        941, "132151",
        942, "132215",
        943, "131512",
        944, "1231512",
        945, "12231512",
        9915, "12231513151",              (*alkali 2-links with two -5 curves*)
        9916, "1231513151",
        9917, "131513151",
        946, "12231513215",               (*alkali 1-links with two -5 curves*)
        947, "1223151315",
        948, "1223151315132",
        949, "12231513151322",
        950, "122315131513",
        951, "1231513215",
        952, "123151315",
        953, "123151315132",
        954, "12315131513",
        955, "131513215",
        956, "13151315",
        957, "1315131513",  
        22, "\\overset{2,2}{\\otimes}",
        32, "\\overset{2,3}{\\otimes}",
        23, "\\overset{3,2}{\\otimes}",
        33, "\\overset{3,3}{\\otimes}",
        42, "\\overset{2,4}{\\otimes}",
        24, "\\overset{4,2}{\\otimes}",
        331, "\\overset{3,3}{\\bigcirc}", 
        43, "\\overset{3,4}{\\otimes}",
        34, "\\overset{4,3}{\\otimes}",
        44, "\\overset{4,4}{\\otimes}",
        53, "\\overset{3,5}{\\otimes}",
        35, "\\overset{5,3}{\\otimes}",
        54, "\\overset{4,5}{\\otimes}",
        45, "\\overset{5,4}{\\otimes}",
        55, "\\overset{5,5}{\\otimes}",           
        _,   " " <> p                           (* \:ae30\:bcf8\:ac12: \:adf8\:b0e5 \:c22b\:c790 *)
    ];
    
    (* \:c544\:b798\:cabd\:c5d0 \:bd99\:b294 SideLink \:d45c\:ae30 *)
    (* \[FivePointedStar] \:ac01 parameter\:bcc4 \:d45c\:ae30\:bc95 \:d558\:b4dc\:cf54\:b529 \[FivePointedStar] *)
    formatBottom[p_] := Switch[ToExpression[p],
        1,   "1 ",                          (*instantons*)    
        882,  "I^{\\oplus 2}",                         
        883, "I^{\\oplus 3} ",                
        884, "I^{\\oplus 4} ",                
        885, "I^{\\oplus 5} ",                     
        886, "I^{\\oplus 6} ",
        887, "I^{\\oplus 7} ",
        8881, "I^{\\oplus 8} ",
        889, "I^{\\oplus 9} ",
        8810, "I^{\\oplus 10} ",
        8811, "I^{\\oplus 11} ",
        991, "2\\overset{1}{3}1",	        (* alkali 2-links with no -5 curve*)
        9920, "12\\overset{2}{3}1",
        9902, "1\\overset{2}{3}21",
        993, "2\\overset{1}{3}21",
        91, "3\\overset{2}{2}1",              (* alkali 1-links with no -5 curve*)
        92, "2\\overset{2}{3}1",
        93, "3221",
        94, "2313221",
        95, "22313221",
        96, "313221",
        97, "321",
        98, "2321",
        99, "231321",
        910, "2231321",
        911, "31321",
        912, "31",
        913, "23131",
        914, "223131",
        915, "3131",
        916, "231",
        917, "2231",
        99910, "1\\overset{1}{5}131",           (*alkali 3-links with one -5 curve*)
        99920, "1\\overset{1}{5}1321",
        99930, "1\\overset{1}{5}13221",
        994, "31\\overset{1}{5}131",
        995, "31\\overset{1}{5}1321",
        996, "31\\overset{1}{5}13221",
        997, "231\\overset{1}{5}1321",
        998, "231\\overset{1}{5}13221",
        999, "2231\\overset{1}{5}13221",
        9910, "231\\overset{1}{5}131",
        9911, "2231\\overset{1}{5}1321",
        9912, "1513221",
        9913, "151321",
        9914, "1512321",                         (*alkali 1-links with one -5 curve*)        
        918, "513221",
        919, "321513221",
        920, "231513221",
        921, "2231513221",
        922, "31513221",
        923, "2321513221",
        924, "51321",
        925, "512321",
        926, "32151321",
        927, "23151321",
        928, "223151321",
        929, "3151321",
        930, "232151321",
        931, "231512321",
        932, "2231512321",
        933, "31512321",
        934, "5131",
        935, "3215131",
        936, "2315131",
        937, "22315131",
        938, "315131",
        939, "23215131",
        940, "51231",
        941, "151231",
        942, "512231",
        943, "215131",
        944, "2151321",
        945, "21513221",
        9915, "15131513221",              (*alkali 2-links with two -5 curves*)
        9916, "1513151321",
        9917, "151315131",
        946, "51231513221",               (*alkali 1-links with two -5 curves*)
        947, "5131513221",
        948, "2315131513221",
        949, "22315131513221",
        950, "315131513221",
        951, "5123151321",
        952, "513151321",
        953, "231513151321",
        954, "31513151321",
        955, "512315131",
        956, "51315131",
        957, "3151315131", 
        22, "\\overset{2,2}{\\otimes}",
        32, "\\overset{3,2}{\\otimes}",
        23, "\\overset{2,3}{\\otimes}", 
        33, "\\overset{3,3}{\\otimes}",
        42, "\\overset{4,2}{\\otimes}",
        24, "\\overset{2,4}{\\otimes}",
        331, "\\overset{3,3}{\\bigcirc}", 
        43, "\\overset{4,3}{\\otimes}",
        34, "\\overset{3,4}{\\otimes}",
        44, "\\overset{4,4}{\\otimes}",
        53, "\\overset{5,3}{\\otimes}",
        35, "\\overset{3,5}{\\otimes}",
        54, "\\overset{5,4}{\\otimes}",
        45, "\\overset{4,5}{\\otimes}",
        55, "\\overset{5,5}{\\otimes}",     
        _,   p                              (* \:ae30\:bcf8\:ac12: \:adf8\:b0e5 \:c22b\:c790 *)
    ];
    
    (* Add bottom sidelink - \:b178\:b4dc \:c544\:b798\:c5d0 \:bd99\:c784 *)
    If[bottom != "",
        result = "\\underset{" <> formatBottom[bottom] <> "}{" <> result <> "}"
    ];
    
    (* Add instantons - \:b178\:b4dc \:c704\:c5d0 \:ba3c\:c800 \:bd99\:c784 *)
    (* \[FivePointedStar] \:ac01 instanton parameter\:bcc4 \:d45c\:ae30\:bc95 \:d558\:b4dc\:cf54\:b529 \[FivePointedStar] *)
    formatInstanton[p_] := Switch[p,
        1,  "1",                              (* 1 \[RightArrow] I *)
        882,  "I^{\\oplus 2}",                  (* 2 \[RightArrow] I\[CirclePlus]2 *)
        883,  "I^{\\oplus 3}",                  (* 3 \[RightArrow] I\[CirclePlus]3 *)
        884,  "I^{\\oplus 4}",
        885,  "I^{\\oplus 5}",
        886,  "I^{\\oplus 6}",
        887,  "I^{\\oplus 7}",
        8881,  "I^{\\oplus 8}",
        889,  "I^{\\oplus 9}",
        8810, "I^{\\oplus 10}",
        8811, "I^{\\oplus 11}",
        8812, "I^{\\oplus 12}",
        _,  "I^{\\oplus " <> ToString[p] <> "}"  (* \:ae30\:bcf8\:ac12 *)
    ];
    
    If[Length[instantons] > 0,
        (instStr = StringJoin[Riffle[formatInstanton /@ instantons, ","]];
         result = "\\overset{" <> instStr <> "}{" <> result <> "}")
    ];
    
    (* Add left/right sidelinks - instanton \:bd99\:c778 \:d6c4\:c5d0 \:c591\:c606\:c5d0 \:cd94\:ac00 *)
    result = If[left != "", formatLeft[left], ""] <> result <> 
             If[right != "", formatRight[right], ""];
    
    result
];

(* ===== Convert Topology to LaTeX ===== *)

topologyToLatex[line_String] := Module[
    {parsed, kinds, bparams, sConn, iConn, sp, ip, 
     nodes, links, nodeIndices, sidelinksPerNode, instantonsPerNode,
     latex, i, j, nodeAlgebra, decorated},
    
    parsed = parseLine[line];
    If[parsed === $Failed, Return["% Parse error"]];
    
    {kinds, bparams, sConn, iConn, sp, ip} = parsed;
    
    (* Separate nodes (kind=0) and links (kind=1) *)
    nodes = {};
    links = {};
    nodeIndices = {};  (* Map from block index to node index *)
    
    For[i = 1, i <= Length[kinds], i++,
        Which[
            kinds[[i]] == 0, 
                AppendTo[nodeIndices, i - 1 -> Length[nodes]];
                AppendTo[nodes, bparams[[i]]],
            kinds[[i]] == 1, 
                AppendTo[links, bparams[[i]]]
        ]
    ];
    nodeIndices = Association[nodeIndices];
    
    (* Build sidelinks per node *)
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
                    AppendTo[instantonsPerNode[[nodeIdx]], ip[[instIdx + 1]]];
                ]
            ]
        ]
    ];
    
    (* Build LaTeX *)
    If[Length[nodes] == 0, Return["\\text{empty}"]];
    
    latex = "{";
    For[i = 1, i <= Length[nodes], i++,
        If[i > 1 && i - 1 <= Length[links],
            latex = latex <> connectionToLatex[links[[i - 1]]];
        ];
        nodeAlgebra = gaugeAlgebra[nodes[[i]]];
        
        (* Determine node position: left, middle, or right *)
        With[{position = Which[
                i == 1, "left",
                i == Length[nodes], "right",
                True, "middle"
            ]},
            decorated = buildDecoratedNode[nodeAlgebra, sidelinksPerNode[[i]], instantonsPerNode[[i]], position];
        ];
        latex = latex <> decorated;
    ];
    latex = latex <> "}";
    
    latex
];

(* ===== Process File ===== *)

(* \:b178\:b4dc \:ad6c\:c131 \:cd94\:cd9c (\:c815\:b82c\:b41c \:d615\:d0dc) *)
extractNodeSignature[line_String] := Module[{parsed, kinds, bparams, nodes},
    parsed = parseLine[line];
    If[parsed === $Failed, Return["error"]];
    {kinds, bparams} = parsed[[1;;2]];
    nodes = {};
    Do[If[kinds[[i]] == 0, AppendTo[nodes, bparams[[i]]]], {i, Length[kinds]}];
    Sort[nodes]  (* \:c815\:b82c\:d574\:c11c \:c21c\:c11c \:bb34\:ad00\:d558\:ac8c \:be44\:ad50 *)
];

processFile[inputFile_String, outputFile_String] := Module[
    {lines, latexLines, uniqueLatex, output, i, j, validLines, 
     signatures, grouped, sortedGroups},
    
    (* Read input file *)
    If[!FileExistsQ[inputFile],
        Print["Error: Input file not found: ", inputFile];
        Return[$Failed]
    ];
    
    lines = ReadList[inputFile, String];
    
    (* Filter out empty lines *)
    validLines = Select[lines, StringLength[#] > 0 &];
    
    Print["Processing ", Length[validLines], " topologies..."];
    
    (* Convert each line and get signature *)
    latexLines = {};
    signatures = {};
    For[i = 1, i <= Length[validLines], i++,
        If[Mod[i, 100] == 0, Print["Processed ", i, " / ", Length[validLines]]];
        AppendTo[latexLines, topologyToLatex[validLines[[i]]]];
        AppendTo[signatures, extractNodeSignature[validLines[[i]]]];
    ];
    
    (* Remove exact duplicates first *)
    uniqueLatex = DeleteDuplicates[latexLines];
    Print["After removing exact duplicates: ", Length[uniqueLatex]];
    
    (* Group by node signature for manual symmetry check *)
    grouped = GatherBy[
        Transpose[{latexLines, signatures}], 
        Last  (* signature\:b85c \:adf8\:b8f9\:d654 *)
    ];
    
    (* Sort groups by size (\:d070 \:adf8\:b8f9\:c774 \:ba3c\:c800) *)
    sortedGroups = Reverse[SortBy[grouped, Length]];
    
    (* Build output - grouped format *)
    output = "% Generated LaTeX file from topology database\n";
    output = output <> "% Following Bases_YH.nb notation\n";
    output = output <> "% Use with amsmath package\n";
    output = output <> "% Grouped by node composition for symmetry check\n";
    output = output <> "% Total unique (string): " <> ToString[Length[uniqueLatex]] <> "\n";
    output = output <> "% Total groups: " <> ToString[Length[sortedGroups]] <> "\n\n";
    
    For[i = 1, i <= Length[sortedGroups], i++,
        With[{group = sortedGroups[[i]]},
            With[{sig = group[[1, 2]], items = DeleteDuplicates[group[[All, 1]]]},
                output = output <> "% === Group " <> ToString[i] <> ": nodes = " <> ToString[sig] <> " (" <> ToString[Length[items]] <> " items) ===\n";
                output = output <> "\\begin{align*}\n";
                For[j = 1, j <= Length[items], j++,
                    (* \:ac01 \:d56d\:baa9 \:cd9c\:b825 *)
                    Which[
                        Mod[j, 3] == 1,
                            (* \:c904\:c758 \:ccab \:bc88\:c9f8: & *)
                            (If[j > 1, output = output <> " \\\\\n"];
                             output = output <> "&" <> items[[j]]),
                        True,
                            (* \:c904\:c758 \:b450 \:bc88\:c9f8, \:c138 \:bc88\:c9f8: && *)
                            output = output <> ",  &&" <> items[[j]]
                    ]
                ];
                output = output <> "\n\\end{align*}\n\n";
            ]
        ]
    ];
    
    (* Write output *)
    WriteString[outputFile, output];
    Close[outputFile];
    
    Print["Done! Output saved to: ", outputFile];
    Print["Total groups: ", Length[sortedGroups]];
    Print["Check groups with 2+ items for potential symmetry duplicates"];
    Print["File location: ", FileNameJoin[{Directory[], outputFile}]];
];

(* ===== Usage ===== *)

Print["========================================"];
Print["  Topology to LaTeX Converter"];
Print["  (Following Bases_YH.nb notation)"];
Print["========================================"];
Print[];
Print["GAUGE ALGEBRA MAPPING:"];
Print["  4  -> \\mathfrak{so}"];
Print["  6  -> \\mathfrak{e}_6"];
Print["  7  -> \\mathfrak{e}_7' (prime)"];
Print["  8  -> \\mathfrak{e}_7"];
Print["  12 -> \\mathfrak{e}_8"];
Print[];
Print["CONNECTION NOTATION:"];
Print["  Regular: \\overset{a,b}{\\otimes}"];
Print["  Special 331: \\overset{(3,3,1)}{\\bigcirc}"];
Print[];
Print["USAGE:"];
Print["  processFile[\"input.txt\", \"output.tex\"]"];
Print[];
Print["EXAMPLES:"];
Print["  processFile[\"gLgL.txt\", \"output.tex\"]"];
Print["  processFile[\"LST_xSg\", \"LST_xSg.tex\"]"];
Print[];
Print["========================================"];
