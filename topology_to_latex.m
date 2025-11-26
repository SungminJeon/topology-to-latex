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
        " \\overset{(" <> StringJoin[Riffle[ToString /@ digits, ","]] <> ")}{\\bigcirc} ",
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
    {sortedS, left, top, right, result, instStr},
    
    (* If no decorations, return plain algebra *)
    If[Length[sidelinks] == 0 && Length[instantons] == 0,
        Return[algebra]
    ];
    
    (* Sort sidelinks by param descending *)
    sortedS = Reverse[Sort[sidelinks]];
    
    (* Assign positions based on node position in chain *)
    (* position: "left" = 왼쪽 끝, "right" = 오른쪽 끝, "middle" = 중간 *)
    left = ""; top = ""; right = "";
    
    Which[
        position == "right",
            (* 오른쪽 끝 노드: 오른쪽 -> 위 -> 왼쪽 순서로 붙음 *)
            If[Length[sortedS] >= 1, right = ToString[sortedS[[1]]]];
            If[Length[sortedS] >= 2, top = ToString[sortedS[[2]]]];
            If[Length[sortedS] >= 3, left = ToString[sortedS[[3]]]],
        True,
            (* 왼쪽 끝 또는 중간 노드: 왼쪽 -> 위 -> 오른쪽 순서로 붙음 *)
            If[Length[sortedS] >= 1, left = ToString[sortedS[[1]]]];
            If[Length[sortedS] >= 2, top = ToString[sortedS[[2]]]];
            If[Length[sortedS] >= 3, right = ToString[sortedS[[3]]]]
    ];
    
    (* Build result with overset/underset *)
    result = algebra;
    
    (* Add top sidelink if exists *)
    If[top != "",
        result = "\\overset{" <> top <> "}{" <> result <> "}"
    ];
    
    (* Add left/right sidelinks - 평범하게 붙이기 *)
    (* ★ 여기를 수정하면 표기법 변경 가능 ★ *)
    result = If[left != "", left <> " ", ""] <> result <> 
             If[right != "", " " <> right, ""];
    
    (* Add instantons below *)
    If[Length[instantons] > 0,
        instStr = StringJoin[Riffle[("I^{\\oplus " <> ToString[#] <> "}") & /@ instantons, ","]];
        result = "\\underset{" <> instStr <> "}{" <> result <> "}"
    ];
    
    result
];
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

processFile[inputFile_String, outputFile_String] := Module[{lines, latexLines, output, i, validLines},
    (* Read input file *)
    If[!FileExistsQ[inputFile],
        Print["Error: Input file not found: ", inputFile];
        Return[$Failed]
    ];
    
    lines = ReadList[inputFile, String];
    
    (* Filter out empty lines *)
    validLines = Select[lines, StringLength[#] > 0 &];
    
    Print["Processing ", Length[validLines], " topologies..."];
    
    (* Convert each line *)
    latexLines = {};
    For[i = 1, i <= Length[validLines], i++,
        If[Mod[i, 100] == 0, Print["Processed ", i, " / ", Length[validLines]]];
        AppendTo[latexLines, topologyToLatex[validLines[[i]]]];
    ];
    
    (* Build output *)
    output = "% Generated LaTeX file from topology database\n";
    output = output <> "% Following Bases_YH.nb notation\n";
    output = output <> "% Use with amsmath package\n\n";
    output = output <> "\\begin{align*}\n";
    
    For[i = 1, i <= Length[latexLines], i++,
        If[i > 1, output = output <> ", \\\\\n"];
        output = output <> "&" <> latexLines[[i]];
    ];
    
    output = output <> "\n\\end{align*}\n";
    
    (* Write output *)
    WriteString[outputFile, output];
    Close[outputFile];
    
    Print["Done! Output saved to: ", outputFile];
    Print["Total topologies converted: ", Length[latexLines]];
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
