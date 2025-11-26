(* quick_start.m *)
(* 빠른 시작 가이드 *)

Print["========================================"];
Print["  Quick Start Guide"];
Print["========================================"];
Print[];

(* 1. 현재 디렉토리 확인 *)
Print["1. Current directory:"];
Print["   ", Directory[]];
Print[];

(* 2. 파일 로드 *)
Print["2. Loading topology_to_latex.m..."];
Get["topology_to_latex.m"];
Print["   Loaded!"];
Print[];

(* 3. 작업 디렉토리 설정 (필요시) *)
Print["3. To change directory (if needed):"];
Print["   SetDirectory[\"/path/to/your/files\"]"];
Print[];

(* 4. 파일 변환 *)
Print["4. Convert files:"];
Print["   processFile[\"LST_xSg\", \"LST_xSg.tex\"]"];
Print["   processFile[\"gLgL.txt\", \"gLgL.tex\"]"];
Print[];

Print["5. Output files will be saved in:"];
Print["   ", Directory[]];
Print[];

Print["========================================"];
Print["Ready! Now you can run:"];
Print["  processFile[\"LST_xSg\", \"output.tex\"]"];
Print["========================================"];
