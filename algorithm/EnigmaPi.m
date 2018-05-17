(* ::Package:: *)

(* ::Section:: *)
(*Global *)


$BlockSize = 1*^6;
$Redundancy = 64;
$SavePath = FileNameJoin[{$UserBaseDirectory, "ApplicationData", "EnigmaPi"}];




(* ::Section:: *)
(*Create Directories*)


If[!FileExistsQ@#, CreateDirectory[#]]& /@ {
	$SavePath,
	FileNameJoin@{$SavePath, "Pi"}
};





(* ::Section:: *)
(*Evaluate a Constant*)


SetAttributes[EvaluateConstant, HoldAll];
EvaluateConstant[offset_, C_ : Pi] := Block[
	{eval, dir, name, path},
	dir = FileNameJoin[{$SavePath, ToString[FromDigits[RealDigits[N@C][[1, 1 ;; 10]]]]}];
	name = StringJoin[ToString[offset + 1], "-", ToString[offset + $BlockSize], ".calc.m"];
	path = FileNameJoin[{dir, name}];
	If[!FileExistsQ@dir, CreateDirectory[dir]];
	If[FileExistsQ@path, Echo[name, "Skip: " ];Return[path]];
	eval = RealDigits[Pi, 10, $BlockSize + $Redundancy, -1 - offset];
	Export[path, <|
		"Constant" -> Hold[C],
		"Type" -> "Original Calculation Results",
		"Offset" -> Last[eval],
		"Time" -> Now,
		"Data" -> BinarySerialize[First@eval, PerformanceGoal -> "Size"]
	|>]
];


(* ::Section:: *)
(*Count*)


CountFirst = Append[List @@ First[#], Length[#]]&;
CountAll[data_, length_ : 1] := Block[
	{pat = FromDigits /@ Take[Partition[data, length, 1], $BlockSize]},
	CountFirst /@ GatherBy[Thread[pat -> Range[$BlockSize]], First]
];


SetAttributes[CountExportTxt, HoldAll];
CountExportTxt[file_, offset_ : 0, C_ : Pi] := Block[
	{name, path, input, data, ans},
	name = StringJoin[ToString /@ {offset + 1, "-", offset + $BlockSize, ".data.mx"}];
	path = FileNameJoin[{$SavePath, ToString@C, name}];
	If[FileExistsQ@path, Echo[name, "Skip: " ];Return[path]];
	input = OpenRead[file];
	SetStreamPosition[input, 2 + offset];
	data = ToExpression /@ Read[input, ConstantArray[Character, $BlockSize + $Redundancy]];
	ans = Flatten[CountAll[data, #]& /@ Range[12], 1];
	Export[path, <|
		"Constant" -> Hold[C],
		"Type" -> "Count Data",
		"Offset" -> offset + 1,
		"Time" -> Now,
		"Data" -> BinarySerialize[ans, PerformanceGoal -> "Size"]
	|>]
]
