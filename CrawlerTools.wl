(* ::Package:: *)

(*\:5c06\:5b57\:7b26\:4e32\:8f6c\:6362\:6210\:5408\:6cd5\:7684\:6587\:4ef6\:540d*)
ToVilidFileName[file_String]:=StringReplace[file,{"\\"->"\:ff3c","/"->"\:ff0f",":"->"\:ff1a","*"->"\[Times]","?"->"\:ff1f","\""->"\[OpenCurlyDoubleQuote]","<"->"\:300a",">"->"\:300b","|"->"\:4e28"}];

(*\:5e26header\:7684Import\:548cDownload*)
ImportTextFromURL[url_String,header_List:{}]:=FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"],"UTF-8"];
ImportJSONFromURL[url_String,header_List:{}]:=ImportString[FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"]],"JSON"];
DownloadFileFromURL[url_String,file_String,header_List:{}]:=URLDownload[HTTPRequest[url,<|"Headers"->header|>],file];

(*\:5c06JSON\:8f6c\:6362\:6210\:7ed3\:6784\:5316\:6570\:636e\:96c6*)
FromJSONToDataset[json_]:=Dataset[ReplaceRepeated[json,List[kv__Rule]:>Association[kv]]];

FFmpegFileJoin::error="Error occurred, ffmpeg exited with code `1`.";
(*\:4f7f\:7528FFmpeg\:8fde\:63a5\:6587\:4ef6*)
FFmpegFileJoin[input:{__File},output_String]:=Module[{mylist,prop},
(*\:6784\:5efa\:8f93\:5165\:7684\:7eaf\:6587\:672c\:6587\:4ef6*)
mylist=File[Export["mylist.txt",StringJoin["file '"<>#<>"'\n"&/@input[[All,1]]]]];
(*\:4f7f\:7528FFmpeg\:8fde\:63a5*)
prop=RunProcess[{"ffmpeg","-f","concat","-safe",0,"-i",mylist[[1]],"-c","copy",output,"-y"}];
(*\:5220\:9664\:4e34\:65f6\:6587\:4ef6\:5e76\:8fd4\:56de\:8f93\:51fa\:6587\:4ef6*)
Condition[DeleteFile[input];DeleteFile[mylist];File[output],
(*\:5982\:679c\:9000\:51fa\:4ee3\:7801\:4e3a0*)
If[prop["ExitCode"]==0,
True,
(*\:5426\:5219\:6253\:5370\:9519\:8bef\:4fe1\:606f*)
DeleteFile[mylist];Message[FFmpegFileJoin::error,prop["ExitCode"]];False]]
];

FFmpegFileCombine::error="Error occurred, ffmpeg exited with code `1`.";
(*\:4f7f\:7528FFmpeg\:7ec4\:5408\:6587\:4ef6*)
FFmpegFileCombine[input:{__File},output_String]:=Module[{prop},
(*\:4f7f\:7528FFmpeg\:7ec4\:5408*)
prop=RunProcess[{"ffmpeg",Riffle[input[[All,1]],"-i",{1,-2,2}]/.List->Sequence,"-c","copy",output,"-y"}];
(*\:5220\:9664\:4e34\:65f6\:6587\:4ef6\:5e76\:8fd4\:56de\:8f93\:51fa\:6587\:4ef6*)
Condition[DeleteFile[input];File[output],
(*\:5982\:679c\:9000\:51fa\:4ee3\:7801\:4e3a0*)
If[prop["ExitCode"]==0,
True,
(*\:5426\:5219\:6253\:5370\:9519\:8bef\:4fe1\:606f*)
Message[FFmpegFileCombine::error,prop["ExitCode"]];False]]
];
