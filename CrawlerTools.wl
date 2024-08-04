(* ::Package:: *)

(*将字符串转换成合法的文件名*)
ToVilidFileName[file_String]:=StringReplace[file,{"\\"->"＼","/"->"／",":"->"：","*"->"×","?"->"？","\""->"“","<"->"《",">"->"》","|"->"丨"}];

(*带header的Import和Download*)
ImportTextFromURL[url_String,header_List:{}]:=FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"],"UTF-8"];
ImportJSONFromURL[url_String,header_List:{}]:=ImportString[FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"]],"JSON"];
DownloadFileFromURL[url_String,file_String,header_List:{}]:=URLDownload[HTTPRequest[url,<|"Headers"->header|>],file];

(*将JSON转换成结构化数据集*)
FromJSONToDataset[json_]:=Dataset[ReplaceRepeated[json,List[kv__Rule]:>Association[kv]]];

FFmpegFileJoin::error="Error occurred, ffmpeg exited with code `1`.";
(*使用FFmpeg连接文件*)
FFmpegFileJoin[input:{__File},output_String]:=Module[{mylist,prop},
(*构建输入的纯文本文件*)
mylist=File[Export["mylist.txt",StringJoin["file '"<>#<>"'\n"&/@input[[All,1]]]]];
(*使用FFmpeg连接*)
prop=RunProcess[{"ffmpeg","-f","concat","-safe",0,"-i",mylist[[1]],"-c","copy",output,"-y"}];
(*删除临时文件并返回输出文件*)
Condition[DeleteFile[input];DeleteFile[mylist];File[output],
(*如果退出代码为0*)
If[prop["ExitCode"]==0,
True,
(*否则打印错误信息*)
DeleteFile[mylist];Message[FFmpegFileJoin::error,prop["ExitCode"]];False]]
];

FFmpegFileCombine::error="Error occurred, ffmpeg exited with code `1`.";
(*使用FFmpeg组合文件*)
FFmpegFileCombine[input:{__File},output_String]:=Module[{prop},
(*使用FFmpeg组合*)
prop=RunProcess[{"ffmpeg",Riffle[input[[All,1]],"-i",{1,-2,2}]/.List->Sequence,"-c","copy",output,"-y"}];
(*删除临时文件并返回输出文件*)
Condition[DeleteFile[input];File[output],
(*如果退出代码为0*)
If[prop["ExitCode"]==0,
True,
(*否则打印错误信息*)
Message[FFmpegFileCombine::error,prop["ExitCode"]];False]]
];
