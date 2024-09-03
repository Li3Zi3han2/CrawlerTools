(* ::Package:: *)

(*将字符串转换成合法的文件名*)
ToVilidFileName[file_String]:=StringReplace[file,{"\\"->"＼","/"->"／",":"->"：","*"->"×","?"->"？","\""->"“","<"->"《",">"->"》","|"->"丨"}];

(*带header的Import和Download*)
ImportTextFromURL[url_String,header_List:{}]:=FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"],"UTF-8"];
ImportJSONFromURL[url_String,header_List:{}]:=ImportString[FromCharacterCode[URLRead[HTTPRequest[url,<|"Headers"->header|>]]["BodyBytes"]],"JSON"];
DownloadFileFromURL[url_String,file_String,header_List:{}]:=URLDownload[HTTPRequest[url,<|"Headers"->header|>],file];
DownloadVideoFromM3U8::typeumd="M3U8 type unmatched.";
DownloadVideoFromM3U8[url_String,file_String,header_List:{}]:=Module[{urlParseRuleList,m3u8,variantDataset,variantURL,variantURLParseRuleList,tsURL,tsURLParseRuleList,tsFile},
urlParseRuleList=Normal[URLParse[url]];
m3u8=ImportTextFromURL[url,header];
Which[
(*如果是多变体播放列表*)
StringContainsQ[m3u8,"#EXT-X-STREAM-INF:"],
variantDataset=Dataset[StringCases[m3u8,"#EXT-X-STREAM-INF:"~~Shortest[para___]~~("\r"|"\n")..~~Shortest[variantURL___]~~("\r"|"\n")..:>Join[<|Rule@@@Partition[SemanticImportString[para,Automatic,"List",Delimiters->{",","="}],2]|>,<|"VARIANTURL"->variantURL|>]]];
variantDataset=variantDataset/.{value_String/;StringMatchQ[value,NumberString]:>ToExpression[value],value_String/;StringMatchQ[value,NumberString~~"x"~~NumberString]:>ToExpression[StringSplit[value,"x"]]};
variantURL=variantDataset[TakeLargestBy[#["RESOLUTION"][[2]]&,1]][1,"VARIANTURL"];
variantURLParseRuleList=Normal[URLParse[variantURL]];
variantURL=URLBuild[urlParseRuleList/.HoldPattern["Path"->path_]:>("Path"->Join[Most[path],"Path"/.variantURLParseRuleList])];
DownloadVideoFromM3U8[variantURL,file,header]
,
(*如果是视频点播播放列表*)
StringContainsQ[m3u8,"#EXT-X-PLAYLIST-TYPE:VOD"],
tsURL=StringTrim[StringCases[m3u8,StartOfLine~~Except["#"]~~Shortest[___]~~EndOfLine]];
tsURLParseRuleList=Normal[URLParse/@tsURL];
tsURL=URLBuild[urlParseRuleList/.HoldPattern["Path"->path_]:>("Path"->Join[Most[path],#])]&/@("Path"/.tsURLParseRuleList);
tsFile=Table[DownloadFileFromURL[tsURL[[i]],Directory[],header],{i,1,Length[tsURL]}];
FFmpegFileJoin[tsFile,file],
(*其他情况*)
True,
Message[DownloadVideoFromM3U8::typeumd];
Null
]
];
DownloadVideoFromMPD[url_String,file_String,header_List:{}]:=Module[{urlParseRuleList,mpd,videoDataset,audioDataset,videoURL,audioURL,videoURLParseRuleList,audioURLParseRuleList,videoFile,audioFile},
urlParseRuleList=Normal[URLParse[url]];
mpd=ImportString[ImportTextFromURL[url,header],"XML"];
videoDataset=Dataset[Cases[mpd,XMLElement["Representation",r_,{XMLElement["BaseURL",{},{b_}],_}]:>Join[<|r|>,<|"baseURL"->b|>],+\[Infinity]]];
audioDataset=Dataset[Cases[mpd,XMLElement["Representation",r_,{_,XMLElement["BaseURL",{},{b_}],_}]:>Join[<|r|>,<|"baseURL"->b|>],+\[Infinity]]];
videoURL=videoDataset[TakeLargestBy[ToExpression[#["height"]]&,1]][1,"baseURL"];
audioURL=audioDataset[TakeLargestBy[ToExpression[#["bandwidth"]]&,1]][1,"baseURL"];
videoURLParseRuleList=Normal[URLParse[videoURL]];
audioURLParseRuleList=Normal[URLParse[audioURL]];
videoURL=URLBuild[urlParseRuleList/.HoldPattern["Path"->path_]:>("Path"->Join[Most[path],"Path"/.videoURLParseRuleList])];
audioURL=URLBuild[urlParseRuleList/.HoldPattern["Path"->path_]:>("Path"->Join[Most[path],"Path"/.audioURLParseRuleList])];
videoFile=DownloadFileFromURL[videoURL,Directory[],header];
audioFile=DownloadFileFromURL[audioURL,Directory[],header];
FFmpegFileCombine[{videoFile,audioFile},file]
];

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
