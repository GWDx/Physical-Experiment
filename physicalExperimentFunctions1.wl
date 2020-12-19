(* ::Package:: *)

(* ::Section:: *)
(*Begin*)


BeginPackage["physicalExperimentFunctions1`"];


(* ::Section:: *)
(*Introduce*)


gridGen::usage="\:5c06\:77e9\:9635 da \:7528'\n'\:548c' '\:5206\:9694\:ff0c\:4fbf\:4e8e\:5236\:4f5c\:8868\:683c";


plot::usage="\:7ed8\:56fe\nplot[\:5173\:4e8ex\:7684\:51fd\:6570,\:6807\:6ce8,\:81ea\:53d8\:91cf\:8303\:56f4,\:8303\:56f4,\:7f51\:683c,\:5750\:6807\:8f74\:6807\:8bb0,\:523b\:5ea6,\:6570\:636e\:70b9,\:5176\:4ed6\:9009\:9879]";


gridColor::usage="\:5f53x\:662fy\:7684\:500d\:6570\:65f6\:663e\:793a\:7070\:8272\:ff0c\:5426\:5219\:663e\:793a\:6d45\:7070\:8272\ngridColor[x,y]";


holdRule::usage="\:4ee3\:5165\:89c4\:5219\nholdRule[\:53d8\:91cf,\:7cbe\:5ea6]";


exportFormula::usage="\:5bfc\:51fa\:516c\:5f0f\nexportFormula[LHS,RHS,rules]";


format::usage="\:663e\:793aexportFormula\:7684\:5185\:5bb9\nformat@exportFormula[a,b,r]"


(* ::Section:: *)
(*Main*)


Begin["`Private`"];


(* ::Subsection:: *)
(*plot*)


gridGen[da_]:=StringJoin@@Flatten@Join@Riffle[Riffle[ToString@NumberForm[#,4]&/@#," "]&/@da,"\n"]


plot[f_,{s_:None,po_:Above},{a_,b_},range_,grid_,axes_,ticks_,data_List,rules___Rule]:=
Block[{d=(b-a)/20},Show[Plot[Callout[If[a<=x<=b,f],s,po],{x,a-d,b+d},
PlotRange->range,GridLines->grid,GridLinesStyle->{LightGray},
AxesStyle->Arrowheads[.015],AxesLabel->axes,Ticks->ticks,rules],
ListPlot@data]]


gridColor[x_,y_]:={x,Directive[If[Divisible[x,y],Gray,Lighter@LightGray]]}


(* ::Subsection:: *)
(*exportFormula*)


holdRule[x_,n_:4]:=HoldPattern@x->NumberForm[N@x,n,ScientificNotationThreshold->{-3,4}];
holdRule[{x_},n_:4]:={holdRule[x,n]};
holdRule[{y__,x_},n_:4]:=Append[holdRule[{y},n],holdRule[x,n]];
Attributes[holdRule]={HoldAll};


exportFormula[a_,b_,r_holdRule]:={Defer@a,Defer@b,Defer@b/.r,NumberForm[N@a,3,ScientificNotationThreshold->{-3,3}]}
Attributes[export]={HoldAll};


format[{a_,b_,c_,d_}]:=Defer[a=b=c=d]


(* ::Section:: *)
(*End*)


End[];


EndPackage[]
