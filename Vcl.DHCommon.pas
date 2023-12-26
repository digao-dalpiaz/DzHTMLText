{$IFNDEF FMX}unit Vcl.DHCommon;{$ENDIF}

{$INCLUDE Defines.inc}

interface

uses System.Types, System.UITypes, System.SysUtils,
  {$IFDEF FMX}
    {$IFDEF USE_NEW_UNITS}FMX.Graphics, {$ENDIF} FMX.DzHTMLText
  {$ELSE}
    Vcl.Graphics, Vcl.DzHTMLText
  {$ENDIF};

const
  STR_SPACE = ' ';

type
  TDHMultipleTokenParams = class
  private
    Params: TArray<string>;

  public
    constructor Create(const StrParams: string);

    function GetParam(const Name: string): string;
    function GetParamAsPixels(const Name: string; Def: TPixels): TPixels;
    function GetFirstParam: String;
    function ParamExists(const Name: string): Boolean;
  end;

  TDHCharUtils = class
  private
    class function IsCJKChar(const C: Char): Boolean; inline;
  public
    class function FindNextWordBreakChar(const A: string; From: Integer): Integer; inline;
  end;

procedure DefineFontColor(C: TCanvas; Color: TAnyColor);
function GetGenericFontColor(C: TCanvas): TAnyColor;
procedure DefineFontPt(F: TFont; Pt: TPixels; Lb: TDzHTMLText);
function GetGenericFontPt(F: TFont): TPixels;
procedure DefineFontName(F: TFont; const Name: string);
function GetGenericFontName(F: TFont): string;
procedure DefineFillColor(C: TCanvas; Color: TAnyColor);
function GetGenericFillColor(C: TCanvas): TAnyColor;

function ParamToColor(const Param: string): TAnyColor;

function StrToPixels(const StrValue: string; Def: TPixels): TPixels;

function RoundIfVCL(Value: Extended): TPixels;

function ParamToHorzAlign(const Param: string): TDHHorzAlign;
function ParamToVertAlign(const Param: string): TDHVertAlign;

function AddPointToSize(Size: TAnySize; Point: TAnyPoint): TSize;


implementation

{$IFDEF FMX}uses System.UIConsts;{$ENDIF}

function GetDecimalSettings: TFormatSettings;
begin
  Result := Default(TFormatSettings);
  Result.DecimalSeparator := '.';
end;

function StrToFloatDefPoint(const StrValue: string; Def: Extended): Extended;
begin
  Result := StrToFloatDef(StrValue, Def, GetDecimalSettings);
end;

function StrToPixels(const StrValue: string; Def: TPixels): TPixels;
begin
  Result :=
  {$IFDEF FMX}
    StrToFloatDefPoint(StrValue, Def)
  {$ELSE}
    StrToIntDef(StrValue, Def)
  {$ENDIF};
end;

function ParamToColor(const Param: string): TAnyColor;
var
  A: string;
begin
  A := Param;
  if A.IsEmpty then Exit(clNone);

  if A.StartsWith('#') then A[1] := '$';

  if A.StartsWith('$') then
  begin
    if A.Length=7 then Insert({$IFDEF FMX}'FF'{$ELSE}'00'{$ENDIF}, A, 2);
    //Allow 6-digit (HTML) or 8-digit (Delphi) color notation
    //The firsts two digits in 8-digit format represents the alpha channel in FMX

    if A.Length<>9 then Exit(clNone);
  end;

  try
    Result := {$IFDEF FMX}StringToAlphaColor(A){$ELSE}StringToColor(A){$ENDIF};
  except
    Result := clNone;
  end;
end;

procedure DefineFontColor(C: TCanvas; Color: TAnyColor);
begin
  C.{$IFDEF FMX}Stroke{$ELSE}Font{$ENDIF}.Color := Color;
end;

function GetGenericFontColor(C: TCanvas): TAnyColor;
begin
  Result := C.{$IFDEF FMX}Stroke{$ELSE}Font{$ENDIF}.Color;
end;

procedure DefineFontPt(F: TFont; Pt: TPixels; Lb: TDzHTMLText);
begin
  {$IFDEF VCL}Pt := Lb.CalcFontHeight(Pt);{$ENDIF}

  F.{$IFDEF FMX}Size{$ELSE}Height{$ENDIF} := Pt;
end;

function GetGenericFontPt(F: TFont): TPixels;
begin
  Result := F.{$IFDEF FMX}Size{$ELSE}Height{$ENDIF};
end;

procedure DefineFontName(F: TFont; const Name: string);
begin
   F.{$IFDEF FMX}Family{$ELSE}Name{$ENDIF} := Name;
end;

function GetGenericFontName(F: TFont): string;
begin
   Result := F.{$IFDEF FMX}Family{$ELSE}Name{$ENDIF};
end;

procedure DefineFillColor(C: TCanvas; Color: TAnyColor);
begin
  C.{$IFDEF FMX}Fill{$ELSE}Brush{$ENDIF}.Color := Color;
end;

function GetGenericFillColor(C: TCanvas): TAnyColor;
begin
  Result := C.{$IFDEF FMX}Fill{$ELSE}Brush{$ENDIF}.Color;
end;

{$REGION 'THTMLTokenParams'}
constructor TDHMultipleTokenParams.Create(const StrParams: string);
begin
  Params := StrParams.Split([',']);
end;

function TDHMultipleTokenParams.GetParam(const Name: string): string;
var
  Param: string;
  Ar: TArray<string>;
begin
  for Param in Params do
  begin
    Ar := Param.Split(['=']);
    if Length(Ar) < 2 then Continue;

    if SameText(Ar[0], Name) then
      Exit(Ar[1]);
  end;

  Result := EmptyStr;
end;

function TDHMultipleTokenParams.GetParamAsPixels(const Name: string; Def: TPixels): TPixels;
begin
  Result := StrToPixels(GetParam(Name), Def)
end;

function TDHMultipleTokenParams.ParamExists(const Name: string): Boolean;
var
  Param: string;
begin
  for Param in Params do
    if SameText(Param, Name) then Exit(True);

  Result := False;
end;

function TDHMultipleTokenParams.GetFirstParam: String;
begin
  Result := Params[0];
end;
{$ENDREGION}

class function TDHCharUtils.FindNextWordBreakChar(const A: string; From: Integer): Integer;
var
  I: Integer;
  C: Char;
begin
  for I := From to A.Length do
  begin
    C := A[I];

    if CharInSet(C, [STR_SPACE,'<','>','/','\']) or IsCJKChar(C) then
      Exit(I);
  end;

  Result := 0;
end;

class function TDHCharUtils.IsCJKChar(const C: Char): Boolean; //return if char is Chinese-Japanese-Korean
begin
//East Asian languages break lines in all chars, so each char must be considered as a full word.
{
Block                                   Range       Comment
CJK Unified Ideographs                  4E00-9FFF   Common
CJK Unified Ideographs Extension A      3400-4DBF   Rare
CJK Unified Ideographs Extension B      20000-2A6DF Rare, historic
CJK Unified Ideographs Extension C      2A700-2B73F Rare, historic
CJK Unified Ideographs Extension D      2B740-2B81F Uncommon, some in current use
CJK Unified Ideographs Extension E      2B820-2CEAF Rare, historic
CJK Compatibility Ideographs            F900-FAFF   Duplicates, unifiable variants, corporate characters
CJK Compatibility Ideographs Supplement 2F800-2FA1F Unifiable variants
}
  if C < #10000 then Exit(False); //fast check

  case Integer(C) of
    $4E00..$9FFF,
    $3400..$4DBF,
    $20000..$2A6DF,
    $2A700..$2B73F,
    $2B740..$2B81F,
    $2B820..$2CEAF,
    $F900..$FAFF,
    $2F800..$2FA1F: Exit(True);
  end;

  Result := False;
end;

function RoundIfVCL(Value: Extended): TPixels;
begin
  Result := {$IFDEF VCL}Round{$ENDIF}(Value);
end;

function ParamToHorzAlign(const Param: string): TDHHorzAlign;
begin
    if SameText(Param, 'left') then Result := haLeft else
    if SameText(Param, 'center') then Result := haCenter else
    if SameText(Param, 'right') then Result := haRight else
      Result := haLeft;
end;

function ParamToVertAlign(const Param: string): TDHVertAlign;
begin
    if SameText(Param, 'top') then Result := vaTop else
    if SameText(Param, 'center') then Result := vaCenter else
    if SameText(Param, 'bottom') then Result := vaBottom else
      Result := vaTop;
end;

function AddPointToSize(Size: TAnySize; Point: TAnyPoint): TSize;
begin
  Size.Width := Size.Width + Point.X;
  Size.Height := Size.Height + Point.Y;
end;

end.
