{$IFNDEF FMX}unit Vcl.DHTokenEngine;{$ENDIF}

{$INCLUDE Defines.inc}

interface

uses
{$IFDEF FMX}FMX.DzHTMLText{$ELSE}Vcl.DzHTMLText{$ENDIF},
{$IFDEF FPC}
  Classes, Types, Graphics, FGL
{$ELSE}
  System.Classes, System.Generics.Collections, System.Types, System.UITypes,
  {$IFDEF FMX}
    FMX.Types, System.UIConsts
    {$IFDEF USE_NEW_UNITS}, FMX.Graphics{$ENDIF}
  {$ELSE}
    Vcl.Graphics
  {$ENDIF}
{$ENDIF};

{$INCLUDE Types.inc}
type
  TDHBorderRec = record
  private
    Margin, Thick, Pad: TPixels;
    LnColor: TAnyColor;
  end;
  TDHBordersRec = record
  private
    Left, Top, Right, Bottom: TDHBorderRec;
  end;

  TDHOffsetRec = record
  private
    Top, Bottom: TPixels;
    function GetHeight: TPixels;
  end;
  TDHPropsStore = class
  private
    FontColor: TAnyColor;
    BackColor: TAnyColor;
    Offset: TDHOffsetRec;
    HorzAlign: TDHHorzAlign;
    VertAlign: TDHVertAlign;

    //--Superscript and Subscript
    SS_Inside: Boolean;
    SS_FullHeight: TPixels;
    SS_YPos: TPixels;
    //--

    //--List (Ordered and Unordered)
    List_Level: Byte;
    List_Number: Word;
    List_Bullet: Boolean;
    //--

    LineSpace, ParagraphSpace, ParagraphIndent: TPixels;

    procedure AssignProps(Source: TDHPropsStore);
  end;

  {$REGION 'Token base classes'}
  TDHBuilder = class;
  TDHTokenBlock = class;

  TDHToken = class abstract
  private
    Builder: TDHBuilder;
    Parent: TDHTokenBlock;

    Param: string;
    ValidParam: Boolean;

    procedure Init(Builder: TDHBuilder);

    procedure ReadParam; virtual;
    procedure Process; virtual;

    function GetCanvas: TCanvas;
    property Canvas: TCanvas read GetCanvas;

    function GetProps: TDHPropsStore;
    property Props: TDHPropsStore read GetProps;

    function GetLb: TDzHTMLText;
    property Lb: TDzHTMLText read GetLb;

    function IsBreakableToken: Boolean; virtual;
  public
    constructor Create; virtual; //must be here to run constructor when creating by TDHTokenClass
  end;
  TDHTokenClass = class of TDHToken;

  TDHTokenSingle = class abstract(TDHToken);

  TDHTokenBlock = class abstract(TDHToken)
  private
    Children: TObjectList<TDHToken>;

    function IsBypassProps: Boolean; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;
  end;
  {$ENDREGION}

  {$REGION 'Token single classes'}
  TDHToken_Break = class(TDHTokenSingle)
  private
    NoBreak: Boolean;
    Continuous: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
    function IsBreakableToken: Boolean; override;
  end;

  TDHToken_Word = class(TDHTokenSingle)
  private
    Word: string;
    Breakable: Boolean;

    procedure Process; override;
    procedure CheckForSpaceWhenNoBreak;
  end;

  TDHToken_Line = class(TDHTokenSingle)
  private
    Width, Height: TPixels;
    Color, ColorAlt: TAnyColor;
    Full: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Image = class(TDHTokenSingle)
  private
    ImageIndex: Integer;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_ImageResource = class(TDHTokenSingle)
  private
    ResourceName: string;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_TabBase = class abstract(TDHTokenSingle)
  private
    Margin: TPixels;

    procedure ReadParam; override;
    procedure Process; override;

    function IsBreakableToken: Boolean; override;
  end;
  TDHToken_Tab = class(TDHToken_TabBase);
  TDHToken_TabF = class(TDHToken_TabBase);
  {$ENDREGION}

  {$REGION 'Token block classes'}
  TDHToken_Main = class(TDHTokenBlock);

  TDHToken_FontStyleItem = class abstract(TDHTokenBlock)
  private
    Disabled: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Bold = class(TDHToken_FontStyleItem);
  TDHToken_Italic = class(TDHToken_FontStyleItem);
  TDHToken_Underline = class(TDHToken_FontStyleItem);
  TDHToken_Strikeout = class(TDHToken_FontStyleItem);

  TDHToken_FontName = class(TDHTokenBlock)
  private
    Name: string;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_FontSize = class(TDHTokenBlock)
  private
    Size: TPixels;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Color = class abstract(TDHTokenBlock)
  private
    Color: TAnyColor;

    procedure ReadParam; override;
  end;

  TDHToken_FontColor = class(TDHToken_Color)
  private
    procedure Process; override;
  end;

  TDHToken_BackColor = class(TDHToken_Color)
  private
    procedure Process; override;
  end;

  TDHToken_Header = class(TDHTokenBlock)
  private
    Level: Byte;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_CustomStyle = class(TDHTokenBlock)
  private
    Ident: string;

    procedure ReadParam; override;
    procedure Process; override;

    procedure ApplyFontStyle(Value: TDHCustomStyleBoolValue;
      FontStyle: TFontStyle);
  end;

  TDHToken_Link = class(TDHTokenBlock)
  private
    Target: string;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_AlignLeft = class(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_AlignCenter = class(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_AlignRight = class(TDHTokenBlock)
  private
    procedure Process; override;
  end;

  TDHToken_VertAlign = class(TDHTokenBlock)
  private
    Align: TDHVertAlign;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Offset = class(TDHTokenBlock)
  private
    Top, Bottom: TPixels;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Spoiler = class abstract(TDHTokenBlock)
  private
    Ident: string;
  end;
  TDHToken_SpoilerTitle = class(TDHToken_Spoiler)
  private
    StartExpanded: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
  end;
  TDHToken_SpoilerDetail = class(TDHToken_Spoiler)
  private
    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_SuperOrSubScript = class abstract(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_Superscript = class(TDHToken_SuperOrSubScript);
  TDHToken_Subscript = class(TDHToken_SuperOrSubScript);

  TDHToken_List = class abstract(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_OrderedList = class(TDHToken_List);
  TDHToken_UnorderedList = class(TDHToken_List);
  TDHToken_ListItem = class(TDHTokenBlock)
  private
    procedure Process; override;

    function IsBreakableToken: Boolean; override;
    function IsBypassProps: Boolean; override;
  end;

  TDHToken_LineSpace = class(TDHTokenBlock)
  private
    Space, ParagraphSpace: TPixels;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_ParagraphIndent = class(TDHTokenBlock)
  private
    Indent: TPixels;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  {$SCOPEDENUMS ON}
  TDHDivSizeType = (Auto, Outer, Inner, Full, Percent, Reverse, Line);
  {$SCOPEDENUMS OFF}
  TDHToken_Div = class(TDHTokenBlock)
  private
    Borders: TDHBordersRec;
    BackColor, BorderColor: TAnyColor;
    Size: TAnySize;
    FloatPos: TAnyPoint;
    MaxWidth: TPixels;
    HorzAlign: TDHHorzAlign;
    VertAlign: TDHVertAlign;
    WidthType, HeightType: TDHDivSizeType;
    KeepProps: Boolean;
    FloatingBehind: Boolean;
    CornerRadius: TPixels;

    Floating: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
    function IsBreakableToken: Boolean; override;
  end;
  TDHToken_Float = class(TDHToken_Div);
  {$ENDREGION}

  TDHPreVisualItem = class;
  TDHPreVisualItemList = class;
  TDHDivAreaLine = class
  private
    Continuous: Boolean; //when this line is a continuation of the previous one
    TabUsed: Boolean; //when a tab is used in the line
    Items: TDHPreVisualItemList;
    YPos: TPixels;

    TextSize: TAnySize;
    function GetHighHeight: TPixels;
  public
    constructor Create;
    destructor Destroy; override;
  end;
  TDHDivArea = class
  private
    Parent: TDHDivArea;
    Main: Boolean;

    Point: TAnyPoint; //current processing point

    AutoWidth, AutoHeight: Boolean;
    MaxWidth: TPixels;

    HorzAlign: TDHHorzAlign;
    VertAlign: TDHVertAlign;
    Borders: TDHBordersRec;

    Lines: TObjectList<TDHDivAreaLine>;

    HeightByLine: Boolean;

    PreVisualItem: TDHPreVisualItem;
    FloatingDivs: TDHPreVisualItemList;

    FloatingBehind: Boolean;

    FixedSize, TextSize: TAnySize;

    LeftMarginOn: Boolean;
    LeftMargin: TPixels;

    procedure SetLeftMargin(Flag: Boolean; Value: TPixels = 0);

    procedure CalcTextSize;
    function GetParagraphCount: Integer;

    function GetHorzBorder: TPixels;
    function GetVertBorder: TPixels;

    function GetAreaSizeWOB: TAnySize;
    function GetAreaSize: TAnySize;
    function GetAbsoluteStartingPos: TAnyPoint;

    function GetLastItem: TDHPreVisualItem;

    function ClearLinesIfNoItems: Boolean;
  public
    constructor Create(Parent: TDHDivArea);
    destructor Destroy; override;
  end;

  TDHPreVisualItem = class
  private
    VisualObject: TDHVisualItem;
    Size: TAnySize;
    Position: TAnyPoint;
    HorzAlign: TDHHorzAlign;
    VertAlign: TDHVertAlign;
    Offset: TDHOffsetRec;

    SelfDiv: TDHDivArea;

    function GetFullHeight: TPixels;
    function IsSpace: Boolean;
  public
    destructor Destroy; override;
  end;
  TDHPreVisualItemList = class(TObjectList<TDHPreVisualItem>)
  private
    function GetSumWidth: TPixels;
  end;

  TDHProcBoundsAndLines = procedure(InnerSize, OuterSize: TAnySize;
    LineCount, ParagraphCount: Integer) of object;

  TDHBuilder = class
  private
    Lb: TDzHTMLText;
    Canvas: TCanvas;
    VisualItems: TDHVisualItemList;
    ProcBoundsAndLines: TDHProcBoundsAndLines;

    Props, QueueProps: TDHPropsStore;

    CurrentLink: TDHBaseLink;

    MainToken: TDHToken_Main;
    CurrentBlock: TDHTokenBlock;

    MainDiv, CurrentDiv: TDHDivArea;

    QueueVisualItems: TDHPreVisualItemList;

    LastNoBreak: Boolean;

    function AddToken<T: TDHToken, constructor>: T;
    procedure AddInvalidToken(Position: Integer; const ErrorDescription: string);

    function CalcTextHeight(const Text: string): TPixels;
    procedure EndOfLine;
    function JumpLine(Continuous: Boolean): TDHDivAreaLine;
    function AddNewLineObject(Continuous: Boolean): TDHDivAreaLine;

    procedure ReadTokens;
    function ProcessTag(const Tag: string): string;
    procedure ProcessChildrenTokens(Block: TDHTokenBlock);
    procedure SendObjectsToComponent(DivArea: TDHDivArea);
    procedure CheckAlign(Item: TDHPreVisualItem; Line: TDHDivAreaLine; DivArea: TDHDivArea);

    procedure ApplyLineMargin;
    procedure ApplyLineSpace;

    procedure AddVisualItemToQueue(V: TDHVisualItem; Size: TAnySize; Breakable: Boolean = False);
    function CreatePreVisualItem(V: TDHVisualItem; Size: TAnySize): TDHPreVisualItem;
    procedure ProcessPendingObjects;
    procedure ProcessOneObject(Item: TDHPreVisualItem);
    procedure ProcessSpecificObjects(List: TDHPreVisualItemList);

    procedure CheckMissingClosingTags;
    function GetDefaultHorzTextAlign: TDHHorzAlign;
  public
    constructor Create(Lb: TDzHTMLText; Canvas: TCanvas;
      VisualItems: TDHVisualItemList; ProcBoundsAndLines: TDHProcBoundsAndLines);
    destructor Destroy; override;

    procedure Execute;
  end;

implementation

uses
{$IFDEF FMX}FMX.DHCommon{$ELSE}Vcl.DHCommon{$ENDIF},
{$IFDEF FPC}
  Variants, SysUtils, StrUtils
{$ELSE}
  System.Variants, System.SysUtils, System.StrUtils,
  {$IFDEF FMX}
  FMX.Controls
  {$ELSE}
  Vcl.Themes, Vcl.Controls
  {$ENDIF}
{$ENDIF};

type
  TDHTokenObjectDef = record
    Ident: string;
    Clazz: TDHTokenClass;
    AllowPar: Boolean;
    OptionalPar: Boolean;
  end;

const
  TOKENS_OBJECTS: array[0..32] of TDHTokenObjectDef = (
    //single
    (Ident: 'BR'; Clazz: TDHToken_Break; AllowPar: True; OptionalPar: True), //breakable!
    (Ident: 'LINE'; Clazz: TDHToken_Line; AllowPar: True; OptionalPar: True),
    (Ident: 'IMG'; Clazz: TDHToken_Image; AllowPar: True),
    (Ident: 'IMGRES'; Clazz: TDHToken_ImageResource; AllowPar: True),
    (Ident: 'T'; Clazz: TDHToken_Tab; AllowPar: True), //breakable! {OBSOLETE}
    (Ident: 'TF'; Clazz: TDHToken_TabF; AllowPar: True), //breakable! {OBSOLETE}

    //block
    (Ident: 'B'; Clazz: TDHToken_Bold; AllowPar: True; OptionalPar: True),
    (Ident: 'I'; Clazz: TDHToken_Italic; AllowPar: True; OptionalPar: True),
    (Ident: 'U'; Clazz: TDHToken_Underline; AllowPar: True; OptionalPar: True),
    (Ident: 'S'; Clazz: TDHToken_Strikeout; AllowPar: True; OptionalPar: True),
    (Ident: 'FN'; Clazz: TDHToken_FontName; AllowPar: True),
    (Ident: 'FC'; Clazz: TDHToken_FontColor; AllowPar: True),
    (Ident: 'FS'; Clazz: TDHToken_FontSize; AllowPar: True),
    (Ident: 'BC'; Clazz: TDHToken_BackColor; AllowPar: True),
    (Ident: 'OFFSET'; Clazz: TDHToken_Offset; AllowPar: True),
    (Ident: 'DIV'; Clazz: TDHToken_Div; AllowPar: True; OptionalPar: True), //breakable!
    (Ident: 'A'; Clazz: TDHToken_Link; AllowPar: True; OptionalPar: True),
    (Ident: 'L'; Clazz: TDHToken_AlignLeft),
    (Ident: 'C'; Clazz: TDHToken_AlignCenter),
    (Ident: 'R'; Clazz: TDHToken_AlignRight),
    (Ident: 'VALIGN'; Clazz: TDHToken_VertAlign; AllowPar: True),
    (Ident: 'SPOILER'; Clazz: TDHToken_SpoilerTitle; AllowPar: True),
    (Ident: 'SDETAIL'; Clazz: TDHToken_SpoilerDetail; AllowPar: True),
    (Ident: 'STYLE'; Clazz: TDHToken_CustomStyle; AllowPar: True),
    (Ident: 'H'; Clazz: TDHToken_Header; AllowPar: True),
    (Ident: 'SUP'; Clazz: TDHToken_Superscript),
    (Ident: 'SUB'; Clazz: TDHToken_Subscript),
    (Ident: 'UL'; Clazz: TDHToken_UnorderedList),
    (Ident: 'OL'; Clazz: TDHToken_OrderedList),
    (Ident: 'LI'; Clazz: TDHToken_ListItem), //breakable!
    (Ident: 'LS'; Clazz: TDHToken_LineSpace; AllowPar: True),
    (Ident: 'PI'; Clazz: TDHToken_ParagraphIndent; AllowPar: True),
    (Ident: 'FLOAT'; Clazz: TDHToken_Float; AllowPar: True) {OBSOLETE}
  );

function GetTokenObjectDefIndexFromIdent(const Ident: string): Integer;
var
  I: Integer;
begin
  for I := Low(TOKENS_OBJECTS) to High(TOKENS_OBJECTS) do
    if TOKENS_OBJECTS[I].Ident = Ident then Exit(I);

  Result := -1;
end;

function GetTokenIdentFromClass(Clazz: TDHTokenClass): string;
var
  I: Integer;
begin
  for I := Low(TOKENS_OBJECTS) to High(TOKENS_OBJECTS) do
    if TOKENS_OBJECTS[I].Clazz = Clazz then Exit(TOKENS_OBJECTS[I].Ident);

  raise EDHInternalExcept.CreateFmt('Token class "%s" invalid', [Clazz.ClassName]);
end;

procedure SetWhenBigger(var V: TPixels; This: TPixels);
begin
  if This > V then V := This;
end;

{$REGION 'Token types'}

{ TDHToken }

constructor TDHToken.Create;
begin
  //this method must be declared to ensure constructor exists when creating inherited class by TDHTokenClass
end;

function TDHToken.GetCanvas: TCanvas;
begin
  Result := Builder.Canvas;
end;

function TDHToken.GetLb: TDzHTMLText;
begin
  Result := Builder.Lb;
end;

function TDHToken.GetProps: TDHPropsStore;
begin
  Result := Builder.Props;
end;

procedure TDHToken.Init(Builder: TDHBuilder);
begin
  Self.Builder := Builder;

  Parent := Builder.CurrentBlock;
  Parent.Children.Add(Self);
end;

procedure TDHToken.ReadParam;
begin
  raise EDHInternalExcept.CreateFmt('%s must implement ReadParam method', [Self.ClassName]);
end;

procedure TDHToken.Process;
begin
  raise EDHInternalExcept.CreateFmt('%s must implement Process method', [Self.ClassName]);
end;

function TDHToken.IsBreakableToken: Boolean;
begin
  Result := False;
end;

{ TDHTokenBlock }

constructor TDHTokenBlock.Create;
begin
  inherited;
  Children := TObjectList<TDHToken>.Create;
end;

destructor TDHTokenBlock.Destroy;
begin
  Children.Free;
  inherited;
end;

function TDHTokenBlock.IsBypassProps: Boolean;
begin
  Result := False;
end;

{ TDHToken_FontStyleItem }

procedure TDHToken_FontStyleItem.ReadParam;
begin
  Disabled := SameText(Param, 'off');
end;

procedure TDHToken_FontStyleItem.Process;
var
  FS: TFontStyles;
  Style: TFontStyle;
begin
  FS := Canvas.Font.Style;

  if Self is TDHToken_Bold then Style := TFontStyle.fsBold else
  if Self is TDHToken_Italic then Style := TFontStyle.fsItalic else
  if Self is TDHToken_Underline then Style := TFontStyle.fsUnderline else
  if Self is TDHToken_Strikeout then Style := TFontStyle.fsStrikeOut else
    raise EDHInternalExcept.Create('Invalid Font Style Item class');

  if Disabled then
    Exclude(FS, Style)
  else
    Include(FS, Style);

  Canvas.Font.Style := FS;
end;

{ TDHToken_FontName }

procedure TDHToken_FontName.ReadParam;
begin
  Name := Param;
end;

procedure TDHToken_FontName.Process;
begin
  DefineFontName(Canvas.Font, Name);
end;

{ TDHToken_FontSize }

procedure TDHToken_FontSize.ReadParam;
begin
  Size := StrToPixels(Param, 0);
  ValidParam := Size>0;
end;

procedure TDHToken_FontSize.Process;
begin
  DefineFontPt(Canvas.Font, Size, Lb); //scaled
end;

{ TDHToken_Color }

procedure TDHToken_Color.ReadParam;
begin
  Color := ParamToColor(Param);
  ValidParam := Color<>clNone;
end;

{ TDHToken_FontColor }

procedure TDHToken_FontColor.Process;
begin
  Props.FontColor := Color;
end;

{ TDHToken_BackColor }

procedure TDHToken_BackColor.Process;
begin
  Props.BackColor := Color;
end;

{ TDHToken_Link }

procedure TDHToken_Link.ReadParam;
begin
  Target := Param;
end;

procedure TDHToken_Link.Process;
var
  Link: TDHLinkRef;
begin
  Link := TDHLinkRef.Create(Target);
  Lb.LinkRefs.Add(Link);

  Builder.CurrentLink := Link;
end;

{ TDHToken_CustomStyle }

procedure TDHToken_CustomStyle.ReadParam;
begin
  Ident := Param;
end;

procedure TDHToken_CustomStyle.Process;
var
  Style: TDHCustomStyle;
begin
  Style := Lb.CustomStyles.FindByIdent(Ident);
  if Style=nil then Exit; //style not found

  ApplyFontStyle(Style.StyleBold, TFontStyle.fsBold);
  ApplyFontStyle(Style.StyleItalic, TFontStyle.fsItalic);
  ApplyFontStyle(Style.StyleUnderline, TFontStyle.fsUnderline);
  ApplyFontStyle(Style.StyleStrikeout, TFontStyle.fsStrikeOut);
  if Style.FontName<>EmptyStr then DefineFontName(Canvas.Font, Style.FontName);
  if Style.FontSize>0 then DefineFontPt(Canvas.Font, Style.FontSize, Lb); //scaled
  if Style.FontColor<>clNone then Props.FontColor := Style.FontColor;
  if Style.BackColor<>clNone then Props.BackColor := Style.BackColor;

  if Style.HorzAlign<>TDHCustomStyleHorzAlignValue.Undefined then
  begin
    case Style.HorzAlign of
      TDHCustomStyleHorzAlignValue.Left: Props.HorzAlign := haLeft;
      TDHCustomStyleHorzAlignValue.Center: Props.HorzAlign := haCenter;
      TDHCustomStyleHorzAlignValue.Right: Props.HorzAlign := haRight;
      else raise EDHInternalExcept.Create('Invalid horizontal align style');
    end;
  end;

  if Style.VertAlign<>TDHCustomStyleVertAlignValue.Undefined then
  begin
    case Style.VertAlign of
      TDHCustomStyleVertAlignValue.Top: Props.VertAlign := vaTop;
      TDHCustomStyleVertAlignValue.Center: Props.VertAlign := vaCenter;
      TDHCustomStyleVertAlignValue.Bottom: Props.VertAlign := vaBottom;
      else raise EDHInternalExcept.Create('Invalid vertical align style');
    end;
  end;

  if Style.OffsetTop>=0 then Props.Offset.Top := Lb.CalcScale(Style.OffsetTop);
  if Style.OffsetBottom>=0 then Props.Offset.Bottom := Lb.CalcScale(Style.OffsetBottom);

  if Style.LineSpacing>=0 then Props.LineSpace := Lb.CalcScale(Style.LineSpacing);
  if Style.ParagraphSpacing>=0 then Props.ParagraphSpace := Lb.CalcScale(Style.ParagraphSpacing);
  if Style.ParagraphIndent>=0 then Props.ParagraphIndent := Lb.CalcScale(Style.ParagraphIndent);
end;

procedure TDHToken_CustomStyle.ApplyFontStyle(Value: TDHCustomStyleBoolValue; FontStyle: TFontStyle);
var
  FS: TFontStyles;
begin
  if Value = TDHCustomStyleBoolValue.Undefined then Exit;

  FS := Canvas.Font.Style;

  if Value = TDHCustomStyleBoolValue.False then
    Exclude(FS, FontStyle)
  else
    Include(FS, FontStyle);

  Canvas.Font.Style := FS;
end;

{ TDHToken_Header }

procedure TDHToken_Header.ReadParam;
begin
  Level := StrToIntDef(Param, 0);
  ValidParam := (Level>=1) and (Level<=6);
end;

procedure TDHToken_Header.Process;
begin
  Canvas.Font.Size := RoundIfVCL(Lb.Font.Size * ((6-Level+2)/2)); //Font auto scaled
  Canvas.Font.Style := Canvas.Font.Style + [TFontStyle.fsBold];
end;

{ TDHToken_Offset }

procedure TDHToken_Offset.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Top := Lb.CalcScale(P.GetParamAsPixels('top', -1));
    Bottom := Lb.CalcScale(P.GetParamAsPixels('bottom', -1));
  finally
    P.Free;
  end;
end;

procedure TDHToken_Offset.Process;
begin
  if Top>=0 then Props.Offset.Top := Top;
  if Bottom>=0 then Props.Offset.Bottom := Bottom;
end;

{ TDHToken_Word }

procedure TDHToken_Word.Process;
var
  V: TDHVisualItem_Word;
  Size: TAnySize;
begin
  CheckForSpaceWhenNoBreak;

  Size := TAnySize.Create(Canvas.TextWidth(Word), Builder.CalcTextHeight(Word)); //scaled

  V := TDHVisualItem_Word.Create;
  V.Text := Word;
  V.Font.Assign(Canvas.Font);
  V.{$IFDEF FMX}FontColor{$ELSE}Font.Color{$ENDIF} := Props.FontColor;

  if Props.SS_Inside then
    V.YPos := Props.SS_YPos;

  if Builder.CurrentLink is TDHLinkRef then
    Builder.CurrentLink.LinkRef.Text.Append(Word);

  Builder.AddVisualItemToQueue(V, Size, Breakable);
end;

procedure TDHToken_Word.CheckForSpaceWhenNoBreak;
var
  Token: TDHToken_Word;
begin
  if Builder.LastNoBreak then
  begin
    Builder.LastNoBreak := False; //reset to avoid circular loop

    Token := TDHToken_Word.Create;
    try
      Token.Builder := Builder;
      Token.Word := STR_SPACE;
      Token.Breakable := True;
      Token.Process;
    finally
      Token.Free;
    end;
  end;
end;

{ TDHToken_Image }

procedure TDHToken_Image.ReadParam;
begin
  ImageIndex := StrToIntDef(Param, -1);
  ValidParam := ImageIndex >= 0;
end;

procedure TDHToken_Image.Process;
var
  V: TDHVisualItem_Image;
  Size: TAnySize;
  IgnoreScale: Boolean;
begin
  IgnoreScale := False;

  {$IFDEF USE_IMGLST}
  if Assigned(Lb.Images) then
  begin
    {$IFDEF FMX}
    with Lb.Images.Destination[ImageIndex].Layers[0].SourceRect do
      Size := TAnySize.Create(Width, Height);
    {$ELSE}
    Size := TAnySize.Create(Lb.Images.Width, Lb.Images.Height);
    if Lb.Images.IsScaled then IgnoreScale := True; //imagelist already scaled
    {$ENDIF}
  end;
  {$ENDIF}

  if not IgnoreScale then
  begin
    Size.Width := Lb.CalcScale(Size.Width);
    Size.Height := Lb.CalcScale(Size.Height);
  end;

  V := TDHVisualItem_Image.Create;
  V.ImageIndex := ImageIndex;

  Builder.AddVisualItemToQueue(V, Size);
end;

{ TDHToken_ImageResource }

procedure TDHToken_ImageResource.ReadParam;
begin
  ResourceName := Param;
end;

procedure TDHToken_ImageResource.Process;
var
  V: TDHVisualItem_ImageResource;
  Size: TAnySize;
begin
  V := TDHVisualItem_ImageResource.Create;
  V.Load(Lb, ResourceName);

  Size := TAnySize.Create(Lb.CalcScale(V.Picture.Width), Lb.CalcScale(V.Picture.Height));

  Builder.AddVisualItemToQueue(V, Size);
end;

{ TDHToken_Break }

function TDHToken_Break.IsBreakableToken: Boolean;
begin
  Result := True;
end;

procedure TDHToken_Break.ReadParam;
begin
  Continuous := SameText(Param, 'cont');
end;

procedure TDHToken_Break.Process;
var
  Item: TDHPreVisualItem;
begin
  if NoBreak then
  begin
    Item := Builder.CurrentDiv.GetLastItem;
    if (Item<>nil) and (Item.VisualObject is TDHVisualItem_Word) then
      Builder.LastNoBreak := True;

    Exit;
  end;

  Builder.JumpLine(Continuous);
end;

{ TDHToken_Line }

procedure TDHToken_Line.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Width := Lb.CalcScale(P.GetParamAsPixels('width', 100));
    Height := Lb.CalcScale(P.GetParamAsPixels('height', 1));

    Color := P.GetParamAsColor('color');
    ColorAlt := P.GetParamAsColor('coloralt');

    Full := SameText(P.GetParam('width'), 'full');
  finally
    P.Free;
  end;
end;

procedure TDHToken_Line.Process;
var
  V: TDHVisualItem_Line;
  Size: TAnySize;
begin
  V := TDHVisualItem_Line.Create;
  V.Color := Color;
  V.ColorAlt := ColorAlt;

  if Full and not Builder.CurrentDiv.AutoWidth then
    Width := Builder.CurrentDiv.GetAreaSizeWOB.Width - Builder.CurrentDiv.Point.X;

  Size := TAnySize.Create(Width, Height);

  Builder.AddVisualItemToQueue(V, Size);
end;

{ TDHToken_TabBase }

function TDHToken_TabBase.IsBreakableToken: Boolean;
begin
  Result := True;
end;

procedure TDHToken_TabBase.ReadParam;
begin
  Margin := Lb.CalcScale(StrToPixels(Param, -1));
  ValidParam := Margin>=0;
end;

procedure TDHToken_TabBase.Process;
begin
  Builder.CurrentDiv.Point.X := Margin;

  if Self is TDHToken_TabF then
    Builder.CurrentDiv.SetLeftMargin(True, Margin)
  else
    Builder.CurrentDiv.SetLeftMargin(False);

  Builder.CurrentDiv.Lines.Last.TabUsed := True;
end;

{ TDHToken_AlignLeft }

procedure TDHToken_AlignLeft.Process;
begin
  Props.HorzAlign := haLeft;
end;

{ TDHToken_AlignCenter }

procedure TDHToken_AlignCenter.Process;
begin
  Props.HorzAlign := haCenter;
end;

{ TDHToken_AlignRight }

procedure TDHToken_AlignRight.Process;
begin
  Props.HorzAlign := haRight;
end;

{ TDHToken_VertAlign }

procedure TDHToken_VertAlign.ReadParam;
begin
  Align := ParamToVertAlign(Param);
end;

procedure TDHToken_VertAlign.Process;
begin
  Props.VertAlign := Align;
end;

{ TDHToken_SpoilerTitle }

procedure TDHToken_SpoilerTitle.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Ident := P.GetFirstParam;
    StartExpanded := P.ParamExists('exp');
  finally
    P.Free;
  end;
end;

procedure TDHToken_SpoilerTitle.Process;
var
  Spoiler: TDHSpoiler;
begin
  //When first time rebuild (or after text changes), the LSpoiler is empty.
  //If rebuilding by spoiler click, the LSpoiler already contains all items.
  //Anyway, we need to check if spoiler exists because it could already exists
  //even at first building if there are multiple spoilers with same name.

  Spoiler := Lb.Spoilers.Find(Ident);
  if Spoiler=nil then
  begin
    Spoiler := TDHSpoiler.Create(Ident, StartExpanded);
    Lb.Spoilers.Add(Spoiler);
  end;

  Builder.CurrentLink := Spoiler;
end;

{ TDHToken_SpoilerDetail }

procedure TDHToken_SpoilerDetail.ReadParam;
begin
  Ident := Param;
end;

procedure TDHToken_SpoilerDetail.Process;
begin
  //do nothing
end;

{ TDHToken_SuperOrSubScript }

procedure TDHToken_SuperOrSubScript.Process;
var
  Delta, ParentHeight: TPixels;
begin
  ParentHeight := Canvas.TextHeight(STR_SPACE);

  if not Props.SS_Inside then
    Props.SS_FullHeight := ParentHeight;
  Props.SS_Inside := True;

  Canvas.Font.Size := RoundIfVCL(Canvas.Font.Size * 0.75); //Font auto scaled!

  if Self is TDHToken_Superscript then Delta := 0 else
  if Self is TDHToken_Subscript then Delta := ParentHeight - Canvas.TextHeight(STR_SPACE) else
    raise EDHInternalExcept.Create('Invalid Super or Sub Script class');

  Props.SS_YPos := Props.SS_YPos + Delta;
end;

{ TDHToken_List }

procedure TDHToken_List.Process;
begin
  Inc(Props.List_Level);
  Props.List_Number := 0;
  Props.List_Bullet := Self is TDHToken_UnorderedList;
end;

{ TDHToken_ListItem }

function TDHToken_ListItem.IsBreakableToken: Boolean;
begin
  Result := True;
end;

function TDHToken_ListItem.IsBypassProps: Boolean;
begin
  Result := True;
end;

procedure TDHToken_ListItem.Process;
var
  Token: TDHToken_Word;
  Line: TDHDivAreaLine;
begin
  Line := Builder.CurrentDiv.Lines.Last;
  if Line.Items.Count>0 then //line already contains items
    Builder.JumpLine(True);

  Inc(Props.List_Number);

  Token := TDHToken_Word.Create;
  try
    Token.Builder := Builder;

    if Props.List_Bullet then
      Token.Word := '• '
    else
      Token.Word := IntToStr(Props.List_Number)+'. ';

    Builder.CurrentDiv.Point.X := Props.List_Level * Lb.CalcScale(Lb.ListLevelPadding);
    Token.Breakable := True;
    Token.Process;
  finally
    Token.Free;
  end;

  Builder.CurrentDiv.SetLeftMargin(True, Builder.CurrentDiv.Point.X); //to next lines
end;

{ TDHToken_LineSpace }

procedure TDHToken_LineSpace.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Space := Lb.CalcScale(StrToPixels(P.GetFirstParam, 0));
    ParagraphSpace := Lb.CalcScale(P.GetParamAsPixels('par', 0));
  finally
    P.Free;
  end;
end;

procedure TDHToken_LineSpace.Process;
begin
  Props.LineSpace := Space;
  Props.ParagraphSpace := ParagraphSpace;

  Builder.ApplyLineSpace;
end;

{ TDHToken_ParagraphIndent }

procedure TDHToken_ParagraphIndent.ReadParam;
begin
  Indent := Lb.CalcScale(StrToPixels(Param, 0));
end;

procedure TDHToken_ParagraphIndent.Process;
begin
  Props.ParagraphIndent := Indent;

  if not Builder.CurrentDiv.Lines.Last.TabUsed then
    Builder.ApplyLineMargin;
end;

{ TDHToken_Div }

function TDHToken_Div.IsBreakableToken: Boolean;
begin
  Result := True;
end;

procedure TDHToken_Div.ReadParam;
var
  P: TDHMultipleTokenParams;
  AllMargin, AllThick, AllPad: TPixels;
  AllLnColor: TAnyColor;

  procedure UpdVar(var &Var: TPixels; All, Value: TPixels);
  begin
    if Value >= 0 then
      &Var := Value
    else
      &Var := All;

    &Var := Lb.CalcScale(&Var);
  end;

  procedure ReadBorder(var B: TDHBorderRec; const Ident: string);
  var
    LnColor: TAnyColor;
  begin
    UpdVar(B.Margin, AllMargin, P.GetParamAsPixels('margin_'+Ident, -1));
    UpdVar(B.Thick, AllThick, P.GetParamAsPixels('thick_'+Ident, -1));
    UpdVar(B.Pad, AllPad, P.GetParamAsPixels('pad_'+Ident, -1));

    LnColor := P.GetParamAsColor('lncolor_'+Ident);
    if LnColor<>clNone then
      B.LnColor := LnColor
    else
      B.LnColor := AllLnColor;

    B.Margin := B.Margin + B.Thick + B.Pad;
  end;

  function DetectSizeType(const Value: string; var SizeType: TDHDivSizeType; AllowLine: Boolean): TPixels;
  var
    A: string;
    Px: TPixels;
  begin
    Result := 0;

    if SameText(Value, 'full') then SizeType := TDHDivSizeType.Full else
    if AllowLine and SameText(Value, 'line') then SizeType := TDHDivSizeType.Line else
    begin
      if Value.EndsWith('%') then SizeType := TDHDivSizeType.Percent else
      if Value.EndsWith('#') then SizeType := TDHDivSizeType.Inner else
      if Value.EndsWith('-') then SizeType := TDHDivSizeType.Reverse else
        SizeType := TDHDivSizeType.Outer;

      A := Value;
      if SizeType <> TDHDivSizeType.Outer then Delete(A, A.Length, 1);

      Px := StrToPixels(A, 0);
      if Px>0 then
      begin
        if SizeType <> TDHDivSizeType.Percent then Px := Lb.CalcScale(Px);
        Result := Px;
      end
      else
        SizeType := TDHDivSizeType.Auto;
    end;
  end;

var
  OldAr: TArray<string>;
begin
  if Self is TDHToken_Float then //old FLOAT tag
  begin
    OldAr := Param.Split([',']);
    if Length(OldAr)>=2 then
    begin
      FloatPos := TAnyPoint.Create(Lb.CalcScale(StrToPixels(OldAr[0], 0)), Lb.CalcScale(StrToPixels(OldAr[1], 0)));
      Floating := True;

      if Length(OldAr)>=3 then
        Size.Width := Lb.CalcScale(StrToPixels(OldAr[2], 0));
    end
      else ValidParam := False;

    if Size.Width>0 then WidthType := TDHDivSizeType.Outer else WidthType := TDHDivSizeType.Auto;
    HeightType := TDHDivSizeType.Auto;

    HorzAlign := haLeft;
    VertAlign := vaTop;
    KeepProps := True;
    BackColor := clNone;
    BorderColor := clNone;

    Exit;
  end;

  P := TDHMultipleTokenParams.Create(Param);
  try
    Size.Width := DetectSizeType(P.GetParam('width'), WidthType, False);
    Size.Height := DetectSizeType(P.GetParam('height'), HeightType, True);

    //--Borders
    AllMargin := P.GetParamAsPixels('margin', 0);
    AllThick := P.GetParamAsPixels('thick', 0);
    AllPad := P.GetParamAsPixels('pad', 0);
    AllLnColor := P.GetParamAsColor('lncolor');

    ReadBorder(Borders.Left, 'left');
    ReadBorder(Borders.Top, 'top');
    ReadBorder(Borders.Right, 'right');
    ReadBorder(Borders.Bottom, 'bottom');

    //

    if WidthType=TDHDivSizeType.Inner then Size.Width := Size.Width + Borders.Left.Margin + Borders.Right.Margin;
    if HeightType=TDHDivSizeType.Inner then Size.Height := Size.Height + Borders.Top.Margin + Borders.Bottom.Margin;

    //--

    FloatPos := TAnyPoint.Create(Lb.CalcScale(P.GetParamAsPixels('x', -1)), Lb.CalcScale(P.GetParamAsPixels('y', -1)));
    Floating := (FloatPos.X>=0) and (FloatPos.Y>=0);

    HorzAlign := ParamToHorzAlign(P.GetParam('align'));
    VertAlign := ParamToVertAlign(P.GetParam('valign'));
    MaxWidth := Lb.CalcScale(P.GetParamAsPixels('maxwidth', 0));
    BackColor := P.GetParamAsColor('color');
    BorderColor := P.GetParamAsColor('outcolor');
    KeepProps := P.ParamExists('holdprops');
    FloatingBehind := P.ParamExists('behind');
    CornerRadius := Lb.CalcScale(P.GetParamAsPixels('radius', 0));
  finally
    P.Free;
  end;
end;

procedure TDHToken_Div.Process;

  procedure ToDivLineAttr(var A: TDHBorderRec; var Attr: TDHDivBorderLineAttrRec);
  begin
    Attr.Thick := A.Thick;
    Attr.Pad := A.Pad;
    Attr.Color := A.LnColor;
  end;

var
  D: TDHDivArea;
  V: TDHVisualItem_Div;
  Item: TDHPreVisualItem;
  Sz: TPixels;
begin
  if not Builder.CurrentDiv.AutoWidth and
    (WidthType in [TDHDivSizeType.Full, TDHDivSizeType.Reverse, TDHDivSizeType.Percent]) then
  begin
    Sz := Builder.CurrentDiv.GetAreaSizeWOB.Width;
    case WidthType of
      TDHDivSizeType.Full: Size.Width := Sz - Builder.CurrentDiv.Point.X;
      TDHDivSizeType.Reverse: Size.Width := Sz - Builder.CurrentDiv.Point.X - Size.Width;
      TDHDivSizeType.Percent: Size.Width := RoundIfVCL(Sz * Size.Width/100);
    end;
  end;
  if not Builder.CurrentDiv.AutoHeight and
    (HeightType in [TDHDivSizeType.Full, TDHDivSizeType.Reverse, TDHDivSizeType.Percent]) then
  begin
    Sz := Builder.CurrentDiv.GetAreaSizeWOB.Height;
    case HeightType of
      TDHDivSizeType.Full: Size.Height := Sz - Builder.CurrentDiv.Point.Y;
      TDHDivSizeType.Reverse: Size.Height := Sz - Builder.CurrentDiv.Point.Y - Size.Height;
      TDHDivSizeType.Percent: Size.Height := RoundIfVCL(Sz * Size.Height/100);
    end;
  end;

  D := TDHDivArea.Create(Builder.CurrentDiv);
  D.FixedSize := Size;
  D.AutoWidth := WidthType=TDHDivSizeType.Auto;
  D.AutoHeight := (HeightType=TDHDivSizeType.Auto) or (HeightType=TDHDivSizeType.Line);
  D.MaxWidth := MaxWidth;
  D.Borders := Borders;
  D.VertAlign := VertAlign;
  D.HorzAlign := HorzAlign;
  D.HeightByLine := HeightType=TDHDivSizeType.Line;
  D.FloatingBehind := FloatingBehind;

  V := TDHVisualItem_Div.Create;
  V.InnerColor := BackColor;
  V.OuterColor := BorderColor;
  V.CornerRadius := CornerRadius;

  ToDivLineAttr(Borders.Left, V.Left);
  ToDivLineAttr(Borders.Top, V.Top);
  ToDivLineAttr(Borders.Right, V.Right);
  ToDivLineAttr(Borders.Bottom, V.Bottom);

  Item := Builder.CreatePreVisualItem(V, TAnySize.Create(0, 0)); //we don't know the size yet
  if Floating then
  begin
    Item.Position := FloatPos;
    Builder.CurrentDiv.FloatingDivs.Add(Item);
  end;
  Item.SelfDiv := D;

  D.PreVisualItem := Item;

  if not KeepProps then
  begin
    Props.Offset.Top := 0;
    Props.Offset.Bottom := 0;
    Props.BackColor := clNone;
    Props.HorzAlign := Builder.GetDefaultHorzTextAlign;
    Props.VertAlign := vaTop;
    Props.LineSpace := 0;
    Props.ParagraphSpace := 0;
    Props.ParagraphIndent := 0;
  end;

  Builder.CurrentDiv := D; //change current div!
  Builder.AddNewLineObject(False); //always start with one line
end;

{$ENDREGION}

{ TDHOffsetRec }

function TDHOffsetRec.GetHeight: TPixels;
begin
  Result := Top + Bottom;
end;

{ TDHPreVisualItem }

destructor TDHPreVisualItem.Destroy;
begin
  if VisualObject<>nil then VisualObject.Free;
  if SelfDiv<>nil then SelfDiv.Free;

  inherited;
end;

function TDHPreVisualItem.GetFullHeight: TPixels;
begin
  Result := Size.Height + Offset.GetHeight;
end;

function TDHPreVisualItem.IsSpace: Boolean;
begin
  Result := (VisualObject is TDHVisualItem_Word)
    and (TDHVisualItem_Word(VisualObject).Text = STR_SPACE);
end;

{ TDHDivArea }

constructor TDHDivArea.Create(Parent: TDHDivArea);
begin
  inherited Create;
  Lines := TObjectList<TDHDivAreaLine>.Create;
  FloatingDivs := TDHPreVisualItemList.Create;

  Self.Parent := Parent;
end;

destructor TDHDivArea.Destroy;
begin
  Lines.Free;
  FloatingDivs.Free;
  inherited;
end;

function TDHDivArea.GetAbsoluteStartingPos: TAnyPoint;
begin
  Result := TAnyPoint.Create(Borders.Left.Margin, Borders.Top.Margin);
  if Main then Exit;

  Result.Offset(PreVisualItem.Position);
  Result.Offset(Parent.GetAbsoluteStartingPos);
end;

function TDHDivArea.GetAreaSize: TAnySize;
var
  W, H: TPixels;
begin
  if AutoWidth then W := TextSize.Width + GetHorzBorder else W := FixedSize.Width;
  if AutoHeight then H := TextSize.Height + GetVertBorder else H := FixedSize.Height;

  Result := TAnySize.Create(W, H);
end;

function TDHDivArea.GetAreaSizeWOB: TAnySize;
begin
  //area size without borders
  Result := GetAreaSize;
  Result.Width := Result.Width - GetHorzBorder;
  Result.Height := Result.Height - GetVertBorder;
end;

function TDHDivArea.GetHorzBorder: TPixels;
begin
  Result := Borders.Left.Margin + Borders.Right.Margin;
end;

function TDHDivArea.GetVertBorder: TPixels;
begin
  Result := Borders.Top.Margin + Borders.Bottom.Margin;
end;

function TDHDivArea.GetLastItem: TDHPreVisualItem;
var
  Line: TDHDivAreaLine;
begin
  Line := Lines.Last;
  if Line.Items.Count>0 then
    Exit(Line.Items.Last);

  Result := nil;
end;

procedure TDHDivArea.CalcTextSize;
var
  Line: TDHDivAreaLine;
  W, H: TPixels;
  Item: TDHPreVisualItem;
begin
  W := 0;
  H := Point.Y;

  for Line in Lines do
  begin
    SetWhenBigger(W, Line.TextSize.Width); //high width
  end;

  for Item in FloatingDivs do
  begin
    SetWhenBigger(W, Item.Position.X + Item.Size.Width);
    SetWhenBigger(H, Item.Position.Y + Item.Size.Height);
  end;

  TextSize := TAnySize.Create(W, H);
end;

function TDHDivArea.GetParagraphCount: Integer;
var
  Line: TDHDivAreaLine;
begin
  Result := 0;
  for Line in Lines do
    if not Line.Continuous then Inc(Result);
end;

procedure TDHDivArea.SetLeftMargin(Flag: Boolean; Value: TPixels);
begin
  LeftMarginOn := Flag;
  LeftMargin := Value;
end;

function TDHDivArea.ClearLinesIfNoItems: Boolean;
begin
  if (Lines.Count=1) and (Lines[0].Items.Count=0) then
  begin
    Lines.Clear;
    Exit(True);
  end;

  Result := False;
end;

{ TDHDivAreaLine }

constructor TDHDivAreaLine.Create;
begin
  inherited;
  Items := TDHPreVisualItemList.Create;
end;

destructor TDHDivAreaLine.Destroy;
begin
  Items.Free;
  inherited;
end;

function TDHDivAreaLine.GetHighHeight: TPixels;
var
  H: TPixels;
  Item: TDHPreVisualItem;
begin
  H := 0;

  for Item in Items do
  begin
    SetWhenBigger(H, Item.GetFullHeight);
  end;

  Result := H;
end;

{ TDHPropsStore }

procedure TDHPropsStore.AssignProps(Source: TDHPropsStore);
begin
  FontColor := Source.FontColor;
  BackColor := Source.BackColor;

  Offset := Source.Offset;

  HorzAlign := Source.HorzAlign;
  VertAlign := Source.VertAlign;

  SS_Inside := Source.SS_Inside;
  SS_FullHeight := Source.SS_FullHeight;
  SS_YPos := Source.SS_YPos;

  List_Level := Source.List_Level;
  List_Number := Source.List_Number;
  List_Bullet := Source.List_Bullet;

  LineSpace := Source.LineSpace;
  ParagraphSpace := Source.ParagraphSpace;
  ParagraphIndent := Source.ParagraphIndent;
end;

{ TDHPreVisualItemList }

function TDHPreVisualItemList.GetSumWidth: TPixels;
var
  Item: TDHPreVisualItem;
begin
  Result := 0;
  for Item in Self do
    Result := Result + Item.Size.Width;
end;

{ TDHBuilder }

constructor TDHBuilder.Create(Lb: TDzHTMLText; Canvas: TCanvas;
  VisualItems: TDHVisualItemList; ProcBoundsAndLines: TDHProcBoundsAndLines);
begin
  inherited Create;

  Self.Lb := Lb;
  Self.Canvas := Canvas;
  Self.VisualItems := VisualItems;
  Self.ProcBoundsAndLines := ProcBoundsAndLines;

  QueueVisualItems := TDHPreVisualItemList.Create;
  QueueProps := TDHPropsStore.Create;

  MainToken := TDHToken_Main.Create;

  //--Canvas
  Canvas.Font.Assign(Lb.Font);
  //--

  //--Props
  Props := TDHPropsStore.Create;
  Props.FontColor := Lb.{$IFDEF FMX}FontColor{$ELSE}Font.Color{$ENDIF};
  Props.BackColor := clNone;

  Props.Offset.Top := Lb.CalcScale(Lb.Offset.Top);
  Props.Offset.Bottom := Lb.CalcScale(Lb.Offset.Bottom);

  Props.VertAlign := Lb.LineVertAlign;
  Props.HorzAlign := GetDefaultHorzTextAlign;

  Props.LineSpace := Lb.CalcScale(Lb.LineSpacing);
  Props.ParagraphSpace := Lb.CalcScale(Lb.ParagraphSpacing);
  Props.ParagraphIndent := Lb.CalcScale(Lb.ParagraphIndent);
  //--

  //--MainDiv
  MainDiv := TDHDivArea.Create(nil);
  MainDiv.Main := True;

  MainDiv.AutoWidth := Lb.AutoWidth;
  MainDiv.AutoHeight := Lb.AutoHeight;

  MainDiv.MaxWidth := Lb.CalcScale(Lb.MaxWidth);

  MainDiv.FixedSize := TAnySize.Create(Lb.Width, Lb.Height); //control size is auto scaled

  MainDiv.HorzAlign := Lb.OverallHorzAlign;
  MainDiv.VertAlign := Lb.OverallVertAlign;

  MainDiv.Borders.Left.Margin := Lb.CalcScale(Lb.Borders.Left);
  MainDiv.Borders.Top.Margin := Lb.CalcScale(Lb.Borders.Top);
  MainDiv.Borders.Right.Margin := Lb.CalcScale(Lb.Borders.Right);
  MainDiv.Borders.Bottom.Margin := Lb.CalcScale(Lb.Borders.Bottom);
  //--
end;

destructor TDHBuilder.Destroy;
begin
  QueueVisualItems.Free;
  QueueProps.Free;
  Props.Free;
  MainToken.Free;
  MainDiv.Free;

  inherited;
end;

procedure TDHBuilder.Execute;
begin
  CurrentBlock := MainToken;
  ReadTokens;
  CheckMissingClosingTags;
  CurrentBlock := nil;

  CurrentDiv := MainDiv;
  AddNewLineObject(False); //always start with one line
  ProcessChildrenTokens(MainToken);
  ProcessPendingObjects; //process remaining objects in queue list
  if CurrentDiv<>MainDiv then raise EDHInternalExcept.Create('Incorrect final div');
  if ((Lb.Lines.Count=1) and Lb.Lines[0].IsEmpty and Lb.AutoBreak) //allow one blank line
    or not CurrentDiv.ClearLinesIfNoItems then EndOfLine;
  CurrentDiv := nil;

  MainDiv.CalcTextSize;
  ProcBoundsAndLines(
    MainDiv.TextSize, MainDiv.GetAreaSize,
    MainDiv.Lines.Count, MainDiv.GetParagraphCount);

  SendObjectsToComponent(MainDiv);
end;

procedure TDHBuilder.CheckMissingClosingTags;
var
  Block: TDHTokenBlock;
begin
  Block := CurrentBlock;
  while Block<>MainToken do
  begin
    AddInvalidToken(0, Format('Missing closing tag <%s>',
      [GetTokenIdentFromClass(TDHTokenClass(Block.ClassType)).ToLower]));
    Block := Block.Parent;
  end;
end;

function TDHBuilder.GetDefaultHorzTextAlign: TDHHorzAlign;
begin
  Result := Lb.LineHorzAlign;
  if Lb.RightToLeftText and (Result = haLeft) then Result := haRight;
end;

procedure TDHBuilder.AddInvalidToken(Position: Integer; const ErrorDescription: string);
var
  Token: TDHToken_Word;
begin
  Token := AddToken<TDHToken_Word>;
  Token.Word := '<?>';
  Token.Breakable := True;

  Lb.SyntaxErrors.Add(TDHSyntaxError.Create(Position, ErrorDescription));
end;

function TDHBuilder.AddToken<T>: T;
begin
  Result := T.Create;
  Result.Init(Self);
end;

{$REGION 'Token reading'}
procedure TDHBuilder.ReadTokens;
const NBR_TAG = '<NBR>';
var
  Text, Tag, TagResult: string;
  CharIni: Char;
  I, CurPos, Len: Integer;
  BreakableChar, Nbr: Boolean;
begin
  Text := Lb.Text;
  CurPos := 1;
  Len := Text.Length;
  while CurPos <= Len do
  begin
    CharIni := Text[CurPos];

    if CharIni = '<' then //starts with tag opening
    begin
      I := PosEx('>', Text, CurPos+1); //find tag closing
      if I>0 then
      begin
        Tag := Copy(Text, CurPos+1, I-CurPos-1);
        TagResult := ProcessTag(Tag);
        if TagResult<>'OK' then AddInvalidToken(CurPos, Format('<%s> - %s', [Tag.ToLower, TagResult]));
        CurPos := I+1;
      end else
      begin
        //losted tag opening
        AddInvalidToken(CurPos, 'Char "<" left alone');
        Inc(CurPos);
      end;
    end else
    if CharIni = '>' then
    begin
      //losted tag closing
      AddInvalidToken(CurPos, 'Char ">" left alone');
      Inc(CurPos);
    end else
    if (CharIni = #13) or (CharIni = #10) then
    begin
      Inc(CurPos);
      if Text[CurPos]=#10 then Inc(CurPos); //when #13#10 sequence

      Nbr := UpperCase(Copy(Text, CurPos, NBR_TAG.Length)) = NBR_TAG;
      if Nbr then Inc(CurPos, NBR_TAG.Length);

      AddToken<TDHToken_Break>.NoBreak := Nbr or not Lb.AutoBreak;
    end else
    begin //all the rest is text
      I := TDHCharUtils.FindNextWordBreakChar(Text, CurPos);
      BreakableChar := (I=CurPos);
      //when word break at first char, let add the char itself alone.
      //when word break at other next chars, consider until char before word-break char.
      if BreakableChar then Inc(I) else
        if I=0 then I := Len+1;

      with AddToken<TDHToken_Word> do
      begin
        Word := TDzHTMLText.UnescapeHTMLToText(Copy(Text, CurPos, I-CurPos));
        Breakable := BreakableChar and not TDHCharUtils.IsPunctuationChar(CharIni);
      end;
      
      CurPos := I;
    end;
  end;
end;

function TDHBuilder.ProcessTag(const Tag: string): string;
var
  CloseTag, HasPar, Block: Boolean;
  A, Par: string;
  I: Integer;
  Def: TDHTokenObjectDef;
  TokenClass: TDHTokenClass;
  Token: TDHToken;
begin
  Result := 'unknown';
  A := Tag;

  CloseTag := A.StartsWith('/');
  if CloseTag then Delete(A, 1, 1);

  HasPar := SplitStr(A, ':', A, Par);
  if HasPar then
  begin
    if Par.IsEmpty then Exit('Blank parameter specified');
    if CloseTag then Exit('Parameter not allowed in closing tag');
  end;

  if A.IsEmpty then Exit('Blank tag');

  I := GetTokenObjectDefIndexFromIdent(UpperCase(A));
  if I = -1 then Exit('Invalid tag');

  Def := TOKENS_OBJECTS[I];
  TokenClass := Def.Clazz;
  Block := TokenClass.InheritsFrom(TDHTokenBlock);

  if CloseTag then
  begin
    if not Block then Exit('Single tag does not allow closing tag');
    if CurrentBlock.ClassType <> TokenClass then Exit('Not the current tag to close');

    CurrentBlock := CurrentBlock.Parent;
  end else
  begin
    if (not Def.AllowPar) and (HasPar) then Exit('Parameter not allowed');
    if (Def.AllowPar) and (not Def.OptionalPar) and (not HasPar) then Exit('Parameter required');

    Token := TokenClass.Create;
    Token.Init(Self);
    if Def.AllowPar then
    begin
      Token.Param := Par;
      Token.ValidParam := True;
      Token.ReadParam;
      if not Token.ValidParam then
      begin
        CurrentBlock.Children.Remove(Token);
        Exit('Invalid parameter');
      end;
    end;

    if Block then CurrentBlock := TDHTokenBlock(Token);
  end;

  Result := 'OK';
end;
{$ENDREGION}

procedure TDHBuilder.ProcessChildrenTokens(Block: TDHTokenBlock);
var
  Token: TDHToken;
  Original: TDHPropsStore; OriginalFont: TFont;
  Spoiler: TDHSpoiler;
  DivArea: TDHDivArea;
begin
  for Token in Block.Children do
  begin
    if Token is TDHToken_SpoilerDetail then
    begin
      Spoiler := Lb.Spoilers.Find(TDHToken_SpoilerDetail(Token).Ident);
      if (Spoiler=nil) or not Spoiler.Expanded then Continue;
    end;

    if Token.IsBreakableToken then
    begin
      ProcessPendingObjects;
      LastNoBreak := False;
    end;

    if Token is TDHTokenBlock then
    begin
      Original := TDHPropsStore.Create;
      OriginalFont := TFont.Create;
      try
        Original.AssignProps(Props);
        OriginalFont.Assign(Canvas.Font);

        Token.Process;
        ProcessChildrenTokens(TDHTokenBlock(Token));

        if Token.IsBreakableToken then ProcessPendingObjects;

        if not TDHTokenBlock(Token).IsBypassProps then
        begin
          Props.AssignProps(Original);
          Canvas.Font.Assign(OriginalFont);
        end;
      finally
        Original.Free;
        OriginalFont.Free;
      end;

      if Token is TDHToken_Div then //closing Div token
      begin
        EndOfLine;

        DivArea := CurrentDiv;
        CurrentDiv := CurrentDiv.Parent;

        DivArea.CalcTextSize;
        DivArea.PreVisualItem.Size := DivArea.GetAreaSize; //update visual item size

        if not TDHToken_Div(Token).Floating then
          ProcessOneObject(DivArea.PreVisualItem);
      end else
      if (Token is TDHToken_Link) or (Token is TDHToken_SpoilerTitle) then CurrentLink := nil; //closing Link or Spoiler Title token
    end
    else
      Token.Process;
  end;
end;

function TDHBuilder.CalcTextHeight(const Text: string): TPixels;
begin
  if Props.SS_Inside then
    Result := Props.SS_FullHeight
  else
    Result := Canvas.TextHeight(Text);
end;

function TDHBuilder.AddNewLineObject(Continuous: Boolean): TDHDivAreaLine;
begin
  Result := TDHDivAreaLine.Create;
  Result.Continuous := Continuous;
  Result.YPos := CurrentDiv.Point.Y;
  CurrentDiv.Lines.Add(Result);

  ApplyLineSpace;
  ApplyLineMargin;
end;

procedure TDHBuilder.EndOfLine;
var
  Line: TDHDivAreaLine;
begin
  Line := CurrentDiv.Lines.Last;
  Line.TextSize := TAnySize.Create(CurrentDiv.Point.X, Line.GetHighHeight);
  if Line.Items.Count=0 then //line without visual items
    Line.TextSize.Height := CalcTextHeight(STR_SPACE) + Props.Offset.GetHeight;

  CurrentDiv.Point.Offset(0, Line.TextSize.Height);
end;

function TDHBuilder.JumpLine(Continuous: Boolean): TDHDivAreaLine;
begin
  EndOfLine; //must always contains lines here

  if not Continuous and (Props.List_Level=0) then
    CurrentDiv.SetLeftMargin(False);

  Result := AddNewLineObject(Continuous);
end;

procedure TDHBuilder.ApplyLineMargin;
var
  X: TPixels;
  Line: TDHDivAreaLine;
begin
  Line := CurrentDiv.Lines.Last;
  if Line.Items.Count>0 then Exit; //only apply once per line by items

  X := 0;
  if CurrentDiv.LeftMarginOn then
    X := CurrentDiv.LeftMargin
  else
    if not Line.Continuous then X := Props.ParagraphIndent;

  CurrentDiv.Point.X := X; //do not use offset because may apply multiple times
end;

procedure TDHBuilder.ApplyLineSpace;
var
  Line: TDHDivAreaLine;
  Space: TPixels;
begin
  if CurrentDiv.Lines.Count<=1 then Exit; //only apply space after second line

  Line := CurrentDiv.Lines.Last;
  if Line.Items.Count>0 then Exit; //only apply once per line by items

  Space := Props.LineSpace;
  if not Line.Continuous then
    Space := Space + Props.ParagraphSpace;

  CurrentDiv.Point.Y := Line.YPos + Space; //do not use offset because may apply multiple times
end;

function TDHBuilder.CreatePreVisualItem(V: TDHVisualItem; Size: TAnySize): TDHPreVisualItem;
begin
  V.Link := CurrentLink;

  if V is TDHVisualItem_Div then
    V.BColor := clNone //div handles background color using specific method
  else
    V.BColor := Props.BackColor;

  Result := TDHPreVisualItem.Create;
  Result.VisualObject := V;
  Result.Size := Size;
  Result.HorzAlign := Props.HorzAlign;
  Result.VertAlign := Props.VertAlign;
  Result.Offset := Props.Offset;
end;

procedure TDHBuilder.AddVisualItemToQueue(V: TDHVisualItem; Size: TAnySize; Breakable: Boolean);
begin
  //if breakable, process pending itens, and then process breakable item immediately
  if Breakable then ProcessPendingObjects;

  if QueueVisualItems.Count=0 then QueueProps.AssignProps(Props); //store first item props
  QueueVisualItems.Add(CreatePreVisualItem(V, Size));

  if Breakable then ProcessPendingObjects;
end;

procedure TDHBuilder.ProcessPendingObjects;
var
  OriginalProps: TDHPropsStore;
begin
  if QueueVisualItems.Count=0 then Exit;

  //when processing queue items, props used to new line must be before first item, and not current props!
  OriginalProps := Props;

  Props := QueueProps;
  ProcessSpecificObjects(QueueVisualItems);
  Props := OriginalProps;
end;

procedure TDHBuilder.ProcessOneObject(Item: TDHPreVisualItem);
var
  List: TDHPreVisualItemList;
begin
  List := TDHPreVisualItemList.Create;
  try
    List.Add(Item);
    ProcessSpecificObjects(List);
  finally
    List.Free;
  end;
end;

procedure TDHBuilder.ProcessSpecificObjects(List: TDHPreVisualItemList);
var
  MaxW: TPixels;
  Line: TDHDivAreaLine;
  Item: TDHPreVisualItem;
  PrevSpaceRemoved: Boolean;
begin
  PrevSpaceRemoved := False;

  Line := CurrentDiv.Lines.Last;
  if Line.Items.Count>0 then
  begin
    if CurrentDiv.AutoWidth then
      MaxW := CurrentDiv.MaxWidth
    else
      MaxW := CurrentDiv.FixedSize.Width;

    if MaxW > 0 then
    begin
      MaxW := MaxW - CurrentDiv.GetHorzBorder;
      if CurrentDiv.Point.X + List.GetSumWidth > MaxW then //out of width bound
      begin
        Item := Line.Items.Last;
        if Item.IsSpace then //remove previous SPACE if is the last item in the line
        begin
          CurrentDiv.Point.Offset(-Item.Size.Width, 0); //rollback current point
          Line.Items.Remove(Item);
          PrevSpaceRemoved := True;
        end;
        Line := JumpLine(True);
      end;
    end;
  end;

  while List.Count>0 do
  begin
    Item := List.First;
    List.Extract(Item); //Lazarus does not support ExtractAt

    if not PrevSpaceRemoved and Line.Continuous and Item.IsSpace and (Line.Items.Count=0) then
    begin
      //skip SPACE if is the first item in the line
      Item.Free;
      Continue;
    end;

    Line.Items.Add(Item);

    Item.Position := CurrentDiv.Point;
    Item.Position.Offset(0, Item.Offset.Top);
    CurrentDiv.Point.Offset(Item.Size.Width, 0);
  end;
end;

procedure TDHBuilder.SendObjectsToComponent(DivArea: TDHDivArea);
var
  PDiv: TAnyPoint;
  Item: TDHPreVisualItem;

  procedure DoItem;
  var
    P: TAnyPoint;
  begin
    P := PDiv;
    P.Offset(Item.Position);
    Item.VisualObject.Rect := TAnyRect.Create(P, Item.Size.Width, Item.Size.Height);

    VisualItems.Add(Item.VisualObject);
    Item.VisualObject := nil;

    if Item.SelfDiv<>nil then
      SendObjectsToComponent(Item.SelfDiv);
  end;

  procedure ProcessFloatingDivs(Behind: Boolean);
  var
    DivItem: TDHPreVisualItem;
  begin
    for DivItem in DivArea.FloatingDivs do
    begin
      if DivItem.SelfDiv.FloatingBehind <> Behind then Continue;

      Item := DivItem;
      DoItem;
    end;
  end;

var
  Line: TDHDivAreaLine;
  WrPlainText: Boolean;
begin
  PDiv := DivArea.GetAbsoluteStartingPos;

  ProcessFloatingDivs(True);

  WrPlainText := Lb.GeneratePlainText and (DivArea = MainDiv);

  for Line in DivArea.Lines do
  begin
    for Item in Line.Items do
    begin
      if (Item.SelfDiv<>nil) and Item.SelfDiv.HeightByLine then
      begin
        Item.Size.Height := Line.TextSize.Height - Item.Offset.GetHeight; //size of div rectangle

        Item.SelfDiv.AutoHeight := False;
        Item.SelfDiv.FixedSize.Height := Item.Size.Height; //size of div area for align children
      end;

      if WrPlainText and (Item.VisualObject is TDHVisualItem_Word) then
        Lb.PlainText.Append(TDHVisualItem_Word(Item.VisualObject).Text);

      CheckAlign(Item, Line, DivArea);
      DoItem;
    end;

    if WrPlainText then Lb.PlainText.Append(sLineBreak);
  end;

  ProcessFloatingDivs(False);
end;

procedure TDHBuilder.CheckAlign(Item: TDHPreVisualItem; Line: TDHDivAreaLine; DivArea: TDHDivArea);
type
  TFuncAlignResult = record
    Outside, Inside: TPixels;
  end;

  function FuncAlignHorz: TFuncAlignResult;
  var
    DivLim: TPixels;
  begin
    if DivArea.HorzAlign in [haCenter, haRight] then
      DivLim := DivArea.TextSize.Width
    else
      DivLim := DivArea.GetAreaSizeWOB.Width;

    Result.Outside := DivLim;
    Result.Inside := Line.TextSize.Width;
  end;

  function FuncAlignVert: TFuncAlignResult;
  begin
    Result.Outside := Line.TextSize.Height;
    Result.Inside := Item.GetFullHeight;
  end;

  function FuncDivAlignHorz: TFuncAlignResult;
  begin
    Result.Outside := DivArea.GetAreaSizeWOB.Width;
    Result.Inside := DivArea.TextSize.Width;
  end;

  function FuncDivAlignVert: TFuncAlignResult;
  begin
    Result.Outside := DivArea.GetAreaSizeWOB.Height;
    Result.Inside := DivArea.TextSize.Height;
  end;

  procedure Check(FnIndex: Byte; Horz: Boolean; Prop: Variant);
  var
    R: TFuncAlignResult;
    P: TAnyPoint;
    Offset: TPixels;
  begin
    if Prop>0 then //center or right
    begin
      case FnIndex of
        0: R := FuncAlignHorz;
        1: R := FuncAlignVert;
        2: R := FuncDivAlignHorz;
        3: R := FuncDivAlignVert;
      end;

      Offset := R.Outside - R.Inside;
      if Prop=1 then Offset := RoundIfVCL(Offset / 2); //center

      P := TAnyPoint.Create(0, 0);
      if Horz then
        P.X := Offset
      else
        P.Y := Offset;

      Item.Position.Offset(P);
    end;
  end;

begin
  if Lb.RightToLeftText then
    Item.Position.X := Line.TextSize.Width - (Item.Position.X + Item.Size.Width);

  Check(0, True, Item.HorzAlign);
  Check(1, False, Item.VertAlign);

  Check(2, True, DivArea.HorzAlign);
  Check(3, False, DivArea.VertAlign);
end;

end.
