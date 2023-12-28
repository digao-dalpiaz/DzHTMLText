{$IFNDEF FMX}unit Vcl.DHTokenEngine;{$ENDIF}

{$INCLUDE Defines.inc}

interface

uses
{$IFDEF FMX}FMX.DzHTMLText{$ELSE}Vcl.DzHTMLText{$ENDIF},
{$IFDEF FPC}
  Classes, Types
{$ELSE}
  System.Classes, System.Generics.Collections, System.Types, System.UITypes,
  {$IFDEF FMX}
    {$IFDEF USE_NEW_UNITS}FMX.Graphics, {$ENDIF}System.UIConsts
  {$ELSE}
    Vcl.Graphics
  {$ENDIF}
{$ENDIF};

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
    procedure Process; override;
    function IsBreakableToken: Boolean; override;
  end;

  TDHToken_Word = class(TDHTokenSingle)
  private
    Word: string;
    Breakable: Boolean;

    procedure Process; override;
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
  {$ENDREGION}

  {$REGION 'Token block classes'}
  TDHToken_Main = class(TDHTokenBlock);

  TDHToken_FontStyleItem = class(TDHTokenBlock)
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

  TDHToken_Color = class(TDHTokenBlock)
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

  TDHToken_Spoiler = class(TDHTokenBlock)
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

  TDHToken_SuperOrSubScript = class(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_Superscript = class(TDHToken_SuperOrSubScript);
  TDHToken_Subscript = class(TDHToken_SuperOrSubScript);

  TDHToken_List = class(TDHTokenBlock)
  private
    procedure Process; override;
  end;
  TDHToken_OrderedList = class(TDHToken_List);
  TDHToken_UnorderedList = class(TDHToken_List);
  TDHToken_ListItem = class(TDHTokenBlock)
  private
    procedure Process; override;

    function IsBypassProps: Boolean; override;
  end;

  TDHToken_LineSpace = class(TDHTokenBlock)
  private
    Space, ParagraphSpace: TPixels;

    procedure ReadParam; override;
    procedure Process; override;
  end;

  TDHToken_Div = class(TDHTokenBlock)
  private
    Borders: TDHBordersRec;
    BackColor, BorderColor: TAnyColor;
    Size: TAnySize;
    FloatPos: TAnyPoint;
    MaxWidth: TPixels;
    HorzAlign: TDHHorzAlign;
    VertAlign: TDHVertAlign;
    HeightByLine: Boolean;
    KeepProps: Boolean;
    PercentWidth, PercentHeight, FullWidth, FullHeight: Boolean;

    AutoWidth: Boolean;
    AutoHeight: Boolean;
    Floating: Boolean;

    procedure ReadParam; override;
    procedure Process; override;
    function IsBreakableToken: Boolean; override;
  end;
  {$ENDREGION}

  TDHPreVisualItem = class;
  TDHPreVisualItemList = class;
  TDHDivAreaLine = class
  private
    Continuous: Boolean; //when this line is a continuation of the previous one
    Items: TDHPreVisualItemList;
    Space: TPixels;

    LonelyHeight: TPixels; //when line does not contains any object

    TextSize: TAnySize;
    procedure CalcTextSize;
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
    SubDivs: TObjectList<TDHDivArea>;

    Floating: Boolean;
    HeightByLine: Boolean;

    PreVisualItem: TDHPreVisualItem;

    FixedSize, TextSize: TAnySize;

    procedure CalcTextSize;
    function GetParagraphCount: Integer;

    function AddNewLineObject: TDHDivAreaLine;
    procedure CheckForLinesInitialization;

    function GetHorzBorder: TPixels;
    function GetVertBorder: TPixels;

    function GetAreaSizeWOB: TAnySize;
    function GetAreaSize: TAnySize;
    function GetAbsoluteStartingPos: TAnyPoint;
  public
    constructor Create(Parent: TDHDivArea);
    destructor Destroy; override;
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

    //--Superscript and Subscript  ***************
    SS_Inside: Boolean;
    SS_FullHeight: TPixels;
    SS_YPos: TPixels;
    //--

    List_Level: Byte;
    List_Number: Word;
    List_Bullet: Boolean;

    LineSpace, ParagraphSpace: TPixels;

    procedure AssignProps(Source: TDHPropsStore);
  end;

  TDHPreVisualItem = class
  private
    VisualObject: TDHVisualItem;
    Size: TAnySize;
    Position: TAnyPoint;
    DivArea: TDHDivArea;
    Line: TDHDivAreaLine;
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

  TDHProcBoundsAndLines = procedure(Size: TAnySize; LineCount, ParagraphCount: Integer) of object;

  TDHBuilder = class
  private
    Lb: TDzHTMLText;
    Canvas: TCanvas;
    VisualItems: TDHVisualItemList;
    ProcBoundsAndLines: TDHProcBoundsAndLines;

    Props: TDHPropsStore;

    CurrentLink: TDHBaseLink;

    MainToken: TDHToken_Main;
    CurrentBlock: TDHTokenBlock;

    MainDiv, CurrentDiv: TDHDivArea;

    QueueVisualItems: TDHPreVisualItemList;

    function AddToken<T: TDHToken, constructor>: T;
    procedure AddInvalidToken;

    function NewLine(Continuous: Boolean): TDHDivAreaLine;

    procedure ReadTokens;
    procedure ReadTokensOfLine(const Text: string; From: Integer);
    function ProcessTag(const Tag: string): Boolean;
    procedure ProcessChildrenTokens(Block: TDHTokenBlock);
    procedure SendObjectsToComponent(DivArea: TDHDivArea);
    procedure CheckAlign(Item: TDHPreVisualItem);

    procedure AddVisualItemToQueue(V: TDHVisualItem; Size: TAnySize; Breakable: Boolean = False);
    procedure ProcessPendingObjects;
    procedure ProcessSpecificObjects(List: TDHPreVisualItemList);
    function CreatePreVisualItem(V: TDHVisualItem; Size: TAnySize): TDHPreVisualItem;
    procedure ProcessOneObject(Item: TDHPreVisualItem);

    function CalcTextHeight(const Text: string): TPixels;
    procedure EndOfLine;
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
  Variants, SysUtils, StrUtils, Math,
{$ELSE}
  System.Variants, System.SysUtils, System.StrUtils, System.Math,
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
  TOKENS_OBJECTS: array[0..28] of TDHTokenObjectDef = (
    //single
    (Ident: 'BR'; Clazz: TDHToken_Break), //breakable!
    (Ident: 'LINE'; Clazz: TDHToken_Line; AllowPar: True; OptionalPar: True),
    (Ident: 'IMG'; Clazz: TDHToken_Image; AllowPar: True),
    (Ident: 'IMGRES'; Clazz: TDHToken_ImageResource; AllowPar: True),

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
    (Ident: 'DIV'; Clazz: TDHToken_Div; AllowPar: True), //breakable!  //optional par ???
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
    (Ident: 'LI'; Clazz: TDHToken_ListItem),
    (Ident: 'LS'; Clazz: TDHToken_LineSpace; AllowPar: True)
  );

function GetTokenObjectDefIndexFromIdent(const Ident: string): Integer;
var
  I: Integer;
begin
  for I := Low(TOKENS_OBJECTS) to High(TOKENS_OBJECTS) do
    if TOKENS_OBJECTS[I].Ident = Ident then Exit(I);

  Exit(-1);
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
  DefineFontPt(Canvas.Font, Size, Builder.Lb);
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
  Builder.Props.FontColor := Color;
end;

{ TDHToken_BackColor }

procedure TDHToken_BackColor.Process;
begin
  Builder.Props.BackColor := Color;
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
  Builder.Lb.LinkRefs.Add(Link);

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
  Style := Builder.Lb.CustomStyles.FindByIdent(Ident);
  if Style=nil then Exit; //style not found

  ApplyFontStyle(Style.StyleBold, TFontStyle.fsBold);
  ApplyFontStyle(Style.StyleItalic, TFontStyle.fsItalic);
  ApplyFontStyle(Style.StyleUnderline, TFontStyle.fsUnderline);
  ApplyFontStyle(Style.StyleStrikeout, TFontStyle.fsStrikeOut);
  if Style.FontName<>EmptyStr then DefineFontName(Canvas.Font, Style.FontName);
  if Style.FontSize>0 then DefineFontPt(Canvas.Font, Style.FontSize, Builder.Lb);
  if Style.FontColor<>clNone then Builder.Props.FontColor := Style.FontColor;
  if Style.BackColor<>clNone then Builder.Props.BackColor := Style.BackColor;

  if Style.HorzAlign<>TDHCustomStyleHorzAlignValue.Undefined then
  begin
    case Style.HorzAlign of
      TDHCustomStyleHorzAlignValue.Left: Builder.Props.HorzAlign := haLeft;
      TDHCustomStyleHorzAlignValue.Center: Builder.Props.HorzAlign := haCenter;
      TDHCustomStyleHorzAlignValue.Right: Builder.Props.HorzAlign := haRight;
      else raise EDHInternalExcept.Create('Invalid horizontal align style');
    end;
  end;

  if Style.VertAlign<>TDHCustomStyleVertAlignValue.Undefined then
  begin
    case Style.VertAlign of
      TDHCustomStyleVertAlignValue.Top: Builder.Props.VertAlign := vaTop;
      TDHCustomStyleVertAlignValue.Center: Builder.Props.VertAlign := vaCenter;
      TDHCustomStyleVertAlignValue.Bottom: Builder.Props.VertAlign := vaBottom;
      else raise EDHInternalExcept.Create('Invalid vertical align style');
    end;
  end;

  if Style.OffsetTop>=0 then Builder.Props.Offset.Top := Style.OffsetTop;
  if Style.OffsetBottom>=0 then Builder.Props.Offset.Bottom := Style.OffsetBottom;

  if Style.LineSpace>=0 then Builder.Props.LineSpace := Style.LineSpace;
  if Style.ParagraphSpace>=0 then Builder.Props.ParagraphSpace := Style.ParagraphSpace;
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
  DefineFontPt(Canvas.Font, RoundIfVCL(Builder.Lb.Font.Size * ((6-Level+2)/2)), Builder.Lb);
  Canvas.Font.Style := Canvas.Font.Style + [TFontStyle.fsBold];
end;

{ TDHToken_Offset }

procedure TDHToken_Offset.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Top := P.GetParamAsPixels('top', -1);
    Bottom := P.GetParamAsPixels('bottom', -1);
  finally
    P.Free;
  end;
end;

procedure TDHToken_Offset.Process;
begin
  if Top>=0 then Builder.Props.Offset.Top := Top;
  if Bottom>=0 then Builder.Props.Offset.Bottom := Bottom;
end;

{ TDHToken_Word }

procedure TDHToken_Word.Process;
var
  V: TDHVisualItem_Word;
  Size: TAnySize;
begin
  Size := TAnySize.Create(Canvas.TextWidth(Word), Builder.CalcTextHeight(Word));

  V := TDHVisualItem_Word.Create;
  V.Text := Word;
  V.Font.Assign(Canvas.Font);
  V.{$IFDEF FMX}FontColor{$ELSE}Font.Color{$ENDIF} := Builder.Props.FontColor;

  if Builder.Props.SS_Inside then
    V.YPos := Builder.Props.SS_YPos;

  if Builder.CurrentLink is TDHLinkRef then
    Builder.CurrentLink.LinkRef.Text.Append(Word);

  Builder.AddVisualItemToQueue(V, Size, Breakable);
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
begin
  {$IFDEF USE_IMGLST}
  if Assigned(Builder.Lb.Images) then
  begin
    {$IFDEF FMX}
    with Builder.Lb.Images.Destination[ImageIndex].Layers[0].SourceRect do
      Size := TAnySize.Create(Width, Height);
    {$ELSE}
    Size := TAnySize.Create(Builder.Lb.Images.Width, Builder.Lb.Images.Height);
    {$ENDIF}
  end;
  {$ENDIF}

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
  V.Load(Builder.Lb, ResourceName);

  Size := TAnySize.Create(V.Picture.Width, V.Picture.Height);

  Builder.AddVisualItemToQueue(V, Size);
end;

{ TDHToken_Break }

function TDHToken_Break.IsBreakableToken: Boolean;
begin
  Result := True;
end;

procedure TDHToken_Break.Process;
begin
  Builder.CurrentDiv.CheckForLinesInitialization;
  Builder.NewLine(False);
end;

{ TDHToken_Line }

procedure TDHToken_Line.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Width := P.GetParamAsPixels('width', 100);
    Height := P.GetParamAsPixels('height', 1);

    Color := ParamToColor(P.GetParam('color'));
    ColorAlt := ParamToColor(P.GetParam('coloralt'));

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

{ TDHToken_AlignLeft }

procedure TDHToken_AlignLeft.Process;
begin
  Builder.Props.HorzAlign := haLeft;
end;

{ TDHToken_AlignCenter }

procedure TDHToken_AlignCenter.Process;
begin
  Builder.Props.HorzAlign := haCenter;
end;

{ TDHToken_AlignRight }

procedure TDHToken_AlignRight.Process;
begin
  Builder.Props.HorzAlign := haRight;
end;

{ TDHToken_VertAlign }

procedure TDHToken_VertAlign.ReadParam;
begin
  Align := ParamToVertAlign(Param);
end;

procedure TDHToken_VertAlign.Process;
begin
  Builder.Props.VertAlign := Align;
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

  Spoiler := Builder.Lb.Spoilers.Find(Ident);
  if Spoiler=nil then
  begin
    Spoiler := TDHSpoiler.Create(Ident, StartExpanded);
    Builder.Lb.Spoilers.Add(Spoiler);
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
  if not Builder.Props.SS_Inside then
    Builder.Props.SS_FullHeight := Canvas.TextHeight(STR_SPACE);
  Builder.Props.SS_Inside := True;

  ParentHeight := Canvas.TextHeight(STR_SPACE);

  DefineFontPt(Canvas.Font, RoundIfVCL(Canvas.Font.Size * 0.75), Builder.Lb);

  if Self is TDHToken_Superscript then Delta := 0 else
  if Self is TDHToken_Subscript then Delta := ParentHeight - Canvas.TextHeight(STR_SPACE) else
    raise EDHInternalExcept.Create('Invalid Super or Sub Script class');

  Builder.Props.SS_YPos := Builder.Props.SS_YPos + Delta;
end;

{ TDHToken_List }

procedure TDHToken_List.Process;
begin
  Inc(Builder.Props.List_Level);
  Builder.Props.List_Number := 0;
  Builder.Props.List_Bullet := Self is TDHToken_UnorderedList;
end;

{ TDHToken_ListItem }

function TDHToken_ListItem.IsBypassProps: Boolean;
begin
  Result := True;
end;

procedure TDHToken_ListItem.Process;
var
  Token: TDHToken_Word;
begin
  Inc(Builder.Props.List_Number);

  Token := TDHToken_Word.Create;
  try
    Token.Builder := Builder;

    if Builder.Props.List_Bullet then
      Token.Word := '� '
    else
      Token.Word := IntToStr(Builder.Props.List_Number)+'. ';

    Builder.CurrentDiv.Point.X := Builder.Props.List_Level * Builder.Lb.ListLevelPadding;
    Token.Breakable := True;
    Token.Process;
  finally
    Token.Free;
  end;
end;

{ TDHToken_LineSpace }

procedure TDHToken_LineSpace.ReadParam;
var
  P: TDHMultipleTokenParams;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    Space := StrToPixels(P.GetFirstParam, 0);
    ParagraphSpace := P.GetParamAsPixels('par', 0);
  finally
    P.Free;
  end;
end;

procedure TDHToken_LineSpace.Process;
begin
  Builder.Props.LineSpace := Space;
  Builder.Props.ParagraphSpace := ParagraphSpace;
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
  AllLnCol: TAnyColor;

  procedure UpdVar(var &Var: TPixels; All, Value: TPixels);
  begin
    if Value >= 0 then
      &Var := Value
    else
      &Var := All;
  end;

  procedure ReadBorder(var B: TDHBorderRec; const Ident: string);
  var
    LnCol: TAnyColor;
  begin
    UpdVar(B.Margin, AllMargin, P.GetParamAsPixels('margin_'+Ident, -1));
    UpdVar(B.Thick, AllThick, P.GetParamAsPixels('thick_'+Ident, -1));
    UpdVar(B.Pad, AllPad, P.GetParamAsPixels('pad_'+Ident, -1));

    LnCol := ParamToColor(P.GetParam('lncol_'+Ident));
    if LnCol<>clNone then
      B.LnColor := LnCol
    else
      B.LnColor := AllLnCol;

    B.Margin := B.Margin + B.Thick + B.Pad;
  end;

  procedure DetectSizeParams(var Value: string; var Percent: Boolean; var Internal: Boolean);
  begin
    //only one attribute allowed
    Percent := Value.EndsWith('%');
    Internal := Value.EndsWith('#');
    if Percent or Internal then Delete(Value, Value.Length, 1);
  end;

var
  ParW, ParH: string;
  WInt, HInt: Boolean;
begin
  P := TDHMultipleTokenParams.Create(Param);
  try
    ParW := P.GetParam('width');
    ParH := P.GetParam('height');

    DetectSizeParams(ParW, PercentWidth, WInt);
    DetectSizeParams(ParH, PercentHeight, HInt);

    Size := TAnySize.Create(StrToPixels(ParW, 0), StrToPixels(ParH, 0));
    //if size in percent, here size contains percent value already

    FullWidth := SameText(ParW, 'full');
    FullHeight := SameText(ParH, 'full');

    AutoWidth := (Size.Width <= 0) and not FullWidth;
    AutoHeight := (Size.Height <= 0) and not FullHeight;

    HeightByLine := SameText(ParH, 'line');

    //--Borders
    AllMargin := P.GetParamAsPixels('margin', 0);
    AllThick := P.GetParamAsPixels('thick', 0);
    AllPad := P.GetParamAsPixels('pad', 0);
    AllLnCol := ParamToColor(P.GetParam('lncol'));

    ReadBorder(Borders.Left, 'left');
    ReadBorder(Borders.Top, 'top');
    ReadBorder(Borders.Right, 'right');
    ReadBorder(Borders.Bottom, 'bottom');

    //

    if WInt then Size.Width := Size.Width + Borders.Left.Margin + Borders.Right.Margin;
    if HInt then Size.Height := Size.Height + Borders.Top.Margin + Borders.Bottom.Margin;

    //--

    FloatPos := TAnyPoint.Create(P.GetParamAsPixels('x', -1), P.GetParamAsPixels('y', -1));
    Floating := (FloatPos.X>=0) and (FloatPos.Y>=0);

    HorzAlign := ParamToHorzAlign(P.GetParam('align'));
    VertAlign := ParamToVertAlign(P.GetParam('valign'));
    MaxWidth := P.GetParamAsPixels('maxwidth', 0);
    BackColor := ParamToColor(P.GetParam('color'));
    BorderColor := ParamToColor(P.GetParam('outcolor'));
    KeepProps := P.ParamExists('keep');
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
begin
  if not Builder.CurrentDiv.AutoWidth then
  begin
    if PercentWidth then Size.Width := RoundIfVCL(Builder.CurrentDiv.GetAreaSizeWOB.Width * Size.Width/100) else
      if FullWidth then Size.Width := Builder.CurrentDiv.GetAreaSizeWOB.Width - Builder.CurrentDiv.Point.X;
  end;
  if not Builder.CurrentDiv.AutoHeight then
  begin
    if PercentHeight then Size.Height := RoundIfVCL(Builder.CurrentDiv.GetAreaSizeWOB.Height * Size.Height/100) else
      if FullHeight then Size.Height := Builder.CurrentDiv.GetAreaSizeWOB.Height - Builder.CurrentDiv.Point.Y;
  end;

  D := TDHDivArea.Create(Builder.CurrentDiv);
  D.FixedSize := Size;
  D.AutoWidth := AutoWidth;
  D.AutoHeight := AutoHeight;
  D.MaxWidth := MaxWidth;
  D.Floating := Floating;
  D.Borders := Borders;
  D.VertAlign := VertAlign;
  D.HorzAlign := HorzAlign;
  D.HeightByLine := HeightByLine;

  V := TDHVisualItem_Div.Create;
  V.InnerColor := BackColor;
  V.OutterColor := BorderColor;

  ToDivLineAttr(Borders.Left, V.Left);
  ToDivLineAttr(Borders.Top, V.Top);
  ToDivLineAttr(Borders.Right, V.Right);
  ToDivLineAttr(Borders.Bottom, V.Bottom);

  Item := Builder.CreatePreVisualItem(V, TAnySize.Create(0, 0)); //we don't know the size yet
  if Floating then Item.Position := FloatPos;
  Item.SelfDiv := D;

  D.PreVisualItem := Item;

  if not KeepProps then
  begin
    Builder.Props.Offset.Top := 0;
    Builder.Props.Offset.Bottom := 0;
    Builder.Props.BackColor := clNone;
    Builder.Props.HorzAlign := haLeft;
    Builder.Props.VertAlign := vaTop;
    Builder.Props.LineSpace := 0;
    Builder.Props.ParagraphSpace := 0;
  end;

  Builder.CurrentDiv.SubDivs.Add(D);
  Builder.CurrentDiv := D; //change current div!
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
  SubDivs := TObjectList<TDHDivArea>.Create;

  Self.Parent := Parent;
end;

destructor TDHDivArea.Destroy;
begin
  Lines.Free;
  SubDivs.Free;
  inherited;
end;

procedure TDHDivArea.CheckForLinesInitialization;
begin
  if Lines.Count=0 then
    AddNewLineObject;
end;

function TDHDivArea.AddNewLineObject: TDHDivAreaLine;
begin
  Result := TDHDivAreaLine.Create;
  Lines.Add(Result);
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

procedure TDHDivArea.CalcTextSize;
var
  Line: TDHDivAreaLine;
  W, H: TPixels;
begin
  W := 0;
  H := 0;

  for Line in Lines do
  begin
    if Line.TextSize.Width > W then W := Line.TextSize.Width;
    H := H + Line.TextSize.Height + Line.Space;
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

procedure TDHDivAreaLine.CalcTextSize;
var
  W, H, FullH: TPixels;
  Item: TDHPreVisualItem;
begin
  W := 0;
  H := LonelyHeight;

  for Item in Items do
  begin
    W := W + Item.Size.Width;

    FullH := Item.GetFullHeight;
    if FullH > H then H := FullH;
  end;

  TextSize := TAnySize.Create(W, H);
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

  MainToken := TDHToken_Main.Create;

  //--Canvas
  Canvas.Font.Assign(Lb.Font);
  //--

  //--Props
  Props := TDHPropsStore.Create;
  Props.FontColor := Lb.{$IFDEF FMX}FontColor{$ELSE}Font.Color{$ENDIF};
  {$IF Defined(DCC) and Defined(VCL)}
  if TStyleManager.IsCustomStyleActive and (seFont in Lb.StyleElements) and not (csDesigning in Lb.ComponentState) then
    Props.FontColor := TStyleManager.ActiveStyle.GetStyleFontColor(TStyleFont.sfWindowTextNormal);
  {$ENDIF}
  Props.BackColor := clNone;

  Props.Offset.Top := Lb.Offset.Top;
  Props.Offset.Bottom := Lb.Offset.Bottom;

  Props.VertAlign := Lb.LineVertAlign;
  Props.HorzAlign := Lb.LineHorzAlign;

  Props.LineSpace := Lb.LineSpace;
  Props.ParagraphSpace := Lb.ParagraphSpace;
  //--

  //--MainDiv
  MainDiv := TDHDivArea.Create(nil);
  MainDiv.Main := True;

  MainDiv.AutoWidth := Lb.AutoWidth;
  MainDiv.AutoHeight := Lb.AutoHeight;

  MainDiv.MaxWidth := Lb.MaxWidth;

  MainDiv.FixedSize := TAnySize.Create(Lb.Width, Lb.Height);

  MainDiv.HorzAlign := Lb.OverallHorzAlign;
  MainDiv.VertAlign := Lb.OverallVertAlign;

  MainDiv.Borders.Left.Margin := Lb.Borders.Left;
  MainDiv.Borders.Top.Margin := Lb.Borders.Top;
  MainDiv.Borders.Right.Margin := Lb.Borders.Right;
  MainDiv.Borders.Bottom.Margin := Lb.Borders.Bottom;
  //--
end;

destructor TDHBuilder.Destroy;
begin
  QueueVisualItems.Free;
  Props.Free;
  MainToken.Free;
  MainDiv.Free;

  inherited;
end;

procedure TDHBuilder.Execute;
begin
  CurrentBlock := MainToken;
  ReadTokens;
  if CurrentBlock<>MainToken then AddInvalidToken; //missing some tag closing
  CurrentBlock := nil;

  CurrentDiv := MainDiv;
  ProcessChildrenTokens(MainToken);
  ProcessPendingObjects; //process remaining objects in queue list
  EndOfLine;
  CurrentDiv := nil;

  MainDiv.CalcTextSize;
  ProcBoundsAndLines(MainDiv.TextSize, MainDiv.Lines.Count, MainDiv.GetParagraphCount);

  SendObjectsToComponent(MainDiv);
end;

procedure TDHBuilder.AddInvalidToken;
var
  Token: TDHToken_Word;
begin
  Token := AddToken<TDHToken_Word>;
  Token.Word := '<?>';
  Token.Breakable := True;
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
  I, From: Integer;
  A: string;
begin
  for I := 0 to Lb.Lines.Count-1 do
  begin
    A := Lb.Lines[I];
    From := 1;
    if I>0 then
    begin
      if A.StartsWith(NBR_TAG, True) then
        From := NBR_TAG.Length+1 //jump NBR tag
      else
        AddToken<TDHToken_Break>; //break previous line
    end;
    ReadTokensOfLine(A, From);
  end;
end;

procedure TDHBuilder.ReadTokensOfLine(const Text: string; From: Integer);
var
  CharIni: Char;
  I, CurPos, Len: Integer;
  BreakableChar: Boolean;
begin
  CurPos := From;
  Len := Text.Length;
  while CurPos <= Len do
  begin
    CharIni := Text[CurPos];

    if CharIni = '<' then //starts with tag opening
    begin
      I := PosEx('>', Text, CurPos+1); //find tag closing
      if I>0 then
      begin
        if not ProcessTag(Copy(Text, CurPos+1, I-CurPos-1)) then AddInvalidToken;
        CurPos := I+1;
      end else
      begin
        //losted tag opening
        AddInvalidToken;
        Inc(CurPos);
      end;
    end else
    if CharIni = '>' then
    begin
      //losted tag closing
      AddInvalidToken;
      Inc(CurPos);
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
        Breakable := BreakableChar;
      end;
      
      CurPos := I;
    end;
  end;
end;

function TDHBuilder.ProcessTag(const Tag: string): Boolean;
var
  CloseTag, HasPar, Block: Boolean;
  A, Par: string;
  I: Integer;
  Def: TDHTokenObjectDef;
  TokenClass: TDHTokenClass;
  Token: TDHToken;
begin
  Result := False;
  A := Tag;

  CloseTag := A.StartsWith('/');
  if CloseTag then Delete(A, 1, 1);

  I := Pos(':', A); //find parameter
  HasPar := I>0;
  if HasPar then
  begin
    Par := A.Substring(I); //zero-based
    A := Copy(A, 1, I-1);

    if Par.IsEmpty then Exit; //blank parameter specified
    if CloseTag then Exit; //tag closing with parameter
  end;

  if A.IsEmpty then Exit; //blank tag

  I := GetTokenObjectDefIndexFromIdent(UpperCase(A));
  if I = -1 then Exit; //invalid tag

  Def := TOKENS_OBJECTS[I];
  TokenClass := Def.Clazz;
  Block := TokenClass.InheritsFrom(TDHTokenBlock);

  if CloseTag then
  begin
    if not Block then Exit; //close-tag on single tag
    if CurrentBlock.ClassType <> TokenClass then Exit; //closing different tag

    CurrentBlock := CurrentBlock.Parent;
  end else
  begin
    if (not Def.AllowPar) and (HasPar) then Exit; //parameter not allowed
    if (Def.AllowPar) and (not Def.OptionalPar) and (not HasPar) then Exit; //parameter required

    Token := TokenClass.Create;
    Token.Init(Self);
    if HasPar then
    begin
      Token.Param := Par;
      Token.ValidParam := True;
      Token.ReadParam;
      if not Token.ValidParam then
      begin
        CurrentBlock.Children.Remove(Token);
        Exit;
      end;
    end;

    if Block then CurrentBlock := TDHTokenBlock(Token);
  end;

  Result := True;
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

    if Token.IsBreakableToken then ProcessPendingObjects;

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

procedure TDHBuilder.EndOfLine;
var
  Line: TDHDivAreaLine;
begin
  if CurrentDiv.Lines.Count=0 then Exit;
  
  Line := CurrentDiv.Lines.Last;
  if Line.Items.Count=0 then //line without visual items
    Line.LonelyHeight := CalcTextHeight(STR_SPACE) + Props.Offset.GetHeight;

  Line.CalcTextSize;
end;

function TDHBuilder.NewLine(Continuous: Boolean): TDHDivAreaLine;
var
  Line: TDHDivAreaLine;
  Space: TPixels;
begin
  EndOfLine; //must always contain lines here

  Line := CurrentDiv.Lines.Last;

  if Continuous then
    Space := Props.LineSpace
  else
    Space := Props.ParagraphSpace;

  CurrentDiv.Point.X := 0;
  CurrentDiv.Point.Offset(0, Line.TextSize.Height + Space);

  Result := CurrentDiv.AddNewLineObject;
  Result.Continuous := Continuous;
  Result.Space := Space;
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
  QueueVisualItems.Add(CreatePreVisualItem(V, Size));
  if Breakable then ProcessPendingObjects;
end;

procedure TDHBuilder.ProcessPendingObjects;
begin
  if QueueVisualItems.Count=0 then Exit;

  ProcessSpecificObjects(QueueVisualItems);
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

  CurrentDiv.CheckForLinesInitialization;

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
          Line.Items.Remove(Item);
          PrevSpaceRemoved := True;
        end;
        Line := NewLine(True);
      end;
    end;
  end;

  while List.Count>0 do
  begin
    Item := List.ExtractAt(0);

    if not PrevSpaceRemoved and Line.Continuous and Item.IsSpace and (Line.Items.Count=0) then
    begin
      //skip SPACE if is the first item in the line
      Item.Free;
      Continue;
    end;

    Item.DivArea := CurrentDiv;
    Item.Line := Line;

    Line.Items.Add(Item);

    if not ((Item.SelfDiv<>nil) and Item.SelfDiv.Floating) then
    begin
      Item.Position := CurrentDiv.Point;
      Item.Position.Offset(0, Item.Offset.Top);
      CurrentDiv.Point.Offset(Item.Size.Width, 0);
    end;
  end;
end;

procedure TDHBuilder.SendObjectsToComponent(DivArea: TDHDivArea);
var
  Line: TDHDivAreaLine;
  Item: TDHPreVisualItem;
  SubDiv: TDHDivArea;
  P: TAnyPoint;
begin
  for Line in DivArea.Lines do
  begin
    for Item in Line.Items do
    begin
      if (Item.SelfDiv<>nil) and Item.SelfDiv.HeightByLine then
      begin
        Item.Size.Height := Item.Line.TextSize.Height - Item.Offset.GetHeight; //size of div rectangle

        Item.SelfDiv.AutoHeight := False;
        Item.SelfDiv.FixedSize.Height := Item.Size.Height; //size of div area for align children
      end;

      CheckAlign(Item);

      P := Item.DivArea.GetAbsoluteStartingPos;
      P.Offset(Item.Position);
      Item.VisualObject.Rect := TAnyRect.Create(P, Item.Size.Width, Item.Size.Height);

      VisualItems.Add(Item.VisualObject);
      Item.VisualObject := nil;
    end;
  end;

  for SubDiv in DivArea.SubDivs do
    SendObjectsToComponent(SubDiv);
end;

procedure TDHBuilder.CheckAlign(Item: TDHPreVisualItem);
type
  TFuncAlignResult = record
    Outside, Inside: TPixels;
  end;

  function FuncAlignHorz: TFuncAlignResult;
  var
    DivLim: TPixels;
  begin
    if Item.DivArea.HorzAlign in [haCenter, haRight] then
      DivLim := Item.DivArea.TextSize.Width
    else
      DivLim := Item.DivArea.GetAreaSizeWOB.Width;

    Result.Outside := DivLim;
    Result.Inside := Item.Line.TextSize.Width;
  end;

  function FuncAlignVert: TFuncAlignResult;
  begin
    Result.Outside := Item.Line.TextSize.Height;
    Result.Inside := Item.Size.Height;
  end;

  function FuncDivAlignHorz: TFuncAlignResult;
  begin
    Result.Outside := Item.DivArea.GetAreaSizeWOB.Width;
    Result.Inside := Item.DivArea.TextSize.Width;
  end;

  function FuncDivAlignVert: TFuncAlignResult;
  begin
    Result.Outside := Item.DivArea.GetAreaSizeWOB.Height;
    Result.Inside := Item.DivArea.TextSize.Height;
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
  Check(0, True, Item.HorzAlign);
  Check(1, False, Item.VertAlign);

  Check(2, True, Item.DivArea.HorzAlign);
  Check(3, False, Item.DivArea.VertAlign);
end;

end.