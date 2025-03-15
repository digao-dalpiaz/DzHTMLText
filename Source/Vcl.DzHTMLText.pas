{------------------------------------------------------------------------------
TDzHTMLText component
Developed by Rodrigo Depine Dalpiaz (digao dalpiaz)
Label with formatting tags support

https://github.com/digao-dalpiaz/DzHTMLText

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

{$IFNDEF FMX}unit Vcl.DzHTMLText;{$ENDIF}

{$INCLUDE Defines.inc}

interface

uses
{$IFDEF FPC}
  Controls, Forms, Classes, Messages, Graphics, Types, FGL, LCLIntf, ImgList, SysUtils
{$ELSE}
  System.Generics.Collections, System.Types, System.Classes, System.SysUtils,
  {$IFDEF FMX}
  FMX.Forms, FMX.Controls, FMX.Types, System.UITypes
    {$IFDEF USE_NEW_UNITS}, FMX.StdCtrls, FMX.Graphics, FMX.MultiResBitmap{$ENDIF}
    {$IFDEF USE_IMGLST}, FMX.ImgList{$ENDIF}
  {$ELSE}
  Vcl.Controls, Vcl.Forms, Vcl.Graphics, Vcl.ImgList, Vcl.Imaging.pngimage,
  Winapi.Messages
  {$ENDIF}
{$ENDIF};

const DZHTMLTEXT_INTERNAL_VERSION = 714; //Synchronizes TDam component

const _DEF_LISTLEVELPADDING = 20;

{$INCLUDE Types.inc}
type
  TMyFontStyle = TFontStyle;

  TDzHTMLText = class;

  TDHLinkKind = (lkLinkRef, lkSpoiler);

  TDHLinkRef = class;
  TDHSpoiler = class;
  TDHBaseLink = class
  private
    function GetKind: TDHLinkKind;
    function GetLinkRef: TDHLinkRef;
    function GetSpoiler: TDHSpoiler;
  public
    property Kind: TDHLinkKind read GetKind;
    property LinkRef: TDHLinkRef read GetLinkRef;
    property Spoiler: TDHSpoiler read GetSpoiler;
  end;

  TDHLinkRef = class(TDHBaseLink)
  private
    FTarget: string;
    FText: TStringBuilder;
  public
    property Target: string read FTarget;
    property Text: TStringBuilder read FText;

    constructor Create(const Target: string);
    destructor Destroy; override;
  end;
  TDHLinkRefList = class(TObjectList<TDHLinkRef>);

  TDHSpoiler = class(TDHBaseLink)
  private
    FName: string;
    FExpanded: Boolean;
  public
    property Name: string read FName;
    property Expanded: Boolean read FExpanded;

    constructor Create(const Name: string; Expanded: Boolean);
  end;
  TDHSpoilerList = class(TObjectList<TDHSpoiler>)
  public
    function Find(const Name: string): TDHSpoiler;
  end;

  TDHVisualItem = class //represents each visual item printed to then canvas
  public
    Rect: TAnyRect;
    BColor: TAnyColor; //background color
    Link: TDHBaseLink;
  end;
  TDHVisualItemList = class(TObjectList<TDHVisualItem>);

  TDHDivBorderLineAttrRec = record
  public
    Thick, Pad: TPixels;
    Color: TAnyColor;
  end;
  TDHVisualItem_Div = class(TDHVisualItem)
  public
    OuterColor, InnerColor: TAnyColor;
    Left, Top, Right, Bottom: TDHDivBorderLineAttrRec;
    CornerRadius: TPixels;
  end;

  TDHVisualItem_Word = class(TDHVisualItem)
  public
    Text: string;
    Font: TFont;
    {$IFDEF FMX}
    FontColor: TAnyColor;
    {$ENDIF}
    YPos: TPixels;

    constructor Create;
    destructor Destroy; override;
  end;

  TDHVisualItem_Image = class(TDHVisualItem)
  public
    ImageIndex: Integer;
  end;

  TDHVisualItem_ImageResource = class(TDHVisualItem)
  public
    Picture: TAnyPicture;
    procedure Load(Lb: TDzHTMLText; const ResourceName: string);

    constructor Create;
    destructor Destroy; override;
  end;

  TDHVisualItem_Line = class(TDHVisualItem)
  public
    Color: TAnyColor;
    ColorAlt: TAnyColor;
  end;

  TDHOffset = class(TPersistent)
  private
    Lb: TDzHTMLText; //owner

    FTop, FBottom: TPixels;

    procedure SetTop(const Value: TPixels);
    procedure SetBottom(const Value: TPixels);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Lb: TDzHTMLText);
    procedure Assign(Source: TPersistent); override;
  published
    property Top: TPixels read FTop write SetTop {$IFDEF VCL}default 0{$ENDIF};
    property Bottom: TPixels read FBottom write SetBottom {$IFDEF VCL}default 0{$ENDIF};
  end;

  TDHKindStyleLinkProp = (tslpNormal, tslpHover); //kind of link style

  {DHStyleLinkProp is a sub-property used at Object Inspector that contains
   link formatting when selected and not selected}
  TDHStyleLinkProp = class(TPersistent)
  private
    Lb: TDzHTMLText; //owner
    Kind: TDHKindStyleLinkProp;

    FFontColor: TAnyColor;
    FBackColor: TAnyColor;
    FUnderline: Boolean;
    procedure SetFontColor(const Value: TAnyColor);
    procedure SetBackColor(const Value: TAnyColor);
    procedure SetUnderline(const Value: Boolean);
    function GetDefaultFontColor: TAnyColor;
    function GetStoredFontColor: Boolean;
    procedure SetPropsToCanvas(C: TCanvas); //method to use at paint event
    function GetStored: Boolean; //GetStored general to use at owner
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Lb: TDzHTMLText; Kind: TDHKindStyleLinkProp);
    procedure Assign(Source: TPersistent); override;
  published
    property FontColor: TAnyColor read FFontColor write SetFontColor stored GetStoredFontColor;
    property BackColor: TAnyColor read FBackColor write SetBackColor default clNone;
    property Underline: Boolean read FUnderline write SetUnderline default False;
  end;

  TDHBorders = class(TPersistent)
  private
    Lb: TDzHTMLText;

    FLeft, FTop, FRight, FBottom: TPixels;
    function HasAnyValue: Boolean;
    function GetAll: TPixels;
    procedure SetAll(const Value: TPixels);
    function GetStoredAll: Boolean;
    function GetStoredSides: Boolean;
    procedure SetLeft(const Value: TPixels);
    procedure SetTop(const Value: TPixels);
    procedure SetRight(const Value: TPixels);
    procedure SetBottom(const Value: TPixels);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Lb: TDzHTMLText);
    procedure Assign(Source: TPersistent); override;
  published
    property All: TPixels read GetAll write SetAll stored GetStoredAll;
    property Left: TPixels read FLeft write SetLeft stored GetStoredSides;
    property Top: TPixels read FTop write SetTop stored GetStoredSides;
    property Right: TPixels read FRight write SetRight stored GetStoredSides;
    property Bottom: TPixels read FBottom write SetBottom stored GetStoredSides;
  end;

  {$SCOPEDENUMS ON}
  TDHCustomStyleBoolValue = (Undefined, True, False);
  TDHCustomStyleHorzAlignValue = (Undefined, Left, Center, Right);
  TDHCustomStyleVertAlignValue = (Undefined, Top, Center, Bottom);
  {$SCOPEDENUMS OFF}
  TDHCustomStyle = class(TCollectionItem)
  private
    FIdent: string;
    FFontName: string;
    FFontSize: TPixels;
    FStyleBold, FStyleItalic, FStyleUnderline, FStyleStrikeout: TDHCustomStyleBoolValue;
    FFontColor: TAnyColor;
    FBackColor: TAnyColor;
    FHorzAlign: TDHCustomStyleHorzAlignValue;
    FVertAlign: TDHCustomStyleVertAlignValue;
    FOffsetTop, FOffsetBottom: TPixels;
    FLineSpacing, FParagraphSpacing, FParagraphIndent: TPixels;

    procedure Modified;

    procedure SetIdent(const Value: string);
    procedure SetFontName(const Value: string);
    procedure SetFontSize(const Value: TPixels);
    procedure SetStyleBold(const Value: TDHCustomStyleBoolValue);
    procedure SetStyleItalic(const Value: TDHCustomStyleBoolValue);
    procedure SetStyleStrikeout(const Value: TDHCustomStyleBoolValue);
    procedure SetStyleUnderline(const Value: TDHCustomStyleBoolValue);
    procedure SetBackColor(const Value: TAnyColor);
    procedure SetFontColor(const Value: TAnyColor);
    procedure SetHorzAlign(const Value: TDHCustomStyleHorzAlignValue);
    procedure SetVertAlign(const Value: TDHCustomStyleVertAlignValue);
    procedure SetOffsetTop(const Value: TPixels);
    procedure SetOffsetBottom(const Value: TPixels);
    procedure SetLineSpacing(const Value: TPixels);
    procedure SetParagraphSpacing(const Value: TPixels);
    procedure SetParagraphIndent(const Value: TPixels);

    function GetStoredOffsetTop: Boolean;
    function GetStoredOffsetBottom: Boolean;
    function GetStoredLineSpacing: Boolean;
    function GetStoredParagraphSpacing: Boolean;
    function GetStoredParagraphIndent: Boolean;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property Ident: string read FIdent write SetIdent;
    property FontName: string read FFontName write SetFontName;
    property FontSize: TPixels read FFontSize write SetFontSize {$IFDEF VCL}default 0{$ENDIF};
    property StyleBold: TDHCustomStyleBoolValue read FStyleBold write SetStyleBold default TDHCustomStyleBoolValue.Undefined;
    property StyleItalic: TDHCustomStyleBoolValue read FStyleItalic write SetStyleItalic default TDHCustomStyleBoolValue.Undefined;
    property StyleUnderline: TDHCustomStyleBoolValue read FStyleUnderline write SetStyleUnderline default TDHCustomStyleBoolValue.Undefined;
    property StyleStrikeout: TDHCustomStyleBoolValue read FStyleStrikeout write SetStyleStrikeout default TDHCustomStyleBoolValue.Undefined;
    property FontColor: TAnyColor read FFontColor write SetFontColor default clNone;
    property BackColor: TAnyColor read FBackColor write SetBackColor default clNone;
    property HorzAlign: TDHCustomStyleHorzAlignValue read FHorzAlign write SetHorzAlign default TDHCustomStyleHorzAlignValue.Undefined;
    property VertAlign: TDHCustomStyleVertAlignValue read FVertAlign write SetVertAlign default TDHCustomStyleVertAlignValue.Undefined;
    property OffsetTop: TPixels read FOffsetTop write SetOffsetTop stored GetStoredOffsetTop;
    property OffsetBottom: TPixels read FOffsetBottom write SetOffsetBottom stored GetStoredOffsetBottom;
    property LineSpacing: TPixels read FLineSpacing write SetLineSpacing stored GetStoredLineSpacing;
    property ParagraphSpacing: TPixels read FParagraphSpacing write SetParagraphSpacing stored GetStoredParagraphSpacing;
    property ParagraphIndent: TPixels read FParagraphIndent write SetParagraphIndent stored GetStoredParagraphIndent;
  end;

  TDHCustomStyles = class(TCollection)
  private
    Lb: TDzHTMLText;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(Lb: TDzHTMLText);

    function FindByIdent(const Ident: string): TDHCustomStyle;
  end;

  TDHSyntaxError = class
  private
    FPosition: Integer;
    FDescription: string;
  public
    property Position: Integer read FPosition;
    property Description: string read FDescription;

    constructor Create(Position: Integer; const Description: string);
  end;
  TDHSyntaxErrorList = class(TObjectList<TDHSyntaxError>);

  TDHEvLink = procedure(Sender: TObject; Link: TDHBaseLink) of object;
  TDHEvLinkClick = procedure(Sender: TObject; Link: TDHBaseLink; var Handled: Boolean) of object;

  TDHVertAlign = (vaTop, vaCenter, vaBottom);
  TDHHorzAlign = (haLeft, haCenter, haRight);

  TDHEvRetrieveImgRes = procedure(Sender: TObject; const ResourceName: string; Picture: TAnyPicture; var Handled: Boolean) of object;

  TDHModifiedFlag = (mfBuild, mfPaint);
  TDHModifiedFlags = set of TDHModifiedFlag;

  TDzHTMLText = class(
    {$IFDEF FMX}
      {$IFDEF USE_NEW_ENV}TPresentedTextControl{$ELSE}TTextControl{$ENDIF}
    {$ELSE}
      TGraphicControl
    {$ENDIF})
  private
    FAbout: string;

    {$IFDEF FMX}
    FirstRebuild: Boolean;
    {$ENDIF}

    VisualItems: TDHVisualItemList;

    LError: TDHSyntaxErrorList;

    LSpoiler: TDHSpoilerList;
    LLinkRef: TDHLinkRefList;

    FLines: TStrings;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    FMaxWidth: TPixels; //max width when using AutoWidth
    FAutoOpenLink: Boolean;
    FAutoBreak: Boolean;

    FLineCount: Integer; //read-only
    FParagraphCount: Integer; //read-only
    FTextWidth: TPixels; //read-only
    FTextHeight: TPixels; //read-only

    FOffset: TDHOffset;
    FCustomStyles: TDHCustomStyles;

    FStyleLinkNormal, FStyleLinkHover: TDHStyleLinkProp;

    {$IFDEF FMX}
    FColor: TAnyColor;
    FFontColor: TAnyColor;
    {$ELSE}
    FTransparent: Boolean;
    {$ENDIF}

    {$IFDEF USE_IMGLST}
    FImages: TCustomImageList;
    {$ENDIF}

    FOnRetrieveImgRes: TDHEvRetrieveImgRes;

    FLineVertAlign: TDHVertAlign;
    FLineHorzAlign: TDHHorzAlign;
    FOverallVertAlign: TDHVertAlign;
    FOverallHorzAlign: TDHHorzAlign;
    FListLevelPadding: TPixels;

    FBorders: TDHBorders;

    FLineSpacing, FParagraphSpacing, FParagraphIndent: TPixels;

    FOnLinkEnter, FOnLinkLeave: TDHEvLink;
    FOnLinkClick, FOnLinkRightClick: TDHEvLinkClick;

    FSelectedLink: TDHBaseLink; //selected link

    FCursor: TCursor;

    FRightToLeftText: Boolean;

    UpdatingSemaphore: Integer;
    InternalResizing: Boolean;

    {$IFDEF VCL}
    ParentForm: TCustomForm;
    {$ENDIF}

    FGeneratePlainText: Boolean;
    FPlainText: TStringBuilder;

    procedure OnLinesChange(Sender: TObject);
    procedure SetLines(const Value: TStrings);
    function GetText: string;
    procedure SetText(const Value: string); {$IFDEF FMX}reintroduce;{$ENDIF}
    procedure SetAutoBreak(const Value: Boolean);

    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetMaxWidth(const Value: TPixels);

    function GetStoredStyleLink(const Index: Integer): Boolean;
    function GetStoredMaxWidth: Boolean;
    function GetStoredListLevelPadding: Boolean;
    function GetStoredBorders: Boolean;
    function GetStoredOffset: Boolean;
    function GetStoredCustomStyles: Boolean;

    procedure ExecPaint;
    procedure CanvasProcess(C: TCanvas);
    procedure Paint_VisualItem(W: TDHVisualItem; C: TCanvas);
    procedure Paint_Div(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Div);
    procedure Paint_Word(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Word);
    procedure Paint_Image(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Image);
    procedure Paint_ImageResource(C: TCanvas; R: TAnyRect; W: TDHVisualItem_ImageResource);
    procedure Paint_Line(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Line);
    procedure BuildAndPaint; //rebuild and repaint
    procedure Modified(Flags: TDHModifiedFlags);

    function GetIsLinkHover: Boolean;
    function GetVisualItemByMousePoint(Point: TAnyPoint): TDHVisualItem;
    procedure CheckMouse(X, Y: TPixels); //check links by mouse position
    procedure SetCursorByLink(Selected: Boolean);
    procedure SetCursor(const Value: TCursor); reintroduce;

    procedure SetLineVertAlign(const Value: TDHVertAlign);
    procedure SetLineHorzAlign(const Value: TDHHorzAlign);
    procedure SetOverallVertAlign(const Value: TDHVertAlign);
    procedure SetOverallHorzAlign(const Value: TDHHorzAlign);
    procedure SetListLevelPadding(const Value: TPixels);

    procedure SetStyleLink(const Index: Integer; const Value: TDHStyleLinkProp);
    procedure SetBorders(const Value: TDHBorders);
    procedure SetOffset(const Value: TDHOffset);
    procedure SetLineSpacing(const Value: TPixels);
    procedure SetParagraphSpacing(const Value: TPixels);
    procedure SetParagraphIndent(const Value: TPixels);
    procedure SetCustomStyles(const Value: TDHCustomStyles);

    procedure SetRightToLeftText(const Value: Boolean);

    {$IFDEF USE_IMGLST}
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}

    {$IFDEF FMX}
    procedure SetFontColor(const Value: TAnyColor);
    procedure SetColor(const Value: TAnyColor);

    procedure OnFontChanged(Sender: TObject);
    {$ELSE}
    procedure SetTransparent(const Value: Boolean);
    procedure UpdateTransparentFlag;
    {$ENDIF}

    procedure SetTextSize(Inner, Outer: TAnySize);

    procedure SetTextSizeAndLineCount(InnerSize, OuterSize: TAnySize; LineCount, ParagraphCount: Integer);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Click; override;
    procedure Resize; override;

    {$IFDEF FMX}
    procedure MouseMove(Shift: TShiftState; X: Single; Y: Single); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Single;
      Y: Single); override;

    procedure DoMouseLeave; override;
    {$ELSE}
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;

    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    {$ENDIF}

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;

    {$IFDEF VCL}
    procedure SetParent(AParent: TWinControl); override;
    {$ENDIF}
  public
    property GeneratePlainText: Boolean read FGeneratePlainText write FGeneratePlainText;
    property PlainText: TStringBuilder read FPlainText;

    property Spoilers: TDHSpoilerList read LSpoiler;
    property LinkRefs: TDHLinkRefList read LLinkRef;

    property SyntaxErrors: TDHSyntaxErrorList read LError;

    {$IFDEF VCL}
    function CalcMulDiv(Size: Integer): Integer;
    function CalcFontHeight(Size: Integer): Integer;
    {$ENDIF}
    function CalcScale(Size: TPixels): TPixels;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsLinkHover: Boolean read GetIsLinkHover;
    property SelectedLink: TDHBaseLink read FSelectedLink;

    procedure Rebuild; //rebuild words
    procedure CallTokenEngine;
    {$IFDEF FMX}
    procedure FMXCallTokenEngine;
    {$ENDIF}

    procedure BeginUpdate; {$IFDEF FMX}reintroduce;{$ENDIF}
    procedure EndUpdate(ForceRepaint: Boolean = True); {$IFDEF FMX}reintroduce;{$ENDIF}

    property Text: string read GetText write SetText;

    class function UnescapeHTMLToText(const aHTML: string): string;
    class function EscapeTextToHTML(const aText: string): string;
  published
    property Align;
    property Anchors;
    property Font;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;

    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;

    {$IFDEF DCC}//Only in Delphi
    property OnGesture;
    {$ENDIF}

    {$IFDEF FMX}
    property Action;
    property AutoTranslate default True;
    property ClipChildren;
    property ClipParent;
    property DragMode;
    property Enabled;
    property EnableDragHighlight;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HitTest;
    property Locked;
    property Margins;
    property Opacity;
    property Padding;
    property RotationAngle;
    property RotationCenter;
    property Scale;
    property StyledSettings;
    property StyleLookup;
    property TabOrder;
    property TouchTargetExpansion;

    property Width;
    property Height;
    property Position;

      {$IF CompilerVersion >= 30} //D10 Seattle
      property ParentShowHint;
      {$ENDIF}

      {$IFDEF USE_NEW_ENV}
      property ControlType;
      property TabStop;
      property Size;
      {$ENDIF}

    property Color: TAnyColor read FColor write SetColor default clNone;
    property FontColor: TAnyColor read FFontColor write SetFontColor default TAlphaColors.Black;
    {$ELSE}//VCL
    property Color;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property OnStartDock;
    property OnStartDrag;
    property OnEndDock;
    property OnEndDrag;
      {$IFDEF DCC}//Only in Delphi
      property OnMouseActivate;
      property StyleElements;
      {$ENDIF}
    {$ENDIF}

    property Lines: TStrings read FLines write SetLines;

    property Cursor: TCursor read FCursor write SetCursor default crDefault;

    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default False;
    property MaxWidth: TPixels read FMaxWidth write SetMaxWidth stored GetStoredMaxWidth;

    property AutoBreak: Boolean read FAutoBreak write SetAutoBreak default True;

    property Offset: TDHOffset read FOffset write SetOffset stored GetStoredOffset;
    property CustomStyles: TDHCustomStyles read FCustomStyles write SetCustomStyles stored GetStoredCustomStyles;

    property StyleLinkNormal: TDHStyleLinkProp index 1 read FStyleLinkNormal write SetStyleLink stored GetStoredStyleLink;
    property StyleLinkHover: TDHStyleLinkProp index 2 read FStyleLinkHover write SetStyleLink stored GetStoredStyleLink;

    {$IFDEF USE_IMGLST}
    property Images: TCustomImageList read FImages write SetImages;
    {$ENDIF}

    {$IFDEF VCL}
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    {$ENDIF}

    property LineCount: Integer read FLineCount;
    property ParagraphCount: Integer read FParagraphCount;
    property TextWidth: TPixels read FTextWidth;
    property TextHeight: TPixels read FTextHeight;

    property OnLinkEnter: TDHEvLink read FOnLinkEnter write FOnLinkEnter;
    property OnLinkLeave: TDHEvLink read FOnLinkLeave write FOnLinkLeave;
    property OnLinkClick: TDHEvLinkClick read FOnLinkClick write FOnLinkClick;
    property OnLinkRightClick: TDHEvLinkClick read FOnLinkRightClick write FOnLinkRightClick;

    property OnRetrieveImgRes: TDHEvRetrieveImgRes read FOnRetrieveImgRes write FOnRetrieveImgRes;

    property AutoOpenLink: Boolean read FAutoOpenLink write FAutoOpenLink default True;

    property LineVertAlign: TDHVertAlign read FLineVertAlign write SetLineVertAlign default vaTop;
    property LineHorzAlign: TDHHorzAlign read FLineHorzAlign write SetLineHorzAlign default haLeft;
    property OverallVertAlign: TDHVertAlign read FOverallVertAlign write SetOverallVertAlign default vaTop;
    property OverallHorzAlign: TDHHorzAlign read FOverallHorzAlign write SetOverallHorzAlign default haLeft;
    property ListLevelPadding: TPixels read FListLevelPadding write SetListLevelPadding stored GetStoredListLevelPadding;

    property Borders: TDHBorders read FBorders write SetBorders stored GetStoredBorders;

    property LineSpacing: TPixels read FLineSpacing write SetLineSpacing {$IFDEF VCL}default 0{$ENDIF};
    property ParagraphSpacing: TPixels read FParagraphSpacing write SetParagraphSpacing {$IFDEF VCL}default 0{$ENDIF};
    property ParagraphIndent: TPixels read FParagraphIndent write SetParagraphIndent {$IFDEF VCL}default 0{$ENDIF};

    property RightToLeftText: Boolean read FRightToLeftText write SetRightToLeftText default False;

    property About: string read FAbout;
  end;

  EDHInternalExcept = class(Exception)
  public
    constructor Create(const Msg: string);
  end;

implementation

uses
{$IFDEF FMX}
  FMX.DHTokenEngine, FMX.DHCommon
{$ELSE}
  Vcl.DHTokenEngine, Vcl.DHCommon
{$ENDIF}
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}, Windows{$ENDIF}, StrUtils, LResources, Variants
{$ELSE}
  , System.StrUtils, System.Variants
  {$IFDEF FMX}
    , System.UIConsts
    {$IF Defined(ANDROID)}
    , Androidapi.JNI.GraphicsContentViewText
    , Androidapi.Helpers
    {$ELSEIF Defined(IOS)}
    , macapi.helpers, FMX.helpers.iOS
    {$ELSEIF Defined(MACOS)}
    , Posix.Stdlib
    {$ENDIF}
  {$ELSE}
    , System.UITypes, Vcl.Themes
  {$ENDIF}
  {$IFDEF MSWINDOWS}
  , Winapi.Windows, Winapi.ShellAPI
  {$ENDIF}
{$ENDIF}
{$IFDEF USE_GDI}
  , Winapi.GDIPOBJ, Winapi.GDIPAPI
{$ENDIF};

const STR_VERSION = '6.8';

const DEFAULT_PPI = 96;

{$REGION 'EDHInternalExcept'}
constructor EDHInternalExcept.Create(const Msg: string);
begin
  inherited CreateFmt('%s internal error: %s', [TDzHTMLText.ClassName, Msg]);
end;
{$ENDREGION}

{$REGION 'TDHSyntaxError'}
constructor TDHSyntaxError.Create(Position: Integer; const Description: string);
begin
  FPosition := Position;
  FDescription := Description;
end;
{$ENDREGION}

{$REGION 'TDHBaseLink'}
function TDHBaseLink.GetKind: TDHLinkKind;
begin
  if Self is TDHLinkRef then Result := lkLinkRef else
  if Self is TDHSpoiler then Result := lkSpoiler else
    raise EDHInternalExcept.Create('Invalid link kind');
end;

function TDHBaseLink.GetLinkRef: TDHLinkRef;
begin
  if Self is TDHLinkRef then
    Result := TDHLinkRef(Self)
  else
    Result := nil;
end;

function TDHBaseLink.GetSpoiler: TDHSpoiler;
begin
  if Self is TDHSpoiler then
    Result := TDHSpoiler(Self)
  else
    Result := nil;
end;
{$ENDREGION}

{$REGION 'TDHSpoilerList'}
function TDHSpoilerList.Find(const Name: string): TDHSpoiler;
var
  DHSpoiler: TDHSpoiler;
begin
  for DHSpoiler in Self do
    if SameText(DHSpoiler.FName, Name) then Exit(DHSpoiler);

  Result := nil;
end;
{$ENDREGION}

{$REGION 'TDHVisualItem_Word'}
constructor TDHVisualItem_Word.Create;
begin
  inherited;
  Font := TFont.Create;
end;

destructor TDHVisualItem_Word.Destroy;
begin
  Font.Free;
  inherited;
end;
{$ENDREGION}

{$REGION 'TDHVisualItem_ImageResource'}
constructor TDHVisualItem_ImageResource.Create;
begin
  inherited;
  Picture := TAnyPicture.Create{$IFNDEF USE_NEW_ENV}(0, 0){$ENDIF};
end;

destructor TDHVisualItem_ImageResource.Destroy;
begin
  Picture.Free;
  inherited;
end;

procedure TDHVisualItem_ImageResource.Load(Lb: TDzHTMLText; const ResourceName: string);
{$IFDEF VCL}
type TPNG={$IFDEF FPC}TPortableNetworkGraphic{$ELSE}TPngImage{$ENDIF};
{$ENDIF}
var
  Handled: Boolean;
{$IFDEF FMX}
  Res: TResourceStream;
{$ELSE}
  PNG: TPNG;
{$ENDIF}
begin
  if csDesigning in Lb.ComponentState then Exit;

  Handled := False;
  if Assigned(Lb.FOnRetrieveImgRes) then
    Lb.FOnRetrieveImgRes(Lb, ResourceName, Picture, Handled);

  if not Handled then
  begin
    try
      {$IFDEF FMX}
      Res := TResourceStream.Create(HInstance, ResourceName, RT_RCDATA);
      try
        Picture.LoadFromStream(Res);
      finally
        Res.Free;
      end;
      {$ELSE}
      PNG := TPNG.Create;
      try
        PNG.LoadFromResourceName(HInstance, ResourceName);
        Picture.Assign(PNG);
      finally
        PNG.Free;
      end;
      {$ENDIF}
    except
      //resource not found or invalid
    end;
  end;
end;
{$ENDREGION}

{$REGION 'TDzHTMLText class functions'}
class function TDzHTMLText.EscapeTextToHTML(const aText: string): string;
begin
  Result := aText;

  Result := StringReplace(Result, '&', '&amp;', [rfReplaceAll]);

  Result := StringReplace(Result, '<', '&lt;', [rfReplaceAll]);
  Result := StringReplace(Result, '>', '&gt;', [rfReplaceAll]);
end;

class function TDzHTMLText.UnescapeHTMLToText(const aHTML: string): string;
begin
  Result := aHTML;

  Result := StringReplace(Result, '&lt;', '<', [rfReplaceAll]);
  Result := StringReplace(Result, '&gt;', '>', [rfReplaceAll]);

  Result := StringReplace(Result, '&amp;', '&', [rfReplaceAll]);
end;
{$ENDREGION}

{$REGION 'TDzHTMLText'}
constructor TDzHTMLText.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF VCL}
  UpdateTransparentFlag;
  {$ENDIF}

  FAbout := 'Digao Dalpiaz / Version '+STR_VERSION;

  FLines := TStringList.Create;
  //FLines.TrailingLineBreak := False; -- only supported by Delphi 10.1 and not full functional in Lazarus
  TStringList(FLines).OnChange := OnLinesChange;

  FStyleLinkNormal := TDHStyleLinkProp.Create(Self, tslpNormal);
  FStyleLinkHover := TDHStyleLinkProp.Create(Self, tslpHover);
  VisualItems := TDHVisualItemList.Create;
  LLinkRef := TDHLinkRefList.Create;
  LSpoiler := TDHSpoilerList.Create;
  LError := TDHSyntaxErrorList.Create;

  FAutoBreak := True;
  FAutoOpenLink := True;
  FListLevelPadding := _DEF_LISTLEVELPADDING;

  FBorders := TDHBorders.Create(Self);
  FOffset := TDHOffset.Create(Self);
  FCustomStyles := TDHCustomStyles.Create(Self);

  FPlainText := TStringBuilder.Create;

  FCursor := crDefault;

  {$IFDEF FMX}
  FColor := clNone;
  FFontColor := TAlphaColors.Black;

  Font.OnChanged := OnFontChanged;

  AutoTranslate := True;
  {$ENDIF}

  {$IFDEF FPC}
  //Lazarus object starts too small
  Width := 200;
  Height := 100;
  {$ENDIF}
end;

destructor TDzHTMLText.Destroy;
begin
  FLines.Free;
  FStyleLinkNormal.Free;
  FStyleLinkHover.Free;
  FBorders.Free;
  FOffset.Free;
  FCustomStyles.Free;
  VisualItems.Free;
  LLinkRef.Free;
  LSpoiler.Free;
  LError.Free;
  FPlainText.Free;
  inherited;
end;

procedure TDzHTMLText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
  begin
    {$IFDEF USE_IMGLST}
    if AComponent = FImages then FImages := nil;
    {$ENDIF}
  end;
end;

{$IFDEF VCL}
procedure TDzHTMLText.SetParent(AParent: TWinControl);
begin
  inherited;
  ParentForm := GetParentForm(Self);
end;
{$ENDIF}

{$IFDEF USE_IMGLST}
procedure TDzHTMLText.SetImages(const Value: TCustomImageList);
begin
  if Value <> FImages then
  begin
    FImages := Value;
    if FImages <> nil then
      FImages.FreeNotification(Self);

    BuildAndPaint;
  end;
end;
{$ENDIF}

procedure TDzHTMLText.Loaded;
begin
  {Warning! When a component is inserted at design-time, the Loaded
  is not fired, because there is nothing to load. The Loaded is only fired
  when loading component that already has saved properties on DFM file.}
  inherited;
  Rebuild;
end;

procedure TDzHTMLText.Modified(Flags: TDHModifiedFlags);
begin
  if UpdatingSemaphore>0 then Exit;

  if mfBuild in Flags then Rebuild;
  if mfPaint in Flags then
    {$IFDEF FMX}InvalidateRect(LocalRect){$ELSE}Invalidate{$ENDIF};
end;

procedure TDzHTMLText.BuildAndPaint;
begin
  //Rebuild words and repaint
  Modified([mfBuild, mfPaint]);
end;

procedure TDzHTMLText.SetAutoHeight(const Value: Boolean);
begin
  if Value<>FAutoHeight then
  begin
    FAutoHeight := Value;

    if Value then Modified([mfBuild]);
  end;
end;

procedure TDzHTMLText.SetAutoWidth(const Value: Boolean);
begin
  if Value<>FAutoWidth then
  begin
    FAutoWidth := Value;

    if Value then Modified([mfBuild]);
  end;
end;

procedure TDzHTMLText.SetMaxWidth(const Value: TPixels);
begin
  if Value<>FMaxWidth then
  begin
    FMaxWidth := Value;

    Modified([mfBuild]);
  end;
end;

procedure TDzHTMLText.OnLinesChange(Sender: TObject);
begin
  LSpoiler.Clear;
  BuildAndPaint;
end;

procedure TDzHTMLText.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

function TDzHTMLText.GetText: string;
begin
  Result := FLines.Text;
  Result := Result.Substring(0, Result.Length-FLines.LineBreak.Length); //remove last line break
end;

procedure TDzHTMLText.SetText(const Value: string);
begin
  FLines.Text := Value;
end;

procedure TDzHTMLText.SetAutoBreak(const Value: Boolean);
begin
  if Value<>FAutoBreak then
  begin
    FAutoBreak := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetLineVertAlign(const Value: TDHVertAlign);
begin
  if Value<>FLineVertAlign then
  begin
    FLineVertAlign := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetLineHorzAlign(const Value: TDHHorzAlign);
begin
  if Value<>FLineHorzAlign then
  begin
    FLineHorzAlign := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetOverallVertAlign(const Value: TDHVertAlign);
begin
  if Value<>FOverallVertAlign then
  begin
    FOverallVertAlign := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetOverallHorzAlign(const Value: TDHHorzAlign);
begin
  if Value<>FOverallHorzAlign then
  begin
    FOverallHorzAlign := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetLineSpacing(const Value: TPixels);
begin
  if Value<>FLineSpacing then
  begin
    FLineSpacing := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetParagraphSpacing(const Value: TPixels);
begin
  if Value<>FParagraphSpacing then
  begin
    FParagraphSpacing := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetParagraphIndent(const Value: TPixels);
begin
  if Value<>FParagraphIndent then
  begin
    FParagraphIndent := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetRightToLeftText(const Value: Boolean);
begin
  if Value<>FRightToLeftText then
  begin
    FRightToLeftText := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetListLevelPadding(const Value: TPixels);
begin
  if Value<>FListLevelPadding then
  begin
    FListLevelPadding := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetStyleLink(const Index: Integer;
  const Value: TDHStyleLinkProp);
begin
  case Index of
    1: FStyleLinkNormal.Assign(Value);
    2: FStyleLinkHover.Assign(Value);
  end;
end;

procedure TDzHTMLText.SetBorders(const Value: TDHBorders);
begin
  FBorders.Assign(Value);
end;

procedure TDzHTMLText.SetOffset(const Value: TDHOffset);
begin
  FOffset.Assign(Value);
end;

procedure TDzHTMLText.SetCustomStyles(const Value: TDHCustomStyles);
begin
  FCustomStyles.Assign(Value);
end;

procedure TDzHTMLText.BeginUpdate;
begin
  Inc(UpdatingSemaphore);
end;

procedure TDzHTMLText.EndUpdate(ForceRepaint: Boolean = True);
begin
  if UpdatingSemaphore=0 then
    raise Exception.Create('There is no update started'); //standard exception

  Dec(UpdatingSemaphore);
  if ForceRepaint and (UpdatingSemaphore=0) then
    BuildAndPaint;
end;

{$IFDEF VCL}
procedure TDzHTMLText.CMColorchanged(var Message: TMessage);
begin
  Modified([mfPaint]);
end;

procedure TDzHTMLText.CMFontchanged(var Message: TMessage);
begin
  BuildAndPaint;
end;
{$ENDIF}

{$IFDEF FMX}
procedure TDzHTMLText.SetColor(const Value: TAnyColor);
begin
  if Value<>FColor then
  begin
    FColor := Value;

    Modified([mfPaint]);
  end;
end;

procedure TDzHTMLText.SetFontColor(const Value: TAnyColor);
begin
  if Value<>FFontColor then
  begin
    FFontColor := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.OnFontChanged(Sender: TObject);
begin
  BuildAndPaint;
end;
{$ELSE}
procedure TDzHTMLText.SetTransparent(const Value: Boolean);
begin
  if Value <> FTransparent then
  begin
    FTransparent := Value;

    UpdateTransparentFlag;
    Modified([mfPaint]);
  end;
end;

procedure TDzHTMLText.UpdateTransparentFlag;
begin
  if FTransparent then
    ControlStyle := ControlStyle - [csOpaque]
  else
    ControlStyle := ControlStyle + [csOpaque];
end;
{$ENDIF}

procedure TDzHTMLText.SetTextSize(Inner, Outer: TAnySize);
begin
  FTextWidth := Inner.Width;
  FTextHeight := Inner.Height;

  InternalResizing := True;
  try
    if FAutoWidth then Width := Outer.Width;
    if FAutoHeight then Height := Outer.Height;
  finally
    InternalResizing := False;
  end;
end;

procedure TDzHTMLText.SetTextSizeAndLineCount(InnerSize, OuterSize: TAnySize; LineCount, ParagraphCount: Integer);
begin
  SetTextSize(InnerSize, OuterSize);
  FLineCount := LineCount;
  FParagraphCount := ParagraphCount;
end;

procedure TDzHTMLText.Resize;
begin
  if InternalResizing then Exit;

  //on component creating, there is no parent and the resize is fired,
  //so, the canvas is not present at this moment.
  if HasParent and Assigned(Canvas) then
    Modified([mfBuild]);

  inherited;
end;

procedure TDzHTMLText.Paint;
begin
  inherited;

  {$IFDEF FMX}
  if not FirstRebuild then Rebuild;
  {$ENDIF}

  ExecPaint;
end;

procedure TDzHTMLText.ExecPaint;
var
{$IFDEF VCL}
  B: TAnyBitmap;
{$ELSE}
  State: TCanvasSaveState;
{$ENDIF}
begin
  {$IFDEF VCL}
  if FTransparent then
    CanvasProcess(Canvas)
  else
  begin
    //Using internal bitmap as a buffer to reduce flickering
    B := TAnyBitmap.Create;
    try
      B.SetSize(Width, Height);
      CanvasProcess(B.Canvas);
      Canvas.Draw(0, 0, B);
    finally
      B.Free;
    end;
  end;
  {$ELSE}
  //In FMX, Paint method calls BeginScene/EndScene automatically,
  //so there is no need for Bitmap and there is no concern about flickering.
  State := Canvas.SaveState;
  try
    CanvasProcess(Canvas);
  finally
    Canvas.RestoreState(State); //keep original canvas properties (FMX shares canvas with other controls)
  end;
  {$ENDIF}
end;

procedure TDzHTMLText.CanvasProcess(C: TCanvas);
var
  W: TDHVisualItem;
{$IFDEF FMX}
  RectDsn: TRectF;
  ColorDsn: TAlphaColor;
{$ENDIF}
begin
  //draw background color
  {$IFDEF FMX}
  if Color<>clNone then
  begin
    C.Fill.Color := FColor;
    C.FillRect(LocalRect, 0, 0, [], Opacity);
  end;
  {$ELSE}
  if not FTransparent then
  begin
    {$IFDEF FPC}
    if (Color=clDefault) and (ParentColor) then
      C.Brush.Color := GetColorresolvingParent
    else
    {$ELSE}
    if TStyleManager.IsCustomStyleActive and (seClient in StyleElements) and not (csDesigning in ComponentState) then
      C.Brush.Color := TStyleManager.ActiveStyle.GetStyleColor(TStyleColor.scWindow)
    else
    {$ENDIF}
      C.Brush.Color := Color;

    C.FillRect(ClientRect);
  end;
  {$ENDIF}

  if csDesigning in ComponentState then
  begin
    {$IFDEF FMX}
    if not FLocked and not FInPaintTo then
    begin
      RectDsn := LocalRect;
      System.Types.InflateRect(RectDsn, -0.5, -0.5);
      if LError.Count>0 then ColorDsn := TAlphaColors.Red else ColorDsn := $A0909090;
      C.DrawDashRect(RectDsn, 0, 0, AllCorners, AbsoluteOpacity, ColorDsn);
    end;
    {$ELSE}
    C.Pen.Style := psDot;
    C.Pen.Width := 1;
    if LError.Count>0 then C.Pen.Color := clRed else C.Pen.Color := clBtnShadow;
    C.Brush.Style := bsClear;
    C.Rectangle(ClientRect);
    {$ENDIF}
  end;

  for W in VisualItems do
    Paint_VisualItem(W, C);
end;

procedure TDzHTMLText.Paint_VisualItem(W: TDHVisualItem; C: TCanvas);
var
  R: TAnyRect;
begin
  R := W.Rect;

  DefineFillColor(C, W.BColor);

  if W is TDHVisualItem_Word then
  begin
    C.Font.Assign(TDHVisualItem_Word(W).Font);
    {$IFDEF FMX}
    C.Stroke.Color := TDHVisualItem_Word(W).FontColor;
    {$ENDIF}
  end;

  if Assigned(W.Link) then
  begin
    if FSelectedLink = W.Link then //selected
      FStyleLinkHover.SetPropsToCanvas(C)
    else
      FStyleLinkNormal.SetPropsToCanvas(C);
  end;

  if GetGenericFillColor(C)<>clNone then GenericFillRect(Self, C, R, True);

  if W is TDHVisualItem_Div then
    Paint_Div(C, R, TDHVisualItem_Div(W))
  else
  if W is TDHVisualItem_Word then
    Paint_Word(C, R, TDHVisualItem_Word(W))
  else
  if W is TDHVisualItem_Image then
    Paint_Image(C, R, TDHVisualItem_Image(W))
  else
  if W is TDHVisualItem_ImageResource then
    Paint_ImageResource(C, R, TDHVisualItem_ImageResource(W))
  else
  if W is TDHVisualItem_Line then
    Paint_Line(C, R, TDHVisualItem_Line(W))
  else
    raise EDHInternalExcept.Create('Invalid visual item object');
end;

{$IFDEF USE_GDI}
function ToGPColor(Color: TColor): TGPColor;
var
  ColRef: COLORREF;
begin
  ColRef := ColorToRGB(Color);
  Result := MakeColor(GetRValue(ColRef), GetGValue(ColRef), GetBValue(ColRef));
end;

procedure PaintRoundRectangleUsingWindowsGDI(Canvas: TCanvas; Thick, Radius: Single; Rect: TRect; PenColor, BrushColor: TColor);
var
  Gpx: TGPGraphics;
  Pen: TGPPen;
  Path: TGPGraphicsPath;
  Brush: TGPSolidBrush;
begin
  Gpx := TGPGraphics.Create(Canvas.Handle);
  Pen := TGPPen.Create(ToGPColor(PenColor), Thick);
  Brush := TGPSolidBrush.Create(ToGPColor(BrushColor));
  Path := TGPGraphicsPath.Create;
  try
    Gpx.SetSmoothingMode(SmoothingModeAntiAlias);

    Path.AddArc(Rect.Left, Rect.Top, Radius, Radius, 180, 90);
    Path.AddArc(Rect.Left + Rect.Width - Radius, Rect.Top, Radius, Radius, 270, 90);
    Path.AddArc(Rect.Left + Rect.Width - Radius, Rect.Top + Rect.Height - Radius, Radius, Radius, 0, 90);
    Path.AddArc(Rect.Left, Rect.Top + Rect.Height - Radius, Radius, Radius, 90, 90);
    Path.CloseFigure;

    if BrushColor<>clNone then Gpx.FillPath(Brush, Path);
    if (PenColor<>clNone) and (Thick>0) then Gpx.DrawPath(Pen, Path);
  finally
    Path.Free;
    Brush.Free;
    Pen.Free;
    Gpx.Free;
  end;
end;
{$ENDIF}

procedure TDzHTMLText.Paint_Div(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Div);

  procedure PaintSide(var Side: TDHDivBorderLineAttrRec; X, Y, W, H: TPixels);
  begin
    if (Side.Thick=0) or (Side.Color=clNone) then Exit;

    DefineFillColor(C, Side.Color);
    GenericFillRect(Self, C, TAnyRect.Create(TAnyPoint.Create(R.Left+X, R.Top+Y), W, H));
  end;

begin
  if W.OuterColor<>clNone then
  begin
    DefineFillColor(C, W.OuterColor);
    GenericFillRect(Self, C, R);
  end;

  R.Left := R.Left + W.Left.Pad;
  R.Top := R.Top + W.Top.Pad;
  R.Right := R.Right - W.Right.Pad;
  R.Bottom := R.Bottom - W.Bottom.Pad;

  if W.CornerRadius>0 then
  begin
   {$IFDEF USE_GDI}
    PaintRoundRectangleUsingWindowsGDI(C, W.Left.Thick, W.CornerRadius, R, W.Left.Color, W.InnerColor);
   {$ELSE}
    if (W.Left.Thick>0) and (W.Left.Color<>clNone) then
    begin
      {$IFDEF FMX}
      C.Stroke.Thickness := W.Left.Thick;
      C.Stroke.Color := W.Left.Color;
      C.Stroke.Kind := TBrushKind.{$IF CompilerVersion >= 27}{XE6}Solid{$ELSE}bkSolid{$ENDIF};
      {$ELSE}
      C.Pen.Width := W.Left.Thick;
      C.Pen.Color := W.Left.Color;
      C.Pen.Style := psSolid;
      {$ENDIF}
    end else
    begin
      {$IFDEF FMX}
      C.Stroke.Kind := TBrushKind.{$IF CompilerVersion >= 27}{XE6}None{$ELSE}bkNone{$ENDIF};
      {$ELSE}
      C.Pen.Style := psClear;
      {$ENDIF}
    end;

    DefineFillColor(C, W.InnerColor);

    {$IFDEF FMX}
    C.FillRect(R, W.CornerRadius, W.CornerRadius, AllCorners, Opacity); //backgound
    C.DrawRect(R, W.CornerRadius, W.CornerRadius, AllCorners, Opacity); //border
    {$ELSE}
    C.RoundRect(R, W.CornerRadius, W.CornerRadius);
    {$ENDIF}
   {$ENDIF}
  end else
  begin
    if W.InnerColor<>clNone then
    begin
      DefineFillColor(C, W.InnerColor);
      GenericFillRect(Self, C, R);
    end;

    PaintSide(W.Left, 0, 0, W.Left.Thick, R.Height);
    PaintSide(W.Top, 0, 0, R.Width, W.Top.Thick);
    PaintSide(W.Right, R.Width-W.Right.Thick, 0, W.Right.Thick, R.Height);
    PaintSide(W.Bottom, 0, R.Height-W.Bottom.Thick, R.Width, W.Bottom.Thick);
  end;
end;

procedure TDzHTMLText.Paint_Word(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Word);
begin
  R.Top := R.Top + W.YPos;

  {$IFDEF FMX}
  C.Fill.Color := C.Stroke.Color;
  C.FillText(R, W.Text, False, Opacity, [],
    TTextAlign.{$IF CompilerVersion >= 27}{XE6}Leading{$ELSE}taLeading{$ENDIF});
  {$ELSE}
  C.Brush.Style := bsClear;
    {$IFDEF MSWINDOWS}
    DrawTextW(C.Handle,
      PWideChar({$IFDEF FPC}UnicodeString(W.Text){$ELSE}W.Text{$ENDIF}),
      -1, R, DT_NOCLIP or DT_NOPREFIX);
    {Using DrawText, because TextOut has no clip option, which causes
    bad overload of text when painting using background, oversizing the
    text area wildly.}
    {$ELSE}
    //FPC Linux
    C.TextOut(R.Left, R.Top, W.Text);
    {$ENDIF}
  {$ENDIF}
end;

procedure TDzHTMLText.Paint_Image(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Image);
{$IF Defined(VCL) and Defined(DCC)}
var
  Icon: TIcon;
{$ENDIF}
begin
  {$IFDEF USE_IMGLST}
  if Assigned(FImages) then
  begin
    {$IFDEF FMX}
    FImages.Draw(C, R, W.ImageIndex, Opacity);
    {$ELSE}
      {$IFDEF FPC}
      FImages.StretchDraw(C, W.ImageIndex, R);
      {$ELSE}
      Icon := TIcon.Create;
      try
        FImages.GetIcon(W.ImageIndex, Icon);
        DrawIconEx(C.Handle, R.Left, R.Top, Icon.Handle, R.Width, R.Height, 0, 0, DI_NORMAL); //Windows only
      finally
        Icon.Free;
      end;
      {$ENDIF}
    {$ENDIF}
  end;
  {$ENDIF}
end;

procedure TDzHTMLText.Paint_ImageResource(C: TCanvas; R: TAnyRect; W: TDHVisualItem_ImageResource);
begin
  {$IFDEF FMX}
  C.DrawBitmap(W.Picture, TAnyRect.Create(0, 0, W.Picture.Width, W.Picture.Height), R, Opacity);
  {$ELSE}
  C.StretchDraw(R, W.Picture.Graphic);
  {$ENDIF}
end;

procedure TDzHTMLText.Paint_Line(C: TCanvas; R: TAnyRect; W: TDHVisualItem_Line);
begin
  if W.ColorAlt <> clNone then
    R.Height := RoundIfVCL(R.Height / 2); //half height when double color

  DefineFillColor(C, W.Color);
  GenericFillRect(Self, C, R);

  if W.ColorAlt <> clNone then
  begin
    R.Offset(0, R.Height);

    DefineFillColor(C, W.ColorAlt);
    GenericFillRect(Self, C, R);
  end;
end;

procedure TDzHTMLText.SetCursor(const Value: TCursor);
begin
  if Value<>FCursor then
  begin
    FCursor := Value;
    inherited Cursor := Value;
  end;
end;

function TDzHTMLText.GetIsLinkHover: Boolean;
begin
  Result := Assigned(FSelectedLink);
end;

function TDzHTMLText.GetVisualItemByMousePoint(Point: TAnyPoint): TDHVisualItem;
var
  W: TDHVisualItem;
begin
  for W in VisualItems do
    if (W.Link<>nil) and W.Rect.Contains(Point) then Exit(W);

  Result := nil;
end;

procedure TDzHTMLText.CheckMouse(X, Y: TPixels);
var
  Link: TDHBaseLink;
  W: TDHVisualItem;
begin
  Link := nil;
  W := GetVisualItemByMousePoint(TAnyPoint.Create(X, Y));
  if W<>nil then Link := W.Link;

  if Link <> FSelectedLink then //changed
  begin
    if Assigned(Link) then //enter the link
    begin
      SetCursorByLink(True);
      FSelectedLink := Link;
      if Assigned(FOnLinkEnter) then
        FOnLinkEnter(Self, Link);
    end else
    begin //leave the link
      SetCursorByLink(False);
      Link := FSelectedLink; //save to use on OnLinkLeave event
      FSelectedLink := nil;
      if Assigned(FOnLinkLeave) then
        FOnLinkLeave(Self, Link);
    end;

    {$IFDEF FMX}
    InvalidateRect(LocalRect);
    {$ELSE}
    Invalidate;
    {$ENDIF}
  end;
end;

procedure TDzHTMLText.SetCursorByLink(Selected: Boolean);
begin
  if Selected then
    inherited Cursor := crHandPoint
  else
    inherited Cursor := FCursor;
end;

procedure TDzHTMLText.Click;
var
  Handled: Boolean;
  aTarget: string;
  Link: TDHBaseLink;
begin
  Link := FSelectedLink;
  if Assigned(Link) then
  begin
    Handled := False;
    if Assigned(FOnLinkClick) then
      FOnLinkClick(Self, Link, Handled);

    if not Handled then
    begin
      if Link is TDHLinkRef then
      begin
        if FAutoOpenLink then
        begin
          aTarget := TDHLinkRef(Link).FTarget;
          if not aTarget.IsEmpty then
          begin
            {$IF Defined(MSWINDOWS)}
            ShellExecute(0, 'open', PChar(aTarget), '', '', SW_SHOWNORMAL);
            {$ELSEIF Defined(FPC)}
            if aTarget.StartsWith('http://', True)
              or aTarget.StartsWith('https://', True)
              or aTarget.StartsWith('www.', True)
            then
              OpenURL(aTarget)
            else
              OpenDocument(aTarget);
            {$ELSEIF Defined(ANDROID)}
            try
              TAndroidHelper.Activity.startActivity(
                TJIntent.Create
                  .setAction(TJIntent.JavaClass.ACTION_VIEW)
                  .setData(StrToJURI(aTarget))
              );
            except
              on E: EJNIException do
                if not E.ExceptionClassName.Contains('ActivityNotFoundException') then raise;
            end;
            {$ELSEIF Defined(IOS)}
              SharedApplication.OpenURL(StrToNSUrl(aTarget));
            {$ELSEIF Defined(MACOS)}
              _system(PAnsiChar('open ' + AnsiString(aTarget)));
            {$ELSE}
            raise EInternalExcept.Create('Unsupported platform');
            {$ENDIF}
          end;
        end;
      end else
      if Link is TDHSpoiler then
      begin
        TDHSpoiler(Link).FExpanded :=
          not TDHSpoiler(Link).FExpanded;

        BuildAndPaint;
      end else
        raise EDHInternalExcept.Create('Invalid link object');
    end;
  end;

  inherited;
end;

procedure TDzHTMLText.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF});
var
  Handled: Boolean;
begin
  if Button = TMouseButton.mbRight then
    if IsLinkHover then
      if Assigned(FOnLinkRightClick) then
      begin
        Handled := False;
        FOnLinkRightClick(Self, FSelectedLink, Handled);
      end;

  inherited;
end;

procedure TDzHTMLText.MouseMove(Shift: TShiftState;
  X, Y: {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF});
begin
  CheckMouse(X, Y);

  inherited;
end;

{$IFDEF FMX}
procedure TDzHTMLText.DoMouseLeave;
{$ELSE}
procedure TDzHTMLText.CMMouseleave(var Message: TMessage);
{$ENDIF}
begin
  //Mouse leaves the component
  CheckMouse(-1, -1);

  inherited;
end;

{$IFDEF VCL}
  {$IF (Defined(DCC) and (CompilerVersion >= 30)) or Defined(FPC)} //D10 Seattle or Lazarus
    {$DEFINE PPI_SCALING}
  {$ENDIF}
type THackForm = class(TCustomForm); //only in Delphi 11 the property "Scaled" is public (before is protected)
function TDzHTMLText.CalcMulDiv(Size: Integer): Integer;
{$IFDEF PPI_SCALING}
var
  MonitorPPI, DesignPPI: Integer;
{$ENDIF}
begin
  {$IFDEF PPI_SCALING}
  if (ParentForm<>nil) and THackForm(ParentForm).Scaled and (ParentForm.Monitor<>nil)
    {$IFDEF DCC}and not (csDesigning in ComponentState){$ENDIF} //design always based on Default PPI in Delphi
  then
  begin
    MonitorPPI := ParentForm.Monitor.PixelsPerInch;
    DesignPPI := {$IFDEF FPC}ParentForm.PixelsPerInch{$ELSE}DEFAULT_PPI{$ENDIF};
    //in Delphi, form PixelsPerInch changes by current monitor
    //in Lazarus, form PixelsPerInch stay fixed by designer (default screen)

    Result := Round(Size * MonitorPPI / DesignPPI); //MulDiv equivalent
  end else
  {$ENDIF}
    Result := Size;
end;

function TDzHTMLText.CalcFontHeight(Size: Integer): Integer;
var
  H: Integer;
begin
  H := -Round(Size * Screen.PixelsPerInch / 72);
  Result := CalcMulDiv(H);
end;
{$ENDIF}

function TDzHTMLText.CalcScale(Size: TPixels): TPixels;
begin
  Result :=
  {$IFDEF VCL}
    CalcMulDiv(Size)
  {$ELSE}
    Size
  {$ENDIF};
end;

function TDzHTMLText.GetStoredStyleLink(const Index: Integer): Boolean;
begin
  Result := False;
  case Index of
    1: Result := FStyleLinkNormal.GetStored;
    2: Result := FStyleLinkHover.GetStored;
  end;
end;

function TDzHTMLText.GetStoredMaxWidth: Boolean;
begin
  Result := FMaxWidth <> 0;
end;

function TDzHTMLText.GetStoredListLevelPadding: Boolean;
begin
  Result := FListLevelPadding <> _DEF_LISTLEVELPADDING;
end;

function TDzHTMLText.GetStoredBorders: Boolean;
begin
  Result := FBorders.HasAnyValue;
end;

function TDzHTMLText.GetStoredOffset: Boolean;
begin
  Result := (FOffset.FTop <> 0) or (FOffset.FBottom <> 0);
end;

function TDzHTMLText.GetStoredCustomStyles: Boolean;
begin
  Result := FCustomStyles.Count>0;
end;

procedure TDzHTMLText.Rebuild;
var
  P: TAnyPoint;
begin
  if csLoading in ComponentState then Exit;

  {$IFDEF FMX}
  //when using component inside a Frame, canvas is not available imediatelly
  //so this var controls when canvas becomes available
  if Canvas = nil then Exit;
  FirstRebuild := True;
  {$ENDIF}

  VisualItems.Clear; //clean visual items
  LLinkRef.Clear; //clean old links
  LError.Clear; //clean syntax errors

  FPlainText.Clear;

  {$IFDEF FMX}FMXCallTokenEngine{$ELSE}CallTokenEngine{$ENDIF};

  //reset selected link
  FSelectedLink := nil;
  SetCursorByLink(False);

  //update link by cursor pos
  P :=
    {$IFDEF FMX}
    ScreenToLocal(Screen.MousePos)
    {$ELSE}
    ScreenToClient(Mouse.CursorPos)
    {$ENDIF};
  CheckMouse(P.X, P.Y);
end;

procedure TDzHTMLText.CallTokenEngine;
var
  B: TDHBuilder;
begin
  B := TDHBuilder.Create(Self, Canvas, VisualItems, SetTextSizeAndLineCount);
  try
    B.Execute;
  finally
    B.Free;
  end;
end;

{$IFDEF FMX}
procedure TDzHTMLText.FMXCallTokenEngine;
var
  OldFont: TFont;
begin
  OldFont := TFont.Create;
  try
    OldFont.Assign(Canvas.Font);
    try
      CallTokenEngine;
    finally
      Canvas.Font.Assign(OldFont); //keep original canvas font (FMX shares canvas with other controls)
    end;
  finally
    OldFont.Free;
  end;
end;
{$ENDIF}
{$ENDREGION}

{$REGION 'TDHStyleLinkProp'}
constructor TDHStyleLinkProp.Create(Lb: TDzHTMLText; Kind: TDHKindStyleLinkProp);
begin
  Self.Lb := Lb;
  Self.Kind := Kind;

  FFontColor := GetDefaultFontColor;
  FBackColor := clNone;
end;

function TDHStyleLinkProp.GetOwner: TPersistent;
begin
  Result := Lb;
end;

function TDHStyleLinkProp.GetDefaultFontColor: TAnyColor;
begin
  Result := clNone;
  case Kind of
    tslpNormal: Result := {$IFDEF FMX}TAlphaColors.Blue{$ELSE}clBlue{$ENDIF};
    tslpHover: Result := {$IFDEF FMX}TAlphaColors.Red{$ELSE}clRed{$ENDIF};
  end;
end;

function TDHStyleLinkProp.GetStoredFontColor: Boolean;
begin
  Result := FFontColor<>GetDefaultFontColor;
end;

procedure TDHStyleLinkProp.SetFontColor(const Value: TAnyColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetBackColor(const Value: TAnyColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetUnderline(const Value: Boolean);
begin
  if Value <> FUnderline then
  begin
    FUnderline := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetPropsToCanvas(C: TCanvas);
begin
  if FFontColor<>clNone then DefineFontColor(C, FFontColor);
  if FBackColor<>clNone then DefineFillColor(C, FBackColor);
  if FUnderline then C.Font.Style := C.Font.Style + [TMyFontStyle.fsUnderline];
end;

procedure TDHStyleLinkProp.Assign(Source: TPersistent);
var
  P: TDHStyleLinkProp;
begin
  if not (Source is TDHStyleLinkProp) then
    raise Exception.CreateFmt('Could not assign %s class', [Source.ClassName]);

  P := TDHStyleLinkProp(Source);

  FFontColor := P.FFontColor;
  FBackColor := P.FBackColor;
  FUnderline := P.FUnderline;
end;

function TDHStyleLinkProp.GetStored: Boolean;
begin
  Result := GetStoredFontColor
         or FUnderline
         or (FBackColor<>clNone);
end;
{$ENDREGION}

{$REGION 'TDHBorders'}
constructor TDHBorders.Create(Lb: TDzHTMLText);
begin
  Self.Lb := Lb;
end;

function TDHBorders.GetOwner: TPersistent;
begin
  Result := Lb;
end;

procedure TDHBorders.Assign(Source: TPersistent);
var
  P: TDHBorders;
begin
  if not (Source is TDHBorders) then
    raise Exception.CreateFmt('Could not assign %s class', [Source.ClassName]);

  P := TDHBorders(Source);

  FLeft := P.FLeft;
  FTop := P.FTop;
  FRight := P.FRight;
  FBottom := P.FBottom;
end;

function TDHBorders.HasAnyValue: Boolean;
begin
  Result := not (
      (FLeft = 0) and (FTop = 0) and (FRight = 0) and (FBottom = 0)
    );
end;

function TDHBorders.GetAll: TPixels;
begin
  if (FLeft=FTop) and (FLeft=FRight) and (FLeft=FBottom) then
    Result := FLeft
  else
    Result := 0;
end;

procedure TDHBorders.SetAll(const Value: TPixels);
var
  Changed: Boolean;

  procedure SetProp(var Prop: TPixels);
  begin
    if Value<>Prop then
    begin
      Prop := Value;
      Changed := True;
    end;
  end;

begin
  Changed := False;

  SetProp(FLeft);
  SetProp(FTop);
  SetProp(FRight);
  SetProp(FBottom);

  if Changed then
    Lb.BuildAndPaint;
end;

function TDHBorders.GetStoredAll: Boolean;
begin
  Result := All<>0;
end;

function TDHBorders.GetStoredSides: Boolean;
begin
  Result := HasAnyValue and not GetStoredAll;
end;

procedure TDHBorders.SetLeft(const Value: TPixels);
begin
  if Value <> FLeft then
  begin
    FLeft := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHBorders.SetTop(const Value: TPixels);
begin
  if Value <> FTop then
  begin
    FTop := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHBorders.SetRight(const Value: TPixels);
begin
  if Value <> FRight then
  begin
    FRight := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHBorders.SetBottom(const Value: TPixels);
begin
  if Value <> FBottom then
  begin
    FBottom := Value;

    Lb.BuildAndPaint;
  end;
end;
{$ENDREGION}

{$REGION 'TDHOffset'}
constructor TDHOffset.Create(Lb: TDzHTMLText);
begin
  Self.Lb := Lb;
end;

function TDHOffset.GetOwner: TPersistent;
begin
  Result := Lb;
end;

procedure TDHOffset.Assign(Source: TPersistent);
var
  P: TDHOffset;
begin
  if not (Source is TDHOffset) then
    raise Exception.CreateFmt('Could not assign %s class', [Source.ClassName]);

  P := TDHOffset(Source);

  FTop := P.FTop;
  FBottom := P.FBottom;
end;

procedure TDHOffset.SetTop(const Value: TPixels);
begin
  if Value <> FTop then
  begin
    FTop := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHOffset.SetBottom(const Value: TPixels);
begin
  if Value <> FBottom then
  begin
    FBottom := Value;

    Lb.BuildAndPaint;
  end;
end;
{$ENDREGION}

{$REGION 'TDHCustomStyles'}
constructor TDHCustomStyles.Create(Lb: TDzHTMLText);
begin
  inherited Create(TDHCustomStyle);
  Self.Lb := Lb;
end;

function TDHCustomStyles.GetOwner: TPersistent;
begin
  Result := Lb;
end;

procedure TDHCustomStyles.Update(Item: TCollectionItem);
begin
  Lb.BuildAndPaint;
end;

function TDHCustomStyles.FindByIdent(const Ident: string): TDHCustomStyle;
var
  Item: TCollectionItem;
  Style: TDHCustomStyle;
begin
  for Item in Self do
  begin
    Style := TDHCustomStyle(Item);
    if SameText(Style.FIdent, Ident) then Exit(Style);
  end;

  Result := nil;
end;
{$ENDREGION}

{$REGION 'TDHCustomStyle'}
constructor TDHCustomStyle.Create(Collection: TCollection);
begin
  inherited;

  FFontColor := clNone;
  FBackColor := clNone;

  FOffsetTop := -1;
  FOffsetBottom := -1;

  FLineSpacing := -1;
  FParagraphSpacing := -1;
  FParagraphIndent := -1;
end;

function TDHCustomStyle.GetDisplayName: string;
begin
  Result := FIdent;
end;

procedure TDHCustomStyle.Modified;
begin
  Changed(False);
end;

procedure TDHCustomStyle.SetIdent(const Value: string);
begin
  if Value <> FIdent then
  begin
    FIdent := Value;

    Modified; //style name changed - rebuild because of tag property
  end;
end;

procedure TDHCustomStyle.SetFontName(const Value: string);
begin
  if Value <> FFontName then
  begin
    FFontName := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetFontSize(const Value: TPixels);
begin
  if Value <> FFontSize then
  begin
    FFontSize := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetStyleBold(const Value: TDHCustomStyleBoolValue);
begin
  if Value <> FStyleBold then
  begin
    FStyleBold := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetStyleItalic(const Value: TDHCustomStyleBoolValue);
begin
  if Value <> FStyleItalic then
  begin
    FStyleItalic := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetStyleUnderline(const Value: TDHCustomStyleBoolValue);
begin
  if Value <> FStyleUnderline then
  begin
    FStyleUnderline := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetStyleStrikeout(const Value: TDHCustomStyleBoolValue);
begin
  if Value <> FStyleStrikeout then
  begin
    FStyleStrikeout := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetFontColor(const Value: TAnyColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetBackColor(const Value: TAnyColor);
begin
  if Value <> FBackColor then
  begin
    FBackColor := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetHorzAlign(const Value: TDHCustomStyleHorzAlignValue);
begin
  if Value <> FHorzAlign then
  begin
    FHorzAlign := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetVertAlign(const Value: TDHCustomStyleVertAlignValue);
begin
  if Value <> FVertAlign then
  begin
    FVertAlign := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetOffsetTop(const Value: TPixels);
begin
  if Value <> FOffsetTop then
  begin
    FOffsetTop := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetOffsetBottom(const Value: TPixels);
begin
  if Value <> FOffsetBottom then
  begin
    FOffsetBottom := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetLineSpacing(const Value: TPixels);
begin
  if Value <> FLineSpacing then
  begin
    FLineSpacing := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetParagraphSpacing(const Value: TPixels);
begin
  if Value <> FParagraphSpacing then
  begin
    FParagraphSpacing := Value;

    Modified;
  end;
end;

procedure TDHCustomStyle.SetParagraphIndent(const Value: TPixels);
begin
  if Value <> FParagraphIndent then
  begin
    FParagraphIndent := Value;

    Modified;
  end;
end;

function TDHCustomStyle.GetStoredOffsetTop: Boolean;
begin
  Result := FOffsetTop<>-1;
end;

function TDHCustomStyle.GetStoredOffsetBottom: Boolean;
begin
  Result := FOffsetBottom<>-1;
end;

function TDHCustomStyle.GetStoredLineSpacing: Boolean;
begin
  Result := FLineSpacing<>-1;
end;

function TDHCustomStyle.GetStoredParagraphSpacing: Boolean;
begin
  Result := FParagraphSpacing<>-1;
end;

function TDHCustomStyle.GetStoredParagraphIndent: Boolean;
begin
  Result := FParagraphIndent<>-1;
end;
{$ENDREGION}

{$REGION 'TDHLinkRef'}
constructor TDHLinkRef.Create(const Target: string);
begin
  FTarget := Target;
  FText := TStringBuilder.Create;
end;

destructor TDHLinkRef.Destroy;
begin
  FText.Free;
end;
{$ENDREGION}

{$REGION 'TDHSpoiler'}
constructor TDHSpoiler.Create(const Name: string; Expanded: Boolean);
begin
  FName := Name;
  FExpanded := Expanded;
end;
{$ENDREGION}

end.
