{$DEFINE FMX}
{------------------------------------------------------------------------------
TDzHTMLText component
Developed by Rodrigo Depine Dalpiaz (digao dalpiaz)
Label with formatting tags support

https://github.com/digao-dalpiaz/DzHTMLText

Please, read the documentation at GitHub link.
------------------------------------------------------------------------------}

unit {$IFDEF FMX}FMX{$ELSE}Vcl{$ENDIF}.DzHTMLText;

{$IFDEF FMX}
  {$IF CompilerVersion >= 26} //XE5
    {$DEFINE USE_NEW_UNITS}
  {$ENDIF}
  {$IF CompilerVersion >= 29} //XE8
    {$DEFINE USE_NEW_ENV}
    {$DEFINE USE_IMGLST}
  {$ENDIF}
{$ELSE}
  {$DEFINE VCL}
  {$DEFINE USE_NEW_ENV}
  {$DEFINE USE_IMGLST}
{$ENDIF}

{$IFDEF FPC}
{$mode delphi}
{$WARN 6058 off : Call to subroutine "$1" marked as inline is not inlined}
{$WARN 3175 off : Some fields coming before "$1" were not initialized}
{$WARN 3177 off : Some fields coming after "$1" were not initialized}
{$ENDIF}

{$ZEROBASEDSTRINGS OFF}

interface

uses
{$IFDEF FPC}
  Controls, Classes, Messages, Graphics, Types, FGL, LCLIntf, ImgList
{$ELSE}
  System.Generics.Collections, System.Types, System.Classes,
  {$IFDEF FMX}
  FMX.Forms, FMX.Controls, FMX.Types, System.UITypes
    {$IFDEF USE_NEW_UNITS}, FMX.StdCtrls, FMX.Graphics, FMX.MultiResBitmap{$ENDIF}
    {$IFDEF USE_IMGLST}, FMX.ImgList{$ENDIF}
  {$ELSE}
  Vcl.Controls, Vcl.Graphics, Vcl.ImgList, Vcl.Imaging.pngimage,
  Winapi.Messages
  {$ENDIF}
{$ENDIF};

const DZHTMLTEXT_INTERNAL_VERSION = 705; //Synchronizes TDam component

const _DEF_LISTLEVELPADDING = 20;

{$IFDEF FMX}
const clNone = TAlphaColors.Null;
{$ENDIF}

type
  {$IFDEF FPC}
  TObjectList<T: TObject> = class(TFPGObjectList<T>);
  TList<T> = class(TFPGList<T>);

  TBitmap = Graphics.TBitmap;
  {$ELSE}
    {$IFDEF FMX}
    TRect = TRectF;
    TPoint = TPointF;
    TSize = TSizeF;

    TColor = TAlphaColor;
    TBitmap = FMX.{$IFDEF USE_NEW_UNITS}Graphics{$ELSE}Types{$ENDIF}.TBitmap;
    TPicture = FMX.{$IFDEF USE_NEW_UNITS}Graphics{$ELSE}Types{$ENDIF}.TBitmap;
    {$ELSE}
    TBitmap = Vcl.Graphics.TBitmap;
    {$ENDIF}
  {$ENDIF}

  TPixels = {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};
  TFontPt = {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF};

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
    FText: string;
  public
    property Target: string read FTarget;
    property Text: string read FText;
  end;
  TDHLinkRefList = class(TObjectList<TDHLinkRef>);

  TDHSpoiler = class(TDHBaseLink)
  private
    FName: string;
    FExpanded: Boolean;
  public
    property Name: string read FName;
    property Expanded: Boolean read FExpanded;
  end;
  TDHSpoilerList = class(TObjectList<TDHSpoiler>)
  public
    function Find(const Name: string): TDHSpoiler;
  end;

  TDHVisualItem = class //represents each visual item printed to then canvas
  private
    Rect: TRect;
    BColor: TColor; //background color
    Link: TDHBaseLink;
    {The link number is created sequentially, when reading text links
    and works to know the link target, stored on a TStringList, because if
    the link was saved here at a work, it will be repeat if has multiple words
    per link, spending a lot of unnecessary memory.}
  end;

  TDHVisualItem_Word = class(TDHVisualItem)
  private
    Text: string;
    Font: TFont;
    {$IFDEF FMX}
    FontColor: TColor;
    {$ENDIF}
    YPos: TPixels;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDHVisualItem_Image = class(TDHVisualItem)
  private
    ImageIndex: Integer;
  end;

  TDHVisualItem_ImageResource = class(TDHVisualItem)
  private
    Picture: TPicture;
    procedure Load(Lb: TDzHTMLText; const ResourceName: string);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TDHVisualItemList = class(TObjectList<TDHVisualItem>);

  TDHKindStyleLinkProp = (tslpNormal, tslpHover); //kind of link style

  {DHStyleLinkProp is a sub-property used at Object Inspector that contains
   link formatting when selected and not selected}
  TDHStyleLinkProp = class(TPersistent)
  private
    Lb: TDzHTMLText; //owner
    Kind: TDHKindStyleLinkProp;

    FFontColor: TColor;
    FBackColor: TColor;
    FUnderline: Boolean;
    procedure SetFontColor(const Value: TColor);
    procedure SetBackColor(const Value: TColor);
    procedure SetUnderline(const Value: Boolean);
    function GetDefaultFontColor: TColor;
    function GetStoredFontColor: Boolean;
    procedure SetPropsToCanvas(C: TCanvas); //method to use at paint event
    function GetStored: Boolean; //GetStored general to use at owner
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(Lb: TDzHTMLText; Kind: TDHKindStyleLinkProp);
    procedure Assign(Source: TPersistent); override;
  published
    property FontColor: TColor read FFontColor write SetFontColor stored GetStoredFontColor;
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
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
    function GetHorizontal: TPixels;
    function GetVertical: TPixels;
    function GetRealRect(R: TRect): TRect; inline;
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

  TDHEvLink = procedure(Sender: TObject; Link: TDHBaseLink) of object;
  TDHEvLinkClick = procedure(Sender: TObject; Link: TDHBaseLink; var Handled: Boolean) of object;

  TDHVertAlign = (vaTop, vaCenter, vaBottom);
  TDHHorzAlign = (haLeft, haCenter, haRight);

  TDHEvRetrieveImgRes = procedure(Sender: TObject; const ResourceName: string; Picture: TPicture; var Handled: Boolean) of object;

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

    LVisualItem: TDHVisualItemList; //visual item list to paint event
    LLinkRef: TDHLinkRefList; //list of links info
    LSpoiler: TDHSpoilerList;

    FLines: TStrings;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    FMaxWidth: TPixels; //max width when using AutoWidth
    FAutoOpenLink: Boolean;

    FLineCount: Integer; //read-only
    FTextWidth: TPixels; //read-only
    FTextHeight: TPixels; //read-only

    FStyleLinkNormal, FStyleLinkHover: TDHStyleLinkProp;

    {$IFDEF FMX}
    FColor: TColor;
    FFontColor: TColor;
    {$ENDIF}

    {$IFDEF USE_IMGLST}
    FImages: TCustomImageList;
    {$ENDIF}

    FOnRetrieveImgRes: TDHEvRetrieveImgRes;

    FLineVertAlign: TDHVertAlign;
    FOverallVertAlign: TDHVertAlign;
    FOverallHorzAlign: TDHHorzAlign;
    FLineSpacing: TPixels;
    FListLevelPadding: TPixels;

    FBorders: TDHBorders;

    FOnLinkEnter, FOnLinkLeave: TDHEvLink;
    FOnLinkClick, FOnLinkRightClick: TDHEvLinkClick;

    FSelectedLink: TDHBaseLink; //selected link

    FCursor: TCursor;

    UpdatingSemaphore: Integer;
    InternalResizing: Boolean;

    procedure OnLinesChange(Sender: TObject);
    procedure SetLines(const Value: TStrings);
    function GetText: string;
    procedure SetText(const Value: string); {$IFDEF FMX}reintroduce;{$ENDIF}

    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetMaxWidth(const Value: TPixels);

    function GetStoredMaxWidth: Boolean;
    function GetStoredLineSpacing: Boolean;
    function GetStoredListLevelPadding: Boolean;
    function GetStoredBorders: Boolean;

    function GetStoredStyleLink(const Index: Integer): Boolean;
    procedure SetStyleLink(const Index: Integer; const Value: TDHStyleLinkProp);

    procedure DoPaint; {$IFDEF FMX}reintroduce;{$ENDIF}
    procedure BuildAndPaint; //rebuild and repaint
    procedure Modified(Flags: TDHModifiedFlags);

    function GetIsLinkHover: Boolean;
    procedure CheckMouse(X, Y: TPixels); //check links by mouse position
    procedure SetCursorByLink(Selected: Boolean);
    procedure SetCursor(const Value: TCursor); reintroduce;
    procedure SetLineVertAlign(const Value: TDHVertAlign);
    procedure SetOverallVertAlign(const Value: TDHVertAlign);
    procedure SetOverallHorzAlign(const Value: TDHHorzAlign);
    procedure SetLineSpacing(const Value: TPixels);
    procedure SetListLevelPadding(const Value: TPixels);
    procedure SetBorders(const Value: TDHBorders);

    {$IFDEF USE_IMGLST}
    procedure SetImages(const Value: TCustomImageList);
    {$ENDIF}

    {$IFDEF FMX}
    procedure SetFontColor(const Value: TColor);
    procedure SetColor(const Value: TColor);

    procedure OnFontChanged(Sender: TObject);
    {$ENDIF}

    procedure SetTextSize(W, H: TPixels);

    function GetAreaHeight: TPixels; inline;
    function GetAreaWidth: TPixels; inline;
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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsLinkHover: Boolean read GetIsLinkHover;
    property SelectedLink: TDHBaseLink read FSelectedLink;

    property LinkRefs: TDHLinkRefList read LLinkRef;
    property Spoilers: TDHSpoilerList read LSpoiler;

    procedure Rebuild; //rebuild words

    procedure BeginUpdate; {$IFDEF FMX}reintroduce;{$ENDIF}
    procedure EndUpdate(ForceRepaint: Boolean = True); {$IFDEF FMX}reintroduce;{$ENDIF}

    property Text: string read GetText write SetText;

    class function UnescapeHTMLToText(const aHTML: string): string;
    class function EscapeTextToHTML(const aText: string): string;
    class function HTMLToPlainText(const aHTML: string): string;
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

    property Color: TColor read FColor write SetColor default clNone;
    property FontColor: TColor read FFontColor write SetFontColor default TAlphaColors.Black;
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

    property StyleLinkNormal: TDHStyleLinkProp index 1 read FStyleLinkNormal write SetStyleLink stored GetStoredStyleLink;
    property StyleLinkHover: TDHStyleLinkProp index 2 read FStyleLinkHover write SetStyleLink stored GetStoredStyleLink;

    {$IFDEF USE_IMGLST}
    property Images: TCustomImageList read FImages write SetImages;
    {$ENDIF}

    property LineCount: Integer read FLineCount;
    property TextWidth: TPixels read FTextWidth;
    property TextHeight: TPixels read FTextHeight;

    property OnLinkEnter: TDHEvLink read FOnLinkEnter write FOnLinkEnter;
    property OnLinkLeave: TDHEvLink read FOnLinkLeave write FOnLinkLeave;
    property OnLinkClick: TDHEvLinkClick read FOnLinkClick write FOnLinkClick;
    property OnLinkRightClick: TDHEvLinkClick read FOnLinkRightClick write FOnLinkRightClick;

    property OnRetrieveImgRes: TDHEvRetrieveImgRes read FOnRetrieveImgRes write FOnRetrieveImgRes;

    property AutoOpenLink: Boolean read FAutoOpenLink write FAutoOpenLink default True;

    property LineVertAlign: TDHVertAlign read FLineVertAlign write SetLineVertAlign default vaTop;
    property OverallVertAlign: TDHVertAlign read FOverallVertAlign write SetOverallVertAlign default vaTop;
    property OverallHorzAlign: TDHHorzAlign read FOverallHorzAlign write SetOverallHorzAlign default haLeft;
    property LineSpacing: TPixels read FLineSpacing write SetLineSpacing stored GetStoredLineSpacing;
    property ListLevelPadding: TPixels read FListLevelPadding write SetListLevelPadding stored GetStoredListLevelPadding;

    property Borders: TDHBorders read FBorders write SetBorders stored GetStoredBorders;

    property About: string read FAbout;
  end;

procedure Register;

implementation

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}SysUtils, StrUtils, Math, LResources
{$ELSE}
  System.SysUtils, System.StrUtils, System.Math
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
{$ENDIF};

const STR_VERSION = '3.10';

procedure Register;
begin
  {$IFDEF FPC}{$I DzHTMLText.lrs}{$ENDIF}
  RegisterComponents('Digao', [TDzHTMLText]);
end;

//

type
  EInternalExcept = class(Exception)
    constructor Create(const Msg: string);
  end;

constructor EInternalExcept.Create(const Msg: string);
begin
  inherited CreateFmt('%s internal error: %s', [TDzHTMLText.ClassName, Msg]);
end;

{ TDHBaseLink }

function TDHBaseLink.GetKind: TDHLinkKind;
begin
  if Self is TDHLinkRef then Result := lkLinkRef else
  if Self is TDHSpoiler then Result := lkSpoiler else
    raise EInternalExcept.Create('Invalid link kind');
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

{ TDHSpoilerList }

function TDHSpoilerList.Find(const Name: string): TDHSpoiler;
var DHSpoiler: TDHSpoiler;
begin
  for DHSpoiler in Self do
    if DHSpoiler.FName = Name then Exit(DHSpoiler);

  Exit(nil);
end;

{ TDHVisualItem_Word }

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

{ TDHVisualItem_ImageResource }

constructor TDHVisualItem_ImageResource.Create;
begin
  inherited;
  Picture := TPicture.Create{$IFNDEF USE_NEW_ENV}(0, 0){$ENDIF};
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

//

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

class function TDzHTMLText.HTMLToPlainText(const aHTML: string): string;
var
  X, XEnd: Integer;
begin
  Result := aHTML;

  while True do
  begin
    X := Pos('<', Result);
    if X=0 then Break;

    XEnd := PosEx('>', Result, X+1);
    if XEnd=0 then Break;

    Delete(Result, X, XEnd-X+1);
  end;

  Result := UnescapeHTMLToText(Result);
end;

//

constructor TDzHTMLText.Create(AOwner: TComponent);
begin
  inherited;
  {$IFDEF VCL}
  ControlStyle := ControlStyle + [csOpaque];
  //Warning! The use of transparency in the component causes flickering
  {$ENDIF}

  FAbout := 'Digao Dalpiaz / Version '+STR_VERSION;

  FLines := TStringList.Create;
  //FLines.TrailingLineBreak := False; -- only supported by Delphi 10.1 and not full functional in Lazarus
  TStringList(FLines).OnChange := OnLinesChange;

  FStyleLinkNormal := TDHStyleLinkProp.Create(Self, tslpNormal);
  FStyleLinkHover := TDHStyleLinkProp.Create(Self, tslpHover);
  LVisualItem := TDHVisualItemList.Create;
  LLinkRef := TDHLinkRefList.Create;
  LSpoiler := TDHSpoilerList.Create;

  FAutoOpenLink := True;
  FListLevelPadding := _DEF_LISTLEVELPADDING;

  FBorders := TDHBorders.Create(Self);

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
  LVisualItem.Free;
  LLinkRef.Free;
  LSpoiler.Free;
  inherited;
end;

procedure TDzHTMLText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    {$IFDEF USE_IMGLST}
    if AComponent = FImages then FImages := nil;
    {$ENDIF}
  end;
end;

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

function TDzHTMLText.GetAreaWidth: TPixels;
begin
  Result := Width - FBorders.GetHorizontal;
end;

function TDzHTMLText.GetAreaHeight: TPixels;
begin
  Result := Height - FBorders.GetVertical;
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

procedure TDzHTMLText.SetLineVertAlign(const Value: TDHVertAlign);
begin
  if Value<>FLineVertAlign then
  begin
    FLineVertAlign := Value;

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

procedure TDzHTMLText.SetListLevelPadding(const Value: TPixels);
begin
  if Value<>FListLevelPadding then
  begin
    FListLevelPadding := Value;

    BuildAndPaint;
  end;
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
  {$IFDEF FPC}if Message.Result=0 then {};{$ENDIF} //avoid unused var warning
  Modified([mfPaint]);
end;

procedure TDzHTMLText.CMFontchanged(var Message: TMessage);
begin
  {$IFDEF FPC}if Message.Result=0 then {};{$ENDIF} //avoid unused var warning
  BuildAndPaint;
end;
{$ENDIF}

{$IFDEF FMX}
procedure TDzHTMLText.SetColor(const Value: TColor);
begin
  if Value<>FColor then
  begin
    FColor := Value;

    Modified([mfPaint]);
  end;
end;

procedure TDzHTMLText.SetFontColor(const Value: TColor);
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
{$ENDIF}

procedure TDzHTMLText.SetTextSize(W, H: TPixels);
begin
  FTextWidth := W;
  FTextHeight := H;

  InternalResizing := True;
  try
    if FAutoWidth then Width := W + FBorders.GetHorizontal;
    if FAutoHeight then Height := H + FBorders.GetVertical;
  finally
    InternalResizing := False;
  end;
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
  DoPaint;
end;

procedure TDzHTMLText.DoPaint;
var
  W: TDHVisualItem;
  C: TCanvas;
  R: TRect;
  {$IFDEF VCL}B: TBitmap;{$ENDIF}
begin
  {$IFDEF VCL}
  //Using internal bitmap as a buffer to reduce flickering
  B := TBitmap.Create;
  try
    B.SetSize(Width, Height);
    C := B.Canvas;
  {$ELSE}
  //In FMX, Paint method calls BeginScene/EndScene automatically,
  //so there is no need for Bitmap and there is no concern about flickering.
  C := Canvas;
  {$ENDIF}

    //draw background color
    {$IFDEF FMX}
    if Color<>clNone then
    begin
      C.Fill.Color := FColor;
      C.FillRect(LocalRect, 0, 0, [], 1);
    end;
    {$ELSE}
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
    {$ENDIF}

    if csDesigning in ComponentState then
    begin
      {$IFDEF FMX}
      C.Stroke.Thickness := 0.5;
      C.Stroke.Kind := TBrushKind.{$IF CompilerVersion >= 27}{XE6}Solid{$ELSE}bkSolid{$ENDIF};
      C.Stroke.Dash := TStrokeDash.{$IF CompilerVersion >= 27}{XE6}Dash{$ELSE}sdDash{$ENDIF};
      C.Stroke.Color := TAlphaColors.Black;
      C.DrawRect(LocalRect, 0, 0, [], 1);
      {$ELSE}
      C.Pen.Style := psDot;
      C.Pen.Color := clBtnShadow;
      C.Brush.Style := bsClear;
      C.Rectangle(ClientRect);
      {$ENDIF}
    end;

    for W in LVisualItem do
    begin
      R := FBorders.GetRealRect(W.Rect);

      C.{$IFDEF FMX}Fill{$ELSE}Brush{$ENDIF}.Color := W.BColor;

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

      if C.{$IFDEF FMX}Fill{$ELSE}Brush{$ENDIF}.Color<>clNone then
        C.FillRect(
          {$IFDEF FMX}
          R, 0, 0, [], 1
          {$ELSE}
          R
          {$ENDIF});

      if W is TDHVisualItem_Word then
        with TDHVisualItem_Word(W) do
        begin
          R.Top := R.Top + YPos;

          {$IFDEF FMX}
          C.Fill.Color := C.Stroke.Color;
          C.FillText(R, Text, False, 1, [],
            TTextAlign.{$IF CompilerVersion >= 27}{XE6}Leading{$ELSE}taLeading{$ENDIF});
          {$ELSE}
          C.Brush.Style := bsClear;
            {$IFDEF MSWINDOWS}
            DrawTextW(C.Handle,
              PWideChar({$IFDEF FPC}UnicodeString(Text){$ELSE}Text{$ENDIF}),
              -1, R, DT_NOCLIP or DT_NOPREFIX);
            {Using DrawText, because TextOut has no clip option, which causes
            bad overload of text when painting using background, oversizing the
            text area wildly.}
            {$ELSE}
            //FPC Linux
            C.TextOut(R.Left, R.Top, Text);
            {$ENDIF}
          {$ENDIF}
        end
      else
      if W is TDHVisualItem_Image then
        with TDHVisualItem_Image(W) do
        begin
          {$IFDEF USE_IMGLST}
          if Assigned(FImages) then
            {$IFDEF FMX}
            FImages.Draw(C, R, ImageIndex, 1);
            {$ELSE}
            FImages.Draw(C, R.Left, R.Top, ImageIndex);
            {$ENDIF}
          {$ENDIF}
        end
      else
      if W is TDHVisualItem_ImageResource then
        with TDHVisualItem_ImageResource(W) do
        begin
          {$IFDEF FMX}
          C.DrawBitmap(Picture, TRect{F}.Create(0, 0, Picture.Width, Picture.Height), R, 1);
          {$ELSE}
          C.Draw(R.Left, R.Top, Picture.Graphic);
          {$ENDIF}
        end
      else
        raise EInternalExcept.Create('Invalid visual item object');
    end;

  {$IFDEF VCL}
    Canvas.Draw(0, 0, B);
  finally
    B.Free;
  end;
  {$ENDIF}
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

procedure TDzHTMLText.CheckMouse(X, Y: TPixels);
var
  Link: TDHBaseLink;
  W: TDHVisualItem;
  P: TPoint;
begin
  Link := nil;

  P := TPoint.Create(X, Y);

  //find the first word, if there is any
  for W in LVisualItem do
    if Assigned(W.Link) then
    begin
      if FBorders.GetRealRect(W.Rect).Contains(P) then //selected
      begin
        Link := W.Link;
        Break;
      end;
    end;

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
var Handled: Boolean;
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
            raise Exception.Create('Unsupported platform');
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
        raise EInternalExcept.Create('Invalid link object');
    end;
  end;

  inherited;
end;

procedure TDzHTMLText.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: {$IFDEF FMX}Single{$ELSE}Integer{$ENDIF});
var Handled: Boolean;
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

//

type
  TTokenKind = (
    ttInvalid,
    ttBold, ttItalic, ttUnderline, ttStrike,
    ttFontName, ttFontSize, ttFontColor, ttBackColor,
    ttTab, ttTabF,
    ttBreak, ttText, ttLink,
    ttAlignLeft, ttAlignCenter, ttAlignRight,
    ttImage, ttImageResource,
    ttBulletList, ttNumberList, ttListItem,
    ttFloat,
    ttSpoilerTitle, ttSpoilerDetail,
    ttLineSpace,
    ttSuperscript, ttSubscript);

  TTokenValue = Int64;

  TToken = class
    Kind: TTokenKind;
    TagClose: Boolean;
    Text: string;
    Value: TTokenValue;
  end;

  TListToken = class(TObjectList<TToken>);

  TBuilder = class
    Lb: TDzHTMLText;
    LToken: TListToken;

    function ProcessTag(const Tag: string): Boolean;
    procedure AddToken(aKind: TTokenKind; aTagClose: Boolean = False; const aText: string = ''; aValue: TTokenValue = 0);

    procedure ReadTokens; //create list of tokens
    procedure ProcessTokens; //create list of visual itens

    constructor Create;
    destructor Destroy; override;
  end;

constructor TBuilder.Create;
begin
  inherited;
  LToken := TListToken.Create;
end;

destructor TBuilder.Destroy;
begin
  LToken.Free;
  inherited;
end;

procedure TDzHTMLText.Rebuild;
var
  B: TBuilder;
  P: TPoint;
begin
  if csLoading in ComponentState then Exit;

  LVisualItem.Clear; //clean old words
  LLinkRef.Clear; //clean old links

  B := TBuilder.Create;
  try
    B.Lb := Self;

    B.ReadTokens;
    B.ProcessTokens;
  finally
    B.Free;
  end;

  //reset selected link
  FSelectedLink := nil;
  SetCursorByLink(False);

  //update link by cursor pos
  {$IFDEF FMX}
  P := ScreenToLocal(Screen.MousePos);
  {$ELSE}
  P := ScreenToClient(Mouse.CursorPos);
  {$ENDIF}
  CheckMouse(P.X, P.Y);
end;

//

function ParamToColor(A: string): TColor;
begin
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

procedure TBuilder.AddToken(aKind: TTokenKind; aTagClose: Boolean = False; const aText: string = ''; aValue: TTokenValue = 0);
var T: TToken;
begin
  T := TToken.Create;
  T.Kind := aKind;
  T.TagClose := aTagClose;
  T.Text := aText;
  T.Value := aValue;
  LToken.Add(T);
end;

function Tag_IntZeroBased_ProcValue(const Value: string; var Valid: Boolean): TTokenValue;
begin
  Result := StrToIntDef(Value, -1);
  Valid := (Result>-1);
end;

function Tag_IntOneBased_ProcValue(const Value: string; var Valid: Boolean): TTokenValue;
begin
  Result := StrToIntDef(Value, 0);
  Valid := (Result>0);
end;

function Tag_Color_ProcValue(const Value: string; var Valid: Boolean): TTokenValue;
begin
  Result := ParamToColor(Value);
  Valid := (Result<>clNone);
end;

type TDefToken = record
  Ident: string;
  Kind: TTokenKind;
  Single: Boolean; //without close tag
  AllowPar, OptionalPar: Boolean;
  ProcValue: function(const Value: string; var Valid: Boolean): TTokenValue;
end;
const DEF_TOKENS: array[0..25] of TDefToken = (
  (Ident: 'BR'; Kind: ttBreak; Single: True),
  (Ident: 'B'; Kind: ttBold),
  (Ident: 'I'; Kind: ttItalic),
  (Ident: 'U'; Kind: ttUnderline),
  (Ident: 'S'; Kind: ttStrike),
  (Ident: 'FN'; Kind: ttFontName; AllowPar: True),
  (Ident: 'FS'; Kind: ttFontSize; AllowPar: True; ProcValue: Tag_IntOneBased_ProcValue),
  (Ident: 'FC'; Kind: ttFontColor; AllowPar: True; ProcValue: Tag_Color_ProcValue),
  (Ident: 'BC'; Kind: ttBackColor; AllowPar: True; ProcValue: Tag_Color_ProcValue),
  (Ident: 'A'; Kind: ttLink; AllowPar: True; OptionalPar: True),
  (Ident: 'L'; Kind: ttAlignLeft),
  (Ident: 'C'; Kind: ttAlignCenter),
  (Ident: 'R'; Kind: ttAlignRight),
  (Ident: 'T'; Kind: ttTab; Single: True; AllowPar: True; ProcValue: Tag_IntOneBased_ProcValue),
  (Ident: 'TF'; Kind: ttTabF; Single: True; AllowPar: True; ProcValue: Tag_IntOneBased_ProcValue),
  (Ident: 'IMG'; Kind: ttImage; Single: True; AllowPar: True; ProcValue: Tag_IntZeroBased_ProcValue),
  (Ident: 'IMGRES'; Kind: ttImageResource; Single: True; AllowPar: True),
  (Ident: 'UL'; Kind: ttBulletList), //Unordered HTML List
  (Ident: 'OL'; Kind: ttNumberList), //Ordered HTML List
  (Ident: 'LI'; Kind: ttListItem), //HTML List Item
  (Ident: 'FLOAT'; Kind: ttFloat; AllowPar: True), //Floating div
  (Ident: 'SPOILER'; Kind: ttSpoilerTitle; AllowPar: True),
  (Ident: 'SDETAIL'; Kind: ttSpoilerDetail; AllowPar: True),
  (Ident: 'LS'; Kind: ttLineSpace; AllowPar: True; ProcValue: Tag_IntZeroBased_ProcValue),
  (Ident: 'SUP'; Kind: ttSuperscript),
  (Ident: 'SUB'; Kind: ttSubscript)
);

function TBuilder.ProcessTag(const Tag: string): Boolean;
var TOff, TOn, HasPar, ValidPar: Boolean;
    Value: TTokenValue;
    A, Par: string;
    I: Integer;
    Def: TDefToken;
begin
  //Result=True means valid tag
  Result := False;
  A := Tag;

  TOff := False;
  if A.StartsWith('/') then //closing tag
  begin
    TOff := True;
    Delete(A, 1, 1);
  end;
  TOn := not TOff;

  HasPar := False;
  Par := EmptyStr;
  I := Pos(':', A); //find parameter
  if I>0 then //has parameter
  begin
    HasPar := True;
    Par := A.Substring(I); //zero-based
    A := Copy(A, 1, I-1);
  end;

  if HasPar then
  begin
    if Par=EmptyStr then Exit; //blank parameter specified
    if TOff then Exit; //tag closing with parameter
  end;

  A := UpperCase(A);

  for Def in DEF_TOKENS do
  begin
    if Def.Ident=A then
    begin
      if TOn then
      begin
        if (not Def.AllowPar) and (HasPar) then Exit; //parameter not allowed
        if (Def.AllowPar) and (not Def.OptionalPar) and (not HasPar) then Exit; //parameter required
      end else
      begin
        if Def.Single then Exit; //close-tag on single tag        
      end;

      Value := 0;
      if TOn and HasPar and Assigned(Def.ProcValue) then
      begin
        ValidPar := True;
        Value := Def.ProcValue(Par, ValidPar);
        if not ValidPar then Exit;
      end;

      AddToken(Def.Kind, TOff, Par, Value);
      Result := True;
      Exit;
    end;
  end;
end;

const
  STR_SPACE = ' ';
  INT_BREAKABLE_CHAR = -1;

type
  TCharUtils = class
    class function FindNextWordBreakChar(const A: string): Integer; inline;
    class function IsCJKChar(const C: Char): Boolean; inline;
  end;

class function TCharUtils.FindNextWordBreakChar(const A: string): Integer;
var
  I: Integer;
  C: Char;
begin
  Result := 0;

  for I := 1 to A.Length do
  begin
    C := A[I];

    if CharInSet(C, [STR_SPACE,'<','>','/','\']) or IsCJKChar(C) then
    begin // !!! should never find tags at first char
      Result := I;
      Break;
    end;
  end;
end;

class function TCharUtils.IsCJKChar(const C: Char): Boolean; //return if char is Chinese-Japanese-Korean
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
  Result := False;
  if C < #10000 then Exit; //fast check

  case Integer(C) of
    $4E00..$9FFF,
    $3400..$4DBF,
    $20000..$2A6DF,
    $2A700..$2B73F,
    $2B740..$2B81F,
    $2B820..$2CEAF,
    $F900..$FAFF,
    $2F800..$2FA1F: Result := True;
  end;
end;

procedure TBuilder.ReadTokens;
var
  Text, A: string;
  CharIni: Char;
  I, Jump: Integer;
  BreakableChar: Boolean;
begin
  Text := Lb.FLines.Text; //when is not empty, always comes with a final line break

  Text := StringReplace(Text, sLineBreak+'<NBR>', EmptyStr, [rfReplaceAll, rfIgnoreCase]); //ignore next break
  Text := StringReplace(Text, sLineBreak, '<BR>', [rfReplaceAll]);

  while not Text.IsEmpty do
  begin
    A := Text;
    CharIni := A[1];

    if CharIni = '<' then //starts with tag opening
    begin
      Delete(A, 1, 1);
      I := Pos('>', A); //find tag closing
      if I>0 then
      begin
        A := Copy(A, 1, I-1);
        if not ProcessTag(A) then AddToken(ttInvalid);
        Jump := 1+Length(A)+1;
      end else
      begin
        //losted tag opening
        AddToken(ttInvalid);
        Jump := 1;
      end;
    end else
    if CharIni = '>' then
    begin
      //losted tag closing
      AddToken(ttInvalid);
      Jump := 1;
    end else
    begin //all the rest is text
      I := TCharUtils.FindNextWordBreakChar(A);
      BreakableChar := (I=1);
      //when word break at first char, let add the char itself alone.
      //when word break at other next chars, consider until char before word-break char.
      if I>1 then Dec(I) else
        if I=0 then I := Length(A);

      A := Copy(A, 1, I);
      AddToken(ttText, False, TDzHTMLText.UnescapeHTMLToText(A), IfThen(BreakableChar, INT_BREAKABLE_CHAR));
      Jump := I;
    end;

    Delete(Text, 1, Jump);
  end;
end;

type
  TListStack<T> = class(TList<T>)
    procedure AddOrDel(Token: TToken; const XValue: T);
  end;

procedure TListStack<T>.AddOrDel(Token: TToken; const XValue: T);
begin
  if Token.TagClose then
  begin
    if Count>1 then
      Delete(Count-1);
  end else
    Add(XValue);
end;

type
  TObjectListStackItem = class(TObject);
  TObjectListStackItemClass = class of TObjectListStackItem;
  TObjectListStack<T: TObjectListStackItem{, constructor}> = class(TObjectList<T>)
    procedure AddOrDel(Token: TToken; &Class: TObjectListStackItemClass);
  end;

  THTMLList = class(TObjectListStackItem);
  THTMLList_Bullet = class(THTMLList);
  THTMLList_Number = class(THTMLList)
    Sequence: Integer;
  end;

  THTMLSpoilerDet = class(TObjectListStackItem)
    Name: string;
  end;
  THTMLSpoilerDetList = class(TObjectListStack<THTMLSpoilerDet>)
    function IsAllOpened(Lb: TDzHTMLText): Boolean;
  end;

  THTMLSupSubTag = class(TObjectListStackItem);
  THTMLSupTag = class(THTMLSupSubTag);
  THTMLSubTag = class(THTMLSupSubTag);

procedure TObjectListStack<T>.AddOrDel(Token: TToken; &Class: TObjectListStackItemClass);
begin
  if Token.TagClose then
  begin
    if (Count>0) and (Last is &Class) then
      Delete(Count-1);
  end else
  begin
    Add(&Class.Create as T);
  end;
end;

function THTMLSpoilerDetList.IsAllOpened(Lb: TDzHTMLText): Boolean;
var
  SpoilerDet: THTMLSpoilerDet;
  DHSpoiler: TDHSpoiler;
begin
  for SpoilerDet in Self do
  begin
    DHSpoiler := Lb.LSpoiler.Find(SpoilerDet.Name);
    if not ( (DHSpoiler<>nil) and (DHSpoiler.FExpanded) ) then Exit(False);
  end;

  Exit(True);
end;

type
  TLineInfo = class
    Height, Space: TPixels;
  end;
  TGroupBound = class
    Right, Limit: TPixels;
  end;

  TPreObj = class(TObject);

  TPreObj_Break = class(TPreObj)
    Height: TPixels;
  end;

  TPreObj_Tab = class(TPreObj)
    Position: TPixels;
    Fixed: Boolean;
  end;

  TPreObj_Float = class(TPreObj)
    Rect: TRect;
    Close: Boolean;
  end;

  TFixedPosition = record
  private
    Active: Boolean;
    Left: TPixels;
  end;

  TPreObj_Visual = class(TPreObj)
    Size: TSize;
    Line: Integer; //line number
    Group: Integer; //group number
    {The group is isolated at each line or tabulation to delimit text horizontal align area}
    FixedPos: TFixedPosition;
    Align: TDHHorzAlign;
    LineSpace: TPixels;
    Space: Boolean;
    Print: Boolean;
    BreakableChar: Boolean; //text with only one letter using breakable char
    BreakCheckDone: Boolean; //if already checked for break line behavior

    Visual: TDHVisualItem;
    destructor Destroy; override;
  end;

  TListPreObj = class(TObjectList<TPreObj>);

destructor TPreObj_Visual.Destroy;
begin
  if Assigned(Visual) then Visual.Free;
  inherited;
end;

type
  TTokensProcess = class
    Builder: TBuilder;
    Lb: TDzHTMLText;
    C: TCanvas;

    LLineInfo: TObjectList<TLineInfo>;
    LGroupBound: TObjectList<TGroupBound>;

    Items: TListPreObj;

    CurrentProps: record
      BackColor: TColor;
      Align: TDHHorzAlign;
      LineSpace: TPixels;
    end;

    LBold: TListStack<Boolean>;
    LItalic: TListStack<Boolean>;
    LUnderline: TListStack<Boolean>;
    LStrike: TListStack<Boolean>;
    LFontName: TListStack<string>;
    LFontSize: TListStack<TFontPt>;
    LFontColor: TListStack<TColor>;
    LBackColor: TListStack<TColor>;
    LAlign: TListStack<TDHHorzAlign>;
    LLineSpace: TListStack<TPixels>;
    LHTMLList: TObjectListStack<THTMLList>;
    LSupAndSubScript: TObjectListStack<THTMLSupSubTag>;
    LSpoilerDet: THTMLSpoilerDetList;

    CurrentLink: TDHBaseLink;

    constructor Create(xBuilder: TBuilder);
    destructor Destroy; override;
    procedure Execute;

    procedure DoTypographicalEmphasis(T: TToken);
    procedure DoFontName(T: TToken);
    procedure DoFontSize(T: TToken);
    procedure DoFontColor(T: TToken);
    procedure DoBackColor(T: TToken);
    procedure DoSupOrSubScript(T: TToken);
    procedure DoAlignment(T: TToken);
    procedure DoLineSpace(T: TToken);
    procedure DoTextAndRelated(T: TToken);
    procedure DoLink(T: TToken);
    procedure DoLists(T: TToken);
    procedure DoFloat(T: TToken);
    procedure DoSpoilerTitle(T: TToken);
    procedure DoSpoilerDetail(T: TToken);
    procedure DoTab(T: TToken);
    procedure DoBreak;

    procedure CheckSupSubScript(W: TDHVisualItem_Word; var Size: TSize);

    procedure DefineVisualRect;
    procedure Publish;

    procedure CheckAlign(V: TPreObj_Visual);
  end;

procedure TBuilder.ProcessTokens;
var P: TTokensProcess;
begin
  P := TTokensProcess.Create(Self);
  try
    P.Execute;
    P.DefineVisualRect;
    P.Publish;
  finally
    P.Free;
  end;
end;

constructor TTokensProcess.Create(xBuilder: TBuilder);
var vBool: Boolean; //Required for Lazarus
begin
  inherited Create;
  Builder := xBuilder;
  Lb := Builder.Lb;
  C := {$IFDEF FMX}TCanvasManager.MeasureCanvas{$ELSE}Lb.Canvas{$ENDIF};
  C.Font.Assign(Lb.Font);
  {$IFDEF FMX}
  C.Stroke.Color := Lb.FFontColor;
  {$ELSE}
    {$IFDEF DCC}
    if TStyleManager.IsCustomStyleActive and (seFont in Lb.StyleElements) and not (csDesigning in Lb.ComponentState) then
      C.Font.Color := TStyleManager.ActiveStyle.GetStyleFontColor(TStyleFont.sfWindowTextNormal);
    {$ENDIF}
  {$ENDIF}

  CurrentProps.BackColor := clNone;
  CurrentProps.Align := haLeft;
  CurrentProps.LineSpace := Lb.FLineSpacing;

  Items := TListPreObj.Create;
  LLineInfo := TObjectList<TLineInfo>.Create;
  LGroupBound := TObjectList<TGroupBound>.Create;

  LBold := TListStack<Boolean>.Create;
  LItalic := TListStack<Boolean>.Create;
  LUnderline := TListStack<Boolean>.Create;
  LStrike := TListStack<Boolean>.Create;
  LFontName := TListStack<string>.Create;
  LFontSize := TListStack<TFontPt>.Create;
  LFontColor := TListStack<TColor>.Create;
  LBackColor := TListStack<TColor>.Create;
  LAlign := TListStack<TDHHorzAlign>.Create;
  LLineSpace := TListStack<TPixels>.Create;

  LHTMLList := TObjectListStack<THTMLList>.Create;
  LSupAndSubScript := TObjectListStack<THTMLSupSubTag>.Create;
  LSpoilerDet := THTMLSpoilerDetList.Create;

  vBool := TFontStyle.fsBold in C.Font.Style; LBold.Add(vBool);
  vBool := TFontStyle.fsItalic in C.Font.Style; LItalic.Add(vBool);
  vBool := TFontStyle.fsUnderline in C.Font.Style; LUnderline.Add(vBool);
  vBool := TFontStyle.fsStrikeOut in C.Font.Style; LStrike.Add(vBool);
  LFontName.Add(C.Font.{$IFDEF FMX}Family{$ELSE}Name{$ENDIF});
  LFontSize.Add(C.Font.Size);
  LFontColor.Add(C.{$IFDEF FMX}Stroke{$ELSE}Font{$ENDIF}.Color);
  LBackColor.Add(CurrentProps.BackColor);
  LAlign.Add(CurrentProps.Align);
  LLineSpace.Add(CurrentProps.LineSpace);
end;

destructor TTokensProcess.Destroy;
begin
  Items.Free;
  LLineInfo.Free;
  LGroupBound.Free;

  LBold.Free;
  LItalic.Free;
  LUnderline.Free;
  LStrike.Free;
  LFontName.Free;
  LFontSize.Free;
  LFontColor.Free;
  LBackColor.Free;
  LAlign.Free;
  LLineSpace.Free;

  LHTMLList.Free;
  LSupAndSubScript.Free;
  LSpoilerDet.Free;
  inherited;
end;

procedure TTokensProcess.Execute;
var
  T: TToken;
  ListItemAlreadyInThisLine: Boolean;
begin
  ListItemAlreadyInThisLine := False;

  for T in Builder.LToken do
  begin
    if not (T.Kind in [ttSpoilerTitle, ttSpoilerDetail]) then
    begin
      //Bypass when inside a closed spoiler detail tag
      if LSpoilerDet.Count>0 then
        if not LSpoilerDet.IsAllOpened(Lb) then Continue;
    end;

    if (T.Kind = ttListItem) and not T.TagClose then
    begin
      if ListItemAlreadyInThisLine then DoBreak;
      ListItemAlreadyInThisLine := True;
    end;

    case T.Kind of
      ttBold, ttItalic, ttUnderline, ttStrike: DoTypographicalEmphasis(T);
      ttFontName: DoFontName(T);
      ttFontSize: DoFontSize(T);
      ttFontColor: DoFontColor(T);
      ttBackColor: DoBackColor(T);
      ttSuperscript, ttSubscript: DoSupOrSubScript(T);
      ttAlignLeft, ttAlignCenter, ttAlignRight: DoAlignment(T);
      ttLineSpace: DoLineSpace(T);
      ttText, ttInvalid, ttImage, ttImageResource, ttListItem: DoTextAndRelated(T);
      ttLink: DoLink(T);
      ttBulletList, ttNumberList: DoLists(T);
      ttFloat: DoFloat(T);
      ttSpoilerTitle: DoSpoilerTitle(T);
      ttSpoilerDetail: DoSpoilerDetail(T);
      ttTab, ttTabF: DoTab(T);
      ttBreak:
        begin
          DoBreak;
          ListItemAlreadyInThisLine := False;
        end;
    end;
  end;
end;

procedure TTokensProcess.DoTypographicalEmphasis(T: TToken);
var
  FS: TFontStyles;
begin
  case T.Kind of
    ttBold: LBold.AddOrDel(T, True);
    ttItalic: LItalic.AddOrDel(T, True);
    ttUnderline: LUnderline.AddOrDel(T, True);
    ttStrike: LStrike.AddOrDel(T, True);
    else raise EInternalExcept.Create('Invalid typographical emphasis token kind');
  end;

  FS := [];
  if LBold.Last then Include(FS, TFontStyle.fsBold);
  if LItalic.Last then Include(FS, TFontStyle.fsItalic);
  if LUnderline.Last then Include(FS, TFontStyle.fsUnderline);
  if LStrike.Last then Include(FS, TFontStyle.fsStrikeOut);
  C.Font.Style := FS;
end;

procedure TTokensProcess.DoFontName(T: TToken);
begin
  LFontName.AddOrDel(T, T.Text);
  C.Font.{$IFDEF FMX}Family{$ELSE}Name{$ENDIF} := LFontName.Last;
end;

procedure TTokensProcess.DoFontSize(T: TToken);
begin
  LFontSize.AddOrDel(T, T.Value);
  C.Font.Size := LFontSize.Last;
end;

procedure TTokensProcess.DoFontColor(T: TToken);
begin
  LFontColor.AddOrDel(T, T.Value);
  C.{$IFDEF FMX}Stroke{$ELSE}Font{$ENDIF}.Color := LFontColor.Last;
end;

procedure TTokensProcess.DoBackColor(T: TToken);
begin
  LBackColor.AddOrDel(T, T.Value);
  CurrentProps.BackColor := LBackColor.Last;
end;

procedure TTokensProcess.DoAlignment(T: TToken);
var Align: TDHHorzAlign;
begin
  case T.Kind of
    ttAlignLeft: Align := haLeft;
    ttAlignCenter: Align := haCenter;
    ttAlignRight: Align := haRight;
    else raise EInternalExcept.Create('Invalid align token kind');
  end;
  LAlign.AddOrDel(T, Align);
  CurrentProps.Align := LAlign.Last;
end;

procedure TTokensProcess.DoLineSpace(T: TToken);
begin
  LLineSpace.AddOrDel(T, T.Value);
  CurrentProps.LineSpace := LLineSpace.Last;
end;

procedure TTokensProcess.DoTextAndRelated(T: TToken);
var
  Ex: TSize;
  Z: TPreObj_Visual;
  W: TDHVisualItem;
  FixedPos: TFixedPosition;
begin
  Ex := TSize.Create(0, 0);
  FixedPos := Default(TFixedPosition);

  case T.Kind of
    ttInvalid: T.Text := '<?>';
    ttListItem:
    begin
      if T.TagClose then Exit;
      if LHTMLList.Count=0 then Exit;

      if LHTMLList.Last is THTMLList_Number then
        Inc(THTMLList_Number(LHTMLList.Last).Sequence);

      if LHTMLList.Last is THTMLList_Bullet then T.Text := '• ' else
      if LHTMLList.Last is THTMLList_Number then T.Text := IntToStr(THTMLList_Number(LHTMLList.Last).Sequence)+'. ' else
        raise EInternalExcept.Create('Invalid HTML List object');

      FixedPos.Active := True;
      FixedPos.Left := LHTMLList.Count * Lb.FListLevelPadding;
    end;
  end;

  case T.Kind of
    ttImage:
    begin
      W := TDHVisualItem_Image.Create;
      with TDHVisualItem_Image(W) do
      begin
        ImageIndex := T.Value;
      end;

      {$IFDEF USE_IMGLST}
      if Assigned(Lb.FImages) then
      begin
        {$IFDEF FMX}
        with Lb.FImages.Destination[T.Value].Layers[0].SourceRect do
          Ex := TSize.Create(Width, Height);
        {$ELSE}
        Ex := TSize.Create(Lb.FImages.Width, Lb.FImages.Height);
        {$ENDIF}
      end;
      {$ENDIF}
    end;

    ttImageResource:
    begin
      W := TDHVisualItem_ImageResource.Create;
      with TDHVisualItem_ImageResource(W) do
      begin
        Load(Lb, T.Text);

        Ex := TSize.Create(Picture.Width, Picture.Height);
      end;
    end;

    else
    begin
      W := TDHVisualItem_Word.Create;
      with TDHVisualItem_Word(W) do
      begin
        Text := T.Text;
        Font.Assign(C.Font);
        {$IFDEF FMX}
        FontColor := C.Stroke.Color;
        {$ENDIF}

        Ex := TSize.Create(C.TextWidth(Text), C.TextHeight(Text));

        CheckSupSubScript(TDHVisualItem_Word(W), Ex);
      end;
    end;
  end;

  if Assigned(CurrentLink) and (CurrentLink is TDHLinkRef) and (T.Kind=ttText) then
    with TDHLinkRef(CurrentLink) do FText := FText + T.Text; //set link display text on the link data object

  W.BColor := CurrentProps.BackColor;
  W.Link := CurrentLink;

  Z := TPreObj_Visual.Create;
  Z.Size := Ex;
  Z.Align := CurrentProps.Align;
  Z.LineSpace := CurrentProps.LineSpace;
  Z.Space := (T.Kind=ttText) and (T.Text=STR_SPACE);
  Z.BreakableChar := (T.Kind=ttText) and (T.Value=INT_BREAKABLE_CHAR);
  Z.FixedPos := FixedPos;
  Z.Visual := W;

  Items.Add(Z);
end;

procedure TTokensProcess.CheckSupSubScript(W: TDHVisualItem_Word; var Size: TSize);
var
  OriginalFontSize: TFontPt;
  I: Integer;
  Tag: THTMLSupSubTag;
  H, Y, TextH, OuterY: TPixels;
begin
  if LSupAndSubScript.Count=0 then Exit;

  OriginalFontSize := C.Font.Size;

  H := Size.Height; //initial height
  OuterY := 0;

  for I := 0 to LSupAndSubScript.Count-1 do
  begin
    Tag := LSupAndSubScript[I];

    C.Font.Size := {$IFDEF VCL}Round{$ENDIF}(OriginalFontSize * Power(0.75, I+1));
    TextH := C.TextHeight(STR_SPACE);

    if Tag is THTMLSupTag then Y := 0 else
    if Tag is THTMLSubTag then Y := H - TextH else
      raise EInternalExcept.Create('Invalid sup/sub object');

    H := TextH;
    OuterY := OuterY + Y;
  end;

  //keep height but adjust new text width
  Size.Width := C.TextWidth(W.Text);
  W.Font.Size := C.Font.Size;
  W.YPos := OuterY;

  //restore canvas original font size
  C.Font.Size := OriginalFontSize;
end;

procedure TTokensProcess.DoLink(T: TToken);
var
  LinkRef: TDHLinkRef;
begin
  if T.TagClose then
    CurrentLink := nil
  else
  begin
    LinkRef := TDHLinkRef.Create;
    LinkRef.FTarget := T.Text;
    Lb.LLinkRef.Add(LinkRef); //add target of the link on list

    CurrentLink := LinkRef;
  end;
end;

procedure TTokensProcess.DoLists(T: TToken);
var
  &Class: TObjectListStackItemClass;
begin
  case T.Kind of
    ttBulletList: &Class := THTMLList_Bullet;
    ttNumberList: &Class := THTMLList_Number;
    else raise EInternalExcept.Create('Invalid HTML List token kind');
  end;

  LHTMLList.AddOrDel(T, &Class);
end;

procedure TTokensProcess.DoSupOrSubScript(T: TToken);
var
  &Class: TObjectListStackItemClass;
begin
  case T.Kind of
    ttSuperscript: &Class := THTMLSupTag;
    ttSubscript: &Class := THTMLSubTag;
    else raise EInternalExcept.Create('Invalid sup/sub token kind');
  end;

  LSupAndSubScript.AddOrDel(T, &Class);
end;

procedure TTokensProcess.DoFloat(T: TToken);
var Z: TPreObj_Float;
  Ar: TArray<string>;
begin
  Z := TPreObj_Float.Create;
  if not T.TagClose then
  begin
    Ar := T.Text.Split([',']);
    if Length(Ar)>=2 then
    begin
      Z.Rect.Left := StrToIntDef(Ar[0], 0);
      Z.Rect.Top := StrToIntDef(Ar[1], 0);
      if Length(Ar)>=3 then
        Z.Rect.Width := StrToIntDef(Ar[2], 0);
    end;
  end;
  Z.Close := T.TagClose;
  Items.Add(Z);
end;

procedure TTokensProcess.DoSpoilerTitle(T: TToken);
var DHSpoiler: TDHSpoiler;
begin
  //When first time rebuild (or after text changes), the LSpoiler is empty.
  //If rebuilding by spoiler click, the LSpoiler already contains all items.
  //Anyway, we need to check if spoiler exists because it could already exists
  //even at first building if there are multiple spoilers with same name.

  if T.TagClose then
    CurrentLink := nil
  else
  begin
    DHSpoiler := Lb.LSpoiler.Find(T.Text);
    if DHSpoiler=nil then
    begin
      DHSpoiler := TDHSpoiler.Create;
      DHSpoiler.FName := T.Text;
      Lb.LSpoiler.Add(DHSpoiler);
    end;
    CurrentLink := DHSpoiler;
  end;
end;

procedure TTokensProcess.DoSpoilerDetail(T: TToken);
begin
  LSpoilerDet.AddOrDel(T, THTMLSpoilerDet);
  if not T.TagClose then
    LSpoilerDet.Last.Name := T.Text;
end;

procedure TTokensProcess.DoTab(T: TToken);
var Z: TPreObj_Tab;
begin
  Z := TPreObj_Tab.Create;
  Z.Position := T.Value;
  Z.Fixed := (T.Kind=ttTabF);
  Items.Add(Z);
end;

procedure TTokensProcess.DoBreak;
var Z: TPreObj_Break;
begin
  Z := TPreObj_Break.Create;
  Z.Height := C.TextHeight(STR_SPACE);
  Items.Add(Z);
end;

//

procedure TTokensProcess.DefineVisualRect;
type TSizes = record
  LineHeight, LineSpace, OverallWidth, OverallHeight: TPixels;
end;
var
  Z: TPreObj;
  V: TPreObj_Visual;
  I: Integer;
  X, Y: TPixels;
  Max, OldMax: TSizes;
  LastTabX: TPixels; LastTabF: Boolean;
  PrevPos: TPoint; PrevLine, CurLine, LineCount: Integer;
  FloatRect: TRect; InFloat: Boolean;

  procedure IncPreviousGroup(Right, Limit: TPixels);
  var B: TGroupBound;
  begin
    B := TGroupBound.Create;
    B.Right := Right;
    B.Limit := Limit;
    LGroupBound.Add(B);
  end;

  function GetXbnd: TPixels;
  begin
    Result := FloatRect.Left + LastTabX;
  end;

  function IsToWrapText: Boolean;
  var J: Integer;
    EndPos: TPixels;
    PV: TPreObj_Visual;
  begin
    if TPreObj_Visual(Z).BreakCheckDone then Exit(False); //avoid re-break continuous text

    EndPos := X + TPreObj_Visual(Z).Size.Width;
    //if tags are used in the middle of a word, we need to check where text ends by breakable char
    for J := I+1 to Items.Count-1 do
    begin
      if not (Items[J] is TPreObj_Visual) then Break;
      PV := TPreObj_Visual(Items[J]);
      if PV.Space or PV.BreakableChar then Break; //Space always BreakableChar now?
      EndPos := EndPos + PV.Size.Width;
      PV.BreakCheckDone := True;
    end;

    if FloatRect.Width>0 then Exit(EndPos>FloatRect.Right);

    Result :=
      ( (Lb.FAutoWidth) and (Lb.FMaxWidth>0) and (EndPos>Lb.FMaxWidth) )
      or
      ( (not Lb.FAutoWidth) and (EndPos>Lb.GetAreaWidth) );
  end;

  procedure CheckPriorSpace;
  var PV: TPreObj_Visual;
  begin
    if (I>0) and (Items[I-1] is TPreObj_Visual) then
    begin
      PV := TPreObj_Visual(Items[I-1]);
      if PV.Space and (PV.Visual.Rect.Left>GetXbnd) then
      begin //space remains at previous line before line break
        PV.Print := False;
        X := PV.Visual.Rect.Left;
        Max := OldMax; //revert bounds
      end;
    end;
  end;

  procedure BreakGroupAndLineCtrl(Forward: Boolean; NewPoint: TPoint);
  var GrpLim: TPixels;
    LI: TLineInfo;
  begin
    GrpLim := -1;
    if FloatRect.Width>0 then GrpLim := FloatRect.Right;
    IncPreviousGroup(X, GrpLim);

    LI := TLineInfo.Create;
    LI.Height := Max.LineHeight;
    LI.Space := Max.LineSpace;
    LLineInfo.Add(LI);
    if Forward then
    begin
      CurLine := LLineInfo.Count;
      Max.LineHeight := 0;
      Max.LineSpace := 0;
    end else
    begin
      //restore line info
      CurLine := PrevLine;
      Max.LineHeight := LLineInfo[CurLine].Height;
      Max.LineSpace := LLineInfo[CurLine].Space;
    end;

    X := NewPoint.X;
    Y := NewPoint.Y;
  end;

begin
  X := 0;
  Y := 0;
  LineCount := 0;
  CurLine := 0;
  PrevLine := -1;
  PrevPos := TPoint.Create(0, 0);
  FloatRect := TRect.Empty;
  LastTabX := 0;
  LastTabF := False;
  InFloat := False;

  Max := Default(TSizes);
  OldMax := Default(TSizes);

  for I := 0 to Items.Count-1 do
  begin
    Z := Items[I];

    if Z is TPreObj_Float then
    begin
      if TPreObj_Float(Z).Close<>InFloat then Continue; //avoid float inside float
      if TPreObj_Float(Z).Close then
      begin
        BreakGroupAndLineCtrl(False, PrevPos);
        FloatRect := TRect.Empty;
        InFloat := False;
      end else
      begin
        PrevLine := CurLine; //save current line
        PrevPos := TPoint.Create(X, Y); //save current position
        BreakGroupAndLineCtrl(True, TPreObj_Float(Z).Rect.Location);
        FloatRect := TPreObj_Float(Z).Rect;
        InFloat := True;
      end;
      Continue;
    end;

    if Z is TPreObj_Tab then
    begin
      LastTabX := TPreObj_Tab(Z).Position;
      LastTabF := TPreObj_Tab(Z).Fixed;

      IncPreviousGroup(X, GetXbnd);
      X := GetXbnd;
      Continue;
    end;

    if (Z is TPreObj_Break) or
      ((Z is TPreObj_Visual) {and (X>GetXbnd)} and IsToWrapText) then
    begin //LINE BREAK
      if Z is TPreObj_Break then
      begin
        if Max.LineHeight=0 then Max.LineHeight := TPreObj_Break(Z).Height; //line without content
      end else
      if not TPreObj_Visual(Z).Space then //avoid duplicate space missing
        CheckPriorSpace; //remove space at previous line if is the last obj

      if not InFloat then Inc(LineCount);
      BreakGroupAndLineCtrl(True, TPoint.Create(FloatRect.Left, Y+Max.LineHeight+Max.LineSpace));
      //if line is empty, there is no visual item to check overall height
      if Y>Max.OverallHeight then Max.OverallHeight := Y;

      if (Z is TPreObj_Break) then
      begin
        LastTabX := 0;
        LastTabF := False;
        Continue;
      end;
      if LastTabF then X := GetXbnd;
      if TPreObj_Visual(Z).Space then Continue; //space made a line break
    end;

    if not (Z is TPreObj_Visual) then
      raise EInternalExcept.Create('Unexpected object');

    V := TPreObj_Visual(Z);

    if V.FixedPos.Active then X := V.FixedPos.Left;

    V.Visual.Rect := TRect.Create(X, Y, X+V.Size.Width, Y+V.Size.Height);
    V.Line := CurLine;
    V.Group := LGroupBound.Count;
    V.Print := True;

    OldMax := Max;
    if V.Visual.Rect.Right>Max.OverallWidth then Max.OverallWidth := V.Visual.Rect.Right;
    if V.Visual.Rect.Bottom>Max.OverallHeight then Max.OverallHeight := V.Visual.Rect.Bottom;
    if V.Visual.Rect.Height>Max.LineHeight then Max.LineHeight := V.Visual.Rect.Height;
    if V.LineSpace>Max.LineSpace then Max.LineSpace := V.LineSpace;

    X := V.Visual.Rect.Right;
  end;

  Lb.SetTextSize(Max.OverallWidth, Max.OverallHeight);
  Lb.FLineCount := LineCount;
end;

procedure TTokensProcess.CheckAlign(V: TPreObj_Visual);
type
  TFuncAlignResult = record
    Outside, Inside: TPixels;
  end;

  function funcAlignHorz: TFuncAlignResult;
  var
    B: TGroupBound;
    GrpLim: TPixels;
  begin
    B := LGroupBound[V.Group];
    if B.Limit = -1 then
    begin //group has no limit
      if Lb.FOverallHorzAlign in [haCenter, haRight] then
        GrpLim := Lb.FTextWidth
      else
        GrpLim := Lb.GetAreaWidth;
    end
      else GrpLim := B.Limit;

    Result.Outside := GrpLim;
    Result.Inside := B.Right;
  end;

  function funcAlignVert: TFuncAlignResult;
  begin
    Result.Outside := LLineInfo[V.Line].Height;
    Result.Inside := V.Visual.Rect.Height;
  end;

  function funcOverallAlignHorz: TFuncAlignResult;
  begin
    Result.Outside := Lb.GetAreaWidth;
    Result.Inside := Lb.FTextWidth;
  end;

  function funcOverallAlignVert: TFuncAlignResult;
  begin
    Result.Outside := Lb.GetAreaHeight;
    Result.Inside := Lb.FTextHeight;
  end;

  procedure Check(fnIndex: Byte; horz: Boolean; prop: Variant);
  var
    R: TFuncAlignResult;
    P: TPoint;
    Offset: TPixels;
  begin
    if prop>0 then //center or right
    begin
      case fnIndex of
        0: R := funcAlignHorz;
        1: R := funcAlignVert;
        2: R := funcOverallAlignHorz;
        3: R := funcOverallAlignVert;
      end;

      Offset := R.Outside - R.Inside;
      if prop=1 then Offset := {$IFDEF VCL}Round{$ENDIF}(Offset / 2); //center

      P := TPoint.Create(0, 0);
      if horz then
        P.X := Offset
      else
        P.Y := Offset;

      V.Visual.Rect.Offset(P);
    end;
  end;

begin
  Check(0, True, V.Align);
  Check(1, False, Lb.FLineVertAlign);

  Check(2, True, Lb.FOverallHorzAlign);
  Check(3, False, Lb.FOverallVertAlign);
end;

procedure TTokensProcess.Publish;
var
  Z: TPreObj;
  V: TPreObj_Visual;
begin
  for Z in Items do
  begin
    if not (Z is TPreObj_Visual) then Continue;
    V := TPreObj_Visual(Z);
    if not V.Print then Continue;

    CheckAlign(V);

    Lb.LVisualItem.Add(V.Visual);
    V.Visual := nil;
  end;
end;

{$REGION 'StyleLinkProp'}
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

function TDHStyleLinkProp.GetDefaultFontColor: TColor;
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

procedure TDHStyleLinkProp.SetFontColor(const Value: TColor);
begin
  if Value <> FFontColor then
  begin
    FFontColor := Value;

    Lb.BuildAndPaint;
  end;
end;

procedure TDHStyleLinkProp.SetBackColor(const Value: TColor);
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
  if FFontColor<>clNone then C.{$IFDEF FMX}Stroke{$ELSE}Font{$ENDIF}.Color := FFontColor;
  if FBackColor<>clNone then C.{$IFDEF FMX}Fill{$ELSE}Brush{$ENDIF}.Color := FBackColor;
  if FUnderline then C.Font.Style := C.Font.Style + [TFontStyle.fsUnderline];
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

procedure TDzHTMLText.SetStyleLink(const Index: Integer;
  const Value: TDHStyleLinkProp);
begin
  case Index of
    1: FStyleLinkNormal.Assign(Value);
    2: FStyleLinkHover.Assign(Value);
  end;
end;

function TDzHTMLText.GetStoredStyleLink(const Index: Integer): Boolean;
begin
  Result := False;
  case Index of
    1: Result := FStyleLinkNormal.GetStored;
    2: Result := FStyleLinkHover.GetStored;
  end;
end;
{$ENDREGION}

{$REGION 'Borders'}
procedure TDzHTMLText.SetBorders(const Value: TDHBorders);
begin
  FBorders.Assign(Value);
end;

function TDzHTMLText.GetStoredBorders: Boolean;
begin
  Result := FBorders.HasAnyValue;
end;

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

function TDHBorders.GetHorizontal: TPixels;
begin
  Result := FLeft + FRight;
end;

function TDHBorders.GetVertical: TPixels;
begin
  Result := FTop + FBottom;
end;

function TDHBorders.GetRealRect(R: TRect): TRect;
begin
  Result := R;
  Result.Offset(FLeft, FTop);
end;
{$ENDREGION}

function TDzHTMLText.GetStoredMaxWidth: Boolean;
begin
  Result := FMaxWidth <> 0;
end;

function TDzHTMLText.GetStoredLineSpacing: Boolean;
begin
  Result := FLineSpacing <> 0;
end;

function TDzHTMLText.GetStoredListLevelPadding: Boolean;
begin
  Result := FListLevelPadding <> _DEF_LISTLEVELPADDING;
end;

end.
