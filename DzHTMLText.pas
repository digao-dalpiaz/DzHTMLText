{------------------------------------------------------------------------------
TDzHTMLText component
Developed by Rodrigo Depiné Dalpiaz (digão dalpiaz)
Label with formatting tags support

https://github.com/digao-dalpiaz/DzHTMLText

Please, read the documentation at GitHub link.

Supported Tags:
<A[:abc]></A> - Link
<B></B> - Bold
<I></I> - Italic
<U></U> - Underline
<S></S> - Strike out
<FN:abc></FN> - Font Name
<FS:123></FS> - Font Size
<FC:clColor|$999999></FC> - Font Color
<BC:clColor|$999999></BC> - Background Color
<BR> - Line Break
<L></L> - Align Left
<C></C> - Align Center
<R></R> - Align Right
<T:123> - Tab
<TF:123> - Tab with aligned break
<IMG:nnn> - Image from ImageList where nnn is image index
------------------------------------------------------------------------------}

unit DzHTMLText;

{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
{$IFDEF FPC}
  Controls, Classes, Messages, Graphics, Types, FGL, LCLIntf, ImgList
{$ELSE}
  Vcl.Controls, System.Classes, Winapi.Messages, Vcl.ImgList,
  System.Generics.Collections, Vcl.Graphics, System.Types
{$ENDIF};

type
  {$IFDEF FPC}
  TObjectList<T> = class(TFPGObjectList<T>);
  TList<T> = class(TFPGList<T>);
  {$ENDIF}

  {DHWord is an object to each word. Will be used to paint event.
  The words are separated by space/tag/line break.}
  TDHWord = class
  private
    Rect: TRect;
    Text: String;
    Group: Integer; //group number
    {The group is isolated at each line or tabulation to delimit text align area}
    Align: TAlignment;
    Font: TFont;
    BColor: TColor; //background color
    Link: Boolean; //is a link
    LinkID: Integer; //link number
    {The link number is created sequentially, when reading text links
    and works to know the link target, stored on a TStringList, because if
    the link was saved here at a work, it will be repeat if has multiple words
    per link, spending a lot of unnecessary memory.}
    Space: Boolean; //is an space

    Line: Integer; //line number
    ImageIndex: Integer;

    Hover: Boolean; //the mouse is over the link
  public
    constructor Create;
    destructor Destroy; override;
  end;
  TDHWordList = class(TObjectList<TDHWord>)
  private
    procedure Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
      Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean; Line: Integer; ImageIndex: Integer);
      {$IFDEF FPC}reintroduce;{$ENDIF}
  end;

  TDzHTMLText = class;

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
    constructor Create(xLb: TDzHTMLText; xKind: TDHKindStyleLinkProp);
    procedure Assign(Source: TPersistent); override;
  published
    property FontColor: TColor read FFontColor write SetFontColor stored GetStoredFontColor;
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
    property Underline: Boolean read FUnderline write SetUnderline default False;
  end;

  TDHLinkData = class
  private
    FTarget: String;
    FText: String;
  public
    property Target: String read FTarget;
    property Text: String read FText;
  end;
  TDHLinkDataList = class(TObjectList<TDHLinkData>);

  TDHEvLink = procedure(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData) of object;
  TDHEvLinkClick = procedure(Sender: TObject; LinkID: Integer; LinkData: TDHLinkData; var Handled: Boolean) of object;

  TDHLineVertAlign = (vaTop, vaCenter, vaBottom);

  TDzHTMLText = class(TGraphicControl)
  private
    FAbout: String;

    LWords: TDHWordList; //word list to paint event
    LLinkData: TDHLinkDataList; //list of links info

    FText: String;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    FMaxWidth: Integer; //max width when using AutoWidth
    //FTransparent: Boolean; //not used because of flickering
    FAutoOpenLink: Boolean; //link auto-open with ShellExecute

    FLines: Integer; //read-only
    FTextWidth: Integer; //read-only
    FTextHeight: Integer; //read-only

    FStyleLinkNormal, FStyleLinkHover: TDHStyleLinkProp;

    FImages: TCustomImageList;

    FLineVertAlign: TDHLineVertAlign;

    FOnLinkEnter, FOnLinkLeave: TDHEvLink;
    FOnLinkClick, FOnLinkRightClick: TDHEvLinkClick;

    FIsLinkHover: Boolean; //if has a selected link
    FSelectedLinkID: Integer; //selected link ID

    NoCursorChange: Boolean; //lock CursorChange event
    DefaultCursor: TCursor; //default cursor when not over a link

    procedure SetText(const Value: String);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetMaxWidth(const Value: Integer);

    function GetStoredStyleLink(const Index: Integer): Boolean;
    procedure SetStyleLink(const Index: Integer; const Value: TDHStyleLinkProp);

    procedure DoPaint;
    procedure Rebuild; //rebuild words
    procedure BuildAndPaint; //rebuild and repaint
    procedure CheckMouse(X, Y: Integer); //check links by mouse position
    procedure SetCursorWithoutChange(C: TCursor);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetLineVertAlign(const Value: TDHLineVertAlign);
    //procedure SetTransparent(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure Click; override;
    procedure Resize; override;

    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure CMCursorchanged(var Message: TMessage); message CM_CURSORCHANGED;

    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsLinkHover: Boolean read FIsLinkHover;
    property SelectedLinkID: Integer read FSelectedLinkID;
    function GetLinkData(LinkID: Integer): TDHLinkData; //get data by link id
    function GetSelectedLinkData: TDHLinkData; //get data of selected link

  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;

    {$IFDEF DCC}
    property OnGesture;
    property OnMouseActivate;
    {$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;

    property Text: String read FText write SetText;
    //property Transparent: Boolean read FTransparent write SetTransparent default False;

    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default False;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default False;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 0;

    property StyleLinkNormal: TDHStyleLinkProp index 1 read FStyleLinkNormal write SetStyleLink stored GetStoredStyleLink;
    property StyleLinkHover: TDHStyleLinkProp index 2 read FStyleLinkHover write SetStyleLink stored GetStoredStyleLink;

    property Images: TCustomImageList read FImages write SetImages;

    property Lines: Integer read FLines;
    property TextWidth: Integer read FTextWidth;
    property TextHeight: Integer read FTextHeight;

    property OnLinkEnter: TDHEvLink read FOnLinkEnter write FOnLinkEnter;
    property OnLinkLeave: TDHEvLink read FOnLinkLeave write FOnLinkLeave;
    property OnLinkClick: TDHEvLinkClick read FOnLinkClick write FOnLinkClick;
    property OnLinkRightClick: TDHEvLinkClick read FOnLinkRightClick write FOnLinkRightClick;

    property AutoOpenLink: Boolean read FAutoOpenLink write FAutoOpenLink default True;

    property LineVertAlign: TDHLineVertAlign read FLineVertAlign write SetLineVertAlign default vaTop;

    property About: String read FAbout;
  end;

procedure Register;

implementation

uses
{$IFDEF FPC}
  {$IFDEF MSWINDOWS}Windows, {$ENDIF}SysUtils, LResources
{$ELSE}
  System.SysUtils, System.UITypes, Winapi.Windows, Winapi.ShellAPI
{$ENDIF};

procedure Register;
begin
  {$IFDEF FPC}{$I DzHTMLText.lrs}{$ENDIF}
  RegisterComponents('Digao', [TDzHTMLText]);
end;

//

constructor TDHWord.Create;
begin
  inherited;
  Font := TFont.Create;
end;

destructor TDHWord.Destroy;
begin
  Font.Free;
  inherited;
end;

procedure TDHWordList.Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
  Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean; Line: Integer; ImageIndex: Integer);
var W: TDHWord;
begin
  W := TDHWord.Create;
  inherited Add(W);

  W.Rect := Rect;
  W.Text := Text;
  W.Group := Group;
  W.Align := Align;
  W.Font.Assign(Font);
  W.BColor := BColor;
  W.Link := Link;
  W.LinkID := LinkID;
  W.Space := Space;
  W.Line := Line;
  W.ImageIndex := ImageIndex;
end;

//

constructor TDzHTMLText.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csOpaque];
  //Warning! The use of transparency in the component causes flickering

  FAbout := 'Digao Dalpiaz / Version 1.1';

  FStyleLinkNormal := TDHStyleLinkProp.Create(Self, tslpNormal);
  FStyleLinkHover := TDHStyleLinkProp.Create(Self, tslpHover);
  LWords := TDHWordList.Create;
  LLinkData := TDHLinkDataList.Create;

  FAutoOpenLink := True;

  FSelectedLinkID := -1;

  DefaultCursor := Cursor;

  {$IFDEF FPC}
  //Lazarus object starts too small
  Width := 200;
  Height := 100;
  {$ENDIF}
end;

destructor TDzHTMLText.Destroy;
begin
  FStyleLinkNormal.Free;
  FStyleLinkHover.Free;
  LWords.Free;
  LLinkData.Free;
  inherited;
end;

procedure TDzHTMLText.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then
      FImages := nil;
  end;
end;

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

procedure TDzHTMLText.Loaded;
begin
  {Warning! When a component is inserted at design-time, the Loaded
  is not fired, because there is nothing to load. The Loaded is only fired
  when loading component that already has saved properties on DFM file.}
  inherited;
  Rebuild;
end;

procedure TDzHTMLText.BuildAndPaint;
begin
  //Rebuild words and repaint
  Rebuild;
  Invalidate;
end;

procedure TDzHTMLText.SetAutoHeight(const Value: Boolean);
begin
  if Value<>FAutoHeight then
  begin
    FAutoHeight := Value;

    if Value then Rebuild;
  end;
end;

procedure TDzHTMLText.SetAutoWidth(const Value: Boolean);
begin
  if Value<>FAutoWidth then
  begin
    FAutoWidth := Value;

    if Value then Rebuild;
  end;
end;

procedure TDzHTMLText.SetMaxWidth(const Value: Integer);
begin
  if Value<>FMaxWidth then
  begin
    FMaxWidth := Value;

    Rebuild;
  end;
end;

procedure TDzHTMLText.SetText(const Value: String);
begin
  if Value<>FText then
  begin
    FText := Value;

    BuildAndPaint;
  end;
end;

procedure TDzHTMLText.SetLineVertAlign(const Value: TDHLineVertAlign);
begin
  if Value<>FLineVertAlign then
  begin
    FLineVertAlign := Value;

    BuildAndPaint;
  end;
end;

{procedure TDzHTMLText.SetTransparent(const Value: Boolean);
begin
  if Value<>FTransparent then
  begin
    FTransparent := Value;

    Invalidate;
  end;
end;}

procedure TDzHTMLText.CMColorchanged(var Message: TMessage);
begin
  {$IFDEF FPC}if Message.Result=0 then {};{$ENDIF} //avoid unused var warning
  Invalidate;
end;

procedure TDzHTMLText.CMFontchanged(var Message: TMessage);
begin
  {$IFDEF FPC}if Message.Result=0 then {};{$ENDIF} //avoid unused var warning
  BuildAndPaint;
end;

procedure TDzHTMLText.Resize;
begin
  //on component creating, there is no parent and the resize is fired,
  //so, the canvas is not present at this moment.
  if HasParent then
    Rebuild;

  inherited;
end;

procedure TDzHTMLText.Paint;
begin
  inherited;
  DoPaint;
end;

procedure TDzHTMLText.DoPaint;
var W: TDHWord;
    B: {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap;
begin
  //Using internal bitmap as a buffer to reduce flickering
  B := {$IFDEF DCC}Vcl.{$ENDIF}Graphics.TBitmap.Create;
  try
    B.SetSize(Width, Height);

    //if not FTransparent then
    //begin
      {$IFDEF FPC}
      if (Color=clDefault) and (ParentColor) then B.Canvas.Brush.Color := GetColorresolvingParent else
      {$ENDIF}
      B.Canvas.Brush.Color := Color;
      B.Canvas.FillRect(ClientRect);
    //end;

    if csDesigning in ComponentState then
    begin
      B.Canvas.Pen.Style := psDot;
      B.Canvas.Pen.Color := clBtnShadow;
      B.Canvas.Brush.Style := bsClear;
      B.Canvas.Rectangle(ClientRect);
    end;

    for W in LWords do
    begin
      B.Canvas.Font.Assign(W.Font);

      if W.BColor<>clNone then
        B.Canvas.Brush.Color := W.BColor
      else
        B.Canvas.Brush.Style := bsClear;

      if W.Link then
      begin
          if W.Hover then //selected
            FStyleLinkHover.SetPropsToCanvas(B.Canvas)
          else
            FStyleLinkNormal.SetPropsToCanvas(B.Canvas);
      end;

      if W.ImageIndex>-1 then //is an image
      begin
        if Assigned(FImages) then
          FImages.Draw(B.Canvas, W.Rect.Left, W.Rect.Top, W.ImageIndex);
      end else
        DrawText(B.Canvas.Handle,
          {$IFDEF FPC}PChar({$ENDIF}W.Text{$IFDEF FPC}){$ENDIF},
          -1, W.Rect, DT_NOCLIP or DT_NOPREFIX);
      {Using DrawText, because TextOut has not clip option, which causes
      bad overload of text when painting using background, oversizing the
      text area wildly.}
    end;

    Canvas.Draw(0, 0, B); //to reduce flickering
  finally
    B.Free;
  end;
end;

function TDzHTMLText.GetLinkData(LinkID: Integer): TDHLinkData;
begin
  Result := LLinkData[LinkID];
end;

function TDzHTMLText.GetSelectedLinkData: TDHLinkData;
begin
  Result := LLinkData[FSelectedLinkID];
end;

procedure TDzHTMLText.CMCursorchanged(var Message: TMessage);
begin
  {$IFDEF FPC}if Message.Result=0 then {};{$ENDIF} //avoid unused var warning

  if NoCursorChange then Exit;

  DefaultCursor := Cursor; //save default cursor to when link not selected
end;

procedure TDzHTMLText.SetCursorWithoutChange(C: TCursor);
begin
  //Set cursor, but without fire cursor change event
  NoCursorChange := True;
  try
    Cursor := C;
  finally
    NoCursorChange := False;
  end;
end;

procedure TDzHTMLText.CheckMouse(X, Y: Integer);
var FoundHover, HasChange, Old: Boolean;
    LinkID: Integer;
    W: TDHWord;
begin
  FoundHover := False;
  HasChange := False;
  LinkID := -1;

  //find the first word, if there is any
  for W in LWords do
    if W.Link then
    begin
      if W.Rect.Contains({$IFDEF FPC}Types.{$ENDIF}Point(X, Y)) then //selected
      begin
        FoundHover := True; //found word of a link selected
        LinkID := W.LinkID;

        Break;
      end;
    end;

  //set as selected all the words of same link, and unselect another links
  for W in LWords do
    if W.Link then
    begin
      Old := W.Hover;
      W.Hover := (W.LinkID = LinkID);
      if Old<>W.Hover then HasChange := True; //changed
    end;

  if HasChange then //there is any change
  begin
    if FoundHover then //enter the link
    begin
      SetCursorWithoutChange(crHandPoint); //set HandPoint cursor
      FIsLinkHover := True;
      FSelectedLinkID := LinkID;
      if Assigned(FOnLinkEnter) then
        FOnLinkEnter(Self, LinkID, LLinkData[LinkID]);
    end else
    begin //leave the link
      SetCursorWithoutChange(DefaultCursor); //back to default cursor
      FIsLinkHover := False;
      LinkID := FSelectedLinkID; //save to use on OnLinkLeave event
      FSelectedLinkID := -1;
      if Assigned(FOnLinkLeave) then
        FOnLinkLeave(Self, LinkID, LLinkData[LinkID]);
    end;

    Invalidate;
  end;
end;

procedure TDzHTMLText.Click;
var Handled: Boolean;
  aTarget: String;
begin
  if FIsLinkHover then
  begin
    Handled := False;
    if Assigned(FOnLinkClick) then
      FOnLinkClick(Self, FSelectedLinkID, LLinkData[FSelectedLinkID], Handled);

    if FAutoOpenLink and not Handled then
    begin
      aTarget := LLinkData[FSelectedLinkID].FTarget;
      {$IFDEF MSWINDOWS}
      ShellExecute(0, '', PChar(aTarget), '', '', 0);
      {$ELSE}
      if aTarget.StartsWith('http://', True)
        or aTarget.StartsWith('https://', True)
        or aTarget.StartsWith('www.', True)
      then
        OpenURL(aTarget)
      else
        OpenDocument(aTarget);
      {$ENDIF}
    end;
  end;

  inherited;
end;

procedure TDzHTMLText.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Handled: Boolean;
begin
  if Button = mbRight then
    if IsLinkHover then
      if Assigned(FOnLinkRightClick) then
      begin
        Handled := False;
        FOnLinkRightClick(Self, FSelectedLinkID, LLinkData[FSelectedLinkID], Handled);
      end;

  inherited;
end;

procedure TDzHTMLText.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  CheckMouse(X, Y);

  inherited;
end;

procedure TDzHTMLText.CMMouseleave(var Message: TMessage);
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
    ttTab, ttTabF, ttSpace,
    ttBreak, ttText, ttLink,
    ttAlignLeft, ttAlignCenter, ttAlignRight,
    ttImage);

  TToken = class
    Kind: TTokenKind;
    TagClose: Boolean;
    Text: String;
    Value: Integer;
  end;

  TListToken = class(TObjectList<TToken>)
    function GetLinkText(IEnd: Integer): String;
  end;

  TBuilder = class
  private
    Lb: TDzHTMLText;
    L: TListToken;
    LGroupBound: TList<Integer>; //bounds list of the group
    {The list of created with the X position of limit where the group ends
     to use on text align until the group limit}

    LinesHeight: TList<Integer>;

    CalcWidth, CalcHeight: Integer; //width and height to set at component when using auto

    function ProcessTag(const Tag: String): Boolean;
    procedure AddToken(aKind: TTokenKind; aTagClose: Boolean = False; aText: String = ''; aValue: Integer = 0);

    procedure BuildTokens; //create list of tokens
    procedure BuildWords; //create list of words
    procedure CheckAligns; //realign words
  public
    constructor Create;
    destructor Destroy; override;
  end;

constructor TBuilder.Create;
begin
  inherited;
  L := TListToken.Create;
  LGroupBound := TList<Integer>.Create;
  LinesHeight := TList<Integer>.Create;
end;

destructor TBuilder.Destroy;
begin
  L.Free;
  LGroupBound.Free;
  LinesHeight.Free;
  inherited;
end;

procedure TDzHTMLText.Rebuild;
var B: TBuilder;
begin
  if csLoading in ComponentState then Exit;

  LWords.Clear; //clean old words
  LLinkData.Clear; //clean old links

  B := TBuilder.Create;
  try
    B.Lb := Self;

    B.BuildTokens;
    B.BuildWords;
    B.CheckAligns;

    FTextWidth := B.CalcWidth;
    FTextHeight := B.CalcHeight;

    if FAutoWidth then Width := B.CalcWidth;
    if FAutoHeight then Height := B.CalcHeight;

  finally
    B.Free;
  end;
end;

//

function ReplaceForcedChars(A: String): String;
begin
  //Allow tag characters at text

  A := StringReplace(A, '&lt;', '<', [rfReplaceAll]);
  A := StringReplace(A, '&gt;', '>', [rfReplaceAll]);

  Result := A;
end;

function ParamToColor(A: String): TColor;
begin
  if A.StartsWith('$') then Insert('00', A, 2);
  {At HTML, is used Hexadecimal color code with 6 digits, the same used at
  this component. However the Delphi works with 8 digits, but the first two
  digits are always "00"}

  try
    Result := StringToColor(A);
  except
    Result := clNone;
  end;
end;

procedure TBuilder.AddToken(aKind: TTokenKind; aTagClose: Boolean = False; aText: String = ''; aValue: Integer = 0);
var T: TToken;
begin
  T := TToken.Create;
  T.Kind := aKind;
  T.TagClose := aTagClose;
  T.Text := aText;
  T.Value := aValue;
  L.Add(T);
end;

function Tag_Index_ProcValue(const Value: String; var Valid: Boolean): Integer;
begin
  Result := StrToIntDef(Value, -1);
  Valid := (Result>-1);
end;

function Tag_Number_ProcValue(const Value: String; var Valid: Boolean): Integer;
begin
  Result := StrToIntDef(Value, 0);
  Valid := (Result>0);
end;

function Tag_Color_ProcValue(const Value: String; var Valid: Boolean): Integer;
begin
  Result := ParamToColor(Value);
  Valid := (Result<>clNone);
end;

type TDefToken = record
  Ident: String;
  Kind: TTokenKind;
  Single: Boolean; //without close tag
  AllowPar, OptionalPar: Boolean;
  ProcValue: function(const Value: String; var Valid: Boolean): Integer;
end;
const DEF_TOKENS: array[0..15] of TDefToken = (
  (Ident: 'BR'; Kind: ttBreak; Single: True),
  (Ident: 'B'; Kind: ttBold),
  (Ident: 'I'; Kind: ttItalic),
  (Ident: 'U'; Kind: ttUnderline),
  (Ident: 'S'; Kind: ttStrike),
  (Ident: 'FN'; Kind: ttFontName; AllowPar: True),
  (Ident: 'FS'; Kind: ttFontSize; AllowPar: True; ProcValue: Tag_Number_ProcValue),
  (Ident: 'FC'; Kind: ttFontColor; AllowPar: True; ProcValue: Tag_Color_ProcValue),
  (Ident: 'BC'; Kind: ttBackColor; AllowPar: True; ProcValue: Tag_Color_ProcValue),
  (Ident: 'A'; Kind: ttLink; AllowPar: True; OptionalPar: True),
  (Ident: 'L'; Kind: ttAlignLeft),
  (Ident: 'C'; Kind: ttAlignCenter),
  (Ident: 'R'; Kind: ttAlignRight),
  (Ident: 'T'; Kind: ttTab; Single: True; AllowPar: True; ProcValue: Tag_Number_ProcValue),
  (Ident: 'TF'; Kind: ttTabF; Single: True; AllowPar: True; ProcValue: Tag_Number_ProcValue),
  (Ident: 'IMG'; Kind: ttImage; Single: True; AllowPar: True; ProcValue: Tag_Index_ProcValue)
);

function TBuilder.ProcessTag(const Tag: String): Boolean;
var TOff, TOn, HasPar, ValidPar: Boolean;
    Value: Integer;
    A, Par: String;
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
  Par := '';
  I := Pos(':', A); //find parameter
  if I>0 then //has parameter
  begin
    HasPar := True;
    Par := A.Substring(I); //zero-based
    A := Copy(A, 1, I-1);
  end;

  if HasPar then
  begin
    if Par='' then Exit; //blank parameter specified
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

procedure TBuilder.BuildTokens;
var Text, A: String;
    CharIni: Char;
    I, Jump: Integer;
begin
  Text := StringReplace(Lb.FText, #13#10, '<BR>', [rfReplaceAll]);
  while Text<>'' do
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
    if CharIni = ' ' then //space
    begin
      AddToken(ttSpace, False, ' ');
      Jump := 1;
    end else
    if CharInSet(CharIni, ['/','\']) then
    begin
      //this is to break line when using paths
      AddToken(ttText, False, CharIni);
      Jump := 1;
    end else
    begin //all the rest is text
      I := A.IndexOfAny([' ','<','>','/','\'])+1; //warning: 0-based function!!!
      if I=0 then I := Length(A)+1;

      Dec(I);
      A := Copy(A, 1, I);
      AddToken(ttText, False, ReplaceForcedChars(A));
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

procedure TBuilder.BuildWords;
var C: TCanvas;

    X, Y, HighW, HighH, LineCount: Integer;
    LastTabF: Boolean; //last tabulation was TabF (with break align)
    LastTabF_X: Integer;

  procedure DoLineBreak;
  begin
    if HighH=0 then HighH := C.TextHeight(' '); //line without content
    Inc(Y, HighH); //inc biggest height of the line
    LinesHeight.Add(HighH); //include total line height in list
    HighH := 0; //clear line height

    if X>HighW then HighW := X; //store width of biggest line
    X := 0; //carriage return :)
    if LastTabF then X := LastTabF_X; //last line breaks with TabF

    LGroupBound.Add(Lb.Width); //add line bound to use in group align
    Inc(LineCount);
  end;

var
  T: TToken;
  I: Integer;

  Ex: TSize; FS: TFontStyles; PreWidth: Integer;

  LinkOn: Boolean;
  LinkID: Integer;

  BackColor: TColor;
  Align: TAlignment;

  LBold: TListStack<Boolean>;
  LItalic: TListStack<Boolean>;
  LUnderline: TListStack<Boolean>;
  LStrike: TListStack<Boolean>;
  LFontName: TListStack<String>;
  LFontSize: TListStack<Integer>;
  LFontColor: TListStack<TColor>;
  LBackColor: TListStack<TColor>;
  LAlign: TListStack<TAlignment>;

  LinkData: TDHLinkData;

  ImageIndex: Integer;

  vBool: Boolean; //Required for Lazarus
begin
  C := Lb.Canvas;
  C.Font.Assign(Lb.Font);

  BackColor := clNone;
  Align := taLeftJustify;

  LBold := TListStack<Boolean>.Create;
  LItalic := TListStack<Boolean>.Create;
  LUnderline := TListStack<Boolean>.Create;
  LStrike := TListStack<Boolean>.Create;
  LFontName := TListStack<String>.Create;
  LFontSize := TListStack<Integer>.Create;
  LFontColor := TListStack<TColor>.Create;
  LBackColor := TListStack<TColor>.Create;
  LAlign := TListStack<TAlignment>.Create;
  try
    vBool := fsBold in C.Font.Style; LBold.Add(vBool);
    vBool := fsItalic in C.Font.Style; LItalic.Add(vBool);
    vBool := fsUnderline in C.Font.Style; LUnderline.Add(vBool);
    vBool := fsStrikeOut in C.Font.Style; LStrike.Add(vBool);
    LFontName.Add(C.Font.Name);
    LFontSize.Add(C.Font.Size);
    LFontColor.Add(C.Font.Color);
    LBackColor.Add(BackColor);
    LAlign.Add(Align);

    X := 0;
    Y := 0;

    HighW := 0;
    HighH := 0;

    LineCount := 0;

    LastTabF := False;
    LastTabF_X := 0;

    LinkOn := False;
    LinkID := -1;

    for I := 0 to L.Count-1 do
    begin
      T := L[I];

      case T.Kind of
        ttBold, ttItalic, ttUnderline, ttStrike:
          begin
            case T.Kind of
              ttBold: LBold.AddOrDel(T, True);
              ttItalic: LItalic.AddOrDel(T, True);
              ttUnderline: LUnderline.AddOrDel(T, True);
              ttStrike: LStrike.AddOrDel(T, True);
            end;

            FS := [];
            if LBold.Last then Include(FS, fsBold);
            if LItalic.Last then Include(FS, fsItalic);
            if LUnderline.Last then Include(FS, fsUnderline);
            if LStrike.Last then Include(FS, fsStrikeOut);
            C.Font.Style := FS;
          end;
        ttFontName:
          begin
            LFontName.AddOrDel(T, T.Text);
            C.Font.Name := LFontName.Last;
          end;
        ttFontSize:
          begin
            LFontSize.AddOrDel(T, T.Value);
            C.Font.Size := LFontSize.Last;
          end;
        ttFontColor:
          begin
            LFontColor.AddOrDel(T, T.Value);
            C.Font.Color := LFontColor.Last;
          end;
        ttBackColor:
          begin
            LBackColor.AddOrDel(T, T.Value);
            BackColor := LBackColor.Last;
          end;

        ttAlignLeft, ttAlignCenter, ttAlignRight:
          begin
            case T.Kind of
              ttAlignLeft: Align := taLeftJustify;
              ttAlignCenter: Align := taCenter;
              ttAlignRight: Align := taRightJustify;
            end;
            LAlign.AddOrDel(T, Align);
            Align := LAlign.Last;
          end;

        ttText, ttSpace, ttInvalid, ttImage:
        begin
          case T.Kind of
            ttSpace: T.Text := ' ';
            ttInvalid: T.Text := '<?>';
          end;

          if T.Kind=ttImage then
          begin
            if Assigned(Lb.FImages) then
            begin
              Ex.Width := Lb.FImages.Width;
              Ex.Height := Lb.FImages.Height;
            end else
            begin
              Ex.Width := 0;
              Ex.Height := 0;
            end;

            ImageIndex := T.Value;
          end else
          begin
            Ex := C.TextExtent(T.Text);

            ImageIndex := -1;
          end;

          PreWidth := X+Ex.Width;
          if ((Lb.FAutoWidth) and (Lb.FMaxWidth>0) and (PreWidth>Lb.FMaxWidth))
            or ((not Lb.FAutoWidth) and (PreWidth>Lb.Width)) then
          begin
            //clear last word on line break when is space to not consume pixels at end of line
            if Lb.LWords.Count>0 then
              if Lb.LWords.Last.Space then
              begin
                Dec(X, Lb.LWords.Last.Rect.Width);
                Lb.LWords.Delete(Lb.LWords.Count-1);
              end;

            DoLineBreak;
            if T.Kind=ttSpace then Continue;
          end;
          if Ex.Height>HighH then HighH := Ex.Height; //biggest height of the line

          Lb.LWords.Add({$IFDEF FPC}Types.{$ENDIF}Rect(X, Y, X+Ex.Width, Y+Ex.Height),
            T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, T.Kind=ttSpace,
            LineCount, ImageIndex);

          Inc(X, Ex.Width);
        end;

        ttLink:
        begin
          if T.TagClose then
          begin
            if LinkID<>-1 then
              Lb.LLinkData[LinkID].FText := L.GetLinkText(I); //set link display text on the link data object

            LinkOn := False;
            LinkID := -1;
          end else
          begin
            LinkData := TDHLinkData.Create;
            LinkData.FTarget := T.Text;

            LinkOn := True;
            LinkID := Lb.LLinkData.Add(LinkData); //add target of the link on list
          end;
        end;

        ttTab, ttTabF:
        begin
          X := T.Value; //cursor position

          LastTabF := T.Kind=ttTabF;
          LastTabF_X := X;

          LGroupBound.Add(X); //add bound on last group to use at text align
        end;

        ttBreak:
        begin
          LastTabF := False; //clear TabF
          DoLineBreak;
        end;
      end;
    end;
  finally
    LBold.Free;
    LItalic.Free;
    LUnderline.Free;
    LStrike.Free;
    LFontName.Free;
    LFontSize.Free;
    LFontColor.Free;
    LBackColor.Free;
    LAlign.Free;
  end;

  if Lb.LWords.Count>0 then DoLineBreak;
  CalcWidth := HighW;
  CalcHeight := Y;
  Lb.FLines := LineCount;
end;

procedure TBuilder.CheckAligns;
var W: TDHWord;
    LW: array of Integer;
    Group, I, SumW, Offset: Integer;
begin
  SetLength(LW, LGroupBound.Count);

  Group := -1;
  SumW := 0;

  for I := 0 to Lb.LWords.Count-1 do
  begin
    W := Lb.LWords[I];

    if W.Group<>Group then //enter new group
    begin
      if I>0 then
        LW[Group] := SumW; //add last group width sum

      Group := W.Group;
      SumW := W.Rect.Left; //where first group starts
    end;

    Inc(SumW, W.Rect.Width);
    if I=Lb.LWords.Count-1 then LW[Group] := SumW;
  end;

  for W in Lb.LWords do
  begin
    //horizontal align
    if W.Align in [taCenter, taRightJustify] then
    begin
      Offset := LGroupBound[W.Group] - LW[W.Group];
      if W.Align=taCenter then Offset := Offset div 2;

      W.Rect.Offset(Offset, 0);
    end;

    //vertical align
    if Lb.FLineVertAlign in [vaCenter, vaBottom] then
    begin
      Offset := LinesHeight[W.Line] - W.Rect.Height;
      if Lb.FLineVertAlign=vaCenter then Offset := Offset div 2;

      W.Rect.Offset(0, Offset);
    end;
  end;
end;

{$REGION 'StyleLinkProp'}
constructor TDHStyleLinkProp.Create(xLb: TDzHTMLText; xKind: TDHKindStyleLinkProp);
begin
  inherited Create;

  Lb := xLb;
  Kind := xKind;

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
    tslpNormal: Result := clBlue;
    tslpHover: Result := clRed;
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
  if FFontColor<>clNone then C.Font.Color := FFontColor;
  if FBackColor<>clNone then C.Brush.Color := FBackColor;
  if FUnderline then C.Font.Style := C.Font.Style + [fsUnderline];
end;

procedure TDHStyleLinkProp.Assign(Source: TPersistent);
begin
  if Source is TDHStyleLinkProp then
  begin
    Self.FFontColor := TDHStyleLinkProp(Source).FFontColor;
    Self.FBackColor := TDHStyleLinkProp(Source).FBackColor;
    Self.FUnderline := TDHStyleLinkProp(Source).FUnderline;
  end else
    inherited;
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

{ TListToken }

function TListToken.GetLinkText(IEnd: Integer): String;
var I: Integer;
  T: TToken;
begin
  //returns the link display text where IEnd is Link Close tag Token on the list
  //so, it will start from the end until find the Link Open tag.

  Result := '';
  for I := IEnd-1 downto 0 do
  begin
    T := Items[I];
    if T.Kind = ttLink then Break; //should be open tag

    if T.Kind in [ttText, ttSpace] then
      Result := T.Text + Result;
  end;
end;

end.
