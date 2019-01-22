{THTLabel component
Developed by Rodrigo Dalpiaz
Label with formatting tags support

<A[:abc]></A> - Link
<B></B> - Bold
<I></I> - Italic
<U></U> - Underline
<S></S> - Strike out
<FN:abc></FN> - Font Name
<FS:123></FS> - Font Size
<FC:clcolor|$999999></FC> - Font Color
<BC:clcolor|$999999></BC> - Background Color
<BR> - Line Break
<L></L> - Align Left
<C></C> - Align Center
<R></R> - Aligh Right
<T:123></T> - Tab
<TF:123></TF> - Tab with aligned break
}

unit HTLabel;

interface

uses Vcl.Controls, System.Classes, Winapi.Messages,
  System.Generics.Collections, Vcl.Graphics, System.Types;

type
  {HTWord is an object to each word. Will be used to paint event.
  The words are separated by space/tag/line break.}
  THTWord = class
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
    {The link number is created sequencially, when reading text links
    and works to know the link target, stored on a TStringList, because if
    the link was saved here at a work, it will be repeat if has multiple words
    per link, spending a lot of unnecessary memory.}
    Space: Boolean; //is an space

    Hover: Boolean; //the mouse is over the link
  public
    constructor Create;
    destructor Destroy; override;
  end;
  THTWordList = class(TObjectList<THTWord>)
  private
    procedure Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
      Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean);
  end;

  THTLabel = class;

  THTTipoStyleLinkProp = (tslpNormal, tslpHover); //kind of link style

  {HTStyleLinkProp is a subproperty used at Object Inspector that contains
   link formatting when selected and not selected}
  THTStyleLinkProp = class(TPersistent)
  private
    Lb: THTLabel; //owner
    Kind: THTTipoStyleLinkProp;

    FFontColor: TColor;
    FBackColor: TColor;
    FUnderline: Boolean;
    procedure SetFontColor(const Value: TColor);
    procedure SetBackColor(const Value: TColor);
    procedure SetUnderline(const Value: Boolean);
    function GetDefaultFontColor: TColor;
    function GetStoredFontColor: Boolean;
    procedure SetPropsToCanvas(C: TCanvas); //method to use at paint event
    function GetStored: Boolean; //getstored general to use at owner
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(xLb: THTLabel; xKind: THTTipoStyleLinkProp);
    procedure Assign(Source: TPersistent); override;
  published
    property FontColor: TColor read FFontColor write SetFontColor stored GetStoredFontColor;
    property BackColor: TColor read FBackColor write SetBackColor default clNone;
    property Underline: Boolean read FUnderline write SetUnderline default False;
  end;

  THTEvLink = procedure(Sender: TObject; LinkID: Integer; Target: String) of object;
  THTEvLinkClick = procedure(Sender: TObject; LinkID: Integer; Target: String; var Handled: Boolean) of object;

  THTLabel = class(TGraphicControl)
  private
    LWords: THTWordList; //word list to paint event
    LLinkTargets: TStringList; //target list of links

    FText: String;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    FMaxWidth: Integer; //max width when using AutoWidth
    //FTransparent: Boolean; //not used becaus of flickering
    FAutoOpenLink: Boolean; //link auto-open with ShellExecute

    FLines: Integer; //read-only
    FTextWidth: Integer; //read-only
    FTextHeight: Integer; //read-only

    FStyleLinkNormal, FStyleLinkHover: THTStyleLinkProp;

    FOnLinkEnter, FOnLinkLeave: THTEvLink;
    FOnLinkClick, FOnLinkRightClick: THTEvLinkClick;

    FIsLinkHover: Boolean; //if has a selected link
    FSelectedLinkID: Integer; //selected link ID

    NoCursorChange: Boolean; //lock CursorChange event
    DefaultCursor: TCursor; //default cursor when not over a link

    procedure SetText(const Value: String);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetMaxWidth(const Value: Integer);

    function GetStoredStyleLink(const Index: Integer): Boolean;
    procedure SetStyleLink(const Index: Integer; const Value: THTStyleLinkProp);

    procedure DoPaint;
    procedure Rebuild; //rebuild words
    procedure BuildAndPaint; //rebuild and repaint
    procedure CheckMouse(X, Y: Integer);
    procedure SetCursorWithoutChange(C: TCursor); //check links by mouse position
    //procedure SetTransparent(const Value: Boolean);
  protected
    procedure Loaded; override;
    procedure Paint; override;
    procedure CMColorchanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMFontchanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure Resize; override;
    procedure MouseMove(Shift: TShiftState; X: Integer; Y: Integer); override;
    procedure CMMouseleave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X: Integer;
      Y: Integer); override;
    procedure CMCursorchanged(var Message: TMessage); message CM_CURSORCHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property IsLinkHover: Boolean read FIsLinkHover;
    property SelectedLinkID: Integer read FSelectedLinkID;
    function GetLinkTarget(LinkID: Integer): String; //get target by link id
    function GetSelectedLinkTarget: String; //get target of selected link

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
    property OnGesture;
    property OnMouseActivate;
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

    property StyleLinkNormal: THTStyleLinkProp index 1 read FStyleLinkNormal write SetStyleLink stored GetStoredStyleLink;
    property StyleLinkHover: THTStyleLinkProp index 2 read FStyleLinkHover write SetStyleLink stored GetStoredStyleLink;

    property Lines: Integer read FLines;
    property TextWidth: Integer read FTextWidth;
    property TextHeight: Integer read FTextHeight;

    property OnLinkEnter: THTEvLink read FOnLinkEnter write FOnLinkEnter;
    property OnLinkLeave: THTEvLink read FOnLinkLeave write FOnLinkLeave;
    property OnLinkClick: THTEvLinkClick read FOnLinkClick write FOnLinkClick;
    property OnLinkRightClick: THTEvLinkClick read FOnLinkRightClick write FOnLinkRightClick;

    property AutoOpenLink: Boolean read FAutoOpenLink write FAutoOpenLink default True;
  end;

procedure Register;

implementation

uses System.SysUtils, System.UITypes, Winapi.Windows, Winapi.ShellAPI;

procedure Register;
begin
  RegisterComponents('Digao', [THTLabel]);
end;

//

constructor THTWord.Create;
begin
    inherited;
    Font := TFont.Create;
end;

destructor THTWord.Destroy;
begin
    Font.Free;
    inherited;
end;

procedure THTWordList.Add(Rect: TRect; Text: String; Group: Integer; Align: TAlignment;
  Font: TFont; BColor: TColor; Link: Boolean; LinkID: Integer; Space: Boolean);
var W: THTWord;
begin
    W := THTWord.Create;
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
end;

//

constructor THTLabel.Create(AOwner: TComponent);
begin
    inherited;
    ControlStyle := ControlStyle + [csOpaque];
    //Warning! The use of transparency in the component causes flickering

    FStyleLinkNormal := THTStyleLinkProp.Create(Self, tslpNormal);
    FStyleLinkHover := THTStyleLinkProp.Create(Self, tslpHover);
    LWords := THTWordList.Create;
    LLinkTargets := TStringList.Create;

    FAutoOpenLink := True;

    FSelectedLinkID := -1;

    DefaultCursor := Cursor;
end;

destructor THTLabel.Destroy;
begin
    FStyleLinkNormal.Free;
    FStyleLinkHover.Free;
    LWords.Free;
    LLinkTargets.Free;
    inherited;
end;

procedure THTLabel.Loaded;
begin
    {Warning! When a component is inserted at design-time, the Loaded
    is not fired, because there is nothing to load. The Loaded is only fired
    when loading component that already has saved properties on DFM file.}
    inherited;
    Rebuild;
end;

procedure THTLabel.BuildAndPaint;
begin
    //Rebuild words and repaint
    Rebuild;
    Invalidate;
end;

procedure THTLabel.SetAutoHeight(const Value: Boolean);
begin
    if Value<>FAutoHeight then
    begin
        FAutoHeight := Value;

        if Value then Rebuild;
    end;
end;

procedure THTLabel.SetAutoWidth(const Value: Boolean);
begin
    if Value<>FAutoWidth then
    begin
        FAutoWidth := Value;

        if Value then Rebuild;
    end;
end;

procedure THTLabel.SetMaxWidth(const Value: Integer);
begin
    if Value<>FMaxWidth then
    begin
        FMaxWidth := Value;

        Rebuild;
    end;
end;

procedure THTLabel.SetText(const Value: String);
begin
    if Value<>FText then
    begin
        FText := Value;

        BuildAndPaint;
    end;
end;

{procedure THTLabel.SetTransparent(const Value: Boolean);
begin
    if Value<>FTransparent then
    begin
        FTransparent := Value;

        Invalidate;
    end;
end;}

procedure THTLabel.CMColorchanged(var Message: TMessage);
begin
    Invalidate;
end;

procedure THTLabel.CMFontchanged(var Message: TMessage);
begin
    BuildAndPaint;
end;

procedure THTLabel.Resize;
begin
    //on component creating, there is no parent and the resize is fired,
    //so, the canvas is not present at this moment.
    if HasParent then
      Rebuild;

    inherited;
end;

procedure THTLabel.Paint;
begin
    inherited;
    DoPaint;
end;

procedure THTLabel.DoPaint;
var W: THTWord;
    B: Vcl.Graphics.TBitmap;
begin
    //Using internal bitmap as a buffer to reduce flickering
    B := Vcl.Graphics.TBitmap.Create;
  try
    B.SetSize(Width, Height);

    //if not FTransparent then
    //begin
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

        DrawText(B.Canvas.Handle, W.Text, -1, W.Rect, DT_NOCLIP or DT_NOPREFIX);
        {Using DrawText, because TextOut has not clip option, which causes
        bad overload of text when painting using background, oversizing the
        text area wildly.}
    end;

    Canvas.Draw(0, 0, B); //to reduce flickering
  finally
    B.Free;
  end;
end;

function THTLabel.GetLinkTarget(LinkID: Integer): String;
begin
    Result := LLinkTargets[LinkID];
end;

function THTLabel.GetSelectedLinkTarget: String;
begin
    Result := GetLinkTarget(FSelectedLinkID);
end;

procedure THTLabel.CMCursorchanged(var Message: TMessage);
begin
    if NoCursorChange then Exit;

    DefaultCursor := Cursor; //save default cursor to when link not selected
end;

procedure THTLabel.SetCursorWithoutChange(C: TCursor);
begin
    //Set cursor, but without fire cursos change event
    NoCursorChange := True;
    try
      Cursor := C;
    finally
      NoCursorChange := False;
    end;
end;

procedure THTLabel.CheckMouse(X, Y: Integer);
var FoundHover, HasChange, Old: Boolean;
    LinkID: Integer;
    W: THTWord;
begin
    FoundHover := False;
    HasChange := False;
    LinkID := -1;

    //find the first work, if there is any
    for W in LWords do
      if W.Link then
      begin
        if W.Rect.Contains(Point(X, Y)) then //selected
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
              FOnLinkEnter(Self, LinkID, GetLinkTarget(LinkID));
        end else
        begin //leave the link
            SetCursorWithoutChange(DefaultCursor); //back to default cursor
            FIsLinkHover := False;
            LinkID := FSelectedLinkID; //save to use on OnLinkLeave event
            FSelectedLinkID := -1;
            if Assigned(FOnLinkLeave) then
              FOnLinkLeave(Self, LinkID, GetLinkTarget(LinkID));
        end;

        Invalidate;
    end;
end;

procedure THTLabel.Click;
var Handled: Boolean;
begin
    if FIsLinkHover then
    begin
       Handled := False;
       if Assigned(FOnLinkClick) then
         FOnLinkClick(Self, FSelectedLinkID, GetSelectedLinkTarget, Handled);

       if FAutoOpenLink and not Handled then
         ShellExecute(0, '', PChar(GetSelectedLinkTarget), '', '', 0);
    end;

    inherited;
end;

procedure THTLabel.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var Handled: Boolean;
begin
    if Button = mbRight then
      if IsLinkHover then
        if Assigned(FOnLinkRightClick) then
        begin
            Handled := False;
            FOnLinkRightClick(Self, FSelectedLinkID, GetSelectedLinkTarget, Handled);
        end;

    inherited;
end;

procedure THTLabel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
    CheckMouse(X, Y);

    inherited;
end;

procedure THTLabel.CMMouseleave(var Message: TMessage);
begin
    //Mouse leaves the componente
    CheckMouse(-1, -1);
end;

//

type
  TTokenKind = (
    ttInvalid,
    ttBold, ttItalic, ttUnderline, ttStrike,
    ttFontName, ttFontSize, ttFontColor, ttBackColor,
    ttTab, ttTabF, ttSpace,
    ttBreak, ttText, ttLink,
    ttAlignLeft, ttAlignCenter, ttAlignRight);

  TToken = class
    Kind: TTokenKind;
    TagClose: Boolean;
    Text: String;
    Value: Integer;
  end;

  TListToken = class(TObjectList<TToken>);

  TBuilder = class
  private
    Lb: THTLabel;
    L: TListToken;
    LGroupBound: TList<Integer>; //bounds list of the grupo
    {The list of created with the X position of limit where the group ends
     to use on text align until the group limit}

    CalcWidth, CalcHeight: Integer; //width and height to set at component when using auto

    function ProcessTag(Tag: String): Boolean;
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
end;

destructor TBuilder.Destroy;
begin
    L.Free;
    LGroupBound.Free;
    inherited;
end;

procedure THTLabel.Rebuild;
var B: TBuilder;
begin
    if csLoading in ComponentState then Exit;

    LWords.Clear; //clean old words
    LLinkTargets.Clear; //clean old links

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

function TBuilder.ProcessTag(Tag: String): Boolean;
var TOff, TOn, HasPar: Boolean;
    Kind: TTokenKind;
    Value: Integer;
    A, Par: String;
    I: Integer;
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
        Par := A;
        Delete(Par, 1, I);
        //Par := Copy(A, I+1, Length(A)-I);
        A := Copy(A, 1, I-1);
    end;

    if TOff and HasPar then Exit; //tag closing with parameter

    Value := 0;
    A := UpperCase(A);
    if (A='BR') and TOn and not HasPar then //LINE BREAK
    begin
        AddToken(ttBreak);
        Result := True;
    end else
    if (A='B') and not HasPar then //BOLD
    begin
        AddToken(ttBold, TOff);
        Result := True;
    end else
    if (A='I') and not HasPar then //ITALIC
    begin
        AddToken(ttItalic, TOff);
        Result := True;
    end else
    if (A='U') and not HasPar then //UNDERLINE
    begin
        AddToken(ttUnderline, TOff);
        Result := True;
    end else
    if (A='S') and not HasPar then //STRIKEOUT
    begin
        AddToken(ttStrike, TOff);
        Result := True;
    end else
    if A='FN' then //FONT NAME
    begin
        if TOn and (Par='') then Exit;
        AddToken(ttFontName, TOff, Par);
        Result := True;
    end else
    if A='FS' then //FONT SIZE
    begin
        if TOn then
        begin
          Value := StrToIntDef(Par, 0);
          if Value<=0 then Exit;
        end;
        AddToken(ttFontSize, TOff, '', Value);
        Result := True;
    end else
    if A='FC' then //FONT COLOR
    begin
        if TOn then
        begin
          Value := ParamToColor(Par);
          if Value=clNone then Exit;
        end;
        AddToken(ttFontColor, TOff, '', Value);
        Result := True;
    end else
    if A='BC' then //BACKGROUND COLOR
    begin
        if TOn then
        begin
          Value := ParamToColor(Par);
          if Value=clNone then Exit;
        end;
        AddToken(ttBackColor, TOff, '', Value);
        Result := True;
    end else
    if A='A' then //LINK
    begin
        if TOn and HasPar and (Par='') then Exit;
        AddToken(ttLink, TOff, Par);
        Result := True;
    end else
    if (A='L') and not HasPar then //ALIGN LEFT
    begin
        AddToken(ttAlignLeft, TOff);
        Result := True;
    end else
    if (A='C') and not HasPar then //ALIGN CENTER
    begin
        AddToken(ttAlignCenter, TOff);
        Result := True;
    end else
    if (A='R') and not HasPar then //ALIGN RIGHT
    begin
        AddToken(ttAlignRight, TOff);
        Result := True;
    end else
    if ((A='T') or (A='TF')) and TOn then //TAB
    begin
        Value := StrToIntDef(Par, 0);
        if Value<=0 then Exit;
        Kind := ttTab;
        if A='TF' then Kind := ttTabF;
        AddToken(Kind, TOff, '', Value);
        Result := True;
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
            AddToken(ttSpace);
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
    T: TToken;

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

    X, Y, HighW, HighH, LineCount: Integer;
    LastTabF: Boolean; //last tabulation was TabF (with break align)
    LastTabF_X: Integer;

    Ex: TSize; FS: TFontStyles; PreWidth: Integer;

    LinkOn: Boolean;
    LinkID: Integer;

  procedure DoLineBreak;
  begin
      if HighH=0 then HighH := C.TextHeight(' '); //line without content
      Inc(Y, HighH); //inc biggest height of the line
      HighH := 0; //clear line height

      if X>HighW then HighW := X; //store width of biggest line
      X := 0; //carriage return :)
      if LastTabF then X := LastTabF_X; //last line breaks with TabF

      LGroupBound.Add(Lb.Width); //add line bound to use in group align
      Inc(LineCount);
  end;

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
    LBold.Add(fsBold in C.Font.Style);
    LItalic.Add(fsItalic in C.Font.Style);
    LUnderline.Add(fsUnderline in C.Font.Style);
    LStrike.Add(fsStrikeOut in C.Font.Style);
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

    for T in L do
    begin
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

          ttText, ttSpace, ttInvalid:
          begin
              case T.Kind of
                ttSpace: T.Text := ' ';
                ttInvalid: T.Text := '<?>';
              end;

              Ex := C.TextExtent(T.Text);
              PreWidth := X+Ex.Width;
              if ((Lb.FAutoWidth) and (Lb.FMaxWidth>0) and (PreWidth>Lb.FMaxWidth))
                or ((not Lb.FAutoWidth) and (PreWidth>Lb.Width)) then
              begin
                  //clear last word on line break when is space to not comsume pixels at end of line
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

              Lb.LWords.Add(Rect(X, Y, X+Ex.Width, Y+Ex.Height),
                T.Text, LGroupBound.Count, Align, C.Font, BackColor, LinkOn, LinkID, T.Kind=ttSpace);

              Inc(X, Ex.Width);
          end;

          ttLink:
          begin
              if T.TagClose then
              begin
                  LinkOn := False;
                  LinkID := -1;
              end else
              begin
                  LinkOn := True;
                  LinkID := Lb.LLinkTargets.Add(T.Text); //add target of the link on list
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
var W: THTWord;
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
      if W.Align in [taCenter, taRightJustify] then
      begin
        Offset := LGroupBound[W.Group] - LW[W.Group];
        if W.Align=taCenter then Offset := Offset div 2;

        W.Rect.Offset(Offset, 0);
      end;
end;

{$REGION 'StyleLinkProp'}
constructor THTStyleLinkProp.Create(xLb: THTLabel; xKind: THTTipoStyleLinkProp);
begin
    inherited Create;

    Lb := xLb;
    Kind := xKind;

    FFontColor := GetDefaultFontColor;
    FBackColor := clNone;
end;

function THTStyleLinkProp.GetOwner: TPersistent;
begin
    Result := Lb;
end;

function THTStyleLinkProp.GetDefaultFontColor: TColor;
begin
    Result := clNone;
    case Kind of
      tslpNormal: Result := clBlue;
      tslpHover: Result := clRed;
    end;
end;

function THTStyleLinkProp.GetStoredFontColor: Boolean;
begin
    Result := FFontColor<>GetDefaultFontColor;
end;

procedure THTStyleLinkProp.SetFontColor(const Value: TColor);
begin
    if Value <> FFontColor then
    begin
        FFontColor := Value;

        Lb.BuildAndPaint;
    end;
end;

procedure THTStyleLinkProp.SetBackColor(const Value: TColor);
begin
    if Value <> FBackColor then
    begin
        FBackColor := Value;

        Lb.BuildAndPaint;
    end;
end;

procedure THTStyleLinkProp.SetUnderline(const Value: Boolean);
begin
    if Value <> FUnderline then
    begin
        FUnderline := Value;

        Lb.BuildAndPaint;
    end;
end;

procedure THTStyleLinkProp.SetPropsToCanvas(C: TCanvas);
begin
    if FFontColor<>clNone then C.Font.Color := FFontColor;
    if FBackColor<>clNone then C.Brush.Color := FBackColor;
    if FUnderline then C.Font.Style := C.Font.Style + [fsUnderline];
end;

procedure THTStyleLinkProp.Assign(Source: TPersistent);
begin
    if Source is THTStyleLinkProp then
    begin
        Self.FFontColor := THTStyleLinkProp(Source).FFontColor;
        Self.FBackColor := THTStyleLinkProp(Source).FBackColor;
        Self.FUnderline := THTStyleLinkProp(Source).FUnderline;
    end else
      inherited;
end;

function THTStyleLinkProp.GetStored: Boolean;
begin
    Result := GetStoredFontColor
           or FUnderline
           or (FBackColor<>clNone);
end;

procedure THTLabel.SetStyleLink(const Index: Integer;
  const Value: THTStyleLinkProp);
begin
    case Index of
      1: FStyleLinkNormal.Assign(Value);
      2: FStyleLinkHover.Assign(Value);
    end;
end;

function THTLabel.GetStoredStyleLink(const Index: Integer): Boolean;
begin
    Result := False;
    case Index of
      1: Result := FStyleLinkNormal.GetStored;
      2: Result := FStyleLinkHover.GetStored;
    end;
end;
{$ENDREGION}

end.
