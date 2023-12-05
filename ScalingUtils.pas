unit ScalingUtils;

interface

uses
  {$IFDEF FPC}
  Forms
  {$ELSE}
  Vcl.Forms
  {$ENDIF};

type
  TDzFormScaling = class
  private
    FScaled: Boolean;
    FDesignerPPI: Integer;
    FMonitorPPI: Integer;
  public
    property Scaled: Boolean read FScaled;
    property DesignerPPI: Integer read FDesignerPPI;
    property MonitorPPI: Integer read FMonitorPPI;

    procedure Update(F: TCustomForm; xDesignerDPI: Integer);
    function Calc(Value: Integer): Integer;
  end;

function RetrieveDesignerPPI(F: TCustomForm): Integer;
function RetrieveMonitorPPI(F: TCustomForm): Integer;

implementation

uses
  {$IFDEF FPC}
  SysUtils, Windows
  {$ELSE}
  System.SysUtils, Winapi.Windows
  {$ENDIF};

{$IF (Defined(DCC) and (CompilerVersion >= 30)) or Defined(FPC)} //D10 Seattle or Lazarus
  {$DEFINE NEW_MONITOR}
{$ENDIF}

function RetrieveMonitorPPI(F: TCustomForm): Integer;
{$IFNDEF NEW_MONITOR}
var
  DC: HDC;
{$ENDIF}
begin
  {$IFDEF NEW_MONITOR}
  Result := F.Monitor.PixelsPerInch;
  {$ELSE}
  DC := GetDC(0);
  Result := GetDeviceCaps(DC, LOGPIXELSY);
  ReleaseDC(0, DC);
  {$ENDIF}
end;

//

type
  TFormScaleHack = class(TCustomForm);

function RetrieveDesignerPPI(F: TCustomForm): Integer;
begin
  Result :=
    {$IFDEF FPC}
    F.PixelsPerInch
    {$ELSE}
      {$IF CompilerVersion >= 31} //D10.1 Berlin
      TFormScaleHack(F).GetDesignDpi
      {$ELSE}
      TFormScaleHack(F).PixelsPerInch
      {$ENDIF}
    {$ENDIF};
end;

procedure TDzFormScaling.Update(F: TCustomForm; xDesignerDPI: Integer);
begin
  if F<>nil then
  begin
    FScaled := TFormScaleHack(F).Scaled;
    FDesignerPPI := xDesignerDPI; //Delphi 11 is not storing original design DPI (it changes by current monitor PPI)
    FMonitorPPI := RetrieveMonitorPPI(F);
  end else
  begin
    FScaled := False;
    FDesignerPPI := 0;
    FMonitorPPI := 0;
  end;
end;

function TDzFormScaling.Calc(Value: Integer): Integer;
begin
  if FScaled then
    Result := MulDiv(Value, FMonitorPPI, FDesignerPPI) //{$IFDEF FPC}ScaleDesignToForm(Value){$ELSE}ScaleValue(Value){$ENDIF} - only supported in Delphi 10.4 (Monitor.PixelsPerInch supported too)
  else
    Result := Value;
end;

end.
