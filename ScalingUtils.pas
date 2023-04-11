unit ScalingUtils;

interface

uses
  {$IFDEF FPC}
  Windows, SysUtils
  {$ELSE}
  Winapi.Windows, System.SysUtils, Winapi.MultiMon
  {$ENDIF};

function GetMonitorPPI(FHandle: HMONITOR): Integer;

implementation

type
  TMonitorDpiType = (
    MDT_EFFECTIVE_DPI = 0,
    MDT_ANGULAR_DPI = 1,
    MDT_RAW_DPI = 2,
    MDT_DEFAULT = MDT_EFFECTIVE_DPI
  );

{$WARN SYMBOL_PLATFORM OFF}
function GetDpiForMonitor(
  hmonitor: HMONITOR;
  dpiType: TMonitorDpiType;
  out dpiX: UINT;
  out dpiY: UINT
  ): HRESULT; stdcall; external 'Shcore.dll' {$IFDEF DCC}delayed{$ENDIF};
{$WARN SYMBOL_PLATFORM ON}

function GetMonitorPPI(FHandle: HMONITOR): Integer;
var
  Ydpi: Cardinal;
  Xdpi: Cardinal;
  DC: HDC;
begin
  if CheckWin32Version(6,3) then
  begin
    if GetDpiForMonitor(FHandle, TMonitorDpiType.MDT_EFFECTIVE_DPI, Ydpi, Xdpi) = S_OK then
      Result := Ydpi
    else
      Result := 0;
  end
  else
  begin
    DC := GetDC(0);
    Result := GetDeviceCaps(DC, LOGPIXELSY);
    ReleaseDC(0, DC);
  end;
end;

end.
