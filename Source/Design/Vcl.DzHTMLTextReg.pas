{$IFNDEF FMX}unit Vcl.DzHTMLTextReg;{$ENDIF}

interface

procedure Register;

implementation

uses
{$IFDEF FPC}
  ComponentEditors, LResources, Classes, SysUtils, Dialogs,
  Vcl.DzHTMLText
{$ELSE}
  DesignEditors, DesignIntf, System.Classes, System.SysUtils, Vcl.Dialogs,
  {$IFDEF FMX}
     FMX.DzHTMLText
  {$ELSE}
     Vcl.DzHTMLText
  {$ENDIF}
{$ENDIF};

type
  TDHPropEdit = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;

    procedure ShowSyntaxErrors;
  end;

procedure TDHPropEdit.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowSyntaxErrors;
    else raise EDHInternalExcept.Create('Property editor verb index invalid');
  end;
end;

function TDHPropEdit.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Show Syntax Errors';
    else raise EDHInternalExcept.Create('Property editor verb index invalid');
  end;
end;

function TDHPropEdit.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure TDHPropEdit.ShowSyntaxErrors;
var
  Lb: TDzHTMLText;
  SB: TStringBuilder;
  Error: TDHSyntaxError;
  Line: string;
begin
  Lb := TDzHTMLText(Component);

  SB := TStringBuilder.Create;
  try
    if Lb.SyntaxErrors.Count=0 then
      SB.Append('No syntax errors')
    else
    begin
      SB.AppendLine(Format('Errors (%d):', [Lb.SyntaxErrors.Count]));
      for Error in Lb.SyntaxErrors do
      begin
        if Error.Position>0 then
          Line := Format('Position %d: %s', [Error.Position, Error.Description])
        else
          Line := Error.Description;

        SB.AppendLine('- ' + Line);
      end;
    end;

    ShowMessage(SB.ToString);
  finally
    SB.Free;
  end;
end;

//

procedure Register;
begin
  {$IFDEF FPC}{$I DzHTMLText.lrs}{$ENDIF}
  RegisterComponents('Digao', [TDzHTMLText]);
  RegisterComponentEditor(TDzHTMLText, TDHPropEdit);
end;

end.
