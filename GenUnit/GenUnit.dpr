program GenUnit;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes;

var
  S: TStringList;
begin
  try
    S := TStringList.Create;
    try
      S.LoadFromFile('DzHTMLText.pas');
      S.Insert(0, '{$DEFINE FMX}');
      S.SaveToFile('FMX.DzHTMLText.pas');
    finally
      S.Free;
    end;
  except
    on E: Exception do
    begin
      Writeln(E.ClassName, ': ', E.Message);
      Readln;
    end;
  end;
end.
