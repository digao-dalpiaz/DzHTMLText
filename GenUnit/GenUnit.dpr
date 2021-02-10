program GenUnit;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes, System.IOUtils;

function GetFileDateTime(const Path: string): TDateTime;
begin
  Result := 0;

  if TFile.Exists(Path) then
    Result := TFile.GetLastWriteTime(Path);
end;

const
  FILE_VCL = 'Vcl.DzHTMLText.pas';
  FILE_FMX = 'FMX.DzHTMLText.pas';

  DEFINE_FMX = '{$DEFINE FMX}';

var
  S: TStringList;
  TimeVcl, TimeFmx: TDateTime;
  FileFrom, FileTo, aNewest, aRet: string;
begin
  try
    Writeln('DzHTMLText Source Synchronizer');
    Writeln('------------------------------');
    Writeln('');

    TimeVcl := GetFileDateTime(FILE_VCL);
    TimeFmx := GetFileDateTime(FILE_FMX);

    Writeln('VCL Timestamp: '+DateTimeToStr(TimeVcl));
    Writeln('FMX Timestamp: '+DateTimeToStr(TimeFmx));
    Writeln('');

    if TimeVcl > TimeFmx then
    begin
      FileFrom := FILE_VCL;
      FileTo := FILE_FMX;

      aNewest := 'VCL';
    end else
    begin
      FileFrom := FILE_FMX;
      FileTo := FILE_VCL;

      aNewest := 'FMX';
    end;

    Writeln('Newest: '+aNewest);
    Writeln('');
    Writeln('Confirm update from '+FileFrom+' to '+FileTo+'? (y/n)');
    Readln(aRet);
    if aRet<>'y' then Exit;

    Writeln('');
    Writeln('Writing file...');

    S := TStringList.Create;
    try
      S.LoadFromFile(FileFrom);

      if S[0]=DEFINE_FMX then
        S.Delete(0)
      else
        S.Insert(0, DEFINE_FMX);

      S.SaveToFile(FileTo);
    finally
      S.Free;
    end;

    Writeln('File updated!');
    Readln;

  except
    on E: Exception do
    begin
      Writeln('ERROR: '+E.Message);
      Readln;
    end;
  end;
end.
