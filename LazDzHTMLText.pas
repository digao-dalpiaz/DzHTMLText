{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit LazDzHTMLText;

{$warn 5023 off : no warning about unused units}
interface

uses
  Vcl.DzHTMLText, Vcl.DHCommon, Vcl.DHTokenEngine, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Vcl.DzHTMLText', @Vcl.DzHTMLText.Register);
end;

initialization
  RegisterPackage('LazDzHTMLText', @Register);
end.
